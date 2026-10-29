{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.SizeofTarget (descr) where

import           Control.Monad                   (forM_, unless)
import           Data.Functor.Identity           (Identity)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (GlobalDecls, Type (..),
                                                  TypeName (..), declType)
import           Language.C.Analysis.TravMonad   (Trav, TravT)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  CUnaryOp (..), annotation)
import           Prettyprinter                   (pretty, (<+>))
import           Tokstyle.C.Env                  (Env, recordLinterError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (backticks)


-- | Parameters that take a buffer of bytes or numbers.
isBufferParam :: Type -> Bool
isBufferParam ty = case canonicalType ty of
    PtrType t _ _     -> isVoid t || isIntegral t
    ArrayType t _ _ _ -> isIntegral t
    _                 -> False
  where
    isVoid (DirectType TyVoid _ _) = True
    isVoid _                       = False

-- | What an argument points at, without casts or `&`.
target :: CExpr -> CExpr
target = \case
    CCast _ e _       -> target e
    CUnary CAdrOp e _ -> target e
    e                 -> e

-- | `sizeof(*p)` is the size of what `p` points at.
sized :: CExpr -> CExpr
sized = \case
    CUnary CIndOp e _ -> target e
    e                 -> target e

render :: CExpr -> String
render = show . C.pretty

checkCall :: [Type] -> [CExpr] -> TravT Env Identity ()
checkCall params args =
    forM_ (zip3 [0 ..] params args) $ \case
        (i, ty, CSizeofExpr e _) | isIntegral ty -> do
            let buffers = [ a | (pty, a) <- take i (zip params args), isBufferParam pty ]
            unless (null buffers || render (sized e) `elem` map (render . target) buffers) $
                recordLinterError (annotation e) $
                    backticks (pretty ("sizeof(" <> render e <> ")"))
                    <+> "is not the size of"
                    <+> pretty (Text.intercalate " or " [ "`" <> Text.pack (render b) <> "`" | b <- buffers ])
        _ -> return ()

linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCall fun args _ -> do
            tExpr [] RValue fun >>= \case
                FunPtrParams params -> checkCall (map declType params) args
                _                   -> return ()
            act
        _ -> act
    }

analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter

descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("sizeof-target", Text.unlines
    [ "Checks that a `sizeof` passed as a size is the size of a buffer in the same call."
    , ""
    , "In a call such as `crypto_memzero(key, sizeof(other_key))`, the size must be"
    , "of `key` (or `*key` when it's a pointer). For `memcpy`, either buffer will do."
    , ""
    , "**Reason:** two buffers often have the same size by chance, such as public"
    , "and secret keys, so the wrong `sizeof` works until one of them changes."
    ]))
