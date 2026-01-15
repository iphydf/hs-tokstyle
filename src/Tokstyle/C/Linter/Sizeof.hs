{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.Sizeof (analyse) where

import           Data.Functor.Identity           (Identity)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (GlobalDecls, Type (..))
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  annotation)
import           Prettyprinter                   (pretty)
import           Tokstyle.C.Env                  (Env, recordLinterError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)


-- | This catches `sizeof(buf)` where `buf` is a pointer instead of an array.
checkSizeof :: CExpr -> Type -> TravT Env Identity ()
checkSizeof _ (canonicalType -> TY_struct _) = return ()
checkSizeof _ (canonicalType -> TY_struct_ptr "IPPTsPng") = return ()
checkSizeof _ ArrayType{} = return ()
checkSizeof e ty
  | isIntegral ty = return ()
  | otherwise =
      recordLinterError (annotation e) $
          "disallowed sizeof argument of type `" <> pretty (show (C.pretty ty)) <>
          "` - did you mean for `" <> pretty (show (C.pretty e)) <> "` to be an array?"


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CSizeofExpr e _ -> do
            ty <- tExpr [] RValue e
            checkSizeof e ty
            act

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
