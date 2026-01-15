{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Memset (analyse) where

import           Control.Monad                   (unless)
import           Data.Functor.Identity           (Identity)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.DefTable    (lookupTag)
import           Language.C.Analysis.SemRep      (CompType (..),
                                                  CompTypeRef (..), GlobalDecls,
                                                  MemberDecl (..), TagDef (..),
                                                  Type (..), TypeName (..),
                                                  VarDecl (..))
import           Language.C.Analysis.TravMonad   (MonadTrav, Trav, TravT,
                                                  getDefTable, throwTravError)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Error           (userErr)
import           Language.C.Data.Ident           (Ident (..))
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST           (CExpression (..), annotation)
import           Prettyprinter                   (pretty)
import           Tokstyle.C.Env                  (Env, recordLinterError)
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)


hasPtrs :: MonadTrav m => Type -> m Bool
hasPtrs ty = case canonicalType ty of
    DirectType (TyComp (CompTypeRef name _ _)) _ _ -> do
        defs <- getDefTable
        case lookupTag name defs of
            Just (Right (CompDef (CompType _ _ members _ _))) ->
                and <$> mapM memberHasPtrs members
            _ ->
                throwTravError $ userErr $
                    "couldn't find struct/union type `" <> show (C.pretty name) <> "`"
    PtrType{} -> return True
    _ -> return False

memberHasPtrs :: MonadTrav m => MemberDecl -> m Bool
memberHasPtrs (MemberDecl (VarDecl _ _ ty) _ _) = hasPtrs ty
memberHasPtrs _                                 = return False


memsetAllowed :: MonadTrav m => Type -> m Bool
memsetAllowed ty = case canonicalType ty of
    PtrType pointee _ _ -> not <$> hasPtrs pointee
    ArrayType memTy _ _ _ -> not <$> hasPtrs memTy
    _ ->
        throwTravError $ userErr $
            "value of type `" <> show (C.pretty ty) <> "` cannot be passed to memset"


linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCall (CVar (Ident "memset" _ _) _) [s, _, _] _ -> do
            ty <- tExpr [] RValue s
            allowed <- memsetAllowed ty
            unless allowed $
                recordLinterError (annotation s) $
                    "disallowed memset argument `" <> pretty (show (C.pretty s)) <> "` of type `"
                     <> pretty (show (C.pretty ty)) <> "`, which contains pointers"
            act

        _ -> act
    }


analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter
