{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.C.Linter.Memcpy (descr) where

import           Control.Monad                   (unless)
import           Data.Functor.Identity           (Identity)
import           Data.Text                       (Text)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (CompTypeRef (CompTypeRef),
                                                  EnumTypeRef (EnumTypeRef),
                                                  GlobalDecls,
                                                  IntType (TyUChar), Type (..),
                                                  TypeName (TyComp, TyEnum, TyIntegral, TyVoid))
import           Language.C.Analysis.TravMonad   (Trav, TravT)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (Ident (..))
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST           (CExpr, CExpression (..),
                                                  annotation)
import           Prettyprinter                   (pretty, (<+>))
import           Tokstyle.C.Env                  (Env, recordLinterError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (backticks)

compatibleType :: Type -> Type -> Bool
compatibleType TY_sockaddr_storage_ptr TY_sockaddr_ptr     = True
compatibleType TY_sockaddr_storage_ptr TY_sockaddr_in_ptr  = True
compatibleType TY_sockaddr_storage_ptr TY_sockaddr_in6_ptr = True
compatibleType TY_sockaddr_ptr         TY_sockaddr_storage_ptr = True
compatibleType TY_sockaddr_in_ptr      TY_sockaddr_storage_ptr = True
compatibleType TY_sockaddr_in6_ptr     TY_sockaddr_storage_ptr = True
compatibleType (PtrType    a _ _  ) (PtrType    b _ _  ) = compatibleType a b
compatibleType (ArrayType  a _ _ _) (PtrType    b _ _  ) = compatibleType a b
compatibleType (PtrType    a _ _  ) (ArrayType  b _ _ _) = compatibleType a b
compatibleType (ArrayType  a _ _ _) (ArrayType  b _ _ _) = compatibleType a b
compatibleType (DirectType a _ _  ) (DirectType b _ _  ) = compatibleTypeName a b
compatibleType _ _                                       = False

compatibleTypeName :: TypeName -> TypeName -> Bool
-- `uint8_t*` can can be memcpy'd to and from anything.
compatibleTypeName (TyIntegral TyUChar) _ = True
compatibleTypeName _ (TyIntegral TyUChar) = True
-- Integral types can only be memcpy'd to the same integral type.
compatibleTypeName (TyIntegral a) (TyIntegral b) = a == b
-- Structs can only be memcpy'd to the exact same struct.
compatibleTypeName (TyComp (CompTypeRef a _ _)) (TyComp (CompTypeRef b _ _)) = a == b
-- Enums can only be memcpy'd to the exact same enum.
compatibleTypeName (TyEnum (EnumTypeRef a _)) (TyEnum (EnumTypeRef b _)) = a == b
-- Void pointers are disallowed.
compatibleTypeName TyVoid _ = False
compatibleTypeName _ TyVoid = False
-- Everything else is disallowed.
compatibleTypeName _ _ = False

validMemType :: Type -> Bool
validMemType (PtrType   DirectType{} _ _  ) = True
validMemType (ArrayType DirectType{} _ _ _) = True
validMemType _                              = False

checkMemType :: String -> CExpr -> Type -> Trav Env ()
checkMemType fname expr ty =
    unless (validMemType (canonicalType ty)) $
        recordLinterError (annotation expr) $
            backticks (pretty fname) <+> "argument type" <+> backticks (pretty (show (C.pretty ty)))
            <+> "is not a valid memory type (pointers to arrays are not allowed)"

checkCompatibility :: String -> CExpr -> CExpr -> Trav Env ()
checkCompatibility fname dst src = do
    dstTy <- tExpr [] RValue dst
    srcTy <- tExpr [] RValue src
    checkMemType fname dst dstTy
    checkMemType fname src srcTy
    unless (compatibleType (canonicalType dstTy) (canonicalType srcTy)) $
        recordLinterError (annotation dst) $
            backticks (pretty fname) <+> "first argument type" <+> backticks (pretty (show (C.pretty dstTy)))
            <+> "is not compatible with second argument type"
            <+> backticks (pretty (show (C.pretty srcTy)))

linter :: AstActions (TravT Env Identity)
linter = astActions
    { doExpr = \node act -> case node of
        CCall (CVar (Ident fname _ _) _) [dst, src, _] _ | fname `elem` ["memcpy", "memmove", "memcmp"] -> do
            checkCompatibility fname dst src
            act

        _ -> act
    }

analyse :: GlobalDecls -> Trav Env ()
analyse = traverseAst linter


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("memcpy", "Checks compatibility of dst and src in memcpy/memcmp."))
