{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.CallbackData (descr) where

import           Control.Monad                   (forM_, when)
import           Data.Functor.Identity           (Identity)
import           Data.List                       (isSuffixOf)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (listToMaybe, mapMaybe)
import           Data.Text                       (Text)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep      (FunDef (..), FunType (..),
                                                  GlobalDecls (..),
                                                  IdentDecl (..),
                                                  ParamDecl (..), Type (..),
                                                  TypeQuals (..), VarDecl (..),
                                                  VarName (..), noTypeQuals)
import           Language.C.Analysis.TravMonad   (Trav, TravT, getUserState,
                                                  modifyUserState)
import           Language.C.Analysis.TypeUtils   (canonicalType, sameType)
import           Language.C.Data.Ident           (Ident (Ident))
import qualified Language.C.Pretty               as C
import           Language.C.Syntax.AST
import           Prettyprinter                   (pretty, (<+>))
import           Tokstyle.C.Env                  (Env (..), recordLinterError)
import           Tokstyle.C.Patterns
import           Tokstyle.C.TraverseAst          (AstActions (..), astActions,
                                                  traverseAst)
import           Tokstyle.C.TravUtils            (backticks)


-- | Get the name of an identifier.
idName :: Ident -> String
idName (Ident name _ _) = name


-- | Strip qualifiers from a type.
stripQualifiers :: Type -> Type
stripQualifiers = \case
    DirectType ty _ attrs    -> DirectType ty noTypeQuals attrs
    PtrType ty _ attrs       -> PtrType (stripQualifiers ty) noTypeQuals attrs
    ArrayType ty sz _ attrs  -> ArrayType (stripQualifiers ty) sz noTypeQuals attrs
    FunctionType ty attrs    -> FunctionType ty attrs
    TypeDefType tdr _ attrs  -> TypeDefType tdr noTypeQuals attrs

-- | Check if two types are compatible context types.
-- T* is compatible with T* and void*.
compatibleContext :: Type -> Type -> Bool
compatibleContext (canonicalType -> TY_void_ptr) _ = True
compatibleContext _ (canonicalType -> TY_void_ptr) = True
compatibleContext a b = compatible (canonicalType a) (canonicalType b)
  where
    compatible (PtrType aTy _ _) (PtrType bTy _ _) = compatible aTy bTy
    compatible a' b' = sameType (stripQualifiers a') (stripQualifiers b')


-- | Check if a type is a function pointer or a function.
isFunPtr :: Type -> Bool
isFunPtr (canonicalType -> PtrType FunctionType{} _ _) = True
isFunPtr (canonicalType -> FunctionType{})             = True
isFunPtr _                                             = False


-- | Check if a type is a void pointer.
isVoidPtr :: Type -> Bool
isVoidPtr (canonicalType -> TY_void_ptr) = True
isVoidPtr _                              = False


-- | Get the type of a parameter.
getParamType :: ParamDecl -> Type
getParamType (ParamDecl (VarDecl _ _ ty) _)         = ty
getParamType (AbstractParamDecl (VarDecl _ _ ty) _) = ty


-- | Find the first function pointer and first void pointer in a list of arguments.
findCallbackPair :: [(ParamDecl, CExpr, Type)] -> Maybe (CExpr, Type, CExpr, Type)
findCallbackPair args =
    let funPtrs = filter (\(p, _, _) -> isFunPtr (getParamType p)) args
        voidPtrs = filter (\(p, _, _) -> isVoidPtr (getParamType p)) args
    in case (funPtrs, voidPtrs) of
        ((_, f, fTy):_, (_, p, pTy):_) -> Just (f, fTy, p, pTy)
        _                              -> Nothing


-- | Extract the identifier from a CExpr if it's a simple variable or address of a variable.
getIdent :: CExpr -> Maybe Ident
getIdent (CVar ident _)      = Just ident
getIdent (CUnary CAdrOp e _) = getIdent e
getIdent (CCast _ e _)       = getIdent e
getIdent _                   = Nothing


checkCall :: Map Ident Type -> [ParamDecl] -> [CExpr] -> Trav Env ()
checkCall inferred params args = do
    argTys <- mapM (tExpr [] RValue) args
    case findCallbackPair (zip3 params args argTys) of
        Just (f, _, p, pTy) ->
            case getIdent f >>= \ident -> Map.lookup (idName ident) inferredByName of
                Just expectedTy ->
                    when (not $ compatibleContext expectedTy pTy) $
                        recordLinterError (annotation p) $
                            "callback expects context of type" <+> backticks (pretty (show (C.pretty expectedTy)))
                             <> ", but got" <+> backticks (pretty (show (C.pretty pTy)))
                Nothing -> return ()
        Nothing -> return ()
  where
    inferredByName = Map.fromList [ (idName k, v) | (k, v) <- Map.toList inferred ]

-- | Find assignments to struct members that look like callback registration.
checkBlockItems :: Map Ident Type -> [CBlockItem] -> Trav Env ()
checkBlockItems inferred items = do
    let assigns = mapMaybe getAssign items
    -- For each struct being assigned to, find its callback and object assignments.
    let groups = Map.fromListWith (++) [ (idName s, [(f, v)]) | (s, f, v) <- assigns ]
    forM_ (Map.elems groups) $ \group -> do
        let cb = listToMaybe [ v | (f, v) <- group, "callback" `isSuffixOf` idName f ]
        let obj = listToMaybe [ v | (f, v) <- group, "object" `isSuffixOf` idName f ]
        case (cb, obj) of
            (Just cbExpr, Just objExpr) -> do
                _ <- tExpr [] RValue cbExpr
                objTy <- tExpr [] RValue objExpr
                case getIdent cbExpr >>= \ident -> Map.lookup (idName ident) inferredByName of
                    Just expectedTy ->
                        when (not $ compatibleContext expectedTy objTy) $
                            recordLinterError (annotation objExpr) $
                                "callback expects context of type" <+> backticks (pretty (show (C.pretty expectedTy)))
                                 <> ", but got" <+> backticks (pretty (show (C.pretty objTy)))
                    Nothing -> return ()
            _ -> return ()
  where
    inferredByName = Map.fromList [ (idName k, v) | (k, v) <- Map.toList inferred ]

    getAssign (CBlockStmt (CExpr (Just (CAssign _ (CMember (CVar s _) f _ _) v _)) _)) = Just (s, f, v)
    getAssign _ = Nothing


linter :: Map Ident Type -> AstActions (TravT Env Identity)
linter inferred = astActions
    { doExpr = \node act -> case node of
        CCall fun args _ -> do
            tExpr [] RValue fun >>= \case
                FunctionType (FunType _ params _) _ ->
                    checkCall inferred params args
                PtrType (FunctionType (FunType _ params _) _) _ _ ->
                    checkCall inferred params args
                _ -> return ()
            act

        _ -> act

    , doBlockItems = \items act -> do
        checkBlockItems inferred items
        act
    }


-- | Collector for inferred callback types.
collector :: AstActions (TravT Env Identity)
collector = astActions
    { doIdentDecl = \node act -> case node of
        FunctionDef (FunDef (VarDecl (VarName ident _) _ (FunctionType (FunType _ (ParamDecl (VarDecl (VarName arg1 _) _ TY_void_ptr) _:_) _) _)) (CCompound _ items _) _) -> do
            case findCast arg1 items of
                Just tyExpr -> do
                    ty <- tExpr [] RValue tyExpr
                    modifyUserState $ \env -> env { inferredTypes = Map.insert ident ty (inferredTypes env) }
                Nothing -> return ()
            act
        _ -> act
    }
  where
    findCast arg (CBlockDecl (CDecl _ [(Just _, Just (CInitExpr tyExpr@(CCast _ (CVar arg' _) _) _), _)] _) : _)
        | idName arg == idName arg' = Just tyExpr
    findCast arg (_ : ss) = findCast arg ss
    findCast _ [] = Nothing


analyse :: GlobalDecls -> Trav Env ()
analyse decls = do
    traverseAst collector decls
    inferred <- inferredTypes <$> getUserState
    traverseAst (linter inferred) decls


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("callback-data", "Checks that the context pointer passed to a callback matches the expected type inferred from the callback's first argument."))
