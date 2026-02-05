{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module Tokstyle.Linter.OwnershipDecls (descr) where

import           Control.Monad               (unless, when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import qualified Data.List                   as List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Nullability (..), Scope (..),
                                              lexemeText)
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic,
                                              HasDiagnosticInfo (..),
                                              HasDiagnosticsRich (..))
import           Language.Cimple.Pretty      (ppNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (hsep, pretty, punctuate, (<+>))
import           Tokstyle.Common             (backticks, functionName, warnDoc)


data DeclInfo = DeclInfo
    { declAnnotated    :: Bool
    , declPublicHeader :: Bool
    }

data Linter = Linter
    { diags           :: [Diagnostic CimplePos]
    , decls           :: Map Text DeclInfo
    , pointerTypedefs :: Set Text
    }

empty :: Linter
empty = Linter [] Map.empty Set.empty


instance HasDiagnosticsRich Linter CimplePos where
    addDiagnosticRich diag l@Linter{diags} = l{diags = diag : diags}


hasNullability :: Linter -> Node (Lexeme Text) -> Bool
hasNullability l (Fix node) = case node of
    TyNonnull _       -> True
    TyNullable _      -> True
    TyPointer t       -> hasNullability l t
    TyConst t         -> hasNullability l t
    TyForce t         -> hasNullability l t
    VarDecl t _ specs -> hasNullability l t || any isAnnotatedArray specs
    _                 -> False
  where
    isAnnotatedArray (Fix (DeclSpecArray n _)) = n /= NullabilityUnspecified
    isAnnotatedArray _                         = False


isPointerType :: Linter -> Node (Lexeme Text) -> Bool
isPointerType l (Fix node) = case node of
    TyPointer _                -> True
    TyNullable t               -> isPointerType l t
    TyNonnull t                -> isPointerType l t
    TyConst t                  -> isPointerType l t
    TyForce t                  -> isPointerType l t
    VarDecl t _ specs          -> isPointerType l t || any isArray specs
    TyUserDefined (L _ _ name) -> name `Set.member` pointerTypedefs l
    TyFunc (L _ _ name)        -> name `Set.member` pointerTypedefs l
    _                          -> False
  where
    isArray (Fix (DeclSpecArray {})) = True
    isArray _                        = False


isThirdParty :: FilePath -> Bool
isThirdParty path = "third_party/" `List.isInfixOf` path


isPublicHeader :: FilePath -> Bool
isPublicHeader path = any (`List.isSuffixOf` path) ["tox.h", "tox_events.h", "tox_dispatch.h", "toxav.h", "toxencryptsave.h", "tox_options.h", "tox_log_level.h"]


checkNullability :: HasDiagnosticInfo at CimplePos => FilePath -> at -> Node (Lexeme Text) -> State Linter ()
checkNullability file at ty = do
    l <- State.get
    when (isPointerType l ty && not (hasNullability l ty)) $
        warnDoc file at $ "pointer type" <+> backticks (ppNode ty)
            <+> "should have an explicit nullability annotation (`_Nullable` or `_Nonnull`)"


checkPrototypeNullability :: FilePath -> Node (Lexeme Text) -> State Linter ()
checkPrototypeNullability file (Fix (FunctionPrototype retType _ params)) = do
    checkNullability file retType retType
    mapM_ checkParamNullability params
  where
    checkParamNullability (Fix (VarDecl ty name _)) = checkNullability file name ty
    checkParamNullability _                         = return ()
checkPrototypeNullability _ _ = return ()


findQualifiers :: Node (Lexeme Text) -> [Text]
findQualifiers (Fix node) = case node of
    TyOwner t    -> "_Owner" : findQualifiers t
    TyNonnull t  -> "_Nonnull" : findQualifiers t
    TyNullable t -> "_Nullable" : findQualifiers t
    TyConst t    -> findQualifiers t
    TyPointer t  -> findQualifiers t
    _            -> []

findPrototypeQualifiers :: Node (Lexeme Text) -> [Text]
findPrototypeQualifiers (Fix (FunctionPrototype retType _ params)) =
    findQualifiers retType ++ concatMap findParamQualifiers params
  where
    findParamQualifiers (Fix (VarDecl ty _ _)) = findQualifiers ty
    findParamQualifiers _                      = []
findPrototypeQualifiers _ = []


collectDefsWithFile :: AstActions (State Linter) Text
collectDefsWithFile = astActions
    { doNode = \file node act ->
        case unFix node of
            Typedef ty name specs -> do
                st <- State.get
                let isPtr = isPointerType st ty || any isArray specs
                when isPtr $ State.modify $ \s -> s { pointerTypedefs = Set.insert (lexemeText name) (pointerTypedefs s) }
                act
              where
                isArray (Fix (DeclSpecArray {})) = True
                isArray _                        = False

            TypedefFunction proto -> do
                case functionName proto of
                    Just name -> State.modify $ \s -> s { pointerTypedefs = Set.insert name (pointerTypedefs s) }
                    Nothing -> return ()
                act

            FunctionDecl _ proto@(Fix (FunctionPrototype _ name _)) -> do
                State.modify $ \s -> s { decls = Map.insert (lexemeText name) (DeclInfo (not . null $ findPrototypeQualifiers proto) (isPublicHeader file)) (decls s) }
                act

            _ -> act
    }


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        if isThirdParty file then act else case unFix node of
            FunctionDecl _ proto@(Fix (FunctionPrototype _ name _)) -> do
                unless (isPublicHeader file) $ checkPrototypeNullability file proto
                act

            FunctionDefn scope proto@(Fix (FunctionPrototype _ name _)) _ -> do
                Linter{decls} <- State.get
                let nameText = lexemeText name
                let mInfo = Map.lookup nameText decls
                let qs = List.nub $ findPrototypeQualifiers proto
                let shouldAllowQualifiers = case mInfo of
                        Just info -> declPublicHeader info && not (declAnnotated info)
                        Nothing   -> scope == Static
                unless (null qs || shouldAllowQualifiers) $
                    warnDoc file name $ "qualifier" <> (if length qs > 1 then "s" else "")
                        <+> hsep (punctuate " and" (map (backticks . pretty) qs))
                        <+> "should only be used on function declarations, not definitions"

                let shouldExpectAnnotations = case mInfo of
                        Just info -> declPublicHeader info && not (declAnnotated info)
                        Nothing   -> True
                when shouldExpectAnnotations $ checkPrototypeNullability file proto
                act

            Struct _ members -> do
                unless (isPublicHeader file) $ mapM_ checkMember members
                act
              where
                checkMember (Fix (MemberDecl (Fix (VarDecl ty fieldName _)) _)) =
                    checkNullability file fieldName ty
                checkMember _ = return ()

            _ -> act
    }


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus =
    let linterState = State.execState (traverseAst collectDefsWithFile tus) empty
    in reverse . diags $ State.execState (traverseAst linter tus) linterState

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("ownership-decls", Text.unlines
    [ "Checks that `_Owner`, `_Nullable`, and `_Nonnull` are only set on declarations,"
    , "not definitions, unless it's a static definition without prior declaration."
    , ""
    , "**Reason:** keeping qualifiers on declarations only reduces clutter in the"
    , "implementation and ensures that the interface is the single source of truth"
    , "for ownership or nullability information."
    ]))
