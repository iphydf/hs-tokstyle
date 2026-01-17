{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Tokstyle.Linter.Nullability (descr) where

import           Control.Monad               (forM_, when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Foldable               (traverse_)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp (..), Lexeme (..), Node,
                                              UnaryOp (..))
import qualified Language.Cimple             as C
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty, (<+>))
import           Tokstyle.Common             (backticks, warn, warnDoc)

data Nullability
    = NullableVar
    | NonNullVar
    | UnspecifiedNullability
    deriving (Show, Eq, Ord)

type VarInfo = (Nullability, Maybe (Node (Lexeme Text)))

type TypeEnv = Map Text VarInfo

data LinterState = LinterState
    { typeEnv      :: TypeEnv
    , structDefs   :: Map Text TypeEnv
    , functionDefs :: Map Text (Nullability, [Nullability])
    , nonNullSet   :: Set Text
    , currentFile  :: FilePath
    }

type LinterM = State.StateT LinterState (State [Diagnostic CimplePos])

isNullable :: Node (Lexeme Text) -> Bool
isNullable = \case
    Fix (C.TyNullable _) -> True
    Fix (C.TyPointer t)  -> isNullable t
    Fix (C.TyConst t)    -> isNullable t
    _                    -> False

isNonnull :: Node (Lexeme Text) -> Bool
isNonnull = \case
    Fix (C.TyNonnull _) -> True
    Fix (C.TyPointer t) -> isNonnull t
    Fix (C.TyConst t)   -> isNonnull t
    _                   -> False

exprToText :: Node (Lexeme Text) -> Maybe Text
exprToText (Fix node) = case node of
    C.VarExpr (C.L _ _ name) -> Just name
    C.PointerAccess e (C.L _ _ member) -> do
        base <- exprToText e
        Just $ base <> "->" <> member
    C.MemberAccess e (C.L _ _ member) -> do
        base <- exprToText e
        Just $ base <> "." <> member
    C.UnaryExpr C.UopDeref e -> do
        base <- exprToText e
        Just $ "*" <> base
    C.ParenExpr e -> exprToText e
    _ -> Nothing

getParamTypes :: Node (Lexeme Text) -> [(Text, VarInfo)]
getParamTypes (Fix (C.FunctionPrototype _ _ params)) = concatMap getVarDecls $ params
  where
    getVarDecls decl@(Fix (C.VarDecl ty (C.L _ _ name) _)) =
        [(name, (getNullability' ty, Just decl))]
    getVarDecls _ = []
getParamTypes _ = []

getNullability' :: Node (Lexeme Text) -> Nullability
getNullability' ty
  | isNullable ty = NullableVar
  | isNonnull ty  = NonNullVar
  | otherwise     = UnspecifiedNullability

getStructName :: Node (Lexeme Text) -> Maybe Text
getStructName (Fix node) = case node of
    C.TyPointer t                  -> getStructName t
    C.TyConst t                    -> getStructName t
    C.TyNonnull t                  -> getStructName t
    C.TyStruct (C.L _ _ name)      -> Just name
    C.TyUserDefined (C.L _ _ name) -> Just name
    _                              -> Nothing

getNullability :: Text -> LinterState -> Maybe Nullability
getNullability name st =
    case Text.splitOn "->" name of
        [var] -> fst <$> Map.lookup var (typeEnv st)
        [base, member] -> do
            (_, baseTypeNodeM) <- Map.lookup base (typeEnv st)
            baseTypeNode <- baseTypeNodeM
            baseType <- case baseTypeNode of
                Fix (C.VarDecl ty _ _) -> Just ty
                _                      -> Just baseTypeNode
            structName <- getStructName baseType
            structDef <- Map.lookup structName (structDefs st)
            (memberNullability, _) <- Map.lookup member structDef
            Just memberNullability
        _ -> Nothing

isExprNonNull :: Node (Lexeme Text) -> LinterM Bool
isExprNonNull (Fix node) = case node of
    C.UnaryExpr C.UopAddress _ -> return True
    C.LiteralExpr C.String _   -> return True
    C.CastExpr _ e             -> isExprNonNull e
    C.ParenExpr e              -> isExprNonNull e
    C.VarExpr (C.L _ _ name)   -> checkVar name
    C.LiteralExpr C.ConstId (C.L _ _ name) -> checkVar name
    C.FunctionCall funcExpr _  -> do
        st <- State.get
        case getFuncName funcExpr of
            Just name -> case Map.lookup name (functionDefs st) of
                Just (retNull, _) -> return $ retNull == NonNullVar
                Nothing           -> return False
            Nothing -> return False
    _ -> do
        st <- State.get
        case exprToText (Fix node) of
            Just name -> return $ Set.member name (nonNullSet st) || getNullability name st == Just NonNullVar
            Nothing   -> return False
  where
    checkVar name = do
        st <- State.get
        let isArray = case Map.lookup name (typeEnv st) of
                Just (_, Just (Fix (C.VarDecl _ _ specs))) -> any isArraySpec specs
                _ -> False
        return $ isArray || Set.member name (nonNullSet st) || getNullability name st == Just NonNullVar

    isArraySpec (Fix (C.DeclSpecArray {})) = True
    isArraySpec _                          = False

getFuncName :: Node (Lexeme Text) -> Maybe Text
getFuncName (Fix (C.VarExpr (C.L _ _ name)))               = Just name
getFuncName (Fix (C.LiteralExpr C.ConstId (C.L _ _ name))) = Just name
getFuncName (Fix (C.ParenExpr e))                          = getFuncName e
getFuncName _                                              = Nothing

checkCondition :: Node (Lexeme Text) -> (Set Text, Set Text)
checkCondition e = go e
  where
    go n@(Fix node) = case node of
        C.BinaryExpr lhs BopNe (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) ->
            (fromText $ exprToText lhs, Set.empty)
        C.BinaryExpr (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) BopNe rhs ->
            (fromText $ exprToText rhs, Set.empty)
        C.BinaryExpr lhs BopEq (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) ->
            (Set.empty, fromText $ exprToText lhs)
        C.BinaryExpr (Fix (C.LiteralExpr C.ConstId (C.L _ _ "NULL"))) BopEq rhs ->
            (Set.empty, fromText $ exprToText rhs)
        C.BinaryExpr lhs BopNe (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) ->
            (fromText $ exprToText lhs, Set.empty)
        C.BinaryExpr (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) BopNe rhs ->
            (fromText $ exprToText rhs, Set.empty)
        C.BinaryExpr lhs BopEq (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) ->
            (Set.empty, fromText $ exprToText lhs)
        C.BinaryExpr (Fix (C.LiteralExpr _ (C.L _ _ "nullptr"))) BopEq rhs ->
            (Set.empty, fromText $ exprToText rhs)

        C.UnaryExpr UopNot inner ->
            let (thenSet, elseSet) = checkCondition inner
            in (elseSet, thenSet)

        C.BinaryExpr lhs BopAnd rhs ->
            let (then1, else1) = checkCondition lhs
                (then2, else2) = checkCondition rhs
            in (then1 `Set.union` then2, else1 `Set.union` else2)

        C.BinaryExpr lhs BopOr rhs ->
            let (then1, else1) = checkCondition lhs
                (then2, else2) = checkCondition rhs
            in (then1 `Set.union` then2, else1 `Set.union` else2)

        C.ParenExpr inner -> checkCondition inner

        C.VarExpr {} -> (fromText $ exprToText n, Set.empty)
        C.PointerAccess {} -> (fromText $ exprToText n, Set.empty)
        C.MemberAccess {} -> (fromText $ exprToText n, Set.empty)
        C.UnaryExpr C.UopDeref _ -> (fromText $ exprToText n, Set.empty)

        _ -> (Set.empty, Set.empty)

    fromText Nothing  = Set.empty
    fromText (Just t) = Set.singleton t

isTerminating :: Node (Lexeme Text) -> Bool
isTerminating = \case
    Fix (C.Return _)            -> True
    Fix C.Break                 -> True
    Fix C.Continue              -> True
    Fix (C.Goto _)              -> True
    Fix (C.FunctionCall expr _) ->
        case getFuncName expr of
            Just "abort"        -> True
            Just "exit"         -> True
            Just "LOGGER_FATAL" -> True
            _                   -> False
    Fix (C.ExprStmt e)          -> isTerminating e
    Fix (C.IfStmt _ t (Just e)) -> isTerminating t && isTerminating e
    Fix (C.CompoundStmt stmts)  -> any isTerminating stmts
    _                           -> False

analyseStmts :: [Node (Lexeme Text)] -> LinterM ()
analyseStmts = mapM_ analyseExpr

analyseExpr :: Node (Lexeme Text) -> LinterM ()
analyseExpr (Fix fixNode) = case fixNode of
    C.CastExpr toType fromExpr -> do
        st <- State.get
        let fromNameM = exprToText fromExpr
        forM_ fromNameM $ \fromName ->
            forM_ (getNullability fromName st) $ \nullability ->
                if isNonnull toType && nullability == NullableVar && not (Set.member fromName (nonNullSet st))
                then State.lift . warnDoc (currentFile st) fromExpr $
                        "expression" <+> backticks (pretty fromName)
                        <+> "is nullable and has not been checked before this cast"
                else return ()
        traverse_ analyseExpr fixNode

    C.AssignExpr lhs _ rhs -> do
        analyseExpr lhs
        analyseExpr rhs
        let lhsNameM = exprToText lhs
        rhsIsNonNull <- isExprNonNull rhs
        forM_ lhsNameM $ \lhsName ->
            if rhsIsNonNull
                then State.modify $ \s -> s { nonNullSet = Set.insert lhsName (nonNullSet s) }
                else State.modify $ \s -> s { nonNullSet = Set.delete lhsName (nonNullSet s) }

    C.BinaryExpr lhs C.BopAnd rhs -> do
        analyseExpr lhs
        let (nonNullInThen, _) = checkCondition lhs
        st <- State.get
        State.put $ st { nonNullSet = nonNullSet st `Set.union` nonNullInThen }
        analyseExpr rhs
        State.put st

    C.IfStmt condition thenBranch elseBranchM -> do
        analyseExpr condition
        let (nonNullInThen, nonNullInElse) = checkCondition condition

        initialState <- State.get

        State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInThen }
        analyseStmts' thenBranch
        let thenReturns = isTerminating thenBranch
        stateAfterThen <- State.get

        (stateAfterElse, elseReturns) <- case elseBranchM of
            Nothing -> do
                let s = initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInElse }
                return (s, False)
            Just elseBranch -> do
                State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInElse }
                analyseStmts' elseBranch
                s <- State.get
                return (s, isTerminating elseBranch)

        let finalSet = if thenReturns && not elseReturns
                         then nonNullSet stateAfterElse
                         else if not thenReturns && elseReturns
                         then nonNullSet stateAfterThen
                         else nonNullSet stateAfterThen `Set.intersection` nonNullSet stateAfterElse

        State.put $ stateAfterThen { nonNullSet = finalSet }

    C.TernaryExpr condition thenBranch elseBranch -> do
        analyseExpr condition
        let (nonNullInThen, nonNullInElse) = checkCondition condition

        initialState <- State.get

        State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInThen }
        analyseExpr thenBranch
        stateAfterThen <- State.get

        State.put $ initialState { nonNullSet = nonNullSet initialState `Set.union` nonNullInElse }
        analyseExpr elseBranch
        stateAfterElse <- State.get

        State.put $ stateAfterThen { nonNullSet = nonNullSet stateAfterThen `Set.intersection` nonNullSet stateAfterElse }

    C.VarDeclStmt decl@(Fix (C.VarDecl ty (C.L _ _ name) _)) initM -> do
        let nullability = if isNullable ty then NullableVar else NonNullVar
        State.modify $ \s -> s { typeEnv = Map.insert name (nullability, Just decl) (typeEnv s) }
        forM_ initM $ \i -> do
            analyseExpr i
            isInitNonNull <- isExprNonNull i
            if isInitNonNull
                then State.modify $ \s -> s { nonNullSet = Set.insert name (nonNullSet s) }
                else return ()

    C.FunctionCall funcExpr args -> do
        analyseExpr funcExpr
        mapM_ analyseExpr args
        st <- State.get
        case getFuncName funcExpr of
            Just "assert" -> case args of
                [arg] -> do
                    let (nonNullInThen, _) = checkCondition arg
                    State.modify $ \s -> s { nonNullSet = nonNullSet s `Set.union` nonNullInThen }
                _ -> return ()
            Just "LOGGER_ASSERT" -> case args of
                (_:arg:_) -> do
                    let (nonNullInThen, _) = checkCondition arg
                    State.modify $ \s -> s { nonNullSet = nonNullSet s `Set.union` nonNullInThen }
                _ -> return ()
            Just name ->
                case Map.lookup name (functionDefs st) of
                    Just (_, paramNullabilities) ->
                        forM_ (zip args paramNullabilities) $ \(arg, paramNullability) ->
                            when (paramNullability == NonNullVar) $
                                case exprToText arg of
                                    Just argName -> do
                                        let nullability = getNullability argName st
                                        when (nullability == Just NullableVar && not (Set.member argName (nonNullSet st))) $
                                            State.lift . warnDoc (currentFile st) arg $
                                                "expression" <+> backticks (pretty argName)
                                                <+> "is nullable and has not been checked before this call"
                                    Nothing -> return ()
                    Nothing -> return ()
            Nothing -> return ()

    _ -> traverse_ analyseExpr fixNode

analyseStmts' :: Node (Lexeme Text) -> LinterM ()
analyseStmts' (Fix (C.CompoundStmt stmts)) = analyseStmts stmts
analyseStmts' node                         = analyseExpr node

data ProgramDefs = ProgramDefs
    { programStructs   :: Map Text TypeEnv
    , programFunctions :: Map Text (Nullability, [Nullability], FilePath, Node (Lexeme Text))
    }

mergeNullability :: Nullability -> Nullability -> Maybe Nullability
mergeNullability UnspecifiedNullability x = Just x
mergeNullability x UnspecifiedNullability = Just x
mergeNullability x y                      | x == y              = Just x
mergeNullability _ _                      = Nothing

mergeFunctionNullability :: (Nullability, [Nullability]) -> (Nullability, [Nullability]) -> Maybe (Nullability, [Nullability])
mergeFunctionNullability (r1, p1) (r2, p2) = do
    r <- mergeNullability r1 r2
    p <- if length p1 == length p2
         then mapM (uncurry mergeNullability) (zip p1 p2)
         else Nothing
    return (r, p)

collectDefs :: AstActions (State (ProgramDefs, [Diagnostic CimplePos])) Text
collectDefs = astActions
    { doNode = \file node act ->
        case unFix node of
            C.Typedef (Fix (C.Struct _ members)) structName _ -> do
                let fieldEnv = Map.fromList . concatMap getFieldDecls $ members
                State.modify $ \(s, errs) -> (s { programStructs = Map.insert (C.lexemeText structName) fieldEnv (programStructs s) }, errs)
                act
            C.FunctionPrototype retType (C.L _ _ name) params -> do
                let retNullability = getNullability' retType
                let paramNullabilities = map getParamNullability params
                (s, errs) <- State.get
                case Map.lookup name (programFunctions s) of
                    Nothing ->
                        State.put (s { programFunctions = Map.insert name (retNullability, paramNullabilities, file, node) (programFunctions s) }, errs)
                    Just (oldRet, oldParams, oldFile, _) ->
                        case mergeFunctionNullability (oldRet, oldParams) (retNullability, paramNullabilities) of
                            Just (newRet, newParams) ->
                                State.put (s { programFunctions = Map.insert name (newRet, newParams, oldFile, node) (programFunctions s) }, errs)
                            Nothing -> do
                                let errs' = flip State.execState errs $
                                        warnDoc file node $ "nullability mismatch for function" <+> backticks (pretty name) <> ", conflicts with declaration at" <+> pretty oldFile
                                State.put (s, errs')
                                act
            _ -> act
    }
  where
    getFieldDecls (Fix (C.MemberDecl (Fix (C.VarDecl ty name _)) _)) =
        [(C.lexemeText name, (getNullability' ty, Just ty))]
    getFieldDecls _ = []

    getParamNullability (Fix (C.VarDecl ty _ _)) = getNullability' ty
    getParamNullability _                        = NonNullVar

linter :: ProgramDefs -> AstActions (State [Diagnostic CimplePos]) Text
linter pdefs = astActions
    { doNode = \file node act ->
        case unFix node of
            C.FunctionDefn _ proto@(Fix (C.FunctionPrototype _ (C.L _ _ name) _)) body ->
                let localParams = getParamTypes proto
                    mergedParamsInfo = case Map.lookup name (programFunctions pdefs) of
                        Just (_, mergedParams, _, _) | length mergedParams == length localParams ->
                            zipWith (\(n, (_, ty)) m -> (n, (m, ty))) localParams mergedParams
                        _ -> localParams
                    tenv = Map.fromList mergedParamsInfo
                    initialNonNulls = Map.keysSet . Map.filter (\(m, _) -> m == NonNullVar || m == UnspecifiedNullability) $ tenv
                    initialState = LinterState tenv (programStructs pdefs) (Map.map (\(r, p, _, _) -> (r, p)) (programFunctions pdefs)) initialNonNulls file
                in State.evalStateT (analyseStmts' body) initialState
            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse input =
    let initialPdefs = ProgramDefs Map.empty Map.empty
        (pdefs, globalErrs) = State.execState (traverseAst collectDefs input) (initialPdefs, [])
    in reverse . flip State.execState globalErrs . traverseAst (linter pdefs) $ input

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("nullability", Text.unlines
    [ "Warns when a `_Nullable` pointer is cast to a `_Nonnull` pointer without a null check."
    , ""
    , "**Reason:** Casting a nullable pointer to a non-null pointer without ensuring it's not"
    , "null can lead to null pointer dereferences and crashes."
    ]))
