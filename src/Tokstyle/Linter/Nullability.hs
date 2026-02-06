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

import           Control.Monad                (foldM, forM, forM_, unless, void,
                                               when)
import           Control.Monad.State.Strict   (State)
import qualified Control.Monad.State.Strict   as State
import           Data.Fix                     (Fix (..), unFix)
import           Data.Foldable                (traverse_)
import           Data.List                    (find, zip3)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (AssignOp (..), BinaryOp (..),
                                               Lexeme (..), NodeF (..),
                                               UnaryOp (..))
import qualified Language.Cimple              as C
import           Language.Cimple.Diagnostics  (CimplePos, Diagnostic (..),
                                               DiagnosticLevel (..),
                                               DiagnosticSpan (..),
                                               nodePosAndLen, warnRich)
import           Language.Cimple.Pretty       (ppNode, showNodePlain)
import           Language.Cimple.TraverseAst  (AstActions, astActions, doNode,
                                               traverseAst)
import           Prettyprinter                (parens, pretty, (<+>))
import           Tokstyle.Analysis.AccessPath
import           Tokstyle.Analysis.Dataflow   (Dataflow (..), solve)
import qualified Tokstyle.Analysis.Symbolic   as S
import           Tokstyle.Analysis.Symbolic   (lookupStore, sAddr, sBinOp, sIte,
                                               sUnaryOp, sVar)

import           Tokstyle.Cimple.Analysis.CFG (CFG, EdgeType (..), Node (..),
                                               NodeKind (..), fromFunction,
                                               getFuncName)
import           Tokstyle.Common              (backticks, functionName, warnDoc)

data Nullability
    = NullableVar
    | NonNullVar
    | UnspecifiedNullability
    deriving (Show, Eq, Ord)

type VarInfo = (Nullability, Maybe (C.Node (Lexeme Text)))

type TypeEnv = Map Text VarInfo

data LinterState = LinterState
    { typeEnv        :: TypeEnv
    , structDefs     :: Map Text TypeEnv
    , functionDefs   :: Map Text (Nullability, [(Text, Nullability)])
    , currentFile    :: FilePath
    , currentFuncRet :: Nullability
    }

type LinterM = State.StateT LinterState (State [Diagnostic CimplePos])

isNullable :: C.Node (Lexeme Text) -> Bool
isNullable (Fix node) = case node of
    C.TyNullable _      -> True
    C.TyPointer t       -> isNullable t
    C.TyConst t         -> isNullable t
    C.TyForce t         -> isNullable t
    C.VarDecl ty _ specs ->
        if any isArraySpec specs
        then any isNullable specs
        else isNullable ty
    C.DeclSpecArray C.Nullable _ -> True
    _                   -> False

isNonnull :: C.Node (Lexeme Text) -> Bool
isNonnull (Fix node) = case node of
    C.TyNonnull _       -> True
    C.TyPointer t       -> isNonnull t
    C.TyConst t         -> isNonnull t
    C.TyForce t         -> isNonnull t
    C.VarDecl ty _ specs ->
        if any isArraySpec specs
        then any isNonnull specs
        else isNonnull ty
    C.DeclSpecArray C.Nonnull _ -> True
    _                   -> False

isPointerType :: C.Node (Lexeme Text) -> Bool
isPointerType (Fix node) = case node of
    C.TyPointer _        -> True
    C.TyNullable t       -> isPointerType t
    C.TyNonnull t        -> isPointerType t
    C.TyConst t          -> isPointerType t
    C.TyForce t          -> isPointerType t
    C.VarDecl ty _ specs -> isPointerType ty || any isArraySpec specs
    C.TyStd _            -> False
    C.TyStruct _         -> False
    C.TyUnion _          -> False
    C.TyUserDefined _    -> True -- Assume pointers can be hidden in typedefs if we don't know
    C.DeclSpecArray {}   -> True
    _                    -> False

exprToPath :: C.Node (Lexeme Text) -> Maybe AccessPath
exprToPath (Fix node) = case node of
    C.VarExpr (C.L _ _ name) -> Just $ PathVar (Text.unpack name)
    C.PointerAccess e (C.L _ _ member) -> PathField <$> exprToPath e <*> pure (Text.unpack member)
    C.MemberAccess e (C.L _ _ member) -> PathField <$> exprToPath e <*> pure (Text.unpack member)
    C.ArrayAccess e idx -> PathIndex <$> exprToPath e <*> pure (Text.unpack $ showNodePlain idx)
    C.UnaryExpr C.UopDeref e -> PathDeref <$> exprToPath e
    C.ParenExpr e -> exprToPath e
    _ -> Nothing

getParamTypes :: C.Node (Lexeme Text) -> [(Text, VarInfo)]
getParamTypes (Fix (C.FunctionPrototype _ _ params)) = concatMap getVarDecls $ params
  where
    getVarDecls decl@(Fix (C.VarDecl ty (C.L _ _ name) _)) =
        [(name, (getNullability' ty, Just decl))]
    getVarDecls _ = []
getParamTypes _ = []

getNullability' :: C.Node (Lexeme Text) -> Nullability
getNullability' ty
  | isNullable ty = NullableVar
  | isNonnull ty  = NonNullVar
  | otherwise     = UnspecifiedNullability

getStructName :: C.Node (Lexeme Text) -> Maybe Text
getStructName (Fix node) = case node of
    C.VarDecl ty _ _               -> getStructName ty
    C.TyPointer t                  -> getStructName t
    C.TyConst t                    -> getStructName t
    C.TyNonnull t                  -> getStructName t
    C.TyNullable t                 -> getStructName t
    C.TyStruct (C.L _ _ name)      -> Just name
    C.TyUserDefined (C.L _ _ name) -> Just name
    _                              -> Nothing

getDeclaredNullability :: AccessPath -> LinterState -> Nullability
getDeclaredNullability path st = fromMaybe UnspecifiedNullability $ case path of
    PathVar var -> fst <$> Map.lookup (Text.pack var) (typeEnv st)
    PathField base member -> do
        baseType <- getDeclaredType base st
        case baseType of
            Fix (C.VarDecl ty _ specs) | any isArraySpec specs ->
                return $ getNullability' ty
            baseType' -> do
                structName <- getStructName baseType'
                structDef <- Map.lookup structName (structDefs st)
                fst <$> Map.lookup (Text.pack member) structDef
    PathIndex base _ -> do
        baseType <- getDeclaredType base st
        case baseType of
            Fix (C.VarDecl ty _ _) -> return $ getNullability' ty
            _                      -> Nothing
    _ -> Nothing

getDeclaredType :: AccessPath -> LinterState -> Maybe (C.Node (Lexeme Text))
getDeclaredType path st = case path of
    PathVar name -> snd =<< Map.lookup (Text.pack name) (typeEnv st)
    PathField base member -> do
        baseType <- getDeclaredType base st
        case baseType of
            Fix (C.VarDecl ty _ specs) | any isArraySpec specs ->
                Just ty
            baseType' -> do
                structName <- getStructName baseType'
                structDef <- Map.lookup structName (structDefs st)
                snd =<< Map.lookup (Text.pack member) structDef
    PathIndex base _ -> do
        baseType <- getDeclaredType base st
        case baseType of
            Fix (C.VarDecl ty _ _) -> Just ty
            _                      -> Nothing
    _ -> Nothing

isArraySpec :: C.Node (Lexeme Text) -> Bool
isArraySpec (Fix (C.DeclSpecArray {})) = True
isArraySpec _                          = False

evaluate :: C.Node (Lexeme Text) -> S.SState -> S.SVal
evaluate expr@(Fix node) st = case node of
    C.LiteralExpr C.ConstId (C.L _ _ "NULL") -> S.SNull
    C.LiteralExpr _ (C.L _ _ "nullptr")      -> S.SNull
    C.LiteralExpr C.Int (C.L _ _ "0")        -> S.SNull
    C.LiteralExpr C.String _                 -> sAddr (PathVar "<string>")
    C.UnaryExpr C.UopAddress e               -> case exprToPath e of
        Just p  -> sAddr p
        Nothing -> S.STop
    C.ParenExpr e                            -> evaluate e st
    C.CastExpr _ e                           -> evaluate e st
    C.TernaryExpr cond thenBranch elseBranch ->
        let c = evaluate cond st
            v1 = evaluate thenBranch st
            v2 = evaluate elseBranch st
        in if v1 == v2 then v1 else sIte c v1 v2
    C.BinaryExpr lhs op rhs                  ->
        let v1 = evaluate lhs st
            v2 = evaluate rhs st
        in sBinOp op v1 v2
    C.UnaryExpr op e                         ->
        let v = evaluate e st
        in sUnaryOp op v
    _ -> case exprToPath expr of
        Just path -> fromMaybe (sVar path) (lookupStore path st)
        Nothing   -> S.STop

nullabilityProblem :: LinterState -> Dataflow Node EdgeType S.SState
nullabilityProblem lst = Dataflow
    { transfer     = transferFunc
    , edgeTransfer = edgeTransferFunc
    , merge        = S.merge (isDeclNonNull lst) Nothing
    , initial      = S.emptyState
    }
  where
    transferFunc (Node _ nk) s = case nk of
        StmtNode stmt -> transferStmt stmt s
        _             -> s

    transferStmt (Fix node) s = case node of
        C.ExprStmt e -> transferStmt e s
        C.VarDeclStmt (Fix (C.VarDecl ty (C.L _ _ name) _)) (Just i) ->
            let v = evaluate i s
                path = PathVar (Text.unpack name)
            in if isPointerType ty then S.assign path v s else s
        C.VarDeclStmt (Fix (C.VarDecl _ (C.L _ _ name) _)) Nothing ->
            let path = PathVar (Text.unpack name)
            in S.assign path (sVar path) s
        C.AssignExpr lhs AopEq rhs ->
            let v = evaluate rhs s
            in case exprToPath lhs of
                Just path -> S.assign path v s
                Nothing   -> s
        C.FunctionCall funcExpr args ->
            let s' = foldl (flip transferStmt) s (funcExpr : args)
            in case getFuncName funcExpr of
                Just "assert" -> case args of
                    [arg] -> S.addConstraint (S.SBool (evaluate arg s')) s'
                    _     -> s'
                Just "LOGGER_ASSERT" -> case args of
                    (_:arg:_) -> S.addConstraint (S.SBool (evaluate arg s')) s'
                    _         -> s'
                _ -> s'
        _ -> foldl (flip transferStmt) s (unFix (Fix node))

    edgeTransferFunc (Node _ (BranchNode cond)) branch s =
        let v = evaluate cond s
            constraint = case branch of
                TrueBranch  -> S.SBool v
                FalseBranch -> S.negateConstraint (S.SBool v)
                _           -> S.SEquals S.STop S.STop -- No-op
        in S.addConstraint constraint s
    edgeTransferFunc _ _ s = s

collectTypeEnv :: C.Node (Lexeme Text) -> TypeEnv
collectTypeEnv = flip State.execState Map.empty . traverseAst actions
  where
    actions = astActions
        { doNode = \_ node act -> case unFix node of
            C.VarDecl _ (C.L _ _ name) _ -> do
                let nullability = getNullability' node
                State.modify $ Map.insert name (nullability, Just node)
                act
            _ -> act
        }

isDeclNonNull :: LinterState -> S.SVal -> Bool
isDeclNonNull lst = \case
    S.SVar path -> getDeclaredNullability path lst /= NullableVar
    S.SAddr _   -> True
    S.SBinOp op v1 _ | op `elem` [BopPlus, BopMinus] -> isDeclNonNull lst v1
    _           -> False

analyseExpr :: S.SState -> C.Node (Lexeme Text) -> LinterM S.SState
analyseExpr st expr@(Fix fixNode) = case fixNode of
    C.TernaryExpr cond thenBranch elseBranch -> do
        st' <- analyseExpr st cond
        let v = evaluate cond st'
        lst <- State.get
        stThen <- analyseExpr (S.addConstraint (S.SBool v) st') thenBranch
        stElse <- analyseExpr (S.addConstraint (S.negateConstraint (S.SBool v)) st') elseBranch
        return $ S.merge (isDeclNonNull lst) (Just v) stThen stElse

    C.BinaryExpr lhs BopAnd rhs -> do
        st' <- analyseExpr st lhs
        let v = evaluate lhs st'
        analyseExpr (S.addConstraint (S.SBool v) st') rhs

    C.BinaryExpr lhs BopOr rhs -> do
        st' <- analyseExpr st lhs
        let v = evaluate lhs st'
        analyseExpr (S.addConstraint (S.negateConstraint (S.SBool v)) st') rhs

    C.CastExpr toType fromExpr -> do
        st' <- analyseExpr st fromExpr
        lst <- State.get
        let v = evaluate fromExpr st'
        when (isNonnull toType && S.canBeNull (isDeclNonNull lst) v st') $
             State.lift . warnDoc (currentFile lst) fromExpr $
                 "expression" <+> backticks (ppNode fromExpr)
                 <+> "is nullable and has not been checked before this cast"
        -- After a non-null cast, assume it's non-null in this branch
        return $ if isNonnull toType then S.addConstraint (S.SNotEquals v S.SNull) st' else st'

    C.PointerAccess e _ -> do
        st' <- analyseExpr st e
        lst <- State.get
        let v = evaluate e st'
        when (S.canBeNull (isDeclNonNull lst) v st') $
            State.lift . warnDoc (currentFile lst) e $
                "pointer" <+> backticks (ppNode e)
                <+> "is nullable and has not been checked before this access"
        -- After access, it must have been non-null
        return $ S.addConstraint (S.SNotEquals v S.SNull) st'

    C.ArrayAccess e _ -> do
        st' <- analyseExpr st e
        lst <- State.get
        let v = evaluate e st'
        when (not (isDeclNonNull lst v) && S.canBeNull (isDeclNonNull lst) v st') $
            State.lift . warnDoc (currentFile lst) e $
                "pointer" <+> backticks (ppNode e)
                <+> "is nullable and has not been checked before this access"
        return $ S.addConstraint (S.SNotEquals v S.SNull) st'

    C.UnaryExpr C.UopDeref e -> do
        st' <- analyseExpr st e
        lst <- State.get
        let v = evaluate e st'
        when (S.canBeNull (isDeclNonNull lst) v st') $
            State.lift . warnDoc (currentFile lst) e $
                "pointer" <+> backticks (ppNode e)
                <+> "is nullable and has not been checked before this dereference"
        return $ S.addConstraint (S.SNotEquals v S.SNull) st'

    C.FunctionCall funcExpr args -> do
        st' <- foldM analyseExpr st (funcExpr : args)
        lst <- State.get
        let mFuncInfo = case getFuncName funcExpr of
                Just name -> Map.lookup name (functionDefs lst)
                Nothing   -> Nothing

        let mFuncInfo' = case mFuncInfo of
                Just info -> Just info
                Nothing -> case exprToPath funcExpr of
                    Just path -> case getDeclaredType path lst of
                        Just (Fix (C.VarDecl ty _ _)) ->
                            let getTypeName t = case unFix t of
                                    C.TyUserDefined name -> Just (C.lexemeText name)
                                    C.TyFunc name -> Just (C.lexemeText name)
                                    C.TyPointer t' -> getTypeName t'
                                    C.TyConst t' -> getTypeName t'
                                    _ -> Nothing
                            in case getTypeName ty of
                                Just name -> Map.lookup name (functionDefs lst)
                                Nothing   -> Nothing
                        _ -> Nothing
                    Nothing -> Nothing

        case mFuncInfo' of
            Just (_, paramNullabilities) ->
                forM_ (zip args paramNullabilities) $ \(arg, (_, paramNullability)) ->
                    let v = evaluate arg st'
                        -- Don't warn again if it's a non-null cast (CastExpr already warned)
                        isCastToNonnull = case unFix arg of
                            C.CastExpr ty _ -> isNonnull ty
                            _               -> False
                    in when (paramNullability == NonNullVar && not isCastToNonnull && S.canBeNull (isDeclNonNull lst) v st') $
                        State.lift . warnDoc (currentFile lst) arg $
                            "expression" <+> backticks (ppNode arg)
                            <+> "is nullable and has not been checked before this call"
            Nothing -> return ()

        let st'' = case getFuncName funcExpr of
                Just "assert" -> case args of
                    [arg] -> S.addConstraint (S.SBool (evaluate arg st')) st'
                    _     -> st'
                Just "LOGGER_ASSERT" -> case args of
                    (_:arg:_) -> S.addConstraint (S.SBool (evaluate arg st')) st'
                    _         -> st'
                _ -> st'
        return st''

    C.AssignExpr lhs op rhs -> do
        st' <- analyseExpr st lhs
        st'' <- analyseExpr st' rhs
        lst <- State.get
        case (op, exprToPath lhs) of
            (AopEq, Just lhsPath) -> do
                let lhsNullability = getDeclaredNullability lhsPath lst
                    v = evaluate rhs st''
                when (lhsNullability == NonNullVar && S.canBeNull (isDeclNonNull lst) v st'') $
                    State.lift . warnDoc (currentFile lst) rhs $
                        "expression" <+> backticks (ppNode rhs)
                        <+> "is nullable and has not been checked before this assignment"
                return $ S.assign lhsPath v st''
            _ -> return st''

    C.BinaryExpr lhs op rhs | op `elem` [BopPlus, BopMinus] -> do
        st' <- analyseExpr st lhs
        st'' <- analyseExpr st' rhs
        lst <- State.get
        let check e s = case (exprToPath e, exprToPath e >>= (`getDeclaredType` lst)) of
                (Just _, Just ty) | isPointerType ty ->
                    let v = evaluate e s
                    in when (S.canBeNull (isDeclNonNull lst) v s) $
                        State.lift . warnDoc (currentFile lst) e $
                            "pointer" <+> backticks (ppNode e)
                            <+> "is nullable and has not been checked before this arithmetic"
                _ -> return ()
        check lhs st''
        check rhs st''
        return st''

    _ -> foldM analyseExpr st (unFix expr)

data ProgramDefs = ProgramDefs
    { programStructs   :: Map Text TypeEnv
    , programFunctions :: Map Text (Nullability, [(Text, Nullability)], FilePath, C.Node (Lexeme Text))
    , programGlobals   :: TypeEnv
    , programTypedefs  :: Map Text (Nullability, [(Text, Nullability)])
    }

mergeNullability :: Nullability -> Nullability -> Maybe Nullability
mergeNullability UnspecifiedNullability x = Just x
mergeNullability x UnspecifiedNullability = Just x
mergeNullability x y                      | x == y              = Just x
mergeNullability _ _                      = Nothing

data Mismatch
    = RetMismatch Nullability Nullability
    | ParamMismatch Int Text Nullability Nullability
    | ParamCountMismatch Int Int

mergeFunctionInfos :: (Nullability, [(Text, Nullability)]) -> (Nullability, [(Text, Nullability)]) -> Either Mismatch (Nullability, [(Text, Nullability)])
mergeFunctionInfos (r1, p1) (r2, p2) = do
    r <- maybe (Left $ RetMismatch r1 r2) Right $ mergeNullability r1 r2
    p <- if length p1 == length p2
         then forM (zip3 [0..] p1 p2) $ \(i, (n1, v1), (_, v2)) ->
            case mergeNullability v1 v2 of
                Just v  -> Right (n1, v)
                Nothing -> Left $ ParamMismatch i n1 v1 v2
         else Left $ ParamCountMismatch (length p1) (length p2)
    return (r, p)

getParams :: C.Node (Lexeme Text) -> [C.Node (Lexeme Text)]
getParams (Fix (C.FunctionPrototype _ _ ps)) = ps
getParams _                                  = []

collectDefs :: AstActions (State (ProgramDefs, [Diagnostic CimplePos])) Text
collectDefs = astActions
    { doNode = \file node act ->
        case unFix node of
            C.Typedef (Fix (C.Struct _ members)) structName _ -> do
                let fieldEnv = Map.fromList . concatMap getFieldDecls $ members
                State.modify $ \(s, errs) -> (s { programStructs = Map.insert (C.lexemeText structName) fieldEnv (programStructs s) }, errs)
                act
            C.TypedefFunction proto -> do
                case functionName proto of
                    Just name -> do
                        let (Fix (C.FunctionPrototype retType _ params)) = proto
                        let retNullability = getNullability' retType
                        let paramInfos = map getParamInfo params
                        State.modify $ \(s, errs) -> (s { programTypedefs = Map.insert name (retNullability, paramInfos) (programTypedefs s) }, errs)
                    Nothing -> return ()
                act
            C.FunctionPrototype retType (C.L _ _ name) params -> do
                let retNullability = getNullability' retType
                let currentParamInfos = map getParamInfo params
                (s, errs) <- State.get
                case Map.lookup name (programFunctions s) of
                    Nothing ->
                        State.put (s { programFunctions = Map.insert name (retNullability, currentParamInfos, file, node) (programFunctions s) }, errs)
                    Just (oldRet, oldParamInfos, oldFile, oldNode) ->
                        case mergeFunctionInfos (oldRet, oldParamInfos) (retNullability, currentParamInfos) of
                            Right (newRet, newParamInfos) ->
                                State.put (s { programFunctions = Map.insert name (newRet, newParamInfos, oldFile, node) (programFunctions s) }, errs)
                            Left mismatch -> do
                                let (pos, len) = nodePosAndLen file node
                                let diag = case mismatch of
                                        RetMismatch _ _ ->
                                            Diagnostic pos len WarningLevel
                                                ("nullability mismatch for return type of function" <+> backticks (pretty name))
                                                (Just "nullability")
                                                [ DiagnosticSpan (fst $ nodePosAndLen oldFile oldNode) (snd $ nodePosAndLen oldFile oldNode) [ "conflict with declaration here" ]
                                                , DiagnosticSpan pos len [ "found mismatch here" ]
                                                ]
                                                []
                                        ParamMismatch i pname _ _ ->
                                            let oldParam = getParams oldNode !! i
                                                newParam = params !! i
                                                (oldPos, oldLen) = nodePosAndLen oldFile oldParam
                                                (newPos, newLen) = nodePosAndLen file newParam
                                            in Diagnostic newPos newLen WarningLevel
                                                ("nullability mismatch for parameter" <+> pretty (i + 1) <+> parens (backticks (pretty pname)) <+> "of function" <+> backticks (pretty name))
                                                (Just "nullability")
                                                [ DiagnosticSpan oldPos oldLen [ "conflict with declaration here" ]
                                                , DiagnosticSpan newPos newLen [ "found mismatch here" ]
                                                ]
                                                []
                                        ParamCountMismatch _ _ ->
                                            Diagnostic pos len WarningLevel
                                                ("parameter count mismatch for function" <+> backticks (pretty name))
                                                (Just "nullability")
                                                [ DiagnosticSpan (fst $ nodePosAndLen oldFile oldNode) (snd $ nodePosAndLen oldFile oldNode) [ "conflict with declaration here" ]
                                                , DiagnosticSpan pos len [ "found mismatch here" ]
                                                ]
                                                []
                                State.put (s, diag : errs)
                                act
            C.ConstDecl ty (C.L _ _ name) -> do
                State.modify $ \(s, errs) -> (s { programGlobals = Map.insert name (getNullability' ty, Just ty) (programGlobals s) }, errs)
                act
            C.ConstDefn _ ty (C.L _ _ name) _ -> do
                State.modify $ \(s, errs) -> (s { programGlobals = Map.insert name (getNullability' ty, Just ty) (programGlobals s) }, errs)
                act
            _ -> act
    }
  where
    getFieldDecls (Fix (C.MemberDecl (Fix (C.VarDecl ty name _)) _)) =
        [(C.lexemeText name, (getNullability' ty, Just ty))]
    getFieldDecls _ = []

    getParamInfo node@(Fix (C.VarDecl {})) =
        let (C.VarDecl _ (C.L _ _ name) _) = unFix node
        in (name, getNullability' node)
    getParamInfo _ = ("", NonNullVar)

runAnalysis :: C.Node (Lexeme Text) -> LinterM ()
runAnalysis defn@(Fix (C.FunctionDefn _ proto body)) = do
    st <- State.get
    let localDecls = collectTypeEnv defn
    -- Preserve merged param info from declarations; localDecls from the definition
    -- might have fewer annotations. Map.union is left-biased, so we want (typeEnv st) first.
    let tenv = Map.union (typeEnv st) localDecls
    let st' = st { typeEnv = tenv }
    let (entry, cfg) = fromFunction defn
    let problem = nullabilityProblem st'
    let results = solve entry cfg problem

    forM_ (Map.toList results) $ \(node, s) ->
        case node of
            Node _ (StmtNode stmt)   -> State.withStateT (\_ -> st') $ void $ analyseAtomicStmt s stmt
            Node _ (BranchNode cond) -> State.withStateT (\_ -> st') $ void $ analyseExpr s cond
            _ -> return ()
runAnalysis _ = return ()

analyseAtomicStmt :: S.SState -> C.Node (Lexeme Text) -> LinterM S.SState
analyseAtomicStmt s (Fix node) = case node of
    C.ExprStmt e -> analyseExpr s e
    C.VarDeclStmt (Fix (C.VarDecl ty (C.L _ _ name) _)) (Just i) -> do
        s' <- analyseExpr s i
        lst <- State.get
        let v = evaluate i s'
            path = PathVar (Text.unpack name)
        when (getNullability' ty == NonNullVar && S.canBeNull (isDeclNonNull lst) v s') $
            State.lift . warnDoc (currentFile lst) i $
                "expression" <+> backticks (ppNode i)
                <+> "is nullable and has not been checked before this assignment"
        return $ if isPointerType ty then S.assign path v s' else s'
    C.Return (Just e) -> do
        s' <- analyseExpr s e
        lst <- State.get
        let v = evaluate e s'
        when (currentFuncRet lst == NonNullVar && S.canBeNull (isDeclNonNull lst) v s') $
            State.lift . warnDoc (currentFile lst) e $
                "expression" <+> backticks (ppNode e)
                <+> "is nullable and has not been checked before this return"
        return s'
    C.IfStmt cond _ _ -> analyseExpr s cond
    C.WhileStmt cond _ -> analyseExpr s cond
    C.DoWhileStmt _ cond -> analyseExpr s cond
    C.ForStmt init' cond step _ -> do
        s' <- analyseExpr s init'
        s'' <- analyseExpr s' cond
        analyseExpr s'' step
    C.SwitchStmt expr _ -> analyseExpr s expr
    C.Case expr _ -> analyseExpr s expr
    C.Label _ _ -> return s -- Body analyzed as its own node
    C.PreprocIf cond _ _ -> analyseExpr s cond
    C.PreprocIfdef _ _ _ -> return s -- Identifier is atomic
    C.PreprocIfndef _ _ _ -> return s -- Identifier is atomic
    C.PreprocElif cond _ _ -> analyseExpr s cond
    _ -> return s -- Other nodes don't contain expressions to check

linter :: ProgramDefs -> AstActions (State [Diagnostic CimplePos]) Text
linter pdefs = astActions
    { doNode = \file node act ->
        case unFix node of
            C.FunctionDefn _ proto@(Fix (C.FunctionPrototype retType (C.L _ _ name) _)) _ ->
                let localParams = getParamTypes proto
                    (retNull, mergedParamsInfo) = case Map.lookup name (programFunctions pdefs) of
                        Just (r, mergedParams, _, _) | length mergedParams == length localParams ->
                            (r, zipWith (\(n, (_, ty)) (_, m) -> (n, (m, ty))) localParams mergedParams)
                        _ -> (getNullability' retType, localParams)
                    tenv = Map.union (Map.fromList mergedParamsInfo) (programGlobals pdefs)
                    fDefs = Map.union (Map.map (\(r, p, _, _) -> (r, p)) (programFunctions pdefs)) (programTypedefs pdefs)
                    initialState = LinterState tenv (programStructs pdefs) fDefs file retNull
                in State.evalStateT (runAnalysis node) initialState
            _ -> act
    }

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse input =
    let initialPdefs = ProgramDefs Map.empty Map.empty Map.empty Map.empty
        (pdefs, globalErrs) = State.execState (traverseAst collectDefs input) (initialPdefs, [])
    in reverse . flip State.execState globalErrs . traverseAst (linter pdefs) $ input

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("nullability", Text.unlines
    [ "Warns when a `_Nullable` pointer is cast to a `_Nonnull` pointer or dereferenced without a null check."
    , ""
    , "**Reason:** Casting a nullable pointer to a non-null pointer or dereferencing it without ensuring it's not"
    , "null can lead to null pointer dereferences and crashes."
    ]))
