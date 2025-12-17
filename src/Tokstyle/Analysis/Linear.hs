{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Tokstyle.Analysis.Linear
    ( Ownership (..)
    , Path (..)
    , LinearFacts (..)
    , AnalysisState (..)
    , ScopedNode
    , ScopedTranslationUnit
    , isPathOwned
    , joinOwnership
    , collectInfo
    , propagateChecked
    , buildCFG
    , fixpoint
    , LinearContext (..)
    , toPath
    , getExprOwnership
    , isExprOwned
    , handleStmt
    , handleExpr
    , handleMove
    , isOwner
    , hasOwned
    , cimpleToTypeInfo
    , isConstBorrow
    , isPathMoved
    , showPath
    ) where

import           Control.Monad               (forM_, mplus, unless, when)
import           Control.Monad.State.Strict
import           Data.Fix                    (Fix (..), unFix)
import           Data.List                   (find, foldl', partition)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (fromMaybe, mapMaybe)
import           Data.Set                    (Set, (\\))
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Debug.Trace                 (trace)
import           Language.Cimple             (Lexeme (..), Node, NodeF (..), getLoc)
import qualified Language.Cimple             as C
import           Language.Cimple.TraverseAst (astActions, doNode, traverseAst)
import           Language.Cimple.Analysis.DataFlow  (CFG, CFGNode (..), DataFlow (..), buildCFG, fixpoint)
import           Language.Cimple.Analysis.Scope     (ScopedId (..), dummyScopedId, sidName)
import           Tokstyle.Common.TypeSystem  (StdType (..), TypeDescr (..), TypeInfo (..), TypeRef (..), TypeSystem, lookupType, getTypeRefName, resolveRef)

debugging :: Bool
debugging = False

dtrace :: String -> a -> a
dtrace msg x = if debugging then trace msg x else x

-- | Ownership state of a variable.
data Ownership
    = Uninit
    | Owned
    | Null
    | BorrowedMut
    | BorrowedConst
    | Moved
    | NullChecked Text
    | Invalid Text -- ^ Invalidated borrow, carries name of original owner
    deriving (Eq, Ord, Show)

-- | A path to a resource (variable or struct member).
-- The second element of the tuple is True if it's a pointer access (->).
data Path = Path ScopedId [(Text, Bool)]
    deriving (Eq, Ord, Show)

-- | Facts for data flow analysis.
data LinearFacts = LinearFacts
    { varStates   :: !(Map Path Ownership)
    , varTypes    :: !(Map Path Bool) -- ^ True if variable has 'owned' qualifier
    , borrows     :: !(Map Path (Set Path)) -- ^ Variable -> Set of variables it borrows from
    , borrowLocs  :: !(Map Path (Lexeme ScopedId)) -- ^ Variable -> Location where it was borrowed
    , moveLocs    :: !(Map Path (Lexeme ScopedId)) -- ^ Variable -> Location where it was moved
    , warnings    :: !(Set (Text, Lexeme ScopedId))
    } deriving (Eq, Show)

type ScopedNode = Node (C.Lexeme ScopedId)
type ScopedTranslationUnit = (FilePath, [ScopedNode])

-- | Analysis state for global analysis.
data AnalysisState = AnalysisState
    { currentFile            :: !FilePath
    , checkedFuncs           :: !(Set ScopedId)
    , inherentlyCheckedFuncs :: !(Set ScopedId)
    , funcParams             :: !(Map Text [Bool]) -- ^ True if parameter is 'owned'
    , funcReturns            :: !(Map Text Bool)   -- ^ True if return value is 'owned'
    , callGraph              :: !(Map ScopedId (Set ScopedId))
    , reverseCallGraph       :: !(Map ScopedId (Set ScopedId))
    , typeSystem             :: !TypeSystem
    , varTypesInfo           :: !(Map ScopedId Text)   -- ^ Variable -> Type name (for struct/union)
    , varTypesFull           :: !(Map ScopedId TypeInfo) -- ^ Variable -> Full TypeInfo
    }

-- | Check if a type info represents an immutable borrow.
isConstBorrow :: TypeInfo -> Bool
isConstBorrow = \case
    Pointer (Const _) -> True
    Pointer t         -> isConstBorrow t
    Const t           -> isConstBorrow t
    Owner t           -> isConstBorrow t
    Nonnull t         -> isConstBorrow t
    Nullable t        -> isConstBorrow t
    _                 -> False

isCopy :: TypeInfo -> Bool
isCopy (BuiltinType _) = True
isCopy (Const t)       = isCopy t
isCopy _               = False

-- | Convert a Cimple type node to TypeInfo.
cimpleToTypeInfo :: Node (C.Lexeme ScopedId) -> TypeInfo
cimpleToTypeInfo (Fix node) = case node of
    C.TyConst t     -> Const (cimpleToTypeInfo t)
    C.TyOwner t     -> Owner (cimpleToTypeInfo t)
    C.TyNonnull t   -> Nonnull (cimpleToTypeInfo t)
    C.TyNullable t  -> Nullable (cimpleToTypeInfo t)
    C.TyPointer t   -> Pointer (cimpleToTypeInfo t)
    C.TyStruct (L _ _ sid) -> TypeRef StructRef (L (C.AlexPn 0 0 0) C.IdVar (sidName sid))
    C.TyUnion (L _ _ sid)  -> TypeRef UnionRef (L (C.AlexPn 0 0 0) C.IdVar (sidName sid))
    C.TyFunc (L _ _ sid)   -> TypeRef FuncRef (L (C.AlexPn 0 0 0) C.IdVar (sidName sid))
    C.TyUserDefined (L _ _ sid) -> TypeRef UnresolvedRef (L (C.AlexPn 0 0 0) C.IdVar (sidName sid))
    C.TyStd (L _ _ name) -> BuiltinType $ case sidName name of
        "void" -> VoidTy
        "bool" -> BoolTy
        "char" -> CharTy
        "uint8_t" -> U08Ty
        "int8_t"  -> S08Ty
        "uint16_t" -> U16Ty
        "int16_t"  -> S16Ty
        "uint32_t" -> U32Ty
        "int32_t"  -> S32Ty
        "uint64_t" -> U64Ty
        "int64_t"  -> S64Ty
        "size_t"   -> SizeTy
        _      -> S32Ty
    _ -> BuiltinType VoidTy

isOwner :: Node (C.Lexeme l) -> Bool
isOwner (Fix node) = case node of
    C.TyOwner _    -> True
    C.TyConst t    -> isOwner t
    C.TyNonnull t  -> isOwner t
    C.TyNullable t -> isOwner t
    _              -> False

-- | Check if a type contains an 'owned' qualifier.
hasOwned :: Node (C.Lexeme l) -> Bool
hasOwned (Fix node) = case node of
    C.TyOwner _    -> True
    C.TyPointer t  -> hasOwned t
    C.TyConst t    -> hasOwned t
    C.TyNullable t -> hasOwned t
    C.TyNonnull t  -> hasOwned t
    _              -> False

-- | Get the name of a type (for struct/union lookup).
getTypeName :: Node (C.Lexeme ScopedId) -> Maybe Text
getTypeName (Fix node) = case node of
    C.TyUserDefined (L _ _ sid) -> Just (sidName sid)
    C.TyStruct (L _ _ sid)      -> Just (sidName sid)
    C.TyUnion (L _ _ sid)       -> Just (sidName sid)
    C.TyPointer t               -> getTypeName t
    C.TyConst t                 -> getTypeName t
    C.TyOwner t                 -> getTypeName t
    C.TyNonnull t               -> getTypeName t
    C.TyNullable t              -> getTypeName t
    _                           -> Nothing

-- | Check if a path is qualified as 'owner'.
isPathOwned :: AnalysisState -> LinearFacts -> Path -> Bool
isPathOwned info facts p = dtrace ("isPathOwned: path=" ++ show p) $
    Map.findWithDefault False p (varTypes facts) ||
    case getPathInfo info p of
        Just ty -> dtrace ("  type=" ++ show ty) $ isTypeInfoOwner ty
        Nothing -> dtrace "  type=Nothing" False
  where
    isTypeInfoOwner = \case
        Owner _    -> True
        Nonnull t  -> isTypeInfoOwner t
        Nullable t -> isTypeInfoOwner t
        Const t    -> isTypeInfoOwner t
        _          -> False

-- | Get TypeInfo for a path.
getPathInfo :: AnalysisState -> Path -> Maybe TypeInfo
getPathInfo info (Path sid members) = dtrace ("getPathInfo: sid=" ++ show sid ++ " members=" ++ show members) $
    case members of
        [] -> Map.lookup sid (varTypesFull info)
        _  -> checkMember 0 (Map.lookup sid (varTypesInfo info)) members
  where
    checkMember :: Int -> Maybe Text -> [(Text, Bool)] -> Maybe TypeInfo
    checkMember depth mTypeName ms' = dtrace ("    checkMember: depth=" ++ show depth ++ " type=" ++ show mTypeName ++ " ms=" ++ show ms') $
        case (mTypeName, ms') of
            (Just typeName, (m, isPtr):ms) ->
                if m == "*"
                then Nothing -- Should be handled in checkType
                else case lookupType typeName (typeSystem info) of
                    Just (StructDescr _ fields) -> dtrace ("      found struct " ++ show typeName ++ " fields=" ++ show (map (sidName . dummyScopedId . C.lexemeText . fst) fields)) $
                        case find (\(L _ _ name, _) -> name == m) fields of
                            Just (_, info') -> checkType (depth + 1) ms info'
                            Nothing -> Nothing
                    Just (UnionDescr _ fields) -> dtrace ("      found union " ++ show typeName ++ " fields=" ++ show (map (sidName . dummyScopedId . C.lexemeText . fst) fields)) $
                        case find (\(L _ _ name, _) -> name == m) fields of
                            Just (_, info') -> checkType (depth + 1) ms info'
                            Nothing -> Nothing
                    Just (AliasDescr _ target) ->
                        case getTypeRefName target of
                            Just next -> checkMember (depth + 1) (Just next) ((m, isPtr):ms)
                            Nothing   -> Nothing
                    _ -> dtrace ("      type " ++ show typeName ++ " not found or not aggregate") Nothing
            _ -> Nothing

    checkType :: Int -> [(Text, Bool)] -> TypeInfo -> Maybe TypeInfo
    checkType depth ms' t = dtrace ("    checkType: depth=" ++ show depth ++ " ms=" ++ show ms' ++ " type=" ++ show t) $
        case t of
                _ | null ms' -> Just t
                Owner t' -> checkType (depth + 1) ms' t'
                Pointer t' -> case ms' of
                    (("*", _):rest) -> checkType (depth + 1) rest t'
                    _ -> checkType (depth + 1) ms' t'
                Const t' -> checkType (depth + 1) ms' t'
                Nonnull t' -> checkType (depth + 1) ms' t'
                Nullable t' -> checkType (depth + 1) ms' t'
                Sized t' _ -> checkType (depth + 1) ms' t'
                Array (Just t') _ -> checkType (depth + 1) ms' t'
                TypeRef _ (L _ _ typeName) -> checkMember (depth + 1) (Just typeName) ms'
                Var _ t' -> checkType (depth + 1) ms' t'
                _ -> Nothing

-- | Check if a function uses any linear type features.
usesLinearFeatures :: AnalysisState -> Node (C.Lexeme ScopedId) -> Node (C.Lexeme ScopedId) -> [Node (C.Lexeme ScopedId)] -> Bool
usesLinearFeatures info retType body params =
    hasOwned retType || any isOwnedParam params || anyLocalHasOwned body
  where
    isOwnedParam (Fix (C.VarDecl ty _ _)) = hasOwned ty
    isOwnedParam _                        = False

    anyLocalHasOwned = flip execState False . traverseAst @ScopedId (astActions
        { doNode = \_ node continuation -> do
            case unFix node of
                C.VarDecl ty _ _ -> when (hasOwned ty) $ put True
                _                -> return ()
            continuation
        })

-- | Collect functions that use 'owned' and build the call graph.
collectInfo :: TypeSystem -> [ScopedTranslationUnit] -> AnalysisState
collectInfo ts tus =
    let
        pass1 = execState (mapM_ collectPass1 tus) (initialState ts)
        pass2 = execState (mapM_ (collectPass2 pass1) tus) pass1
    in pass2
  where
    initialState ts' = AnalysisState "" Set.empty Set.empty Map.empty Map.empty Map.empty Map.empty ts' Map.empty Map.empty

    collectPass1 (file, nodes) = do
        modify $ \s -> s { currentFile = file }
        ts' <- gets typeSystem
        mapM_ (traverseAst (collector1 ts')) nodes

    collector1 ts' = astActions
        {
            doNode = \_ node continuation -> do
            case unFix node of
                C.FunctionDefn _ (Fix (C.FunctionPrototype retType (L _ _ name) params)) body -> do
                    let paramOwnership = map isOwnedParam params
                    modify $ \s -> s { funcParams = Map.insertWith (zipWith (||)) (sidName name) paramOwnership (funcParams s)
                                     , funcReturns = Map.insertWith (||) (sidName name) (hasOwned retType) (funcReturns s)
                                     }
                    forM_ params $ \case
                        Fix (C.VarDecl ty (L _ _ sid) _) -> do
                            let typeInfo = resolveRef ts' (cimpleToTypeInfo ty)
                            modify $ \s -> s { varTypesFull = Map.insert sid typeInfo (varTypesFull s) }
                            forM_ (getTypeName ty) $ \typeName ->
                                modify $ \s -> s { varTypesInfo = Map.insert sid typeName (varTypesInfo s) }
                        _ -> return ()

                    -- Also collect local variables from body
                    traverseAst (localCollector ts') body

                C.FunctionDecl _ (Fix (C.FunctionPrototype retType (L _ _ name) params)) -> do
                    let paramOwnership = map isOwnedParam params
                    modify $ \s -> s { funcParams = Map.insertWith (zipWith (||)) (sidName name) paramOwnership (funcParams s)
                                     , funcReturns = Map.insertWith (||) (sidName name) (hasOwned retType) (funcReturns s)
                                     }

                C.TypedefFunction (Fix (C.FunctionPrototype retType (L _ _ name) params)) -> do
                    let paramOwnership = map isOwnedParam params
                    modify $ \s -> s { funcParams = Map.insertWith (zipWith (||)) (sidName name) paramOwnership (funcParams s)
                                     , funcReturns = Map.insertWith (||) (sidName name) (hasOwned retType) (funcReturns s)
                                     }

                _ -> return ()
            continuation
        }

    collectPass2 info (file, nodes) = do
        modify $ \s -> s { currentFile = file }
        mapM_ (traverseAst (collector2 info)) nodes

    collector2 info = astActions
        {
            doNode = \_ node continuation -> do
            case unFix node of
                C.FunctionDefn _ (Fix (C.FunctionPrototype retType (L _ _ name) params)) body -> do
                    isAlreadyChecked <- gets (Set.member name . checkedFuncs)
                    let isChecked = isAlreadyChecked || usesLinearFeatures info retType body params
                    when isChecked $
                        modify $ \s -> s { checkedFuncs = Set.insert name (checkedFuncs s)
                                         , inherentlyCheckedFuncs = Set.insert name (inherentlyCheckedFuncs s)
                                         }

                    let callees = collectCallees body
                    modify $ \s -> s { callGraph = Map.insertWith Set.union name callees (callGraph s) }
                    forM_ (Set.toList callees) $ \callee ->
                        modify $ \s -> s { reverseCallGraph = Map.insertWith Set.union callee (Set.singleton name) (reverseCallGraph s) }

                C.FunctionDecl _ (Fix (C.FunctionPrototype retType (L _ _ name) params)) -> do
                    let isOwned = hasOwned retType || any isOwnedParam params
                    when isOwned $
                        modify $ \s -> s { checkedFuncs = Set.insert name (checkedFuncs s)
                                         , inherentlyCheckedFuncs = Set.insert name (inherentlyCheckedFuncs s)
                                         }
                _ -> return ()
            continuation
        }

    isOwnedParam (Fix (C.VarDecl ty _ _)) = hasOwned ty
    isOwnedParam _                        = False

    localCollector ts' = astActions
        {
            doNode = \_ node continuation -> do
            case unFix node of
                C.VarDecl ty (L _ _ sid) _ -> do
                    let typeInfo = resolveRef ts' (cimpleToTypeInfo ty)
                    modify $ \s -> s { varTypesFull = Map.insert sid typeInfo (varTypesFull s) }
                    forM_ (getTypeName ty) $ \typeName ->
                        modify $ \s -> s { varTypesInfo = Map.insert sid typeName (varTypesInfo s) }
                _ -> return ()
            continuation
        }

    collectCallees body = execState (traverseAst calleeCollector body) Set.empty
    calleeCollector = astActions
        {
            doNode = \_ node continuation -> do
            case unFix node of
                C.FunctionCall fun _ ->
                    case toVar fun of
                        Just name -> modify (Set.insert name)
                        Nothing   -> return ()
                _ -> return ()
            continuation
        }

toVar :: ScopedNode -> Maybe ScopedId
toVar (Fix (C.VarExpr (L _ _ name))) = Just name
toVar (Fix (C.MemberAccess _ (L _ _ name))) = Just name
toVar (Fix (C.PointerAccess _ (L _ _ name))) = Just name
toVar (Fix (C.ParenExpr e)) = toVar e
toVar (Fix (C.CastExpr _ e)) = toVar e
toVar _ = Nothing

-- | Propagate 'checked' status to all functions called by a checked function.
propagateChecked :: AnalysisState -> AnalysisState
propagateChecked st =
    let
        newChecked = propagate (checkedFuncs st) (checkedFuncs st)
    in
        st { checkedFuncs = newChecked }
  where
    propagate allChecked toProcess
        | Set.null toProcess = allChecked
        | otherwise =
            let
                callees = Set.unions $ mapMaybe (`Map.lookup` callGraph st) (Set.toList toProcess)
                newlyChecked = callees \\ allChecked
            in
                propagate (Set.union allChecked newlyChecked) newlyChecked


showPath :: Path -> Text
showPath (Path sid []) = sidName sid
showPath (Path sid members) = sidName sid <> foldl' showMember "" members
  where
    showMember acc (name, isPtr) = acc <> (if isPtr then "->" else ".") <> name


newtype LinearContext l = LinearContext AnalysisState

joinOwnership :: Ownership -> Ownership -> Ownership
joinOwnership s1 s2
    | s1 == s2 = s1
    | s1 == Uninit = s2
    | s2 == Uninit = s1
    | s1 == Moved && isOwnedVal s2 = Moved
    | s2 == Moved && isOwnedVal s1 = Moved
    | s1 == Owned && (s2 == Null || isNullChecked s2) = Owned
    | s2 == Owned && (s1 == Null || isNullChecked s1) = Owned
    | isNullChecked s1 && s2 == Null = s1
    | isNullChecked s2 && s1 == Null = s2
    | otherwise = s1
  where
    isOwnedVal Owned = True
    isOwnedVal Null = True
    isOwnedVal (NullChecked _) = True
    isOwnedVal Moved = True
    isOwnedVal _ = False

isNullChecked :: Ownership -> Bool
isNullChecked (NullChecked _) = True
isNullChecked _               = False

instance DataFlow (State LinearFacts) LinearContext ScopedId LinearFacts ()
    where
    emptyFacts _ = return $ LinearFacts Map.empty Map.empty Map.empty Map.empty Map.empty Set.empty

    join (LinearContext linterInfo) f1 f2 = dtrace ("join\n  f1: " ++ show f1 ++ "\n  f2: " ++ show f2) $
        let
            states1 = varStates f1
            states2 = varStates f2
            allVars = Set.toList $ Set.union (Map.keysSet states1) (Map.keysSet states2)

            checkVar (accStates, accWarnings) p =
                let
                    s1 = Map.findWithDefault Uninit p states1
                    s2 = Map.findWithDefault Uninit p states2
                    inScope = p `Map.member` states1 && p `Map.member` states2
                    isOwned = isPathOwned linterInfo f1 p || isPathOwned linterInfo f2 p
                    joined = joinOwnership s1 s2
                    s1Moved = s1 == Moved
                    s2Moved = s2 == Moved
                    isEitherMoved = s1Moved || s2Moved
                    isBothEmpty = (s1Moved || isNullChecked s1 || s1 == Null || isPathMoved linterInfo f1 p) && (s2Moved || isNullChecked s2 || s2 == Null || isPathMoved linterInfo f2 p)
                in
                    if inScope && s1 /= s2 && isOwned && isEitherMoved && not isBothEmpty
                    then
                        let msg = "inconsistent ownership: variable `" <> showPath p <> "` is moved in some branches but not others"
                            -- We now have move locations!
                            (Path sid _) = p
                            loc = if s1Moved 
                                  then fromMaybe (L (C.AlexPn 0 0 0) C.IdVar sid) $ Map.lookup p (moveLocs f1)
                                  else fromMaybe (L (C.AlexPn 0 0 0) C.IdVar sid) $ Map.lookup p (moveLocs f2)
                            newWarns = Set.insert (msg, loc) accWarnings
                        in (Map.insert p Moved accStates, newWarns)
                    else
                        (Map.insert p joined accStates, accWarnings)

            (mergedStates, newWarnings) = foldl' checkVar (Map.empty, Set.union (warnings f1) (warnings f2)) allVars
            res = LinearFacts
                {
                    varStates = mergedStates
                ,   varTypes = Map.unionWith (||) (varTypes f1) (varTypes f2)
                ,   borrows = Map.unionWith Set.union (borrows f1) (borrows f2)
                ,   borrowLocs = Map.union (borrowLocs f1) (borrowLocs f2)
                ,   moveLocs = Map.union (moveLocs f1) (moveLocs f2)
                ,   warnings = newWarnings
                }
        in
            dtrace ("  res: " ++ show res) $ return res

    transfer (LinearContext info) funcName nodeId facts stmt = dtrace ("transfer " ++ show nodeId ++ ": " ++ show facts) $ do
        put facts
        handleStmt info funcName stmt
        finalFacts <- get
        dtrace ("  final: " ++ show finalFacts) $ return (finalFacts, Set.empty)

-- | Check if a type is an owner-returning function or function pointer.
isOwnerReturn :: AnalysisState -> TypeInfo -> Bool
isOwnerReturn info ty = case ty of
    Owner _      -> True -- Direct owner return
    Pointer t    -> isOwnerReturn info t
    Const t      -> isOwnerReturn info t
    Nonnull t    -> isOwnerReturn info t
    Nullable t   -> isOwnerReturn info t
    Var _ t      -> isOwnerReturn info t
    TypeRef FuncRef (L _ _ name) ->
        case lookupType name (typeSystem info) of
            Just (FuncDescr _ ret _) -> isOwnerReturn info ret
            _ -> False
    TypeRef UnresolvedRef (L _ _ name) ->
        case lookupType name (typeSystem info) of
            Just (FuncDescr _ ret _) -> isOwnerReturn info ret
            _ -> False
    _            -> False

addWarning :: Node (C.Lexeme ScopedId) -> Text -> State LinearFacts ()
addWarning node msg = 
    modify $ \f -> f { warnings = Set.insert (msg, getLoc node) (warnings f) }

toPath :: Node (C.Lexeme ScopedId) -> Maybe Path
toPath node = dtrace ("toPath: node=" ++ show node) $ case unFix node of
    C.VarExpr (L _ _ sid) -> Just (Path sid [])
    C.UnaryExpr C.UopDeref e -> do
        Path base sidList <- toPath e
        return $ Path base (sidList ++ [("*", True)])
    C.MemberAccess e (L _ _ sid) -> do
        Path base sidList <- toPath e
        return $ Path base (sidList ++ [(sidName sid, False)])
    C.PointerAccess e (L _ _ sid) -> do
        Path base sidList <- toPath e
        return $ Path base (sidList ++ [(sidName sid, True)])
    C.ParenExpr e -> toPath e
    C.CastExpr _ e -> toPath e
    _ -> Nothing

isNull :: Node (C.Lexeme ScopedId) -> Bool
isNull (Fix (C.LiteralExpr C.ConstId (L _ _ sid))) = sidName sid == "NULL" || sidName sid == "nullptr"
isNull (Fix (C.VarExpr (L _ _ sid))) = sidName sid == "NULL" || sidName sid == "nullptr"
isNull (Fix (C.CastExpr _ e)) = isNull e
isNull (Fix (C.ParenExpr e)) = isNull e
isNull _ = False

handleStmt :: AnalysisState -> ScopedId -> ScopedNode -> State LinearFacts ()
handleStmt info funcName n@(Fix node) = case node of
    C.VarDeclStmt (Fix (C.VarDecl ty (L _ _ sid) _)) mInit -> do
        let isOwnedVar = isOwner ty
        let path = Path sid []
        mState <- gets (Map.lookup path . varStates)
        case mState of
            Nothing -> 
                modify $ \f -> f { varStates = Map.insert path Uninit (varStates f)
                                 , varTypes = Map.insert path isOwnedVar (varTypes f)
                                 }
            Just _ -> return ()
        case mInit of 
            Just initExpr -> handleAssign info path initExpr
            Nothing -> return ()

    C.Return (Just expr) -> do
        handleExpr info expr
        let isOwnerReturnVal = Map.findWithDefault False (sidName funcName) (funcReturns info)
        when isOwnerReturnVal $ do
            owned <- isExprOwned info expr
            unless owned $ 
                addWarning expr $ "returning non-owned value from a function that returns `owner`"
        handleMove info expr

    C.ExprStmt e -> do
        case unFix e of 
            C.FunctionCall (Fix (C.VarExpr (L _ _ fSid))) [cond] | sidName fSid == "__tokstyle_assume_true" -> 
                handleAssume info True cond
            C.FunctionCall (Fix (C.VarExpr (L _ _ fSid))) [cond] | sidName fSid == "__tokstyle_assume_false" -> 
                handleAssume info False cond
            C.FunctionCall (Fix (C.VarExpr (L _ _ fSid))) _ -> do
                handleExpr info e
                when (Map.findWithDefault False (sidName fSid) (funcReturns info)) $ 
                    addWarning n $ "leak: return value of `" <> sidName fSid <> "` is ignored"
            _ -> handleExpr info e

    _ -> handleExpr info n


handleAssume :: AnalysisState -> Bool -> ScopedNode -> State LinearFacts ()
handleAssume info isTrue cond = case unFix cond of 
    C.BinaryExpr lhs C.BopEq rhs | isNull rhs -> 
        when isTrue $ forM_ (toPath lhs) $ \path -> do
            let (Path sid _) = path
            modify $ \f -> f { varStates = Map.insert path (NullChecked (sidName sid)) (varStates f) }
    C.BinaryExpr lhs C.BopNe rhs | isNull rhs -> 
        when (not isTrue) $ forM_ (toPath lhs) $ \path -> do
            let (Path sid _) = path
            modify $ \f -> f { varStates = Map.insert path (NullChecked (sidName sid)) (varStates f) }
    C.BinaryExpr lhs C.BopAnd rhs -> do
        when isTrue $ do
            handleAssume info True lhs
            handleAssume info True rhs
    C.BinaryExpr lhs C.BopOr rhs -> do
        if isTrue
        then do
            -- Heuristic: if we are in the true branch of an OR, either side could be true.
            -- To avoid false positives on early returns, we assume both sides hold
            -- for the purpose of marking things as NullChecked.
            handleAssume info True lhs
            handleAssume info True rhs
        else do
            handleAssume info False lhs
            handleAssume info False rhs
    C.UnaryExpr C.UopNot inner -> 
        handleAssume info (not isTrue) inner
    C.ParenExpr inner -> 
        handleAssume info isTrue inner
    C.VarExpr (L _ _ sid) -> 
        -- if (p) -> in true branch p is NOT null, in false branch p IS null.
        unless isTrue $ do
            let path = Path sid []
            facts <- get
            when (isPathOwned info facts path) $ 
                modify $ \f -> f { varStates = Map.insert path (NullChecked (sidName sid)) (varStates f) }
    _ -> return ()


handleUse :: ScopedNode -> State LinearFacts ()
handleUse expr = do
    case toPath expr of
        Just path -> do
            st <- gets (Map.lookup path . varStates)
            case st of
                Just Moved -> addWarning expr $ "use-after-move of variable `" <> showPath path <> "`"
                Just (Invalid owner) -> addWarning expr $ "use-after-move: borrow `" <> showPath path <> "` is invalid because `" <> owner <> "` was moved"
                _ -> return ()
        Nothing -> return ()

getExprType :: AnalysisState -> ScopedNode -> State LinearFacts (Maybe Text)
getExprType info (Fix expr) = case expr of
    C.VarExpr (L _ _ sid) -> return $ Map.lookup sid (varTypesInfo info)
    C.MemberAccess e (L _ _ field) -> do
        typeName <- getExprType info e
        case typeName of
            Just tn -> 
                case lookupType tn (typeSystem info) of
                    Just (StructDescr _ fields) -> 
                        case find (\(L _ _ name, _) -> name == sidName field) fields of 
                            Just (_, info') -> return $ getTypeRefName info'
                            _ -> return Nothing
                    _ -> return Nothing
            Nothing -> return Nothing
    C.PointerAccess e (L _ _ field) -> do
        typeName <- getExprType info e
        case typeName of
            Just tn -> 
                case lookupType tn (typeSystem info) of
                    Just (StructDescr _ fields) -> 
                        case find (\(L _ _ name, _) -> name == sidName field) fields of 
                            Just (_, info') -> return $ getTypeRefName info'
                            _ -> return Nothing
                    _ -> return Nothing
            Nothing -> return Nothing
    C.CastExpr ty _ -> return $ getTypeName ty
    C.ParenExpr e  -> getExprType info e
    _ -> return Nothing

getExprOwnership :: AnalysisState -> ScopedNode -> State LinearFacts Ownership
getExprOwnership info n@(Fix expr) = do
    if isNull n
    then return Null
    else case toPath n of
        Just path -> do
            facts <- get
            st <- gets (Map.lookup path . varStates)
            case st of
                Just s -> return s
                Nothing -> if isPathOwned info facts path
                           then return Owned
                           else return BorrowedMut
        Nothing -> case expr of
            C.FunctionCall fun _ -> getCallOwnership info fun
            C.CastExpr _ e -> getExprOwnership info e
            C.ParenExpr e  -> getExprOwnership info e
            _ -> return BorrowedMut

getCallOwnership :: AnalysisState -> ScopedNode -> State LinearFacts Ownership
getCallOwnership info (Fix expr) = do
    case expr of
        C.VarExpr (L _ _ fSid) | sidName fSid `elem` ["malloc", "calloc", "realloc"] -> 
            return Owned
        C.VarExpr (L _ _ fSid) -> do
            let isGlobalOwner = Map.findWithDefault False (sidName fSid) (funcReturns info)
            if isGlobalOwner 
            then return Owned 
            else case Map.lookup fSid (varTypesFull info) of 
                Just ty -> return $ if isOwnerReturn info ty then Owned else BorrowedMut
                Nothing -> return BorrowedMut
        C.MemberAccess e (L _ _ field) -> do
            typeName <- getExprType info e
            case typeName of
                Just tn -> 
                    case lookupType tn (typeSystem info) of
                        Just (StructDescr _ fields) -> 
                            case find (\(L _ _ name, _) -> name == sidName field) fields of 
                                Just (_, info') -> return $ if isOwnerReturn info info' then Owned else BorrowedMut
                                Nothing -> return BorrowedMut
                        _ -> return BorrowedMut
                Nothing -> return BorrowedMut
        C.PointerAccess e (L _ _ field) -> do
            typeName <- getExprType info e
            case typeName of
                Just tn -> 
                    case lookupType tn (typeSystem info) of
                        Just (StructDescr _ fields) -> 
                            case find (\(L _ _ name, _) -> name == sidName field) fields of 
                                Just (_, info') -> return $ if isOwnerReturn info info' then Owned else BorrowedMut
                                Nothing -> return BorrowedMut
                        _ -> return BorrowedMut
                Nothing -> return BorrowedMut
        C.UnaryExpr C.UopDeref e -> getCallOwnership info e
        C.ParenExpr e -> getCallOwnership info e
        C.CastExpr _ e -> getCallOwnership info e
        _ -> return BorrowedMut

isExprOwned :: AnalysisState -> ScopedNode -> State LinearFacts Bool
isExprOwned info expr = do
    st <- getExprOwnership info expr
    if st `elem` [Owned, Null] || isNullChecked st
    then return True
    else case toPath expr of
        Just path -> do
            facts <- get
            return $ isPathOwned info facts path
        Nothing -> return False

handleExpr :: AnalysisState -> ScopedNode -> State LinearFacts ()
handleExpr info n@(Fix expr) = do
    case expr of
        C.AssignExpr lhs C.AopEq rhs -> dtrace ("AssignExpr: lhs=" ++ show lhs) $ do
            case toPath lhs of
                Just path -> dtrace ("  path=" ++ show path) $ do
                    st <- gets (Map.lookup path . varStates)
                    case st of 
                        Just Owned -> addWarning n $ "leak: variable `" <> showPath path <> "` is overwritten while holding ownership"
                        _ -> return ()
                    
                    handleAssign info path rhs
                Nothing -> dtrace "  path=Nothing" $ do
                    handleExpr info rhs
                    handleExpr info lhs

        C.FunctionCall fun args -> do
            handleUse n
            handleExpr info fun
            let mFuncName = fmap sidName (toVar fun)
            let isFree = mFuncName == Just "free"
            let isRealloc = mFuncName == Just "realloc"

            mParams <- case mFuncName >>= (`Map.lookup` (funcParams info)) of
                Just ps -> return $ Just ps
                Nothing -> do
                    mTypeName <- getExprType info fun
                    return $ mTypeName >>= (`Map.lookup` (funcParams info))

            let checkOwnership arg = do
                    st <- getExprOwnership info arg
                    let owned = st `elem` [Owned, Null] || isNullChecked st
                    let moved = st == Moved || case st of Invalid _ -> True; _ -> False

                    dtrace ("checkOwnership: arg=" ++ show (getLoc arg) ++ " st=" ++ show st ++ " owned=" ++ show owned) $ do
                        unless (owned || moved) $ do
                            case toPath arg of
                                Just path -> addWarning arg $ "passing non-owned variable `" <> showPath path <> "` to function expecting owner"
                                Nothing   -> addWarning arg "passing non-owned expression to function expecting owner"

            let traceMsg = "handleExpr Call: " ++ show mFuncName ++ " params=" ++ show mParams

            case mParams of
                Just params -> do
                    forM_ args (handleExpr info)
                    forM_ (zip3 ([0..] :: [Int]) params args) $ \(i, isOwned, arg) ->
                        dtrace (traceMsg ++ " arg " ++ show i ++ " isOwned=" ++ show isOwned) $
                        if | isOwned -> do
                                checkOwnership arg
                                handleMove info arg
                           | isFree -> do
                                checkOwnership arg
                                handleMove info arg
                           | isRealloc && i == 0 -> do
                                checkOwnership arg
                                handleMove info arg
                           | otherwise -> return ()
                Nothing ->
                    forM_ (zip ([0..] :: [Int]) args) $ \(i, arg) ->
                        dtrace (traceMsg ++ " arg " ++ show i ++ " (no params info)") $
                        if | isFree -> do
                                checkOwnership arg
                                handleMove info arg
                           | isRealloc && i == 0 -> do
                                checkOwnership arg
                                handleMove info arg
                           | otherwise -> handleExpr info arg

        C.BinaryExpr lhs op rhs -> do
            handleExpr info lhs
            handleExpr info rhs
            case (op, toPath lhs, toPath rhs) of 
                (C.BopPlus, Just srcPath, _) -> handleArithmetic info srcPath n
                (C.BopMinus, Just srcPath, _) -> handleArithmetic info srcPath n
                (C.BopPlus, _, Just srcPath) -> handleArithmetic info srcPath n
                _ -> return ()
        C.UnaryExpr op e -> do
            handleExpr info e
            case (op, toPath e) of 
                (C.UopIncr, Just path) -> forbidArithmetic info path n
                (C.UopDecr, Just path) -> forbidArithmetic info path n
                _ -> return ()
        C.ParenExpr e -> handleExpr info e
        C.CastExpr _ e -> handleExpr info e
        C.MemberAccess e _ -> handleExpr info e
        C.PointerAccess e _ -> handleExpr info e
        C.ArrayAccess e1 e2 -> do
            handleExpr info e1
            handleExpr info e2
            case toPath e1 of 
                Just srcPath -> handleArithmetic info srcPath n
                Nothing -> return ()
        C.TernaryExpr c t e -> handleExpr info c >> handleExpr info t >> handleExpr info e
        C.InitialiserList es -> mapM_ (handleExpr info) es
        C.LiteralExpr {} -> return ()
        C.VarExpr {} -> handleUse n
        _ -> return ()

handleArithmetic :: AnalysisState -> Path -> ScopedNode -> State LinearFacts ()
handleArithmetic info srcPath n = do
    currentFacts <- get
    let isSrcOwned = isPathOwned info currentFacts srcPath
    let allBorrows = borrows currentFacts
    let transitiveSources = case Map.lookup srcPath allBorrows of 
            Just srcs -> srcs
            Nothing   -> Set.empty

    when (isSrcOwned || not (Set.null transitiveSources)) $ do
        let newSrcs = if isSrcOwned then Set.insert srcPath transitiveSources else transitiveSources
        modify $ \f -> f { borrows = Map.insert (Path (dummyScopedId "expr") []) newSrcs (borrows f)
                         , borrowLocs = Map.insert (Path (dummyScopedId "expr") []) (getLoc n) (borrowLocs f)
                         }

forbidArithmetic :: AnalysisState -> Path -> ScopedNode -> State LinearFacts ()
forbidArithmetic info path n = do
    currentFacts <- get
    when (isPathOwned info currentFacts path) $ 
        addWarning n $ "cannot perform pointer arithmetic on an `owner` pointer"


getExprPaths :: LinearFacts -> ScopedNode -> Set Path
getExprPaths facts n = case toPath n of
    Just p  -> Set.singleton p
    Nothing -> case unFix n of
        C.BinaryExpr lhs op rhs -> case op of
            C.BopPlus -> getExprPaths facts lhs `Set.union` getExprPaths facts rhs
            C.BopMinus -> getExprPaths facts lhs `Set.union` getExprPaths facts rhs
            _ -> Set.empty
        C.UnaryExpr _ e -> getExprPaths facts e
        C.ParenExpr e -> getExprPaths facts e
        C.CastExpr _ e -> getExprPaths facts e
        C.ArrayAccess e1 _ -> getExprPaths facts e1
        C.FunctionCall _ args -> Set.unions $ map (getExprPaths facts) args
        _ -> Set.empty

handleAssign :: AnalysisState -> Path -> ScopedNode -> State LinearFacts ()
handleAssign info path expr@(Fix node) = do
    handleExpr info expr
    currentFacts <- get
    let isTargetOwned = isPathOwned info currentFacts path
    newState <- getExprOwnership info expr

    if isTargetOwned
    then do
        handleMove info expr
        modify $ \f -> f { varStates = Map.insert path newState (varStates f)
                         , varTypes = Map.insert path True (varTypes f)
                         }
        updateUnionMembers info expr path newState
    else do
        let directSrcPaths = getExprPaths currentFacts expr
        let expand p = p : case Map.lookup p (borrows currentFacts) of
                            Just srcs -> Set.toList srcs
                            Nothing   -> []
        let srcPaths = Set.fromList $ concatMap expand (Set.toList directSrcPaths)
        let realSrcPaths = Set.filter (\p -> case getPathInfo info p of
                                                Just ty -> not (isCopy ty)
                                                Nothing -> True) srcPaths

        unless (Set.null realSrcPaths) $ do
            let existingBorrows = [ (b, st) 
                                  | (b, srcs) <- Map.toList (borrows currentFacts)
                                  , not (Set.null (realSrcPaths `Set.intersection` srcs))
                                  , let st = Map.findWithDefault Uninit b (varStates currentFacts)
                                  , st `elem` [BorrowedMut, BorrowedConst]
                                  ]

            let isNewMut = case getPathInfo info path of 
                    Just ty -> not (isConstBorrow ty)
                    Nothing -> True

            let (mutBorrows, constBorrows) = partition ((== BorrowedMut) . snd) existingBorrows

            -- Check for conflicts with each source path
            forM_ (Set.toList realSrcPaths) $ \srcPath -> do
                let isSrcOwned = isPathOwned info currentFacts srcPath
                when isSrcOwned $ 
                    if isNewMut
                    then when (not (null mutBorrows) || not (null constBorrows)) $ 
                            addWarning expr $ "cannot borrow `" <> showPath srcPath <> "` as mutable because it is already borrowed"
                    else when (not (null mutBorrows)) $ 
                            addWarning expr $ "cannot borrow `" <> showPath srcPath <> "` as immutable because it is already borrowed as mutable"

            modify $ \f -> f { borrows = Map.insert path realSrcPaths (borrows f)
                             , borrowLocs = Map.insert path (getLoc expr) (borrowLocs f)
                             }

        let actualState = case newState of 
                Owned -> BorrowedMut
                Null  -> Null
                _     -> newState

        let finalState = case (actualState, getPathInfo info path) of 
                (BorrowedMut, Just ty) | isConstBorrow ty -> BorrowedConst
                _ -> actualState

        modify $ \f -> f { varStates = Map.insert path finalState (varStates f) }

handleMove :: AnalysisState -> ScopedNode -> State LinearFacts ()
handleMove info expr = do
    handleUse expr
    case toPath expr of
        Just path -> do
            facts <- get
            let st = Map.lookup path (varStates facts)
            case st of 
                _ | st == Just Owned || isNullChecked (fromMaybe Uninit st) || (st == Nothing && isPathOwned info facts path) -> do
                    modify $ \f -> f { varStates = Map.insert path Moved (varStates f)
                                     , moveLocs = Map.insert path (getLoc expr) (moveLocs f)
                                     }
                    updateUnionMembers info expr path Moved
                    -- Invalidate borrows
                    allBorrows <- gets borrows
                    let dependents = [d | (d, srcs) <- Map.toList allBorrows, path `Set.member` srcs]
                    forM_ dependents $ \d -> 
                        modify $ \f -> f { varStates = Map.insert d (Invalid (showPath path)) (varStates f) }

                _ -> return ()
        Nothing -> return ()

isPathMoved :: AnalysisState -> LinearFacts -> Path -> Bool
isPathMoved info facts (Path sid members) =
    any isPrefixMoved (prefixes members)
  where
    prefixes ms = [ Path sid (take i ms) | i <- [0..length ms] ]
    isPrefixMoved p =
        case Map.findWithDefault Uninit p (varStates facts) of
            Moved -> True
            Invalid _ -> True
            Null -> True
            NullChecked _ -> True
            _ -> False

updateUnionMembers :: AnalysisState -> ScopedNode -> Path -> Ownership -> State LinearFacts ()
updateUnionMembers info triggerNode path@(Path sid members) newState = dtrace ("updateUnionMembers: path=" ++ show path ++ " newState=" ++ show newState) $
    case reverse members of
        ((m, isPtr) : rest) -> do
            let parentPath = Path sid (reverse rest)
            case getPathInfo info parentPath of
                Just ty -> dtrace ("  parentType=" ++ show ty) $
                    case getTypeRefName ty of
                        Just typeName ->
                            case lookupType typeName (typeSystem info) of
                                Just (UnionDescr _ fields) -> dtrace ("  unionFields=" ++ show fields) $ do
                                    facts <- get
                                    forM_ fields $ \(L _ _ fieldName, _) ->
                                        when (fieldName /= m) $ do
                                            let otherPath = Path sid (reverse rest ++ [(fieldName, isPtr)])
                                            -- If we are assigning to one member, the others are effectively overwritten.
                                            -- If another member was Owned, that's a leak.
                                            st <- gets (Map.lookup otherPath . varStates)
                                            dtrace ("    checking otherPath=" ++ show otherPath ++ " st=" ++ show st) $
                                                when (isPathOwned info facts otherPath && (st == Just Owned || maybe False isNullChecked st)) $
                                                    addWarning triggerNode $ "leak: union member `" <> showPath otherPath <> "` is overwritten by assignment to `" <> m <> "`"
                                            modify $ \f -> f { varStates = Map.insert otherPath Moved (varStates f) }
                                _ -> return ()
                        _ -> return ()
                Nothing -> dtrace "  parentType=Nothing" $ return ()
        _ -> return ()
