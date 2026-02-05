{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.Linter.BorrowCheck (descr) where

import           Control.Monad                   (foldM, forM_, mplus, unless,
                                                  when)
import           Data.List                       (findIndex)
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe                      (fromMaybe, isNothing,
                                                  listToMaybe, mapMaybe)
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Traversable                (forM)
import           Language.C.Analysis.AstAnalysis (ExprSide (..), tExpr)
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TravMonad   (Trav, lookupObject)
import           Language.C.Analysis.TypeUtils   (canonicalType)
import           Language.C.Data.Ident           (Ident (..))
import           Language.C.Data.Node            (NodeInfo, nodeInfo)
import           Language.C.Syntax.AST
import           Prettyprinter                   (Pretty, pretty)
import qualified Prettyprinter                   as PP
import           Prettyprinter.Render.Terminal   (AnsiStyle)
import           Tokstyle.Analysis.AccessPath
import           Tokstyle.Analysis.Dataflow      as Dataflow
import           Tokstyle.C.Analysis.CFG         as CFG
import           Tokstyle.C.Analysis.Liveness    (Liveness, liveness)
import           Tokstyle.C.Env                  (DiagnosticLevel (..),
                                                  DiagnosticSpan (..), Env,
                                                  posAndLen, recordLinterError,
                                                  recordRichError)

-- | Possible states of an owned variable.
data VarState
    = StateUninitialized NodeInfo
    | StateOwned NodeInfo
    | StateSharedBorrowedFrom AccessPath NodeInfo
    | StateMutableBorrowedFrom AccessPath NodeInfo
    | StateMoved NodeInfo
    | StateInconsistent NodeInfo NodeInfo
    deriving (Show, Eq)

type Lattice = Map AccessPath VarState

data FunctionSummary = FunctionSummary
    { summaryReturnProvenance :: Maybe AccessPath
    } deriving (Show, Eq)

data Effect
    = Move AccessPath NodeInfo
    | Use AccessPath NodeInfo
    | Define AccessPath NodeInfo
    | Declare AccessPath NodeInfo
    | SharedBorrow AccessPath AccessPath NodeInfo
    | MutableBorrow AccessPath AccessPath NodeInfo
    deriving (Show, Eq)

type NodeEffects = Map Node [Effect]

-- | Helper to get attributes from any Type.
typeAttrs :: Type -> Attributes
typeAttrs = \case
    DirectType _ _ a                   -> a
    PtrType _ _ a                      -> a
    ArrayType _ _ _ a                  -> a
    FunctionType _ a                   -> a
    TypeDefType (TypeDefRef _ t _) _ a -> a ++ typeAttrs t

-- | Get the name of an identifier.
idName :: Ident -> String
idName (Ident name _ _) = name

isOwnedCAttr :: CAttribute a -> Bool
isOwnedCAttr (CAttr i _ _) = idName i `elem` ["owned", "__owned__"]

isOwnedAttr :: Attr -> Bool
isOwnedAttr (Attr i _ _) = idName i `elem` ["owned", "__owned__"]

-- | Check if a type or attributes are marked as __owned__.
isOwned :: Type -> Bool
isOwned ty = case ty of
    PtrType {}                          -> any isOwnedAttr (typeAttrs ty)
    ArrayType {}                        -> any isOwnedAttr (typeAttrs ty)
    TypeDefType (TypeDefRef _ t' _) _ _ -> isOwned t'
    _                                   -> False

-- | Check if a type is a pointer or array.
isPointerType :: Type -> Bool
isPointerType ty = case canonicalType ty of
    PtrType {}   -> True
    ArrayType {} -> True
    _            -> False

isSpecPointer :: GlobalDecls -> CDeclarationSpecifier a -> Bool
isSpecPointer d (CTypeSpec (CTypeDef i _)) =
    case Map.lookup i (gTypeDefs d) of
        Just (TypeDef _ t _ _) -> isPointerType t
        _                      -> False
isSpecPointer _ _ = False

isCDeclrPointer :: CDeclarator a -> Bool
isCDeclrPointer (CDeclr _ derived _ _ _) = any isDerivedPointer derived

isNullPtr :: AccessPath -> Bool
isNullPtr (PathVar "nullptr") = True
isNullPtr _                   = False

isDerivedPointer :: CDerivedDeclarator a -> Bool
isDerivedPointer (CPtrDeclr {}) = True
isDerivedPointer (CArrDeclr {}) = True
isDerivedPointer _              = False

isOwnedDecl :: Attributes -> Type -> Bool
isOwnedDecl attrs ty = any isOwnedAttr attrs || isOwned ty

isOwnedSpec :: CDeclarationSpecifier a -> Bool
isOwnedSpec (CTypeQual (CAttrQual attr)) = isOwnedCAttr attr
isOwnedSpec _                            = False

isConstSpec :: CDeclarationSpecifier a -> Bool
isConstSpec (CTypeQual (CConstQual _)) = True
isConstSpec _                          = False

isTypeMutable :: Type -> Bool
isTypeMutable = \case
    DirectType _ q _                   -> not (constant q)
    PtrType t _ _                      -> isTypeMutable t
    ArrayType t _ _ _                  -> isTypeMutable t
    FunctionType _ _                   -> False
    TypeDefType (TypeDefRef _ t _) q _ -> not (constant q) && isTypeMutable t

isCDeclrOwned :: CDeclarator a -> Bool
isCDeclrOwned (CDeclr _ derived _ attrs _) =
    any isOwnedCAttr attrs || any isDerivedOwned derived

isCDeclrOwnedMutable :: CDeclarator a -> Bool
isCDeclrOwnedMutable (CDeclr _ derived _ _ _) =
    not (any isDerivedConst derived)

isDerivedOwned :: CDerivedDeclarator a -> Bool
isDerivedOwned (CPtrDeclr quals _)   = any isQualOwned quals
isDerivedOwned (CArrDeclr quals _ _) = any isQualOwned quals
isDerivedOwned (CFunDeclr _ attrs _) = any isOwnedCAttr attrs

isDerivedConst :: CDerivedDeclarator a -> Bool
isDerivedConst (CPtrDeclr quals _)   = any isQualConst quals
isDerivedConst (CArrDeclr quals _ _) = any isQualConst quals
isDerivedConst (CFunDeclr _ _ _)     = False

isQualOwned :: CTypeQualifier a -> Bool
isQualOwned (CAttrQual attr) = isOwnedCAttr attr
isQualOwned _                = False

isQualConst :: CTypeQualifier a -> Bool
isQualConst (CConstQual _) = True
isQualConst _              = False

exprPath :: CExpression a -> Maybe AccessPath
exprPath = \case
    CVar i _          -> Just (PathVar (idName i))
    CUnary CIndOp e _ -> PathDeref <$> exprPath e
    CUnary CAdrOp e _ -> exprPath e
    CMember e i _ _   -> PathField <$> exprPath e <*> pure (idName i)
    CBinary _ l r _   -> exprPath l `mplus` exprPath r
    _                 -> Nothing

borrowProblem :: NodeEffects -> Dataflow Node EdgeType Lattice
borrowProblem nodeEffects = Dataflow
    { transfer     = transferFunc
    , edgeTransfer = \_ _ l -> Map.filterWithKey (\k _ -> not (isTemporary k)) l
    , merge        = Map.unionWith mergeVar
    , initial      = Map.empty
    }
  where
        mergeVar s1 s2 | s1 == s2 = s1
        mergeVar (StateInconsistent ni mi) _ = StateInconsistent ni mi
        mergeVar _ (StateInconsistent ni mi) = StateInconsistent ni mi
        mergeVar (StateOwned ni) (StateMoved mi) = StateInconsistent ni mi
        mergeVar (StateMoved mi) (StateOwned ni) = StateInconsistent ni mi
        mergeVar (StateOwned ni) (StateSharedBorrowedFrom _ mi) = StateInconsistent ni mi
        mergeVar (StateSharedBorrowedFrom _ mi) (StateOwned ni) = StateInconsistent ni mi
        mergeVar (StateOwned ni) (StateMutableBorrowedFrom _ mi) = StateInconsistent ni mi
        mergeVar (StateMutableBorrowedFrom _ mi) (StateOwned ni) = StateInconsistent ni mi
        mergeVar s1 s2 = StateInconsistent (getNi s1) (getNi s2)

        getNi (StateUninitialized ni)         = ni
        getNi (StateOwned ni)                 = ni
        getNi (StateSharedBorrowedFrom _ ni)  = ni
        getNi (StateMutableBorrowedFrom _ ni) = ni
        getNi (StateMoved ni)                 = ni
        getNi (StateInconsistent ni _)        = ni

        transferFunc :: Node -> Lattice -> Lattice
        transferFunc node lat =
            let effects = Map.findWithDefault [] node nodeEffects
            in foldl applyEffect lat effects

        applyEffect l (Move i ni)     = Map.insert i (StateMoved ni) l
        applyEffect l (Define i ni)   = Map.insert i (StateOwned ni) l
        applyEffect l (Declare i ni)  = Map.insert i (StateUninitialized ni) l
        applyEffect l (SharedBorrow q p ni) =
            let origin = findOrigin p l
            in Map.insert q (StateSharedBorrowedFrom origin ni) l
        applyEffect l (MutableBorrow q p ni) =
            let origin = findOrigin p l
            in Map.insert q (StateMutableBorrowedFrom origin ni) l
        applyEffect l (Use _ _)       = l


findOrigin :: AccessPath -> Lattice -> AccessPath
findOrigin p l = findOrigin' Set.empty p
  where
    findOrigin' visited curr
        | curr `Set.member` visited = curr
        | otherwise = case Map.lookup curr l of
            Just (StateSharedBorrowedFrom x _)  -> findOrigin' (Set.insert curr visited) x
            Just (StateMutableBorrowedFrom x _) -> findOrigin' (Set.insert curr visited) x
            _                                   -> curr

isTemporary :: AccessPath -> Bool
isTemporary = \case
    PathParam {} -> True
    PathReturn   -> True
    _            -> False

summariseFunction :: IdentDecl -> Maybe (String, FunctionSummary)
summariseFunction = \case
    FunctionDef (FunDef (VarDecl (VarName (idName -> name) _) _ (FunctionType (FunType _ params _) _)) (CCompound _ items _) _) ->
        -- Simplified summarization: just look at what's returned.
        -- We'll look for simple returns like "return p;" or "return p->f;".
        let provenance = listToMaybe $ mapMaybe (findReturnProvenance params) items
        in Just (name, FunctionSummary provenance)
    _ -> Nothing
  where
    findReturnProvenance params = \case
        CBlockStmt (CReturn (Just e) _) -> mapToParam params <$> exprPath e
        CBlockStmt (CCompound _ bis _)  -> listToMaybe $ mapMaybe (findReturnProvenance params) bis
        -- Note: this is very simplified, doesn't handle branches.
        _ -> Nothing

    mapToParam params p@(PathVar v) =
        case findIndex (isParam v) params of
            Just i  -> PathParam i
            Nothing -> p
    mapToParam _ p@PathParam{} = p
    mapToParam _ PathReturn = PathReturn
    mapToParam params (PathDeref p) = PathDeref (mapToParam params p)
    mapToParam params (PathField p f) = PathField (mapToParam params p) f
    mapToParam params (PathIndex p i) = PathIndex (mapToParam params p) i

    isParam v (ParamDecl (VarDecl (VarName (idName -> name) _) _ _) _) = name == v
    isParam _ _                                                       = False

isExprMutable :: CExpression NodeInfo -> Trav Env Bool
isExprMutable expr = do
    t <- tExpr [] RValue expr
    return $ isTypeMutable t

analyse :: GlobalDecls -> Trav Env ()
analyse decls = do
    let summaries = Map.fromList $ mapMaybe summariseFunction (Map.elems $ gObjs decls)
    forM_ (Map.elems $ gObjs decls) $ \case
        FunctionDef fd@(FunDef (VarDecl (VarName _ _) _ (FunctionType (FunType _ params _) _)) _ _) -> do
            let (entry, cfg) = fromFunDef fd
            let localDecls = collectDecls decls fd
            effects <- computeNodeEffects decls summaries localDecls (Map.keys cfg)
            let initialLat = Map.fromList [ (PathVar (idName name), StateOwned (nodeInfo name)) | ParamDecl (VarDecl (VarName name _) (DeclAttrs _ _ attrs) t) _ <- params, isOwnedDecl attrs t ]
            let problem = (borrowProblem effects) { initial = initialLat }
            let result = solve entry cfg problem
            let live = liveness localDecls [Node 0 ExitNode] cfg -- FIXME: ExitNode ID
            let isExpectedOwned var = case var of
                    PathVar name -> maybe False fst (Map.lookup name localDecls)
                    PathReturn   -> True -- Handled separately in _ case
                    _            -> False
            reportErrors isExpectedOwned live effects result
            checkLeaks isExpectedOwned localDecls effects (transfer problem) result
        _ -> return ()
  where
    collectDecls d (FunDef (VarDecl _ _ (FunctionType (FunType _ params _) _)) (CCompound _ items _) _) =
        let fromParams = [ (idName name, (isOwnedDecl attrs t, isPointerType t)) | ParamDecl (VarDecl (VarName name _) (DeclAttrs _ _ attrs) t) _ <- params ]
            fromItems = concatMap (collectBlockItemDecls d) items
        in Map.fromList (fromParams ++ fromItems)
    collectDecls _ _ = Map.empty

    collectBlockItemDecls d = \case
        CBlockStmt s                  -> collectStatDecls d s
        CBlockDecl (CDecl specs ds _) -> collectSingleDecl d specs ds
        _                             -> []

    collectSingleDecl d specs ds =
        let declOwned = any isOwnedSpec specs
            specsPtr = any (isSpecPointer d) specs
        in mapMaybe (\case
            (Just declr@(CDeclr (Just i) _ _ _ _), _, _) ->
                Just (idName i, (declOwned || isCDeclrOwned declr, specsPtr || isCDeclrPointer declr))
            _ -> Nothing) ds

    collectStatDecls d = \case
        CCompound _ items _ -> concatMap (collectBlockItemDecls d) items
        CIf _ t e _ -> collectStatDecls d t ++ maybe [] (collectStatDecls d) e
        CWhile _ s _ _ -> collectStatDecls d s
        CFor init' _ _ s _ ->
            let fromInit = case init' of
                    Right dc -> collectBlockItemDecls d (CBlockDecl dc)
                    _        -> []
            in fromInit ++ collectStatDecls d s
        CSwitch _ s _ -> collectStatDecls d s
        _ -> []

    computeNodeEffects d s localDecls nodes = do
        effectList <- forM nodes $ \node -> do
            effs <- case node of
                Node _ (StatNode st)   -> collectStatEffects d s localDecls st
                Node _ (ExprNode e)    -> collectExprEffects d s localDecls Nothing e
                Node _ (DeclNode dc)   -> collectDeclEffects d s localDecls dc
                Node _ (BranchNode e)  -> collectExprEffects d s localDecls Nothing e
                _                      -> return []
            return (node, effs)
        return $ Map.fromList effectList

    collectStatEffects d s localDecls = \case
        CExpr (Just e) _ -> collectExprEffects d s localDecls Nothing e
        CReturn (Just e) _ -> collectMoveEffects d s localDecls Nothing True e
        CCompound _ items _ -> concat <$> mapM (collectItemEffects d s localDecls) items
        CIf c t e _ -> do
            ce <- collectExprEffects d s localDecls Nothing c
            te <- collectStatEffects d s localDecls t
            ee <- maybe (return []) (collectStatEffects d s localDecls) e
            return $ ce ++ te ++ ee
        -- TODO: more stats
        _ -> return []

    collectItemEffects d s localDecls = \case
        CBlockStmt st -> collectStatEffects d s localDecls st
        CBlockDecl dc -> collectDeclEffects d s localDecls dc
        _             -> return []

    collectDeclEffects d s localDecls = \case
        CDecl specs ds _ ->
            let declOwned = any isOwnedSpec specs
                isConst = any isConstSpec specs
            in concat <$> forM ds (\case
                (Just declr@(CDeclr (Just i) _ _ _ niDecl), mInit, _) -> do
                    let owned = declOwned || isCDeclrOwned declr
                    let mutable = not isConst && isCDeclrOwnedMutable declr
                    let pl = PathVar (idName i)
                    case mInit of
                        Just (CInitExpr e _) -> do
                            lp <- isPointerDecl localDecls i
                            let mBorrow = if lp then Just (pl, mutable) else Nothing
                            re <- collectMoveEffects d s localDecls mBorrow owned e
                            ro <- isExprOwned localDecls e
                            let defEff = if ro && (owned || isNothing (exprPath e)) then [Define pl niDecl] else []
                            let leakEff = if owned && not ro then [Define pl niDecl, Move pl niDecl] else []
                            return $ re ++ defEff ++ leakEff
                        Just (CInitList _ _) -> return []
                        Nothing ->
                            if owned
                                then return [Declare pl niDecl]
                                else return []
                _ -> return []
                )
        CStaticAssert {} -> return []

    collectExprEffects d s localDecls mBorrow expr = case expr of
        CVar i ni -> do
            let p = PathVar (idName i)
            let borrowEff = case mBorrow of
                    Just (dest, mut) | not (isNullPtr p) -> [if mut then MutableBorrow dest p ni else SharedBorrow dest p ni]
                    _ -> []
            return $ (Use p ni :) borrowEff
        CUnary CIndOp e ni -> do
            ee <- collectExprEffects d s localDecls Nothing e
            case exprPath expr of
                Just p  -> do
                    let borrowEff = case mBorrow of
                            Just (dest, mut) | not (isNullPtr p) -> [if mut then MutableBorrow dest p ni else SharedBorrow dest p ni]
                            _ -> []
                    return $ (ee ++) . (Use p ni :) $ borrowEff
                Nothing -> return ee
        CMember e _ _ ni -> do
            ee <- collectExprEffects d s localDecls Nothing e
            case exprPath expr of
                Just p  -> do
                    let borrowEff = case mBorrow of
                            Just (dest, mut) | not (isNullPtr p) -> [if mut then MutableBorrow dest p ni else SharedBorrow dest p ni]
                            _ -> []
                    return $ (ee ++) . (Use p ni :) $ borrowEff
                Nothing -> return ee
        CUnary CAdrOp e ni -> do
            ee <- collectExprEffects d s localDecls Nothing e
            case exprPath e of
                Just p -> do
                    let borrowEff = case mBorrow of
                            Just (dest, mut) | not (isNullPtr p) -> [if mut then MutableBorrow dest p ni else SharedBorrow dest p ni]
                            _ -> []
                    return $ (ee ++) . (Use p ni :) $ borrowEff
                Nothing -> return ee
        CCall f args ni -> do
            fTy <- tExpr [] RValue f
            let (paramOwned, paramMutable, paramIsPtr, rtOwned) = case fTy of
                                FunctionType (FunType rt ps _) _ -> (map isParamOwned ps, map isParamMutable ps, map isParamPointer ps, isOwned rt)
                                FunctionType (FunTypeIncomplete rt) _ -> ([], [], [], isOwned rt)
                                PtrType (canonicalType -> FunctionType (FunType rt ps _) _) _ _ -> (map isParamOwned ps, map isParamMutable ps, map isParamPointer ps, isOwned rt)
                                PtrType (canonicalType -> FunctionType (FunTypeIncomplete rt) _) _ _ -> ([], [], [], isOwned rt)
                                _ -> ([], [], [], False)
            argEffs <- forM (zip3 [0..] args paramOwned) $ \(i, arg, owned) ->
                let isPtr = fromMaybe False (listToMaybe (drop i paramIsPtr))
                    mArgBorrow = if not owned && isPtr then Just (PathParam i, fromMaybe False (listToMaybe (drop i paramMutable))) else Nothing
                in collectMoveEffects d s localDecls mArgBorrow owned arg
            let effs = concat argEffs

            -- Apply function summary for provenance tracking
            let destInfo = case mBorrow of
                    Just (dest, _) -> Just dest
                    Nothing        -> Nothing
            let dest = fromMaybe PathReturn destInfo

            let (provEffs, rtOwnedActual) = case f of
                    CVar (Ident name _ _) _ -> case Map.lookup name s of
                        Just (FunctionSummary (Just prov)) ->
                            case applySummary prov args of
                                Just pSrc | not (isNullPtr pSrc) ->
                                    case mBorrow of
                                        Just (dest', mut) | dest' /= pSrc -> ([if mut then MutableBorrow dest' pSrc ni else SharedBorrow dest' pSrc ni], rtOwned)
                                        _ -> ([], rtOwned)
                                _ -> ([], rtOwned)
                        Nothing ->
                            -- Conservative assumption: if we don't know the summary,
                            -- assume return value borrows from all non-owned pointer arguments.
                            let inputs = [ (p, mut) | (i, arg) <- zip [0..] args
                                         , let owned = fromMaybe False (listToMaybe (drop i paramOwned))
                                         , let mut = fromMaybe False (listToMaybe (drop i paramMutable))
                                         , let isPtr = fromMaybe False (listToMaybe (drop i paramIsPtr))
                                         , not owned && isPtr
                                         , Just p <- [exprPath arg]
                                         ]
                                effs' = case mBorrow of
                                    Just (dest', mutDest) ->
                                        [ if mutDest || mutSrc then MutableBorrow dest' pSrc ni else SharedBorrow dest' pSrc ni
                                        | (pSrc, mutSrc) <- inputs, dest' /= pSrc
                                        ]
                                    Nothing -> []
                            in (effs', rtOwned)
                        _ -> ([], rtOwned)
                    _ -> ([], rtOwned)

            -- Standalone calls (mDest is Nothing) must define their return value so it can be checked for leaks.
            -- Calls assigned to something will have their destination defined by the caller (CAssign/CDecl).
            let retEff = if rtOwnedActual && isNothing destInfo then [Define dest ni] else []

            return $ effs ++ provEffs ++ retEff
        CAssign CAssignOp l r ni -> do
            lo <- isExprOwned localDecls l
            ro <- isExprOwned localDecls r
            mut <- isExprMutable l
            case exprPath l of
                Just pl -> do
                    lp <- isExprPointer localDecls l
                    let mAssignBorrow = if lp then Just (pl, mut) else Nothing
                    re <- collectMoveEffects d s localDecls mAssignBorrow lo r
                    let defEff = if ro && (lo || isNothing (exprPath r)) then [Define pl ni] else []
                    let leakEff = if lo && not ro then [Define pl ni, Move pl ni] else []
                    return $ re ++ defEff ++ leakEff
                Nothing -> (++) <$> collectExprEffects d s localDecls Nothing l <*> collectMoveEffects d s localDecls Nothing lo r
        CAssign _ l r _ -> do
            le <- collectExprEffects d s localDecls Nothing l
            re <- collectMoveEffects d s localDecls Nothing False r
            return $ le ++ re
        CBinary _ l r _ -> do
            le <- collectExprEffects d s localDecls Nothing l
            re <- collectMoveEffects d s localDecls Nothing False r
            return $ le ++ re
        CUnary _ e _ -> do
            ee <- collectExprEffects d s localDecls Nothing e
            return ee
        _ -> return []
      where
        applySummary (PathParam i) args | i < length args = exprPath (args !! i)
        applySummary (PathDeref p) args = PathDeref <$> applySummary p args
        applySummary (PathField p f) args = PathField <$> applySummary p args <*> pure f
        applySummary _ _ = Nothing


    isParamOwned (ParamDecl (VarDecl _ _ t) _)         = isOwned t
    isParamOwned (AbstractParamDecl (VarDecl _ _ t) _) = isOwned t

    isParamMutable (ParamDecl (VarDecl _ _ t) _)         = isPtrTargetMutable t
    isParamMutable (AbstractParamDecl (VarDecl _ _ t) _) = isPtrTargetMutable t

    isParamPointer (ParamDecl (VarDecl _ _ t) _)         = isPointerType t
    isParamPointer (AbstractParamDecl (VarDecl _ _ t) _) = isPointerType t

    isExprPointer localDecls expr = case expr of
        CVar i _ -> case Map.lookup (idName i) localDecls of
            Just (_, ptr) -> return ptr
            Nothing       -> isExprPointerGlobal expr
        _ -> isExprPointerGlobal expr

    isExprPointerGlobal expr = do
        t <- tExpr [] RValue expr
        return $ isPointerType (canonicalType t)

    isPointerDecl localDecls i = do
        case Map.lookup (idName i) localDecls of
            Just (_, ptr) -> return ptr
            Nothing -> lookupObject i >>= \case
                Just (ObjectDef (ObjDef (VarDecl _ _ t) _ _)) -> return $ isPointerType t
                Just (Declaration (Decl (VarDecl _ _ t) _))   -> return $ isPointerType t
                _                                             -> return False

    isPtrTargetMutable = \case
        PtrType t _ _     -> isTypeMutable t
        ArrayType t _ _ _ -> isTypeMutable t
        _                 -> False

    isExprOwned localDecls = \case
        CVar i _ -> case Map.lookup (idName i) localDecls of
            Just (owned, _) -> return owned
            Nothing -> lookupObject i >>= \case
                Just (ObjectDef (ObjDef (VarDecl _ (DeclAttrs _ _ attrs) _) _ _)) -> return $ any isOwnedAttr attrs
                Just (Declaration (Decl (VarDecl _ (DeclAttrs _ _ attrs) _) _))   -> return $ any isOwnedAttr attrs
                _                                             -> return False
        CUnary CIndOp e _ -> do
            t <- tExpr [] RValue e
            case canonicalType t of
                PtrType t' _ _ -> return $ isOwned t'
                _              -> return False
        CMember e i _ _ -> do
            t <- tExpr [] RValue e
            case canonicalType t of
                DirectType (TyComp (CompTypeRef sueref _ _)) _ _ -> isFieldOwned sueref i
                PtrType (DirectType (TyComp (CompTypeRef sueref _ _)) _ _) _ _ -> isFieldOwned sueref i
                _ -> return False
        CCall f _ _ -> do
            fTy <- tExpr [] RValue f
            return $ case fTy of
                FunctionType (FunType rt _ _) _ -> isOwned rt
                FunctionType (FunTypeIncomplete rt) _ -> isOwned rt
                PtrType (canonicalType -> FunctionType (FunType rt _ _) _) _ _ -> isOwned rt
                PtrType (canonicalType -> FunctionType (FunTypeIncomplete rt) _) _ _ -> isOwned rt
                _ -> False
        _ -> return False

    isFieldOwned sueref i = do
        case Map.lookup sueref (gTags decls) of
            Just (CompDef (CompType _ _ members _ _)) ->
                return $ any (isMemberOwned i) members
            _ -> return False

    isMemberOwned i (MemberDecl (VarDecl (VarName i' _) (DeclAttrs _ _ attrs) t) _ _) =
        idName i == idName i' && isOwnedDecl attrs t
    isMemberOwned _ _ = False

    collectMoveEffects d s localDecls mBorrow owned expr = case expr of
        (exprPath -> Just p) -> do
            ro <- isExprOwned localDecls expr
            if owned && ro
                then return [Move p (nodeInfo expr)]
                else collectExprEffects d s localDecls mBorrow expr
        _ -> do
            ee <- collectExprEffects d s localDecls mBorrow expr
            ro <- isExprOwned localDecls expr
            let dest = fromMaybe PathReturn (fst <$> mBorrow)
            -- Only move the return value if it's not being placed into a destination by the caller.
            if owned && ro && isNothing mBorrow
                then return $ ee ++ [Move dest (nodeInfo expr)]
                else return ee

    reportErrors isExpectedOwned live effects result =
        forM_ (Map.toList result) $ \(node, lattice) ->
            let effs = Map.findWithDefault [] node effects
                nodeLive = Map.findWithDefault Set.empty node live
            in foldM_ (checkEffect isExpectedOwned nodeLive) lattice effs

    mkSpan ni label =
        let (pos, len) = posAndLen ni
        in DiagnosticSpan pos len [label]

    checkEffect isExpectedOwned nodeLive lattice = \case
        Use i ni -> do
            case Map.lookup i lattice of
                Just (StateMoved niMove) ->
                    recordRichError ni ErrorLevel ("use after move: " <> pretty i)
                        [mkSpan niMove "variable moved here"] []
                Just (StateInconsistent _ niMove) ->
                    when (isExpectedOwned i) $
                        recordRichError ni ErrorLevel ("use of variable that may be moved: " <> pretty i)
                            [mkSpan niMove "variable may be moved here"] []
                Just (StateUninitialized niDef) ->
                    recordRichError ni ErrorLevel ("use of uninitialized variable: " <> pretty i)
                        [mkSpan niDef "variable declared here"] []
                _ -> return ()
            return lattice
        Move i ni -> do
            case Map.lookup i lattice of
                Just (StateMoved niMove) ->
                    recordRichError ni ErrorLevel ("move of already moved variable: " <> pretty i)
                        [mkSpan niMove "variable first moved here"] []
                Just (StateInconsistent _ niMove) ->
                    recordRichError ni ErrorLevel ("move of variable that may be moved: " <> pretty i)
                        [mkSpan niMove "variable may be moved here"] []
                Just (StateUninitialized niDef) ->
                    recordRichError ni ErrorLevel ("move of uninitialized variable: " <> pretty i)
                        [mkSpan niDef "variable declared here"] []
                _ -> return ()
            checkBorrows nodeLive lattice i Nothing ni "move"
            return $ Map.insert i (StateMoved ni) lattice
        Define i ni -> do
            checkOverwrite isExpectedOwned lattice i ni
            checkBorrows nodeLive lattice i (Just i) ni "overwrite"
            return $ Map.insert i (StateOwned ni) lattice
        Declare i ni -> return $ Map.insert i (StateUninitialized ni) lattice
        SharedBorrow q p ni -> do
            checkOverwrite isExpectedOwned lattice q ni
            checkMutableBorrows nodeLive lattice p (Just q) ni "shared borrow"
            let origin = findOrigin p lattice
            return $ Map.insert q (StateSharedBorrowedFrom origin ni) lattice
        MutableBorrow q p ni -> do
            checkOverwrite isExpectedOwned lattice q ni
            checkAnyBorrows nodeLive lattice p (Just q) ni "mutable borrow"
            let origin = findOrigin p lattice
            return $ Map.insert q (StateMutableBorrowedFrom origin ni) lattice

    checkOverwrite isExpectedOwned lattice i ni =
        case Map.lookup i lattice of
            Just (StateOwned niOld) ->
                recordRichError ni ErrorLevel ("memory leak: " <> pretty i <> " is overwritten")
                    [mkSpan niOld "variable defined here"] []
            Just (StateInconsistent niDef _) ->
                when (isExpectedOwned i) $
                    recordRichError ni ErrorLevel ("memory leak: " <> pretty i <> " may be leaked")
                        [mkSpan niDef "variable defined here"] []
            _ -> return ()

    checkBorrows :: Liveness -> Lattice -> AccessPath -> Maybe AccessPath -> NodeInfo -> PP.Doc AnsiStyle -> Trav Env ()
    checkBorrows nodeLive lattice p mIgnore ni action =
        checkAnyBorrows nodeLive lattice p mIgnore ni action

    checkAnyBorrows :: Liveness -> Lattice -> AccessPath -> Maybe AccessPath -> NodeInfo -> PP.Doc AnsiStyle -> Trav Env ()
    checkAnyBorrows nodeLive lattice p mIgnore ni action =
        forM_ (Map.toList lattice) $ \(q, state) ->
            unless (maybe False (`isPathPrefixOf` q) mIgnore) $
            case state of
                StateSharedBorrowedFrom p' niBorrow | p `isPathPrefixOf` p' || p' `isPathPrefixOf` p ->
                    when (q `Set.member` nodeLive) $
                        recordRichError ni ErrorLevel (action <> " of " <> pretty p <> " while borrowed by " <> pretty q)
                            [mkSpan niBorrow "borrowed here"] []
                StateMutableBorrowedFrom p' niBorrow | p `isPathPrefixOf` p' || p' `isPathPrefixOf` p ->
                    when (q `Set.member` nodeLive) $
                        recordRichError ni ErrorLevel (action <> " of " <> pretty p <> " while borrowed by " <> pretty q)
                            [mkSpan niBorrow "borrowed here"] []
                _ -> return ()

    checkMutableBorrows :: Liveness -> Lattice -> AccessPath -> Maybe AccessPath -> NodeInfo -> PP.Doc AnsiStyle -> Trav Env ()
    checkMutableBorrows nodeLive lattice p mIgnore ni action =
        forM_ (Map.toList lattice) $ \(q, state) ->
            unless (maybe False (`isPathPrefixOf` q) mIgnore) $
            case state of
                StateMutableBorrowedFrom p' niBorrow | p `isPathPrefixOf` p' || p' `isPathPrefixOf` p ->
                    when (q `Set.member` nodeLive) $
                        recordRichError ni ErrorLevel (action <> " of " <> pretty p <> " while borrowed by " <> pretty q)
                            [mkSpan niBorrow "borrowed here"] []
                _ -> return ()

    checkLeaks isExpectedOwned _ _ transferFunc result =
        forM_ (Map.toList result) $ \(node, lattice) -> do
            let isLeak var state = case state of
                    StateOwned _          -> True
                    StateInconsistent _ _ -> isExpectedOwned var
                    _                     -> False

            case node of
                Node _ ExitNode ->
                    forM_ (Map.toList lattice) $ \(var, state) ->
                        unless (isTemporary var) $ case state of
                            StateOwned ni ->
                                recordRichError ni ErrorLevel ("memory leak: " <> pretty var) [] []
                            StateInconsistent ni _ | isExpectedOwned var ->
                                recordRichError ni ErrorLevel ("memory leak: " <> pretty var <> " may be leaked")
                                    [mkSpan ni "variable defined here"] []
                            _             -> return ()
                _ -> do
                    -- Check if an owned temporary is leaked (defined but not moved/consumed in the same node)
                    let nextLattice = transferFunc node lattice
                    forM_ (Map.toList nextLattice) $ \(var, state) ->
                        when (isTemporary var && isLeak var state) $
                            recordRichError (getNi state) ErrorLevel ("memory leak: " <> pretty var) [] []
      where
        getNi (StateOwned ni)                 = ni
        getNi (StateInconsistent ni _)        = ni
        getNi (StateUninitialized ni)         = ni
        getNi (StateMoved ni)                 = ni
        getNi (StateSharedBorrowedFrom _ ni)  = ni
        getNi (StateMutableBorrowedFrom _ ni) = ni

    foldM_ f a (x:xs) = f a x >>= \a' -> foldM_ f a' xs
    foldM_ _ a []     = return a


descr :: (GlobalDecls -> Trav Env (), (Text, Text))
descr = (analyse, ("borrow-check", Text.unlines
    [ "Checks for borrow-checker violations in C code."
    , ""
    , "This linter implements a Rust-like borrow checker for C. It tracks ownership,"
    , "moves, and borrows of pointers marked with `__attribute__((owned))`."
    , ""
    , "It also makes conservative assumptions about un-annotated functions to ensure"
    , "soundness: it assumes the return value of a function borrows from all its"
    , "non-owned pointer arguments."
    ]))
