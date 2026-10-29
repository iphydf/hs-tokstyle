{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | What a function frees and allocates, followed through its body.
module Tokstyle.Linter.Frees (doubleFree, leakedField) where

import           Control.Monad.State.Strict         (State)
import qualified Control.Monad.State.Strict         as State
import           Data.Fix                           (Fix (..))
import qualified Data.Graph                         as Graph
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Language.Cimple                    (BinaryOp (..), Lexeme (..),
                                                     NodeF (..), UnaryOp (..))
import qualified Language.Cimple                    as C
import           Language.Cimple.Diagnostics        (CimplePos, Diagnostic (..),
                                                     DiagnosticLevel (..),
                                                     HasDiagnosticInfo (..),
                                                     IsPosition (..), warnRich)
import           Language.Cimple.Pretty             (showNodePlain)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath       (AccessPath (..),
                                                     isPathPrefixOf)
import           Tokstyle.Analysis.Dataflow         (Dataflow (..), solve)
import           Tokstyle.Cimple.Analysis.CFG       (EdgeType (..), Node (..),
                                                     NodeKind (..),
                                                     fromFunction)
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Common                    (backticks)


-- | Functions that free what their argument at a position points to.
freers :: Map Text Int
freers = Map.fromList [("free", 0), ("mem_delete", 1)]

-- | Functions that return memory nothing else points to.
allocators :: Set Text
allocators = Set.fromList ["malloc", "calloc", "mem_alloc", "mem_balloc", "mem_valloc"]

-- | What the functions of the program do with memory.
data Summary = Summary
    { sumFrees  :: Map Text (Map Outcome (Set Int))
    -- ^ The arguments each function frees, by the outcome it reports.
    , sumFresh  :: Set Text
    -- ^ The functions that may return memory they allocated.
    , sumShapes :: Map Text Shape
    }

-- | The arguments a call frees, when it has one of these outcomes.
freedBy :: Summary -> Text -> [Outcome] -> [Int]
freedBy summary g outcomes = case Map.lookup g freers of
    Just i  -> [i]
    Nothing -> Set.toList . Set.unions $
        [ Map.findWithDefault Set.empty o byOutcome
        | Just byOutcome <- [Map.lookup g (sumFrees summary)], o <- outcomes ]

-- | Summaries of every function, worked out callees first. Without recursion
-- each function needs one look; the few cycles there are get repeated looks.
summarise :: [Function] -> Summary
summarise fns = List.foldl' component (Summary Map.empty fresh shapes) (Graph.stronglyConnComp graph)
  where
    shapes = Map.fromList [ (fnName f, fnShape f) | f <- fns ]
    graph = [ (f, fnName f, callees f) | f <- fns ]
    component s = \case
        Graph.AcyclicSCC f -> add s f
        Graph.CyclicSCC fs -> cycleFix s fs (length fs + 2 :: Int)
    add s f = case snd (analyseFunction s f) of
        frees | Map.null frees -> s
              | otherwise      -> s { sumFrees = Map.insert (fnName f) frees (sumFrees s) }
    cycleFix s _ 0 = s
    cycleFix s fs k =
        let s' = List.foldl' add s fs
            look x = map (\f -> Map.lookup (fnName f) (sumFrees x)) fs
        in if look s' == look s then s' else cycleFix s' fs (k - 1)

    fresh = fixpoint allocators
    fixpoint s = let s' = List.foldl' freshStep s fns in if s' == s then s else fixpoint s'
    freshStep s fn
        | any (isFresh s) (returned fn) = Set.insert (fnName fn) s
        | otherwise = s
    -- What a function returns, and for a returned local, what it was given.
    returned fn = concat
        [ case bare e of
            Fix (VarExpr (L _ _ v)) -> givenTo fn (Text.unpack v)
            e'                      -> [e']
        | Fix (Return (Just e)) <- universe (fnDefn fn) ]
    givenTo fn v = [ r | (v', Just r) <- localDecls (fnDefn fn), v' == v ]
        ++ Map.findWithDefault [] v (assignments (fnDefn fn))

-- | Whether an expression gets memory nothing else points to.
isFresh :: Set Text -> C.Node (Lexeme Text) -> Bool
isFresh fresh e = case bare e of
    Fix (FunctionCall callee _) | L _ _ g <- calleeLexeme callee -> g `Set.member` fresh
    _                                                            -> False

bare :: C.Node (Lexeme Text) -> C.Node (Lexeme Text)
bare = \case
    Fix (ParenExpr e)  -> bare e
    Fix (CastExpr _ e) -> bare e
    e                  -> e

pathOf :: C.Node (Lexeme Text) -> Maybe AccessPath
pathOf (Fix node) = case node of
    VarExpr (L _ _ v)         -> Just (PathVar (Text.unpack v))
    PointerAccess e (L _ _ m) -> PathField . PathDeref <$> pathOf e <*> pure (Text.unpack m)
    MemberAccess e (L _ _ m)  -> PathField <$> pathOf e <*> pure (Text.unpack m)
    ArrayAccess e i           -> PathIndex <$> pathOf e <*> pure (Text.unpack (showNodePlain i))
    UnaryExpr UopDeref e      -> PathDeref <$> pathOf e
    ParenExpr e               -> pathOf e
    CastExpr _ e              -> pathOf e
    _                         -> Nothing

-- | What holds on the paths to a point in a function.
data Facts = Facts
    { freed :: Map AccessPath (Lexeme Text)
    -- ^ What may be freed, and the call that freed it.
    , owned :: Map AccessPath (Lexeme Text)
    -- ^ Fields the function allocated that may still hold that memory, and
    -- where it allocated it.
    } deriving (Eq)

joinFacts :: Facts -> Facts -> Facts
joinFacts (Facts a b) (Facts c d) = Facts (Map.union a c) (Map.union b d)

data Finding
    = DoubleFree (Lexeme Text) AccessPath (Lexeme Text)
    -- ^ A free, what it frees, and an earlier free of it.
    | LeakedField (Lexeme Text) AccessPath AccessPath (Lexeme Text)
    -- ^ A free, what it frees, a field of that still holding memory, and
    -- where it got it.
    deriving (Eq, Ord)

-- | Forget what is known about a path and everything under it, and for a
-- variable, the elements it indexes.
forget :: AccessPath -> Facts -> Facts
forget p (Facts f o) = Facts (Map.filterWithKey keep f) (Map.filterWithKey keep o)
  where
    keep k _ = not (p `isPathPrefixOf` k) && not (indexedBy k)
    indexedBy k = case p of
        PathVar v -> any (elem (Text.pack v) . Text.split (not . isIdent) . Text.pack) (indices k)
        _         -> False
    isIdent c = c == '_' || c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] || c `elem` ['0'..'9']
    indices = \case
        PathIndex q i -> i : indices q
        PathField q _ -> indices q
        PathDeref q   -> indices q
        _             -> []

-- | How evaluating an expression changes the facts, with what it finds,
-- given the outcomes some calls are known to have had.
evalExpr :: Summary -> [(Lexeme Text, Outcome)] -> C.Node (Lexeme Text) -> (Facts, [Finding]) -> (Facts, [Finding])
evalExpr summary known = go
  where
    go (Fix node) acc = case node of
        AssignExpr l _ r ->
            let (facts, found) = go r acc
            in (maybe facts (\p -> assign p r facts) (pathOf l), found)
        UnaryExpr op x | op `elem` [UopIncr, UopDecr], Just p <- pathOf x ->
            let (facts, found) = go x acc in (forget p facts, found)
        -- What has its address taken may be changed through it.
        UnaryExpr UopAddress x | Just p <- pathOf x ->
            let (facts, found) = go x acc in (forget p facts, found)
        FunctionCall callee args ->
            call callee args (List.foldl' (flip go) acc args)
        _ -> List.foldl' (flip go) acc (foldr (:) [] node)

    assign p r facts =
        let facts' = forget p facts
            -- Memory stored somewhere else belongs to that place now.
            moved = case pathOf (bare r) of
                Just q  -> facts' { owned = Map.delete q (owned facts') }
                Nothing -> facts'
        in case bare r of
            Fix (FunctionCall callee _) | isFresh (sumFresh summary) r, isField p ->
                moved { owned = Map.insert p (calleeLexeme callee) (owned moved) }
            _ -> moved
    isField = \case
        PathField q _ -> throughPointer q || isDeref q
        _             -> False
    isDeref = \case
        PathDeref{} -> True
        _           -> False

    call callee args (facts, found) =
        let here@(L _ _ g) = calleeLexeme callee
            outcomes = case lookup here known of
                Just o  -> [o]
                Nothing -> [minBound .. maxBound]
            freedArgs = [ p | i <- freedBy summary g outcomes, (a : _) <- [drop i args], Just p <- [pathOf a] ]
            free (fs, found') p =
                let twice = [ DoubleFree here p at | Just at <- [Map.lookup p (freed fs)] ]
                    -- Only a function that frees no more than the memory it
                    -- is given leaves the fields of that memory behind.
                    leaks = [ LeakedField here p q at
                            | g `Map.member` freers
                            , (q, at) <- Map.toList (owned fs), q /= p, p `isPathPrefixOf` q ]
                    fs' = forget p fs
                in (fs' { freed = Map.insert p here (freed fs') }, found' ++ twice ++ leaks)
        in List.foldl' free (facts, found) freedArgs

-- | The findings in a function, and the parameters it frees by the outcome it
-- reports.
analyseFunction :: Summary -> Function -> ([Finding], Map Outcome (Set Int))
analyseFunction summary fn = (findings, frees)
  where
    (entry, cfg) = fromFunction (fnDefn fn)
    decls = localDecls (fnDefn fn)
    assigned = assignments (fnDefn fn)
    -- The locals that only hold enum constants, whose values are followed.
    flagVars = Set.fromList
        [ v | (v, ini) <- decls
            , let given = maybe [] pure ini ++ Map.findWithDefault [] v assigned
            , not (null given), all (maybe False isEnumConst . constName) given ]

    -- A branch's condition is evaluated on each edge out of it, knowing the
    -- outcomes it implies there.
    run n facts = case n of
        Node _ BranchNode{} -> (facts, [])
        _ -> List.foldl' (flip (evalExpr summary [])) (declare n facts, []) (nodeExprs n)
    declare n facts = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) _))) -> forget (PathVar (Text.unpack v)) facts
        _ -> facts
    onEdge cond e facts = evalExpr summary (knownOn cond e) cond (facts, [])

    setFlags n flags = List.foldl' setFlag flags $ case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) -> [(v, ini)]
        _ -> [ (v, Just r) | e <- nodeExprs n, Fix (AssignExpr (Fix (VarExpr (L _ _ v))) _ r) <- universe e ]
    setFlag flags (v, ini)
        | name `Set.member` flagVars, Just c <- ini >>= constName = Map.insert name c flags
        | otherwise = Map.delete name flags
      where name = Text.unpack v

    -- The facts on the paths to a point are kept by the values the flags
    -- have on them. No entries means no path gets there.
    step n st = Map.fromListWith joinFacts
        [ (setFlags n flags, fst (run n facts)) | (flags, facts) <- Map.toList st ]

    edge n e st = case n of
        Node _ (BranchNode cond) ->
            Map.map (dropNull cond e . fst . onEdge cond e) (Map.filterWithKey (\flags _ -> holds flags cond e) st)
        _ -> st
    -- The outcomes of the calls a branch implies on one of its edges.
    knownOn cond e = case (unFix' (strip cond), e) of
        (BinaryExpr l BopAnd r, TrueBranch) -> knownOn l e ++ knownOn r e
        (BinaryExpr l BopOr r, FalseBranch) -> knownOn l e ++ knownOn r e
        _ -> case (branchOn (sumShapes summary) cond, e) of
            (Just (l, o), TrueBranch)  -> [(l, o)]
            (Just (l, o), FalseBranch) -> [(l, if o == Success then Failure else Success)]
            _                          -> []
    -- Where a pointer is null, it holds no memory.
    dropNull cond e facts = case nullOn cond of
        Just (p, on) | on == e -> facts { owned = Map.delete p (owned facts) }
        _                      -> facts
    nullOn cond = case unFix' (strip cond) of
        UnaryExpr UopNot x | Just p <- pathOf x -> Just (p, TrueBranch)
        BinaryExpr x op r | constName r `elem` [Just "nullptr", Just "NULL"], Just p <- pathOf x -> case op of
            BopEq -> Just (p, TrueBranch)
            BopNe -> Just (p, FalseBranch)
            _     -> Nothing
        _ | Just p <- pathOf cond -> Just (p, FalseBranch)
        _ -> Nothing
    -- Whether a branch on a flag can be taken with the values the flags have.
    holds flags cond e = case unFix' (strip cond) of
        BinaryExpr (Fix (VarExpr (L _ _ v))) op r
            | Just c <- constName r, Just c' <- Map.lookup (Text.unpack v) flags, op `elem` [BopEq, BopNe] ->
                let equal = c == c'
                in case e of
                    TrueBranch  -> if op == BopEq then equal else not equal
                    FalseBranch -> if op == BopEq then not equal else equal
                    _           -> True
        _ -> True

    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = edge
        , merge        = Map.unionWith joinFacts
        , initial      = Map.singleton Map.empty (Facts Map.empty Map.empty)
        }

    findings = Set.toList . Set.fromList $ concat
        [ found
        | (n, st) <- Map.toList result
        , (flags, facts) <- Map.toList st
        , found <- case n of
            Node _ (BranchNode cond) ->
                [ snd (onEdge cond e facts) | (e, _) <- Map.findWithDefault [] n cfg, holds flags cond e ]
            _ -> [snd (run n facts)]
        ]

    frees = Map.fromListWith Set.union
        [ (o, Set.fromList [ i | PathVar v <- Map.keys (freed after), Just i <- [List.elemIndex (Text.pack v) (fnParams fn)] ])
        | (n, edges) <- Map.toList cfg
        , any (isExit . snd) edges
        , Just st <- [Map.lookup n result]
        , facts <- Map.elems st
        , let after = fst (run n facts)
        , o <- case n of
            Node _ (StmtNode (Fix (Return e))) -> returnOutcomes (fnShape fn) e
            _                                  -> [Success]
        ]
    isExit (Node _ ExitNode) = True
    isExit _                 = False

analyse :: (Finding -> Bool) -> [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse wanted tus = reverse $ State.execState (mapM_ report findings) []
  where
    fns = definitions tus
    summary = summarise fns
    findings = [ (fnFile f, x) | f <- fns, x <- fst (analyseFunction summary f), wanted x ]

report :: (FilePath, Finding) -> State [Diagnostic CimplePos] ()
report (file, finding) = case finding of
    DoubleFree here p at -> do
        let (pos, len) = getDiagnosticInfo file here
            (atPos, _) = getDiagnosticInfo file at
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render p) <+> "is freed here, and may already be freed at line" <+> pretty (posLine atPos))
            Nothing [] []
    LeakedField here p q at -> do
        let (pos, len) = getDiagnosticInfo file here
            (atPos, _) = getDiagnosticInfo file at
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render p) <+> "is freed here while" <+> backticks (render q)
                <+> "may still hold the memory it got at line" <+> pretty (posLine atPos))
            Nothing [] []

isDoubleFree :: Finding -> Bool
isDoubleFree = \case
    DoubleFree{} -> True
    _            -> False

doubleFree :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
doubleFree = (analyse isDoubleFree, ("double-free", Text.unlines
    [ "Checks that nothing is freed again where it may already be freed."
    , ""
    , "**Reason:** the second free corrupts the allocator's bookkeeping, or frees"
    , "memory that was allocated again in the meantime."
    ]))

leakedField :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
leakedField = (analyse (not . isDoubleFree), ("leaked-field", Text.unlines
    [ "Checks that memory a function stores in a field is freed before the struct"
    , "holding the field is freed with `free` or `mem_delete`."
    , ""
    , "**Reason:** those free only the struct, so the field's memory is lost, as"
    , "on an error path that frees what it half built."
    ]))
