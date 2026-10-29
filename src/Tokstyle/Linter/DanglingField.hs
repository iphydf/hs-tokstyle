{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.DanglingField (descr) where

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
                                                     warnRich)
import           Language.Cimple.Pretty             (showNodePlain)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Analysis.AccessPath       (AccessPath (..),
                                                     isPathPrefixOf)
import           Tokstyle.Analysis.Dataflow         (Dataflow (..), solve)
import           Tokstyle.Cimple.Analysis.CFG       (EdgeType (..), Node (..),
                                                     NodeKind (..),
                                                     fromFunction, getFuncName)
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Common                    (backticks)


-- | Functions that free the object their argument at this position points to.
freers :: [(Text, Int)]
freers = [("free", 0), ("mem_delete", 1)]

-- | Where a field got freed: the file, the call that freed it, and the path
-- the field had there.
data Origin = Origin FilePath (Lexeme Text) AccessPath
    deriving (Eq, Ord)

-- | A step from a parameter to one of its fields.
data Step = Field String | Index String | Deref
    deriving (Eq, Ord)

-- | What a call does to its arguments when it returns with some outcome.
data Effects = Effects
    { effFrees  :: Set Int
    -- ^ The parameters it frees.
    , effLeaves :: Map (Int, [Step]) Origin
    -- ^ The fields of its parameters it leaves freed.
    } deriving (Eq)

instance Semigroup Effects where
    Effects a b <> Effects c d = Effects (Set.union a c) (Map.union b d)

instance Monoid Effects where
    mempty = Effects Set.empty Map.empty

-- | What is known about the functions of the program.
data Summary = Summary
    { effects :: Map Text (Map Outcome Effects)
    , shapes  :: Map Text Shape
    }

-- | What holds on the paths to a point in a function.
data Facts = Facts
    { dangling :: Map AccessPath Origin
    -- ^ Freed fields that still point at what they pointed at.
    , freed    :: Set String
    -- ^ Variables whose object is freed.
    } deriving (Eq)

joinFacts :: Facts -> Facts -> Facts
joinFacts (Facts a b) (Facts c d) = Facts (Map.union a c) (Set.union b d)

-- | The enum constants the error-flag locals hold.
type Flags = Map String Text

-- | The facts on the paths to a point, by the values the flags have on them.
-- No entries means no path gets there.
type St = Map Flags Facts

-- | What the analysis of a function knows about its body.
data Context = Context
    { ctxFile    :: FilePath
    , ctxParams  :: [Text]
    , ctxAliases :: Map String String
    -- ^ Locals that only ever hold another variable, by that variable.
    , ctxFlags   :: Set String
    -- ^ Locals that only ever hold enum constants.
    , ctxSlots   :: Map String [Text]
    -- ^ Locals got from @get_*(container, index)@, by those arguments.
    }

contextOf :: Function -> Context
contextOf fn = Context
    { ctxFile = fnFile fn
    , ctxParams = fnParams fn
    , ctxAliases = Map.fromList
        [ (v, x) | (v, Just ini) <- decls, Map.notMember v assigned, Just x <- [plainVar ini] ]
    , ctxFlags = Set.fromList
        [ v | (v, ini) <- decls, all (maybe False isEnumConst . constName) (maybe [] pure ini ++ Map.findWithDefault [] v assigned)
            , not (null (maybe [] pure ini ++ Map.findWithDefault [] v assigned)) ]
    , ctxSlots = Map.fromList
        [ (v, map showNodePlain args)
        | (v, ini) <- decls, e <- maybe [] pure ini ++ Map.findWithDefault [] v assigned
        , Fix (FunctionCall callee args) <- [strip e], Just name <- [getFuncName callee]
        , "get_" `Text.isPrefixOf` name || "_get" `Text.isInfixOf` name, length args >= 2 ]
    }
  where
    decls = localDecls (fnDefn fn)
    assigned = assignments (fnDefn fn)
    plainVar e = case unFix' (strip e) of
        VarExpr (L _ _ x) -> Just (Text.unpack x)
        CastExpr _ x      -> plainVar x
        _                 -> Nothing

-- | The variable a local stands for.
canonical :: Context -> String -> String
canonical ctx = go (10 :: Int)
  where
    go 0 v = v
    go k v = maybe v (go (k - 1)) (Map.lookup v (ctxAliases ctx))

pathOf :: Context -> C.Node (Lexeme Text) -> Maybe AccessPath
pathOf ctx (Fix node) = case node of
    VarExpr (L _ _ v)         -> Just (PathVar (canonical ctx (Text.unpack v)))
    PointerAccess e (L _ _ m) -> PathField . PathDeref <$> pathOf ctx e <*> pure (Text.unpack m)
    MemberAccess e (L _ _ m)  -> PathField <$> pathOf ctx e <*> pure (Text.unpack m)
    ArrayAccess e i           -> PathIndex <$> pathOf ctx e <*> pure (Text.unpack (showNodePlain i))
    UnaryExpr UopDeref e      -> PathDeref <$> pathOf ctx e
    ParenExpr e               -> pathOf ctx e
    CastExpr _ e              -> pathOf ctx e
    _                         -> Nothing

-- | The steps from the root of a path to its end.
stepsOf :: AccessPath -> [Step]
stepsOf = reverse . go
  where
    go = \case
        PathField p s -> Field s : go p
        PathIndex p s -> Index s : go p
        PathDeref p   -> Deref : go p
        _             -> []

extend :: AccessPath -> [Step] -> AccessPath
extend = List.foldl' $ \p -> \case
    Field s -> PathField p s
    Index s -> PathIndex p s
    Deref   -> PathDeref p

-- | Forget what is known about a path and everything under it.
forget :: AccessPath -> Facts -> Facts
forget p (Facts d f) = Facts (Map.filterWithKey (\k _ -> not (p `isPathPrefixOf` k)) d) (maybe f (`Set.delete` f) (varOf p))
  where
    varOf (PathVar v) = Just v
    varOf _           = Nothing

-- | How evaluating an expression changes the facts on a path, given the
-- outcome some call is known to have had.
evalExpr :: Summary -> Context -> Maybe (Lexeme Text, Outcome) -> C.Node (Lexeme Text) -> (Flags, Facts) -> (Flags, Facts)
evalExpr summary ctx known = go
  where
    go (Fix node) st = case node of
        FunctionCall callee args ->
            let (flags', facts') = List.foldl' (flip go) st args
            in (flags', call callee args facts')
        AssignExpr l _ r ->
            let (flags', facts') = go r st
                assigned = maybe facts' (`forget` facts') (pathOf ctx l)
            in case (unFix' (strip l), constName r) of
                (VarExpr (L _ _ v), Just c) | Text.unpack v `Set.member` ctxFlags ctx ->
                    (Map.insert (Text.unpack v) c flags', assigned)
                _ -> (flags', assigned)
        _ -> List.foldl' (flip go) st (foldr (:) [] node)

    call callee args facts = case getFuncName callee of
        Nothing -> List.foldl' passArg facts args
        Just fn ->
            let direct = Effects (Set.fromList [ i | (f, i) <- freers, f == fn ]) Map.empty
                byOutcome = Map.findWithDefault Map.empty fn (effects summary)
                outcomes = case known of
                    Just (l, o) | l == calleeLexeme callee -> [o]
                    _                                      -> [minBound .. maxBound]
                eff = direct <> mconcat [ Map.findWithDefault mempty o byOutcome | o <- outcomes ]
                handled = effFrees eff `Set.union` Set.fromList (map fst (Map.keys (effLeaves eff)))
                arg i = case drop i args of
                    (a : _) -> pathOf ctx a
                    []      -> Nothing
                freeArg i fs = case arg i of
                    Just p@(PathVar v) -> (forget p fs) { freed = Set.insert v (freed fs) }
                    Just p -> (forget p fs) { dangling = Map.insert p (Origin (ctxFile ctx) (calleeLexeme callee) p) (dangling (forget p fs)) }
                    Nothing -> fs
                leaveArg fs ((i, steps), origin) = case arg i of
                    Just p  -> fs { dangling = Map.insert (extend p steps) origin (dangling fs) }
                    Nothing -> fs
                others = [ a | (i, a) <- zip [0 ..] args, i `Set.notMember` handled ]
                slotsTouched = [ PathVar v | (v, key) <- Map.toList (ctxSlots ctx), not ("get_" `Text.isPrefixOf` fn)
                                           , all (`elem` map showNodePlain args) key ]
                afterFree = List.foldl' (flip freeArg) facts (Set.toList (effFrees eff))
                afterLeave = List.foldl' leaveArg afterFree (Map.toList (effLeaves eff))
            in List.foldl' (flip forget) (List.foldl' passArg afterLeave others) slotsTouched

    -- A variable passed on to anything else, or the address of something,
    -- may get its fields reset there.
    passArg facts a = case unFix' (strip a) of
        UnaryExpr UopAddress x | Just p <- pathOf ctx x -> forget p facts
        _ -> case pathOf ctx a of
            Just p@(PathVar _) -> forget p facts
            _                  -> facts

joinSt :: St -> St -> St
joinSt = Map.unionWith joinFacts

mapSt :: ((Flags, Facts) -> (Flags, Facts)) -> St -> St
mapSt f = Map.fromListWith joinFacts . map f . Map.toList

-- | The facts at each return of a function, with the outcomes it reports
-- there.
atReturns :: Summary -> Function -> [([Outcome], Facts)]
atReturns summary fn =
    [ (outcomes, facts)
    | (n, edges) <- Map.toList cfg
    , any (isExit . snd) edges
    , Just st <- [Map.lookup n result]
    , let (outcomes, st') = case n of
              Node _ (StmtNode (Fix (Return e))) -> (returnOutcomes (fnShape fn) e, step n st)
              _                                  -> ([Success], step n st)
    , facts <- Map.elems st'
    ]
  where
    ctx = contextOf fn
    (entry, cfg) = fromFunction (fnDefn fn)
    isExit (Node _ ExitNode) = True
    isExit _                 = False
    declare n st = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) ini))) ->
            let name = Text.unpack v
                setFlag flags = case ini >>= constName of
                    Just c | name `Set.member` ctxFlags ctx -> Map.insert name c flags
                    _                                       -> Map.delete name flags
            in mapSt (\(flags, facts) -> (setFlag flags, forget (PathVar name) facts)) st
        _ -> st
    step n st = case n of
        Node _ BranchNode{} -> st
        _ -> declare n (List.foldl' (\s e -> mapSt (evalExpr summary ctx Nothing e) s) st (nodeExprs n))
    edge n e st = case n of
        Node _ (BranchNode cond) ->
            let known = case (branchOn (shapes summary) cond, e) of
                    (Just (l, o), TrueBranch)  -> Just (l, o)
                    (Just (l, o), FalseBranch) -> Just (l, if o == Success then Failure else Success)
                    _                          -> Nothing
            in Map.map (dropNull cond e)
                . Map.filterWithKey (\flags _ -> holds flags cond e)
                $ mapSt (evalExpr summary ctx known cond) st
        _ -> st
    -- Where a pointer is null, nothing it points to dangles.
    dropNull cond e facts = case (nullOn cond, e) of
        (Just (p, TrueBranch), TrueBranch)   -> forgetUnder p facts
        (Just (p, FalseBranch), FalseBranch) -> forgetUnder p facts
        _                                    -> facts
    forgetUnder p facts = facts { dangling = Map.filterWithKey (\k _ -> not (p `isPathPrefixOf` k) || k == p) (dangling facts) }
    -- The pointer a condition tests for null, and the branch on which it is.
    nullOn cond = case unFix' (strip cond) of
        UnaryExpr UopNot x | Just p <- pathOf ctx x -> Just (p, TrueBranch)
        BinaryExpr x op r | constName r `elem` [Just "nullptr", Just "NULL"], Just p <- pathOf ctx x -> case op of
            BopEq -> Just (p, TrueBranch)
            BopNe -> Just (p, FalseBranch)
            _     -> Nothing
        _ | Just p <- pathOf ctx cond -> Just (p, FalseBranch)
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
        , merge        = joinSt
        , initial      = Map.singleton Map.empty (Facts Map.empty Set.empty)
        }

-- | What a function does to its parameters, by outcome.
effectsOf :: Summary -> Function -> Map Outcome Effects
effectsOf summary fn = Map.fromListWith (<>)
    [ (o, Effects
        (Set.fromList [ i | v <- Set.toList (freed facts), Just i <- [paramIndex v] ])
        (Map.fromList [ ((i, stepsOf p), origin) | (p, origin) <- Map.toList (dangling facts), Just r <- [rootOf p], Just i <- [paramIndex r] ]))
    | (outcomes, facts) <- atReturns summary fn, o <- outcomes ]
  where
    paramIndex v = List.elemIndex (Text.pack v) (fnParams fn)

-- | Summaries of every function, worked out callees first. Without recursion
-- each function needs one look; the few cycles there are get repeated looks.
summarise :: [Function] -> Summary
summarise fns = List.foldl' component (Summary Map.empty shapeMap) (Graph.stronglyConnComp graph)
  where
    shapeMap = Map.fromList [ (fnName f, fnShape f) | f <- fns ]
    graph = [ (f, fnName f, callees f) | f <- fns ]
    component s = \case
        Graph.AcyclicSCC f -> add s f
        Graph.CyclicSCC fs -> cycleFix s fs (length fs + 2 :: Int)
    add s f = s { effects = Map.insertWith (Map.unionWith (<>)) (fnName f) (effectsOf s f) (effects s) }
    cycleFix s _ 0 = s
    cycleFix s fs k =
        let s' = List.foldl' add s fs
        in if map (\f -> Map.lookup (fnName f) (effects s')) fs == map (\f -> Map.lookup (fnName f) (effects s)) fs
           then s' else cycleFix s' fs (k - 1)

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ State.execState (mapM_ report (Map.toList reports)) []
  where
    fns = definitions tus
    summary = summarise fns
    reports = Map.fromListWith (++)
        [ (o, [(fnName f, p)])
        | f <- fns
        , (_, facts) <- atReturns summary f
        , (p, o) <- Map.toList (dangling facts)
        , throughPointer p
        , Just r <- [rootOf p]
        , Text.pack r `notElem` fnParams f ]

report :: (Origin, [(Text, AccessPath)]) -> State [Diagnostic CimplePos] ()
report (Origin file at freedPath, sites) = case List.sort sites of
    [] -> return ()
    (fn, p) : _ -> do
        let (pos, len) = getDiagnosticInfo file at
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render freedPath) <+> "is freed here, and" <+> backticks (render p)
                <+> "still points at it when" <+> backticks (pretty fn) <+> "returns")
            Nothing [] []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("dangling-field", Text.unlines
    [ "Checks that a struct field whose object gets freed is set to something else"
    , "before the struct is used again."
    , ""
    , "**Reason:** the struct outlives the function, and the next code that reads"
    , "the field uses freed memory."
    ]))

