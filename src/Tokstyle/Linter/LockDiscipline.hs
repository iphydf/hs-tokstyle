{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.LockDiscipline (descr) where

import           Control.Monad.State.Strict         (State)
import qualified Control.Monad.State.Strict         as State
import           Data.Fix                           (Fix (..), foldFix)
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
import           Tokstyle.Analysis.AccessPath       (AccessPath (..))
import           Tokstyle.Analysis.Dataflow         (Dataflow (..), solve)
import           Tokstyle.Cimple.Analysis.CFG       (EdgeType (..), Node (..),
                                                     NodeKind (..),
                                                     fromFunction)
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Cimple.Analysis.Threads
import           Tokstyle.Cimple.Analysis.Types
import           Tokstyle.Common                    (backticks)


-- | The tags in a doc comment.
docTags :: C.Comment (Lexeme Text) -> [C.CommentF (Lexeme Text) ()]
docTags = foldFix $ \c -> (() <$ c) : concat c

-- | The field guarding each guarded field, by struct.
guards :: [(FilePath, [C.Node (Lexeme Text)])] -> Map Text (Map Text Text)
guards tus = Map.fromListWith Map.union
    [ (name, Map.fromList fields)
    | (_, nodes) <- tus, node <- nodes, Fix n <- universe node
    , (name, members) <- case n of
        Struct (L _ _ s) ms                   -> [(s, ms)]
        Typedef (Fix (Struct _ ms)) (L _ _ s) _ -> [(s, ms)]
        _                                     -> []
    , let fields =
            [ (f, g)
            | Fix (Commented (Fix (CommentInfo doc)) (Fix (MemberDecl (Fix (VarDecl _ (L _ _ f) _)) _))) <- members
            , C.DocGuardedBy (L _ _ g) <- docTags doc ]
    , not (null fields)
    ]

-- | The locks each function requires its callers to hold: a parameter, and
-- the field of what it points to that is the lock.
requirements :: [(FilePath, [C.Node (Lexeme Text)])] -> Map Text [(Text, Text)]
requirements tus = Map.fromListWith (++)
    [ (name, [(p, f)])
    | (_, nodes) <- tus, node <- nodes
    , Fix (Commented (Fix (CommentInfo doc)) target) <- universe node
    , Just name <- [declared target]
    , C.DocRequires (L _ _ p) (L _ _ f) <- docTags doc ]
  where
    declared (Fix node) = case node of
        FunctionDecl _ proto   -> protoName proto
        FunctionDefn _ proto _ -> protoName proto
        _                      -> Nothing
    protoName = \case
        Fix (FunctionPrototype _ (L _ _ name) _) -> Just name
        _                                        -> Nothing

-- | The parameter names of each function.
paramNames :: [(FilePath, [C.Node (Lexeme Text)])] -> Map Text [Text]
paramNames tus = Map.fromList
    [ (name, [ p | Fix (VarDecl _ (L _ _ p) _) <- params ])
    | (_, nodes) <- tus, node <- nodes
    , Fix (FunctionPrototype _ (L _ _ name) params) <- universe node ]

-- | The functions that may run on any thread: the roots, and what they call.
anyThreadFunctions :: [(FilePath, [C.Node (Lexeme Text)])] -> Structs -> Map Text Function -> Set Text
anyThreadFunctions tus types defined = go Set.empty (map fst (roots (anyThread tus) types (paramTypes tus) defined))
  where
    go seen [] = seen
    go seen (f : rest)
        | f `Set.member` seen = go seen rest
        | otherwise = case Map.lookup f defined of
            Nothing -> go seen rest
            Just fn -> go (Set.insert f seen) (rest ++ [ g | Direct g <- callsIn types fn ])

pathOf :: C.Node (Lexeme Text) -> Maybe AccessPath
pathOf (Fix node) = case node of
    VarExpr (L _ _ v)         -> Just (PathVar (Text.unpack v))
    PointerAccess e (L _ _ m) -> PathField . PathDeref <$> pathOf e <*> pure (Text.unpack m)
    MemberAccess e (L _ _ m)  -> PathField <$> pathOf e <*> pure (Text.unpack m)
    ArrayAccess e i           -> PathIndex <$> pathOf e <*> pure (Text.unpack (showNodePlain i))
    UnaryExpr UopDeref e      -> PathDeref <$> pathOf e
    UnaryExpr UopAddress e    -> pathOf e
    ParenExpr e               -> pathOf e
    CastExpr _ e              -> pathOf e
    _                         -> Nothing

-- | A call: the function called, what each argument names, and the locks
-- held there.
data Site = Site Text [Maybe AccessPath] (Set AccessPath)

-- | A path with what one path starts with replaced by another.
rebase :: AccessPath -> AccessPath -> AccessPath -> Maybe AccessPath
rebase from to p
    | p == from = Just to
    | otherwise = case p of
        PathField q f -> (`PathField` f) <$> rebase from to q
        PathIndex q i -> (`PathIndex` i) <$> rebase from to q
        PathDeref q   -> PathDeref <$> rebase from to q
        _             -> Nothing

data Finding
    = Unguarded (Lexeme Text) Bool AccessPath AccessPath
    -- ^ An access, whether it writes, what it accesses, and the lock that
    -- guards it.
    | Unheld (Lexeme Text) Text AccessPath
    -- ^ A call, the function called, and the lock it requires.
    deriving (Eq, Ord)

-- | What the program says about its locks.
data Program = Program
    { progGuards   :: Map Text (Map Text Text)
    , progRequires :: Map Text [(Text, Text)]
    , progParams   :: Map Text [Text]
    , progTypes    :: Structs
    , progAny      :: Set Text
    -- ^ The functions that may run on any thread.
    , progRoots    :: Set Text
    -- ^ Those that may be called on any thread from outside the program.
    }

-- | The findings in a function, and its calls, given whether it runs on any
-- thread there and the locks held when it is called.
analyseFunction :: Program -> Bool -> Set AccessPath -> Function -> ([Finding], [Site])
analyseFunction prog strict entryHeld fn = (concat (map fst evaluated), concat (map snd evaluated))
  where
    evaluated =
        [ (found, sites)
        | (n, locks) <- Map.toList result
        , let (_, found, sites) = List.foldl' (flip eval) (locks, [], []) (nodeExprs n)
        ]
    (entry, cfg) = fromFunction (fnDefn fn)
    typeOf = typeIn (progTypes prog) fn
    initialLocks = Set.union entryHeld $ Set.fromList
        [ PathField (PathDeref (PathVar (Text.unpack p))) (Text.unpack f)
        | (p, f) <- Map.findWithDefault [] (fnName fn) (progRequires prog) ]
    -- Locals that only ever hold what a path names, by that path.
    aliases = Map.fromList
        [ (v, p) | (v, Just ini) <- localDecls (fnDefn fn)
                 , Map.notMember v (assignments (fnDefn fn))
                 , Just p <- [pathOf ini], rootOf p /= Just v ]
    pathIn e = expand (10 :: Int) <$> pathOf e
    expand 0 p = p
    expand k p = case rootOf p of
        Just v | Just a <- Map.lookup v aliases, Just p' <- rebase (PathVar v) a p -> expand (k - 1) p'
        _ -> p
    -- Locals given memory nothing else points to yet, which no other thread
    -- can see.
    fresh = Set.fromList
        [ v | (v, ini) <- localDecls (fnDefn fn)
            , any allocates (maybe [] pure ini ++ Map.findWithDefault [] v (assignments (fnDefn fn))) ]
    allocates e = case bare e of
        Fix (FunctionCall callee _) | L _ _ g <- calleeLexeme callee ->
            g `elem` ["malloc", "calloc", "mem_alloc", "mem_balloc", "mem_valloc"]
        _ -> False

    eval e acc = go e acc

    go (Fix node) acc@(locks, found, sites) = case node of
        FunctionCall callee args ->
            let (locks', found', sites') = List.foldl' (flip go) acc args
                L _ _ g = calleeLexeme callee
                lockArg = case args of
                    (a : _) -> pathIn a
                    []      -> Nothing
                unheld = [ Unheld (calleeLexeme callee) g lock
                         | (p, f) <- Map.findWithDefault [] g (progRequires prog)
                         , Just i <- [List.elemIndex p (Map.findWithDefault [] g (progParams prog))]
                         , (a : _) <- [drop i args], Just ap <- [pathIn a]
                         , let lock = PathField (PathDeref ap) (Text.unpack f)
                         , lock `Set.notMember` locks' ]
                -- No other thread can hold a lock just made.
                locks'' = case lockArg of
                    Just p | g `elem` ["pthread_mutex_lock", "pthread_mutex_init", "create_recursive_mutex"] -> Set.insert p locks'
                           | g `elem` ["pthread_mutex_unlock", "pthread_mutex_destroy"]                      -> Set.delete p locks'
                    _ -> locks'
            in (locks'', found' ++ unheld, Site g (map pathIn args) locks' : sites')
        AssignExpr l _ r ->
            let (locks', found', sites') = go r acc
                (locks'', found'', sites'') = access True l (locks', found', sites')
            in (forgetVar l locks'', found'', sites'')
        UnaryExpr op x | op `elem` [UopIncr, UopDecr] -> access True x acc
        PointerAccess{} -> access False (Fix node) acc
        MemberAccess{} -> access False (Fix node) acc
        _ -> List.foldl' (flip go) (locks, found, sites) (foldr (:) [] node)

    -- An access to a field, and then what its base expression reads.
    access isWrite e@(Fix node) acc = case node of
        PointerAccess base (L _ _ f) -> check isWrite e base f PathDeref (go base acc)
        MemberAccess base (L _ _ f)  -> check isWrite e base f id (go base acc)
        _                            -> go e acc
    check isWrite e base f through (locks, found, sites) =
        case (typeOf base >>= structName >>= (`Map.lookup` progGuards prog) >>= Map.lookup f, pathIn base) of
            (Just g, Just bp) | isWrite || strict, not (exempt bp) ->
                let lock = PathField (through bp) (Text.unpack g)
                    at = case e of
                        Fix (PointerAccess _ l) -> l
                        Fix (MemberAccess _ l)  -> l
                        _                       -> L (C.AlexPn 0 0 0) C.IdVar ""
                in if lock `Set.member` locks
                   then (locks, found, sites)
                   else (locks, found ++ [Unguarded at isWrite (PathField (through bp) (Text.unpack f)) lock], sites)
            _ -> (locks, found, sites)
    exempt bp = maybe False (`Set.member` fresh) (rootOf bp)
    -- A variable given a new value no longer names what it locked.
    forgetVar l locks = case pathOf l of
        Just (PathVar v) -> Set.filter (\p -> rootOf p /= Just v) locks
        _                -> locks

    step n locks = case n of
        Node _ (StmtNode (Fix (VarDeclStmt (Fix (VarDecl _ (L _ _ v) _)) _))) ->
            let locks' = Set.filter (\p -> rootOf p /= Just (Text.unpack v)) locks
            in first (List.foldl' (flip eval) (locks', [], []) (nodeExprs n))
        _ -> first (List.foldl' (flip eval) (locks, [], []) (nodeExprs n))
    first (x, _, _) = x
    edge n e locks = case n of
        Node _ (BranchNode cond) -> case trylock cond of
            Just (p, heldOn) | heldOn == e -> Set.insert p locks
            _                              -> locks
        _ -> locks
    -- The lock a branch tries to take, and the edge on which it holds it.
    trylock cond = case unFix' (strip cond) of
        UnaryExpr UopNot x | Just p <- tried x -> Just (p, TrueBranch)
        BinaryExpr x op r | Just p <- tried x, intLit r == Just 0 -> case op of
            BopEq -> Just (p, TrueBranch)
            BopNe -> Just (p, FalseBranch)
            _     -> Nothing
        _ | Just p <- tried cond -> Just (p, FalseBranch)
        _ -> Nothing
    tried x = case unFix' (strip x) of
        FunctionCall callee (a : _) | L _ _ "pthread_mutex_trylock" <- calleeLexeme callee -> pathIn a
        _ -> Nothing

    result = solve entry cfg Dataflow
        { transfer     = step
        , edgeTransfer = edge
        , merge        = Set.intersection
        , initial      = initialLocks
        }

-- | The ways each function may run: whether on any thread, with the locks
-- held when it is called. A function the program does not keep to itself
-- may be called with none held. A static function is called where the
-- program calls it, with the locks held at every such call, mapped to its
-- parameters. Worked out from none upwards, so a cycle of calls is assumed
-- to hold no more than it is shown to.
entryLocks :: Program -> Map Text Function -> Map (Text, Bool) (Set AccessPath)
entryLocks prog defined = go (2 * Map.size defined + 2) outside
  where
    isStatic fn = case fnDefn fn of
        Fix (FunctionDefn C.Static _ _) -> True
        _                               -> False
    -- Functions used as values may be called from anywhere.
    valued = Set.fromList
        [ n | fn <- Map.elems defined, Fix (VarExpr (L _ _ n)) <- universe (fnDefn fn), n `Map.member` defined ]
        `Set.difference` Set.fromList (concatMap callees (Map.elems defined))
    outside = Map.fromList $
        [ ((n, False), Set.empty) | (n, fn) <- Map.toList defined, not (isStatic fn) || n `Set.member` valued ]
        ++ [ ((n, True), Set.empty)
           | (n, fn) <- Map.toList defined
           , n `Set.member` progRoots prog || (not (isStatic fn) && n `Set.member` progAny prog) ]
    go :: Int -> Map (Text, Bool) (Set AccessPath) -> Map (Text, Bool) (Set AccessPath)
    go 0 entries = entries
    go k entries =
        let sites = [ (strict, site)
                    | ((n, strict), held) <- Map.toList entries, Just fn <- [Map.lookup n defined]
                    , site <- snd (analyseFunction prog strict held fn) ]
            called = Map.fromListWith Set.intersection
                [ ((g, strict), mapped g args held)
                | (strict, Site g args held) <- sites, Just gfn <- [Map.lookup g defined], isStatic gfn ]
            next = Map.unionWith Set.intersection outside called
        in if next == entries then entries else go (k - 1) next
    mapped g args held = Set.fromList
        [ l'
        | (Just a, p) <- zip args (Map.findWithDefault [] g (progParams prog))
        , l <- Set.toList held
        , Just l' <- [rebase a (PathVar (Text.unpack p)) l] ]

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus
    | Map.null (progGuards prog) && Map.null (progRequires prog) = []
    | otherwise = reverse $ State.execState (mapM_ report findings) []
  where
    defined = Map.fromList [ (fnName f, f) | f <- definitions tus ]
    types = structs tus
    prog = Program (guards tus) (requirements tus) (paramNames tus) types (anyThreadFunctions tus types defined)
        (Set.fromList (map fst (roots (anyThread tus) types (paramTypes tus) defined)))
    findings = Set.toList . Set.fromList $
        [ (fnFile fn, fnName fn, x)
        | ((n, strict), held) <- Map.toList (entryLocks prog defined), Just fn <- [Map.lookup n defined]
        , x <- fst (analyseFunction prog strict held fn) ]

report :: (FilePath, Text, Finding) -> State [Diagnostic CimplePos] ()
report (file, name, finding) = case finding of
    Unguarded at isWrite p lock -> do
        let (pos, len) = getDiagnosticInfo file at
            how = if isWrite
                then "is written here"
                else "is read here, in" <+> backticks (pretty name) <+> "that may run on any thread,"
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (render p) <+> how <+> "without" <+> backticks (render lock) <> ", which guards it")
            Nothing [] []
    Unheld at g lock -> do
        let (pos, len) = getDiagnosticInfo file at
        warnRich $ Diagnostic pos len ErrorLevel
            (backticks (pretty g) <+> "requires" <+> backticks (render lock) <> ", which is not held here")
            Nothing [] []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("lock-discipline", Text.unlines
    [ "Checks that fields tagged `@guarded_by lock` are written only with that"
    , "lock held, and read with it held where the code may run on any thread, and"
    , "that functions tagged `@requires(param, lock)` are called with the lock held."
    , ""
    , "Code that does not run on any thread runs on the thread that owns the"
    , "struct, which may read what only it writes. Memory a function has just"
    , "allocated, and what a lock it has just made guards, are its own until it"
    , "shares them."
    , ""
    , "**Reason:** another thread can change or free what an unlocked access reads"
    , "between its steps."
    ]))
