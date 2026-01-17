{-# LANGUAGE LambdaCase #-}
module Tokstyle.C.Analysis.CFG
    ( Node (..)
    , NodeKind (..)
    , EdgeType (..)
    , Edge
    , CFG
    , fromFunDef
    ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Language.C.Analysis.SemRep (FunDef (..))
import           Language.C.Data.Ident      (Ident)
import           Language.C.Syntax.AST

-- | A node in the Control Flow Graph.
data Node = Node Int NodeKind
    deriving (Show)

data NodeKind
    = EntryNode
    | ExitNode
    | StatNode CStat
    | DeclNode CDecl
    | ExprNode CExpr
    | BranchNode CExpr
    deriving (Show)

instance Eq Node where
    (Node a _) == (Node b _) = a == b

instance Ord Node where
    compare (Node a _) (Node b _) = compare a b

-- | The type of transition between nodes.
data EdgeType
    = Unconditional
    | TrueBranch
    | FalseBranch
    | CaseBranch CExpr
    | DefaultBranch
    deriving (Show)

instance Eq EdgeType where
    Unconditional == Unconditional = True
    TrueBranch == TrueBranch       = True
    FalseBranch == FalseBranch     = True
    DefaultBranch == DefaultBranch = True
    _ == _                         = False

type Edge = (EdgeType, Node)
type CFG = Map Node [Edge]

data BuildEnv = BuildEnv
    { envBreak    :: Maybe Node
    , envContinue :: Maybe Node
    , envReturn   :: Node
    , envLabels   :: Map Ident Node
    }

data BuildState = BuildState
    { stateNextId :: Int
    , stateEdges  :: CFG
    }

type BuildM = ReaderT BuildEnv (State BuildState)

newNodeState :: NodeKind -> State BuildState Node
newNodeState nk = do
    i <- gets stateNextId
    modify $ \s -> s { stateNextId = i + 1 }
    return $ Node i nk

newNode :: NodeKind -> BuildM Node
newNode = lift . newNodeState

-- | Build a CFG from a function definition.
fromFunDef :: FunDef -> (Node, CFG)
fromFunDef (FunDef _ (CCompound _ items _) _) =
    let (exitNode, s0) = runState (newNodeState ExitNode) (BuildState 0 Map.empty)
        (entryNode, s1) = runState (newNodeState EntryNode) s0
        (labels, s2) = runState (execStateT (mapM_ collectBlockItem items) Map.empty) s1
        env = BuildEnv Nothing Nothing exitNode labels
        (actualEntry, s3) = runState (runReaderT (buildBlock items exitNode) env) s2
        (_, s4) = runState (addEdgeM entryNode Unconditional actualEntry) s3
    in (entryNode, stateEdges s4)
  where
    collectBlockItem (CBlockStmt s) = collectStat s
    collectBlockItem _              = return ()

    collectStat = \case
        CLabel i s _ ni    -> do
            node <- lift $ newNodeState (StatNode (CExpr Nothing ni))
            modify $ Map.insert i node
            collectStat s
        CCompound _ bis _  -> mapM_ collectBlockItem bis
        CIf _ t e _        -> collectStat t >> maybe (return ()) collectStat e
        CSwitch _ s _      -> collectStat s
        CWhile _ s _ _     -> collectStat s
        CFor _ _ _ s _     -> collectStat s
        _                  -> return ()

fromFunDef _ = (Node 0 ExitNode, Map.empty)

addEdgeM :: Node -> EdgeType -> Node -> State BuildState ()
addEdgeM from et to = modify $ \s ->
    s { stateEdges = Map.insertWith (++) from [(et, to)] (stateEdges s) }

buildBlock :: [CBlockItem] -> Node -> BuildM Node
buildBlock [] next = return next
buildBlock (item:items) next = do
    restEntry <- buildBlock items next
    buildItem item restEntry

buildItem :: CBlockItem -> Node -> BuildM Node
buildItem (CBlockStmt s) next = buildStat s next
buildItem (CBlockDecl d) next = do
    node <- newNode (DeclNode d)
    lift $ addEdgeM node Unconditional next
    return node
buildItem (CNestedFunDef _) next = return next

buildStat :: CStat -> Node -> BuildM Node
buildStat stat next = case stat of
    CLabel i s _ _ -> do
        node <- asks (Map.findWithDefault (error "label missing") i . envLabels)
        sEntry <- buildStat s next
        lift $ addEdgeM node Unconditional sEntry
        return node

    CExpr (Just _) _ -> do
        node <- newNode (StatNode stat)
        lift $ addEdgeM node Unconditional next
        return node

    CExpr Nothing _ -> return next

    CCompound _ items _ -> buildBlock items next

    CIf cond t mElse _ -> do
        node <- newNode (BranchNode cond)
        tEntry <- buildStat t next
        eEntry <- case mElse of
            Just e  -> buildStat e next
            Nothing -> return next
        lift $ addEdgeM node TrueBranch tEntry
        lift $ addEdgeM node FalseBranch eEntry
        return node

    CWhile cond s _ _ -> do
        node <- newNode (BranchNode cond)
        sEntry <- local (\env -> env { envBreak = Just next, envContinue = Just node }) $
            buildStat s node
        lift $ addEdgeM node TrueBranch sEntry
        lift $ addEdgeM node FalseBranch next
        return node

    CFor init' mCond mStep s _ -> do
        condNode <- case mCond of
                         Just cond -> newNode (BranchNode cond)
                         Nothing   -> newNode (StatNode stat)
        stepEntry <- case mStep of
                       Just step -> do
                           stepNode <- newNode (ExprNode step)
                           lift $ addEdgeM stepNode Unconditional condNode
                           return stepNode
                       Nothing -> return condNode

        sEntry <- local (\env -> env { envBreak = Just next, envContinue = Just stepEntry }) $
            buildStat s stepEntry

        case mCond of
            Just _ -> do
                lift $ addEdgeM condNode TrueBranch sEntry
                lift $ addEdgeM condNode FalseBranch next
            Nothing ->
                lift $ addEdgeM condNode Unconditional sEntry

        case init' of
            Left (Just e) -> do
                initNode <- newNode (ExprNode e)
                lift $ addEdgeM initNode Unconditional condNode
                return initNode
            Right d -> do
                initNode <- newNode (DeclNode d)
                lift $ addEdgeM initNode Unconditional condNode
                return initNode
            Left Nothing -> return condNode

    CReturn _ _ -> do
        node <- newNode (StatNode stat)
        retNode <- asks envReturn
        lift $ addEdgeM node Unconditional retNode
        return node

    CBreak _ -> do
        node <- newNode (StatNode stat)
        asks envBreak >>= \case
            Just target -> lift $ addEdgeM node Unconditional target
            Nothing     -> return ()
        return node

    CCont _ -> do
        node <- newNode (StatNode stat)
        asks envContinue >>= \case
            Just target -> lift $ addEdgeM node Unconditional target
            Nothing     -> return ()
        return node

    CGoto i _ -> do
        node <- newNode (StatNode stat)
        asks (Map.lookup i . envLabels) >>= \case
            Just target -> lift $ addEdgeM node Unconditional target
            Nothing     -> return ()
        return node

    _ -> do
        node <- newNode (StatNode stat)
        lift $ addEdgeM node Unconditional next
        return node
