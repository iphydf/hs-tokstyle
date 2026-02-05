{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.CFG
    ( Node (..)
    , NodeKind (..)
    , EdgeType (..)
    , Edge
    , CFG
    , fromFunction
    , getFuncName
    ) where

import           Control.Monad        (foldM_, forM)
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Fix             (Fix (..), unFix)
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map
import           Data.Text            (Text)
import           Language.Cimple      (Lexeme (..), NodeF (..))
import qualified Language.Cimple      as C

-- | A node in the Control Flow Graph.
data Node = Node Int NodeKind
    deriving (Show)

data NodeKind
    = EntryNode
    | ExitNode
    | StmtNode (C.Node (Lexeme Text))
    | BranchNode (C.Node (Lexeme Text))
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
    | CaseBranch (C.Node (Lexeme Text))
    | DefaultBranch
    deriving (Show)

instance Eq EdgeType where
    Unconditional == Unconditional = True
    TrueBranch == TrueBranch       = True
    FalseBranch == FalseBranch     = True
    DefaultBranch == DefaultBranch = True
    CaseBranch _ == CaseBranch _   = True
    _ == _                         = False

type Edge = (EdgeType, Node)
type CFG = Map Node [Edge]

data BuildEnv = BuildEnv
    { envBreak    :: Maybe Node
    , envContinue :: Maybe Node
    , envReturn   :: Node
    , envLabels   :: Map Text Node
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

getFuncName :: C.Node (Lexeme Text) -> Maybe Text
getFuncName (Fix (C.VarExpr (C.L _ _ name)))               = Just name
getFuncName (Fix (C.LiteralExpr C.ConstId (C.L _ _ name))) = Just name
getFuncName (Fix (C.ParenExpr e))                          = getFuncName e
getFuncName _                                              = Nothing

isTerminating :: C.Node (Lexeme Text) -> Bool
isTerminating (Fix node) = case node of
    C.Return _ -> True
    C.Break    -> True
    C.Continue -> True
    C.Goto _   -> True
    C.ExprStmt e -> isTerminating e
    C.FunctionCall f _ -> case getFuncName f of
        Just "abort"        -> True
        Just "exit"         -> True
        Just "LOGGER_FATAL" -> True
        _                   -> False
    _ -> False

addEdgeM :: Node -> EdgeType -> Node -> State BuildState ()
addEdgeM from et to = modify $ \s ->
    s { stateEdges = Map.insertWith (++) from [(et, to)] (stateEdges s) }

-- | Build a CFG from a function definition.
fromFunction :: C.Node (Lexeme Text) -> (Node, CFG)
fromFunction (Fix (FunctionDefn _ _ body)) =
    let (exitNode, s0) = runState (newNodeState ExitNode) (BuildState 0 Map.empty)
        (entryNode, s1) = runState (newNodeState EntryNode) s0
        (labels, s2) = runState (execStateT (collectLabels body) Map.empty) s1
        env = BuildEnv Nothing Nothing exitNode labels
        (actualEntry, s3) = runState (runReaderT (buildStmt body exitNode) env) s2
        (_, s4) = runState (addEdgeM entryNode Unconditional actualEntry) s3
    in (entryNode, stateEdges s4)
fromFunction _ = (Node 0 ExitNode, Map.empty)

collectLabels :: C.Node (Lexeme Text) -> StateT (Map Text Node) (State BuildState) ()
collectLabels (Fix node) = case node of
    Label (L _ _ name) body -> do
        node' <- lift $ newNodeState (StmtNode (Fix node))
        modify $ Map.insert name node'
        collectLabels body
    CompoundStmt stmts -> mapM_ collectLabels stmts
    IfStmt _ t e -> collectLabels t >> maybe (return ()) collectLabels e
    WhileStmt _ body -> collectLabels body
    DoWhileStmt body _ -> collectLabels body
    ForStmt _ _ _ body -> collectLabels body
    SwitchStmt _ cases -> mapM_ collectLabels cases
    PreprocIf _ stmts next -> mapM_ collectLabels stmts >> collectLabels next
    PreprocIfdef _ stmts next -> mapM_ collectLabels stmts >> collectLabels next
    PreprocIfndef _ stmts next -> mapM_ collectLabels stmts >> collectLabels next
    PreprocElif _ stmts next -> mapM_ collectLabels stmts >> collectLabels next
    PreprocElse stmts -> mapM_ collectLabels stmts
    PreprocScopedDefine _ stmts _ -> mapM_ collectLabels stmts
    _ -> return ()

buildStmt :: C.Node (Lexeme Text) -> Node -> BuildM Node
buildStmt (Fix node) next = case node of
    CompoundStmt stmts -> buildStmts stmts next

    IfStmt cond thenBranch elseBranchM -> do
        node' <- newNode (BranchNode cond)
        tEntry <- buildStmt thenBranch next
        eEntry <- case elseBranchM of
            Just e  -> buildStmt e next
            Nothing -> return next
        lift $ addEdgeM node' TrueBranch tEntry
        lift $ addEdgeM node' FalseBranch eEntry
        return node'

    WhileStmt cond body -> do
        node' <- newNode (BranchNode cond)
        bEntry <- local (\env -> env { envBreak = Just next, envContinue = Just node' }) $
            buildStmt body node'
        lift $ addEdgeM node' TrueBranch bEntry
        lift $ addEdgeM node' FalseBranch next
        return node'

    DoWhileStmt body cond -> do
        condNode <- newNode (BranchNode cond)
        bEntry <- local (\env -> env { envBreak = Just next, envContinue = Just condNode }) $
            buildStmt body condNode
        lift $ addEdgeM condNode TrueBranch bEntry
        lift $ addEdgeM condNode FalseBranch next
        return bEntry

    ForStmt init' cond step body -> do
        condNode <- newNode (BranchNode cond)
        stepNode <- newNode (StmtNode step)
        lift $ addEdgeM stepNode Unconditional condNode

        bEntry <- local (\env -> env { envBreak = Just next, envContinue = Just stepNode }) $
            buildStmt body stepNode
        lift $ addEdgeM condNode TrueBranch bEntry
        lift $ addEdgeM condNode FalseBranch next

        initEntry <- buildStmt init' condNode
        return initEntry

    Break -> do
        node' <- newNode (StmtNode (Fix node))
        asks envBreak >>= \case
            Just target -> lift $ addEdgeM node' Unconditional target
            Nothing     -> return ()
        return node'

    Continue -> do
        node' <- newNode (StmtNode (Fix node))
        asks envContinue >>= \case
            Just target -> lift $ addEdgeM node' Unconditional target
            Nothing     -> return ()
        return node'

    Return _ -> do
        node' <- newNode (StmtNode (Fix node))
        retNode <- asks envReturn
        lift $ addEdgeM node' Unconditional retNode
        return node'

    Goto (L _ _ name) -> do
        node' <- newNode (StmtNode (Fix node))
        asks (Map.lookup name . envLabels) >>= \case
            Just target -> lift $ addEdgeM node' Unconditional target
            Nothing     -> return ()
        return node'

    Label (L _ _ name) body -> do
        node' <- asks (Map.findWithDefault (error "label missing") name . envLabels)
        bEntry <- buildStmt body next
        lift $ addEdgeM node' Unconditional bEntry
        return node'

    SwitchStmt expr cases -> do
        node' <- newNode (BranchNode expr)
        buildCases cases node' next
        return node'

    ExprStmt e -> do
        node' <- newNode (StmtNode (Fix node))
        if isTerminating e
            then do
                retNode <- asks envReturn
                lift $ addEdgeM node' Unconditional retNode
            else lift $ addEdgeM node' Unconditional next
        return node'

    VarDeclStmt _ _ -> do
        node' <- newNode (StmtNode (Fix node))
        lift $ addEdgeM node' Unconditional next
        return node'

    VLA {} -> do
        node' <- newNode (StmtNode (Fix node))
        lift $ addEdgeM node' Unconditional next
        return node'

    PreprocIf cond stmts nextBranch ->
        buildPreprocBranch cond stmts nextBranch next

    PreprocIfdef cond stmts nextBranch ->
        buildPreprocBranch (Fix $ VarExpr cond) stmts nextBranch next

    PreprocIfndef cond stmts nextBranch ->
        buildPreprocBranch (Fix $ VarExpr cond) stmts nextBranch next

    PreprocElif cond stmts nextBranch ->
        buildPreprocBranch cond stmts nextBranch next

    PreprocElse stmts ->
        buildStmts stmts next

    PreprocScopedDefine _ stmts _ ->
        buildStmts stmts next

    _ -> do
        node' <- newNode (StmtNode (Fix node))
        lift $ addEdgeM node' Unconditional next
        return node'

buildStmts :: [C.Node (Lexeme Text)] -> Node -> BuildM Node
buildStmts [] next = return next
buildStmts (s:ss) next = do
    restEntry <- buildStmts ss next
    buildStmt s restEntry

buildCases :: [C.Node (Lexeme Text)] -> Node -> Node -> BuildM ()
buildCases cases switchNode next = do
    caseEntries <- forM cases $ \c -> case unFix c of
        Case val body -> do
            cNode <- newNode (StmtNode c)
            lift $ addEdgeM switchNode (CaseBranch val) cNode
            return (cNode, body)
        Default body -> do
            dNode <- newNode (StmtNode c)
            lift $ addEdgeM switchNode DefaultBranch dNode
            return (dNode, body)
        _ -> error "invalid switch case"

    foldM_ (\nextCaseEntry (cNode, body) -> do
        bodyEntry <- local (\env -> env { envBreak = Just next }) $
            buildStmt body nextCaseEntry
        lift $ addEdgeM cNode Unconditional bodyEntry
        return bodyEntry
        ) next (reverse caseEntries)

buildPreprocBranch :: C.Node (Lexeme Text) -> [C.Node (Lexeme Text)] -> C.Node (Lexeme Text) -> Node -> BuildM Node
buildPreprocBranch cond stmts nextBranch next = do
    node' <- newNode (BranchNode cond)
    tEntry <- buildStmts stmts next
    eEntry <- buildStmt nextBranch next
    lift $ addEdgeM node' TrueBranch tEntry
    lift $ addEdgeM node' FalseBranch eEntry
    return node'
