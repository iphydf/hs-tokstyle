{-# LANGUAGE LambdaCase #-}
module Tokstyle.Cimple.Analysis.Liveness
    ( liveness
    , Liveness
    , livenessProblem
    ) where

import           Data.Fix                     (Fix (..), unFix)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as Text
import           Language.Cimple              (Lexeme (..), NodeF (..),
                                               UnaryOp (..))
import qualified Language.Cimple              as C
import           Tokstyle.Analysis.AccessPath
import           Tokstyle.Analysis.Dataflow
import           Tokstyle.Cimple.Analysis.CFG (CFG, EdgeType, Node (..),
                                               NodeKind (..))

type Liveness = Set AccessPath

liveness :: [Node] -> CFG -> Map Node Liveness
liveness exits cfg = solveBackward exits cfg (livenessProblem exits cfg)

livenessProblem :: [Node] -> CFG -> Dataflow Node EdgeType Liveness
livenessProblem _ _ = Dataflow
    { transfer     = transferFunc
    , edgeTransfer = \_ _ s -> s
    , merge        = Set.union
    , initial      = Set.empty
    }
  where
    transferFunc :: Node -> Liveness -> Liveness
    transferFunc (Node _ nk) live =
        let (used, defined) = nodeUsesDefs nk
        in (live `Set.difference` defined) `Set.union` used

    nodeUsesDefs = \case
        StmtNode s   -> stmtUsesDefs s
        BranchNode e -> exprUsesDefs e
        _            -> (Set.empty, Set.empty)

    stmtUsesDefs s = case unFix s of
        ExprStmt e     -> exprUsesDefs e
        Return (Just e) -> exprUsesDefs e
        VarDeclStmt (Fix (VarDecl _ (L _ _ name) _)) mInit ->
            let (u, _) = maybe (Set.empty, Set.empty) exprUsesDefs mInit
            in (u, Set.singleton (PathVar (Text.unpack name)))
        _ -> (Set.empty, Set.empty)

    exprUsesDefs :: C.Node (Lexeme Text) -> (Set AccessPath, Set AccessPath)
    exprUsesDefs (Fix node) = case node of
        VarExpr (L _ _ name) -> (Set.singleton (PathVar (Text.unpack name)), Set.empty)
        UnaryExpr UopDeref e ->
            let (u, d) = exprUsesDefs e
            in (maybe u (`Set.insert` u) (exprPath (Fix node)), d)
        PointerAccess e (L _ _ _) ->
            let (u, d) = exprUsesDefs e
            in (maybe u (`Set.insert` u) (exprPath (Fix node)), d)
        MemberAccess e (L _ _ _) ->
            let (u, d) = exprUsesDefs e
            in (maybe u (`Set.insert` u) (exprPath (Fix node)), d)
        FunctionCall f args ->
            let (uf, df) = exprUsesDefs f
                argsRes = map exprUsesDefs args
            in (Set.unions (uf : map fst argsRes), Set.unions (df : map snd argsRes))
        AssignExpr l _ r ->
            let (ul, dl) = exprUsesDefs l
                (ur, dr) = exprUsesDefs r
                d = maybe dl (`Set.insert` dl) (exprPath l)
            in (ul `Set.union` ur, d `Set.union` dr)
        BinaryExpr l _ r ->
            let (ul, dl) = exprUsesDefs l
                (ur, dr) = exprUsesDefs r
            in (ul `Set.union` ur, dl `Set.union` dr)
        TernaryExpr c t e ->
            let (uc, dc) = exprUsesDefs c
                (ut, dt) = exprUsesDefs t
                (ue, de) = exprUsesDefs e
            in (uc `Set.union` ut `Set.union` ue, dc `Set.union` dt `Set.union` de)
        ParenExpr e -> exprUsesDefs e
        CastExpr _ e -> exprUsesDefs e
        UnaryExpr _ e -> exprUsesDefs e
        _ -> (Set.empty, Set.empty)

    exprPath :: C.Node (Lexeme Text) -> Maybe AccessPath
    exprPath (Fix node) = case node of
        VarExpr (L _ _ name)           -> Just (PathVar (Text.unpack name))
        UnaryExpr UopDeref e           -> PathDeref <$> exprPath e
        PointerAccess e (L _ _ member) -> PathField <$> exprPath e <*> pure (Text.unpack member)
        MemberAccess e (L _ _ member)  -> PathField <$> exprPath e <*> pure (Text.unpack member)
        _                              -> Nothing
