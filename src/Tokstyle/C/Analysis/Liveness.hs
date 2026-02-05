{-# LANGUAGE LambdaCase #-}
module Tokstyle.C.Analysis.Liveness
    ( liveness
    , Liveness
    ) where

import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Language.C.Data.Ident        (Ident (..))
import           Language.C.Syntax.AST
import           Tokstyle.Analysis.AccessPath
import           Tokstyle.Analysis.Dataflow
import           Tokstyle.C.Analysis.CFG

type Liveness = Set AccessPath

liveness :: Map String a -> [Node] -> CFG -> Map Node Liveness
liveness _ exits cfg = solveBackward exits cfg problem
  where
    problem = Dataflow
        { transfer     = transferFunc
        , edgeTransfer = \_ _ s -> s
        , merge        = Set.union
        , initial      = Set.empty
        }

    transferFunc :: Node -> Liveness -> Liveness
    transferFunc node live =
        let (used, defined) = nodeUsesDefs node
        in (live `Set.difference` defined) `Set.union` used

    nodeUsesDefs = \case
        Node _ (StatNode s)   -> statUsesDefs s
        Node _ (ExprNode e)   -> exprUsesDefs e
        Node _ (DeclNode d)   -> declUsesDefs d
        Node _ (BranchNode e) -> exprUsesDefs e
        _                     -> (Set.empty, Set.empty)

    statUsesDefs = \case
        CExpr (Just e) _   -> exprUsesDefs e
        CReturn (Just e) _ -> exprUsesDefs e
        _                  -> (Set.empty, Set.empty)

    declUsesDefs = \case
        CDecl _ ds _ ->
            let results = map maybeDecl ds
            in (Set.unions (map fst results), Set.unions (map snd results))
        CStaticAssert {} -> (Set.empty, Set.empty)
      where
        maybeDecl (Just (CDeclr (Just i) _ _ _ _), mInit, _) =
            let (u, _) = maybe (Set.empty, Set.empty) exprUsesDefs (mInitExpr mInit)
            in (u, Set.singleton (PathVar (idName i)))
        maybeDecl _ = (Set.empty, Set.empty)

        mInitExpr (Just (CInitExpr e _)) = Just e
        mInitExpr _                      = Nothing

    exprUsesDefs :: CExpr -> (Set AccessPath, Set AccessPath)
    exprUsesDefs expr = case expr of
        CVar i _ -> (Set.singleton (PathVar (idName i)), Set.empty)
        CUnary CIndOp e _ ->
            let (u, d) = exprUsesDefs e
            in (maybe u (`Set.insert` u) (exprPath expr), d)
        CMember e _ _ _ ->
            let (u, d) = exprUsesDefs e
            in (maybe u (`Set.insert` u) (exprPath expr), d)
        CCall f args _ ->
            let (uf, df) = exprUsesDefs f
                argsRes = map exprUsesDefs args
            in (Set.unions (uf : map fst argsRes), Set.unions (df : map snd argsRes))
        CAssign _ l r _ ->
            let (ul, dl) = exprUsesDefs l
                (ur, dr) = exprUsesDefs r
                d = maybe dl (`Set.insert` dl) (exprPath l)
            in (ul `Set.union` ur, d `Set.union` dr)
        CBinary _ l r _ ->
            let (ul, dl) = exprUsesDefs l
                (ur, dr) = exprUsesDefs r
                in (ul `Set.union` ur, dl `Set.union` dr)
        CUnary _ e _ -> exprUsesDefs e
        _ -> (Set.empty, Set.empty)

    idName (Ident name _ _) = name

    exprPath = \case
        CVar i _          -> Just (PathVar (idName i))
        CUnary CIndOp e _ -> PathDeref <$> exprPath e
        CMember e i _ _   -> PathField <$> exprPath e <*> pure (idName i)
        _                 -> Nothing
