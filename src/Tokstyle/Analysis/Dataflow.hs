module Tokstyle.Analysis.Dataflow
    ( Dataflow (..)
    , solve
    , solveBackward
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Sequence   (Seq (..), (|>))
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set

-- | A generic dataflow problem definition.
data Dataflow node edge state = Dataflow
    { transfer     :: node -> state -> state               -- ^ How a node changes the state
    , edgeTransfer :: node -> edge -> state -> state       -- ^ How an edge changes the state
    , merge        :: state -> state -> state              -- ^ How to combine states from two paths
    , initial      :: state                                -- ^ Starting state for Entry node (forward) or Exit node (backward)
    }

-- | Solves the forward dataflow problem using a worklist algorithm.
solve :: (Ord node, Eq state)
      => node                        -- ^ The entry node
      -> Map node [(edge, node)]     -- ^ The CFG
      -> Dataflow node edge state
      -> Map node state
solve entry cfg problem =
    let startStates = Map.singleton entry (initial problem)
        worklist = Seq.singleton entry
        inWorklist = Set.singleton entry
    in loop worklist inWorklist startStates
  where
    loop Empty _ states = states
    loop (u :<| ws) inWS states =
        let currentIn = Map.findWithDefault (initial problem) u states
            nodeOut = transfer problem u currentIn
            successors = Map.findWithDefault [] u cfg
            (nextWS, nextInWS, nextStates) = foldl (update u nodeOut) (ws, Set.delete u inWS, states) successors
        in loop nextWS nextInWS nextStates

    update u nodeOut (ws, inWS, states) (e, v) =
        let edgeOut = edgeTransfer problem u e nodeOut
            oldIn = Map.lookup v states
            newIn = case oldIn of
                Just s  -> merge problem s edgeOut
                Nothing -> edgeOut
        in if Just newIn /= oldIn
           then (if v `Set.member` inWS then ws else ws |> v, Set.insert v inWS, Map.insert v newIn states)
           else (ws, inWS, states)

-- | Solves the backward dataflow problem using a worklist algorithm.
solveBackward :: (Ord node, Eq state)
              => [node]                      -- ^ Exit nodes
              -> Map node [(edge, node)]     -- ^ The CFG
              -> Dataflow node edge state
              -> Map node state
solveBackward exits cfg problem =
    let startStates = Map.fromList [ (exit, initial problem) | exit <- exits ]
        worklist = Seq.fromList exits
        inWorklist = Set.fromList exits
    in loop worklist inWorklist startStates
  where
    revCfg = reverseCfg cfg

    loop Empty _ states = states
    loop (u :<| ws) inWS states =
        let currentOut = Map.findWithDefault (initial problem) u states
            nodeIn = transfer problem u currentOut
            predecessors = Map.findWithDefault [] u revCfg
            (nextWS, nextInWS, nextStates) = foldl (update u nodeIn) (ws, Set.delete u inWS, states) predecessors
        in loop nextWS nextInWS nextStates

    update u nodeIn (ws, inWS, states) (e, v) =
        let edgeIn = edgeTransfer problem u e nodeIn
            oldOut = Map.lookup v states
            newOut = case oldOut of
                Just s  -> merge problem s edgeIn
                Nothing -> edgeIn
        in if Just newOut /= oldOut
           then (if v `Set.member` inWS then ws else ws |> v, Set.insert v inWS, Map.insert v newOut states)
           else (ws, inWS, states)

    reverseCfg :: Ord node => Map node [(edge, node)] -> Map node [(edge, node)]
    reverseCfg c = Map.fromListWith (++)
        [ (v, [(e, u)]) | (u, edges) <- Map.toList c, (e, v) <- edges ]
