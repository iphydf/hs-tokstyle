{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict     #-}
module Tokstyle.Analysis.Symbolic
    ( SVal (..)
    , Constraint (..)
    , SState (..)
    , emptyState
    , assign
    , addConstraint
    , canBeNull
    , merge
    , negateConstraint
    ) where

import           Control.Applicative          ((<|>))
import qualified Data.Map.Merge.Strict        as Map
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromMaybe, listToMaybe)
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Language.Cimple              (BinaryOp (..), UnaryOp (..))
import           Tokstyle.Analysis.AccessPath

-- | A Symbolic Value represents the result of an expression.
data SVal
    = STop               -- ^ Unknown value
    | SVar AccessPath    -- ^ Initial value of a path
    | SAddr AccessPath   -- ^ The address of a memory location (e.g. &a, or a literal string)
    | SNull              -- ^ Literal NULL / 0 / nullptr
    | SBinOp BinaryOp SVal SVal
    | SUnaryOp UnaryOp SVal
    | SIte SVal SVal SVal -- ^ If-Then-Else (Phi node)
    deriving (Show, Eq, Ord)

-- | A constraint on symbolic values.
data Constraint
    = SEquals SVal SVal
    | SNotEquals SVal SVal
    | SBool SVal         -- ^ The value is true/non-zero
    deriving (Show, Eq, Ord)

-- | The symbolic state at a point in the program.
data SState = SState
    { store       :: Map AccessPath SVal -- ^ Maps paths (e.g. "p->x") to their symbolic value
    , constraints :: Set Constraint      -- ^ Known truths (e.g. {SVar 1 != SNull})
    } deriving (Show, Eq, Ord)

emptyState :: SState
emptyState = SState Map.empty Set.empty

-- | Assign a value to a path, and handle invalidation of dependent paths.
assign :: AccessPath -> SVal -> SState -> SState
assign path val st =
    let -- Remove the path and all paths that have it as a prefix (e.g. assigning to 'p' invalidates 'p->x')
        store' = Map.filterWithKey (\k _ -> not (path `isPathPrefixOf` k)) (store st)
        store'' = Map.insert path val store'
    in st { store = store'' }

-- | Add a new constraint to the state, with basic simplification.
addConstraint :: Constraint -> SState -> SState
addConstraint c st = st { constraints = Set.union (constraints st) (simplify c) }
  where
    simplify = \case
        SBool (SBinOp BopNe v SNull) -> Set.singleton (SNotEquals v SNull)
        SBool (SBinOp BopNe SNull v) -> Set.singleton (SNotEquals v SNull)
        SBool (SBinOp BopEq v SNull) -> Set.singleton (SEquals v SNull)
        SBool (SBinOp BopEq SNull v) -> Set.singleton (SEquals v SNull)
        SBool (SBinOp BopAnd v1 v2)  -> Set.union (simplify (SBool v1)) (simplify (SBool v2))
        SBool (SUnaryOp UopNot (SBinOp BopOr v1 v2)) -> Set.union (simplify (SBool (SUnaryOp UopNot v1))) (simplify (SBool (SUnaryOp UopNot v2)))
        SBool (SUnaryOp UopNot (SBinOp BopAnd v1 v2)) -> Set.singleton (SBool (SUnaryOp UopNot (SBinOp BopAnd v1 v2)))
        SBool (SUnaryOp UopNot (SUnaryOp UopNot v)) -> simplify (SBool v)
        SBool (SUnaryOp UopNot (SBinOp BopEq v1 v2)) -> simplify (SBool (SBinOp BopNe v1 v2))
        SBool (SUnaryOp UopNot (SBinOp BopNe v1 v2)) -> simplify (SBool (SBinOp BopEq v1 v2))
        SBool (SUnaryOp UopNot v)    -> Set.singleton (SBool (SUnaryOp UopNot v))
        c' -> Set.singleton c'

negateConstraint :: Constraint -> Constraint
negateConstraint = \case
    SEquals v1 v2    -> SNotEquals v1 v2
    SNotEquals v1 v2 -> SEquals v1 v2
    SBool v          -> SBool (SUnaryOp UopNot v)

-- | Solver: check if a value is known to be non-null.
-- Takes a predicate to check if an initial SVar is known to be non-null (e.g. from declarations).
canBeNull :: (SVal -> Bool) -> SVal -> SState -> Bool
canBeNull isDeclNonNull val st = not (isKnownNonNull (Set.singleton val) Set.empty)
  where
    isKnownNonNull todo seen
        | Set.null todo = False
        | otherwise =
            let (v, todo') = Set.deleteFindMin todo
                seen' = Set.insert v seen

                -- Check if this specific symbol is non-null
                direct = case v of
                    SNull -> False
                    SAddr _ -> True
                    SBinOp op _ _ | isComparison op -> True
                    SIte STop t e -> (not (canBeNull isDeclNonNull t st)) &&
                                     (not (canBeNull isDeclNonNull e st))
                    SIte c t e -> (not (canBeNull isDeclNonNull t (addConstraint (SBool c) st))) &&
                                  (not (canBeNull isDeclNonNull e (addConstraint (negateConstraint (SBool c)) st)))
                    _ -> isDeclNonNull v ||
                         any (isNonNullConstraint v) (constraints st) ||
                         any (isEqualAddress v) (constraints st)

                -- Find symbols equal to this one that we haven't checked yet
                equals = Set.fromList [ if v1 == v then v2 else v1
                                      | SEquals v1 v2 <- Set.toList (constraints st)
                                      , v1 == v || v2 == v
                                      ]
                todo'' = (todo' `Set.union` equals) `Set.difference` seen'
            in direct || isKnownNonNull todo'' seen'

    isComparison = \case
        BopEq  -> True
        BopNe  -> True
        BopLt  -> True
        BopLe  -> True
        BopGt  -> True
        BopGe  -> True
        BopAnd -> True
        BopOr  -> True
        _      -> False

    isNonNullConstraint v = \case
        SNotEquals v1 v2 -> (v1 == v && v2 == SNull) || (v1 == SNull && v2 == v)
        SBool v'         -> v' == v
        _                -> False

    isEqualAddress v = \case
        SEquals v1 (SAddr _) -> v1 == v
        SEquals (SAddr _) v2 -> v2 == v
        _                    -> False

-- | Merge two symbolic states.
-- If a condition is provided, differing values are merged into an SIte (Phi node).
-- Otherwise, they are merged into an SIte with an inferred condition if possible,
-- or STop (unknown) if no condition can be inferred.
-- We also preserve non-nullness: if a path is non-null in both branches,
-- the merged value is constrained to be non-null.
merge :: (SVal -> Bool) -> Maybe SVal -> SState -> SState -> SState
merge isDeclNonNull mCond s1 s2 =
    let inferredCond = mCond <|> inferCond s1 s2
        st = SState
            { store       = Map.merge Map.dropMissing Map.dropMissing (Map.zipWithMatched (mergeVal inferredCond)) (store s1) (store s2)
            , constraints = Set.intersection (constraints s1) (constraints s2)
            }
        -- Preserve non-nullness if both branches were non-null.
        -- We check variables that were modified in EITHER branch.
        allPaths = Map.keysSet (store s1) `Set.union` Map.keysSet (store s2)
        nonNullConstraints = [ SNotEquals v_merged SNull
                             | path <- Set.toList allPaths
                             , let v1 = fromMaybe (SVar path) (Map.lookup path (store s1))
                                   v2 = fromMaybe (SVar path) (Map.lookup path (store s2))
                             , not (canBeNull isDeclNonNull v1 s1)
                             , not (canBeNull isDeclNonNull v2 s2)
                             , let v_merged = fromMaybe (SVar path) (Map.lookup path (store st))
                             ]
    in foldl (flip addConstraint) st nonNullConstraints
  where
    mergeVal mC _ v1 v2
        | v1 == v2 = v1
        | Just c <- mC = SIte c v1 v2
        | otherwise = STop

-- | Infer the condition that separates two states by looking for mismatched constraints.
inferCond :: SState -> SState -> Maybe SVal
inferCond s1 s2 = listToMaybe $ concatMap tryInfer (Set.toList (constraints s1))
  where
    tryInfer c1 = if negateConstraint c1 `Set.member` constraints s2
                  then case c1 of
                        SBool v          -> [v]
                        SEquals v1 v2    -> [SBinOp BopEq v1 v2]
                        SNotEquals v1 v2 -> [SBinOp BopNe v1 v2]
                  else []
