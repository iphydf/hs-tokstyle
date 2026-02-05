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
    , sIte
    , sBinOp
    , sUnaryOp
    , sVar
    , sAddr
    , valDepth
    , lookupStore
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

valDepth :: SVal -> Int
valDepth = \case
    SVar p         -> pathDepth p
    SAddr p        -> pathDepth p
    SBinOp _ v1 v2 -> 1 + max (valDepth v1) (valDepth v2)
    SUnaryOp _ v   -> 1 + valDepth v
    SIte c t e     -> 1 + max (valDepth c) (max (valDepth t) (valDepth e))
    _              -> 1

sVar :: AccessPath -> SVal
sVar p | pathDepth p > 10 = STop
       | otherwise = SVar p

sAddr :: AccessPath -> SVal
sAddr p | pathDepth p > 10 = STop
        | otherwise = SAddr p

sIte :: SVal -> SVal -> SVal -> SVal
sIte c t e
    | t == e = t
    | SUnaryOp UopNot c' <- c = sIte c' e t
    | SIte c' t' _ <- t, c == c' = sIte c t' e
    | SIte c' _ e' <- e, c == c' = sIte c t e'
    | valDepth c > 10 || valDepth t > 10 || valDepth e > 10 = STop
    | otherwise = SIte c t e

sBinOp :: BinaryOp -> SVal -> SVal -> SVal
sBinOp op v1 v2
    | valDepth v1 > 10 || valDepth v2 > 10 = STop
    | otherwise = SBinOp op v1 v2

sUnaryOp :: UnaryOp -> SVal -> SVal
sUnaryOp UopNot (SUnaryOp UopNot v) = v
sUnaryOp op v
    | valDepth v > 10 = STop
    | otherwise = SUnaryOp op v

lookupStore :: AccessPath -> SState -> Maybe SVal
lookupStore path st = go path
  where
    go p = case Map.lookup p (store st) of
        Just v -> Just v
        Nothing -> case p of
            PathField p' _ -> case go p' of
                Just STop -> Just STop
                _         -> Nothing
            PathDeref p'   -> case go p' of
                Just STop -> Just STop
                _         -> Nothing
            _              -> Nothing

-- | Assign a value to a path, and handle invalidation of dependent paths.
assign :: AccessPath -> SVal -> SState -> SState
assign path val st =
    let -- Remove the path and all paths that have it as a prefix (e.g. assigning to 'p' invalidates 'p->x')
        store' = Map.filterWithKey (\k _ -> not (path `isPathPrefixOf` k)) (store st)
        store'' = if val == STop then store' else Map.insert path val store'
    in st { store = store'' }

-- | Add a new constraint to the state, with basic simplification.
addConstraint :: Constraint -> SState -> SState
addConstraint c st
    | Set.size (constraints st) > 100 = st
    | otherwise = st { constraints = Set.union (constraints st) (simplify c) }
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
    SEquals v1 v2             -> SNotEquals v1 v2
    SNotEquals v1 v2          -> SEquals v1 v2
    SBool (SUnaryOp UopNot v) -> SBool v
    SBool v                   -> SBool (sUnaryOp UopNot v)

-- | Solver: check if a value is known to be non-null.
-- Takes a predicate to check if an initial SVar is known to be non-null (e.g. from declarations).
canBeNull :: (SVal -> Bool) -> SVal -> SState -> Bool
canBeNull isDeclNonNull val st = not (isKnownNonNull (0 :: Int) (Set.singleton val) Set.empty st)
  where
    isKnownNonNull depth todo seen s
        | depth > 10 = False -- Limit recursion
        | Set.null todo = False
        | otherwise =
            let (v, todo') = Set.deleteFindMin todo
                seen' = Set.insert v seen

                -- Check if this specific symbol is non-null
                direct = case v of
                    SNull -> False
                    SAddr _ -> True
                    SBinOp op _ _ | isComparison op -> True
                    SIte STop t e -> (not (canBeNull' (depth + 1) t s)) &&
                                     (not (canBeNull' (depth + 1) e s))
                    SIte c t e -> (not (canBeNull' (depth + 1) t (addConstraint (SBool c) s))) &&
                                  (not (canBeNull' (depth + 1) e (addConstraint (negateConstraint (SBool c)) s)))
                    _ -> isDeclNonNull v ||
                         any (isNonNullConstraint v) (constraints s) ||
                         any (isEqualAddress v) (constraints s)

                -- Find symbols equal to this one that we haven't checked yet
                equals = Set.fromList [ if v1 == v then v2 else v1
                                      | SEquals v1 v2 <- Set.toList (constraints s)
                                      , v1 == v || v2 == v
                                      ]
                todo'' = (todo' `Set.union` equals) `Set.difference` seen'
            in direct || isKnownNonNull (depth + 1) todo'' seen' s

    canBeNull' d v s = not (isKnownNonNull d (Set.singleton v) Set.empty s)

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
                             , let v1 = fromMaybe (sVar path) (lookupStore path s1)
                                   v2 = fromMaybe (sVar path) (lookupStore path s2)
                             , not (canBeNull isDeclNonNull v1 s1)
                             , not (canBeNull isDeclNonNull v2 s2)
                             , let v_merged = fromMaybe (sVar path) (lookupStore path st)
                             ]
    in foldl (flip addConstraint) st nonNullConstraints
  where
    mergeVal mC _ v1 v2
        | v1 == v2 = v1
        | Just c <- mC = sIte c v1 v2
        | otherwise = STop

-- | Infer the condition that separates two states by looking for mismatched constraints.
inferCond :: SState -> SState -> Maybe SVal
inferCond s1 s2 = listToMaybe $ concatMap tryInfer (Set.toList (constraints s1))
  where
    tryInfer c1 = if negateConstraint c1 `Set.member` constraints s2
                  then case c1 of
                        SBool v          -> [v]
                        SEquals v1 v2    -> [sBinOp BopEq v1 v2]
                        SNotEquals v1 v2 -> [sBinOp BopNe v1 v2]
                  else []
