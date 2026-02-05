{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Tokstyle.Analysis.SymbolicSpec (spec) where

import qualified Data.Map.Strict              as Map
import           Data.Maybe                   (fromMaybe)
import qualified Data.Set                     as Set
import           Language.Cimple              (BinaryOp (..), UnaryOp (..))
import           Test.Hspec
import           Test.Hspec.QuickCheck        (prop)
import           Test.QuickCheck
import           Tokstyle.Analysis.AccessPath
import           Tokstyle.Analysis.Symbolic   as S

instance Arbitrary AccessPath where
    arbitrary = sized arbPath
      where
        arbPath n | n <= 0 = PathVar <$> elements ["p", "q", "r", "s", "t"]
        arbPath n = oneof
            [ PathVar <$> elements ["p", "q", "r", "s", "t"]
            , PathDeref <$> arbPath (n `div` 2)
            , PathField <$> arbPath (n `div` 2) <*> elements ["x", "y", "z"]
            , PathIndex <$> arbPath (n `div` 2) <*> elements ["i", "j", "0", "1"]
            ]

    shrink (PathVar _)     = []
    shrink (PathParam i)   = PathParam <$> shrink i
    shrink PathReturn      = []
    shrink (PathDeref p)   = p : [PathDeref p' | p' <- shrink p]
    shrink (PathField p f) = p : [PathField p' f | p' <- shrink p]
    shrink (PathIndex p i) = p : [PathIndex p' i | p' <- shrink p]

instance Arbitrary SVal where
    arbitrary = sized arbSVal
      where
        arbSVal n | n <= 0 = oneof
            [ pure STop
            , SVar <$> arbitrary
            , SAddr <$> arbitrary
            , pure SNull
            ]
        arbSVal n = oneof
            [ pure STop
            , SVar <$> arbitrary
            , SAddr <$> arbitrary
            , pure SNull
            , SIte <$> arbSVal (n `div` 3) <*> arbSVal (n `div` 3) <*> arbSVal (n `div` 3)
            ]

    shrink STop         = []
    shrink (SVar p)     = STop : [SVar p' | p' <- shrink p]
    shrink (SAddr p)    = STop : [SAddr p' | p' <- shrink p]
    shrink SNull        = [STop]
    shrink (SIte c t e) = [c, t, e] ++ [SIte c' t' e' | (c', t', e') <- shrink (c, t, e)]
    shrink _            = []

-- | Generates a path that is strictly a suffix of the input path.
extendPathStrict :: AccessPath -> Gen AccessPath
extendPathStrict p = oneof
    [ pure (PathDeref p)
    , PathField p <$> elements ["x", "y", "z"]
    ] >>= \p' -> oneof [pure p', extendPathStrict p']

-- | Generates a path that is a prefix of or equal to the input path.
extendPath :: AccessPath -> Gen AccessPath
extendPath p = oneof [pure p, extendPathStrict p]

spec :: Spec
spec = do
    let p = PathVar "p"
    let q = PathVar "q"
    let noDecl = const False

    describe "assign" $ do
        it "tracks simple assignments" $ do
            let st = S.assign p S.SNull S.emptyState
            Map.lookup p (S.store st) `shouldBe` Just S.SNull

        it "invalidates sub-paths on assignment" $ do
            let p_x = PathField p "x"
            let st = S.assign p_x (S.SVar p_x) S.emptyState
            let st' = S.assign p S.SNull st
            Map.lookup p_x (S.store st') `shouldBe` Nothing
            Map.lookup p (S.store st') `shouldBe` Just S.SNull

    describe "merge" $ do
        it "keeps identical values" $ do
            let s1 = S.assign p S.SNull S.emptyState
            let s2 = S.assign p S.SNull S.emptyState
            let sm = S.merge (const False) Nothing s1 s2
            Map.lookup p (S.store sm) `shouldBe` Just S.SNull

        it "drops differing values to STop" $ do
            let s1 = S.assign p S.SNull S.emptyState
            let s2 = S.assign p (S.SAddr q) S.emptyState
            let sm = S.merge (const False) Nothing s1 s2
            Map.lookup p (S.store sm) `shouldBe` Just S.STop

        it "intersects constraints" $ do
            let c1 = S.SNotEquals (S.SVar p) S.SNull
            let c2 = S.SNotEquals (S.SVar q) S.SNull
            let s1 = S.addConstraint c1 S.emptyState
            let s2 = S.addConstraint c1 $ S.addConstraint c2 S.emptyState
            let sm = S.merge (const False) Nothing s1 s2
            constraints sm `shouldBe` Set.singleton c1

    describe "solver (canBeNull)" $ do
        it "knows NULL can be null" $ do
            S.canBeNull noDecl S.SNull S.emptyState `shouldBe` True

        it "knows addresses cannot be null" $ do
            S.canBeNull noDecl (S.SAddr p) S.emptyState `shouldBe` False

        it "respects SNotEquals constraints" $ do
            let v = S.SVar p
            let st = S.addConstraint (S.SNotEquals v S.SNull) S.emptyState
            S.canBeNull noDecl v st `shouldBe` False

        it "handles aliases" $ do
            let st = S.assign q (S.SVar p) S.emptyState
            let st' = S.addConstraint (S.SNotEquals (S.SVar p) S.SNull) st
            -- q evaluates to (SVar p), so it should also be non-null
            let v_q = fromMaybe (S.SVar q) (Map.lookup q (S.store st'))
            S.canBeNull noDecl v_q st' `shouldBe` False

        it "handles SBool simplification" $ do
            let v = S.SVar p
            let st = S.addConstraint (S.SBool v) S.emptyState
            S.canBeNull noDecl v st `shouldBe` False

        it "handles complex boolean simplification (negation)" $ do
            let v = S.SVar p
            -- if (!(p == nullptr))
            let cond = S.SUnaryOp UopNot (S.SBinOp BopEq v S.SNull)
            let st = S.addConstraint (S.SBool cond) S.emptyState
            S.canBeNull noDecl v st `shouldBe` False

        it "handles De Morgan's law: !(p == NULL || q == NULL)" $ do
            let vp = S.SVar p
                vq = S.SVar q
                -- !(vp == NULL || vq == NULL)
                cond = S.SUnaryOp UopNot (S.SBinOp BopOr (S.SBinOp BopEq vp S.SNull) (S.SBinOp BopEq vq S.SNull))
                st = S.addConstraint (S.SBool cond) S.emptyState
            S.canBeNull noDecl vp st `shouldBe` False
            S.canBeNull noDecl vq st `shouldBe` False

        it "handles comparison results as non-null pointers" $ do
            let cond = S.SBinOp BopEq (S.SVar p) (S.SVar q)
            S.canBeNull noDecl cond S.emptyState `shouldBe` False

        it "handles transitive equality (BFS solver)" $ do
            let vp = S.SVar p
                vq = S.SVar q
                vr = S.SVar (PathVar "r")
                st = S.addConstraint (S.SEquals vp vq) $
                     S.addConstraint (S.SEquals vq vr) $
                     S.addConstraint (S.SNotEquals vr S.SNull) S.emptyState
            S.canBeNull noDecl vp st `shouldBe` False

        it "handles SIte nodes with concrete conditions" $ do
            let cond = S.SVar (PathVar "cond")
                vt = S.SAddr p
                ve = S.SAddr q
                v = S.SIte cond vt ve
            S.canBeNull noDecl v S.emptyState `shouldBe` False

        it "handles SIte nodes where only one branch is non-null" $ do
            let cond = S.SVar (PathVar "cond")
                vt = S.SAddr p
                ve = S.SNull
                v = S.SIte cond vt ve
            S.canBeNull noDecl v S.emptyState `shouldBe` True

        it "handles SIte with STop condition if both branches are non-null" $ do
            let vt = S.SAddr p
                ve = S.SAddr q
                v = S.SIte S.STop vt ve
            S.canBeNull noDecl v S.emptyState `shouldBe` False

    describe "merge precision" $ do
        it "preserves non-nullness when values differ but both are non-null" $ do
            let s1 = S.assign p (S.SAddr q) S.emptyState
                s2 = S.assign p (S.SVar (PathVar "other")) S.emptyState
                s2' = S.addConstraint (S.SNotEquals (S.SVar (PathVar "other")) S.SNull) s2
                sm = S.merge noDecl Nothing s1 s2'
                v_p = fromMaybe (S.SVar p) (Map.lookup p (S.store sm))
            S.canBeNull noDecl v_p sm `shouldBe` False

        it "preserves non-nullness when modified in only one branch (address assignment)" $ do
            -- s1: p is checked non-null
            let s1 = S.addConstraint (S.SNotEquals (S.SVar p) S.SNull) S.emptyState
                -- s2: p is assigned an address
                s2 = S.assign p (S.SAddr (PathVar "i")) S.emptyState
                sm = S.merge noDecl Nothing s1 s2
                v_p = fromMaybe (S.SVar p) (Map.lookup p (S.store sm))
            S.canBeNull noDecl v_p sm `shouldBe` False

        it "preserves non-nullness when modified in only one branch (non-null pointer assignment)" $ do
            -- s1: p is checked non-null
            let s1 = S.addConstraint (S.SNotEquals (S.SVar p) S.SNull) S.emptyState
                -- s2: p is assigned another non-null pointer q
                s2 = S.assign p (S.SVar q) $ S.addConstraint (S.SNotEquals (S.SVar q) S.SNull) S.emptyState
                sm = S.merge noDecl Nothing s1 s2
                v_p = fromMaybe (S.SVar p) (Map.lookup p (S.store sm))
            S.canBeNull noDecl v_p sm `shouldBe` False

    describe "merge performance (growth prevention)" $ do
        it "does not grow the store when a variable is modified in only one branch" $ do
            -- s1: p is modified
            let s1 = S.assign p S.SNull S.emptyState
                -- s2: p is not modified
                s2 = S.emptyState
                sm = S.merge noDecl Nothing s1 s2
            -- The store should be empty because p was not modified in both branches.
            -- This prevents the store from growing indefinitely during fixpoint iteration.
            Map.size (S.store sm) `shouldBe` 0

    describe "declared non-null" $ do
        it "respects the isDeclNonNull predicate" $ do
            let v = S.SVar p
            let isPNonNull (S.SVar path) = path == p
                isPNonNull _             = False
            S.canBeNull isPNonNull v S.emptyState `shouldBe` False
            S.canBeNull isPNonNull (S.SVar q) S.emptyState `shouldBe` True

    describe "properties" $ do
        prop "addresses are never null" $ \path ->
            S.canBeNull noDecl (S.SAddr path) S.emptyState == False

        prop "merge preserves non-nullness if both branches are non-null" $ \path ->
            let v = S.SVar path
                s1 = S.addConstraint (S.SNotEquals v S.SNull) S.emptyState
                s2 = S.addConstraint (S.SNotEquals v S.SNull) S.emptyState
                sm = S.merge noDecl Nothing s1 s2
            in S.canBeNull noDecl v sm == False

        prop "merge preserves non-nullness even if one branch reassigns a non-null value" $ \path ->
            let v = S.SVar path
                s1 = S.addConstraint (S.SNotEquals v S.SNull) S.emptyState
                s2 = S.assign path (S.SAddr (PathVar "i")) S.emptyState
                sm = S.merge noDecl Nothing s1 s2
            in S.canBeNull noDecl v sm == False

    describe "structural properties" $ do
        prop "isPathPrefixOf is reflexive" $ \p1 ->
            p1 `isPathPrefixOf` p1 == True

        prop "isPathPrefixOf is transitive" $
            forAll arbitrary $ \p1 ->
            forAll (extendPath p1) $ \p2 ->
            forAll (extendPath p2) $ \p3 ->
            p1 `isPathPrefixOf` p3 == True

        prop "assign p v st removes all paths prefixed by p" $
            forAll arbitrary $ \p1 ->
            forAll (extendPathStrict p1) $ \p2 ->
            forAll arbitrary $ \v ->
            let st = S.assign p2 (S.SAddr p2) S.emptyState
                st' = S.assign p1 v st
            in Map.lookup p2 (S.store st') == Nothing

    describe "solver properties" $ do
        prop "canBeNull is consistent across equality chains" $ \p1 p2 p3 ->
            let v1 = S.SVar p1
                v2 = S.SVar p2
                v3 = S.SVar p3
                st = S.addConstraint (S.SEquals v1 v2) $
                     S.addConstraint (S.SEquals v2 v3) $
                     S.addConstraint (S.SNotEquals v3 S.SNull) S.emptyState
            in S.canBeNull noDecl v1 st == False

        prop "double negation of SBool constraint is equivalent to original" $ \path ->
            let v = S.SVar path
                cond = S.SUnaryOp UopNot (S.SUnaryOp UopNot v)
                st = S.addConstraint (S.SBool cond) S.emptyState
            in S.canBeNull noDecl v st == False
