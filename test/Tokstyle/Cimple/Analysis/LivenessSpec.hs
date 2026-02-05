{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.LivenessSpec (spec) where

import           Data.Fix                          (Fix (..), unFix)
import           Data.List                         (find)
import           Data.Map.Strict                   (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe)
import           Data.Set                          (Set)
import qualified Data.Set                          as Set
import           Language.Cimple                   (Node, NodeF (..))
import           Test.Hspec
import           Tokstyle.Analysis.AccessPath
import           Tokstyle.Analysis.Dataflow        (transfer)
import           Tokstyle.Cimple.Analysis.CFG
import           Tokstyle.Cimple.Analysis.Liveness
import           Tokstyle.LinterSpec               (mustParse)

spec :: Spec
spec = describe "Cimple Liveness Analysis" $ do
    it "correctly identifies live variables in linear code" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "    x = 1;"
            , "    y = x;"
            , "    return y;"
            , "}"
            ]
        let (_, cfg) = fromFunction defn
        let exits = [ n | n@(Node _ ExitNode) <- Set.toList $ Set.fromList $ Map.keys cfg ++ concatMap (map snd) (Map.elems cfg) ]
        let liveMap = liveness exits cfg

        let isReturn n = case n of { Node _ (StmtNode s) -> case unFix s of { Return _ -> True; _ -> False }; _ -> False }

        let returnNode = fromMaybe (error "no return node") $ find isReturn (Map.keys cfg)
        -- Liveness AFTER return should be empty
        Map.findWithDefault Set.empty returnNode liveMap `shouldBe` Set.empty

        -- Liveness BEFORE return should contain y
        let liveBeforeReturn = transfer (livenessProblem exits cfg) returnNode (Map.findWithDefault Set.empty returnNode liveMap)
        PathVar "y" `Set.member` liveBeforeReturn `shouldBe` True

    it "handles branching liveness" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "    x = 1;"
            , "    if (c) {"
            , "        y = x;"
            , "    } else {"
            , "        y = 2;"
            , "    }"
            , "    return y;"
            , "}"
            ]
        let (_, cfg) = fromFunction defn
        let exits = [ n | n@(Node _ ExitNode) <- Set.toList $ Set.fromList $ Map.keys cfg ++ concatMap (map snd) (Map.elems cfg) ]
        let liveMap = liveness exits cfg

        let isBranch n = case n of { Node _ (BranchNode _) -> True; _ -> False }
        let branchNode = fromMaybe (error "no branch node") $ find isBranch (Map.keys cfg)

        -- Liveness AFTER the branch node (at the start of branches)
        let liveAtBranch = Map.findWithDefault Set.empty branchNode liveMap
        PathVar "x" `Set.member` liveAtBranch `shouldBe` True
        PathVar "y" `Set.member` liveAtBranch `shouldBe` True

        -- Liveness BEFORE the branch node (includes its use of 'c')
        let liveBeforeBranch = transfer (livenessProblem exits cfg) branchNode liveAtBranch
        PathVar "c" `Set.member` liveBeforeBranch `shouldBe` True
        PathVar "x" `Set.member` liveBeforeBranch `shouldBe` True

    it "handles preprocessor conditional liveness" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "#ifdef FOO_BAR"
            , "    x = 1;"
            , "#else"
            , "    y = 1;"
            , "#endif /* FOO_BAR */"
            , "    return x + y;"
            , "}"
            ]
        let (_, cfg) = fromFunction defn
        let exits = [ n | n@(Node _ ExitNode) <- Set.toList $ Set.fromList $ Map.keys cfg ++ concatMap (map snd) (Map.elems cfg) ]
        let liveMap = liveness exits cfg

        let isBranch n = case n of { Node _ (BranchNode _) -> True; _ -> False }
        let branchNode = fromMaybe (error "no branch node") $ find isBranch (Map.keys cfg)

        -- Both x and y should be live before the #ifdef because they are only conditionally defined.
        let liveBeforeBranch = transfer (livenessProblem exits cfg) branchNode (Map.findWithDefault Set.empty branchNode liveMap)
        PathVar "x" `Set.member` liveBeforeBranch `shouldBe` True
        PathVar "y" `Set.member` liveBeforeBranch `shouldBe` True
