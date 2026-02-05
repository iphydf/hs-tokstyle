{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.CFGSpec (spec) where

import           Control.Monad                (forM_)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Language.Cimple              (Node, NodeF (..))
import           Test.Hspec
import           Tokstyle.Cimple.Analysis.CFG
import           Tokstyle.LinterSpec          (mustParse)

spec :: Spec
spec = describe "Cimple CFG Construction" $ do
    it "creates a linear graph for simple statements" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "    x = 1;"
            , "    y = 2;"
            , "}"
            ]
        let (entry, cfg) = fromFunction defn

        let successors node = Map.findWithDefault [] node cfg

        let isEntry (Node _ EntryNode) = True
            isEntry _                  = False
        entry `shouldSatisfy` isEntry

        let node1 = case successors entry of
                      [(Unconditional, n)] -> n
                      s -> error $ "Expected one unconditional successor for entry, got " ++ show s

        let isStmt (Node _ (StmtNode _)) = True
            isStmt _                     = False
        node1 `shouldSatisfy` isStmt

        let node2 = case successors node1 of
                      [(Unconditional, n)] -> n
                      s -> error $ "Expected one unconditional successor for node1, got " ++ show s
        node2 `shouldSatisfy` isStmt

        let node3 = case successors node2 of
                      [(Unconditional, n)] -> n
                      s -> error $ "Expected one unconditional successor for node2, got " ++ show s

        let isExit (Node _ ExitNode) = True
            isExit _                 = False
        node3 `shouldSatisfy` isExit

    it "handles if-else branching" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "    if (c) {"
            , "        return;"
            , "    } else {"
            , "        return;"
            , "    }"
            , "}"
            ]
        let (entry, cfg) = fromFunction defn

        let successors node = Map.findWithDefault [] node cfg
        let branchNode = case successors entry of
                           [(_, n)] -> n
                           s -> error $ "Expected one successor for entry, got " ++ show s

        let isBranch (Node _ (BranchNode _)) = True
            isBranch _                       = False
        branchNode `shouldSatisfy` isBranch

        let branchSuccs = successors branchNode
        length branchSuccs `shouldBe` 2

        let tNode = case lookup TrueBranch branchSuccs of
                      Just n  -> n
                      Nothing -> error "Expected TrueBranch"

        let fNode = case lookup FalseBranch branchSuccs of
                      Just n  -> n
                      Nothing -> error "Expected FalseBranch"

        let isReturn (Node _ (StmtNode _)) = True
            isReturn _                     = False
        tNode `shouldSatisfy` isReturn
        fNode `shouldSatisfy` isReturn

    it "handles while loops" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "    while (c) {"
            , "        x = 1;"
            , "    }"
            , "}"
            ]
        let (entry, cfg) = fromFunction defn

        let successors node = Map.findWithDefault [] node cfg
        let branchNode = case successors entry of
                           [(_, n)] -> n
                           s -> error $ "Expected one successor for entry, got " ++ show s

        let isBranch (Node _ (BranchNode _)) = True
            isBranch _                       = False
        branchNode `shouldSatisfy` isBranch

        let branchSuccs = successors branchNode
        length branchSuccs `shouldBe` 2

        let tNode = case lookup TrueBranch branchSuccs of
                      Just n  -> n
                      Nothing -> error "Expected TrueBranch"

        let fNode = case lookup FalseBranch branchSuccs of
                      Just n  -> n
                      Nothing -> error "Expected FalseBranch"

        -- True branch goes to loop body
        let isStmt (Node _ (StmtNode _)) = True
            isStmt _                     = False
        tNode `shouldSatisfy` isStmt

        -- False branch goes to exit
        let isExit (Node _ ExitNode) = True
            isExit _                 = False
        fNode `shouldSatisfy` isExit

        -- Loop body goes back to branch
        let bodyNext = case successors tNode of
                         [(Unconditional, n)] -> n
                         s -> error $ "Expected one successor for body, got " ++ show s
        bodyNext `shouldBe` branchNode

    it "handles preprocessor conditionals" $ do
        [defn] <- mustParse
            [ "void f() {"
            , "#ifdef FOO_BAR"
            , "    x = 1;"
            , "#else"
            , "    x = 2;"
            , "#endif /* FOO_BAR */"
            , "}"
            ]
        let (entry, cfg) = fromFunction defn
        let successors node = Map.findWithDefault [] node cfg

        -- Entry -> PreprocBranch -> (True -> x=1 -> Exit, False -> x=2 -> Exit)
        let branchNode = case successors entry of
                           [(_, n)] -> n
                           s -> error $ "Expected one successor for entry, got " ++ show s

        -- The node should be a branch node (or similar) representing the #ifdef
        let isBranch n = case n of { Node _ (BranchNode _) -> True; _ -> False }
        branchNode `shouldSatisfy` isBranch

        let branchSuccs = successors branchNode
        length branchSuccs `shouldBe` 2

        -- Both branches should eventually reach the exit
        forM_ branchSuccs $ \(_, target) -> do
            let targetSuccs = successors target
            let isExit n = case n of { Node _ ExitNode -> True; _ -> False }
            case targetSuccs of
                [(Unconditional, exit)] -> exit `shouldSatisfy` isExit
                _ -> expectationFailure "Expected branch statement to lead to exit"
