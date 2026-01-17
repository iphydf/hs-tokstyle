{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Analysis.CFGSpec (spec) where
import           Data.Text                   (unlines)
import           Prelude                     hiding (unlines)

import           Prelude                     hiding (lookup)
import           Test.Hspec

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Language.C.Analysis.SemRep
import           Language.C.Data.Ident       (Ident (..))
import           Language.C.Data.Node        (NodeInfo (..), undefNode)
import           Language.C.Syntax.AST
import qualified Language.C.Syntax.Constants as C
import           Tokstyle.C.Analysis.CFG

-- Helper to create a dummy Ident
mkIdent :: String -> Ident
mkIdent s = Ident s 0 undefNode

-- Helper to create a simple type
simpleTy :: Type
simpleTy = DirectType TyVoid noTypeQuals []

spec :: Spec
spec = describe "CFG Construction" $ do
    it "creates a linear graph for simple statements" $ do
        -- void f() { int x; x = 1; }
        let xIdent = mkIdent "x"
            cint = case C.readCInteger C.DecRepr "1" of
                     Right i -> i
                     Left e  -> error e
            decl = CDecl [CTypeSpec (CIntType undefNode)] [(Just (CDeclr (Just xIdent) [] Nothing [] undefNode), Nothing, Nothing)] undefNode
            assign = CExpr (Just (CAssign CAssignOp (CVar xIdent undefNode) (CConst (CIntConst cint undefNode)) undefNode)) undefNode
            items = [CBlockDecl decl, CBlockStmt assign]
            fd = FunDef (VarDecl (VarName (mkIdent "f") Nothing) (DeclAttrs noFunctionAttrs NoStorage []) (FunctionType (FunType simpleTy [] False) [])) (CCompound [] items undefNode) undefNode
            (entry, cfg) = fromFunDef fd

        -- Expect: Entry -> Decl -> Stat -> Exit
        let successors node = Map.findWithDefault [] node cfg

        let isEntry (Node _ EntryNode) = True
            isEntry _                  = False
        entry `shouldSatisfy` isEntry

        let node1 = case successors entry of
                      [(Unconditional, n)] -> n
                      s -> error $ "Expected one unconditional successor for entry, got " ++ show s

        let isDecl (Node _ (DeclNode _)) = True
            isDecl _                     = False
        node1 `shouldSatisfy` isDecl

        let node2 = case successors node1 of
                      [(Unconditional, n)] -> n
                      s -> error $ "Expected one unconditional successor for node1, got " ++ show s

        let isStat (Node _ (StatNode _)) = True
            isStat _                     = False
        node2 `shouldSatisfy` isStat

        let node3 = case successors node2 of
                      [(Unconditional, n)] -> n
                      s -> error $ "Expected one unconditional successor for node2, got " ++ show s

        let isExit (Node _ ExitNode) = True
            isExit _                 = False
        node3 `shouldSatisfy` isExit

    it "handles if-else branching" $ do
        -- void f(int c) { if (c) { return; } else { return; } }
        let cIdent = mkIdent "c"
            ret = CReturn Nothing undefNode
            ifStat = CIf (CVar cIdent undefNode) ret (Just ret) undefNode
            cParamTy = DirectType (TyIntegral TyInt) noTypeQuals []
            fd = FunDef (VarDecl (VarName (mkIdent "f") Nothing) (DeclAttrs noFunctionAttrs NoStorage []) (FunctionType (FunType simpleTy [ParamDecl (VarDecl (VarName cIdent Nothing) (DeclAttrs noFunctionAttrs NoStorage []) cParamTy) undefNode] False) [])) (CCompound [] [CBlockStmt ifStat] undefNode) undefNode
            (entry, cfg) = fromFunDef fd

        -- Entry -> Branch -> (True -> Stat -> Exit, False -> Stat -> Exit)
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

        let isReturn (Node _ (StatNode (CReturn _ _))) = True
            isReturn _                                 = False
        tNode `shouldSatisfy` isReturn
        fNode `shouldSatisfy` isReturn
