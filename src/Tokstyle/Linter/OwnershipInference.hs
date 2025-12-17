{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeApplications      #-}

module Tokstyle.Linter.OwnershipInference (descr) where

import           Control.Monad.State.Strict
import           Control.Monad               (forM_, unless, when, mplus)
import           Data.Fix                    (Fix (..), unFix)
import           Data.List                   (find, foldl', splitAt)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (mapMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..), getLoc)
import qualified Language.Cimple             as C
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode, traverseAst)
import           Language.Cimple.Analysis.DataFlow  (CFG, CFGNode (..), DataFlow (..), buildCFG, fixpoint)
import           Language.Cimple.Analysis.Scope     (ScopedId (..), runScopePass, sidName)
import           Tokstyle.Common.TypeSystem  (collect)
import           Tokstyle.Analysis.Linear   (AnalysisState, LinearContext (..), ScopedTranslationUnit, ScopedNode)
import qualified Tokstyle.Analysis.Linear   as Linear

data Usage = Uninit | Unused | Used | Freed | Returned | Aliased | Mixed deriving (Eq, Show)

data InferenceFacts =
    InferenceFacts { varUsages :: Map ScopedId Usage }
    deriving (Eq, Show)

newtype InferenceContext l = InferenceContext ()

instance DataFlow (State InferenceFacts) InferenceContext ScopedId InferenceFacts () where
    emptyFacts _ = return $ InferenceFacts Map.empty
    join _ f1 f2 = return $ InferenceFacts $ Map.unionWith joinUsage (varUsages f1) (varUsages f2)
      where
        joinUsage u1 u2 | u1 == u2 = u1
                        | u1 == Uninit = u2
                        | u2 == Uninit = u1
                        | otherwise = Mixed

    transfer _ _ _ facts stmt = do
        put facts
        handleStmt stmt
        finalFacts <- get
        return (finalFacts, Set.empty)
      where
        handleStmt (Fix (C.VarDeclStmt (Fix (C.VarDecl _ (L _ _ sid) _)) mInit)) = do
            case mInit of
                Just initExpr | isAlloc initExpr -> 
                    modify $ \f -> f { varUsages = Map.insert sid Unused (varUsages f) }
                Just (Fix (C.VarExpr (L _ _ srcSid))) -> mark srcSid Aliased
                Just initExpr -> handleExpr initExpr
                Nothing -> return ()
        handleStmt (Fix (C.ExprStmt e)) = handleExpr e
        handleStmt (Fix (C.Return (Just e))) = handleReturn e
        handleStmt _ = return ()

        handleExpr (Fix (C.AssignExpr (Fix (C.VarExpr (L _ _ sid))) C.AopEq rhs))
            | isAlloc rhs = do
                alreadyTracked <- gets (Map.member sid . varUsages)
                if alreadyTracked
                then mark sid Aliased
                else modify $ \f -> f { varUsages = Map.insert sid Unused (varUsages f) }
        handleExpr (Fix (C.AssignExpr lhs C.AopEq rhs)) = do
            handleExpr lhs
            case unFix rhs of
                C.VarExpr (L _ _ sid) -> mark sid Aliased
                _ -> handleExpr rhs
        handleExpr (Fix (C.FunctionCall (Fix (C.VarExpr (L _ _ f))) args)) = do
            if sidName f `elem` ["free", "mem_free", "mem_delete"]
            then case args of
                [Fix (C.VarExpr (L _ _ _)), Fix (C.VarExpr (L _ _ sid))] -> mark sid Freed
                [Fix (C.VarExpr (L _ _ sid))] -> mark sid Freed
                _ -> mapM_ handleExpr args
            else mapM_ handleExpr args
        handleExpr (Fix (C.ArrayAccess (Fix (C.VarExpr (L _ _ _))) i)) = handleExpr i
        handleExpr (Fix (C.VarExpr (L _ _ sid))) = mark sid Used
        handleExpr (Fix (C.CastExpr _ e)) = handleExpr e
        handleExpr (Fix (C.ParenExpr e)) = handleExpr e
        handleExpr (Fix (C.MemberAccess e _)) = handleExpr e
        handleExpr (Fix (C.PointerAccess e _)) = handleExpr e
        handleExpr (Fix (C.UnaryExpr C.UopAddress e)) = handleExpr e
        handleExpr (Fix (C.UnaryExpr C.UopDeref e)) = handleExpr e
        handleExpr (Fix (C.ArrayAccess e i)) = handleExpr e >> handleExpr i
        handleExpr _ = return ()

        handleReturn (Fix (C.VarExpr (L _ _ sid))) = mark sid Returned
        handleReturn e = handleExpr e

        mark sid usage = modify $ \f -> 
            let update old = case (old, usage) of
                    (Uninit, _)  -> usage
                    (Unused, _)  -> usage
                    (_, Aliased) -> Aliased
                    (Used, _)    -> usage
                    _            -> old
            in f { varUsages = Map.adjust update sid (varUsages f) }

        isAlloc (Fix (C.FunctionCall (Fix (C.VarExpr (L _ _ f))) _)) = 
            sidName f `elem` ["malloc", "calloc", "mem_alloc", "mem_valloc"]
        isAlloc (Fix (C.CastExpr _ e)) = isAlloc e
        isAlloc _ = False

analyseInference :: [(FilePath, [Node (C.Lexeme Text)])] -> [Text]
analyseInference sources = 
    let 
        flatAst = concatMap snd sources
        ts = collect sources
        (scopedAsts, _) = runScopePass flatAst
        scopedTUs = snd $ foldl' (\(asts, acc) (fp, nodes) -> 
            let (thisScoped, rest) = splitAt (length nodes) asts
            in (rest, acc ++ [(fp, thisScoped)])) (scopedAsts, []) sources
        
        info = Linear.collectInfo ts scopedTUs
    in 
        reverse $ execState (mapM_ (checkTU info) scopedTUs) []
  where
    checkTU info (file, nodes) = forM_ nodes $ \node -> case unFix node of
        C.FunctionDefn _ (Fix (C.FunctionPrototype retType (L _ _ name) params)) body -> do
            let isOwned (Fix (C.VarDecl ty _ _)) = hasOwned ty
                isOwned _                        = False
                hasOwned (Fix node') = case node' of
                    C.TyOwner _    -> True
                    C.TyPointer t  -> hasOwned t
                    C.TyConst t    -> hasOwned t
                    C.TyNullable t -> hasOwned t
                    C.TyNonnull t  -> hasOwned t
                    _              -> False
                
                -- Check pointers for candidates
                isPointer (Fix node') = case node' of
                    C.TyPointer _  -> True
                    C.TyNullable t -> isPointer t
                    C.TyNonnull t  -> isPointer t
                    C.TyConst t    -> isPointer t
                    _              -> False

            -- Skip if it's already using linear types
            unless (hasOwned retType || any isOwned params) $ do
                let initialUsages = Map.fromList [ (sid, Uninit) | Fix (C.VarDecl _ (L _ _ sid) _) <- params ]
                let sidTypes = execState (traverseAst @ScopedId collector body) Map.empty
                    collector :: AstActions (State (Map ScopedId (Node (C.Lexeme ScopedId), [Node (C.Lexeme ScopedId)]))) ScopedId
                    collector = astActions
                        { doNode = \_ n act -> do
                            case unFix n of
                                C.VarDecl ty (L _ _ sid) dsa -> modify $ Map.insert sid (ty, dsa)
                                _ -> return ()
                            act
                        }

                let (cfg, _) = flip evalState (InferenceFacts initialUsages) $ do
                        c <- buildCFG (InferenceContext ()) node (InferenceFacts initialUsages)
                        fixpoint (InferenceContext ()) name c

                let exitNodes = filter (null . cfgSuccs) (Map.elems cfg)
                let candidates = Set.fromList [ sid | n <- exitNodes
                                              , (sid, usage) <- Map.toList (varUsages (cfgOutFacts n))
                                              , usage `elem` [Freed, Returned]
                                              ]
                
                -- Check each candidate with Linear Analysis
                when (not (Set.null candidates)) $ do
                    -- Construct LinearFacts pretending candidates are owned
                    let initialVarStates = Map.fromList 
                            [ (Linear.Path sid [], Linear.Owned) 
                            | sid <- Set.toList candidates
                            ]
                    let initialVarTypes = Map.fromList
                            [ (Linear.Path sid [], True)
                            | sid <- Set.toList candidates
                            ]

                    -- Also include parameter types as borrowed if not candidates
                    let paramStates = Map.fromList 
                            [ (Linear.Path sid [], Linear.BorrowedMut) 
                            | Fix (C.VarDecl _ (L _ _ sid) _) <- params
                            , sid `Set.notMember` candidates
                            ]
                    
                    let combinedStates = Map.union initialVarStates paramStates
                    let combinedTypes = initialVarTypes -- Only owned things need to be in types map

                    let initialFacts = Linear.LinearFacts combinedStates combinedTypes Map.empty Map.empty Map.empty Set.empty
                    
                    -- Run full analysis on this function
                    let (finalCfg, _) = flip evalState initialFacts $ do
                            c <- Linear.buildCFG (LinearContext info) node initialFacts
                            Linear.fixpoint (LinearContext info) name c
                    
                    let linearExitNodes = filter (null . cfgSuccs) (Map.elems finalCfg)
                    let linearWarnings = Set.unions [ Linear.warnings (cfgOutFacts n) | n <- linearExitNodes ]
                    
                    -- If NO warnings, then report candidates
                    when (Set.null linearWarnings) $ do
                         forM_ (Set.toList candidates) $ \sid -> 
                            let (msgPrefix, entity, ty, declSpecArrays) = 
                                    case find (\(Fix (C.VarDecl _ (L _ _ psid) _)) -> psid == sid) params of
                                        Just (Fix (C.VarDecl pty _ dsa)) -> ("parameter", "`" <> sidName sid <> "`", pty, dsa)
                                        _ -> case Map.lookup sid sidTypes of 
                                                Just (vty, vdsa) -> ("variable", "`" <> sidName sid <> "`", vty, vdsa)
                                                _ -> ("variable", "`" <> sidName sid <> "`", retType, [])

                            in when (isPointer ty || not (null declSpecArrays)) $ 
                                warn file (getLoc node) $ "candidate for linear types: " <> msgPrefix <> " " <> entity <> " in function `" <> sidName name <> "`"

        _ -> return ()

descr :: ([(FilePath, [Node (C.Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyseInference, ("ownership-inference", Text.unlines 
    ["Identify functions that appear to follow linear ownership patterns"
    , "but are not yet marked with 'owner' qualifiers. This suggests"
    , "candidates for easy conversion to linear types."
    ]))
