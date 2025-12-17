{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tokstyle.Linter.LinearTypes
    ( descr
    , Linear.isPathOwned
    , Linear.joinOwnership
    , Linear.Path (..)
    , Linear.Ownership (..)
    , Linear.LinearFacts (..)
    , LinterState
    ) where

import           Control.Monad              (forM_, when)
import           Control.Monad.State.Strict
import           Data.Fix                   (Fix (..), unFix)
import           Data.List                  (foldl', splitAt)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                 (fromMaybe, mapMaybe)
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Language.Cimple            (Lexeme (..), Node, getLoc)
import qualified Language.Cimple            as C
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.Analysis.DataFlow (CFG, CFGNode (..))
import           Tokstyle.Analysis.Linear   (AnalysisState, LinearContext (..),
                                             ScopedNode, ScopedTranslationUnit)
import qualified Tokstyle.Analysis.Linear   as Linear
import           Language.Cimple.Analysis.Scope    (ScopedId (..), runScopePass,
                                             sidName)
import           Tokstyle.Common.TypeSystem (collect)

type LinterState = AnalysisState

checkLinearTypes :: [ScopedTranslationUnit] -> LinterState -> State [Text] ()
checkLinearTypes tus info = forM_ tus $ \(file, nodes) -> do
    let checked = Linear.checkedFuncs info
    forM_ nodes $ \node -> case unFix node of
        C.FunctionDefn _ (Fix (C.FunctionPrototype _ (L _ _ sid) params)) body ->
            when (sid `Set.member` checked) $ do
                let initialVarStates = Map.fromList $ mapMaybe paramState params
                let initialVarTypes = Map.fromList $ mapMaybe paramType params
                let initialFacts = Linear.LinearFacts initialVarStates initialVarTypes Map.empty Map.empty Map.empty Set.empty
                let (finalCfg, _) = flip evalState initialFacts $ do
                        cfg <- Linear.buildCFG (LinearContext info) node initialFacts
                        Linear.fixpoint (LinearContext info) sid cfg
                let fallbackLoc = case unFix body of
                        C.CompoundStmt stmts | not (null stmts) -> getLoc (last stmts)
                        _ -> getLoc node
                reportWarnings info fallbackLoc sid params file finalCfg
        _ -> return ()

  where
    paramState (Fix (C.VarDecl ty (L _ _ sid) _))
        | Linear.isOwner ty = Just (Linear.Path sid [], Linear.Owned)
        | otherwise   = Just (Linear.Path sid [], Linear.BorrowedMut)
    paramState _ = Nothing

    paramType (Fix (C.VarDecl ty (L _ _ sid) _)) = Just (Linear.Path sid [], Linear.isOwner ty)
    paramType _ = Nothing

findEnforcementPath :: LinterState -> ScopedId -> Maybe [Text]
findEnforcementPath info startSid = go (Set.singleton startSid) [[sidName startSid]]
  where
    go visited [] = Nothing
    go visited (path : rest) =
        case path of
            [] -> go visited rest
            (currName : _) ->
                let currSids = [ sid | sid <- Map.keys (Linear.reverseCallGraph info), sidName sid == currName ]
                    callers = Set.fromList $ concatMap (\sid -> Set.toList $ Map.findWithDefault Set.empty sid (Linear.reverseCallGraph info)) currSids
                    newCallers = Set.filter (`Set.notMember` visited) callers
                in if any (`Set.member` Linear.inherentlyCheckedFuncs info) (Set.toList callers)
                   then let inherently = Set.toList $ Set.intersection callers (Linear.inherentlyCheckedFuncs info)
                        in case inherently of
                            (h:_) -> Just (sidName h : path)
                            []    -> error "findEnforcementPath: inherently checked set empty despite check"
                   else go (Set.union visited newCallers) (rest ++ [ sidName c : path | c <- Set.toList newCallers ])

reportWarnings :: LinterState -> Lexeme ScopedId -> ScopedId -> [ScopedNode] -> FilePath -> CFG ScopedId Linear.LinearFacts -> State [Text] ()
reportWarnings linterInfo fallbackLoc name _params file cfg = 
    let 
        exitNodes = filter (null . cfgSuccs) (Map.elems cfg)
        enforcementPath = if name `Set.member` Linear.inherentlyCheckedFuncs linterInfo
                          then Nothing
                          else findEnforcementPath linterInfo name
        noteMsg = case enforcementPath of
            Just path -> "note: this function is being checked because it is reachable from a function that uses 'owner' qualifiers: " <> Text.intercalate " -> " path
            Nothing -> "note: this function is being checked because it is reachable from a function that uses 'owner' qualifiers"
    in 
        forM_ exitNodes $ \node -> do 
            let facts = cfgOutFacts node 
            forM_ (Set.toList $ Linear.warnings facts) $ \(msg, nodeLoc) -> do
                let finalLoc = if C.lexemeLine nodeLoc == 0 then fallbackLoc else nodeLoc 
                warn file finalLoc msg 
                when (isJust enforcementPath) $ 
                    warn file finalLoc noteMsg

            let finalStates = Linear.varStates facts 
                allPaths = Map.keysSet (Linear.varTypes facts) `Set.union` Map.keysSet finalStates 
            forM_ (Set.toList allPaths) $ \path -> do 
                let Linear.Path sid members = path 
                when (sid `Map.member` Map.fromList [ (sid', ()) | Linear.Path sid' _ <- Map.keys finalStates ] && Linear.isPathOwned linterInfo facts path && not (Linear.isPathMoved linterInfo facts path)) $ do 
                    let (msgPrefix, pathStr) = if null members 
                            then ("variable", sidName sid) 
                            else ("member", Linear.showPath path) 
                        loc = case reverse (cfgStmts node) of 
                            []    -> fallbackLoc 
                            (s:_) -> getLoc s 
                        msg = "leak: " <> msgPrefix <> " `" <> pathStr <> "` is still owned at function exit"
                    warn file loc msg 
                    when (isJust enforcementPath) $ 
                        warn file loc noteMsg
  where 
    isJust (Just _) = True
    isJust Nothing = False

analyseLinear :: [(FilePath, [Node (C.Lexeme Text)])] -> [Text]
analyseLinear sources = 
    let 
        flatAst = concatMap snd sources 
        ts = collect sources 
        (scopedAsts, _) = runScopePass flatAst 

        -- Re-group scopedAsts by file 
        scopedTUs = snd $ foldl' (\(asts, acc) (fp, nodes) -> 
            let (thisScoped, rest) = splitAt (length nodes) asts 
            in (rest, acc ++ [(fp, thisScoped)])) (scopedAsts, []) sources 

        info = Linear.propagateChecked $ Linear.collectInfo ts scopedTUs 
        linterM = checkLinearTypes scopedTUs info 
    in 
        reverse $ execState linterM [] 

descr :: ([(FilePath, [Node (C.Lexeme Text)])] -> [Text], (Text, Text))
descr = (analyseLinear, ("linear-types", Text.unlines 
    [ "Check for Rust-like linear types (ownership and borrowing)."
    , "This linter is incremental: any function that uses `owned` qualifiers,"
    , "and any function called by such a function, is fully checked."
    ]))