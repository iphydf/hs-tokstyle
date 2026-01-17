{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module Tokstyle.Linter.DeclaredOnce (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic,
                                              HasDiagnosticsRich (..))
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty, (<+>))
import           Tokstyle.Common             (backticks, warn, warnDoc)


data Linter = Linter
    { diags :: [Diagnostic CimplePos]
    , decls :: Map Text (FilePath, Lexeme Text)
    }

empty :: Linter
empty = Linter [] Map.empty

instance HasDiagnosticsRich Linter CimplePos where
    addDiagnosticRich diag l@Linter{diags} = l{diags = diag : diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDecl _ (Fix (FunctionPrototype _ fn@(L _ IdVar fname) _)) -> do
                l@Linter{decls} <- State.get
                case Map.lookup fname decls of
                    Nothing -> State.put l{decls = Map.insert fname (file, fn) decls }
                    Just (file', fn') -> do
                        warnDoc file' fn' $ "duplicate declaration of function" <+> backticks (pretty fname)
                        warnDoc file fn $ "function" <+> backticks (pretty fname) <+> "also declared here"

            FunctionDefn{} -> pure ()
            _ -> act
    }

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse . diags $ State.execState (traverseAst linter tus) empty

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("declared-once", Text.unlines
    [ "Checks that any function is declared exactly once."
    , ""
    , "**Reason:** functions should never be declared in multiple files, and within the"
    , "same file, declaring it twice is unnecessary and confusing."
    ]))
