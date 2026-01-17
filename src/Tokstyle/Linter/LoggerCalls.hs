{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.LoggerCalls (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LiteralType (String),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty, (<+>))
import           System.FilePath             (takeFileName)
import           Tokstyle.Common             (backticks, warn, warnDoc)


linter :: AstActions (State [Diagnostic CimplePos]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            -- Ignore all function calls where the second argument is a string
            -- literal. If it's a logger call, it's a valid one.
            FunctionCall _ (_:Fix (LiteralExpr String _):_) -> act
            -- LOGGER_ASSERT has its format as the third parameter.
            FunctionCall (Fix (LiteralExpr _ (L _ _ "LOGGER_ASSERT"))) (_:_:Fix (LiteralExpr String _):_) -> act

            FunctionCall (Fix (LiteralExpr _ name@(L _ _ func))) _ | Text.isPrefixOf "LOGGER_" func -> do
                warnDoc file name $ "logger call" <+> backticks (pretty func) <+> "has a non-literal format argument"
                act

            _ -> act
    }


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
-- Ignore logger.h, which contains a bunch of macros that call LOGGER functions
-- with their (literal) arguments. We don't know that they are literals at this
-- point, though.
analyse (file, _) | takeFileName file == "logger.h" = []
analyse tu = reverse . flip State.execState [] . traverseAst linter $ tu

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("logger-calls", Text.unlines
    [ "Checks that the format argument in LOGGER calls is a string literal."
    , ""
    , "**Reason:** format arguments must always be string literals so they can be"
    , "statically checked to match with their argument list."
    ]))
