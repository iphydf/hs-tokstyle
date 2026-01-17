{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.FuncPrototypes (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme, Node, NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Tokstyle.Common             (warn)


linter :: AstActions (State [Diagnostic CimplePos]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionPrototype _ name [] -> do
                warn file name "empty parameter list must be written as `(void)`"
                act

            FunctionDefn{} -> return ()
            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("func-prototypes", Text.unlines
    [ "Checks that empty parameter lists in C functions are written as `(void)`."
    , ""
    , "**Reason:** old-style empty parameter lists written as `()` are risky, because"
    , "C interprets them as variadic. GCC warns about this but sometimes misses one."
    ]))
