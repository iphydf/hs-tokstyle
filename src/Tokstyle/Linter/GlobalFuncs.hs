{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.GlobalFuncs (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Scope (..), lexemeText)
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Prettyprinter               (pretty, (<+>))
import           System.FilePath             (takeExtension)
import           Tokstyle.Common             (backticks, warn, warnDoc)


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse (file, _) | takeExtension file /= ".c" = []
analyse (file, ast) = reverse $ State.execState (mapM go ast) []
  where
    go (Fix (FunctionDecl Global (Fix (FunctionPrototype _ name _)))) =
        warnDoc file name $
            "global function" <+> backticks (pretty (lexemeText name)) <+> "declared in .c file"
    go _ = return ()

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("global-funcs", Text.unlines
    [ "Checks that no extern functions are declared in .c files."
    , ""
    , "Extern functions must only be declared in .h files. In .c files all declarations"
    , "must be static."
    , ""
    , "**Reason:** extern declarations in .c files mean that we depend on a function"
    , "not declared in a .h file we can include. This means we're depending on an"
    , "unexported implementation detail, and there is no compiler that can check"
    , "whether our declaration matches the implementation's definition."
    ]))
