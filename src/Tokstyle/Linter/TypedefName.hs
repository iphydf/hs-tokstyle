{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.TypedefName (descr) where

import           Control.Applicative           ((<|>))
import           Control.Monad.State.Strict    (State)
import qualified Control.Monad.State.Strict    as State
import           Data.Fix                      (Fix (..))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Cimple               (Lexeme (..), Node, NodeF (..),
                                                lexemeText)
import           Language.Cimple.Diagnostics   (CimplePos, Diagnostic)
import           Language.Cimple.TraverseAst   (AstActions, astActions, doNode,
                                                traverseAst)
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.Common               (backticks, warn, warnDoc)

valid :: Lexeme Text -> Lexeme Text -> Bool
valid (L _ _ tname) (L _ _ sname) =
    sname == tname || fromMaybe False (do
        t <- Text.stripSuffix "_t" tname
        s <- Text.stripSuffix "_s" sname <|> Text.stripSuffix "_u" sname <|> Text.stripSuffix "_e" sname
        return $ t == s)

linter :: AstActions (State [Diagnostic CimplePos]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Typedef (Fix (TyStruct sname)) tname _ | not $ valid tname sname ->
                warnDoc file sname $ warning "struct" tname sname
            Typedef (Fix (Struct sname _)) tname _ | not $ valid tname sname ->
                warnDoc file sname $ warning "struct" tname sname
            Typedef (Fix (Union uname _)) tname _ | not $ valid tname uname ->
                warnDoc file uname $ warning "union" tname uname
            EnumDecl ename _ tname | not $ valid tname ename ->
                warnDoc file ename $ warning "enum" tname ename

            FunctionDefn{} -> return ()
            _ -> act
    }
  where
    warning :: Text -> Lexeme Text -> Lexeme Text -> Doc AnsiStyle
    warning tag tname name =
        "typedef name" <+> backticks (pretty (lexemeText tname)) <+> "does not match" <+> pretty tag
        <+> "name" <+> backticks (pretty (lexemeText name))

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("typedef-name", Text.unlines
    [ "Checks that typedef names match the struct/union name. E.g."
    , "`typedef struct Foo_ { ... } Foo;` should instead be"
    , "`typedef struct Foo { ... } Foo;`."
    , ""
    , "**Reason:** there is no good reason for them to be different, and it adds"
    , "confusion and a potential for C++ code to pick the wrong name and later break"
    , "in refactorings."
    ]))
