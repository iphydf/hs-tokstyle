{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module Tokstyle.Linter.FuncScopes (descr) where

import           Control.Monad                 (when)
import           Control.Monad.State.Strict    (State)
import qualified Control.Monad.State.Strict    as State
import           Data.Fix                      (Fix (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text
import           Language.Cimple               (Lexeme (..), Node, NodeF (..),
                                                Scope (..), lexemeLine,
                                                lexemeText)
import           Language.Cimple.Diagnostics   (CimplePos, Diagnostic,
                                                HasDiagnosticsRich (..))
import           Language.Cimple.TraverseAst   (AstActions, astActions, doNode,
                                                traverseAst)
import           Prettyprinter                 (Doc, pretty, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle)
import           Tokstyle.Common               (backticks, warn, warnDoc)


data Linter = Linter
    { diags :: [Diagnostic CimplePos]
    , decls :: [(Text, (Lexeme Text, Scope))]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnosticsRich Linter CimplePos where
    addDiagnosticRich diag l@Linter{diags} = l{diags = diag : diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDecl declScope (Fix (FunctionPrototype _ name _)) ->
                State.modify $ \l@Linter{decls} -> l{decls = (lexemeText name, (name, declScope)) : decls}
            FunctionDefn defnScope (Fix (FunctionPrototype _ name _)) _ -> do
                Linter{decls} <- State.get
                case lookup (lexemeText name) decls of
                    Nothing -> return ()
                    Just (decl, declScope) ->
                        when (declScope /= defnScope) $ warnDoc file name $
                            warning decl declScope defnScope

            _ -> act
    }
  where
    warning :: Lexeme Text -> Scope -> Scope -> Doc AnsiStyle
    warning decl declScope defnScope =
        "function definition" <+> backticks (pretty (lexemeText decl))
        <+> "does not agree with its declaration about scope: "
        <> "declaration on line" <+> pretty (lexemeLine decl)
        <+> "is" <+> backticks (pretty (scopeKeyword declScope)) <+> "but definition is"
        <+> backticks (pretty (scopeKeyword defnScope))

    scopeKeyword :: Scope -> Text
    scopeKeyword Global = "extern"
    scopeKeyword Static = "static"
    scopeKeyword Local  = "local"

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse = reverse . diags . flip State.execState empty . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("func-scopes", Text.unlines
    [ "Checks that static function definitions are marked with `static`."
    , ""
    , "In C, a function is `static` even if the definition doesn't use `static`, but"
    , "there happens to be another declaration of the function which does."
    , ""
    , "**Reason:** static/extern qualification of functions should be visible locally."
    , "It takes mental effort otherwise to look up the declaration to check for storage"
    , "qualifiers."
    ]))
