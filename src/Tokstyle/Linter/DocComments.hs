{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module Tokstyle.Linter.DocComments (descr) where

import           Control.Monad               (forM_)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (CommentStyle (..), Lexeme (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic,
                                              HasDiagnosticsRich (..))
import           Language.Cimple.Pretty      (ppTranslationUnit)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (line, pretty)
import           Tokstyle.Common             (functionName, semEq, warnDoc)


data Linter = Linter
    { diags :: [Diagnostic CimplePos]
    , docs  :: [(Text, (FilePath, Node (Lexeme Text)))]
    }

empty :: Linter
empty = Linter [] []

instance HasDiagnosticsRich Linter CimplePos where
    addDiagnosticRich diag l@Linter{diags} = l{diags = diag : diags}


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            Commented doc entity -> do
                forM_ (functionName entity) $
                    checkCommentEquals file node doc
                act

            FunctionDefn{} -> return ()
            _ -> act
    }
  where
    checkCommentEquals file node doc fname = do
        l@Linter{docs} <- State.get
        case lookup fname docs of
            Nothing -> State.put l{docs = (fname, (file, node)):docs}
            Just (_, node') | semEq doc (getDoc node') -> return ()
            Just (file', node') -> do
                warnDoc file node $ "comment on definition of `" <> pretty fname
                    <> "` does not match declaration:" <> line
                    <> ppTranslationUnit [node]
                warnDoc file' node' $ "mismatching comment found here:" <> line
                    <> ppTranslationUnit [node']

    getDoc (Fix (Commented doc _)) = doc
    getDoc _                       = error "getDoc: not a Commented node"

associateComments :: [Node (Lexeme Text)] -> [Node (Lexeme Text)]
associateComments [] = []
associateComments (doc@(Fix c) : nextNode : rest)
    | isFunc (unFix nextNode) && isDocComment c =
        Fix (Commented doc nextNode) : associateComments rest
  where
    isFunc FunctionDecl{} = True
    isFunc FunctionDefn{} = True
    isFunc _              = False
    isDocComment (Comment Doxygen _ _ _) = True
    isDocComment _                       = False
associateComments (x:xs) = x : associateComments xs

analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse files =
    let processedFiles = map (fmap associateComments) files
    in reverse . diags . flip State.execState empty . traverseAst linter . reverse $ processedFiles

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("doc-comments", Text.unlines
    [ "Checks that doc comments on function definitions match the ones on their"
    , "corresponding declarations."
    , ""
    , "**Reason:** ideally, documentation should be only in one place, but if it is"
    , "duplicated, it should not be different."
    ]))
