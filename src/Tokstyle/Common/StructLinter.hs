{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module Tokstyle.Common.StructLinter
    ( MkFunBody
    , analyseStructs
    , mkLAt
    ) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), LexemeClass (..),
                                              Node, NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic,
                                              HasDiagnosticsRich (..))
import           Language.Cimple.Pretty      (ppTranslationUnit)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (line, pretty)
import           Tokstyle.Common             (semEq, warn, warnDoc)
import qualified Tokstyle.Common.TypeSystem  as TypeSystem
import           Tokstyle.Common.TypeSystem  (TypeDescr (..), TypeSystem)

newtype Linter = Linter
    { diags :: [Diagnostic CimplePos]
    }

instance HasDiagnosticsRich Linter CimplePos where
    addDiagnosticRich diag l@Linter{diags} = l{diags = diag : diags}

empty :: Linter
empty = Linter []

mkLAt :: Lexeme a -> LexemeClass -> a -> Lexeme a
mkLAt (L p _ _) = L p

type MkFunBody = TypeSystem -> Lexeme Text -> TypeDescr -> Maybe (Either Text (Node (Lexeme Text)))

checkStructs :: TypeSystem -> Text -> MkFunBody -> [(FilePath, [Node (Lexeme Text)])] -> State Linter ()
checkStructs tys funSuffix mkFunBody = traverseAst actions
  where
    actions :: AstActions (State Linter) Text
    actions = astActions
        { doNode = \file node act ->
            case unFix node of
                FunctionDefn _ (Fix (FunctionPrototype _ (L _ _ fname) (Fix (VarDecl _ varName _):_))) body
                    | funSuffix `Text.isSuffixOf` fname -> do
                    case TypeSystem.lookupType (Text.dropEnd (Text.length funSuffix) fname) tys of
                        Just e@(StructDescr (L _ _ sname) _) -> do
                            case mkFunBody tys varName e of
                                Nothing -> return ()
                                Just (Left err) ->
                                    warn file node $ "invalid struct format for `" <> sname <> "`: " <> err
                                Just (Right wanted) ->
                                    unless (body `semEq` wanted) $
                                        warnDoc file node $ "struct `" <> pretty funSuffix <> "` function for `" <> pretty sname <> "` should be:" <> line
                                            <> ppTranslationUnit [wanted]
                        _ -> return ()  -- not every _to_string function is for structs

                _ -> act
        }


analyseStructs :: Text -> MkFunBody -> [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyseStructs funSuffix mkFunBody =
    reverse . diags . flip State.execState empty . (\tus -> checkStructs (TypeSystem.collect tus) funSuffix mkFunBody tus) . reverse
