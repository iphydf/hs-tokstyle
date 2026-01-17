{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Strict                #-}
module Tokstyle.Linter.OwnershipDecls (descr) where

import           Control.Monad               (unless, when)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import qualified Data.List                   as List
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..),
                                              Scope (..), lexemeText)
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic,
                                              HasDiagnosticsRich (..))
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (hsep, pretty, punctuate, (<+>))
import           Tokstyle.Common             (backticks, warn, warnDoc)


data Linter = Linter
    { diags :: [Diagnostic CimplePos]
    , decls :: Set Text
    }

empty :: Linter
empty = Linter [] Set.empty

instance HasDiagnosticsRich Linter CimplePos where
    addDiagnosticRich diag l@Linter{diags} = l{diags = diag : diags}


findQualifiers :: Node (Lexeme Text) -> [Text]
findQualifiers (Fix node) = case node of
    TyOwner t    -> "_Owner" : findQualifiers t
    TyNonnull t  -> "_Nonnull" : findQualifiers t
    TyNullable t -> "_Nullable" : findQualifiers t
    TyConst t    -> findQualifiers t
    TyPointer t  -> findQualifiers t
    _            -> []

findPrototypeQualifiers :: Node (Lexeme Text) -> [Text]
findPrototypeQualifiers (Fix (FunctionPrototype retType _ params)) =
    findQualifiers retType ++ concatMap findParamQualifiers params
  where
    findParamQualifiers (Fix (VarDecl ty _ _)) = findQualifiers ty
    findParamQualifiers _                      = []
findPrototypeQualifiers _ = []


linter :: AstActions (State Linter) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDecl _ (Fix (FunctionPrototype _ name _)) -> do
                State.modify $ \s -> s { decls = Set.insert (lexemeText name) (decls s) }
                act

            FunctionDefn scope proto@(Fix (FunctionPrototype _ name _)) _ -> do
                Linter{decls} <- State.get
                let nameText = lexemeText name
                let isDeclared = nameText `Set.member` decls
                let qs = List.nub $ findPrototypeQualifiers proto
                unless (null qs || (scope == Static && not isDeclared)) $
                    warnDoc file name $ "qualifier" <> (if length qs > 1 then "s" else "")
                        <+> hsep (punctuate " and" (map (backticks . pretty) qs))
                        <+> "should only be used on function declarations, not definitions"
                act

            _ -> act
    }


analyse :: [(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse . diags $ State.execState (traverseAst linter tus) empty

descr :: ([(FilePath, [Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("ownership-decls", Text.unlines
    [ "Checks that `_Owner`, `_Nullable`, and `_Nonnull` are only set on declarations,"
    , "not definitions, unless it's a static definition without prior declaration."
    , ""
    , "**Reason:** keeping qualifiers on declarations only reduces clutter in the"
    , "implementation and ensures that the interface is the single source of truth"
    , "for ownership or nullability information."
    ]))
