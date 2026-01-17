{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.MemcpyStructs (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.Pretty      (ppNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty)
import           Tokstyle.Common             (warnDoc)

exemptions :: [Text]
exemptions =
    ["IP_Port"
    , "IP4"
    , "IP6"
    ]

checkSize :: Text -> Text -> FilePath -> Node (Lexeme Text) -> State [Diagnostic CimplePos] ()
checkSize fname instead file size = case unFix size of
    SizeofType ty@(Fix (TyUserDefined (L _ _ name))) | name `notElem` exemptions ->
        warnDoc file size $ "`" <> pretty fname <> "` should not be used for structs like `"
            <> ppNode ty <> "`; use " <> pretty instead <> " instead"

    _ -> return ()


linter :: AstActions (State [Diagnostic CimplePos]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionCall (Fix (VarExpr (L _ _ "memset"))) [_, _, size] ->
                checkSize "memset" "`(Type) {0}`" file size
            FunctionCall (Fix (VarExpr (L _ _ "memcpy"))) [_, _, size] ->
                checkSize "memcpy" "assignment" file size

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse = reverse . flip State.execState [] . traverseAst linter

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("memcpy-structs", Text.unlines
    ["Checks that `memcpy` and `memset` aren't used for struct pointers."
    , ""
    , "Exemptions are:"
    , ""
    , Text.intercalate "\n" . map (\x -> "- `" <> x <> "`") $ exemptions
    , ""
    , "**Reason:** structs can contain pointers, so `memset` is risky (it can create"
    , "invalid null pointer representations) and `memcpy` should be replaced by an"
    , "assignment, possibly in a loop, to avoid messing up the size argument of the"
    , "`memcpy` call."
    ]))
