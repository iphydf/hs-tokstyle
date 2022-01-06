{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.Parens (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             (IdentityActions, Lexeme (..),
                                              Node, NodeF (..), defaultActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)


needsParens :: Node a -> Bool
needsParens n = case unFix n of
    BinaryExpr{}  -> True
    TernaryExpr{} -> True
    CastExpr{}    -> True
    _             -> False


linter :: IdentityActions (State [Text]) Text
linter = defaultActions
    { doNode = \file node act ->
        case unFix node of
            -- Extra parentheses inside macro body is allowed (and sometimes needed).
            PreprocDefineConst{} -> return node
            PreprocDefineMacro{} -> return node

            VarDeclStmt _ (Just (Fix ParenExpr{})) -> do
                warn file node $ "variable initialiser does not need parentheses"
                act
            ParenExpr expr | not $ needsParens expr -> do
                warn file node $ "expression does not need parentheses"
                act

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
