{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.CallocArgs (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp (BopMul), Lexeme (..),
                                              Node, NodeF (..), Scope (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty, (<+>))
import qualified Tokstyle.Common             as Common
import           Tokstyle.Common             (backticks, warn, warnDoc)
import           Tokstyle.Common.Patterns


checkSize, checkNmemb :: Text -> FilePath -> Node (Lexeme Text) -> State [Diagnostic CimplePos] ()
checkSize funName file size = case unFix size of
    SizeofType{} -> return ()
    _ -> warnDoc file size $ backticks "size" <+> "argument in call to" <+> backticks (pretty funName) <+> "must be a sizeof expression"

checkNmemb funName file nmemb = case unFix nmemb of
    LiteralExpr{}     -> return ()
    VarExpr{}         -> return ()
    ParenExpr e       -> checkNmemb funName file e
    PointerAccess e _ -> checkNmemb funName file e
    BinaryExpr l _ r -> do
        checkNmemb funName file l
        checkNmemb funName file r

    SizeofType{} ->
        warnDoc file nmemb $ backticks "sizeof" <+> "should not appear in the" <+> backticks "nmemb" <+> "argument to" <+> backticks (pretty funName)

    _ ->
        warnDoc file nmemb $ "invalid expression in" <+> backticks "nmemb" <+> "argument to" <+> backticks (pretty funName)


pattern Calloc :: Text -> [Node (Lexeme Text)] -> Node (Lexeme Text)
pattern Calloc funName args <- Fix (FunctionCall (Fix (VarExpr (L _ _ funName))) args)

linter :: AstActions (State [Diagnostic CimplePos]) Text
linter = astActions
    { doNode = \file node act -> case node of
        Calloc funName@"calloc" [nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"realloc" [_, Fix (BinaryExpr nmemb BopMul size)] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"mem_alloc" [_, size] -> do
            checkSize funName file size
        Calloc funName@"mem_valloc" [_, nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size
        Calloc funName@"mem_vrealloc" [_, _, nmemb, size] -> do
            checkNmemb funName file nmemb
            checkSize funName file size

        Calloc "calloc"       _ -> warnDoc file node $ "invalid" <+> backticks "calloc" <+> "invocation: 2 arguments expected"
        Calloc "realloc"      _ -> warnDoc file node $ "invalid" <+> backticks "realloc" <+> "invocation: 2 arguments expected"
        Calloc "mem_alloc"    _ -> warnDoc file node $ "invalid" <+> backticks "mem_alloc" <+> "invocation: 1 argument after" <+> backticks "mem" <+> "expected"
        Calloc "mem_valloc"   _ -> warnDoc file node $ "invalid" <+> backticks "mem_valloc" <+> "invocation: 2 arguments after" <+> backticks "mem" <+> "expected"
        Calloc "mem_vrealloc" _ -> warnDoc file node $ "invalid" <+> backticks "mem_vrealloc" <+> "invocation: 3 argument after" <+> backticks "mem" <+> "expected"

        Fix (FunctionDefn Static (Fix (FunctionPrototype TY_void_ptr _ _)) _) ->
            -- Ignore static functions returning void pointers. These are allocator
            -- functions from mem.c.
            return ()

        _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse = reverse . flip State.execState [] . traverseAst linter . Common.skip
    [ "toxav/rtp.c"
    , "toxcore/list.c"
    , "toxcore/mem.c"
    , "toxcore/os_memory.c"
    ]

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("calloc-args", Text.unlines
    [ "Checks that `mem_alloc`, `mem_valloc`, and `mem_vrealloc` are used correctly:"
    , ""
    , "- The `size` argument (e.g. for `mem_alloc`, the second argument) should be a"
    , "  pure `sizeof` expression without additions or multiplications."
    , "- There should be no `sizeof` in the `nmemb` argument of a memory allocation"
    , "  call."
    , ""
    , "**Reason:** we want to avoid arbitrary computations in allocation sizes to"
    , "ensure the allocation size is exactly correct for the type of the object"
    , "being allocated."
    ]))
