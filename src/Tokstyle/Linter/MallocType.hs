{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.MallocType (descr) where

import           Control.Monad               (unless)
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (BinaryOp (..), Lexeme (..), Node,
                                              NodeF (..), Scope (..))
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic)
import           Language.Cimple.Pretty      (ppNode)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Prettyprinter               (pretty)
import           Tokstyle.Common             (semEq, skip, warnDoc)
import           Tokstyle.Common.Patterns

supportedTypes :: [Text]
supportedTypes = ["char", "uint8_t", "int16_t"]

mallocs :: [Text]
mallocs = ["mem_balloc", "malloc"]

isByteSize :: Node (Lexeme Text) -> Bool
isByteSize ty = case unFix ty of
    TyStd (L _ _ "char")    -> True
    TyStd (L _ _ "int8_t")  -> True
    TyStd (L _ _ "uint8_t") -> True
    _                       -> False

removeOwner :: Node (Lexeme Text) -> Node (Lexeme Text)
removeOwner (Fix (TyOwner ty)) = ty
removeOwner ty                 = ty

checkType :: FilePath -> Text -> Node (Lexeme Text) -> State [Diagnostic CimplePos] ()
checkType file malloc castTy = case unFix castTy of
    TyPointer (Fix (TyStd (L _ _ tyName))) | tyName `elem` supportedTypes -> return ()
    _ -> warnDoc file castTy $
        "`" <> pretty malloc <> "` should be used for builtin types only "
        <> "(e.g. `uint8_t *` or `int16_t *`); use `mem_alloc` instead"

checkExpr :: FilePath -> Text -> Node (Lexeme Text) -> State [Diagnostic CimplePos] ()
checkExpr file malloc size = case unFix size of
    SizeofType{} ->
        warnDoc file size $ "`sizeof` in call to `" <> pretty malloc <> "` should appear only once, "
            <> "and only on the right hand side of the expression"
    BinaryExpr l _ r -> do
        checkExpr file malloc l
        checkExpr file malloc r
    VarExpr{} -> return ()
    LiteralExpr{} -> return ()
    MemberAccess e _ -> checkExpr file malloc e
    PointerAccess e _ -> checkExpr file malloc e
    x ->
        warnDoc file size $ "`" <> pretty malloc <> "` should only have sizeof and simple expression arguments: " <> pretty (Text.pack (show x))

checkSize :: FilePath -> Text -> Node (Lexeme Text) -> Node (Lexeme Text) -> State [Diagnostic CimplePos] ()
checkSize file malloc castTy@(Fix (TyPointer objTy)) size = case unFix size of
    BinaryExpr l BopMul r -> do
        checkExpr file malloc l
        checkSize file malloc castTy r
    SizeofType sizeTy ->
        unless (sizeTy `semEq` objTy) $
            warnDoc file size $ "`size` argument in call to `" <> pretty malloc <> "` indicates "
                <> "creation of an array with element type `" <> ppNode sizeTy <> "`, "
                <> "but result is cast to `" <> ppNode castTy <> "`"
    _ ->
        unless (isByteSize objTy) $
            warnDoc file size $ "`" <> pretty malloc <> "` result must be cast to a byte-sized type if `sizeof` is omitted"
checkSize file malloc castTy _ =
    warnDoc file castTy $ "`" <> pretty malloc <> "` result must be cast to a pointer type"


linter :: AstActions (State [Diagnostic CimplePos]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            -- Windows API weirdness: ignore completely.
            CastExpr               (Fix (TyPointer (Fix (TyStd (L _ _ "IP_ADAPTER_INFO")))))   _ -> return ()
            CastExpr (Fix (TyOwner (Fix (TyPointer (Fix (TyStd (L _ _ "IP_ADAPTER_INFO"))))))) _ -> return ()

            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "malloc"))) [size])) -> do
                checkType file "malloc" (removeOwner castTy)
                checkSize file "malloc" (removeOwner castTy) size
            CastExpr castTy (Fix (FunctionCall (Fix (VarExpr (L _ _ "mem_balloc"))) [_, size])) -> do
                checkType file "mem_balloc" (removeOwner castTy)
                checkSize file "mem_balloc" (removeOwner castTy) size

            FunctionCall (Fix (VarExpr (L _ _ name))) _ | name `elem` mallocs ->
                warnDoc file node $ "the result of `" <> pretty name <> "` must be cast; plain `void *` is not supported"

            FunctionDefn Static (Fix (FunctionPrototype TY_void_ptr _ _)) _ ->
                -- Ignore static functions returning void pointers. These are allocator
                -- functions from mem.c.
                return ()

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos]
analyse = reverse . flip State.execState [] . traverseAst linter . skip
    [ "toxcore/mem.c"
    , "toxcore/os_memory.c"
    ]

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("malloc-type", Text.unlines
    [ "Checks that `mem_balloc` is only used for built-in types. For struct allocations"
    , "`mem_alloc` and other `calloc`-like functions should be used."
    , ""
    , "**Reason:** `mem_balloc` does not zero-initialise its memory, which is ok for"
    , "byte arrays (at most it can cause incorrect behaviour on most systems), but very"
    , "risky for aggregate types containing pointers, which can point at random (or"
    , "worse, attacker-controlled) memory."
    ]))
