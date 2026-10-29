{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.AllocatorSwap (descr) where

import           Control.Monad.State.Strict         (State)
import qualified Control.Monad.State.Strict         as State
import           Data.Fix                           (Fix (..))
import qualified Data.List                          as List
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Language.Cimple                    (Lexeme (..), NodeF (..))
import qualified Language.Cimple                    as C
import           Language.Cimple.Diagnostics        (CimplePos, Diagnostic (..),
                                                     DiagnosticLevel (..),
                                                     HasDiagnosticInfo (..),
                                                     IsPosition (..), warnRich)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Common                    (backticks)


-- | The fields of the structs of the program that hold an allocator.
allocatorFields :: [(FilePath, [C.Node (Lexeme Text)])] -> Set Text
allocatorFields tus = Set.fromList
    [ name
    | (_, nodes) <- tus, node <- nodes
    , Fix (MemberDecl (Fix (VarDecl ty (L _ _ name) _)) _) <- universe node
    , isAllocator ty ]
  where
    isAllocator ty = not (null [ () | Fix t <- universe ty, isMemory t ]) && isPointer ty
    isMemory = \case
        TyUserDefined (L _ _ "Memory") -> True
        TyStruct (L _ _ "Memory")      -> True
        _                              -> False
    isPointer ty = not (null [ () | Fix (TyPointer _) <- universe ty ])

-- | The local a call's result is given to, as in @T *x = f();@ or @x = f();@.
givenFrom :: Function -> Map String [Lexeme Text]
givenFrom fn = Map.fromListWith (++)
    [ (Text.unpack v, [calleeLexeme callee])
    | (v, Just e) <- [ (Text.pack v', ini) | (v', ini) <- localDecls (fnDefn fn) ]
                 ++ [ (Text.pack v', Just r) | (v', rs) <- Map.toList (assignments (fnDefn fn)), r <- rs ]
    , Fix (FunctionCall callee _) <- [bare e] ]

bare :: C.Node (Lexeme Text) -> C.Node (Lexeme Text)
bare = \case
    Fix (ParenExpr e)  -> bare e
    Fix (CastExpr _ e) -> bare e
    e                  -> e

-- | Stores to an allocator field of a local: the local, the field, and the
-- assignment.
allocatorStores :: Set Text -> Function -> [(String, Text, C.Node (Lexeme Text))]
allocatorStores fields fn =
    [ (Text.unpack v, f, e)
    | e@(Fix (AssignExpr (Fix (PointerAccess (Fix (VarExpr (L _ _ v))) (L _ _ f))) _ _)) <- universe (fnDefn fn)
    , f `Set.member` fields ]

-- | The functions that return an object they set the allocator of, themselves
-- or through the function they got it from.
settingAllocator :: Set Text -> [Function] -> Set Text
settingAllocator fields fns = fixpoint Set.empty
  where
    fixpoint s = let s' = List.foldl' step s fns in if s' == s then s else fixpoint s'
    step s fn
        | any (sets s fn) (returnedLocals fn) = Set.insert (fnName fn) s
        | otherwise = s
    sets s fn v = any (\(v', _, _) -> v' == v) (allocatorStores fields fn)
        || any (\(L _ _ g) -> g `Set.member` s) (Map.findWithDefault [] v (givenFrom fn))
    returnedLocals fn = [ Text.unpack v | Fix (Return (Just e)) <- universe (fnDefn fn), Fix (VarExpr (L _ _ v)) <- [bare e] ]

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ State.execState (mapM_ report findings) []
  where
    fns = definitions tus
    fields = allocatorFields tus
    setting = settingAllocator fields fns
    findings =
        [ (fnFile fn, store, Text.pack v <> "->" <> field, maker)
        | fn <- fns
        , (v, field, store) <- allocatorStores fields fn
        , maker@(L _ _ g) <- Map.findWithDefault [] v (givenFrom fn)
        , g `Set.member` setting ]

report :: (FilePath, C.Node (Lexeme Text), Text, Lexeme Text) -> State [Diagnostic CimplePos] ()
report (file, store, field, maker@(L _ _ g)) = do
    let (pos, len) = getDiagnosticInfo file store
        (makerPos, _) = getDiagnosticInfo file maker
    warnRich $ Diagnostic pos len ErrorLevel
        (backticks (pretty field) <+> "is set here, but" <+> backticks (pretty g)
            <+> "at line" <+> pretty (posLine makerPos) <+> "already set it and may have allocated with it")
        Nothing [] []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("allocator-swap", Text.unlines
    [ "Checks that the allocator of an object is not replaced by code that got"
    , "the object from the function that set it."
    , ""
    , "**Reason:** that function may have allocated with the old allocator, and"
    , "what it allocated is then freed with the new one."
    ]))
