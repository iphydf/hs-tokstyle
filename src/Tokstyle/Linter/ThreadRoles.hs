{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.ThreadRoles (descr) where

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
                                                     warnRich)
import           Language.Cimple.Pretty             (showNodePlain)
import           Prettyprinter                      (pretty, (<+>))
import           Tokstyle.Cimple.Analysis.Functions
import           Tokstyle.Cimple.Analysis.Threads
import           Tokstyle.Cimple.Analysis.Types
import           Tokstyle.Common                    (backticks)


-- | Callbacks of a type not marked `@thread any` passed or stored where one
-- marked so is expected: the function doing it, the callback, its type, and
-- the type expected.
handOvers :: AnyThread -> Structs -> Map Text [Maybe Text] -> [Function] -> [(Function, C.Node (Lexeme Text), Text, Text)]
handOvers marks types params fns =
    [ (fn, arg, u, t)
    | fn <- fns
    , Fix (FunctionCall callee args) <- universe (fnDefn fn)
    , L _ _ g <- [calleeLexeme callee]
    , (arg, Just t) <- zip args (Map.findWithDefault [] g params)
    , t `Set.member` anyTypes marks
    , Just u <- [typeIn types fn arg >>= callbackType]
    , u `Set.notMember` anyTypes marks ]

-- | The calls through pointers whose callbacks may not run on any thread,
-- reached from a function that may: the function making the call, the call,
-- its callback type, and the root it was reached from with why that may run
-- on any thread.
findings :: [(FilePath, [C.Node (Lexeme Text)])] -> [(Function, C.Node (Lexeme Text), Maybe Text, (Text, Text))]
findings tus = List.nubBy (\(_, a, _, _) (_, b, _, _) -> sameSite a b) $ concatMap fromRoot (List.sort (roots marks types params defined))
  where
    marks = anyThread tus
    types = structs tus
    params = paramTypes tus
    defined = Map.fromList [ (fnName f, f) | f <- definitions tus ]
    fromRoot root@(r, _) = go Set.empty [r]
      where
        go _ [] = []
        go seen (f : rest)
            | f `Set.member` seen = go seen rest
            | otherwise = case Map.lookup f defined of
                Nothing -> go (Set.insert f seen) rest
                Just fn ->
                    let calls = callsIn types fn
                        bad = [ (fn, c, t, root) | Indirect c t <- calls, maybe True (`Set.notMember` anyTypes marks) t ]
                        next = [ g | Direct g <- calls ]
                    in bad ++ go (Set.insert f seen) (rest ++ next)
    sameSite a b = position a == position b
    position = \case
        Fix (VarExpr l)         -> Just l
        Fix (PointerAccess _ l) -> Just l
        Fix (MemberAccess _ l)  -> Just l
        Fix (ParenExpr e)       -> position e
        Fix (CastExpr _ e)      -> position e
        _                       -> Nothing

analyse :: [(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos]
analyse tus = reverse $ State.execState (mapM_ report (findings tus) >> mapM_ reportHandOver handed) []
  where
    handed = handOvers (anyThread tus) (structs tus) (paramTypes tus) (definitions tus)

reportHandOver :: (Function, C.Node (Lexeme Text), Text, Text) -> State [Diagnostic CimplePos] ()
reportHandOver (fn, arg, u, t) = do
    let (pos, len) = getDiagnosticInfo (fnFile fn) arg
    warnRich $ Diagnostic pos len ErrorLevel
        (backticks (pretty (showNodePlain arg)) <+> "is a" <+> backticks (pretty u)
            <+> "passed here as a" <+> backticks (pretty t) <> ", which is `@thread any`, but"
            <+> backticks (pretty u) <+> "is not")
        Nothing [] []

report :: (Function, C.Node (Lexeme Text), Maybe Text, (Text, Text)) -> State [Diagnostic CimplePos] ()
report (fn, callee, ty, (root, why)) = do
    let (pos, len) = getDiagnosticInfo (fnFile fn) callee
        reason = if root == why
            then backticks (pretty root) <+> "is `@thread any`"
            else backticks (pretty root) <+> "is a" <+> backticks (pretty why) <> ", which is `@thread any`"
        kind = case ty of
            Just t  -> "its type" <+> backticks (pretty t) <+> "is not `@thread any`"
            Nothing -> "its type is not known to be `@thread any`"
    warnRich $ Diagnostic pos len ErrorLevel
        (backticks (pretty (showNodePlain callee)) <+> "is called here on a path from" <+> backticks (pretty root)
            <+> "that may run on any thread, as" <+> reason <> ", but" <+> kind)
        Nothing [] []

descr :: ([(FilePath, [C.Node (Lexeme Text)])] -> [Diagnostic CimplePos], (Text, Text))
descr = (analyse, ("thread-roles", Text.unlines
    [ "Checks that code that may run on any thread calls back only through"
    , "callback types marked `@thread any`."
    , ""
    , "A function may run on any thread when its doc comment says `@thread any`,"
    , "or when it is passed or stored as a callback type that says so. What it"
    , "calls runs on that thread too."
    , ""
    , "**Reason:** a callback the client wrote for the thread it calls the library"
    , "on may not be safe to call on another, as when it uses state of that thread"
    , "or a runtime bound to it."
    ]))
