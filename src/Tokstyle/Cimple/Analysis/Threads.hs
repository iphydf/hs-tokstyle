{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | What may run on any thread, as the program's `@thread any` tags say.
module Tokstyle.Cimple.Analysis.Threads
    ( AnyThread (..)
    , anyThread
    , paramTypes
    , Call (..)
    , callsIn
    , roots
    ) where

import           Data.Fix                           (Fix (..), foldFix)
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import           Language.Cimple                    (Lexeme (..), NodeF (..))
import qualified Language.Cimple                    as C
import           Tokstyle.Cimple.Analysis.Functions (Function (..), calleeLexeme,
                                                     universe)
import           Tokstyle.Cimple.Analysis.Types


-- | What the program says may run on any thread: functions, and callback
-- types whose callbacks may be called on any thread.
data AnyThread = AnyThread
    { anyFuncs :: Set Text
    , anyTypes :: Set Text
    }

anyThread :: [(FilePath, [C.Node (Lexeme Text)])] -> AnyThread
anyThread tus = AnyThread (Set.fromList [ n | Left n <- marked ]) (Set.fromList [ n | Right n <- marked ])
  where
    marked =
        [ m
        | (_, nodes) <- tus, node <- nodes
        , Fix (Commented (Fix (CommentInfo doc)) target) <- universe node
        , saysAny doc
        , Just m <- [declared target] ]
    saysAny = foldFix $ \case
        C.DocThread (L _ _ "any") -> True
        c                         -> or c
    declared (Fix node) = case node of
        FunctionDecl _ proto                    -> Left <$> protoName proto
        FunctionDefn _ proto _                  -> Left <$> protoName proto
        TypedefFunction proto                   -> Right <$> protoName proto
        _                                       -> Nothing
    protoName = \case
        Fix (FunctionPrototype _ (L _ _ name) _) -> Just name
        _                                        -> Nothing

-- | The callback type of each parameter of each function.
paramTypes :: [(FilePath, [C.Node (Lexeme Text)])] -> Map Text [Maybe Text]
paramTypes tus = Map.fromList
    [ (name, map paramType params)
    | (_, nodes) <- tus, node <- nodes
    , Fix (FunctionPrototype _ (L _ _ name) params) <- universe node ]
  where
    paramType = \case
        Fix (VarDecl ty _ _) -> callbackType ty
        _                    -> Nothing

-- | A call in a function: to a function it names, or through a pointer of
-- some callback type.
data Call
    = Direct Text
    | Indirect (C.Node (Lexeme Text)) (Maybe Text)

callsIn :: Structs -> Function -> [Call]
callsIn types fn =
    [ call
    | Fix (FunctionCall callee _) <- universe (fnDefn fn)
    , let call = case bare callee of
            Fix (VarExpr (L _ _ n)) | Nothing <- typeIn types fn callee -> Direct n
            -- A macro.
            Fix (LiteralExpr _ (L _ _ n)) -> Direct n
            _ -> Indirect callee (typeIn types fn callee >>= callbackType)
    ]

-- | The functions that may run on any thread: those marked so, and those
-- passed or stored where a callback type marked so is expected, with why.
roots :: AnyThread -> Structs -> Map Text [Maybe Text] -> Map Text Function -> [(Text, Text)]
roots marks types params defined =
    [ (f, f) | f <- Set.toList (anyFuncs marks), f `Map.member` defined ]
    ++ [ (n, t)
       | fn <- Map.elems defined
       , Fix node <- universe (fnDefn fn)
       , (n, t) <- case node of
            FunctionCall callee args | L _ _ g <- calleeLexeme callee ->
                [ (n, t) | (arg, Just t) <- zip args (Map.findWithDefault [] g params)
                         , t `Set.member` anyTypes marks, Just n <- [function arg] ]
            AssignExpr l _ r | Just f <- field l
                             , t : _ <- [ t | fs <- Map.elems types, Just ty <- [Map.lookup f fs], Just t <- [callbackType ty] ]
                             , t `Set.member` anyTypes marks, Just n <- function r -> [(n, t)]
            _ -> []
       ]
  where
    function e = case bare e of
        Fix (VarExpr (L _ _ n)) | n `Map.member` defined -> Just n
        _                                               -> Nothing
    field e = case bare e of
        Fix (PointerAccess _ (L _ _ f)) -> Just f
        Fix (MemberAccess _ (L _ _ f))  -> Just f
        _                               -> Nothing

