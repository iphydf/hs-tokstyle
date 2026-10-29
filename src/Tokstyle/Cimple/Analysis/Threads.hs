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
import           Language.Cimple                    (Lexeme (..), NodeF (..),
                                                     UnaryOp (..))
import qualified Language.Cimple                    as C
import           Tokstyle.Cimple.Analysis.FunctionPointers (FieldTargets,
                                                            Targets (..),
                                                            callTargets)
import           Tokstyle.Cimple.Analysis.Functions (Function (..), calleeLexeme,
                                                     universe)
import           Tokstyle.Cimple.Analysis.Types


-- | The functions and callback types tagged `@thread any`.
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

-- | A call by name, or through a pointer of some callback type.
data Call
    = Direct Text
    | Indirect (C.Node (Lexeme Text)) (Maybe Text)

-- | A call through a field calls what the program stores there, and goes
-- through a pointer of its type where clients may store anything.
callsIn :: Structs -> FieldTargets -> Function -> [Call]
callsIn types targets fn =
    [ call
    | Fix (FunctionCall callee _) <- universe (fnDefn fn)
    , call <- case bare callee of
        Fix (VarExpr (L _ _ n)) | Nothing <- typeIn types fn callee -> [Direct n]
        -- A macro.
        Fix (LiteralExpr _ (L _ _ n)) -> [Direct n]
        _ -> case callTargets types targets fn callee of
            Just (Only ns)     -> map Direct (Set.toList ns)
            Just (Anything ns) -> map Direct (Set.toList ns) ++ [indirect callee]
            Nothing            -> [indirect callee]
    ]
  where
    indirect callee = Indirect callee (typeIn types fn callee >>= callbackType)

-- | The functions that may run on any thread, with why: tagged so, or passed
-- or stored where a tagged callback type is expected.
roots :: AnyThread -> Structs -> FieldTargets -> Map Text [Maybe Text] -> Map Text Function -> [(Text, Text)]
roots marks types targets params defined =
    [ (f, f) | f <- Set.toList (anyFuncs marks), f `Map.member` defined ]
    ++ [ (n, t)
       | fn <- Map.elems defined
       , Fix (FunctionCall callee args) <- universe (fnDefn fn)
       , L _ _ g <- [calleeLexeme callee]
       , (arg, Just t) <- zip args (Map.findWithDefault [] g params)
       , t `Set.member` anyTypes marks, Just n <- [function arg] ]
    ++ [ (n, t)
       | ((s, f), t') <- Map.toList targets
       , let ns = case t' of
                Only xs     -> xs
                Anything xs -> xs
       , Just t <- [Map.lookup s types >>= Map.lookup f >>= callbackType]
       , t `Set.member` anyTypes marks
       , n <- Set.toList ns, n `Map.member` defined ]
  where
    function e = case bare e of
        Fix (UnaryExpr UopAddress f) -> function f
        Fix (VarExpr (L _ _ n)) | n `Map.member` defined -> Just n
        _                                               -> Nothing

