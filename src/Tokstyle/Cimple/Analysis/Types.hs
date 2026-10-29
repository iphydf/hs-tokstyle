{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
-- | The declared types of what expressions name.
module Tokstyle.Cimple.Analysis.Types
    ( Structs
    , structs
    , structName
    , callbackType
    , typeIn
    , bare
    ) where

import           Data.Fix                           (Fix (..))
import           Data.Map.Strict                    (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Maybe                         (listToMaybe)
import           Data.Text                          (Text)
import           Language.Cimple                    (Lexeme (..), NodeF (..))
import qualified Language.Cimple                    as C
import           Tokstyle.Cimple.Analysis.Functions (Function (..), universe)


-- | The callback type a declared type holds a pointer to, if any.
callbackType :: C.Node (Lexeme Text) -> Maybe Text
callbackType ty = listToMaybe [ name | Fix (TyFunc (L _ _ name)) <- universe ty ]

-- | The declared type of each field of each struct.
type Structs = Map Text (Map Text (C.Node (Lexeme Text)))

structs :: [(FilePath, [C.Node (Lexeme Text)])] -> Structs
structs tus = Map.fromList
    [ (name, Map.fromList [ (f, ty) | Fix (MemberDecl (Fix (VarDecl ty (L _ _ f) _)) _) <- members ])
    | (_, nodes) <- tus, node <- nodes, Fix n <- universe node
    , (name, members) <- case n of
        Struct (L _ _ s) ms                   -> [(s, ms)]
        Typedef (Fix (Struct _ ms)) (L _ _ s) _ -> [(s, ms)]
        _                                     -> []
    ]

-- | The struct a declared type is or points to.
structName :: C.Node (Lexeme Text) -> Maybe Text
structName ty = listToMaybe
    [ n | Fix t <- universe ty, Just n <- [case t of
            TyStruct (L _ _ n')      -> Just n'
            TyUserDefined (L _ _ n') -> Just n'
            _                        -> Nothing] ]

bare :: C.Node (Lexeme Text) -> C.Node (Lexeme Text)
bare = \case
    Fix (ParenExpr e)  -> bare e
    Fix (CastExpr _ e) -> bare e
    e                  -> e

-- | The declared type of an expression in a function, where it names a
-- variable or a field.
typeIn :: Structs -> Function -> C.Node (Lexeme Text) -> Maybe (C.Node (Lexeme Text))
typeIn types fn = typeOf
  where
    locals = Map.fromList [ (v, ty) | Fix (VarDecl ty (L _ _ v) _) <- universe (fnDefn fn) ]
    typeOf e = case bare e of
        Fix (VarExpr (L _ _ v))         -> Map.lookup v locals
        Fix (PointerAccess x (L _ _ f)) -> field x f
        Fix (MemberAccess x (L _ _ f))  -> field x f
        _                               -> Nothing
    field x f = typeOf x >>= structName >>= (`Map.lookup` types) >>= Map.lookup f

