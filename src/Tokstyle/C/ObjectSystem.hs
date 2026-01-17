{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE ViewPatterns      #-}
module Tokstyle.C.ObjectSystem
    ( ObjectInfo(..)
    , CallbackSlot(..)
    , discoverObjectTypes
    , isCallbackMember
    , isUserdataMember
    , isFuncPtr
    ) where

import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Language.C.Analysis.SemRep
import           Language.C.Analysis.TypeUtils (canonicalType)
import           Language.C.Data.Ident         (Ident (..), SUERef (..))
import           Tokstyle.C.Patterns


-- | Metadata for a struct that contains callbacks.
data ObjectInfo = ObjectInfo
    { objCallbacks :: Set String
    , objUserdata  :: Set String
    } deriving (Show, Eq)


-- | Represents a specific location where a callback can be stored.
data CallbackSlot = CallbackSlot
    { slotStruct :: String
    , slotMember :: String
    } deriving (Eq, Ord, Show)


-- | Check if a type is a function pointer or an array of them.
isFuncPtr :: Type -> Bool
isFuncPtr (canonicalType -> PtrType (FunctionType _ _) _ _) = True
isFuncPtr (canonicalType -> ArrayType t _ _ _)              = isFuncPtr t
isFuncPtr (canonicalType -> FunctionType _ _)               = True
isFuncPtr _                                                 = False


-- | Check if a type looks like a userdata pointer.
isUserdataPtr :: Type -> Bool
isUserdataPtr (canonicalType -> PtrType (DirectType TyVoid _ _) _ _) = True
isUserdataPtr _                                                      = False


-- | Extract the name of a struct member.
memberName :: MemberDecl -> Maybe String
memberName (MemberDecl (VarDecl (VarName (Ident n _ _) _) _ _) _ _) = Just n
memberName _                                                        = Nothing


-- | Get the type of a struct member.
memberType :: MemberDecl -> Maybe Type
memberType (MemberDecl (VarDecl _ _ ty) _ _) = Just ty
memberType _                                 = Nothing


-- | Identify all structs that contain function pointers.
discoverObjectTypes :: GlobalDecls -> Map String ObjectInfo
discoverObjectTypes (GlobalDecls _ tags _) =
    Map.fromList . mapMaybe getObjectInfo . Map.toList $ tags
  where
    mapMaybe f = foldr (\x acc -> case f x of Just y -> y:acc; Nothing -> acc) []

    getObjectInfo (NamedRef (Ident name _ _), CompDef (CompType _ StructTag members _ _)) =
        let
            callbacks = Set.fromList [ n | m <- members, Just n <- [memberName m], Just t <- [memberType m], isFuncPtr t ]
            userdatas = Set.fromList [ n | m <- members, Just n <- [memberName m], Just t <- [memberType m], isUserdataPtr t ]
        in
            if Set.null callbacks
            then Nothing
            else Just (name, ObjectInfo callbacks userdatas)
    getObjectInfo _ = Nothing


isCallbackMember :: Map String ObjectInfo -> String -> String -> Bool
isCallbackMember objs structName fieldName =
    case Map.lookup structName objs of
        Just info -> fieldName `Set.member` objCallbacks info
        Nothing   -> False


isUserdataMember :: Map String ObjectInfo -> String -> String -> Bool
isUserdataMember objs structName fieldName =
    case Map.lookup structName objs of
        Just info -> fieldName `Set.member` objUserdata info
        Nothing   -> False
