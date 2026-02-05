{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.AccessPath
    ( AccessPath (..)
    , isPathPrefixOf
    , pathDepth
    ) where

import           Prettyprinter (Pretty (..))

-- | Access path to a variable or part of it.
data AccessPath
    = PathVar String
    | PathParam Int
    | PathReturn
    | PathDeref AccessPath
    | PathField AccessPath String
    | PathIndex AccessPath String
    deriving (Show, Eq, Ord)

instance Pretty AccessPath where
    pretty = \case
        PathVar s     -> pretty s
        PathParam i   -> "param" <> pretty i
        PathReturn    -> "<return value>"
        PathDeref p   -> "*" <> pretty p
        PathField p s -> pretty p <> "->" <> pretty s
        PathIndex p i -> pretty p <> "[" <> pretty i <> "]"

-- | Check if the first path is a prefix of (or equal to) the second.
-- e.g. "p" is a prefix of "p->f".
isPathPrefixOf :: AccessPath -> AccessPath -> Bool
isPathPrefixOf p1 p2               | p1 == p2 = True
isPathPrefixOf p1 (PathDeref p2)   = p1 `isPathPrefixOf` p2
isPathPrefixOf p1 (PathField p2 _) = p1 `isPathPrefixOf` p2
isPathPrefixOf p1 (PathIndex p2 _) = p1 `isPathPrefixOf` p2
isPathPrefixOf PathReturn _        = False
isPathPrefixOf _ _                 = False

pathDepth :: AccessPath -> Int
pathDepth = \case
    PathVar _     -> 1
    PathParam _   -> 1
    PathReturn    -> 1
    PathDeref p   -> 1 + pathDepth p
    PathField p _ -> 1 + pathDepth p
    PathIndex p _ -> 1 + pathDepth p
