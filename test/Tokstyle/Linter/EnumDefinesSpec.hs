{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.EnumDefinesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["enum-defines"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["enum-defines"]


spec :: Spec
spec = do
    it "suggests using enums for long sequences of #defines" $ do
        shouldWarn'
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 2"
            , "#define FOO_BAR_THREE 3"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
            [[ "warning: sequence of `#define`s longer than 5 could be written as `enum Foo_Bar` [-Wenum-defines]"
             , "   --> test.c:5:9"
             , "    |"
             , "   5| #define FOO_BAR_FIVE 5"
             , "    |         ^^^^^^^^^^^^^^"
             ]]

    it "allows comments to be interspersed in the enum" $ do
        shouldWarn'
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 2"
            , "// some comment here"
            , "#define FOO_BAR_THREE 3"
            , "/* another comment here */"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
            [[ "warning: sequence of `#define`s longer than 5 could be written as `enum Foo_Bar` [-Wenum-defines]"
             , "   --> test.c:7:9"
             , "    |"
             , "   7| #define FOO_BAR_FIVE 5"
             , "    |         ^^^^^^^^^^^^^^"
             ]]

    it "ignores broken sequences" $ do
        shouldAccept'
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 2"
            , "static const uint32_t xxx = 10;"  -- breaks the sequence, we ignore this because it doesn't look like an enum
            , "#define FOO_BAR_THREE 3"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]

    it "ignores defines with large values" $ do
        shouldAccept'
            [ "#define FOO_BAR_ONE 1"
            , "#define FOO_BAR_TWO 0x20"
            , "#define FOO_BAR_THREE 300"
            , "#define FOO_BAR_FOUR 4"
            , "#define FOO_BAR_FIVE 5"
            ]
