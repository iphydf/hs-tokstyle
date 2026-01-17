{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.EnumNamesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["enum-names"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["enum-names"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid enum names" $ do
        shouldAccept'
            [ "enum My_Enum {"
            , "  MY_ENUM_FOO,"
            , "  MY_ENUM_BAR"
            , "};"
            ]

    it "should give diagnostics on invalid enum names" $ do
        shouldWarn'
            [ "enum My_Enum {"
            , "  FOO,"
            , "  MY_ENUM_BAR"
            , "};"
            ]
            [[ "warning: enumerator `FOO` in enum `My_Enum` should start with `MY_ENUM_` [-Wenum-names]"
             , "   --> test.c:2:3"
             , "    |"
             , "   2|   FOO,"
             , "    |   ^^^"
             ]]

    it "should handle _T suffix correctly" $ do
        shouldAccept'
            [ "typedef enum My_Enum_T {"
            , "  MY_ENUM_FOO,"
            , "  MY_ENUM_BAR"
            , "} My_Enum_T;"
            ]

    it "should not check exempted enums" $ do
        shouldAccept'
            [ "enum Friend_Status {"
            , "  FRIEND_ONLINE,"
            , "  FRIEND_OFFLINE"
            , "};"
            ]

    it "should give diagnostics on invalid enum names in typedef" $ do
        shouldWarn'
            [ "typedef enum My_Enum {"
            , "  FOO,"
            , "  MY_ENUM_BAR"
            , "} My_Enum;"
            ]
            [[ "warning: enumerator `FOO` in enum `My_Enum` should start with `MY_ENUM_` [-Wenum-names]"
             , "   --> test.c:2:3"
             , "    |"
             , "   2|   FOO,"
             , "    |   ^^^"
             ]]
