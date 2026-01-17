{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.BooleanReturnSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["boolean-return"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["boolean-return"]


spec :: Spec
spec = do
    it "should give diagnostics on functions with only 2 const returns" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  return 1;"
            , "  return 0;"
            , "}"
            ]
            [[ "warning: function `a` only ever returns two values `0` and `1`; it can return `bool` [-Wboolean-return]"
             , "   --> test.c:1:5"
             , "    |"
             , "   1| int a(int b) {"
             , "    |     ^"
             ]]

    it "should show negative return values in the diagnostic" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  return -1;"
            , "  return 0;"
            , "}"
            ]
            [[ "warning: function `a` only ever returns two values `-1` and `0`; it can return `bool` [-Wboolean-return]"
             , "   --> test.c:1:5"
             , "    |"
             , "   1| int a(int b) {"
             , "    |     ^"
             ]]

    it "should not give diagnostics on functions with non-const returns" $ do
        shouldAccept'
            [ "int a(int b) {"
            , "  return 1;"
            , "  return 0;"
            , "  return foo();"
            , "}"
            ]

    it "should not give diagnostics on functions with more than 2 const returns" $ do
        shouldAccept'
            [ "int a(int b) {"
            , "  return 1;"
            , "  return 0;"
            , "  return -1;"
            , "}"
            ]
