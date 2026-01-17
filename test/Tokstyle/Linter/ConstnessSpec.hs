{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ConstnessSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["constness"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["constness"]


spec :: Spec
spec = do
    it "should not give diagnostics on parameters or pointer or array types" $ do
        shouldAccept'
            [ "int a(int b) {"
            , "  int *a = get();"
            , "  int b[3];"
            , "  return *a + b[0];"
            , "}"
            ]

    it "should give diagnostics on locals that can be const" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  int a = get();"
            , "  return a;"
            , "}"
            ]
            [[ "warning: variable `a` is never written to and can be declared `const` [-Wconstness]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int a = get();"
             , "    |       ^"
             ]]
