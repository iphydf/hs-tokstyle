{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CompoundInitSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["compound-init"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["compound-init"]


spec :: Spec
spec = do
    it "detects compound literal initialisers" $ do
        shouldWarn'
            [ "void f(void) {"
            , "  Foo foo = (Foo){0};"
            , "}"
            ]
            [[ "warning: don't use compound literals in initialisations; use simple `Type var = {0};` [-Wcompound-init]"
             , "   --> test.c:2:3"
             , "    |"
             , "   2|   Foo foo = (Foo){0};"
             , "    |   ^^^^^^^^^^^^^^^^^"
             ]]

    it "accepts aggregate initialisers" $ do
        shouldAccept'
            [ "void f(void) {"
            , "  Foo foo = {0};"
            , "}"
            ]
