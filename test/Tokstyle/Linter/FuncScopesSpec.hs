{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.FuncScopesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["func-scopes"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["func-scopes"]


spec :: Spec
spec = do
    it "should not give diagnostics on matching scopes" $ do
        shouldAccept'
            [ "int foo(void);"
            , "int foo(void) { return 0; }"
            , "static int bar(void);"
            , "static int bar(void) { return 0; }"
            ]

    it "should give diagnostics on scope mismatch (static decl, extern def)" $ do
        shouldWarn'
            [ "static int foo(void);"
            , "int foo(void) { return 0; }"
            ]
            [[ "warning: function definition `foo` does not agree with its declaration about scope: declaration on line 1 is `static` but definition is `extern` [-Wfunc-scopes]"
             , "   --> test.c:2:5"
             , "    |"
             , "   2| int foo(void) { return 0; }"
             , "    |     ^^^"
             ]]

    it "should give diagnostics on scope mismatch (extern decl, static def)" $ do
        shouldWarn'
            [ "int foo(void);"
            , "static int foo(void) { return 0; }"
            ]
            [[ "warning: function definition `foo` does not agree with its declaration about scope: declaration on line 1 is `extern` but definition is `static` [-Wfunc-scopes]"
             , "   --> test.c:2:12"
             , "    |"
             , "   2| static int foo(void) { return 0; }"
             , "    |            ^^^"
             ]]

    it "should not give diagnostics if there is no prior declaration" $ do
        shouldAccept' [ "static int foo(void) { return 0; }" ]
