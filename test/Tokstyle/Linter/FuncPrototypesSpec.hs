{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.FuncPrototypesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["func-prototypes"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["func-prototypes"]


spec :: Spec
spec = do
    it "should not give diagnostics on (void) parameter list" $ do
        shouldAccept' [ "void foo(void);" ]

    it "should give diagnostics on () parameter list in declaration" $ do
        shouldWarn' [ "void foo();" ]
            [[ "warning: empty parameter list must be written as `(void)` [-Wfunc-prototypes]"
             , "   --> test.c:1:6"
             , "    |"
             , "   1| void foo();"
             , "    |      ^^^"
             ]]

    it "should not give diagnostics on () parameter list in definition" $ do
        shouldAccept' [ "int foo() { return 0; }" ]

    it "should not give diagnostics on functions with parameters" $ do
        shouldAccept' [ "void foo(int a, int b);" ]
