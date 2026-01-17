{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.AssertSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["assert"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["assert"]


spec :: Spec
spec = do
    it "should not give diagnostics on pure expressions in assert" $ do
        shouldAccept' [ "void a(int b) { assert(b == 1); }" ]

    it "should not give diagnostics on exempt functions in assert" $ do
        shouldAccept' [ "void a(void *x, void *y, int z) { assert(memcmp(x, y, z) == 0); }" ]

    it "should give diagnostics on non-pure functions in assert" $ do
        shouldWarn'
            [ "void a(int b) { assert(some_function(b) == 1); }" ]
            [[ "warning: non-pure function `some_function` cannot be called inside `assert()` [-Wassert]"
             , "   --> test.c:1:17"
             , "    |"
             , "   1| void a(int b) { assert(some_function(b) == 1); }"
             , "    |                 ^^^^^^"
             ]]

    it "should not give diagnostics on complex pure expressions in assert" $ do
        shouldAccept'
            [ "typedef struct Foo { int c[1]; } Foo;"
            , "void a(int b, Foo *b_ptr) { assert((b > 0) && (b < 10) || (b_ptr->c[0] == 42)); }"
            ]

    it "should not give diagnostics on functions with no arguments" $ do
        shouldAccept' [ "void a(int b) { assert(is_ready()); }" ]
