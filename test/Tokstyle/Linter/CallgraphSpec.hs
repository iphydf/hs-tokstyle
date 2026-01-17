{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallgraphSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["callgraph"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["callgraph"]


spec :: Spec
spec = do
    it "should give diagnostic on unused macros" $ do
        shouldWarn' [ "#define SIZE 10" ]
            [[ "warning: unused symbol `SIZE` [-Wcallgraph]"
             , "   --> test.c:1:9"
             , "    |"
             , "   1| #define SIZE 10"
             , "    |         ^^^^"
             ]]

    it "should not give diagnostics on symbols used in array dimensions" $ do
        shouldAccept'
            [ "#define SIZE 10"
            , "struct Foo {"
            , "  char c[SIZE];"
            , "};"
            , "int main() { struct Foo foo; }"
            ]

    it "should give diagnostics on undefined symbols used in array dimensions" $ do
        shouldWarn'
            [ "typedef struct Foo {"
            , "  char c[SIZE];"
            , "} Foo;"
            , "int main() { Foo foo; }"
            ]
            [[ "warning: definition of `Foo` references undefined global function/constant `SIZE` [-Wcallgraph]"
             , "   --> test.c:2:10"
             , "    |"
             , "   2|   char c[SIZE];"
             , "    |          ^^^^"
             ]]

    it "should warn if a non-testonly function calls a testonly function" $ do
        shouldWarn'
            [ "void testonly_func() { /* empty */ }"
            , "void normal_func() { testonly_func(); }"
            , "int main() { normal_func(); }"
            ]
            [[ "warning: non-testonly function `normal_func` calls testonly function `testonly_func` [-Wcallgraph]"
             , "   --> test.c:2:6"
             , "    |"
             , "   2| void normal_func() { testonly_func(); }"
             , "    |      ^^^^^^^^^^^"
             ]]

    it "should not warn if a testonly function calls a testonly function" $ do
        shouldWarn'
            [ "void testonly_func1() { /* empty */ }"
            , "void testonly_func2() { testonly_func1(); }"
            , "int main() { testonly_func2(); }"
            ]
            [[ "warning: non-testonly function `main` calls testonly function `testonly_func2` [-Wcallgraph]"
             , "   --> test.c:3:5"
             , "    |"
             , "   3| int main() { testonly_func2(); }"
             , "    |     ^^^^"
             ]]

    it "should not warn if a testonly function is unused" $ do
        shouldAccept'
            [ "void testonly_func() { /* empty */ }"
            , "int main() { return 0; }"
            ]
