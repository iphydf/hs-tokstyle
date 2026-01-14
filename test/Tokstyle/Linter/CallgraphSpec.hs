{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallgraphSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should give diagnostic on unused macros" $ do
        ast <- mustParse
            [ "#define SIZE 10"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:1: unused symbol `SIZE` [-Wcallgraph]"
            ]

    it "should not give diagnostics on symbols used in array dimensions" $ do
        ast <- mustParse
            [ "#define SIZE 10"
            , "struct Foo {"
            , "  char c[SIZE];"
            , "};"
            , "int main() { Foo foo; }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            []

    it "should give diagnostics on undefined symbols used in array dimensions" $ do
        ast <- mustParse
            [ "typedef struct Foo {"
            , "  char c[SIZE];"
            , "} Foo;"
            , "int main() { Foo foo; }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:2: definition of `Foo` references undefined global function/constant `SIZE` [-Wcallgraph]"
            ]

    it "should warn if a non-testonly function calls a testonly function" $ do
        ast <- mustParse
            [ "void testonly_func() { /* empty */ }"
            , "void normal_func() { testonly_func(); }"
            , "int main() { normal_func(); }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:2: non-testonly function `normal_func` calls testonly function `testonly_func` [-Wcallgraph]"
            ]

    it "should not warn if a testonly function calls a testonly function" $ do
        ast <- mustParse
            [ "void testonly_func1() { /* empty */ }"
            , "void testonly_func2() { testonly_func1(); }"
            , "int main() { testonly_func2(); }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:3: non-testonly function `main` calls testonly function `testonly_func2` [-Wcallgraph]"
            ]

    it "should not warn if a testonly function is unused" $ do
        ast <- mustParse
            [ "void testonly_func() { /* empty */ }"
            , "int main() { return 0; }"
            ]
        analyseGlobal ["callgraph"] [("test.c", ast)]
            `shouldBe`
            []
