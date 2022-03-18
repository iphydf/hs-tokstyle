{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.VarUnusedInScopeSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyse)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "detects vars declared outside an if-statement that could be declared inside it" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo = 0;"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]
        analyse ["var-unused-in-scope"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects decls that can be for-init-decls" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:3:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "should support #if/#endif" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "supports #if/#else/#endif" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects when the first #if branch is ok while the second isn't" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  for (int i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:5: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:6:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects when the second #if branch is ok while the first isn't" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  for (int i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects multiple uses, as long as all of them are writes" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:3:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "should work on variables declared multiple scopes up" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "    print_int(i);"
            , "  }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "should work on variables only-written in both if branches" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "allows vars read in one of the if-branches" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    print_int(i);"
            , "  }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []

    it "allows vars read in the same scope" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
            , "  print_int(i);"
            , "}"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []

    it "allows vars used as the bound for another for-loop" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
            , "  for (int j = 0; j < i; ++j) { puts(\"hello!\"); }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []

    it "treats array index assignments as reads" $ do
        ast <- mustParse
            [ "int a(char *p) {"
            , "  char *c = p;"
            , "  if (true) { c[0] = 'a'; }"
            , "}"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []

    it "should consider one `if` branch with a write as possibly not writing" $ do
        ast <- mustParse
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
        analyse allWarnings ("test.c", ast) `shouldBe` []
