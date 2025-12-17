{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.OwnershipInferenceSpec where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import           Tokstyle.Linter       (analyseGlobal)
import           Tokstyle.LinterSpec   (mustParse)

spec :: Spec
spec = do
    describe "Ownership inference linter" $ do
        it "suggests a function that returns a malloced pointer" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void * alloc_wrapper() {"
                , "  void *p = malloc(10);"
                , "  return p;"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: candidate for linear types: variable `p` in function `alloc_wrapper` [-Wownership-inference]"
                ]

        it "suggests a function that frees a malloced pointer" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void *ptr);"
                , "void alloc_and_free() {"
                , "  void *p = malloc(10);"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: candidate for linear types: variable `p` in function `alloc_and_free` [-Wownership-inference]"
                ]

        it "does not suggest if the pointer is leaked" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void leak() {"
                , "  void *p = malloc(10);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "handles multiple exit paths correctly" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void *ptr);"
                , "void test(int cond) {"
                , "  void *p = malloc(10);"
                , "  if (cond) {"
                , "    free(p);"
                , "  } else {"
                , "    free(p);"
                , "  }"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: candidate for linear types: variable `p` in function `test` [-Wownership-inference]"
                ]

        it "does not suggest if ownership is inconsistent" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void *ptr);"
                , "void test(int cond) {"
                , "  void *p = malloc(10);"
                , "  if (cond) {"
                , "    free(p);"
                , "  }"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "does not suggest if the pointer is aliased" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void *ptr);"
                , "void test() {"
                , "  void *p = malloc(10);"
                , "  void *q = p;"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "does not suggest if the pointer is overwritten" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void *ptr);"
                , "void test() {"
                , "  void *p = malloc(10);"
                , "  p = malloc(20);"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "suggests a parameter that is consistently freed" $ do
            ast <- mustParse
                [ "void free(void *ptr);"
                , "void free_wrapper(void *p) {"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: candidate for linear types: parameter `p` in function `free_wrapper` [-Wownership-inference]"
                ]

        it "does not suggest 'owner' for non-pointer parameters" $ do
            ast <- mustParse
                [ "void use(int x);"
                , "void test(int x) {"
                , "  use(x);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "does not suggest 'owner' for returned integer parameters" $ do
            ast <- mustParse
                [ "int test(int x) {"
                , "  return x;"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "does not suggest if use-after-free would occur" $ do
            ast <- mustParse
                [ "void free(void *ptr);"
                , "void use(void *ptr);"
                , "void test(void *p) {"
                , "  free(p);"
                , "  use(p);"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

        it "interaction with struct with owned fields (heuristic limitation)" $ do
            ast <- mustParse
                [ "struct Test_S { int * owner a; int * owner b; };"
                , "void test(struct Test_S *s, int *x, int *y) {"
                , "  s->a = x;"
                , "  s->b = y;"
                , "}"
                ]
            analyseGlobal ["ownership-inference"] [("test.c", ast)]
                `shouldBe` []

-- end of tests
