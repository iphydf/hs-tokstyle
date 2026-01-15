{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CastSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings)
import           Tokstyle.C.LinterSpec (check)


spec :: Spec
spec = describe "Cast linter" $ do
    describe "disallowed casts" $ do
        it "warns when casting between char* and int*" $ do
            let code =
                    [ "int *func(char *p) {"
                    , "  return (int *)p;"
                    , "}"
                    ]
            check allWarnings code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: disallowed cast from char * to int * [-Wcast]"
                    , "   --> test.c:2:17"
                    , "    |"
                    , "2   |   return (int *)p;"
                    , "    |                 ^"
                    , "    |"
                    ]
                ]

        it "warns when casting between int* and char*" $ do
            let code =
                    [ "char *func(int *p) {"
                    , "  return (char *)p;"
                    , "}"
                    ]
            check allWarnings code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: disallowed cast from int * to char * [-Wcast]"
                    , "   --> test.c:2:18"
                    , "    |"
                    , "2   |   return (char *)p;"
                    , "    |                  ^"
                    , "    |"
                    ]
                ]

        it "warns when casting between unrelated struct pointers" $ do
            let code =
                    [ "struct A { int a; };"
                    , "struct B { int b; };"
                    , "struct A *func(struct B *p) {"
                    , "  return (struct A *)p;"
                    , "}"
                    ]
            check allWarnings code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: disallowed cast from struct B * to struct A * [-Wcast]"
                    , "   --> test.c:4:22"
                    , "    |"
                    , "4   |   return (struct A *)p;"
                    , "    |                      ^"
                    , "    |"
                    ]
                ]

        it "warns on cast between incompatible enums (different size)" $ do
            let code =
                    [ "enum A { A_1, A_2 };"
                    , "enum B { B_1, B_2, B_3 };"
                    , "enum A func(enum B b) {"
                    , "  return (enum A)b;"
                    , "}"
                    ]
            check allWarnings code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: enum types `enum A` and `enum B` have different a number of enumerators [-Wcast]"
                    , "   --> test.c:4:18"
                    , "    |"
                    , "4   |   return (enum A)b;"
                    , "    |                  ^"
                    , "    |"
                    ]
                ]

        it "warns on cast between enums with different enumerator values" $ do
            let code =
                    [ "enum A { A_1 = 1, A_2 = 2 };"
                    , "enum B { B_1 = 1, B_2 = 3 };"
                    , "enum A func(enum B b) {"
                    , "  return (enum A)b;"
                    , "}"
                    ]
            check allWarnings code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: invalid cast: enumerator value for `A_2 = 2` does not match `B_2 = 3` [-Wcast]"
                    , "   --> test.c:1:24"
                    , "    |"
                    , "1   | enum A { A_1 = 1, A_2 = 2 };"
                    , "    |                        ^"
                    , "    |"
                    ]
                ]

    describe "allowed casts" $ do
        it "allows casting to void" $ do
            let code =
                    [ "void func(int i) {"
                    , "  (void)i;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from void* to other pointers" $ do
            let code =
                    [ "int *func(void *vp) {"
                    , "  int *p = (int *)vp;"
                    , "  return p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from other pointers to void*" $ do
            let code =
                    [ "void *func(int *p) {"
                    , "  return (void *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from char* to uint8_t*" $ do
            let code =
                    [ "typedef unsigned char uint8_t;"
                    , "uint8_t *func(char *p) {"
                    , "  return (uint8_t *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from uint8_t* to char*" $ do
            let code =
                    [ "typedef unsigned char uint8_t;"
                    , "char *func(uint8_t *p) {"
                    , "  return (char *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from 0 to a pointer" $ do
            let code =
                    [ "int *func() {"
                    , "  return (int *)0;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from sockaddr_storage* to sockaddr*" $ do
            let code =
                    [ "struct sockaddr {};"
                    , "struct sockaddr_storage {};"
                    , "struct sockaddr *func(struct sockaddr_storage *p) {"
                    , "  return (struct sockaddr *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting between numeric types" $ do
            let code =
                    [ "float func(int i) {"
                    , "  return (float)i;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from enum to int" $ do
            let code =
                    [ "enum A { A_1 };"
                    , "int func(enum A a) {"
                    , "  return (int)a;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from int to enum" $ do
            let code =
                    [ "enum A { A_1 };"
                    , "enum A func(int i) {"
                    , "  return (enum A)i;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting between compatible enums" $ do
            let code =
                    [ "enum A { A_1 = 1, A_2 = 2 };"
                    , "enum B { B_1 = 1, B_2 = 2 };"
                    , "enum A func(enum B b) {"
                    , "  return (enum A)b;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from a pointer type to itself" $ do
            let code =
                    [ "struct Foo {};"
                    , "struct Foo *func(struct Foo *p) {"
                    , "  return (struct Foo *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from a pointer type to its const pointer version" $ do
            let code =
                    [ "struct Foo {};"
                    , "const struct Foo *func(struct Foo *p) {"
                    , "  return (const struct Foo *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from a typedef to its underlying type" $ do
            let code =
                    [ "typedef struct Foo {} Foo;"
                    , "Foo *func(struct Foo *p) {"
                    , "  return (Foo *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from a struct pointer to a compatible typedef pointer" $ do
            let code =
                    [ "typedef struct Foo {} Foo;"
                    , "struct Foo *func(Foo *p) {"
                    , "  return (struct Foo *)p;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows casting from a T array to a pointer to const T" $ do
            let code =
                    [ "char array[10];"
                    , "const char *func() {"
                    , "  return (const char *)array;"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []
