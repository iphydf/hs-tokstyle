{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CastSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, describe, it)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings)
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn allWarnings


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept allWarnings


spec :: Spec
spec = describe "Cast linter" $ do
    describe "disallowed casts" $ do
        it "warns when casting between char* and int*" $ do
            shouldWarn'
                [ "int *func(char *p) {"
                , "  return (int *)p;"
                , "}"
                ]
                [[ "error: disallowed cast from `char *` to `int *` [-Wcast]"
                 , "   --> test.c:2:17"
                 , "    |"
                 , "   2|   return (int *)p;"
                 , "    |                 ^"
                 ]]

        it "warns when casting between int* and char*" $ do
            shouldWarn'
                [ "char *func(int *p) {"
                , "  return (char *)p;"
                , "}"
                ]
                [[ "error: disallowed cast from `int *` to `char *` [-Wcast]"
                 , "   --> test.c:2:18"
                 , "    |"
                 , "   2|   return (char *)p;"
                 , "    |                  ^"
                 ]]

        it "warns when casting between unrelated struct pointers" $ do
            shouldWarn'
                [ "struct A { int a; };"
                , "struct B { int b; };"
                , "struct A *func(struct B *p) {"
                , "  return (struct A *)p;"
                , "}"
                ]
                [[ "error: disallowed cast from `struct B *` to `struct A *` [-Wcast]"
                 , "   --> test.c:4:22"
                 , "    |"
                 , "   4|   return (struct A *)p;"
                 , "    |                      ^"
                 ]]

        it "warns on cast between incompatible enums (different size)" $ do
            shouldWarn'
                [ "enum A { A_1, A_2 };"
                , "enum B { B_1, B_2, B_3 };"
                , "enum A func(enum B b) {"
                , "  return (enum A)b;"
                , "}"
                ]
                [[ "error: enum types `enum A` and `enum B` have different a number of enumerators [-Wcast]"
                 , "   --> test.c:4:18"
                 , "    |"
                 , "   4|   return (enum A)b;"
                 , "    |                  ^"
                 ]]

        it "warns on cast between enums with different enumerator values" $ do
            shouldWarn'
                [ "enum A { A_1 = 1, A_2 = 2 };"
                , "enum B { B_1 = 1, B_2 = 3 };"
                , "enum A func(enum B b) {"
                , "  return (enum A)b;"
                , "}"
                ]
                [[ "error: invalid cast: enumerator value for `A_2 = 2` does not match `B_2 = 3` [-Wcast]"
                 , "   --> test.c:1:24"
                 , "    |"
                 , "   1| enum A { A_1 = 1, A_2 = 2 };"
                 , "    |                        ^"
                 ]]

    describe "allowed casts" $ do
        it "allows casting to void" $ do
            shouldAccept'
                [ "void func(int i) {"
                , "  (void)i;"
                , "}"
                ]

        it "allows casting from void* to other pointers" $ do
            shouldAccept'
                [ "int *func(void *vp) {"
                , "  int *p = (int *)vp;"
                , "  return p;"
                , "}"
                ]

        it "allows casting from other pointers to void*" $ do
            shouldAccept'
                [ "void *func(int *p) {"
                , "  return (void *)p;"
                , "}"
                ]

        it "allows casting from char* to uint8_t*" $ do
            shouldAccept'
                [ "typedef unsigned char uint8_t;"
                , "uint8_t *func(char *p) {"
                , "  return (uint8_t *)p;"
                , "}"
                ]

        it "allows casting from uint8_t* to char*" $ do
            shouldAccept'
                [ "typedef unsigned char uint8_t;"
                , "char *func(uint8_t *p) {"
                , "  return (char *)p;"
                , "}"
                ]

        it "allows casting from 0 to a pointer" $ do
            shouldAccept'
                [ "int *func() {"
                , "  return (int *)0;"
                , "}"
                ]

        it "allows casting from sockaddr_storage* to sockaddr*" $ do
            shouldAccept'
                [ "struct sockaddr {};"
                , "struct sockaddr_storage {};"
                , "struct sockaddr *func(struct sockaddr_storage *p) {"
                , "  return (struct sockaddr *)p;"
                , "}"
                ]

        it "allows casting between numeric types" $ do
            shouldAccept'
                [ "float func(int i) {"
                , "  return (float)i;"
                , "}"
                ]

        it "allows casting from enum to int" $ do
            shouldAccept'
                [ "enum A { A_1 };"
                , "int func(enum A a) {"
                , "  return (int)a;"
                , "}"
                ]

        it "allows casting from int to enum" $ do
            shouldAccept'
                [ "enum A { A_1 };"
                , "enum A func(int i) {"
                , "  return (enum A)i;"
                , "}"
                ]

        it "allows casting between compatible enums" $ do
            shouldAccept'
                [ "enum A { A_1 = 1, A_2 = 2 };"
                , "enum B { B_1 = 1, B_2 = 2 };"
                , "enum A func(enum B b) {"
                , "  return (enum A)b;"
                , "}"
                ]

        it "allows casting from a pointer type to itself" $ do
            shouldAccept'
                [ "struct Foo {};"
                , "struct Foo *func(struct Foo *p) {"
                , "  return (struct Foo *)p;"
                , "}"
                ]

        it "allows casting from a pointer type to its const pointer version" $ do
            shouldAccept'
                [ "struct Foo {};"
                , "const struct Foo *func(struct Foo *p) {"
                , "  return (const struct Foo *)p;"
                , "}"
                ]

        it "allows casting from a typedef to its underlying type" $ do
            shouldAccept'
                [ "typedef struct Foo {} Foo;"
                , "Foo *func(struct Foo *p) {"
                , "  return (Foo *)p;"
                , "}"
                ]

        it "allows casting from a struct pointer to a compatible typedef pointer" $ do
            shouldAccept'
                [ "typedef struct Foo {} Foo;"
                , "struct Foo *func(Foo *p) {"
                , "  return (struct Foo *)p;"
                , "}"
                ]

        it "allows casting from a T array to a pointer to const T" $ do
            shouldAccept'
                [ "char array[10];"
                , "const char *func() {"
                , "  return (const char *)array;"
                , "}"
                ]
