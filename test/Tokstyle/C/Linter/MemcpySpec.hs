{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.MemcpySpec (spec) where

import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["memcpy"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["memcpy"]


spec :: Spec
spec = do
    it "should accept memcpy of the same struct" $ do
        shouldAccept'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "struct Foo { int a; };"
            , "void f(struct Foo *a, struct Foo *b) {"
            , "  memcpy(a, b, sizeof(*a));"
            , "}"
            ]

    it "should warn about incompatible structs" $ do
        shouldWarn'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "struct Foo { int a; };"
            , "struct Bar { int a; };"
            , "void f(struct Foo *a, struct Bar *b) {"
            , "  memcpy(a, b, sizeof(*a));"
            , "}"
            ]
            [[ "error: `memcpy` first argument type `struct Foo *` is not compatible with second argument type `struct Bar *` [-Wmemcpy]"
             , "   --> test.c:5:10"
             , "    |"
             , "   5|   memcpy(a, b, sizeof(*a));"
             , "    |          ^"
             ]]

    it "should accept memcpy to/from uint8_t*" $ do
        shouldAccept'
            [ "typedef unsigned char uint8_t;"
            , "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "void f(uint8_t *a, int *b) {"
            , "  memcpy(a, b, sizeof(*b));"
            , "  memcpy(b, a, sizeof(*b));"
            , "}"
            ]

    it "should warn about pointers to arrays" $ do
        shouldWarn'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "void f(int (*a)[10], int (*b)[10]) {"
            , "  memcpy(a, b, sizeof(*a));"
            , "}"
            ]
            [[ "error: `memcpy` argument type `int (*)[10]` is not a valid memory type (pointers to arrays are not allowed) [-Wmemcpy]"
             , "   --> test.c:3:10"
             , "    |"
             , "   3|   memcpy(a, b, sizeof(*a));"
             , "    |          ^"
             ]
            ,[ "error: `memcpy` argument type `int (*)[10]` is not a valid memory type (pointers to arrays are not allowed) [-Wmemcpy]"
             , "   --> test.c:3:13"
             , "    |"
             , "   3|   memcpy(a, b, sizeof(*a));"
             , "    |             ^"
             ]]

    it "should warn about void pointers" $ do
        shouldWarn'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "void f(void *a, int *b) {"
            , "  memcpy(a, b, 4);"
            , "}"
            ]
            [[ "error: `memcpy` first argument type `void *` is not compatible with second argument type `int *` [-Wmemcpy]"
             , "   --> test.c:3:10"
             , "    |"
             , "   3|   memcpy(a, b, 4);"
             , "    |          ^"
             ]]

    it "should accept memcpy between uint8_t* and a struct" $ do
        shouldAccept'
            [ "typedef unsigned char uint8_t;"
            , "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "struct Foo { int a; };"
            , "void f(uint8_t *a, struct Foo *b) {"
            , "  memcpy(a, b, sizeof(*b));"
            , "}"
            ]

    it "should warn about incompatible enums" $ do
        shouldWarn'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "enum Foo { A };"
            , "enum Bar { B };"
            , "void f(enum Foo *a, enum Bar *b) {"
            , "  memcpy(a, b, sizeof(*a));"
            , "}"
            ]
            [[ "error: `memcpy` first argument type `enum Foo *` is not compatible with second argument type `enum Bar *` [-Wmemcpy]"
             , "   --> test.c:5:10"
             , "    |"
             , "   5|   memcpy(a, b, sizeof(*a));"
             , "    |          ^"
             ]]

    it "should warn about incompatible structs in memcmp" $ do
        shouldWarn ["memcpy"]
            [ "int memcmp(const void *s1, const void *s2, unsigned int n);"
            , "struct Foo { int a; };"
            , "struct Bar { int a; };"
            , "void f(struct Foo *a, struct Bar *b) {"
            , "  if (memcmp(a, b, sizeof(*a)) == 0) {}"
            , "}"
            ]
            [[ "error: `memcmp` first argument type `struct Foo *` is not compatible with second argument type `struct Bar *` [-Wmemcpy]"
             , "   --> test.c:5:14"
             , "    |"
             , "   5|   if (memcmp(a, b, sizeof(*a)) == 0) {}"
             , "    |              ^"
             ]]

    it "should accept memcpy with typedefs" $ do
        shouldAccept'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "typedef struct Foo { int a; } Foo_t;"
            , "void f(Foo_t *a, struct Foo *b) {"
            , "  memcpy(a, b, sizeof(*a));"
            , "}"
            ]

    it "should accept memcpy between sockaddr types" $ do
        shouldAccept'
            [ "void *memcpy(void *dst, const void *src, unsigned int n);"
            , "struct sockaddr { int a; };"
            , "struct sockaddr_storage { int a; char b[128]; };"
            , "void f(struct sockaddr_storage *a, struct sockaddr *b) {"
            , "  memcpy(a, b, sizeof(*b));"
            , "}"
            ]

    it "should warn about void pointers in memcmp" $ do
        shouldWarn ["memcpy"]
            [ "int memcmp(const void *s1, const void *s2, unsigned int n);"
            , "void f(void *a, void *b) {"
            , "  if (memcmp(a, b, 4) == 0) {}"
            , "}"
            ]
            [[ "error: `memcmp` first argument type `void *` is not compatible with second argument type `void *` [-Wmemcpy]"
             , "   --> test.c:3:14"
             , "    |"
             , "   3|   if (memcmp(a, b, 4) == 0) {}"
             , "    |              ^"
             ]]

    it "should warn about incompatible structs in memmove" $ do
        shouldWarn ["memcpy"]
            [ "void *memmove(void *dst, const void *src, unsigned int n);"
            , "struct Foo { int a; };"
            , "struct Bar { int a; };"
            , "void f(struct Foo *a, struct Bar *b) {"
            , "  memmove(a, b, sizeof(*a));"
            , "}"
            ]
            [[ "error: `memmove` first argument type `struct Foo *` is not compatible with second argument type `struct Bar *` [-Wmemcpy]"
             , "   --> test.c:5:11"
             , "    |"
             , "   5|   memmove(a, b, sizeof(*a));"
             , "    |           ^"
             ]]
