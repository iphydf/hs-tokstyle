{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.MemsetSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["memset"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["memset"]


spec :: Spec
spec = do
    it "should warn about passing structs with pointers to memset" $ do
        shouldWarn'
            [ "void *memset(void *s, int c, unsigned int n);"
            , "struct Foo { char *p; };"
            , "void f(void) {"
            , "  struct Foo foo;"
            , "  struct Foo foos[10];"
            , "  memset(&foo, 0, sizeof(foo));"
            , "  memset(foos, 0, sizeof(foos));"
            , "}"
            ]
            [[ "error: disallowed memset argument `&foo` of type `struct Foo *`, which contains pointers [-Wmemset]"
             , "   --> test.c:6:10"
             , "    |"
             , "   6|   memset(&foo, 0, sizeof(foo));"
             , "    |          ^^^^"
             ]
            ,[ "error: disallowed memset argument `foos` of type `struct Foo [10]`, which contains pointers [-Wmemset]"
             , "   --> test.c:7:10"
             , "    |"
             , "   7|   memset(foos, 0, sizeof(foos));"
             , "    |          ^^^^"
             ]]

    it "should detect nested structs" $ do
        shouldWarn'
            [ "void *memset(void *s, int c, unsigned int n);"
            , "typedef struct Foo { char *p; } Foo;"
            , "struct Bar { Foo foo; };"
            , "void f(void) {"
            , "  struct Bar bar;"
            , "  memset(&bar, 0, sizeof(bar));"
            , "}"
            ]
            [[ "error: disallowed memset argument `&bar` of type `struct Bar *`, which contains pointers [-Wmemset]"
             , "   --> test.c:6:10"
             , "    |"
             , "   6|   memset(&bar, 0, sizeof(bar));"
             , "    |          ^^^^"
             ]]

    it "should not warn about structs without pointers" $ do
        shouldAccept'
            [ "void *memset(void *s, int c, unsigned int n);"
            , "typedef struct Foo { char c; } Foo;"
            , "struct Bar { Foo foo; };"
            , "void f(void) {"
            , "  struct Bar bar;"
            , "  memset(&bar, 0, sizeof(bar));"
            , "}"
            ]
