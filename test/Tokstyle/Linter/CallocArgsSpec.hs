{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallocArgsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["calloc-args"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["calloc-args"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid calloc" $ do
        shouldAccept'
            [ "void f(int n) {"
            , "  int *p = calloc(n, sizeof(int));"
            , "}"
            ]

    it "should give diagnostics on calloc with invalid nmemb" $ do
        shouldWarn'
            [ "void f(int n) {"
            , "  int *p = calloc(sizeof(int), sizeof(int));"
            , "}"
            ]
            [[ "warning: `sizeof` should not appear in the `nmemb` argument to `calloc` [-Wcalloc-args]"
             , "   --> test.c:2:26"
             , "    |"
             , "   2|   int *p = calloc(sizeof(int), sizeof(int));"
             , "    |                          ^^^"
             ]]

    it "should give diagnostics on calloc with invalid size" $ do
        shouldWarn'
            [ "void f(int n) {"
            , "  int *p = calloc(n, 16);"
            , "}"
            ]
            [[ "warning: `size` argument in call to `calloc` must be a sizeof expression [-Wcalloc-args]"
             , "   --> test.c:2:22"
             , "    |"
             , "   2|   int *p = calloc(n, 16);"
             , "    |                      ^^"
             ]]

    it "should not give diagnostics on valid realloc" $ do
        shouldAccept'
            [ "void f(int n, void *ptr) {"
            , "  int *p = realloc(ptr, n * sizeof(int));"
            , "}"
            ]

    it "should not give diagnostics on valid mem_alloc" $ do
        shouldAccept'
            [ "void f(void *mem) {"
            , "  int *p = mem_alloc(mem, sizeof(int));"
            , "}"
            ]

    it "should not give diagnostics on valid mem_valloc" $ do
        shouldAccept'
            [ "void f(void *mem, int n) {"
            , "  int *p = mem_valloc(mem, n, sizeof(int));"
            , "}"
            ]

    it "should give diagnostics on wrong number of arguments" $ do
        shouldWarn'
            [ "void f(void* p, void* mem) {"
            , "  p = calloc(1);"
            , "  p = realloc(p, 1, 2);"
            , "  p = mem_alloc(mem, 1, 2);"
            , "  p = mem_valloc(mem, 1);"
            , "}"
            ]
            [[ "warning: invalid `calloc` invocation: 2 arguments expected [-Wcalloc-args]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   p = calloc(1);"
             , "    |       ^^^^^^^^"
             ]
            ,[ "warning: invalid `realloc` invocation: 2 arguments expected [-Wcalloc-args]"
             , "   --> test.c:3:7"
             , "    |"
             , "   3|   p = realloc(p, 1, 2);"
             , "    |       ^^^^^^^^^^^^^^^"
             ]
            ,[ "warning: invalid `mem_alloc` invocation: 1 argument after `mem` expected [-Wcalloc-args]"
             , "   --> test.c:4:7"
             , "    |"
             , "   4|   p = mem_alloc(mem, 1, 2);"
             , "    |       ^^^^^^^^^^^^^^^^^^^"
             ]
            ,[ "warning: invalid `mem_valloc` invocation: 2 arguments after `mem` expected [-Wcalloc-args]"
             , "   --> test.c:5:7"
             , "    |"
             , "   5|   p = mem_valloc(mem, 1);"
             , "    |       ^^^^^^^^^^^^^^^^^"
             ]]
