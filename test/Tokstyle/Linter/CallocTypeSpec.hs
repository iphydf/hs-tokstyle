{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallocTypeSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["calloc-type"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["calloc-type"]


spec :: Spec
spec = do
    it "detects when mem_alloc() is used with built-in types" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  uint8_t *a = (uint8_t *)mem_alloc(mem, sizeof(uint8_t));"
            , "}"
            ]
            [[ "warning: `mem_alloc` should not be used for `uint8_t*`; use `mem_balloc` instead [-Wcalloc-type]"
             , "   --> test.c:2:17"
             , "    |"
             , "   2|   uint8_t *a = (uint8_t *)mem_alloc(mem, sizeof(uint8_t));"
             , "    |                 ^^^^^^^"
             ]]

    it "detects when mem_valloc() result is cast to the wrong type" $ do
        shouldWarnLocal ["calloc-args", "calloc-type"]
            [ "int a(void) {"
            , "  uint8_t *a = (uint8_t *)mem_valloc(mem, 1);"
            , "}"
            ]
            [[ "warning: invalid `mem_valloc` invocation: 2 arguments after `mem` expected [-Wcalloc-args]"
             , "   --> test.c:2:27"
             , "    |"
             , "   2|   uint8_t *a = (uint8_t *)mem_valloc(mem, 1);"
             , "    |                           ^^^^^^^^^^^^^^^^^"
             ]
            ,[ "warning: the result of `mem_valloc` must be cast to its member type [-Wcalloc-type]"
             , "   --> test.c:2:27"
             , "    |"
             , "   2|   uint8_t *a = (uint8_t *)mem_valloc(mem, 1);"
             , "    |                           ^^^^^^^^^^^^^^^^^"
             ]]

    it "detects when mem_valloc() result is not cast to any type" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  void *a = mem_valloc(mem, 2, sizeof(int));"
            , "}"
            ]
            [[ "warning: the result of `mem_valloc` must be cast to its member type [-Wcalloc-type]"
             , "   --> test.c:2:13"
             , "    |"
             , "   2|   void *a = mem_valloc(mem, 2, sizeof(int));"
             , "    |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "should not give diagnostics mem_alloc() used correctly" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  Foo *a = (Foo *)mem_alloc(mem, sizeof(Foo));"
            , "}"
            ]
