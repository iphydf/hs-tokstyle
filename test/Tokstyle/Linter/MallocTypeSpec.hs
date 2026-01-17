{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MallocTypeSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["malloc-type"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["malloc-type"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid malloc calls" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  uint8_t *buf = (uint8_t *)malloc(10);"
            , "}"
            ]

    it "should give diagnostics on invalid malloc calls" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  My_Struct *s = (My_Struct *)malloc(sizeof(My_Struct));"
            , "}"
            ]
            [[ "warning: `malloc` should be used for builtin types only (e.g. `uint8_t *` or `int16_t *`); use `mem_alloc` instead [-Wmalloc-type]"
             , "   --> test.c:2:19"
             , "    |"
             , "   2|   My_Struct *s = (My_Struct *)malloc(sizeof(My_Struct));"
             , "    |                   ^^^^^^^^^"
             ]]

    it "should give diagnostics on uncasted malloc" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  void *buf = malloc(10);"
            , "}"
            ]
            [[ "warning: the result of `malloc` must be cast; plain `void *` is not supported [-Wmalloc-type]"
             , "   --> test.c:2:15"
             , "    |"
             , "   2|   void *buf = malloc(10);"
             , "    |               ^^^^^^^^^"
             ]]
