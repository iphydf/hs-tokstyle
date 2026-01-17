{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MemcpyStructsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["memcpy-structs"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["memcpy-structs"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid memcpy calls" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  memcpy(a, b, 10);"
            , "}"
            ]

    it "should give diagnostics on invalid memcpy calls" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  memcpy(a, b, sizeof(My_Struct));"
            , "}"
            ]
            [[ "warning: `memcpy` should not be used for structs like `My_Struct`; use assignment instead [-Wmemcpy-structs]"
             , "   --> test.c:2:23"
             , "    |"
             , "   2|   memcpy(a, b, sizeof(My_Struct));"
             , "    |                       ^^^^^^^^^"
             ]]

    it "should not give diagnostics on valid memset calls" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  memset(a, 0, 10);"
            , "}"
            ]

    it "should give diagnostics on invalid memset calls" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  memset(a, 0, sizeof(My_Struct));"
            , "}"
            ]
            [[ "warning: `memset` should not be used for structs like `My_Struct`; use `(Type) {0}` instead [-Wmemcpy-structs]"
             , "   --> test.c:2:23"
             , "    |"
             , "   2|   memset(a, 0, sizeof(My_Struct));"
             , "    |                       ^^^^^^^^^"
             ]]
