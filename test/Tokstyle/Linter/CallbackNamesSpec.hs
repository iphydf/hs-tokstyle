{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallbackNamesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["callback-names"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["callback-names"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid callback names" $ do
        shouldAccept'
            [ "typedef void bar_cb(void);"
            , "void foo(bar_cb *bar_callback);"
            ]

    it "should give diagnostics on invalid callback names" $ do
        shouldWarn'
            [ "typedef void bar_cb(void);"
            , "void foo(bar_cb *bar);"
            ]
            [[ "warning: function pointer `bar` should end in `callback` [-Wcallback-names]"
             , "   --> test.c:2:10"
             , "    |"
             , "   2| void foo(bar_cb *bar);"
             , "    |          ^^^^^^^^^^^"
             ]]

    it "should handle various allowed suffixes" $ do
        shouldAccept'
            [ "typedef void foo_cb(void);"
            , "void foo(foo_cb *bar_callback, foo_cb *bar_function, foo_cb *bar_handler);"
            ]
