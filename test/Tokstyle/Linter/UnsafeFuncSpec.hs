{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.UnsafeFuncSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["unsafe-func"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["unsafe-func"]


spec :: Spec
spec = do
    it "should not give diagnostics on safe functions" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  snprintf(buf, 5, \"foo\");"
            , "}"
            ]

    it "should give diagnostics on unsafe functions" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  sprintf(buf, \"foo\");"
            , "}"
            ]
            [[ "warning: function `sprintf` should not be used, because it has no way of bounding the number of characters written; use `snprintf` instead [-Wunsafe-func]"
             , "   --> test.c:2:3"
             , "    |"
             , "   2|   sprintf(buf, \"foo\");"
             , "    |   ^^^^^^^^^^^^^^^^^^"
             ]]
