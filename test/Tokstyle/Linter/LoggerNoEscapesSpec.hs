{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LoggerNoEscapesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["logger-no-escapes"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["logger-no-escapes"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid logger calls" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  LOGGER_INFO(log, \"foo\");"
            , "}"
            ]

    it "should give diagnostics on invalid logger calls" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  LOGGER_INFO(log, \"foo\\n\");"
            , "}"
            ]
            [[ "warning: logger format \"foo\\n\" contains escape sequences (newlines, tabs, or escaped quotes) [-Wlogger-no-escapes]"
             , "   --> test.c:2:20"
             , "    |"
             , "   2|   LOGGER_INFO(log, \"foo\\n\");"
             , "    |                    ^^^^^^^"
             ]]

    it "should not give diagnostics on LOGGER_ASSERT" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  LOGGER_ASSERT(log, 1, \"foo\");"
            , "}"
            ]
