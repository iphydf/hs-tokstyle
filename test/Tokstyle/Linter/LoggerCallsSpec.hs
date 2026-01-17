{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LoggerCallsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["logger-calls"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["logger-calls"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid logger calls" $ do
        shouldAccept'
            [ "void foo(Logger *log) {"
            , "  LOGGER_INFO(log, \"foo\");"
            , "  LOGGER_INFO(log, \"foo\", 1);"
            , "}"
            ]

    it "should give diagnostics on invalid logger calls" $ do
        shouldWarn'
            [ "void foo(Logger *log) {"
            , "  const char *foo = \"foo\";"
            , "  LOGGER_INFO(log, foo);"
            , "}"
            ]
            [[ "warning: logger call `LOGGER_INFO` has a non-literal format argument [-Wlogger-calls]"
             , "   --> test.c:3:3"
             , "    |"
             , "   3|   LOGGER_INFO(log, foo);"
             , "    |   ^^^^^^^^^^^"
             ]]

    it "should not give diagnostics on LOGGER_ASSERT" $ do
        shouldAccept'
            [ "void foo(Logger *log) {"
            , "  LOGGER_ASSERT(log, 1, \"foo\");"
            , "}"
            ]
