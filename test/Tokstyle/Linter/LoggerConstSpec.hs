{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LoggerConstSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["logger-const"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["logger-const"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid logger declarations" $ do
        shouldAccept' [ "void foo(const Logger *log);" ]

    it "should give diagnostics on invalid logger declarations" $ do
        shouldWarn'
            [ "void foo(Logger *log);" ]
            [[ "warning: Logger parameter should be pointer-to-const [-Wlogger-const]"
             , "   --> test.c:1:18"
             , "    |"
             , "   1| void foo(Logger *log);"
             , "    |                  ^^^"
             ]]

    it "should not give diagnostics on struct members" $ do
        shouldAccept'
            [ "struct Foo {"
            , "  Logger *log;"
            , "};"
            ]

    it "should not give diagnostics on variable declarations" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  Logger *log;"
            , "}"
            ]
