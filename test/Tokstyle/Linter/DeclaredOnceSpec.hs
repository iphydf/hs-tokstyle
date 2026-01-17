{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DeclaredOnceSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn, testC)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["declared-once"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["declared-once"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid declarations" $ do
        shouldAccept'
            [ ("test1.h", [ "int foo(void);" ])
            , ("test2.c", [ "int foo(void) { return 0; }" ])
            ]

    it "should give diagnostics on duplicate declaration in the same file" $ do
        shouldWarn' (testC
            [ "void foo(void);"
            , "void foo(void);"
            ])
            [[ "warning: duplicate declaration of function `foo` [-Wdeclared-once]"
             , "   --> test.c:1:6"
             , "    |"
             , "   1| void foo(void);"
             , "    |      ^^^"
             ]
            ,[ "warning: function `foo` also declared here [-Wdeclared-once]"
             , "   --> test.c:2:6"
             , "    |"
             , "   2| void foo(void);"
             , "    |      ^^^"
             ]]

    it "should give diagnostics on duplicate declaration in different files" $ do
        shouldWarn'
            [ ("test1.h", [ "void foo(void);" ])
            , ("test2.h", [ "void foo(void);" ])
            ]
            [[ "warning: duplicate declaration of function `foo` [-Wdeclared-once]"
             , "   --> test1.h:1:6"
             , "    |"
             , "   1| void foo(void);"
             , "    |      ^^^"
             ]
            ,[ "warning: function `foo` also declared here [-Wdeclared-once]"
             , "   --> test2.h:1:6"
             , "    |"
             , "   1| void foo(void);"
             , "    |      ^^^"
             ]]

    it "should not give diagnostics for multiple definitions" $ do
        shouldAccept'
            [ ("test1.c", [ "int foo(void) { return 0; }" ])
            , ("test2.c", [ "int foo(void) { return 0; }" ])
            ]
