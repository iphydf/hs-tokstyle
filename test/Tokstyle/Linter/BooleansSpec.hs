{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.BooleansSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["booleans"]


spec :: Spec
spec = do
    it "should give diagnostics on simplifiable if-return" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  if (b == 0) { return true; }"
            , "  return false;"
            , "}"
            ]
            [[ "warning: if-statement followed by boolean return can be simplified to return [-Wbooleans]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   if (b == 0) { return true; }"
             , "    |       ^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "should give diagnostics on simplifiable if/else with boolean return" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  if (b == 0) { return true; }"
            , "  else { return false; }"
            , "}"
            ]
            [[ "warning: if/else with return true/false can be simplified to return [-Wbooleans]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   if (b == 0) { return true; }"
             , "    |       ^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "should give diagnostics when there are other statements before the violating code" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  do_something();"
            , "  if (b == 0) { return true; }"
            , "  return false;"
            , "}"
            ]
            [[ "warning: if-statement followed by boolean return can be simplified to return [-Wbooleans]"
             , "   --> test.c:3:7"
             , "    |"
             , "   3|   if (b == 0) { return true; }"
             , "    |       ^^^^^^^^^^^^^^^^^^^^^"
             ]]
