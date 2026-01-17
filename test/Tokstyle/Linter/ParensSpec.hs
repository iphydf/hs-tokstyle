{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ParensSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["parens"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["parens"]


spec :: Spec
spec = do
    it "warns about parentheses around return expressions" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  return (1 + 2);"
            , "}"
            ]
            [[ "warning: return expression does not need parentheses [-Wparens]"
             , "   --> test.c:2:11"
             , "    |"
             , "   2|   return (1 + 2);"
             , "    |           ^^^^^"
             ]]

    it "does not warn about parens in if conditions" $ do
        shouldAccept'
                [ "int a(int b) {"
                , "  if ((true)) { return 3; }"
                , "}"
                ]
