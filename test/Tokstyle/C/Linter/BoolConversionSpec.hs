{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.BoolConversionSpec (spec) where
import           Data.Text             (Text, unlines)
import           Prelude               hiding (unlines)

import           Test.Hspec            (Spec, describe, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["bool-conversion"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["bool-conversion"]


spec :: Spec
spec = describe "BoolConversion linter" $ do
    describe "implicit conversion to bool" $ do
        it "warns on ternary operators" $ do
            shouldWarn'
                [ "void func(char *p) {"
                , "  int i = p ? 1 : 0;"
                , "}"
                ]
                [[ "error: implicit conversion from `char *` to bool [-Wbool-conversion]"
                 , "   --> test.c:2:11"
                 , "    |"
                 , "   2|   int i = p ? 1 : 0;"
                 , "    |           ^"
                 ]]

        it "warns on logical not" $ do
            shouldWarn'
                [ "void func(char *p) {"
                , "  !p;"
                , "}"
                ]
                [[ "error: implicit conversion from `char *` to bool [-Wbool-conversion]"
                 , "   --> test.c:2:4"
                 , "    |"
                 , "   2|   !p;"
                 , "    |    ^"
                 ]]

        it "warns on logical or" $ do
            shouldWarn'
                [ "void func(char *p) {"
                , "  p || p;"
                , "}"
                ]
                [[ "error: implicit conversion from `char *` to bool [-Wbool-conversion]"
                 , "   --> test.c:2:3"
                 , "    |"
                 , "   2|   p || p;"
                 , "    |   ^"
                 ]
                ,[ "error: implicit conversion from `char *` to bool [-Wbool-conversion]"
                 , "   --> test.c:2:8"
                 , "    |"
                 , "   2|   p || p;"
                 , "    |        ^"
                 ]]

        it "warns on logical and" $ do
            shouldWarn'
                [ "void func(char *p) {"
                , "  p && p;"
                , "}"
                ]
                [[ "error: implicit conversion from `char *` to bool [-Wbool-conversion]"
                 , "   --> test.c:2:3"
                 , "    |"
                 , "   2|   p && p;"
                 , "    |   ^"
                 ]
                ,[ "error: implicit conversion from `char *` to bool [-Wbool-conversion]"
                 , "   --> test.c:2:8"
                 , "    |"
                 , "   2|   p && p;"
                 , "    |        ^"
                 ]]

    describe "valid bool expressions" $ do
        it "allows integer literals" $ do
            shouldAccept'
                [ "void func() {"
                , "  if (1) {}"
                , "}"
                ]

        it "allows boolean expressions" $ do
            shouldAccept'
                [ "void func(int i) {"
                , "  if (i == 1) {}"
                , "}"
                ]
