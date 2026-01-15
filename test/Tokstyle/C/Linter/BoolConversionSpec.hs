{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.BoolConversionSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings)
import           Tokstyle.C.LinterSpec (check)


spec :: Spec
spec = describe "BoolConversion linter" $ do
    describe "implicit conversion to bool" $ do
        it "warns on ternary operators" $ do
            let code =
                    [ "void func(char *p) {"
                    , "  int i = p ? 1 : 0;"
                    , "}"
                    ]
            check ["bool-conversion"] code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: implicit conversion from char * to bool [-Wbool-conversion]"
                    , "   --> test.c:2:11"
                    , "    |"
                    , "2   |   int i = p ? 1 : 0;"
                    , "    |           ^"
                    , "    |"
                    ]
                ]

        it "warns on logical not" $ do
            let code =
                    [ "void func(char *p) {"
                    , "  !p;"
                    , "}"
                    ]
            check ["bool-conversion"] code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: implicit conversion from char * to bool [-Wbool-conversion]"
                    , "   --> test.c:2:4"
                    , "    |"
                    , "2   |   !p;"
                    , "    |    ^"
                    , "    |"
                    ]
                ]

        it "warns on logical or" $ do
            let code =
                    [ "void func(char *p) {"
                    , "  p || p;"
                    , "}"
                    ]
            check ["bool-conversion"] code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: implicit conversion from char * to bool [-Wbool-conversion]"
                    , "   --> test.c:2:3"
                    , "    |"
                    , "2   |   p || p;"
                    , "    |   ^"
                    , "    |"
                    ]
                , Text.stripEnd $ Text.unlines
                    [ "error: implicit conversion from char * to bool [-Wbool-conversion]"
                    , "   --> test.c:2:8"
                    , "    |"
                    , "2   |   p || p;"
                    , "    |        ^"
                    , "    |"
                    ]
                ]

        it "warns on logical and" $ do
            let code =
                    [ "void func(char *p) {"
                    , "  p && p;"
                    , "}"
                    ]
            check ["bool-conversion"] code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: implicit conversion from char * to bool [-Wbool-conversion]"
                    , "   --> test.c:2:3"
                    , "    |"
                    , "2   |   p && p;"
                    , "    |   ^"
                    , "    |"
                    ]
                , Text.stripEnd $ Text.unlines
                    [ "error: implicit conversion from char * to bool [-Wbool-conversion]"
                    , "   --> test.c:2:8"
                    , "    |"
                    , "2   |   p && p;"
                    , "    |        ^"
                    , "    |"
                    ]
                ]

    describe "valid bool expressions" $ do
        it "allows integer literals" $ do
            let code =
                    [ "void func() {"
                    , "  if (1) {}"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []

        it "allows boolean expressions" $ do
            let code =
                    [ "void func(int i) {"
                    , "  if (i == 1) {}"
                    , "}"
                    ]
            check allWarnings code `shouldBe` []
