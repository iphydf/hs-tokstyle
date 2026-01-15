{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SizeofSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (check)


spec :: Spec
spec = do
    it "should give diagnostics on passing pointers to sizeof" $ do
        let code =
                [ "int foo(void) {"
                , "  char *ptr;"
                , "  return sizeof ptr;"
                , "}"
                ]
        check ["sizeof"] code
            `shouldBe`
            [ Text.stripEnd $ Text.unlines
                [ "error: disallowed sizeof argument of type `char *` - did you mean for `ptr` to be an array? [-Wsizeof]"
                , "   --> test.c:3:17"
                , "    |"
                , "3   |   return sizeof ptr;"
                , "    |                 ^^^"
                , "    |"
                ]
            ]

    it "should give diagnostics on passing array-element pointers to sizeof" $ do
        let code =
                [ "int foo(void) {"
                , "  char arr[10];"
                , "  return sizeof &arr[0];"
                , "}"
                ]
        check ["sizeof"] code
            `shouldBe`
            [ Text.stripEnd $ Text.unlines
                [ "error: disallowed sizeof argument of type `char *` - did you mean for `&arr[0]` to be an array? [-Wsizeof]"
                , "   --> test.c:3:17"
                , "    |"
                , "3   |   return sizeof &arr[0];"
                , "    |                 ^^^^^^^"
                , "    |"
                ]
            ]

    it "should not diagnostics on passing arrays to sizeof" $ do
        let code =
                [ "int foo(void) {"
                , "  char arr[10];"
                , "  return sizeof arr;"
                , "}"
                ]
        check ["sizeof"] code `shouldBe` []
