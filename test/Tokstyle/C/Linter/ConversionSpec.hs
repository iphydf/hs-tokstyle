{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.ConversionSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (check)


spec :: Spec
spec = describe "Conversion linter" $ do
    describe "invalid conversions" $ do
        it "warns on returning incompatible pointer types" $ do
            let code =
                    [ "char *func(int *p) {"
                    , "  return p;"
                    , "}"
                    ]
            check ["conversion"] code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: invalid conversion from `int *` to `char *` in return [-Wconversion]"
                    , "   --> test.c:2:10"
                    , "    |"
                    , "2   |   return p;"
                    , "    |          ^"
                    , "    |"
                    ]
                ]

    describe "valid conversions" $ do
        it "allows casting to void*" $ do
            let code =
                    [ "void *func(int *p) {"
                    , "  return p;"
                    , "}"
                    ]
            check ["conversion"] code `shouldBe` []

        it "allows integer promotions" $ do
            let code =
                    [ "long func(int i) {"
                    , "  return i;"
                    , "}"
                    ]
            check ["conversion"] code `shouldBe` []

        it "allows Memory const * to const struct Memory * assignment" $ do
            let code =
                    [ "typedef struct Memory Memory;"
                    , "void func() {"
                    , "  const struct Memory *a;"
                    , "  Memory const *b;"
                    , "  a = b;"
                    , "}"
                    ]
            check ["conversion"] code `shouldBe` []

        it "allows const struct Memory * to Memory const * assignment" $ do
            let code =
                    [ "typedef struct Memory Memory;"
                    , "void func() {"
                    , "  const struct Memory *a;"
                    , "  Memory const *b;"
                    , "  b = a;"
                    , "}"
                    ]
            check ["conversion"] code `shouldBe` []

        it "allows int* to const int* assignment" $ do
            let code =
                    [ "void func() {"
                    , "  int *a = 0;"
                    , "  const int *b;"
                    , "  b = a;"
                    , "}"
                    ]
            check ["conversion"] code `shouldBe` []

        it "allows nullptr assignment" $ do
            let code =
                    [ "typedef void *nullptr_t;"
                    , "extern nullptr_t nullptr;"
                    , "void func() {"
                    , "  int *a;"
                    , "  a = nullptr;"
                    , "}"
                    ]
            check ["conversion"] code `shouldBe` []

        it "warns on int to pointer assignment" $ do
            let code =
                    [ "void func() {"
                    , "  int a = 0;"
                    , "  char *b;"
                    , "  b = a;"
                    , "}"
                    ]
            check ["conversion"] code
                `shouldBe`
                [ Text.stripEnd $ Text.unlines
                    [ "error: invalid conversion from `int` to `char *` in assignment [-Wconversion]"
                    , "   --> test.c:4:7"
                    , "    |"
                    , "4   |   b = a;"
                    , "    |       ^"
                    , "    |"
                    ]
                ]
