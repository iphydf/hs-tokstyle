{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.ConversionSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = describe "Conversion linter" $ do
    describe "invalid conversions" $ do
        it "warns on returning incompatible pointer types" $ do
            ast <- mustParse
                [ "char *func(int *p) {"
                , "  return p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 10) [ERROR]  >>> Type mismatch"
                    , "  invalid conversion from `int *` to `char *` in return"
                    ]
                ]

    describe "valid conversions" $ do
        it "allows casting to void*" $ do
            ast <- mustParse
                [ "void *func(int *p) {"
                , "  return p;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows integer promotions" $ do
            ast <- mustParse
                [ "long func(int i) {"
                , "  return i;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows Memory const * to const struct Memory * assignment" $ do
            ast <- mustParse
                [ "typedef struct Memory Memory;"
                , "void func() {"
                , "  const struct Memory *a;"
                , "  Memory const *b;"
                , "  a = b;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows const struct Memory * to Memory const * assignment" $ do
            ast <- mustParse
                [ "typedef struct Memory Memory;"
                , "void func() {"
                , "  const struct Memory *a;"
                , "  Memory const *b;"
                , "  b = a;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows int* to const int* assignment" $ do
            ast <- mustParse
                [ "void func() {"
                , "  int *a = 0;"
                , "  const int *b;"
                , "  b = a;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows nullptr assignment" $ do
            ast <- mustParse
                [ "typedef void *nullptr_t;"
                , "extern nullptr_t nullptr;"
                , "void func() {"
                , "  int *a;"
                , "  a = nullptr;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "warns on int to pointer assignment" $ do
            ast <- mustParse
                [ "void func() {"
                , "  int a = 0;"
                , "  char *b;"
                , "  b = a;"
                , "}"
                ]
            analyse allWarnings ast `shouldBe`
                [ Text.unlines
                    [ "test.c:4: (column 3) [ERROR]  >>> Type mismatch"
                    , "  invalid conversion from `int` to `char *` in assignment"
                    ]
                ]
