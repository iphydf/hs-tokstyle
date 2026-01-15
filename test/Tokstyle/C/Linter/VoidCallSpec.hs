{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.VoidCallSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (check)


spec :: Spec
spec = do
    it "warns if there is code before the cast-from-void" $ do
        let code =
                [ "void add_handler(int *a, void *user_data) {"
                , "  if (user_data == 0) { return; }"  -- this should be done *after* the cast
                , "  const int *b = (const int *)user_data;"
                , "  *a += *b;"
                , "}"
                ]
        check ["void-call"] code
            `shouldBe`
            [ Text.stripEnd $ Text.unlines
                [ "error: first statement must cast `void *user_data` to `const int *` [-Wvoid-call]"
                , "   --> test.c:3:18"
                , "    |"
                , "3   |   const int *b = (const int *)user_data;"
                , "    |                  ^^^^^^^^^^^^^^^^^^^^^^"
                , "    |"
                ]
            ]

    it "warns if there is code before the cast-from-void in multi-vptr function" $ do
        let code =
                [ "void add_handler(void *obj, void *user_data) {"
                , "  if (user_data == 0) { return; }"  -- this should be done *after* the cast
                , "  int *b = (int *)obj;"
                , "  *b += 3;"
                , "}"
                ]
        check ["void-call"] code
            `shouldBe`
            [ Text.stripEnd $ Text.unlines
                [ "error: first statement must cast `void *obj` to `int *` [-Wvoid-call]"
                , "   --> test.c:3:12"
                , "    |"
                , "3   |   int *b = (int *)obj;"
                , "    |            ^^^^^^^^^^"
                , "    |"
                ]
            ]

    it "doesn't warn if there is no cast-from-void" $ do
        let code =
                [ "void add_handler(int *a, void *user_data) {"
                , "  *a += 3;"
                , "}"
                ]
        check ["void-call"] code `shouldBe` []

    it "accepts the correct handler function format" $ do
        let code =
                [ "void add_handler(int *a, void *user_data) {"
                , "  int *b = (int *)user_data;"
                , "  *a += *b;"
                , "}"
                ]
        check ["void-call"] code `shouldBe` []

    it "accepts multiple differently-typed void pointer arguments" $ do
        let code =
                [ "void add_handler(void *va, void *vb) {"
                , "  long *a = (long *)va;"
                , "  int *b = (int *)vb;"
                , "  *a += *b;"
                , "}"
                ]
        check ["void-call"] code `shouldBe` []

    it "allows the declaration to be const-qualified" $ do
        let code =
                [ "void add_handler(int *a, void *user_data) {"
                , "  int *const b = (int *)user_data;"
                , "  *a += *b;"
                , "}"
                ]
        check ["void-call"] code `shouldBe` []

    it "warns on cast-from-vptr outside the first declaration statement" $ do
        let code =
                [ "void do_something(int *a, int *b);"
                , "void add_handler(int *a, void *user_data) {"
                , "  do_something(a, (int *)user_data);"
                , "}"
                ]
        check ["void-call"] code
            `shouldBe`
            [ Text.stripEnd $ Text.unlines
                [ "error: first statement must cast `void *user_data` to `int *` [-Wvoid-call]"
                , "   --> test.c:3:19"
                , "    |"
                , "3   |   do_something(a, (int *)user_data);"
                , "    |                   ^^^^^^^^^^^^^^^^"
                , "    |"
                ]
            ]

    it "accepts passing void pointers as-is" $ do
        let code =
                [ "void do_something(void *user_data);"
                , "void add_handler(int *a, void *user_data) {"
                , "  do_something(user_data);"
                , "}"
                ]
        check ["void-call"] code `shouldBe` []
