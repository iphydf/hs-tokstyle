{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NullabilitySpec (spec) where

import           Data.Text           (Text, unlines)
import qualified Data.Text           as Text
import           Prelude             hiding (unlines)
import           Test.Hspec          (Spec, describe, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldAcceptLocal,
                                      shouldWarn, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["nullability"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["nullability"]


spec :: Spec
spec = do
    describe "warnings" $ do
        it "warns when a nullable pointer is cast to a nonnull pointer without check" $ do
            let code = [ "int *_Nonnull my_func(int *_Nullable p) {"
                       , "  return (int *_Nonnull)p;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                 , "   --> test.c:2:25"
                 , "    |"
                 , "   2|   return (int *_Nonnull)p;"
                 , "    |                         ^"
                 ]]

        it "warns when a nullable pointer is cast to a nonnull pointer without check, in a function call" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                 , "   --> test.c:3:33"
                 , "    |"
                 , "   3|    my_other_func((int *_Nonnull)p);"
                 , "    |                                 ^"
                 ]]

        it "warns when a checked pointer is re-assigned from a nullable source before cast" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p, int *_Nullable q) {"
                       , "   if (p != nullptr) {"
                       , "     p = q; // p is no longer guaranteed to be non-null"
                       , "     my_other_func((int *_Nonnull)p);"
                       , "   }"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                 , "   --> test.c:5:35"
                 , "    |"
                 , "   5|      my_other_func((int *_Nonnull)p);"
                 , "    |                                   ^"
                 ]]

        it "warns when a nullable struct field is cast to nonnull" $ do
            let code = [ "typedef struct Foo { int *_Nullable v; } Foo;"
                       , "void my_void_func(int *_Nonnull p);"
                       , "void my_func(Foo *_Nonnull foo) {"
                       , "   my_void_func((int *_Nonnull)foo->v);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `foo->v` is nullable and has not been checked before this cast [-Wnullability]"
                 , "   --> test.c:4:32"
                 , "    |"
                 , "   4|    my_void_func((int *_Nonnull)foo->v);"
                 , "    |                                ^^^^^^"
                 ]]

        it "warns when a nullable pointer is passed to a nonnull parameter without check" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   my_other_func(p);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this call [-Wnullability]"
                 , "   --> test.c:3:18"
                 , "    |"
                 , "   3|    my_other_func(p);"
                 , "    |                  ^"
                 ]]

        it "warns when a nullable pointer is passed to a nonnull parameter defined in another file" $ do
            let code1 = [ "void my_other_func(int *_Nonnull p);" ]
                code2 = [ "void my_func(int *_Nullable p) {"
                        , "   my_other_func(p);"
                        , "}"
                        ]
            shouldWarn ["nullability"] [("header.h", code1), ("test.c", code2)]
                [[ "warning: expression `p` is nullable and has not been checked before this call [-Wnullability]"
                 , "   --> test.c:2:18"
                 , "    |"
                 , "   2|    my_other_func(p);"
                 , "    |                  ^"
                 ]]

    describe "acceptance" $ do
        it "does not warn when a nonnull pointer is cast to a nullable pointer" $ do
            let code = [ "int *_Nullable my_func(int *_Nonnull p) {"
                       , "   return (int *_Nullable)p;"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a nonnull pointer is cast to a nonnull pointer" $ do
            let code = [ "int *_Nonnull my_func(int *_Nonnull p) {"
                       , "   return (int *_Nonnull)p;"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a nullable pointer is cast to a nonnull pointer with a preceding if-check" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p == nullptr) { return; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a nullable pointer is cast to a nonnull pointer inside an if-statement checking for non-nullness" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p != nullptr) { my_other_func((int *_Nonnull)p); }"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a cast occurs in an else-block after a null-check" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p == nullptr) { return; } else { my_other_func((int *_Nonnull)p); }"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a cast is on the true-branch of a ternary operator" $ do
            let code = [ "int my_other_func(int *_Nonnull p);"
                       , "int my_func(int *_Nullable p) {"
                       , "   return p ? my_other_func((int *_Nonnull)p) : 0;"
                       , "}"
                       ]
            shouldAccept' code

        it "supports struct field access for nullness checks" $ do
            let code = [ "typedef struct Foo { int *_Nullable v; } Foo;"
                       , "void my_other_func(int *_Nonnull p);"
                       , "void my_func(Foo *_Nonnull foo) {"
                       , "   if (foo->v != nullptr) { my_other_func((int *_Nonnull)foo->v); }"
                       , "}"
                       ]
            shouldAccept' code

        it "supports operator && as a context for nullness having been checked" $ do
            let code = [ "typedef struct Foo { int *_Nullable v; } Foo;"
                       , "bool my_bool_func(int *_Nonnull p);"
                       , "int my_func(Foo *_Nonnull foo) {"
                       , "   if (foo->v != nullptr && my_bool_func((int *_Nonnull)foo->v)) { return 123; }"
                       , "   return 0;"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a provably non-null assignment of an address occurs in the null branch" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   int i = 0;"
                       , "   if (p == nullptr) { p = &i; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a provably non-null assignment of an array occurs in the null branch" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   uint8_t my_array[10];"
                       , "   if (p == nullptr) { p = (int *)my_array; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a provably non-null assignment of a non-null pointer occurs in the null branch" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p, int *_Nonnull q) {"
                       , "   if (p == nullptr) { p = q; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using continue as a terminating statement" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   while (1) {"
                       , "     if (p == nullptr) { continue; }"
                       , "     my_other_func((int *_Nonnull)p);"
                       , "   }"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using break as a terminating statement" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   while (1) {"
                       , "     if (p == nullptr) { break; }"
                       , "     my_other_func((int *_Nonnull)p);"
                       , "   }"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using goto as a terminating statement" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p == nullptr) { goto FINAL; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "   FINAL: p = nullptr;"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using abort() as a terminating function" $ do
            let code = [ "void abort(void);"
                       , "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p == nullptr) { abort(); }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using LOGGER_FATAL as a terminating macro" $ do
            let code = [ "#define LOGGER_FATAL(log, ...) do { abort(); } while(0)"
                       , "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p == nullptr) { LOGGER_FATAL(nullptr, \"error\"); }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using assert() as a null check" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   assert(p != nullptr);"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when using LOGGER_ASSERT() as a null check" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   LOGGER_ASSERT(nullptr, p != nullptr, \"error\");"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code
