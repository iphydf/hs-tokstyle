{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NullabilitySpec (spec) where

import           Data.Text           (Text, unlines)
import qualified Data.Text           as Text
import           GHC.Stack           (HasCallStack)
import           Prelude             hiding (unlines)
import           Test.Hspec          (Spec, describe, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldAcceptLocal,
                                      shouldWarn, shouldWarnLocal)


shouldWarn' :: HasCallStack => [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["nullability"]


shouldAccept' :: HasCallStack => [Text] -> IO ()
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

        it "warns when a nullable pointer is returned from a nonnull function" $ do
            let code = [ "int *_Nonnull my_func(int *_Nullable p) {"
                       , "  return p;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this return [-Wnullability]"
                 , "   --> test.c:2:10"
                 , "    |"
                 , "   2|   return p;"
                 , "    |          ^"
                 ]]

        it "warns when a nullable pointer is assigned to a nonnull variable" $ do
            let code = [ "void my_func(int *_Nullable p) {"
                       , "  int *_Nonnull q = p;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this assignment [-Wnullability]"
                 , "   --> test.c:2:21"
                 , "    |"
                 , "   2|   int *_Nonnull q = p;"
                 , "    |                     ^"
                 ]]

        it "warns when a nullable global is used without check" $ do
            let code = [ "extern const int *const _Nullable g_p;"
                       , "void my_func() {"
                       , "   int x = *g_p;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: pointer `g_p` is nullable and has not been checked before this dereference [-Wnullability]"
                 , "   --> test.c:3:13"
                 , "    |"
                 , "   3|    int x = *g_p;"
                 , "    |             ^^^"
                 ]]

        it "warns when nullptr is passed to a nonnull parameter" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func() {"
                       , "   my_other_func(nullptr);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `nullptr` is nullable and has not been checked before this call [-Wnullability]"
                 , "   --> test.c:3:18"
                 , "    |"
                 , "   3|    my_other_func(nullptr);"
                 , "    |                  ^^^^^^^"
                 ]]

        it "warns when a nullable pointer is passed to a nonnull function pointer parameter" $ do
            let code = [ "typedef void my_func_cb(int *_Nonnull p);"
                       , "void my_func(my_func_cb *f, int *_Nullable p) {"
                       , "   f(p);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this call [-Wnullability]"
                 , "   --> test.c:3:6"
                 , "    |"
                 , "   3|    f(p);"
                 , "    |      ^"
                 ]]

        it "warns when pointer arithmetic is performed on a nullable pointer" $ do
            let code = [ "void my_func(int *_Nullable p) {"
                       , "   int *q = p + 1;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: pointer `p` is nullable and has not been checked before this arithmetic [-Wnullability]"
                 , "   --> test.c:2:13"
                 , "    |"
                 , "   2|    int *q = p + 1;"
                 , "    |             ^"
                 ]]

        it "does not warn on integer arithmetic using variables initialized to 0" $ do
            let code = [ "void my_func() {"
                       , "   size_t i = 0;"
                       , "   size_t j = sizeof(float) - i;"
                       , "}"
                       ]
            shouldAccept' code

        it "tracks aliases and does not warn when a checked original is accessed via alias" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   int *q = p;"
                       , "   if (p != nullptr) {"
                       , "     my_other_func((int *_Nonnull)q);"
                       , "   }"
                       , "}"
                       ]
            shouldAccept' code

        it "warns when a ternary expression returns a nullable value assigned to non-null" $ do
            let code = [ "void my_func(int *_Nullable p, int *_Nonnull q, int cond) {"
                       , "   int *_Nonnull r = cond ? p : q;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `cond ? p : q` is nullable and has not been checked before this assignment [-Wnullability]"
                 , "   --> test.c:2:22"
                 , "    |"
                 , "   2|    int *_Nonnull r = cond ? p : q;"
                 , "    |                      ^^^^^^^^^^^^"
                 ]]

        it "warns when 0 is assigned to a non-null pointer" $ do
            let code = [ "void my_func() {"
                       , "   int *_Nonnull p = 0;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `0` is nullable and has not been checked before this assignment [-Wnullability]"
                 , "   --> test.c:2:22"
                 , "    |"
                 , "   2|    int *_Nonnull p = 0;"
                 , "    |                      ^"
                 ]]

        it "warns when a nullable pointer in an array is used without check" $ do
            let code = [ "void my_func(int *_Nullable arr[10]) {"
                       , "   int x = *arr[0];"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: pointer `arr[0]` is nullable and has not been checked before this dereference [-Wnullability]"
                 , "   --> test.c:2:13"
                 , "    |"
                 , "   2|    int x = *arr[0];"
                 , "    |             ^^^^^"
                 ]]

        it "treats unannotated parameters as non-null by default" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *p) {"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "accepts arithmetic on local arrays" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func() {"
                       , "   int arr[10];"
                       , "   my_other_func(arr + 1);"
                       , "}"
                       ]
            shouldAccept' code

        it "respects nullability annotations in declaration even if definition is unannotated" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p);"
                       , "void my_func(int *p) {"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                 , "   --> test.c:4:33"
                 , "    |"
                 , "   4|    my_other_func((int *_Nonnull)p);"
                 , "    |                                 ^"
                 ]]

        it "correctly handles calloc followed by a null check" $ do
            let code = [ "typedef struct Foo { int x; } Foo;"
                       , "void *calloc(unsigned long nmemb, unsigned long size);"
                       , "void my_func() {"
                       , "   Foo *f = (Foo *)calloc(1, sizeof(Foo));"
                       , "   if (f == nullptr) return;"
                       , "   f->x = 1;"
                       , "}"
                       ]
            shouldAccept' code

        it "terminates on msi_kill-like structure" $ do
            let code = [ "typedef struct MSICall { struct MSICall *next; struct MSISession *s; int f; } MSICall;"
                       , "typedef struct MSISession { MSICall **calls; int head; } MSISession;"
                       , "void kill_call(MSICall *call) {"
                       , "  if (call == 0) { return; }"
                       , "  MSISession *s = call->s;"
                       , "  MSICall *next = call->next;"
                       , "  if (next != 0) { s->head = next->f; }"
                       , "}"
                       , "void msi_kill(MSISession *session) {"
                       , "  if (session == 0) { return; }"
                       , "  if (session->calls != 0) {"
                       , "    MSICall *it = session->calls[0];"
                       , "    while (it != 0) {"
                       , "      MSICall *tmp = it;"
                       , "      it = it->next;"
                       , "      kill_call(tmp);"
                       , "    }"
                       , "  }"
                       , "}"
                       ]
            shouldAccept' code

    describe "dereferences" $ do
        it "warns when a nullable pointer is dereferenced without check" $ do
            let code = [ "void my_func(int *_Nullable p) {"
                       , "   int x = *p;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: pointer `p` is nullable and has not been checked before this dereference [-Wnullability]"
                 , "   --> test.c:2:13"
                 , "    |"
                 , "   2|    int x = *p;"
                 , "    |             ^"
                 ]]

        it "warns when a nullable pointer is accessed via -> without check" $ do
            let code = [ "typedef struct Foo { int x; } Foo;"
                       , "void my_func(Foo *_Nullable p) {"
                       , "   int x = p->x;"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: pointer `p` is nullable and has not been checked before this access [-Wnullability]"
                 , "   --> test.c:3:12"
                 , "    |"
                 , "   3|    int x = p->x;"
                 , "    |            ^"
                 ]]

        it "warns when a nullable pointer is accessed via array indexing without check" $ do
            let code = [ "void my_func(int *_Nullable p) {"
                       , "   int x = p[0];"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: pointer `p` is nullable and has not been checked before this access [-Wnullability]"
                 , "   --> test.c:2:12"
                 , "    |"
                 , "   2|    int x = p[0];"
                 , "    |            ^"
                 ]]

    describe "control flow" $ do
        it "warns when a nullable pointer is accessed after being conditionally checked but skipped via goto" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   if (p == nullptr) { goto SKIP; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "   SKIP: my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldWarn' code
                [[ "warning: expression `p` is nullable and has not been checked before this cast [-Wnullability]"
                 , "   --> test.c:5:39"
                 , "    |"
                 , "   5|    SKIP: my_other_func((int *_Nonnull)p);"
                 , "    |                                       ^"
                 ]]

        it "does not warn when a nullable pointer is checked in a loop and used after" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   while (p == nullptr) { return; }"
                       , "   my_other_func((int *_Nonnull)p);"
                       , "}"
                       ]
            shouldAccept' code

        it "does not warn when a nullable pointer is conditionally initialized in an #ifdef" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable p) {"
                       , "   int *q = nullptr;"
                       , "#ifdef FOO_BAR"
                       , "   q = p;"
                       , "#else"
                       , "   int i = 0; q = &i;"
                       , "#endif /* FOO_BAR */"
                       , "   if (p != nullptr) {"
                       , "     my_other_func((int *_Nonnull)q);"
                       , "   }"
                       , "}"
                       ]
            shouldAccept' code

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

        it "supports assignment and then nullness checks" $ do
            let code = [ "void my_other_func(int *_Nonnull p);"
                       , "void my_func(int *_Nullable q) {"
                       , "   int *p = q;"
                       , "   if (p != nullptr) { my_other_func((int *_Nonnull)p); }"
                       , "}"
                       ]
            shouldAccept' code

        it "terminates on a loop that would otherwise cause infinite symbolic growth" $ do
            let code = [ "void my_func(int cond, int *a, int *b) {"
                       , "   int *p = nullptr;"
                       , "   if (cond) {"
                       , "     while (1) {"
                       , "       if (cond) { p = a; } else { p = b; }"
                       , "       int x = *p;"
                       , "     }"
                       , "   }"
                       , "}"
                       ]
            shouldAccept' code

        it "supports nested ternary operators and correctly identifies nullability" $ do
            let code = [ "int *_Nonnull my_func(int *_Nullable p, int *_Nonnull q, int *_Nonnull r, int c1, int c2) {"
                       , "   return c1 ? (c2 ? q : r) : (p ? p : q);"
                       , "}"
                       ]
            -- c1 ? (c2 ? q : r) : (p ? p : q)
            -- if c1 is true, we get (c2 ? q : r), both q and r are non-null, so it's non-null.
            -- if c1 is false, we get (p ? p : q). If p is true, p is non-null. If p is false, q is non-null.
            -- So the whole thing is non-null.
            shouldAccept' code
