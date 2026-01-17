{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.VarUnusedInScopeSpec (spec) where

import           Data.Text           (Text, unlines)
import           Prelude             hiding (unlines)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["var-unused-in-scope"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["var-unused-in-scope"]


spec :: Spec
spec = do
    it "detects vars declared outside an if-statement that could be declared inside it" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int foo = 0;"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]
            [[ "warning: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int foo = 0;"
             , "    |       ^^^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:15"
             , "    |"
             , "   4|     print_int(foo);"
             , "    |               ^^^"
             ]]

    it "ignores variables that escape the inner scope through pointers" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int foo;"
            , "  int *foo_ptr;"
            , "  if (true) {"
            , "    foo_ptr = &foo;"
            , "  }"
            , "  print_int(*foo_ptr);"
            , "}"
            ]

    it "ignores array-typed variables that escape the inner scope through assignment" $ do
        shouldAccept'
            [ "int a(char *p) {"
            , "  char foo[3] = {0};"
            , "  if (p == nullptr) {"
            , "    p = foo;"
            -- ^^^ We don't know that `foo` here is actually `&foo[0]`.
            , "  }"
            , "  print(p);"
            , "}"
            ]

    it "ignores array-typed variables assigned in a loop" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int foo[2] = {0, 0};"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (foo[0] == foo[1]) {"
            , "      print_int(foo[0]);"
            , "    }"
            , "    foo[0] = i % 2;"
            , "    foo[1] = i % 3;"
            , "  }"
            , "}"
            ]

    it "keeps conditional variable initialisation out of the `if` statement if it's used after" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int foo;"
            , "  if (true) {"
            , "    foo = 1;"
            , "  } else {"
            , "    foo = 2;"
            , "  }"
            , "  print_int(foo);"
            , "  for (int i = 0; i < foo; ++i) { /* nothing */ }"
            , "}"
            ]

    it "does not suggest moving complex initialisations into an if-statement" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int foo = maybe_side_effect();"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]

    it "suggests moving trivial (pure) initialisations into an if-statement" $ do
        shouldWarn'
            [ "int f(int a, int b) {"
            , "  int foo = 3 + 4;"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]
            [[ "warning: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int foo = 3 + 4;"
             , "    |       ^^^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:15"
             , "    |"
             , "   4|     print_int(foo);"
             , "    |               ^^^"
             ]]

    it "does not suggest moving loop variables into a `while` statement" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int i = 0;"
            , "  while (true) {"
            , "    ++i;"
            , "    if (i > 10) {"
            , "      break;"
            , "    }"
            , "  }"
            , "}"
            ]

    it "detects decls that can be for-init-decls" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:3:8"
             , "    |"
             , "   3|   for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |        ^"
             ]]

    it "detects reducible for-stmts followed by irreducible for-stmts" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { /* nothing */ }"
            , "  for (int i = 0; i < 10; ++i) { /* nothing */ }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:3:8"
             , "    |"
             , "   3|   for (i = 0; i < 10; ++i) { /* nothing */ }"
             , "    |        ^"
             ]]

    it "leaves already correct for-init-decls alone" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  for (int i = 0; i < 10; ++i) { print_int(i); }"
            , "}"
            ]

    it "does not suggest reducing scope of loop bound constants" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  const int bound = 10;"
            , "  for (int i = 0; i < bound; ++i) { print_int(i); }"
            , "}"
            ]

    it "considers `&var` a write to `var`" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int foo;"
            , "  for (int i = start(&foo); !stop(foo); i = incr(&foo)) { print_ints(i, foo); }"
            , "}"
            ]

    it "ignores function parameters" $ do
        shouldAccept'
            [ "int a(int i) {"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]

    it "supports #if" $ do
        shouldWarn'
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif /* HAHA */"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:3:7"
             , "    |"
             , "   3|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:8"
             , "    |"
             , "   4|   for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |        ^"
             ]]

    it "supports #if/#else/#endif" $ do
        shouldWarn'
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif /* HAHA */"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:3:7"
             , "    |"
             , "   3|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:8"
             , "    |"
             , "   4|   for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |        ^"
             ]]

    it "detects when the first #if branch is ok while the second isn't" $ do
        shouldWarn'
            [ "int a(void) {"
            , "#if HAHA"
            , "  for (int i = 0; i < 10; ++i) { /* nothing */ }"
            , "#else"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { /* nothing */ }"
            , "#endif /* HAHA */"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:5:7"
             , "    |"
             , "   5|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:6:8"
             , "    |"
             , "   6|   for (i = 0; i < 10; ++i) { /* nothing */ }"
             , "    |        ^"
             ]]

    it "detects when the second #if branch is ok while the first isn't" $ do
        shouldWarn'
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  for (int i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif /* HAHA */"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:3:7"
             , "    |"
             , "   3|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:8"
             , "    |"
             , "   4|   for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |        ^"
             ]]

    it "detects multiple uses, as long as all of them are writes" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:3:8"
             , "    |"
             , "   3|   for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |        ^"
             ]]

    it "should work on variables declared multiple scopes up" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "    print_int(i);"
            , "  }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:10"
             , "    |"
             , "   4|     for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |          ^"
             ]]

    it "detects variables only-written in both if branches" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:6:10"
             , "    |"
             , "   6|     for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |          ^"
             ]]

    it "detects variables only-written in multiple (more than 2) if/else branches" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int i;"
             , "    |       ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:8:10"
             , "    |"
             , "   8|     for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |          ^"
             ]]

    it "should not suggest reducing scope if the variable is read in an if-condition" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else if (i > 5) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]

    it "allows vars read in the same scope" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
            , "  print_int(i);"
            , "}"
            ]

    it "allows vars used as the bound for another for-loop" $ do
        shouldAccept'
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
            , "  for (int j = 0; j < i; ++j) { puts(\"hello!\"); }"
            , "}"
            ]

    it "treats array member assignment as read" $ do
        shouldAccept'
            [ "int a(char *p) {"
            , "  char *c = p;"
            , "  if (true) { c[0] = 'a'; }"
            , "}"
            ]

    it "treats dereference-and-assign as read" $ do
        shouldAccept'
            [ "int a(char *p) {"
            , "  char *c = p;"
            , "  if (true) { *c = 'a'; }"
            , "}"
            ]

    it "should consider one `if` branch with a write as possibly not writing" $ do
        shouldAccept'
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]

    it "should combine 'either write or read' into 'just read'" $ do
        shouldAccept'
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    } else {"
            , "      print_int(foo);"
            , "    }"
            , "    print_int(foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]

    it "suggests reducing scope when all if-branches do writes" $ do
        shouldWarn'
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    } else if (i >= 3) {"
            , "      foo = 2;"
            , "    } else {"
            , "      foo = 3;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
            -- This suggestion is not quite correct, but we have nothing to anchor the "possibly to
            -- here" part to, so we make a slightly wrong suggestion. Hence the "possibly". In
            -- reality, the declaration should be right before the first `if` on line 4.
            [[ "warning: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int foo = 1;"
             , "    |       ^^^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:9:7"
             , "    |"
             , "   9|       foo = 3;"
             , "    |       ^^^"
             ]]

    it "detects vars that can always be declared inside the for-loop" $ do
        shouldWarn'
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    foo = 2;" -- any writes to foo will be overwritten in the next loop iteration
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
            [[ "warning: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   int foo = 1;"
             , "    |       ^^^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:4:5"
             , "    |"
             , "   4|     foo = 2;"
             , "    |     ^^^"
             ]]

    it "detects same-named variables where only one branch needs a diagnostic" $ do
        shouldWarn'
            [ "int a(void) {"
            , "  if (true) {"
            , "    for (int i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    int i;"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
            [[ "warning: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
             , "   --> test.c:5:9"
             , "    |"
             , "   5|     int i;"
             , "    |         ^"
             ]
            ,[ "warning:   possibly to here [-Wvar-unused-in-scope]"
             , "   --> test.c:6:10"
             , "    |"
             , "   6|     for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
             , "    |          ^"
             ]]
