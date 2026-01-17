{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.SwitchIfSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["switch-if"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["switch-if"]


spec :: Spec
spec = do
    it "accepts a single if/else" $ do
        shouldAccept'
            [ "bool a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return true;"
            , "  } else {"
            , "    return false;"
            , "  }"
            , "}"
            ]

    it "accepts a if/else with only 2 comparisons" $ do
        shouldAccept'
            [ "bool a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return true;"
            , "  } else if (b == THE_BAR) {"
            , "    print_int(b);"
            , "    return true;"
            , "  } else {"
            , "    return false;"
            , "  }"
            , "}"
            ]

    it "ignores candidates where all branches are single statements" $ do
        shouldAccept'
            [ "int a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    return 0;"
            , "  } else if (b == THE_BAR) {"
            , "    return 1;"
            , "  } else if (b == THE_BAZ) {"
            , "    return 2;"
            , "  }"
            , "}"
            ]

    it "diagnoses a series of if/else-if statements as candidate for switch" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return 0;"
            , "  } else if (b == THE_BAR) {"
            , "    return 1;"
            , "  } else if (b == THE_BAZ) {"
            , "    return 2;"
            , "  }"
            , "}"
            ]
            [[ "warning: if-statement could be a switch [-Wswitch-if]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   if (b == THE_FOO) {"
             , "    |       ^"
             ]]

    it "diagnoses a series of if/else-if statements ending in `else` as candidate for switch" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return 0;"
            , "  } else if (b == THE_BAR) {"
            , "    return 1;"
            , "  } else if (b == THE_BAZ) {"
            , "    return 2;"
            , "  } else {"
            , "    return 3;"
            , "  }"
            , "}"
            ]
            [[ "warning: if-statement could be a switch [-Wswitch-if]"
             , "   --> test.c:2:7"
             , "    |"
             , "   2|   if (b == THE_FOO) {"
             , "    |       ^"
             ]]

    it "diagnoses a candidates for switch nested inside another `if`" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  if (b != something) {"
            , "    if (b == THE_FOO) {"
            , "      print_int(b);"
            , "      return 0;"
            , "    } else if (b == THE_BAR) {"
            , "      return 1;"
            , "    } else if (b == THE_BAZ) {"
            , "      return 2;"
            , "    } else {"
            , "      return 3;"
            , "    }"
            , "  }"
            , "}"
            ]
            [[ "warning: if-statement could be a switch [-Wswitch-if]"
             , "   --> test.c:3:9"
             , "    |"
             , "   3|     if (b == THE_FOO) {"
             , "    |         ^"
             ]]

    it "diagnoses a candidates for switch nested inside an `else if`" $ do
        shouldWarn'
            [ "int a(int b) {"
            , "  if (b != something) {"
            , "    /* nop */"
            , "  } else if (b != another_thing) {"
            , "    if (b == THE_FOO) {"
            , "      print_int(b);"
            , "      return 0;"
            , "    } else if (b == THE_BAR) {"
            , "      return 1;"
            , "    } else if (b == THE_BAZ) {"
            , "      return 2;"
            , "    } else {"
            , "      return 3;"
            , "    }"
            , "  }"
            , "}"
            ]
            [[ "warning: if-statement could be a switch [-Wswitch-if]"
             , "   --> test.c:5:9"
             , "    |"
             , "   5|     if (b == THE_FOO) {"
             , "    |         ^"
             ]]

    it "ignores if/else-if statements with different comparison targets" $ do
        shouldAccept'
            [ "int a(int b, int c) {"
            , "  if (b == THE_FOO) {"
            , "    print_int(b);"
            , "    return 0;"
            , "  } else if (c == THE_BAR) {"
            , "    return 1;"
            , "  } else if (c == THE_BAZ) {"
            , "    return 2;"
            , "  } else {"
            , "    return 2;"
            , "  }"
            , "}"
            ]
