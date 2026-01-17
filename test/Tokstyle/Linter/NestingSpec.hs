{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.NestingSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["nesting"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["nesting"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid nesting" $ do
        shouldAccept'
            [ "void foo(void) {"
            , "  if (1) {"
            , "    if (2) {"
            , "      if (3) {"
            , "        if (4) {"
            , "          if (5) {"
            , "            if (6) {"
            , "              return;"
            , "            }"
            , "          }"
            , "        }"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]

    it "should give diagnostics on invalid nesting" $ do
        shouldWarn'
            [ "void foo(void) {"
            , "  if (1) {"
            , "    if (2) {"
            , "      if (3) {"
            , "        if (4) {"
            , "          if (5) {"
            , "            if (6) {"
            , "              if (7) {"
            , "                return;"
            , "              }"
            , "            }"
            , "          }"
            , "        }"
            , "      }"
            , "    }"
            , "  }"
            , "}"
            ]
            [[ "warning: function is too deeply nested: 8 is deeper than the maximum allowed of 7; consider inversion or extraction [-Wnesting]"
             , "   --> test.c:1:1"
             , "    |"
             , "   1| void foo(void) {"
             , "    | ^^^^^^^^^^^^^^^^"
             ]]
