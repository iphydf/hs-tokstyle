{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SizeofSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["sizeof"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["sizeof"]


spec :: Spec
spec = do
    it "should give diagnostics on passing pointers to sizeof" $ do
        shouldWarn'
            [ "int foo(void) {"
            , "  char *ptr;"
            , "  return sizeof ptr;"
            , "}"
            ]
            [[ "error: disallowed sizeof argument of type `char *` - did you mean for `ptr` to be an array? [-Wsizeof]"
             , "   --> test.c:3:17"
             , "    |"
             , "   3|   return sizeof ptr;"
             , "    |                 ^^^"
             ]]

    it "should give diagnostics on passing array-element pointers to sizeof" $ do
        shouldWarn'
            [ "int foo(void) {"
            , "  char arr[10];"
            , "  return sizeof &arr[0];"
            , "}"
            ]
            [[ "error: disallowed sizeof argument of type `char *` - did you mean for `&arr[0]` to be an array? [-Wsizeof]"
             , "   --> test.c:3:17"
             , "    |"
             , "   3|   return sizeof &arr[0];"
             , "    |                 ^^^^^^^"
             ]]

    it "should not diagnostics on passing arrays to sizeof" $ do
        shouldAccept'
            [ "int foo(void) {"
            , "  char arr[10];"
            , "  return sizeof arr;"
            , "}"
            ]
