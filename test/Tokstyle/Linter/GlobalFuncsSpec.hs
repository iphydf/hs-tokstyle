{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.GlobalFuncsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldAcceptLocal,
                                      shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["global-funcs"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["global-funcs"]


spec :: Spec
spec = do
    it "should give diagnostics on global function declaration in .c file" $ do
        shouldWarn' [ "void foo(void);" ]
            [[ "warning: global function `foo` declared in .c file [-Wglobal-funcs]"
             , "   --> test.c:1:6"
             , "    |"
             , "   1| void foo(void);"
             , "    |      ^^^"
             ]]

    it "should not give diagnostics on global function declaration in .h file" $ do
        -- We cannot use shouldAccept' here because it uses "test.c" internally.
        shouldAccept ["global-funcs"] [("test.h", [ "void foo(void);" ])]

    it "should not give diagnostics on static function declaration in .c file" $ do
        shouldAccept' [ "static void foo(void);" ]

    it "should not give diagnostics on function definition in .c file" $ do
        shouldAccept' [ "int foo(void) { return 0; }" ]
