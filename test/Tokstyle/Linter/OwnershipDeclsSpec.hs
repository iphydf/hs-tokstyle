{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.OwnershipDeclsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["ownership-decls"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["ownership-decls"]


spec :: Spec
spec = do
    it "allows qualifiers on declarations" $ do
        shouldAccept'
            [ "int * _Owner foo(void);"
            , "int * _Nullable bar(void);"
            , "int * _Nonnull baz(void);"
            ]

    it "allows qualifiers on parameters in declarations" $ do
        shouldAccept'
            [ "void foo(int * _Owner p);"
            , "void bar(int * _Nullable p);"
            , "void baz(int * _Nonnull p);"
            ]

    it "forbids qualifiers on global definitions" $ do
        shouldWarn' [ "int * _Owner foo(void) { return 0; }" ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:1:14"
             , "    |"
             , "   1| int * _Owner foo(void) { return 0; }"
             , "    |              ^^^"
             ]]

    it "reports multiple qualifiers when present" $ do
        shouldWarn' [ "int * _Owner _Nullable foo(void) { return 0; }" ]
            [[ "warning: qualifiers `_Owner` and `_Nullable` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:1:24"
             , "    |"
             , "   1| int * _Owner _Nullable foo(void) { return 0; }"
             , "    |                        ^^^"
             ]]

    it "forbids qualifiers on global definitions even if declared before" $ do
        shouldWarn'
            [ "int * _Owner foo(void);"
            , "int * _Owner foo(void) { return 0; }"
            ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:2:14"
             , "    |"
             , "   2| int * _Owner foo(void) { return 0; }"
             , "    |              ^^^"
             ]]

    it "forbids qualifiers on static definitions if declared before" $ do
        shouldWarn'
            [ "static int * _Owner foo(void);"
            , "static int * _Owner foo(void) { return 0; }"
            ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:2:21"
             , "    |"
             , "   2| static int * _Owner foo(void) { return 0; }"
             , "    |                     ^^^"
             ]]

    it "allows qualifiers on static definitions if NOT declared before" $ do
        shouldAccept' [ "static int * _Owner foo(void) { return 0; }" ]

    it "forbids qualifiers on parameters in definitions" $ do
        shouldWarn' [ "void foo(int * _Owner p) { return; }" ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:1:6"
             , "    |"
             , "   1| void foo(int * _Owner p) { return; }"
             , "    |      ^^^"
             ]]

-- end of tests
