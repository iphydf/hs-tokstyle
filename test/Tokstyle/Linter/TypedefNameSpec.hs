{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TypedefNameSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["typedef-name"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["typedef-name"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid typedef names" $ do
        shouldAccept'
            [ "typedef struct Foo Foo;"
            , "typedef struct Bar_s Bar_t;"
            ]

    it "should give diagnostics on invalid typedef names" $ do
        shouldWarn' [ "typedef struct Foo_s Bar_t;" ]
            [[ "warning: typedef name `Bar_t` does not match struct name `Foo_s` [-Wtypedef-name]"
             , "   --> test.c:1:16"
             , "    |"
             , "   1| typedef struct Foo_s Bar_t;"
             , "    |                ^^^^^"
             ]]

    it "should not give diagnostics on valid union names" $ do
        shouldAccept'
            [ "typedef union Foo { int a; } Foo;"
            , "typedef union Bar_u { int a; } Bar_t;"
            ]

    it "should give diagnostics on invalid union names" $ do
        shouldWarn' [ "typedef union Foo_u { int a; } Bar_t;" ]
            [[ "warning: typedef name `Bar_t` does not match union name `Foo_u` [-Wtypedef-name]"
             , "   --> test.c:1:15"
             , "    |"
             , "   1| typedef union Foo_u { int a; } Bar_t;"
             , "    |               ^^^^^"
             ]]

    it "should not give diagnostics on valid enum names" $ do
        shouldAccept'
            [ "typedef enum Foo { FOO } Foo;"
            , "typedef enum Bar_e { BAR } Bar_t;"
            ]

    it "should give diagnostics on invalid enum names" $ do
        shouldWarn' [ "typedef enum Foo_e { FOO } Bar_t;" ]
            [[ "warning: typedef name `Bar_t` does not match enum name `Foo_e` [-Wtypedef-name]"
             , "   --> test.c:1:14"
             , "    |"
             , "   1| typedef enum Foo_e { FOO } Bar_t;"
             , "    |              ^^^^^"
             ]]
