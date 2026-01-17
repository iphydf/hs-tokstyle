{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DeclsHaveDefnsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["decls-have-defns"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["decls-have-defns"]


spec :: Spec
spec = do
    it "should not give diagnostics on valid decl/defn pairs" $ do
        shouldAccept'
            [ ("test1.h", [ "int some_function(void);"
                          , "typedef struct Some_Struct Some_Struct;"
                          ])
            , ("test2.c", [ "int some_function(void) { return 0; }"
                          , "struct Some_Struct { int field; };"
                          ])
            ]

    it "should give diagnostics on missing function definition" $ do
        shouldWarn' [("test.h", [ "void some_other_function(void);" ])]
            [[ "warning: missing definition for `some_other_function` [-Wdecls-have-defns]"
             , "   --> test.h:1:6"
             , "    |"
             , "   1| void some_other_function(void);"
             , "    |      ^^^^^^^^^^^^^^^^^^^"
             ]]

    it "should give diagnostics on missing struct definition" $ do
        shouldWarn' [("test.h", [ "typedef struct Some_Other_Struct Some_Other_Struct;" ])]
            [[ "warning: missing definition for `Some_Other_Struct` [-Wdecls-have-defns]"
             , "   --> test.h:1:16"
             , "    |"
             , "   1| typedef struct Some_Other_Struct Some_Other_Struct;"
             , "    |                ^^^^^^^^^^^^^^^^^"
             ]]

    it "should give suggestion for similar names" $ do
        shouldWarn'
            [ ("test1.h", [ "int my_long_function_name(void);" ])
            , ("test2.c", [ "int my_long_functoin_name(void) { return 0; }" ])
            ]
            [[ "warning: missing definition for `my_long_function_name` [-Wdecls-have-defns]"
             , "   --> test1.h:1:5"
             , "    |"
             , "   1| int my_long_function_name(void);"
             , "    |     ^^^^^^^^^^^^^^^^^^^^^"
             ]
            ,[ "note: did you mean `my_long_functoin_name`? [-Wdecls-have-defns]"
             , "   --> test2.c:1:5"
             , "    |"
             , "   1| int my_long_functoin_name(void) { return 0; }"
             , "    |     ^^^^^^^^^^^^^^^^^^^^^"
             ]]
