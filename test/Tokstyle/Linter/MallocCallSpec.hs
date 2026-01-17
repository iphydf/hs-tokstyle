{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MallocCallSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["malloc-call"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["malloc-call"]


spec :: Spec
spec = do
    it "warns when mem_alloc() is used outside a var decl stmt" $ do
        shouldWarn'
            [ "int a(My_Struct **v) {"
            , "  *v = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "}"
            ]
            [[ "warning: allocations using `mem_alloc` must first be assigned to a local variable or returned directly [-Wmalloc-call]"
             , "   --> test.c:2:21"
             , "    |"
             , "   2|   *v = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
             , "    |                     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns if there is no null-check after an allocation" $ do
        shouldWarn'
            [ "void a(My_Struct **v) {"
            , "  My_Struct *tmp = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "  *v = tmp;"
            , "}"
            ]
            [[ "warning: `tmp`, assigned from `mem_alloc` must immediately be checked against `nullptr` [-Wmalloc-call]"
             , "   --> test.c:3:4"
             , "    |"
             , "   3|   *v = tmp;"
             , "    |    ^^^^^^^"
             ]]

    it "accepts mem_alloc() being used in a var decl stmt" $ do
        shouldAccept'
            [ "bool a(My_Struct **v) {"
            , "  My_Struct *tmp = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "  if (tmp == nullptr) {"
            , "    return false;"
            , "  }"
            , "  *v = tmp;"
            , "  return true;"
            , "}"
            ]

    it "accepts mem_alloc() being used in a return statement" $ do
        shouldAccept'
            [ "My_Struct *my_struct_new(void) {"
            , "  return (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "}"
            ]
