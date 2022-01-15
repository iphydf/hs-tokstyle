{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.DeadStoreSpec where

import           Test.Hspec         (Spec, describe, it, shouldBe)

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Language.Cimple    (Lexeme, Node)
import           Language.Cimple.IO (parseText)
import           Tokstyle.Linter    (analyse)


mustParse :: MonadFail m => [Text] -> m [Node (Lexeme Text)]
mustParse code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok


spec :: Spec
spec =
    describe "analyse" $ do
        it "should give diagnostics on assignments that are overwritten before they are read" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  i = 1;"
                , "  i = 2;"
                , "  print_int(i);"
                , "}"
                ]
            analyse ("test.c", ast)
                `shouldBe`
                [ "test.c:3: value assigned to `i' is never read"
                , "test.c:2: variable `i' can be reduced in scope"
                , "test.c:3:   possibly to here"
                ]

        it "should give diagnostics on assignments that are never read" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i = 1;"
                , "}"
                ]
            analyse ("test.c", ast)
                `shouldBe`
                [ "test.c:3: value assigned to `i' is never read"
                , "test.c:2: variable `i' can be reduced in scope"
                , "test.c:3:   possibly to here"
                ]
