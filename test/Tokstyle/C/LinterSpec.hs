{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.LinterSpec
    ( mustParse
    , spec
    , renderPlain
    , check
    , shouldProduce
    , shouldWarn
    , shouldAccept
    ) where

import           Test.Hspec                  (Expectation, Spec,
                                              expectationFailure, it, shouldBe)

import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text, intercalate)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as TL
import           Language.C                  (CTranslUnit)
import           Language.C.Data.InputStream (inputStreamFromString)
import           Language.C.Data.Position    (Position, position)
import           Language.C.Parser           (parseC)
import           Prettyprinter               (defaultLayoutOptions, layoutSmart,
                                              unAnnotate)
import           Prettyprinter.Render.Text   (renderLazy)
import           Tokstyle.C.Linter           (AnsiStyle, Doc, LinterError,
                                              allWarnings, analyse, renderPure)


startPos :: Position
startPos = position 0 "test.c" 1 0 Nothing


renderPlain :: [LinterError] -> [Text]
renderPlain = map (Text.stripEnd . TL.toStrict . renderLazy . layoutSmart defaultLayoutOptions . unAnnotate) . renderPure mempty


shouldProduce :: [Text] -> [Text] -> Expectation
shouldProduce actual expected =
    if actual == expected
    then return ()
    else expectationFailure $ Text.unpack $ Text.unlines
        [ ""
        , "Expected:"
        , "--------------------------------------------------------------------------------"
        , Text.intercalate "\n---\n" expected
        , "--------------------------------------------------------------------------------"
        , ""
        , "But got:"
        , "--------------------------------------------------------------------------------"
        , Text.intercalate "\n---\n" actual
        , "--------------------------------------------------------------------------------"
        ]


check :: [Text] -> [Text] -> [Text]
check warnings code =
    case mustParse code of
        Nothing -> error "Failed to parse"
        Just ast ->
            let errors = analyse warnings ast
                cache = Map.singleton "test.c" code
            in map (Text.stripEnd . TL.toStrict . renderLazy . layoutSmart defaultLayoutOptions . unAnnotate) $ renderPure cache errors


shouldWarn :: [Text] -> [Text] -> [[Text]] -> IO ()
shouldWarn activeWarnings code expected =
    check activeWarnings code `shouldProduce` map (intercalate "\n") expected


shouldAccept :: [Text] -> [Text] -> IO ()
shouldAccept activeWarnings code = shouldWarn activeWarnings code []


mustParse :: MonadFail m => [Text] -> m CTranslUnit
mustParse code =
    let is = inputStreamFromString $ Text.unpack $ Text.unlines code in
    case parseC is startPos of
        Left err -> fail $ show err
        Right ok -> return ok


spec :: Spec
spec = do
    it "should parse a simple function" $ do
        shouldAccept allWarnings ["int a(void) { return 3; }"]

    it "should parse custom ownership attributes" $ do
        shouldWarn allWarnings ["int f(char *__attribute__ ((__owned__)) p) { return 3; }"]
            [[ "error: memory leak: p [-Wborrow-check]"
                , "   --> test.c:1:40"
                , "    |"
                , "   1| int f(char *__attribute__ ((__owned__)) p) { return 3; }"
                , "    |                                        ^"
                ]]
        shouldAccept allWarnings ["int f(char *__attribute__ ((__owned__)) *p) { return 3; }"]
        shouldWarn allWarnings ["int f(char *__attribute__ ((__owned__)) *__attribute__ ((__owned__)) p) { return 3; }"]
            [[ "error: memory leak: p [-Wborrow-check]"
                , "   --> test.c:1:69"
                , "    |"
                , "   1| int f(char *__attribute__ ((__owned__)) *__attribute__ ((__owned__)) p) { return 3; }"
                , "    |                                                                     ^"
                ]]

    it "should give diagnostics on invalid symbol redeclaration" $ do
        let code =
                [ "typedef struct Foo { char x; } Foo;"
                , "typedef enum Foo { FOO_ONE } Foo;"
                ]
        shouldWarn allWarnings code
            [[ "error: test.c:2: (column 9) [ERROR]  >>> Foo redefined"
                , "         Foo previously declared as a different kind of symbol"
                , "         The previous declaration was here:"
                , "         (\"test.c\": line 1)"
                , "   --> test.c:2:9"
                , "    |"
                , "   2| typedef enum Foo { FOO_ONE } Foo;"
                , "    |         ^"
                ]]
