{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.LinterSpec
    ( mustParse
    , spec
    , renderPlain
    , check
    ) where

import           Test.Hspec                  (Spec, it, shouldBe)

import qualified Data.Map.Strict             as Map
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


renderPlain :: [LinterError] -> [Text.Text]
renderPlain = map (Text.stripEnd . TL.toStrict . renderLazy . layoutSmart defaultLayoutOptions . unAnnotate) . renderPure mempty


check :: [Text.Text] -> [String] -> [Text.Text]
check warnings code =
    case mustParse code of
        Nothing -> error "Failed to parse"
        Just ast ->
            let errors = analyse warnings ast
                cache = Map.singleton "test.c" (map Text.pack code)
            in map (Text.stripEnd . TL.toStrict . renderLazy . layoutSmart defaultLayoutOptions . unAnnotate) $ renderPure cache errors


mustParse :: MonadFail m => [String] -> m CTranslUnit
mustParse code =
    let is = inputStreamFromString $ unlines code in
    case parseC is startPos of
        Left err -> fail $ show err
        Right ok -> return ok


spec :: Spec
spec = do
    it "should parse a simple function" $ do
        check allWarnings ["int a(void) { return 3; }"] `shouldBe` []

    it "should give diagnostics on invalid symbol redeclaration" $ do
        let code =
                [ "typedef struct Foo { char x; } Foo;"
                , "typedef enum Foo { FOO_ONE } Foo;"
                ]
        check allWarnings code
            `shouldBe`
            [ Text.stripEnd $ Text.unlines
                [ "error: test.c:2: (column 9) [ERROR]  >>> Foo redefined"
                , "         Foo previously declared as a different kind of symbol"
                , "         The previous declaration was here:"
                , "         (\"test.c\": line 1)"
                , "   --> test.c:2:9"
                , "    |"
                , "2   | typedef enum Foo { FOO_ONE } Foo;"
                , "    |         ^"
                , "    |"
                ]
            ]
