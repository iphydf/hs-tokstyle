{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.LinterSpec
    ( mustParse
    , mustParseExpr
    , mustParseStmt
    , checkLocal
    , checkGlobal
    , shouldWarn
    , shouldAccept
    , shouldWarnLocal
    , shouldAcceptLocal
    , shouldProduce
    , check
    , testC
    , spec
    ) where

import           GHC.Stack                   (HasCallStack)
import           Test.Hspec                  (Expectation, Spec,
                                              expectationFailure, it, shouldBe)

import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text, intercalate, unlines)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as TL
import           Language.Cimple             (Lexeme, Node)
import           Language.Cimple.Diagnostics (CimplePos, Diagnostic, renderPure)
import           Language.Cimple.IO          (parseExpr, parseStmt, parseText)
import           Prelude                     hiding (unlines)
import           Prettyprinter               (defaultLayoutOptions, layoutSmart,
                                              unAnnotate)
import qualified Prettyprinter.Render.Text   as PP.Text
import           Tokstyle.Linter             (allWarnings, analyse)


renderPlain :: Map FilePath [Text] -> [Diagnostic CimplePos] -> [Text]
renderPlain cache = map (Text.stripEnd . TL.toStrict . PP.Text.renderLazy . layoutSmart defaultLayoutOptions . unAnnotate) . renderPure cache


shouldProduce :: HasCallStack => [Text] -> [Text] -> Expectation
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


check :: [Text] -> [(FilePath, [Text])] -> [Text]
check warnings inputs =
    let tus = map (\(f, c) -> (f, case parseText $ Text.unlines c of Left err -> error err; Right ast -> ast)) inputs
        cache = Map.fromList inputs
    in renderPlain cache $ analyse warnings tus


checkLocal :: [Text] -> [Text] -> [Text]
checkLocal warnings code = check warnings (testC code)


checkGlobal :: [Text] -> [(FilePath, [Text])] -> [Text]
checkGlobal = check


testC :: [Text] -> [(FilePath, [Text])]
testC code = [("test.c", code)]


shouldWarn :: HasCallStack => [Text] -> [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn activeWarnings inputs expected =
    check activeWarnings inputs `shouldProduce` map (intercalate "\n") expected


shouldAccept :: HasCallStack => [Text] -> [(FilePath, [Text])] -> IO ()
shouldAccept activeWarnings inputs = shouldWarn activeWarnings inputs []


shouldWarnLocal :: HasCallStack => [Text] -> [Text] -> [[Text]] -> IO ()
shouldWarnLocal activeWarnings code = shouldWarn activeWarnings (testC code)


shouldAcceptLocal :: HasCallStack => [Text] -> [Text] -> IO ()
shouldAcceptLocal activeWarnings code = shouldWarnLocal activeWarnings code []


mustParse :: MonadFail m => [Text] -> m [Node (Lexeme Text)]
mustParse code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok

mustParseExpr :: MonadFail m => [Text] -> m (Node (Lexeme Text))
mustParseExpr code =
    case parseExpr $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok

mustParseStmt :: MonadFail m => [Text] -> m (Node (Lexeme Text))
mustParseStmt code =
    case parseStmt $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok


spec :: Spec
spec = do
    it "should parse a simple function" $ do
        shouldAcceptLocal ["global-funcs"] ["int a(void) { return 3; }"]

    it "should give diagnostics on extern decls in .c files" $ do
        shouldWarnLocal ["global-funcs"] ["int a(void);"]
            [[ "warning: global function `a` declared in .c file [-Wglobal-funcs]"
                , "   --> test.c:1:5"
                , "    |"
                , "   1| int a(void);"
                , "    |     ^"
                ]]

    it "should not give diagnostics on extern decls in .h files" $ do
        -- We cannot use shouldAcceptLocal here because it uses "test.c" internally.
        shouldAccept ["global-funcs"] [("test.h", ["int a(void);"])]
