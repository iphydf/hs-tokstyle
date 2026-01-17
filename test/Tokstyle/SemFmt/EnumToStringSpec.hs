{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.EnumToStringSpec where
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Test.Hspec             (Spec, it)

import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           Tokstyle.LinterSpec    (mustParseStmt, shouldAccept,
                                         shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["enum-to-string"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["enum-to-string"]


testC :: [Text] -> [(FilePath, [Text])]
testC code = [("test.c", code)]


spec :: Spec
spec = do
    it "should give diagnostics on incorrect to_string function" $ do
        let code =
                [ "typedef enum Foo {"
                , "  FOO_ONE,"
                , "  FOO_TWO,"
                , "} Foo;"
                , "const char *foo_to_string(Foo value) {"
                , "  switch (value) {"
                , "    case FOO_ONE: return \"FOO_ONE\";"
                , "    case FOO_TWO: return \"FOO_ONE\";"
                , "  }"
                , "  return \"<invalid>\";"
                , "}"
                ]
        expectedLines <- Text.lines . render . plain . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "  switch (value) {"
            , "    case FOO_ONE: return \"FOO_ONE\";"
            , "    case FOO_TWO: return \"FOO_TWO\";"
            , "  }"
            , "  return \"<invalid Foo>\";"
            , "}"
            ]
        let indent l = if Text.null (Text.strip l) then "" else "         " <> l
        shouldWarn' (testC code)
            [[ "warning: enum `_to_string` function for `Foo` should be: [-Wenum-to-string]"
                , Text.intercalate "\n" $ map indent expectedLines
                , "   --> test.c:5:7"
                , "    |"
                , "   5| const char *foo_to_string(Foo value) {"
                , "    |       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                ]]
