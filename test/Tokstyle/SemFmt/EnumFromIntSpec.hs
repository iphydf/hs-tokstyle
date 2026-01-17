{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.EnumFromIntSpec where
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Test.Hspec             (Spec, it)

import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           Tokstyle.LinterSpec    (mustParseStmt, shouldAccept,
                                         shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["enum-from-int"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["enum-from-int"]


testC :: [Text] -> [(FilePath, [Text])]
testC code = [("test.c", code)]


spec :: Spec
spec = do
    it "should give diagnostics on incorrect from_int function" $ do
        let code =
                [ "typedef enum Foo {"
                , "  FOO_ONE,"
                , "  FOO_TWO,"
                , "} Foo;"
                , "bool foo_from_int(int b, Foo *out_enum) {"
                , "  switch (b) {"
                , "    case FOO_ONE: {"
                , "      *out_enum = FOO_ONE;"
                , "      return true;"
                , "    }"
                , "    case FOO_TWO: {"
                , "      *out_enum = FOO_ONE;"  -- mistake here
                , "      return true;"
                , "    }"
                , "    default: {"
                , "      *out_enum = FOO_ONE;"
                , "      return false;"
                , "    }"
                , "  }"
                , "}"
                ]
        expectedLines <- Text.lines . render . plain . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "  switch (b) {"
            , "    case FOO_ONE: {"
            , "      *out_enum = FOO_ONE;"
            , "      return true;"
            , "    }"
            , "    case FOO_TWO: {"
            , "      *out_enum = FOO_TWO;"
            , "      return true;"
            , "    }"
            , "    default: {"
            , "      *out_enum = FOO_ONE;"
            , "      return false;"
            , "    }"
            , "  }"
            , "}"
            ]
        let indent l = if Text.null (Text.strip l) then "" else "         " <> l
        shouldWarn' (testC code)
            [[ "warning: enum `_from_int` function for `Foo` should be: [-Wenum-from-int]"
                , Text.intercalate "\n" $ map indent expectedLines
                , "   --> test.c:5:1"
                , "    |"
                , "   5| bool foo_from_int(int b, Foo *out_enum) {"
                , "    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                ]]

    it "should not give diagnostics on correct from_int function" $ do
        shouldAccept' $ testC
                [ "typedef enum Foo {"
                , "  FOO_ONE,"
                , "  FOO_TWO,"
                , "} Foo;"
                , "bool foo_from_int(int b, Foo *out_enum) {"
                , "  switch (b) {"
                , "    case FOO_ONE: {"
                , "      *out_enum = FOO_ONE;"
                , "      return true;"
                , "    }"
                , "    case FOO_TWO: {"
                , "      *out_enum = FOO_TWO;"
                , "      return true;"
                , "    }"
                , "    default: {"
                , "      *out_enum = FOO_ONE;"
                , "      return false;"
                , "    }"
                , "  }"
                , "}"
                ]
