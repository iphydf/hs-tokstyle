{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.SemFmt.EnumUnpackSpec where
import           Data.Text              (Text)
import qualified Data.Text              as Text
import           Test.Hspec             (Spec, it)

import           Language.Cimple.Pretty (plain, ppTranslationUnit, render)
import           Tokstyle.LinterSpec    (mustParseStmt, shouldAccept,
                                         shouldWarn)


shouldWarn' :: [(FilePath, [Text])] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["enum-unpack"]


shouldAccept' :: [(FilePath, [Text])] -> IO ()
shouldAccept' = shouldAccept ["enum-unpack"]


testC :: [Text] -> [(FilePath, [Text])]
testC code = [("test.c", code)]


spec :: Spec
spec = do
    it "should give diagnostics on incorrect unpack function" $ do
        let code =
                [ "typedef enum Foo {"
                , "  FOO_ONE,"
                , "  FOO_TWO,"
                , "} Foo;"
                , "bool foo_unpack(Bin_Unpack *bu, Foo *val) {"
                , "  return true;"
                , "}"
                ]
        expectedLines <- Text.lines . render . plain . ppTranslationUnit . (:[]) <$> mustParseStmt
            [ "{"
            , "    uint32_t u32;"
            , "    return bin_unpack_u32(bu, &u32)"
            , "        && foo_from_int(u32, val);"
            , "}"
            ]
        let indent l = if Text.null (Text.strip l) then "" else "         " <> l
        shouldWarn' (testC code)
            [[ "warning: enum `_unpack` function for `Foo` should be: [-Wenum-unpack]"
                , Text.intercalate "\n" $ map indent expectedLines
                , "   --> test.c:5:1"
                , "    |"
                , "   5| bool foo_unpack(Bin_Unpack *bu, Foo *val) {"
                , "    | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
                ]]
