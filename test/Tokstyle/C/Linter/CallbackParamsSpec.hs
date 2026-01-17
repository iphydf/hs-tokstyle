{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CallbackParamsSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings)
import           Tokstyle.C.LinterSpec (shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn allWarnings


spec :: Spec
spec = do
    it "warns when callback param names don't match the callback type's param names" $ do
        shouldWarn'
            [ "typedef void some_cb(int foo, char bar);"
            , "void some_handler(int boo, char bar);"
            , "void invoke(some_cb *handler, int blep, char bork);"
            , "void wrong(void) {"
            , "  invoke(some_handler, 123, 'a');"
            , "}"
            ]
            [[ "error: parameter 1 of some_handler is named `boo`, but in callback type `some_cb *` it is named `foo` [-Wcallback-params]"
             , "   --> test.c:5:10"
             , "    |"
             , "   5|   invoke(some_handler, 123, 'a');"
             , "    |          ^^^^^^^^^^^^"
             ]]
