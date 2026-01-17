{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CallbackDataSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["callback-data"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["callback-data"]


spec :: Spec
spec = do
    it "does not warn when the context type matches the inferred type" $ do
        shouldAccept'
            [ "typedef void dht_ip_cb(void *object, int number);"
            , "void dht_addfriend(dht_ip_cb *callback, void *object);"
            , "struct Friend_Connections { int a; };"
            , "void dht_ip_callback(void *object, int number) {"
            , "  struct Friend_Connections *fr_c = (struct Friend_Connections *)object;"
            , "}"
            , "void test(void) {"
            , "  struct Friend_Connections fr_c;"
            , "  dht_addfriend(dht_ip_callback, &fr_c);"
            , "}"
            ]

    it "warns when the context type does not match the inferred type" $ do
        shouldWarn'
            [ "typedef void dht_ip_cb(void *object, int number);"
            , "void dht_addfriend(dht_ip_cb *callback, void *object);"
            , "struct Friend_Connections { int a; };"
            , "struct Other { int b; };"
            , "void dht_ip_callback(void *object, int number) {"
            , "  struct Friend_Connections *fr_c = (struct Friend_Connections *)object;"
            , "}"
            , "void test(void) {"
            , "  struct Other o;"
            , "  dht_addfriend(dht_ip_callback, &o);"
            , "}"
            ]
            [[ "error: callback expects context of type `struct Friend_Connections *`, but got `struct Other *` [-Wcallback-data]"
             , "   --> test.c:10:34"
             , "    |"
             , "  10|   dht_addfriend(dht_ip_callback, &o);"
             , "    |                                  ^^"
             ]]

    it "warns when the context type does not match the inferred type in struct assignment" $ do
        shouldWarn'
            [ "typedef void dht_ip_cb(void *object, int number);"
            , "struct Handler {"
            , "  dht_ip_cb *callback;"
            , "  void *object;"
            , "};"
            , "struct Friend_Connections { int a; };"
            , "struct Other { int b; };"
            , "void dht_ip_callback(void *object, int number) {"
            , "  struct Friend_Connections *fr_c = (struct Friend_Connections *)object;"
            , "}"
            , "void test(void) {"
            , "  struct Other o;"
            , "  struct Handler h;"
            , "  h.callback = dht_ip_callback;"
            , "  h.object = &o;"
            , "}"
            ]
            [[ "error: callback expects context of type `struct Friend_Connections *`, but got `struct Other *` [-Wcallback-data]"
             , "   --> test.c:15:14"
             , "    |"
             , "  15|   h.object = &o;"
             , "    |              ^^"
             ]]

    it "allows passing a const pointer to a function expecting a non-const pointer" $ do
        shouldAccept'
            [ "typedef void dht_ip_cb(void *object, int number);"
            , "void dht_addfriend(dht_ip_cb *callback, void *object);"
            , "struct Friend_Connections { int a; };"
            , "void dht_ip_callback(void *object, int number) {"
            , "  struct Friend_Connections *fr_c = (struct Friend_Connections *)object;"
            , "}"
            , "void test(struct Friend_Connections * const fr_c) {"
            , "  dht_addfriend(dht_ip_callback, fr_c);"
            , "}"
            ]

    it "allows passing a non-const pointer to a function expecting a const pointer" $ do
        shouldAccept'
            [ "typedef void dht_ip_cb(void *object, int number);"
            , "void dht_addfriend(dht_ip_cb *callback, void *object);"
            , "struct Friend_Connections { int a; };"
            , "void dht_ip_callback(void *object, int number) {"
            , "  struct Friend_Connections const *fr_c = (struct Friend_Connections const *)object;"
            , "}"
            , "void test(struct Friend_Connections *fr_c) {"
            , "  dht_addfriend(dht_ip_callback, fr_c);"
            , "}"
            ]

    it "allows passing uint8_t** to a callback expecting uint8_t const * const *" $ do
        shouldAccept'
            [ "typedef void cb(void *object);"
            , "void reg(cb *callback, void *object);"
            , "void my_cb(void *object) {"
            , "  unsigned char const * const *p = (unsigned char const * const *)object;"
            , "}"
            , "void test(unsigned char **p) {"
            , "  reg(my_cb, p);"
            , "}"
            ]
