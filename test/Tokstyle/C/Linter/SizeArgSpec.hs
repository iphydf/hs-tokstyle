{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SizeArgSpec (spec) where

import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["size-arg"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["size-arg"]


spec :: Spec
spec = do
    it "warns when constant size argument is not the array size" $ do
        shouldWarn'
            [ "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  char arr[12];"
            , "  consume(arr, 13);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `13` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [12]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:4:16"
             , "    |"
             , "   4|   consume(arr, 13);"
             , "    |                ^^"
             ]]

    it "can see through enum constants" $ do
        shouldWarn'
            [ "enum { SIZE = 12 };"
            , "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  char arr[SIZE];"
            , "  consume(arr, SIZE + 1);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `SIZE + 1` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [SIZE]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:5:16"
             , "    |"
             , "   5|   consume(arr, SIZE + 1);"
             , "    |                ^^^^^^^^"
             ]]

    it "can see through typedefs" $ do
        shouldWarn'
            [ "enum { SIZE = 12 };"
            , "typedef unsigned int size_t;"
            , "void consume(char *arr, size_t size);"
            , "void caller(void) {"
            , "  char arr[SIZE];"
            , "  consume(arr, SIZE + 1);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `SIZE + 1` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [SIZE]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:6:16"
             , "    |"
             , "   6|   consume(arr, SIZE + 1);"
             , "    |                ^^^^^^^^"
             ]]

    it "can see through array typedefs" $ do
        shouldWarn'
            [ "typedef char My_Array[12];"
            , "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  My_Array arr;"
            , "  consume(arr, 13);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `13` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [12]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:5:16"
             , "    |"
             , "   5|   consume(arr, 13);"
             , "    |                ^^"
             ]]

    it "can see through function typedefs" $ do
        shouldWarn'
            [ "typedef void consume_cb(char *arr, int size);"
            , "consume_cb consume;"
            , "void caller(void) {"
            , "  char arr[12];"
            , "  consume(arr, 13);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `13` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [12]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:5:16"
             , "    |"
             , "   5|   consume(arr, 13);"
             , "    |                ^^"
             ]]

    it "works on function pointers" $ do
        shouldWarn'
            [ "typedef void consume_cb(char *arr, int size);"
            , "void caller(consume_cb *consume) {"
            , "  char arr[12];"
            , "  consume(arr, 13);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `13` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [12]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:4:16"
             , "    |"
             , "   4|   consume(arr, 13);"
             , "    |                ^^"
             ]]

    it "works on array parameters" $ do
        shouldWarn'
            [ "typedef void consume_cb(char *arr, int size);"
            , "void caller(consume_cb *consume, char arr[12]) {"
            , "  consume(arr, 13);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `13` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [12]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:3:16"
             , "    |"
             , "   3|   consume(arr, 13);"
             , "    |                ^^"
             ]]

    it "works on typedef array parameters" $ do
        shouldWarn'
            [ "enum { CRYPTO_PUBLIC_KEY_SIZE = 32 };"
            , "enum { EXTENDED_PUBLIC_KEY_SIZE = 64 };"
            , "typedef char Public_Key[CRYPTO_PUBLIC_KEY_SIZE];"
            , "typedef void consume_cb(char *arr, int size);"
            , "void caller(consume_cb *consume, Public_Key pk) {"
            , "  consume(pk, EXTENDED_PUBLIC_KEY_SIZE);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `EXTENDED_PUBLIC_KEY_SIZE` (= 64), [-Wsize-arg]"
             , "         which is greater than the array size of `char [CRYPTO_PUBLIC_KEY_SIZE]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:6:15"
             , "    |"
             , "   6|   consume(pk, EXTENDED_PUBLIC_KEY_SIZE);"
             , "    |               ^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about string literal overrun" $ do
        shouldWarn'
            [ "void consume(char *arr, int size);"
            , "void caller(void) {"
            , "  consume(\"hello world\", 13);"
            , "}"
            ]
            [[ "error: size parameter `size` is passed constant value `13` (= 13), [-Wsize-arg]"
             , "         which is greater than the array size of `char [static 11]`,"
             , "         potentially causing buffer overrun in `consume`"
             , "   --> test.c:3:26"
             , "    |"
             , "   3|   consume(\"hello world\", 13);"
             , "    |                          ^^"
             ]]

    it "ignores calls where the parameter name does not indicate it's a size" $ do
        shouldAccept'
            [ "typedef char My_Array[12];"
            , "void consume(char *file, int line);"
            , "void caller(void) {"
            , "  consume(\"hello.c\", 123);"
            , "}"
            ]
