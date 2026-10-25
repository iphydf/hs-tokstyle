{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.BufferSizeSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["buffer-size"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["buffer-size"]


prelude :: [Text]
prelude =
    [ "typedef unsigned char uint8_t;"
    , "typedef unsigned short uint16_t;"
    , "typedef unsigned int uint32_t;"
    , "void *memcpy(void *dst, const void *src, unsigned long n);"
    , "void *memset(void *dst, int c, unsigned long n);"
    , "void *alloca(unsigned long size);"
    , "void send(const uint8_t *data, uint16_t length);"
    ]


spec :: Spec
spec = do
    it "warns about a copy into an array whose size wraps around" $ do
        shouldWarn' (prelude ++
            [ "void f(const uint8_t *data, uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 65535, so `buf` can be smaller than the 4294967296 bytes `memcpy` reaches [-Wbuffer-size]"
             , "   --> test.c:11:3"
             , "    |"
             , "   9|   const uint16_t size = length + 81;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  11|   memcpy(buf + 1, data, length);"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "warns about an index into allocated memory whose size wraps around" $ do
        shouldWarn' (prelude ++
            [ "void f(uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t *const buf = (uint8_t *)alloca(size * sizeof(uint8_t));"
            , "  buf[0] = 1;"
            , "  send(buf, size);"
            , "}"
            ])
            [[ "error: `size` can wrap around to values from 0 to 65535, so `buf` can be smaller than the 1 byte `buf[0]` reaches [-Wbuffer-size]"
             , "   --> test.c:11:3"
             , "    |"
             , "   9|   const uint16_t size = length + 81;"
             , "    |                         ^^^^^^^^^^^"
             , "    |                         |"
             , "    |                         wraps around here"
             , "  11|   buf[0] = 1;"
             , "    |   ^^^^^^"
             ]]

    it "accepts filling exactly the buffer's size" $ do
        shouldAccept' (prelude ++
            [ "void f(uint32_t length) {"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memset(buf, 0, size);"
            , "  send(buf, size);"
            , "}"
            ])

    it "accepts a size limited before it is narrowed" $ do
        shouldAccept' (prelude ++
            [ "void f(const uint8_t *data, uint32_t length) {"
            , "  if (length > 1000) {"
            , "    return;"
            , "  }"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])

    it "accepts a size whose parts a guard limits" $ do
        shouldAccept' (prelude ++
            [ "void f(const uint8_t *data, uint16_t length, uint16_t max) {"
            , "  if (1 + length + 81 > max) {"
            , "    return;"
            , "  }"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])

    it "knows the limits of integer types declared as objects" $ do
        shouldAccept' (prelude ++
            [ "extern const uint32_t UINT32_MAX;"
            , "void *mem_balloc(const void *mem, uint32_t size);"
            , "void f(const void *mem, const uint8_t *data, uint32_t length) {"
            , "  if (length == UINT32_MAX) {"
            , "    return;"
            , "  }"
            , "  uint8_t *const buf = (uint8_t *)mem_balloc(mem, length + 1);"
            , "  memcpy(buf, data, length);"
            , "}"
            ])

    it "follows a local whose address only a call gets" $ do
        shouldAccept' (prelude ++
            [ "extern const uint32_t UINT32_MAX;"
            , "void *mem_balloc(const void *mem, uint32_t size);"
            , "int read_size(uint32_t *size);"
            , "void f(const void *mem, const uint8_t *data) {"
            , "  uint32_t length;"
            , "  if (!read_size(&length) || length == UINT32_MAX) {"
            , "    return;"
            , "  }"
            , "  uint8_t *const buf = (uint8_t *)mem_balloc(mem, length + 1);"
            , "  memcpy(buf, data, length);"
            , "}"
            ])

    it "knows paths that call a function that never returns end there" $ do
        shouldAccept' (prelude ++
            [ "void abort(void);"
            , "void f(const uint8_t *data, uint32_t length) {"
            , "  if (length > 1000) {"
            , "    abort();"
            , "  }"
            , "  const uint16_t size = length + 81;"
            , "  uint8_t buf[size];"
            , "  memcpy(buf + 1, data, length);"
            , "  send(buf, size);"
            , "}"
            ])
