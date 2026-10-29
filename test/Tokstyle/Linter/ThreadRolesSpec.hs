{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ThreadRolesSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


header :: [Text]
header =
    [ "typedef bool send_cb(uint32_t friend_number, void *user_data);"
    , "/**"
    , " * Queues a packet."
    , " *"
    , " * @thread any"
    , " */"
    , "typedef int queue_cb(void *user_data);"
    , "struct Session_IO {"
    , "  send_cb *send;"
    , "};"
    , "struct Session {"
    , "  Session_IO io;"
    , "  queue_cb *queue;"
    , "};"
    , "/**"
    , " * Sends a frame."
    , " *"
    , " * @thread any"
    , " */"
    , "bool send_frame(Session *s);"
    , "void session_set_queue(Session *s, queue_cb *queue);"
    ]


spec :: Spec
spec = do
    it "warns about a function that may run on any thread calling back the client" $ do
        shouldWarn ["thread-roles"]
            [ ("test.h", header)
            , ("test.c",
                [ "static bool encode(Session *s) {"
                , "  return s->io.send(0, s);"
                , "}"
                , "bool send_frame(Session *s) {"
                , "  return encode(s);"
                , "}"
                ])
            ]
            [[ "error: `s->io.send` is called here on a path from `send_frame` that may run on any thread, as `send_frame` is `@thread any`, but its type `send_cb` is not `@thread any` [-Wthread-roles]"
             , "   --> test.c:2:10"
             , "    |"
             , "   2|   return s->io.send(0, s);"
             , "    |          ^^^^^^^^^^"
             ]]

    it "warns about a callback of a type that may run on any thread calling back the client" $ do
        shouldWarn ["thread-roles"]
            [ ("test.h", header)
            , ("test.c",
                [ "static int queue_packet(void *user_data) {"
                , "  Session *s = (Session *)user_data;"
                , "  return s->io.send(0, s) ? 0 : -1;"
                , "}"
                , "void session_init(Session *s) {"
                , "  session_set_queue(s, queue_packet);"
                , "}"
                ])
            ]
            [[ "error: `s->io.send` is called here on a path from `queue_packet` that may run on any thread, as `queue_packet` is a `queue_cb`, which is `@thread any`, but its type `send_cb` is not `@thread any` [-Wthread-roles]"
             , "   --> test.c:3:10"
             , "    |"
             , "   3|   return s->io.send(0, s) ? 0 : -1;"
             , "    |          ^^^^^^^^^^"
             ]]

    it "warns about a client callback handed over as a type that may run on any thread" $ do
        shouldWarn ["thread-roles"]
            [ ("test.h", header)
            , ("test.c",
                [ "void session_init(Session *s, send_cb *send) {"
                , "  session_set_queue(s, send);"
                , "}"
                ])
            ]
            [[ "error: `send` is a `send_cb` passed here as a `queue_cb`, which is `@thread any`, but `send_cb` is not [-Wthread-roles]"
             , "   --> test.c:2:24"
             , "    |"
             , "   2|   session_set_queue(s, send);"
             , "    |                        ^^^^"
             ]]

    it "accepts calls back through types that may run on any thread, and macros" $ do
        shouldAccept ["thread-roles"]
            [ ("test.h", header)
            , ("test.c",
                [ "bool send_frame(Session *s) {"
                , "  LOGGER_DEBUG(s->log, \"sending\");"
                , "  return s->queue(s) == 0;"
                , "}"
                , "void session_iterate(Session *s) {"
                , "  s->io.send(0, s);"
                , "}"
                ])
            ]
