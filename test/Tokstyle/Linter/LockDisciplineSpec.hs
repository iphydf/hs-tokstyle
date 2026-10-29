{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LockDisciplineSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


header :: [Text]
header =
    [ "struct Session {"
    , "  pthread_mutex_t mutex[1];"
    , "  /** @guarded_by mutex */"
    , "  Call *calls;"
    , "};"
    , "/**"
    , " * Sends a frame."
    , " *"
    , " * @thread any"
    , " */"
    , "bool send_frame(Session *s);"
    ]


spec :: Spec
spec = do
    it "warns about writing a guarded field without its lock" $ do
        shouldWarn ["lock-discipline"]
            [ ("test.h", header)
            , ("test.c",
                [ "void session_clear(Session *s) {"
                , "  s->calls = nullptr;"
                , "}"
                ])
            ]
            [[ "error: `s->calls` is written here without `s->mutex`, which guards it [-Wlock-discipline]"
             , "   --> test.c:2:6"
             , "    |"
             , "   2|   s->calls = nullptr;"
             , "    |      ^^^^^"
             ]]

    it "warns about reading a guarded field without its lock where any thread may" $ do
        shouldWarn ["lock-discipline"]
            [ ("test.h", header)
            , ("test.c",
                [ "bool send_frame(Session *s) {"
                , "  return s->calls != nullptr;"
                , "}"
                ])
            ]
            [[ "error: `s->calls` is read here, in `send_frame` that may run on any thread, without `s->mutex`, which guards it [-Wlock-discipline]"
             , "   --> test.c:2:13"
             , "    |"
             , "   2|   return s->calls != nullptr;"
             , "    |             ^^^^^"
             ]]

    it "accepts what the owning thread reads, and what a lock tried and got guards" $ do
        shouldAccept ["lock-discipline"]
            [ ("test.h", header)
            , ("test.c",
                [ "bool session_has_calls(Session *s) {"
                , "  return s->calls != nullptr;"
                , "}"
                , "bool send_frame(Session *s) {"
                , "  if (pthread_mutex_trylock(s->mutex) != 0) {"
                , "    return false;"
                , "  }"
                , "  const bool has = s->calls != nullptr;"
                , "  pthread_mutex_unlock(s->mutex);"
                , "  return has;"
                , "}"
                ])
            ]

    it "accepts a static function writing what every caller holds the lock for" $ do
        shouldAccept ["lock-discipline"]
            [ ("test.h", header)
            , ("test.c",
                [ "static void calls_clear(Session *session) {"
                , "  session->calls = nullptr;"
                , "}"
                , "void session_clear(Session *s) {"
                , "  pthread_mutex_lock(s->mutex);"
                , "  calls_clear(s);"
                , "  pthread_mutex_unlock(s->mutex);"
                , "}"
                ])
            ]

    it "warns about a static function a caller calls without the lock" $ do
        shouldWarn ["lock-discipline"]
            [ ("test.h", header)
            , ("test.c",
                [ "static void calls_clear(Session *session) {"
                , "  session->calls = nullptr;"
                , "}"
                , "void session_clear(Session *s) {"
                , "  pthread_mutex_lock(s->mutex);"
                , "  calls_clear(s);"
                , "  pthread_mutex_unlock(s->mutex);"
                , "}"
                , "void session_reset(Session *s) {"
                , "  calls_clear(s);"
                , "}"
                ])
            ]
            [[ "error: `session->calls` is written here without `session->mutex`, which guards it [-Wlock-discipline]"
             , "   --> test.c:2:12"
             , "    |"
             , "   2|   session->calls = nullptr;"
             , "    |            ^^^^^"
             ]]

    it "accepts writing what a lock just made guards" $ do
        shouldAccept ["lock-discipline"]
            [ ("test.h", header)
            , ("test.c",
                [ "bool session_init(Session *s) {"
                , "  if (pthread_mutex_init(s->mutex, nullptr) != 0) {"
                , "    return false;"
                , "  }"
                , "  s->calls = nullptr;"
                , "  return true;"
                , "}"
                ])
            ]
