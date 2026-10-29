{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.AllocatorSwapSpec (spec) where

import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn)


spec :: Spec
spec = do
    it "warns about replacing the allocator of an object its maker set" $ do
        shouldWarn ["allocator-swap"]
            [ ("test.h", [ "struct Obj {", "  const Memory *_Nonnull mem;", "};" ])
            , ("test.c",
                [ "Obj *obj_new(const Memory *mem) {"
                , "  Obj *obj = (Obj *)mem_alloc(mem, sizeof(Obj));"
                , "  obj->mem = mem;"
                , "  return obj;"
                , "}"
                , "Obj *obj_new_for(const Tox *tox) {"
                , "  Obj *obj = obj_new(os_memory());"
                , "  obj->mem = tox->sys.mem;"
                , "  return obj;"
                , "}"
                ])
            ]
            [[ "error: `obj->mem` is set here, but `obj_new` at line 7 already set it and may have allocated with it [-Wallocator-swap]"
             , "   --> test.c:8:3"
             , "    |"
             , "   8|   obj->mem = tox->sys.mem;"
             , "    |   ^^^^^^^^^^^^^^^^^^^^^^^"
             ]]

    it "accepts setting the allocator of an object that comes without one" $ do
        shouldAccept ["allocator-swap"]
            [ ("test.h", [ "struct Chat {", "  const Memory *_Nonnull mem;", "};" ])
            , ("test.c",
                [ "Chat *chat_slot(Session *s, int i) {"
                , "  return &s->chats[i];"
                , "}"
                , "void chat_init(Session *s, int i) {"
                , "  Chat *chat = chat_slot(s, i);"
                , "  chat->mem = s->mem;"
                , "}"
                ])
            ]
