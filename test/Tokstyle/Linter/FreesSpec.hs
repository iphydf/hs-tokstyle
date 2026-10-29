{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.FreesSpec (spec) where

import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldWarn, testC)


spec :: Spec
spec = do
    it "warns about a failure path that frees what the shared cleanup frees again" $ do
        shouldWarn ["double-free"] (testC
            [ "void mono_time_free(const Memory *mem, Mono_Time *mono_time) {"
            , "  mem_delete(mem, mono_time);"
            , "}"
            , "Obj *obj_new(const Memory *mem) {"
            , "  Err rc = ERR_OK;"
            , "  Obj *obj = (Obj *)calloc(1, sizeof(Obj));"
            , "  if (obj == nullptr) {"
            , "    return nullptr;"
            , "  }"
            , "  obj->time = mono_time_new(mem);"
            , "  obj->msi = msi_new();"
            , "  if (obj->msi == nullptr) {"
            , "    mono_time_free(mem, obj->time);"
            , "    rc = ERR_MALLOC;"
            , "    goto RETURN;"
            , "  }"
            , "RETURN:"
            , "  if (rc != ERR_OK) {"
            , "    mono_time_free(mem, obj->time);"
            , "    free(obj);"
            , "    return nullptr;"
            , "  }"
            , "  return obj;"
            , "}"
            ])
            [[ "error: `obj->time` is freed here, and may already be freed at line 13 [-Wdouble-free]"
             , "   --> test.c:19:5"
             , "    |"
             , "  19|     mono_time_free(mem, obj->time);"
             , "    |     ^^^^^^^^^^^^^^"
             ]]

    it "accepts a pointer read again through its address before each free" $ do
        shouldAccept ["double-free"] (testC
            [ "void drain(Buffer *b) {"
            , "  void *p;"
            , "  while (rb_read(b, &p)) {"
            , "    free(p);"
            , "  }"
            , "}"
            ])

    it "accepts a free retried only while it fails" $ do
        shouldAccept ["double-free"] (testC
            [ "int session_kill(Session *session) {"
            , "  if (!try_lock(session)) {"
            , "    return -1;"
            , "  }"
            , "  free(session);"
            , "  return 0;"
            , "}"
            , "void obj_kill(Obj *obj) {"
            , "  while (obj->session != nullptr && session_kill(obj->session) != 0) {"
            , "    wait_for(obj);"
            , "  }"
            , "}"
            ])

    it "accepts a field given new memory between frees" $ do
        shouldAccept ["double-free"] (testC
            [ "void reset(Obj *obj) {"
            , "  free(obj->data);"
            , "  obj->data = (uint8_t *)malloc(4);"
            , "  free(obj->data);"
            , "}"
            ])
