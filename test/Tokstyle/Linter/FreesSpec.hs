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

    it "warns about a struct freed while a field it was given memory for still holds it" $ do
        shouldWarn ["leaked-field"] (testC
            [ "Obj *obj_new(void) {"
            , "  Err rc = ERR_OK;"
            , "  Obj *obj = (Obj *)calloc(1, sizeof(Obj));"
            , "  if (obj == nullptr) {"
            , "    return nullptr;"
            , "  }"
            , "  obj->lock = (Lock *)malloc(sizeof(Lock));"
            , "  if (obj->lock == nullptr) {"
            , "    rc = ERR_MALLOC;"
            , "    goto RETURN;"
            , "  }"
            , "  obj->log = logger_new();"
            , "  if (obj->log == nullptr) {"
            , "    rc = ERR_MALLOC;"
            , "    goto RETURN;"
            , "  }"
            , "RETURN:"
            , "  if (rc != ERR_OK) {"
            , "    free(obj);"
            , "    return nullptr;"
            , "  }"
            , "  return obj;"
            , "}"
            ])
            [[ "error: `obj` is freed here while `obj->lock` may still hold the memory it got at line 7 [-Wleaked-field]"
             , "   --> test.c:19:5"
             , "    |"
             , "  19|     free(obj);"
             , "    |     ^^^^"
             ]]

    it "accepts a struct freed by a function that frees its fields too" $ do
        shouldAccept ["leaked-field"] (testC
            [ "static void obj_free(Obj *obj) {"
            , "  free(obj->lock);"
            , "  free(obj);"
            , "}"
            , "Obj *obj_new(void) {"
            , "  Obj *obj = (Obj *)calloc(1, sizeof(Obj));"
            , "  if (obj == nullptr) {"
            , "    return nullptr;"
            , "  }"
            , "  obj->lock = (Lock *)malloc(sizeof(Lock));"
            , "  if (!init(obj)) {"
            , "    obj_free(obj);"
            , "    return nullptr;"
            , "  }"
            , "  return obj;"
            , "}"
            ])

    it "accepts memory a field passed on before its struct is freed" $ do
        shouldAccept ["leaked-field"] (testC
            [ "void hand_over(Obj *obj, Holder *holder) {"
            , "  obj->buf = (uint8_t *)malloc(4);"
            , "  holder->buf = obj->buf;"
            , "  free(obj);"
            , "}"
            ])
