{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.CallbackDisciplineSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, describe, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["callback-discipline"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["callback-discipline"]


spec :: Spec
spec = describe "CallbackDiscipline linter" $ do
    it "identifies a registry and allows its mutation" $ do
        shouldAccept'
            [ "struct Foo { void (*callback)(void); };"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    f->callback = cb;"
            , "}"
            ]

    it "warns on mutation in business logic" $ do
        shouldWarn'
            [ "struct Foo { void (*callback)(void); int x; };"
            , "void business_logic(struct Foo *f, void (*cb)(void)) {"
            , "    f->x = 1;"
            , "    f->callback = cb;"
            , "}"
            ]
            [[ "error: registry function `business_logic` mutates non-callback field `f->x` [-Wcallback-discipline]"
             , "   --> test.c:3:5"
             , "    |"
             , "   3|     f->x = 1;"
             , "    |     ^^^^"
             , "    = note: registry functions should only set callbacks and their associated userdata"
             ]]

    it "warns on complex registry" $ do
        shouldWarn'
            [ "struct Foo { void (*callback)(void); };"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    f->callback = cb;"
            , "    int x = 1; x++; x++; x++; x++; x++; x++; x++; x++; x++; x++;"
            , "    x++; x++; x++; x++; x++; x++; x++; x++; x++; x++;"
            , "}"
            ]
            [[ "error: registry function `register_foo` is too complex (22 statements) [-Wcallback-discipline]"
             , "   --> test.c:2:52"
             , "    |"
             , "   2| void register_foo(struct Foo *f, void (*cb)(void)) {"
             , "    |                                                    ^"
             ]]

    it "warns on loop in registry" $ do
        shouldWarn'
            [ "struct Foo { void (*callback)(void); };"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    f->callback = cb;"
            , "    while (1);"
            , "}"
            ]
            [[ "error: registry function `register_foo` contains a loop [-Wcallback-discipline]"
             , "   --> test.c:4:5"
             , "    |"
             , "   4|     while (1);"
             , "    |     ^^^^^^^^^^"
             ]]

    it "warns on non-trivial call in registry" $ do
        shouldWarn'
            [ "struct Foo { void (*callback)(void); };"
            , "void heavy_call();"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    f->callback = cb;"
            , "    heavy_call();"
            , "}"
            ]
            [[ "error: registry function `register_foo` calls non-trivial function `heavy_call` [-Wcallback-discipline]"
             , "   --> test.c:3:52"
             , "    |"
             , "   3| void register_foo(struct Foo *f, void (*cb)(void)) {"
             , "    |                                                    ^"
             , "    = note: registry functions should be side-effect free and remain simple"
             ]]

    it "warns on mutation of non-callback field in registry" $ do
        shouldWarn'
            [ "struct Foo { void (*callback)(void); int x; };"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    f->callback = cb;"
            , "    f->x = 1;"
            , "}"
            ]
            [[ "error: registry function `register_foo` mutates non-callback field `f->x` [-Wcallback-discipline]"
             , "   --> test.c:4:5"
             , "    |"
             , "   4|     f->x = 1;"
             , "    |     ^^^^"
             , "    = note: registry functions should only set callbacks and their associated userdata"
             ]]

    it "allows mutation in constructor" $ do
        shouldAccept'
            [ "struct Foo { void (*callback)(void); };"
            , "struct Foo *new_foo() {"
            , "    struct Foo *f = 0;"
            , "    f->callback = 0;"
            , "    return f;"
            , "}"
            ]

    it "allows unregistration (NULL)" $ do
        shouldAccept'
            [ "struct Foo { void (*callback)(void); };"
            , "void unregister_foo(struct Foo *f) {"
            , "    f->callback = 0;"
            , "}"
            ]

    it "allows unregistration (nullptr)" $ do
        shouldAccept'
            [ "typedef void *nullptr_t;"
            , "struct Foo { void (*callback)(void); };"
            , "void unregister_foo(struct Foo *f, nullptr_t nullptr) {"
            , "    f->callback = nullptr;"
            , "}"
            ]

    it "identifies registry with array access" $ do
        shouldAccept'
            [ "struct Foo { void (*callbacks[256])(void); };"
            , "void register_foo(struct Foo *f, int i, void (*cb)(void)) {"
            , "    f->callbacks[i] = cb;"
            , "}"
            ]

    it "allows assert in registry" $ do
        shouldAccept'
            [ "void __assert_fail();"
            , "struct Foo { void (*callback)(void); };"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    if (!(f != 0)) __assert_fail();"
            , "    f->callback = cb;"
            , "}"
            ]

    it "warns on mutation outside of a registry and points to the correct registry" $ do
        shouldWarn'
            [ "struct Foo { void (*callback)(void); };"
            , "void register_foo(struct Foo *f, void (*cb)(void)) {"
            , "    f->callback = cb;"
            , "}"
            , "void bad_mutation(struct Foo *f) {"
            , "    void (*cb)(void) = 0;"
            , "    f->callback = cb;"
            , "}"
            ]
            [[ "error: mutation of callback/userdata field `f->callback` outside of a registry function [-Wcallback-discipline]"
             , "   --> test.c:7:5"
             , "    |"
             , "   7|     f->callback = cb;"
             , "    |     ^^^^^^^^^^^"
             , "    = help: use one of these registry functions instead: register_foo"
             ]]
