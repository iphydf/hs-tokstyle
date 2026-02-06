{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.OwnershipDeclsSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, it)

import           Tokstyle.LinterSpec (shouldAccept, shouldAcceptLocal,
                                      shouldWarn, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["ownership-decls"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["ownership-decls"]


spec :: Spec
spec = do
    it "allows qualifiers on declarations" $ do
        shouldAccept'
            [ "int * _Owner foo(void);"
            , "int * _Nullable bar(void);"
            , "int * _Nonnull baz(void);"
            ]

    it "allows qualifiers on parameters in declarations" $ do
        shouldAccept'
            [ "void foo(int * _Owner p);"
            , "void bar(int * _Nullable p);"
            , "void baz(int * _Nonnull p);"
            ]

    it "forbids qualifiers on global definitions" $ do
        shouldWarn' [ "int * _Owner foo(void) { return 0; }" ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:1:14"
             , "    |"
             , "   1| int * _Owner foo(void) { return 0; }"
             , "    |              ^^^"
             , "    |              |"
             , "    |              found qualifier here"
             ]]

    it "reports multiple qualifiers when present" $ do
        shouldWarn' [ "int * _Owner _Nullable foo(void) { return 0; }" ]
            [[ "warning: qualifiers `_Owner` and `_Nullable` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:1:24"
             , "    |"
             , "   1| int * _Owner _Nullable foo(void) { return 0; }"
             , "    |                        ^^^"
             , "    |                        |"
             , "    |                        found qualifier here"
             ]]

    it "forbids qualifiers on global definitions even if declared before" $ do
        shouldWarn'
            [ "int * _Owner foo(void);"
            , "int * _Owner foo(void) { return 0; }"
            ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:2:14"
             , "    |"
             , "   2| int * _Owner foo(void) { return 0; }"
             , "    |              ^^^"
             , "    |              |"
             , "    |              found qualifier here"
             ]]

    it "forbids qualifiers on static definitions if declared before" $ do
        shouldWarn'
            [ "static int * _Owner foo(void);"
            , "static int * _Owner foo(void) { return 0; }"
            ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:2:21"
             , "    |"
             , "   2| static int * _Owner foo(void) { return 0; }"
             , "    |                     ^^^"
             , "    |                     |"
             , "    |                     found qualifier here"
             ]]

    it "allows qualifiers on static definitions if NOT declared before" $ do
        shouldAccept' [ "static int * _Owner foo(void) { return 0; }" ]

    it "forbids qualifiers on parameters in definitions" $ do
        shouldWarn' [ "void foo(int * _Owner p) { return; }" ]
            [[ "warning: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> test.c:1:6"
             , "    |"
             , "   1| void foo(int * _Owner p) { return; }"
             , "    |      ^^^"
             , "    |      |"
             , "    |      found qualifier here"
             ]]

    it "warns on unannotated pointer return types in declarations" $ do
        shouldWarn' [ "int *foo(void);" ]
            [[ "warning: pointer type `int*` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> test.c:1:1"
             , "    |"
             , "   1| int *foo(void);"
             , "    | ^^^"
             , "    | |"
             , "    | missing annotation here"
             ]]

    it "warns on unannotated pointer parameters in declarations" $ do
        shouldWarn' [ "void foo(int *p);" ]
            [[ "warning: pointer type `int*` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> test.c:1:15"
             , "    |"
             , "   1| void foo(int *p);"
             , "    |               ^"
             , "    |               |"
             , "    |               missing annotation here"
             ]]

    it "warns on unannotated pointer return types in static definitions without prior declaration" $ do
        shouldWarn' [ "static int *foo(void) { return 0; }" ]
            [[ "warning: pointer type `int*` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> test.c:1:8"
             , "    |"
             , "   1| static int *foo(void) { return 0; }"
             , "    |        ^^^"
             , "    |        |"
             , "    |        missing annotation here"
             ]]

    it "warns on unannotated pointer parameters in static definitions without prior declaration" $ do
        shouldWarn' [ "static void foo(int *p) { return; }" ]
            [[ "warning: pointer type `int*` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> test.c:1:22"
             , "    |"
             , "   1| static void foo(int *p) { return; }"
             , "    |                      ^"
             , "    |                      |"
             , "    |                      missing annotation here"
             ]]

    it "warns on unannotated pointer struct members" $ do
        shouldWarn' [ "struct Foo { int *p; };" ]
            [[ "warning: pointer type `int*` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> test.c:1:19"
             , "    |"
             , "   1| struct Foo { int *p; };"
             , "    |                   ^"
             , "    |                   |"
             , "    |                   missing annotation here"
             ]]

    it "ignores third_party files" $ do
        shouldAccept ["ownership-decls"]
            [ ("third_party/foo.c", [ "int *foo(void);" ]) ]

    it "ignores public API headers" $ do
        shouldAccept ["ownership-decls"]
            [ ("tox.h", [ "int *foo(void);" ])
            , ("toxav.h", [ "int *bar(void);" ])
            , ("toxencryptsave.h", [ "int *baz(void);" ])
            , ("tox_options.h", [ "int *qux(void);" ])
            , ("tox_log_level.h", [ "int *quux(void);" ])
            ]

    it "allows unannotated definitions when a declaration exists" $ do
        shouldAccept ["ownership-decls"]
            [ ("foo.h", [ "int *_Nonnull foo(void);" ])
            , ("foo.c", [ "int *foo(void) { return 0; }" ])
            ]

    it "expects and allows annotations on definitions if missing from public header decl" $ do
        shouldAccept ["ownership-decls"]
            [ ("tox.h", [ "int *foo(void);" ])
            , ("foo.c", [ "int *_Nonnull foo(void) { return 0; }" ] )
            ]

    it "warns if annotations are missing from both public header decl and definition" $ do
        shouldWarn ["ownership-decls"]
            [ ("tox.h", [ "int *foo(void);" ])
            , ("foo.c", [ "int *foo(void) { return 0; }" ])
            ]
            [[ "warning: pointer type `int*` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> foo.c:1:1"
             , "    |"
             , "   1| int *foo(void) { return 0; }"
             , "    | ^^^"
             , "    | |"
             , "    | missing annotation here"
             , "   ::: tox.h:1:1"
             , "    |"
             , "   1| int *foo(void);"
             , "    | ^^^^^^^^^^^^^"
             , "    | |"
             , "    | because declaration here is unannotated"
             ]]

    it "forbids annotations on definitions if already present in public header decl" $ do
        shouldWarn ["ownership-decls"]
            [ ("tox.h", [ "int *_Nonnull foo(void);" ])
            , ("foo.c", [ "int *_Nonnull foo(void) { return 0; }" ])
            ]
            [[ "warning: qualifier `_Nonnull` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> foo.c:1:15"
             , "    |"
             , "   1| int *_Nonnull foo(void) { return 0; }"
             , "    |               ^^^"
             , "    |               |"
             , "    |               found qualifier here"
             , "   ::: tox.h:1:1"
             , "    |"
             , "   1| int *_Nonnull foo(void);"
             , "    | ^^^^^^^^^^^^^^^^^^^^^^"
             , "    | |"
             , "    | should be added here instead"
             ]]

    it "allows unannotated definitions if already present in public header decl" $ do
        shouldAccept ["ownership-decls"]
            [ ("tox.h", [ "int *_Nonnull foo(void);" ])
            , ("foo.c", [ "int *foo(void) { return 0; }" ])
            ]

    it "accepts annotations on array typedefs" $ do
        shouldAccept ["ownership-decls"]
            [ ("types.h", [ "typedef uint8_t Tox_Public_Key[32];" ])
            , ("foo.h", [ "void foo(Tox_Public_Key _Nonnull pk);" ])
            ]

    it "recognizes function type typedefs as pointer types and expects annotations" $ do
        shouldWarn ["ownership-decls"]
            [ ("types.h", [ "typedef void my_cb(int p);" ])
            , ("foo.h", [ "void foo(my_cb cb);" ])
            ]
            [[ "warning: pointer type `my_cb` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> foo.h:1:16"
             , "    |"
             , "   1| void foo(my_cb cb);"
             , "    |                ^^"
             , "    |                |"
             , "    |                missing annotation here"
             ]]

    it "accepts annotations on function type typedefs" $ do
        shouldAccept ["ownership-decls"]
            [ ("types.h", [ "typedef void my_cb(int p);" ])
            , ("foo.h", [ "void foo(my_cb _Nonnull cb);" ])
            ]

    it "allows annotations in .c file even if the public typedef is collected after the usage" $ do
        -- Here tox.h (typedef) comes after events.h (usage)
        shouldAccept ["ownership-decls"]
            [ ("events.h", [ "public_cb handle_event;" ])
            , ("tox.h", [ "typedef void public_cb(int *p);" ])
            , ("implementation.c", [ "void handle_event(int *_Nonnull p) { /* p */ }" ])
            ]

    it "allows annotations in .c file when implementing a public callback typedef" $ do
        shouldAccept ["ownership-decls"]
            [ ("tox.h", [ "typedef void public_cb(int *p);" ])
            , ("events.h", [ "public_cb handle_event;" ])
            , ("implementation.c", [ "void handle_event(int *_Nonnull p) { /* p */ }" ])
            ]

    it "forbids annotations in .c file when implementing an internal callback typedef" $ do
        shouldWarn ["ownership-decls"]
            [ ("internal.h", [ "typedef void internal_cb(int *p);" ])
            , ("events.h", [ "internal_cb handle_event;" ])
            , ("implementation.c", [ "void handle_event(int *_Nonnull p) { /* p */ }" ])
            ]
            [[ "warning: pointer type `internal_cb handle_event` should have an explicit nullability annotation (`_Nullable` or `_Nonnull`) [-Wownership-decls]"
             , "   --> events.h:1:13"
             , "    |"
             , "   1| internal_cb handle_event;"
             , "    |             ^^^^^^^^^^^^"
             , "    |             |"
             , "    |             missing annotation here"
             , "   ::: internal.h:1:9"
             , "    |"
             , "   1| typedef void internal_cb(int *p);"
             , "    |         ^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |         |"
             , "    |         because declaration here is unannotated"
             ]
            ,[ "warning: qualifier `_Nonnull` should only be used on function declarations, not definitions [-Wownership-decls]"
             , "   --> implementation.c:1:6"
             , "    |"
             , "   1| void handle_event(int *_Nonnull p) { /* p */ }"
             , "    |      ^^^^^^^^^^^^"
             , "    |      |"
             , "    |      found qualifier here"
             , "   ::: internal.h:1:9"
             , "    |"
             , "   1| typedef void internal_cb(int *p);"
             , "    |         ^^^^^^^^^^^^^^^^^^^^^^^"
             , "    |         |"
             , "    |         should be added here instead"
             ]]

-- end of tests
