{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.StrictTypedefSpec (spec) where
import           Data.Text             (Text)
import           Test.Hspec            (Spec, it)

import qualified Data.Text             as Text
import           Tokstyle.C.LinterSpec (shouldAccept, shouldWarn)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarn ["strict-typedef"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAccept ["strict-typedef"]


spec :: Spec
spec = do
    it "warns when passing an underlying type to a strict typedef" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "void f(Friend_Number n);"
            , "void g(int n) {"
            , "    f(n);"
            , "}"
            ]
            [[ "error: passing int to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:4:7"
             , "    |"
             , "   4|     f(n);"
             , "    |       ^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "warns when passing a strict typedef to another strict typedef" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "typedef int Group_Number;"
            , "void f(Friend_Number n);"
            , "void g(Group_Number n) {"
            , "    f(n);"
            , "}"
            ]
            [[ "error: passing Group_Number to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:5:7"
             , "    |"
             , "   5|     f(n);"
             , "    |       ^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found Group_Number (aka int)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "warns when returning the wrong type" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "Friend_Number f(int n) {"
            , "    return n;"
            , "}"
            ]
            [[ "error: returning int from a function expecting Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:3:12"
             , "    |"
             , "   3|     return n;"
             , "    |            ^"
             , "    |            |"
             , "    |            expected Friend_Number (aka int)"
             , "    |               found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "warns on assignment mismatch" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "void f(int n) {"
            , "    Friend_Number m;"
            , "    m = n;"
            , "}"
            ]
            [[ "error: assigning int to variable `m` of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:4:9"
             , "    |"
             , "   4|     m = n;"
             , "    |         ^"
             , "    |         |"
             , "    |         expected Friend_Number (aka int)"
             , "    |            found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "allows passing a struct to a typedef of the same name" $ do
        shouldAccept'
            [ "struct Foo { int x; };"
            , "typedef struct Foo Foo;"
            , "void set_foo(Foo f);"
            , "void test(struct Foo f) {"
            , "  set_foo(f);"
            , "}"
            ]

    it "allows passing a struct to a typedef of the same name and vice-versa" $ do
        shouldAccept'
            [ "enum Bar { A, B };"
            , "typedef enum Bar Bar;"
            , "void set_bar(Bar b);"
            , "void set_enum_bar(enum Bar b);"
            , "void test(Bar b, enum Bar eb) {"
            , "  set_bar(eb);"
            , "  set_enum_bar(b);"
            , "}"
            ]

    it "allows passing a struct to a typedef of a DIFFERENT name" $ do
        shouldAccept'
            [ "struct Foo { int x; };"
            , "typedef struct Foo Foo_t;"
            , "void set_foo(Foo_t f);"
            , "void test(struct Foo f) {"
            , "  set_foo(f);"
            , "}"
            ]

    it "allows passing 0 or 1 to a strict typedef" $ do
        shouldAccept'
            [ "typedef int Friend_Number;"
            , "void set_friend(Friend_Number n);"
            , "void test(void) {"
            , "  set_friend(0);"
            , "  set_friend(1);"
            , "}"
            ]

    it "allows passing a strict typedef to its underlying type" $ do
        shouldAccept'
            [ "typedef int Friend_Number;"
            , "void set_int(int n);"
            , "void test(Friend_Number n) {"
            , "  set_int(n);"
            , "}"
            ]

    it "allows mixing standard types like uint32_t with underlying types" $ do
        shouldAccept'
            [ "typedef unsigned int uint32_t;"
            , "void set_u32(uint32_t n);"
            , "void test(unsigned int x) {"
            , "  set_u32(x);"
            , "}"
            ]

    it "allows const variations of strict typedefs" $ do
        shouldAccept'
            [ "typedef struct Foo *Foo_ptr;"
            , "void set_foo(Foo_ptr f);"
            , "void test(Foo_ptr const f) {"
            , "  set_foo(f);"
            , "}"
            ]

    it "warns on mismatched pointers to strict typedefs" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "typedef int Group_Number;"
            , "void f(Friend_Number *n);"
            , "void g(Group_Number *n) {"
            , "    f(n);"
            , "}"
            ]
            [[ "error: passing Group_Number to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:5:7"
             , "    |"
             , "   5|     f(n);"
             , "    |       ^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found Group_Number (aka int)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "allows passing a function to a typedef of its type" $ do
        shouldAccept'
            [ "typedef void (cb_t)(int);"
            , "void reg(cb_t *f);"
            , "void my_f(int x) {}"
            , "void test(void) {"
            , "  reg(my_f);"
            , "}"
            ]

    it "allows passing a pointer to a struct to a typedef of that struct pointer" $ do
        shouldAccept'
            [ "struct Tox; typedef struct Tox Tox;"
            , "typedef void (*tox_log_cb)(Tox *tox, void *user_data);"
            , "void tox_callback_log(Tox *tox, tox_log_cb cb, void *user_data);"
            , "void my_log(Tox *tox, void *user_data) {}"
            , "void test(Tox *tox) {"
            , "  tox_callback_log(tox, my_log, 0);"
            , "}"
            ]

    it "warns on passing different strict typedefs even if they are both functions" $ do
        shouldWarn'
            [ "typedef void cb1_t(int);"
            , "typedef void cb2_t(int);"
            , "void g(cb1_t *f);"
            , "void h(cb2_t *f) {"
            , "    g(f);"
            , "}"
            ]
            [[ "error: incompatible callback role [-Wstrict-typedef]"
             , "   --> test.c:5:7"
             , "    |"
             , "   5|     g(f);"
             , "    |       ^"
             , "    |       |"
             , "    |       expected cb1_t (aka void(int)*)"
             , "    |          found cb2_t (aka void(int)*)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "allows anonymous enums matched by typedefs" $ do
        shouldAccept'
            [ "typedef enum { ERR_OK } err_t;"
            , "void check(err_t e);"
            , "void test(void) {"
            , "  check(ERR_OK);"
            , "}"
            ]

    it "allows mixing standard types with their internal versions" $ do
        shouldAccept'
            [ "typedef unsigned long size_t;"
            , "typedef unsigned long __size_t;"
            , "void set_size(size_t s);"
            , "void test(__size_t s) {"
            , "  set_size(s);"
            , "}"
            ]

    it "allows explicit C-style casts from the underlying type" $ do
        shouldAccept'
            [ "typedef int Friend_Number;"
            , "void test(int x) {"
            , "  Friend_Number f;"
            , "  f = (Friend_Number)x;"
            , "}"
            ]

    it "forbids explicit C-style casts between different strict typedefs" $ do
        shouldWarn ["strict-typedef", "cast"]
            [ "struct Key { unsigned char data[32]; };"
            , "typedef struct Key Key;"
            , "typedef Key Public_Key;"
            , "typedef Key Secret_Key;"
            , "void set_public(Public_Key k);"
            , "void test(Secret_Key sk) {"
            , "  set_public((Public_Key)sk);"
            , "}"
            ]
            [[ "error: strict typedef mismatch [-Wstrict-typedef]"
             , "   --> test.c:7:26"
             , "    |"
             , "   7|   set_public((Public_Key)sk);"
             , "    |                          ^^"
             , "    |                          |"
             , "    |                          expected Public_Key (aka struct Key)"
             , "    |                             found Secret_Key (aka struct Key)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "allows explicit casts from void* to pointers to strict typedefs" $ do
        shouldAccept'
            [ "typedef struct Foo Foo;"
            , "void test(void *p) {"
            , "  Foo *f = (Foo *)p;"
            , "}"
            ]

    it "allows mixing opus types with underlying types" $ do
        shouldAccept'
            [ "typedef short opus_int16;"
            , "typedef int opus_int32;"
            , "void set_i16(opus_int16 n);"
            , "void set_i32(opus_int32 n);"
            , "void test(short i16, int i32) {"
            , "  set_i16(i16);"
            , "  set_i32(i32);"
            , "}"
            ]

    it "treats an assignment-based implicit_cast as a safe cast (ALLOWED)" $ do
        shouldAccept'
            [ "typedef int Friend_Number;"
            , "typedef int Group_Number;"
            , "void test(Group_Number g) {"
            , "  Friend_Number f;"
            , "  f = (Friend_Number){g};"
            , "}"
            ]

    it "STRICTLY forbids explicit C-style casts between different strict typedefs" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "typedef int Group_Number;"
            , "void f(Group_Number n) {"
            , "    Friend_Number m;"
            , "    m = (Friend_Number)n;"
            , "}"
            ]
            [[ "error: strict typedef mismatch [-Wstrict-typedef]"
             , "   --> test.c:5:24"
             , "    |"
             , "   5|     m = (Friend_Number)n;"
             , "    |                        ^"
             , "    |                        |"
             , "    |                        expected Friend_Number (aka int)"
             , "    |                           found Group_Number (aka int)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "warns on strict typedef mismatch in a callback parameter" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "typedef void (*cb_t)(int n, Friend_Number x);"
            , "void reg(cb_t f);"
            , "void my_cb(int n, int x) {}"
            , "void test(void) {"
            , "  reg(my_cb);"
            , "}"
            ]
            [[ "error: incompatible callback type for parameter 'f' [-Wstrict-typedef]"
             , "   --> test.c:6:7"
             , "    |"
             , "   6|   reg(my_cb);"
             , "    |       ^^^^^"
             , "    |       |"
             , "    |       expected cb_t"
             , "    |          found void(int, int)"
             , "    = note: mismatch in argument 2 ('x'):"
             , "               expected Friend_Number (aka int)"
             , "                  found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "treats the main typedef as lenient but additional ones as strict" $ do
        shouldWarn'
            [ "struct Key { unsigned char data[32]; };"
            , "typedef struct Key Key;"
            , "typedef Key Public_Key;"
            , "void set_key(Key k);"
            , "void set_public(Public_Key k);"
            , "void test(struct Key k, Key ky, Public_Key pk) {"
            , "  set_key(k);"       -- ALLOW (main typedef)
            , "  set_public(k);"    -- DENY (strict mismatch)
            , "  set_key(pk);"      -- DENY (strict mismatch)
            , "  set_public(ky);"   -- DENY (strict mismatch)
            , "}"
            ]
            [[ "error: passing struct Key to parameter 'k' of type Public_Key [-Wstrict-typedef]"
             , "   --> test.c:8:14"
             , "    |"
             , "   8|   set_public(k);"
             , "    |              ^"
             , "    |              |"
             , "    |              expected Public_Key (aka struct Key)"
             , "    |                 found struct Key"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]
            ,[ "error: passing Public_Key to parameter 'k' of type Key [-Wstrict-typedef]"
             , "   --> test.c:9:11"
             , "    |"
             , "   9|   set_key(pk);"
             , "    |           ^^"
             , "    |           |"
             , "    |           expected Key (aka struct Key)"
             , "    |              found Public_Key (aka struct Key)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]
            ,[ "error: passing Key to parameter 'k' of type Public_Key [-Wstrict-typedef]"
             , "   --> test.c:10:14"
             , "    |"
             , "  10|   set_public(ky);"
             , "    |              ^^"
             , "    |              |"
             , "    |              expected Public_Key (aka struct Key)"
             , "    |                 found Key (aka struct Key)"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "treats multiple typedefs as strict if none match the struct name" $ do
        shouldWarn'
            [ "struct Foo { int x; };"
            , "typedef struct Foo Bar;"
            , "typedef struct Foo Baz;"
            , "void set_bar(Bar b);"
            , "void set_baz(Baz b);"
            , "void test(struct Foo f) {"
            , "  set_bar(f);"
            , "  set_baz(f);"
            , "}"
            ]
            [[ "error: passing struct Foo to parameter 'b' of type Bar [-Wstrict-typedef]"
             , "   --> test.c:7:11"
             , "    |"
             , "   7|   set_bar(f);"
             , "    |           ^"
             , "    |           |"
             , "    |           expected Bar (aka struct Foo)"
             , "    |              found struct Foo"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]
            ,[ "error: passing struct Foo to parameter 'b' of type Baz [-Wstrict-typedef]"
             , "   --> test.c:8:11"
             , "    |"
             , "   8|   set_baz(f);"
             , "    |           ^"
             , "    |           |"
             , "    |           expected Baz (aka struct Foo)"
             , "    |              found struct Foo"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "shows mismatch for a specific parameter in a multi-parameter call" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "void f(int a, Friend_Number n);"
            , "void g(int a, int n) {"
            , "    f(a, n);"
            , "}"
            ]
            [[ "error: passing int to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:4:10"
             , "    |"
             , "   4|     f(a, n);"
             , "    |          ^"
             , "    |          |"
             , "    |          expected Friend_Number (aka int)"
             , "    |             found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "shows correct underline for binary expressions with longer operands" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "void f(Friend_Number n);"
            , "void test(int longer_var, int b) {"
            , "    f(longer_var + b);"
            , "}"
            ]
            [[ "error: passing int to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:4:7"
             , "    |"
             , "   4|     f(longer_var + b);"
             , "    |       ^^^^^^^^^^^^^^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "shows correct underline for longer variables" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "void f(Friend_Number n);"
            , "void test(int friend) {"
            , "    f(friend);"
            , "}"
            ]
            [[ "error: passing int to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:4:7"
             , "    |"
             , "   4|     f(friend);"
             , "    |       ^^^^^^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "shows correct underline for binary expressions" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "void f(Friend_Number n);"
            , "void test(int a, int b) {"
            , "    f(a + b);"
            , "}"
            ]
            [[ "error: passing int to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:4:7"
             , "    |"
             , "   4|     f(a + b);"
             , "    |       ^^^^^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]

    it "allows explicit casts from int to strict typedef" $ do
        shouldAccept'
            [ "typedef unsigned int Conference_Number;"
            , "void f(Conference_Number n);"
            , "void g(int n) {"
            , "    f((Conference_Number)n);"
            , "}"
            ]

    it "shows correct underline for member access" $ do
        shouldWarn'
            [ "typedef int Friend_Number;"
            , "struct Call { int friend_number; };"
            , "void f(Friend_Number n);"
            , "void test(struct Call *call) {"
            , "    f(call->friend_number);"
            , "}"
            ]
            [[ "error: passing int to parameter 'n' of type Friend_Number [-Wstrict-typedef]"
             , "   --> test.c:5:7"
             , "    |"
             , "   5|     f(call->friend_number);"
             , "    |       ^^^^^^^^^^^^^^^^^^^"
             , "    |       |"
             , "    |       expected Friend_Number (aka int)"
             , "    |          found int"
             , "    = note: strict typedefs prevent accidental mixing of logically distinct types"
             , "    = help: if this is intentional, use an explicit cast"
             ]]
