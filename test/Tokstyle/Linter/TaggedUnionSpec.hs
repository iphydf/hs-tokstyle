{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TaggedUnionSpec (spec) where

import           Data.Text           (Text)
import           Test.Hspec          (Spec, describe, it)

import           Tokstyle.LinterSpec (shouldAcceptLocal, shouldWarnLocal)


shouldWarn' :: [Text] -> [[Text]] -> IO ()
shouldWarn' = shouldWarnLocal ["tagged-union"]


shouldAccept' :: [Text] -> IO ()
shouldAccept' = shouldAcceptLocal ["tagged-union"]


spec :: Spec
spec = do
    describe "Tagged union linter" $ do
        it "detects untagged unions with pointers" $ do
            shouldWarn'
                [ "typedef union Untagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Untagged;"
                , "struct My_Struct {"
                , "    Untagged u;"
                , "};"
                ]
                [[ "warning: union `Untagged` must be tagged in a struct [-Wtagged-union]"
                 , "   --> test.c:1:15"
                 , "    |"
                 , "   1| typedef union Untagged {"
                 , "    |               ^^^^^^^^^^"
                 ]]

        it "accepts tagged unions" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                ]

        it "detects unguarded access to tagged union member" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    char *p = st->u.ptr;"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:11:21"
                 , "    |"
                 , "  11|     char *p = st->u.ptr;"
                 , "    |                     ^^^"
                 ]]

        it "accepts guarded access in if statement" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_PTR) {"
                , "        char *p = st->u.ptr;"
                , "    }"
                , "}"
                ]

        it "accepts guarded access in switch statement" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    switch (st->tag) {"
                , "    case TAG_PTR:"
                , "        { char *p = st->u.ptr; }"
                , "    }"
                , "}"
                ]

        it "detects access with the WRONG tag check" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_INT) {"
                , "        char *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                 , "   --> test.c:12:25"
                 , "    |"
                 , "  12|         char *p = st->u.ptr;"
                 , "    |                         ^^^"
                 ]]

        it "detects unguarded access even if another member is guarded (updated expectation)" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_INT) {"
                , "        char *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                 , "   --> test.c:12:25"
                 , "    |"
                 , "  12|         char *p = st->u.ptr;"
                 , "    |                         ^^^"
                 ]]

        it "accepts assignment-based tagging for union creation" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->tag = TAG_PTR;"
                , "    st->u.ptr = 0;"
                , "}"
                ]

        it "detects incorrect assignment-based tagging" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->tag = TAG_INT;"
                , "    st->u.ptr = 0;"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                 , "   --> test.c:12:11"
                 , "    |"
                 , "  12|     st->u.ptr = 0;"
                 , "    |           ^^^"
                 ]]

        it "detects access with wrong tag after assignment" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->tag = TAG_INT;"
                , "    char *p = st->u.ptr;"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `TAG_PTR` [-Wtagged-union]"
                 , "   --> test.c:12:21"
                 , "    |"
                 , "  12|     char *p = st->u.ptr;"
                 , "    |                     ^^^"
                 ]]

        it "detects access before tag assignment" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    st->u.ptr = 0;"
                , "    st->tag = TAG_PTR;"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:11:11"
                 , "    |"
                 , "  11|     st->u.ptr = 0;"
                 , "    |           ^^^"
                 ]]

        it "detects enum members matching union members out of order" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_B, TAG_A } Tag;"
                , "typedef union My_Union {"
                , "    int a;"
                , "    char *b;"
                , "} My_Union;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    My_Union u;"
                , "};"
                ]
                -- Enum is B, A. Union is A, B. Order mismatch.
                [[ "warning: order of members in union `My_Union` should be changed to `b, a` to match enum `Tag` [-Wtagged-union]"
                 , "   --> test.c:6:8"
                 , "    |"
                 , "   6| struct My_Struct {"
                 , "    |        ^^^^^^^^^^^"
                 ]]

        it "detects unguarded access to truth member" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    char *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->u.a) { /* empty */ }"
                , "}"
                ]
                [[ "warning: access to union member `a` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:11:15"
                 , "    |"
                 , "  11|     if (st->u.a) { /* empty */ }"
                 , "    |               ^"
                 ]]

        it "detects unguarded access to union member" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    char *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    char *p = st->u.a;"
                , "    st->u.a = (char*)1;"
                , "}"
                ]
                [[ "warning: access to union member `a` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:11:21"
                 , "    |"
                 , "  11|     char *p = st->u.a;"
                 , "    |                     ^"
                 ]
                ,[ "warning: access to union member `a` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:12:11"
                 , "    |"
                 , "  12|     st->u.a = (char*)1;"
                 , "    |           ^"
                 ]]

        it "detects out-of-order enum and union members" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_INT, TAG_PTR, TAG_UNUSED } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                ]
                [[ "warning: order of members in union `Tagged` should be changed to `i, ptr` to match enum `Tag` [-Wtagged-union]"
                 , "   --> test.c:6:8"
                 , "    |"
                 , "   6| struct My_Struct {"
                 , "    |        ^^^^^^^^^^^"
                 ]]

        it "supports switch on struct member tag" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st, Tag t) {"
                , "    st->tag = t;"
                , "    switch (st->tag) {"
                , "    case TAG_PTR: {"
                , "        st->u.ptr = 0;"
                , "        break;"
                , "    }"
                , "    }"
                , "}"
                ]

        it "ignores unknown structs even if union member name matches" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct Known {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct Unknown *st) {"
                , "    void *p = st->u.ptr;"
                , "}"
                ]

        it "ignores known untagged structs even if union member name matches" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct Known {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "struct Untagged_Struct {"
                , "    Tagged u;"
                , "};"
                , "void test(struct Untagged_Struct *st) {"
                , "    void *p = st->u.ptr;"
                , "}"
                ]

        it "handles logical AND in condition" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_PTR && st->u.ptr != 0) {"
                , "        void *p = st->u.ptr;"
                , "    }"
                , "}"
                ]

        it "handles logical OR in condition" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR1, TAG_PTR2, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_PTR1 || st->tag == TAG_PTR2) {"
                , "        void *p = st->u.ptr;"
                , "    }"
                , "}"
                ]

        it "handles early return" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag != TAG_PTR) return;"
                , "    void *p = st->u.ptr;"
                , "}"
                ]

        it "detects access when OR condition is partially unrelated" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st, int other) {"
                , "    if (st->tag == TAG_PTR || other) {"
                , "        void *p = st->u.ptr;"
                , "    }"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:12:25"
                 , "    |"
                 , "  12|         void *p = st->u.ptr;"
                 , "    |                         ^^^"
                 ]]

        it "supports guards in while loop condition" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    while (st->tag == TAG_PTR) {"
                , "        void *p = st->u.ptr;"
                , "        st->tag = TAG_INT;"
                , "    }"
                , "}"
                ]

        it "supports guards in for loop condition" $ do
            shouldAccept'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    for (int i = 0; st->tag == TAG_PTR; ++i) {"
                , "        void *p = st->u.ptr;"
                , "        st->tag = TAG_INT;"
                , "    }"
                , "}"
                ]

        it "detects access after re-assignment to unknown value" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_PTR, TAG_INT } Tag;"
                , "typedef union Tagged {"
                , "    char *ptr;"
                , "    int i;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st, Tag t) {"
                , "    st->tag = TAG_PTR;"
                , "    st->tag = t;"
                , "    void *p = st->u.ptr;"
                , "}"
                ]
                [[ "warning: access to union member `ptr` is not guarded by a check on `tag` [-Wtagged-union]"
                 , "   --> test.c:13:21"
                 , "    |"
                 , "  13|     void *p = st->u.ptr;"
                 , "    |                     ^^^"
                 ]]

        it "exempts IP_Union" $ do
            shouldAccept'
                [ "typedef union IP4 { uint32_t u32; } IP4;"
                , "typedef union IP6 { uint8_t u8[16]; } IP6;"
                , "typedef union IP_Union {"
                , "    IP4 v4;"
                , "    IP6 v6;"
                , "} IP_Union;"
                , "struct IP {"
                , "    int family;"
                , "    IP_Union ip;"
                , "};"
                ]

        it "exempts compatible types (same size, no pointers)" $ do
            shouldAccept'
                [ "typedef union Compatible_Union {"
                , "    uint32_t u32;"
                , "    uint16_t u16[2];"
                , "    uint8_t u8[4];"
                , "} Compatible_Union;"
                , "struct My_Struct {"
                , "    Compatible_Union c;"
                , "};"
                ]

        it "detects incompatible types (different sizes, no pointers)" $ do
            shouldWarn'
                [ "typedef union Incompatible_Union {"
                , "    uint32_t u32;"
                , "    uint64_t u64;"
                , "} Incompatible_Union;"
                , "struct My_Struct {"
                , "    Incompatible_Union i;"
                , "};"
                ]
                [[ "warning: union `Incompatible_Union` must be tagged in a struct [-Wtagged-union]"
                 , "   --> test.c:1:15"
                 , "    |"
                 , "   1| typedef union Incompatible_Union {"
                 , "    |               ^^^^^^^^^^^^^^^^^^^^"
                 ]]

        it "detects same-size pointers as incompatible (requiring tagging)" $ do
            shouldWarn'
                [ "typedef union Pointer_Union {"
                , "    void *ptr1;"
                , "    char *ptr2;"
                , "} Pointer_Union;"
                , "struct My_Struct {"
                , "    Pointer_Union u;"
                , "};"
                ]
                [[ "warning: union `Pointer_Union` must be tagged in a struct [-Wtagged-union]"
                 , "   --> test.c:1:15"
                 , "    |"
                 , "   1| typedef union Pointer_Union {"
                 , "    |               ^^^^^^^^^^^^^^^"
                 ]]

        it "detects incompatible sizes (uint32_t, uint8_t[3])" $ do
            shouldWarn'
                [ "typedef union Incompatible_Size {"
                , "    uint32_t u32;"
                , "    uint8_t u8[3];"
                , "} Incompatible_Size;"
                , "struct My_Struct {"
                , "    Incompatible_Size i;"
                , "};"
                ]
                [[ "warning: union `Incompatible_Size` must be tagged in a struct [-Wtagged-union]"
                 , "   --> test.c:1:15"
                 , "    |"
                 , "   1| typedef union Incompatible_Size {"
                 , "    |               ^^^^^^^^^^^^^^^^^^^"
                 ]]

        it "detects same-size struct as incompatible (requiring tagging)" $ do
            shouldWarn'
                [ "typedef struct Small_Struct { uint32_t x; } Small_Struct;"
                , "typedef union Same_Size_Union {"
                , "    Small_Struct s;"
                , "    uint32_t i;"
                , "} Same_Size_Union;"
                , "struct My_Struct {"
                , "    Same_Size_Union u;"
                , "};"
                ]
                [[ "warning: union `Same_Size_Union` must be tagged in a struct [-Wtagged-union]"
                 , "   --> test.c:2:15"
                 , "    |"
                 , "   2| typedef union Same_Size_Union {"
                 , "    |               ^^^^^^^^^^^^^^^^^"
                 ]]

        it "detects void pointers in tagged unions" $ do
            shouldWarn'
                [ "typedef enum Tag { TAG_A, TAG_B } Tag;"
                , "typedef union Tagged {"
                , "    void *a;"
                , "    int b;"
                , "} Tagged;"
                , "struct My_Struct {"
                , "    Tag tag;"
                , "    Tagged u;"
                , "};"
                , "void test(struct My_Struct *st) {"
                , "    if (st->tag == TAG_A) {"
                , "        void *p = st->u.a;"
                , "    }"
                , "}"
                ]
                [[ "warning: union `Tagged` contains a void pointer: `a` [-Wtagged-union]"
                 , "   --> test.c:6:8"
                 , "    |"
                 , "   6| struct My_Struct {"
                 , "    |        ^^^^^^^^^^^"
                 ]]

        it "detects enum as incompatible (requiring tagging)" $ do
            shouldWarn'
                [ "typedef enum My_Enum { VAL_A, VAL_B } My_Enum;"
                , "typedef union Enum_Union {"
                , "    My_Enum e;"
                , "    uint32_t i;"
                , "} Enum_Union;"
                , "struct My_Struct {"
                , "    Enum_Union u;"
                , "};"
                ]
                [[ "warning: union `Enum_Union` must be tagged in a struct [-Wtagged-union]"
                 , "   --> test.c:2:15"
                 , "    |"
                 , "   2| typedef union Enum_Union {"
                 , "    |               ^^^^^^^^^^^^"
                 ]]

-- end of tests
