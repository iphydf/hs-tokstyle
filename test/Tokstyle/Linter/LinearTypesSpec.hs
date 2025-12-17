{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.LinearTypesSpec where

import           Test.Hspec                  (Spec, describe, it, shouldBe)

import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Language.Cimple             as C
import           Tokstyle.Analysis.Linear    (AnalysisState (..),
                                              LinearFacts (..), Path (..))
import           Language.Cimple.Analysis.Scope     (ScopedId (..), dummyScopedId)
import           Tokstyle.Linter             (analyseGlobal)
import           Tokstyle.Linter.LinearTypes
import           Tokstyle.LinterSpec         (mustParse)


import           Tokstyle.Common.TypeSystem  (StdType (..), TypeDescr (..),
                                              TypeInfo (..), TypeRef (..))

spec :: Spec
spec = do
    describe "Linear types linter" $ do
        it "detects use-after-move" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void test(void * owner ptr) {"
                , "  consume(ptr);"
                , "  consume(ptr);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: use-after-move of variable `ptr` [-Wlinear-types]"
                ]

        it "detects leaks of owned variables" $ do
            ast <- mustParse
                [ "void test(void * owner ptr) {"
                , "  ptr = nullptr;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: leak: variable `ptr` is overwritten while holding ownership [-Wlinear-types]"
                ]

        it "detects leaks at function exit" $ do
            ast <- mustParse
                [ "void * owner alloc(void);"
                , "void test() {"
                , "  void * owner p = alloc();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: leak: variable `p` is still owned at function exit [-Wlinear-types]"
                ]

        it "ignores functions without owned types" $ do
            ast <- mustParse
                [ "void test(void *ptr) {"
                , "  ptr = nullptr;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "checks functions called by a function with owned types" $ do
            ast <- mustParse
                [ "void bar(void *ptr) {"
                , "  void *q = ptr;"
                , "  ptr = nullptr;"
                , "}"
                , "void foo(void * owner ptr) {"
                , "  bar(ptr);"
                , "}"
                ]
            -- bar should be checked because it is called by foo (which uses 'owner').
            -- In our current simplified implementation, bar will not warn because
            -- 'ptr' in bar is not 'owner'.
            -- foo should warn because it leaks its owned 'ptr'.
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: leak: variable `ptr` is still owned at function exit [-Wlinear-types]"
                ]

        it "checks functions called by a function with owned types (triggering a warning in callee)" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void bar(void *p) {"
                , "  consume(p);"
                , "}"
                , "void foo(void * owner ptr) {"
                , "  bar(ptr);"
                , "  free(ptr);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: passing non-owned variable `p` to function expecting owner [-Wlinear-types]"
                , "test.c:3: note: this function is being checked because it is reachable from a function that uses 'owner' qualifiers: foo -> bar [-Wlinear-types]"
                ]

    describe "Combinations of qualifiers" $ do
        it "accepts owner _Nullable" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void * owner _Nullable alloc(void);"
                , "void test() {"
                , "  void * owner _Nullable p = alloc();"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "accepts owner _Nonnull" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void * owner _Nonnull alloc(void);"
                , "void test() {"
                , "  void * owner _Nonnull p = alloc();"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "accepts _Nullable owner" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void * _Nullable owner alloc(void);"
                , "void test() {"
                , "  void * _Nullable owner p = alloc();"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "accepts const owner _Nullable" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void * const owner _Nullable alloc(void);"
                , "void test() {"
                , "  void * const owner _Nullable p = alloc();"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

    describe "Advanced features" $ do
        it "correctly handles variable shadowing" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void * owner alloc(void);"
                , "void test(void * owner p) {"
                , "  {"
                , "    void * owner p = alloc();"
                , "    consume(p);"
                , "  }"
                , "  consume(p); // Outer p should still be valid"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects leaks of ignored return values" $ do
            ast <- mustParse
                [ "void * owner alloc(void);"
                , "void test(void * owner p) {"
                , "  alloc(); // Standalone call leaks"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: leak: return value of `alloc` is ignored [-Wlinear-types]"
                ]

        it "prevents move while a borrow exists" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void use(void *ptr);"
                , "void test(void * owner p) {"
                , "  void *b = p; // borrow"
                , "  consume(p);  // move"
                , "  use(b);      // use borrow after move (ERROR)"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: use-after-move: borrow `b` is invalid because `p` was moved [-Wlinear-types]"
                ]

        it "tracks ownership of struct members" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void * owner alloc(void);"
                , "void test(struct Foo *f, void * owner p) {"
                , "  f->data = alloc();"
                , "  free(p);"
                , "} // f->data leaked"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:5: leak: member `f->data` is still owned at function exit [-Wlinear-types]"
                ]

        it "allows early return in if branch with ownership move" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void test(void * owner p, int cond) {"
                , "  if (cond) {"
                , "    consume(p);"
                , "    return;"
                , "  }"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects inconsistent ownership across branches" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void test(void * owner p, int cond) {"
                , "  if (cond) {"
                , "    consume(p);"
                , "  }"
                , "} // p is moved in one branch, but not the other"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: inconsistent ownership: variable `p` is moved in some branches but not others [-Wlinear-types]"
                ]

        it "detects inconsistent ownership in the else branch" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void test(void * owner p, int cond) {"
                , "  if (cond) {"
                , "    /* no move */"
                , "  } else {"
                , "    consume(p);"
                , "  }"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: inconsistent ownership: variable `p` is moved in some branches but not others [-Wlinear-types]"
                ]

        it "detects use of alias after move" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void use(void *ptr);"
                , "void test(void * owner p) {"
                , "  void *alias = p;"
                , "  consume(p);"
                , "  use(alias); // alias is now invalid"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: use-after-move: borrow `alias` is invalid because `p` was moved [-Wlinear-types]"
                ]

        it "does not check functions that call owner-returning functions but don't use owner themselves" $ do
            ast <- mustParse
                [ "void * owner alloc(void);"
                , "void bar(int *p) {"
                , "  int *b1 = p;"
                , "  int *b2 = p;"
                , "}"
                , "void foo(void) {"
                , "  void *p = alloc(); // calls owner-returning, but doesn't use owner"
                , "  bar(nullptr);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "does not check functions that call owner-parameter functions but don't use owner themselves" $ do
            ast <- mustParse
                [ "void consume(void * owner ptr);"
                , "void test(void *p) {"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "does not issue borrow warnings for non-owned variables" $ do
            ast <- mustParse
                [ "void use(int *p);"
                , "void * owner alloc(void);"
                , "void test(int *p) {"
                , "  void * owner q = alloc();"
                , "  int *b1 = p;"
                , "  int *b2 = p;"
                , "  use(b1);"
                , "  use(b2);"
                , "  free(q);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "prevents multiple mutable borrows" $ do
            ast <- mustParse
                [ "void use(void *ptr);"
                , "void test(void * owner p) {"
                , "  void *b1 = p;"
                , "  void *b2 = p;"
                , "  use(b1);"
                , "  use(b2);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: cannot borrow `p` as mutable because it is already borrowed [-Wlinear-types]"
                , "test.c:6: leak: variable `p` is still owned at function exit [-Wlinear-types]"
                ]

        it "allows multiple immutable borrows" $ do
            ast <- mustParse
                [ "void use(const void *ptr);"
                , "void test(void * owner p) {"
                , "  const void *b1 = p;"
                , "  const void *b2 = p;"
                , "  use(b1);"
                , "use(b2);"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "prevents mutable borrow after immutable borrow" $ do
            ast <- mustParse
                [ "void use(const void *ptr);"
                , "void test(void * owner p) {"
                , "  const void *b1 = p;"
                , "  void *b2 = p;"
                , "  use(b1);"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: cannot borrow `p` as mutable because it is already borrowed [-Wlinear-types]"
                ]

    describe "Return value validation" $ do
        it "warns when returning a non-owned pointer from an owner-returning function" $ do
            ast <- mustParse
                [ "void * get_ptr(void);"
                , "void * owner test() {"
                , "  void *p = get_ptr();"
                , "  return p;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: returning non-owned value from a function that returns `owner` [-Wlinear-types]"
                ]

        it "allows returning an owner parameter" $ do
            ast <- mustParse
                [ "void * owner test(void * owner p) {"
                , "  return p;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows returning a NullChecked owned variable" $ do
            ast <- mustParse
                [ "void * owner test(void * owner p) {"
                , "  if (p) { /* check */ }"
                , "  return p;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows returning newly allocated memory" $ do
            ast <- mustParse
                [ "void * owner alloc(void);"
                , "void * owner test() {"
                , "  return alloc();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows returning nullptr from an owner-returning function" $ do
            ast <- mustParse
                [ "void * owner test() {"
                , "  return nullptr;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

    describe "Soundness across declarations" $ do
        it "warns when implementation returns non-owned value but decl says owner" $ do
            decl <- mustParse ["void * owner test(void);"]
            defn <- mustParse
                [ "void * get_ptr(void);"
                , "void * test() {"
                , "  return get_ptr();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.h", decl), ("test.c", defn)]
                `shouldBe`
                [ "test.c:3: returning non-owned value from a function that returns `owner` [-Wlinear-types]"
                ]

    describe "Standard Library and Function Pointers" $ do
        it "recognizes malloc as returning ownership" $ do
            ast <- mustParse
                [ "void * malloc(uint32_t size);"
                , "void * owner test() {"
                , "  return malloc(10);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership through function pointer returns" $ do
            ast <- mustParse
                [ "typedef void * owner alloc_cb(void);"
                , "void * owner test(alloc_cb *f) {"
                , "  return f();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership through explicit function pointer dereference" $ do
            ast <- mustParse
                [ "typedef void * owner alloc_cb(void);"
                , "void * owner test(alloc_cb *f) {"
                , "  return (*f)();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership through struct member function calls" $ do
            ast <- mustParse
                [ "typedef void * owner alloc_cb(void);"
                , "struct Allocator { alloc_cb *alloc; };"
                , "void * owner test(struct Allocator *a) {"
                , "  return a->alloc();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership through nested struct member function calls" $ do
            ast <- mustParse
                [ "typedef void * owner alloc_cb(void);"
                , "struct Inner { alloc_cb *alloc; };"
                , "struct Outer { struct Inner *inner; };"
                , "void * owner test(struct Outer *o) {"
                , "  return o->inner->alloc();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "recognizes free as consuming ownership" $ do
            ast <- mustParse
                [ "void free(void *ptr);"
                , "void test(void * owner ptr) {"
                , "  free(ptr);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership when passed to a function pointer in a struct" $ do
            ast <- mustParse
                [ "typedef void consume_cb(void * owner p);"
                , "struct Foo { consume_cb *consume; };"
                , "void test(struct Foo *f, void * owner p) {"
                , "  f->consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership in mem_vrealloc-like situation" $ do
            ast <- mustParse
                [ "void mem_delete(void * owner p);"
                , "void * owner tox_memory_realloc(void * owner p);"
                , "void * owner test(void * owner p, int cond) {"
                , "  if (cond) {"
                , "    mem_delete(p);"
                , "    return nullptr;"
                , "  }"
                , "  void * owner q = tox_memory_realloc(p);"
                , "  return q;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows early return when a pointer is NULL" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  if (p == nullptr) {"
                , "    return;"
                , "  }"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects use-after-free even after NULL check" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void use(void *ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  if (p == nullptr) {"
                , "    return;"
                , "  }"
                , "  free(p);"
                , "  use(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` ["test.c:10: use-after-move of variable `p` [-Wlinear-types]"]

        it "detects leak even after NULL check" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  if (p == nullptr) {"
                , "    return;"
                , "  }"
                , "  // leak p"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` ["test.c:4: leak: variable `p` is still owned at function exit [-Wlinear-types]"]

        it "allows early return when a pointer is NULL (with !=)" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  if (p != nullptr) {"
                , "    free(p);"
                , "  } else {"
                , "    return;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows early return with complex NULL check (OR)" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void test(int cond) {"
                , "  void * owner p = malloc(10);"
                , "  if (p == nullptr || cond) {"
                , "    return;"
                , "  }"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows early return with complex NULL check (AND)" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void test(int cond) {"
                , "  void * owner p = malloc(10);"
                , "  if (p != nullptr && cond) {"
                , "    free(p);"
                , "    return;"
                , "  }"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows early return when using pointer as boolean" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  if (!p) {"
                , "    return;"
                , "  }"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows early return when using pointer directly in if" $ do
            ast <- mustParse
                [ "void * malloc(size_t size);"
                , "void free(void * owner ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  if (p) {"
                , "    free(p);"
                , "  } else {"
                , "    return;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership when passed as the first argument" $ do
            ast <- mustParse
                [ "void * owner my_realloc(void * owner p, void * mem);"
                , "void * owner test(void * mem, void * owner p) {"
                , "  void * owner q = my_realloc(p, mem);"
                , "  return q;"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

    describe "Struct and Union member ownership" $ do
        it "allows moving an owner into a struct member" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void free_foo(struct Foo *f);"
                , "void consume(void * owner p);"
                , "void test(void * owner p) {"
                , "  struct Foo f = { nullptr };"
                , "  f.data = p;"
                , "  consume(f.data);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects leak of struct member owner" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void * owner alloc(void);"
                , "void test(void * owner p) {"
                , "  struct Foo f;"
                , "  f.data = alloc();"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` ["test.c:6: leak: member `f.data` is still owned at function exit [-Wlinear-types]"]

        it "allows moving an owner from a struct member" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void consume(void * owner p);"
                , "void test(struct Foo * owner f) {"
                , "  consume(f->data);"
                , "  free(f);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership in unions" $ do
            ast <- mustParse
                [ "typedef union My_Union { void * owner p; int i; } My_Union;"
                , "void consume(void * owner p);"
                , "void test(void * owner p) {"
                , "  My_Union u;"
                , "  u.p = p;"
                , "  consume(u.p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

    describe "Out-parameters" $ do
        it "allows assigning an owner to an out-parameter" $ do
            ast <- mustParse
                [ "void * owner alloc(void);"
                , "void test(void * owner *out) {"
                , "  *out = alloc();"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects leak when out-parameter is not assigned" $ do
            ast <- mustParse
                [ "void * owner alloc(void);"
                , "void test(void * owner *out) {"
                , "  void * owner p = alloc();"
                , "  // leak p"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` ["test.c:3: leak: variable `p` is still owned at function exit [-Wlinear-types]"]

    describe "Pointer arithmetic" $ do
        it "prevents arithmetic on owner pointers" $ do
            ast <- mustParse
                [ "void free(void * owner ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  ++p;"
                , "  free(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: cannot perform pointer arithmetic on an `owner` pointer [-Wlinear-types]"
                ]

        it "treats pointer arithmetic as a borrow" $ do
            ast <- mustParse
                [ "void free(void * owner ptr);"
                , "void use(void *ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  void *q = p + 1;"
                , "  free(p);"
                , "  use(q); // q is invalid because p was moved"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: use-after-move: borrow `q` is invalid because `p` was moved [-Wlinear-types]"
                ]

        it "transitively tracks borrows through arithmetic" $ do
            ast <- mustParse
                [ "void free(void * owner ptr);"
                , "void use(void *ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  void *q = p + 1;"
                , "  void *r = q + 1;"
                , "  free(p);"
                , "  use(r); // r is invalid because p was moved"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: cannot borrow `p` as mutable because it is already borrowed [-Wlinear-types]"
                , "test.c:8: use-after-move: borrow `r` is invalid because `p` was moved [-Wlinear-types]"
                ]

    describe "Function call borrows" $ do
        it "tracks borrows through function calls" $ do
            ast <- mustParse
                [ "void * identity(void *p);"
                , "void free(void * owner ptr);"
                , "void use(void *ptr);"
                , "void test() {"
                , "  void * owner p = malloc(10);"
                , "  void *q = identity(p);"
                , "  free(p);"
                , "  use(q); // q is invalid because p was moved"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:8: use-after-move: borrow `q` is invalid because `p` was moved [-Wlinear-types]"
                ]

        it "allows passing non-owner pointer to realloc" $ do
            ast <- mustParse
                [ "void * realloc(void *ptr, size_t size);"
                , "void * owner os_realloc(void *ptr, size_t size) {"
                , "  return realloc(ptr, size);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: passing non-owned variable `ptr` to function expecting owner [-Wlinear-types]"
                ]

    describe "Proposed improvements" $ do
        it "allows unconditional move of _Nullable owner (Option A)" $ do
            ast <- mustParse
                [ "void consume(void * owner _Nullable ptr);"
                , "void test(void * owner _Nullable p) {"
                , "  consume(p);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "considers members moved when the parent struct is moved (Option C)" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void consume(struct Foo * owner f);"
                , "void test(struct Foo * owner f) {"
                , "  consume(f);"
                , "} // f->data should be considered moved"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows moving a member even if it was already moved" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void consume(void * owner p);"
                , "void test(struct Foo * owner f) {"
                , "  consume(f->data);"
                , "  free(f);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "tracks ownership across union members" $ do
            ast <- mustParse
                [ "typedef union My_Union { void * owner p; int i; void * owner q; } My_Union;"
                , "void consume(void * owner p);"
                , "void test(void * owner p) {"
                , "  My_Union u;"
                , "  u.p = p; // u.p is now Owned"
                , "  consume(u.p); // u.p is now Moved"
                , "  u.q = nullptr; // Should be allowed because it's already Moved (or at least Null)"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects leak when overwriting a union member" $ do
            ast <- mustParse
                [ "union My_Union { void * owner p; void * owner q; };"
                , "struct EventData { union My_Union u; };"
                , "void * owner alloc(void);"
                , "void test(void * owner dummy) {"
                , "  struct EventData event;"
                , "  event.u.p = alloc();"
                , "  event.u.q = nullptr; // Leaks event.u.p"
                , "  free(dummy);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: leak: union member `event.u.p` is overwritten by assignment to `q` [-Wlinear-types]"
                ]

        it "detects leak when overwriting a union member with another owner" $ do
            ast <- mustParse
                [ "union My_Union { void * owner p; void * owner q; };"
                , "struct EventData { union My_Union u; };"
                , "void * owner alloc(void);"
                , "void test(void * owner dummy) {"
                , "  struct EventData event;"
                , "  event.u.p = alloc();"
                , "  event.u.q = alloc(); // Leaks event.u.p"
                , "  free(event.u.q);"
                , "  free(dummy);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: leak: union member `event.u.p` is overwritten by assignment to `q` [-Wlinear-types]"
                ]

        it "correctly handles nested struct/union parent moves" $ do
            ast <- mustParse
                [ "struct Inner { void * owner p; };"
                , "struct Outer { struct Inner inner; };"
                , "void consume(struct Outer * owner o);"
                , "void test(struct Outer * owner o) {"
                , "  consume(o);"
                , "} // o->inner.p should be considered moved"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "allows moving a member from an owned pointer to struct" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void consume(void * owner p);"
                , "void test(struct Foo * owner f) {"
                , "  consume(f->data);"
                , "  free(f);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe` []

        it "detects double move from struct member" $ do
            ast <- mustParse
                [ "struct Foo { void * owner data; };"
                , "void consume(void * owner p);"
                , "void test(struct Foo * owner f) {"
                , "  consume(f->data);"
                , "  consume(f->data);"
                , "  free(f);"
                , "}"
                ]
            analyseGlobal ["linear-types"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:5: use-after-move of variable `f->data` [-Wlinear-types]"
                ]
