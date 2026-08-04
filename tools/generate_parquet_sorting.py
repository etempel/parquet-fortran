#!/usr/bin/env python3
"""Generate the per-type blocks of the `parquet_sorting` module (feature_sort.md M1).

`parquet_sorting` is this library's public sorting API: `pf_sort`, `pf_argsort`, `pf_permute`
and `pf_is_sorted` over eleven element types, plus the `pf_sort_keys` multi-key builder. That
is the same handful of statements repeated per type, which is what this script emits -- by
hand it is ~75 near-identical procedures for M1 alone, each needing its own `!>`/`!!`
doc-comments.

Emitted (all COMMITTED to the repository, exactly like the other generators' output -- nothing
is generated at build time, so the fpm build stays dependency-free):

  src/parquet_sorting.f90           module spec: pf_sort_keys, the public generics, and the
                                    interface bodies for every submodule-implemented procedure.
  src/parquet_sorting_keys.f90      key extraction (one per type) + pf_sort_keys%add.
  src/parquet_sorting_argsort.f90   pf_argsort and pf_sort.
  src/parquet_sorting_permute.f90   pf_permute and pf_is_sorted.

THE KIND TABLE: the nine scalar rows are imported from tools/generate_parquet_columns.py --
the single place a supported column kind is declared -- and the three extras this module adds
(character(len=*), parquet_string_column, parquet_column) are declared locally, because they
are not `parquet_column` storage kinds at all. `--self-test` cross-checks the imported set, so
a change over there fails here rather than silently changing what this module supports.

Usage:  tools/generate_parquet_sorting.py [--check] [--self-test]

  --check      regenerate into memory and compare with the committed files; exit 1 on any
               difference (so CI, or a reviewer, can prove the committed output is current).
  --self-test  verify the imported kind table still has the shape this script assumes.
"""

import argparse
import importlib.util
import pathlib
import sys

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent


def _load_scalar_kinds():
    """Import the 9 SCALAR rows of the columns generator's kind table -- one source of truth."""
    path = REPO_ROOT / "tools" / "generate_parquet_columns.py"
    spec = importlib.util.spec_from_file_location("_pq_columns_gen", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return [k for k in mod.KINDS if k[4] == 1]


#: Tags of the imported scalar kinds, in order. --self-test asserts this still holds; the
#: emitted module's type coverage is defined by it.
EXPECTED_SCALAR_TAGS = ["i32", "i64", "f32", "f64", "bool", "str", "date", "time", "ts"]

# --------------------------------------------------------------------------------------
# The type table. One row per element type this module sorts.
#
#   tag       short name used in every specific's name (argsort_<tag>_i32, ...)
#   decl      the Fortran declaration of ONE element
#   what      human-readable name, used in doc-comments
#   family    how the values reach the engine:
#               'int'    integer-valued, widened into an int64 buffer
#               'int64'  integer-valued and already int64 -- BORROWED, never copied
#               'real'   real-valued, widened into a real64 buffer
#               'real64' already real64 -- BORROWED, never copied
#               'chr'    character(len=*) array, packed into (offsets, data)
#               'strcol' parquet_string_column, packed into (offsets, data)
#               'ts'     parquet_timestamp -- TWO integer keys (seconds, nanoseconds)
#               'col'    parquet_column -- kind resolved at runtime, may be two keys
#   nulls     where nullness comes from:
#               'arg'  an optional is_valid(:) logical argument
#               'elem' the elements themselves (temporal kinds)
#               'own'  the container's own validity (strcol, col)
#   sort      .true. when pf_sort (out-of-place) applies -- false for the container types
#   permute   how pf_permute reorders it: 'gather' (array) or 'reindex' (container)
# --------------------------------------------------------------------------------------
TYPES = [
    ("i32",    "integer(int32)",             "32-bit integer",   "int",    "arg",  True,  "gather"),
    ("i64",    "integer(int64)",             "64-bit integer",   "int64",  "arg",  True,  "gather"),
    ("f32",    "real(real32)",               "32-bit real",      "real",   "arg",  True,  "gather"),
    ("f64",    "real(real64)",               "64-bit real",      "real64", "arg",  True,  "gather"),
    ("bool",   "logical",                    "logical",          "int",    "arg",  True,  "gather"),
    ("chr",    "character(len=*)",           "string",           "chr",    "arg",  True,  "gather"),
    ("date",   "type(parquet_date)",         "date",             "int",    "elem", True,  "gather"),
    ("time",   "type(parquet_time)",         "time",             "int",    "elem", True,  "gather"),
    ("ts",     "type(parquet_timestamp)",    "timestamp",        "ts",     "elem", True,  "gather"),
    ("strcol", "type(parquet_string_column)", "packed string column", "strcol", "own", False, "reindex"),
    ("col",    "type(parquet_column)",       "type-erased column", "col",  "own",  False, "reindex"),
]

#: Families whose values are handed to the engine without a copy (feature_sort.md section 8).
BORROWED = ("int64", "real64")

#: Families that need the multi-key builder rather than a one-shot call: a timestamp is two
#: integer keys (its (seconds, nanoseconds) pair cannot be folded into one int64 without
#: overflowing outside roughly 1678-2262), and a parquet_column may be a timestamp.
MULTIKEY = ("ts", "col")

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!"""

IDX_KINDS = [("i32", "integer(int32)", "int32"), ("i64", "integer(int64)", "int64")]


def wrap_list(names, indent, width=110):
    """Join `names` with ', ', wrapping with Fortran '&' continuations."""
    pad = " " * indent
    out, line = [], ""
    for i, name in enumerate(names):
        piece = name + (", " if i < len(names) - 1 else "")
        if line and len(pad) + len(line) + len(piece) > width:
            out.append(line + "&")
            line = piece
        else:
            line += piece
    out.append(line)
    return ("\n" + pad).join(out)


def val_decl(t, intent, name="values"):
    """Declaration of the values array for type `t`."""
    tag, decl, what, family, nulls, _, _ = t
    if family in ("strcol", "col"):
        return f"        {decl}, intent({intent}) :: {name}"
    return f"        {decl}, intent({intent}) :: {name}(:)"


def opt_valid(t):
    """The optional is_valid argument, for the six types that take one."""
    if t[4] != "arg":
        return []
    return ["        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null."]


COMMON_OPTS = [
    "        logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.",
    "        logical, intent(in), optional :: nulls_first !! .true. places nulls before values; default .false.",
]


# --------------------------------------------------------------------------------------
# src/parquet_sorting.f90 -- the module spec
# --------------------------------------------------------------------------------------
def gen_spec():
    o = []
    w = o.append
    w(BANNER)
    w('''!> Sorting for plain Fortran arrays and for this library's own column types.
!!
!! This module is the public face of the same C++ `std::sort` engine that orders a read-time
!! `parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`. Sharing one engine is the
!! point: a read-time sort, a table sort and a raw-array sort can never disagree about where
!! nulls go, where NaNs go, or how ties are broken.
!!
!! **Naming.** Everything public here carries the `pf_` prefix (parquet-fortran) rather than
!! `parquet_`, because the subject is not a parquet file -- see CLAUDE.md's "Naming
!! conventions". The module is `parquet_sorting` rather than `parquet_sort` because a module
!! cannot share its name with a procedure it declares.
!!
!! Four operations, over eleven element types:
!!
!! * `pf_argsort(values, perm)` -- the permutation that would sort `values`. Never modifies it.
!! * `pf_sort(values, sorted)` -- an independent sorted copy. Never modifies its input.
!! * `pf_permute(values, perm)` -- applies a permutation to `values` IN PLACE.
!! * `pf_is_sorted(values, answer)` -- whether `values` is already in the stated order.
!!
!! **Ordering reproduces `arrow::compute::SortIndices` exactly.** Null and NaN placement is
!! absolute: `descending` reverses the values, never the tiers. Ascending gives values, then
!! NaNs, then nulls; `nulls_first=.true.` gives nulls, then NaNs, then values. Ties always keep
!! their original order -- **every sort here is stable, unconditionally**, so there is no
!! `stable=` argument to pass.
!!
!! **Where nullness comes from depends on the type.** The six types with no null state of their
!! own (`integer`, `real`, `logical`, `character`) take an optional `is_valid(:)` mask; the
!! temporal types and the two column types carry their own and take no such argument.
!!
!! **Sorting one column of a table desynchronises it.** `%col` hands back a writable pointer
!! into a table's live storage, so `call pf_permute(p, perm)` on it reorders that column and
!! leaves every other column where it was, silently breaking row correspondence. Use
!! `parquet_table%sort_by`, which reorders every column together.
module parquet_sorting
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char, c_long_long
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_add_key_int64, &
        parquet_sort_builder_add_key_double, parquet_sort_builder_add_key_string, &
        parquet_sort_builder_build, parquet_sort_builder_is_sorted, parquet_sort_builder_free, &
        parquet_sort_argsort_int64, parquet_sort_argsort_double, parquet_sort_argsort_string, &
        parquet_sort_is_sorted_int64, parquet_sort_is_sorted_double, parquet_sort_is_sorted_string
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use parquet_columns, only : parquet_column, parquet_kind_name, PK_INT32, PK_INT64, PK_FLOAT32, &
        PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP
    !
    implicit none
    private
    !
    public :: pf_sort_keys
    public :: pf_sort
    public :: pf_argsort
    public :: pf_permute
    public :: pf_is_sorted
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_sorting: "
    !
    ! ---- Internal key families ----
    integer, parameter :: SK_INT = 1  !! key values live in `ints`.
    integer, parameter :: SK_REAL = 2 !! key values live in `reals`.
    integer, parameter :: SK_STR = 3  !! key values live in `offsets`/`data`.
    !
    !> One extracted sort key, in the canonical form the C++ engine takes.
    !!
    !! Exactly one of `ints`/`reals`/(`offsets`,`data`) is allocated, matching `family`. `valid`
    !! is left UNALLOCATED when the key has no nulls at all, which is the engine's own fast path
    !! -- the same convention `parquet_column%row_validity` already uses.
    type :: sort_key_buf
        private
        integer :: family = SK_INT                          !! SK_INT / SK_REAL / SK_STR.
        logical :: descending = .false.                     !! .true. sorts high to low.
        logical :: nulls_first = .false.                    !! .true. places nulls before values.
        integer(int64), allocatable :: ints(:)              !! SK_INT values.
        real(real64), allocatable :: reals(:)               !! SK_REAL values.
        integer(int64), allocatable :: offsets(:)           !! SK_STR: n+1 byte offsets, 0-based.
        character(kind=c_char), allocatable :: data(:)      !! SK_STR: the packed bytes.
        integer(c_int8_t), allocatable :: valid(:)          !! 1 = valid; UNALLOCATED means no nulls.
    end type sort_key_buf
    !
    !> A list of sort keys, applied in the order added -- the first key added is the primary one.
    !!
    !! This is how a multi-key sort is expressed, because Fortran cannot offer "an optional
    !! second and third array, each of any type" as a generic: with eleven element types that
    !! would need over a thousand specific procedures. Add as many keys as needed, of any mix of
    !! types, then hand the object to `pf_argsort`:
    !!
    !! ```fortran
    !! type(pf_sort_keys) :: k
    !! call k%add(ra)                          ! primary
    !! call k%add(dec, descending=.true.)      ! breaks ties on ra
    !! call k%add(name)                        ! breaks ties on both
    !! call pf_argsort(k, perm)
    !! ```
    !!
    !! **It holds no C handle**, deliberately: its keys are ordinary allocatable Fortran arrays,
    !! and the C++ builder is created, used and freed entirely inside `pf_argsort`. That keeps
    !! this type free of a `FINAL`, free of an assignment guard, and -- because a finalizable
    !! type must never be given to OpenMP's `private()` -- usable per-thread in the obvious way.
    type :: pf_sort_keys
        private
        integer :: nkeys = 0                                !! keys added so far.
        integer(int64) :: nrows = -1                        !! rows every key must have; -1 until the first add.
        type(sort_key_buf), allocatable :: keys(:)          !! the keys, in precedence order.
    contains''')
    for t in TYPES:
        w(f"        procedure, private :: add_{t[0]} !! %add specific for a {t[2]} key.")
    w("        !> Appends one sort key. Keys apply in the order added, the first being primary.")
    w("        generic :: add => " + wrap_list([f"add_{t[0]}" for t in TYPES], 12))
    w("        procedure :: nkeys_added => keys_count !! Number of keys added so far.")
    w("        procedure :: clear => keys_clear       !! Drops every key, leaving the object reusable.")
    w("    end type pf_sort_keys")
    w("    !")

    # ---- generic interfaces ----
    w("    !> The permutation that would sort `values`: `perm(k)` is the index of the element that")
    w("    !> belongs at position k. `values` is never modified.")
    w("    !>")
    w("    !> The permutation's integer kind is chosen by how the caller declares `perm`. The")
    w("    !> `integer(int32)` form aborts when the array is longer than `huge(1_int32)` rather than")
    w("    !> truncating; declare `perm` as `integer(int64)` for arrays that large.")
    w("    !>")
    w("    !> Also takes a `pf_sort_keys` object in place of `values`, for a multi-key sort.")
    w("    interface pf_argsort")
    for t in TYPES:
        for ik, _, _ in IDX_KINDS:
            w(f"        module procedure argsort_{t[0]}_{ik}")
    for ik, _, _ in IDX_KINDS:
        w(f"        module procedure argsort_keys_{ik}")
    w("    end interface pf_argsort")
    w("    !")
    w("    !> An independent sorted copy of `values`, leaving `values` untouched.")
    w("    !>")
    w("    !> Deliberately not defined for `parquet_string_column` or `parquet_column`: copying a")
    w("    !> whole column to sort it serves no purpose, and reordering one in place is")
    w("    !> `pf_argsort` followed by `pf_permute`, which says what it does at the call site.")
    w("    interface pf_sort")
    for t in TYPES:
        if t[5]:
            w(f"        module procedure sort_{t[0]}")
    w("    end interface pf_sort")
    w("    !")
    w("    !> Applies `perm` to `values` IN PLACE: afterwards element k is what was at `perm(k)`.")
    w("    !> `perm` itself is not modified.")
    w("    !>")
    w("    !> `perm` is validated as a true permutation of 1..n before anything is written, since an")
    w("    !> invalid one would silently duplicate some elements and drop others. Pass")
    w("    !> `assume_valid=.true.` to skip that check when the permutation came from `pf_argsort`")
    w("    !> and is known good. The two column types ignore it -- their own `%reindex` validates")
    w("    !> unconditionally.")
    w("    interface pf_permute")
    for t in TYPES:
        for ik, _, _ in IDX_KINDS:
            w(f"        module procedure permute_{t[0]}_{ik}")
    w("    end interface pf_permute")
    w("    !")
    w("    !> Whether `values` is already in the stated order. O(n) with an early exit, and no copy.")
    w("    !>")
    w("    !> Uses the same comparison `pf_sort` does, so the two can never disagree about nulls,")
    w("    !> NaNs or direction on one array. A run of equal values is sorted.")
    w("    interface pf_is_sorted")
    for t in TYPES:
        w(f"        module procedure is_sorted_{t[0]}")
    w("    end interface pf_is_sorted")
    w("    !")

    # ---- interface bodies ----
    w("    ! ---- Key extraction and pf_sort_keys%add (parquet_sorting_keys) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"        !> Extracts a {what} key into the canonical form the engine takes.")
        w(f"        module subroutine extract_{tag}(values, buf, descending, nulls_first, proc{', is_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        w("            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.")
        w("            logical, intent(in) :: descending !! .true. sorts high to low.")
        w("            logical, intent(in) :: nulls_first !! .true. places nulls before values.")
        w("            character(len=*), intent(in) :: proc !! calling procedure, for messages.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine extract_{tag}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"        !> Appends a {what} sort key.")
        w(f"        module subroutine add_{tag}(self, values, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
        w("            class(pf_sort_keys), intent(inout) :: self !! the key list.")
        w(val_decl(t, "in"))
        w("            logical, intent(in), optional :: descending !! .true. sorts this key high to low.")
        w("            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine add_{tag}")
    w("        !> Number of keys added so far.")
    w("        module function keys_count(self) result(n)")
    w("            class(pf_sort_keys), intent(in) :: self !! the key list.")
    w("            integer :: n                            !! keys added.")
    w("        end function keys_count")
    w("        !> Drops every key, leaving the object reusable.")
    w("        module subroutine keys_clear(self)")
    w("            class(pf_sort_keys), intent(inout) :: self !! the key list.")
    w("        end subroutine keys_clear")
    w("        !> Appends `buf` to `self`, checking every key describes the same number of rows.")
    w("        module subroutine keys_append(self, buf, proc)")
    w("            class(pf_sort_keys), intent(inout) :: self          !! the key list.")
    w("            type(sort_key_buf), allocatable, intent(inout) :: buf(:) !! keys to append; moved from.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("        end subroutine keys_append")
    w("        !> Runs the C++ engine over `keys`, returning a 1-based permutation.")
    w("        module subroutine drive_engine(keys, nrows, proc, perm)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
    w("        end subroutine drive_engine")
    w("        !> Whether every row is already in order under `keys`, using the same comparator")
    w("        !! `drive_engine` sorts with, so the two can never disagree.")
    w("        module subroutine engine_is_sorted(keys, nrows, proc, answer)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            logical, intent(out) :: answer                      !! .true. when already in order.")
    w("        end subroutine engine_is_sorted")
    w("        !> Builds the engine's int8 validity array from a logical mask, leaving `valid`")
    w("        !! UNALLOCATED when the mask marks nothing null (the engine's no-nulls fast path).")
    w("        module subroutine valid_from_mask(mask, n, proc, valid)")
    w("            logical, intent(in) :: mask(:)                          !! .false. marks a null.")
    w("            integer(int64), intent(in) :: n                         !! expected length.")
    w("            character(len=*), intent(in) :: proc                    !! calling procedure, for messages.")
    w("            integer(c_int8_t), allocatable, intent(out) :: valid(:) !! 1 per valid element.")
    w("        end subroutine valid_from_mask")
    w("        !> Aborts unless `perm` is a true permutation of 1..n. Uses a bit-packed seen-set, so")
    w("        !! the scratch is n/8 bytes rather than the 4n a default LOGICAL array would cost.")
    w("        module subroutine check_permutation(perm, n, proc)")
    w("            integer(int64), intent(in) :: perm(:) !! the permutation to validate.")
    w("            integer(int64), intent(in) :: n       !! expected length.")
    w("            character(len=*), intent(in) :: proc  !! calling procedure, for messages.")
    w("        end subroutine check_permutation")
    w("        !> Narrows a 1-based int64 permutation to int32, aborting rather than truncating.")
    w("        module subroutine narrow_perm(perm64, proc, perm32)")
    w("            integer(int64), intent(in) :: perm64(:)                !! the permutation.")
    w("            character(len=*), intent(in) :: proc                   !! calling procedure, for messages.")
    w("            integer(int32), allocatable, intent(out) :: perm32(:)  !! the narrowed copy.")
    w("        end subroutine narrow_perm")
    w("    end interface")
    w("    !")
    w("    ! ---- pf_argsort and pf_sort (parquet_sorting_argsort) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_argsort over a {what} array, returning an {iname} permutation.")
            w(f"        module subroutine argsort_{tag}_{ik}(values, perm, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
            w(val_decl(t, "in"))
            w(f"            {idecl}, allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
            w("            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.")
            w("            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.")
            if nulls == "arg":
                w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
            w(f"        end subroutine argsort_{tag}_{ik}")
    for ik, idecl, iname in IDX_KINDS:
        w(f"        !> pf_argsort over a multi-key `pf_sort_keys`, returning an {iname} permutation.")
        w(f"        module subroutine argsort_keys_{ik}(keys, perm)")
        w("            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.")
        w(f"            {idecl}, allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
        w(f"        end subroutine argsort_keys_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        if not has_sort:
            continue
        w(f"        !> pf_sort over a {what} array: an independent sorted copy.")
        w(f"        module subroutine sort_{tag}(values, sorted, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        if family == "chr":
            w("            character(len=len(values)), allocatable, intent(out) :: sorted(:) !! the sorted copy.")
        else:
            w(f"            {decl}, allocatable, intent(out) :: sorted(:) !! the sorted copy.")
        w("            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.")
        w("            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine sort_{tag}")
    w("    end interface")
    w("    !")
    w("    ! ---- pf_permute and pf_is_sorted (parquet_sorting_permute) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, _, how = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_permute over a {what} array, with an {iname} permutation.")
            w(f"        module subroutine permute_{tag}_{ik}(values, perm, assume_valid)")
            w(val_decl(t, "inout"))
            w(f"            {idecl}, intent(in) :: perm(:) !! 1-based permutation; not modified.")
            w("            logical, intent(in), optional :: assume_valid !! .true. skips validating `perm`.")
            w(f"        end subroutine permute_{tag}_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"        !> pf_is_sorted over a {what} array.")
        w(f"        module subroutine is_sorted_{tag}(values, answer, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        w("            logical, intent(out) :: answer !! .true. when already in the stated order.")
        w("            logical, intent(in), optional :: descending !! .true. tests high-to-low order.")
        w("            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine is_sorted_{tag}")
    w("    end interface")
    w("    !")
    w("end module parquet_sorting ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_keys.f90 -- key extraction, pf_sort_keys%add, and the engine drivers
# --------------------------------------------------------------------------------------

#: How each single-buffer type reduces one element to its key value. Emitted inside
#: `do k = 1_int64, n`. Keyed by TAG, not family: `logical`, `parquet_date` and `parquet_time`
#: all bind as integer keys but reach their integer three different ways.
EXTRACT_LOOP = {
    "i32":  "            buf(1)%ints(k) = int(values(k), int64)",
    "i64":  "            buf(1)%ints(k) = values(k)",
    "f32":  "            buf(1)%reals(k) = real(values(k), real64)",
    "f64":  "            buf(1)%reals(k) = values(k)",
    "bool": "            buf(1)%ints(k) = merge(1_int64, 0_int64, values(k))",
    "date": "            buf(1)%ints(k) = int(values(k)%raw(), int64)",
    "time": "            buf(1)%ints(k) = values(k)%raw()",
}


def emit_extract(w, t):
    """The body of one `extract_<tag>` module procedure."""
    tag, decl, what, family, nulls, _, _ = t
    w(f"    module procedure extract_{tag}")
    # -- declarations
    if family in ("int", "int64", "real", "real64"):
        w("        integer(int64) :: k, n")
    elif family == "chr":
        w("        integer(int64) :: k, n, total, pos, j, ln")
    elif family == "ts":
        w("        integer(int64) :: k, n, s")
        w("        integer(int32) :: ns")
        w("        logical, allocatable :: mask(:)")
    elif family == "strcol":
        w("        integer(int64) :: k, n, total, pos, j, ln")
        w("        character(len=:), allocatable :: s")
        w("        logical :: any_null")
    elif family == "col":
        w("        integer(int64) :: n")
        w("        integer :: kind")
    if family == "bool_unused":
        pass
    if nulls == "elem" and family != "ts":
        w("        logical, allocatable :: mask(:)")
    w("        !")
    # -- row count
    if family in ("strcol",):
        w("        n = values%size()")
    elif family == "col":
        w("        n = values%length()")
    else:
        w("        n = size(values, kind=int64)")

    if family in ("int", "int64", "real", "real64"):
        store = "ints" if family in ("int", "int64") else "reals"
        skind = "SK_INT" if store == "ints" else "SK_REAL"
        zero = "0_int64" if store == "ints" else "0.0_real64"
        w("        allocate(buf(1))")
        w(f"        buf(1)%family = {skind}")
        w("        buf(1)%descending = descending")
        w("        buf(1)%nulls_first = nulls_first")
        w(f"        allocate(buf(1)%{store}(max(n, 1_int64)))")
        w(f"        buf(1)%{store} = {zero}")
        w("        do k = 1_int64, n")
        w(EXTRACT_LOOP[tag])
        w("        end do")
        if nulls == "elem":
            w("        allocate(mask(max(n, 1_int64)))")
            w("        mask = .true.")
            w("        do k = 1_int64, n")
            w("            mask(k) = .not. values(k)%is_null()")
            w("        end do")
            w("        call valid_from_mask(mask, n, proc, buf(1)%valid)")
    elif family == "chr":
        w("        allocate(buf(1))")
        w("        buf(1)%family = SK_STR")
        w("        buf(1)%descending = descending")
        w("        buf(1)%nulls_first = nulls_first")
        w("        ! Sorted on the FULL declared length, trailing blanks included, which is exactly")
        w("        ! Fortran's own `<` for equal-length strings -- so pf_is_sorted agrees with a")
        w("        ! hand-written a(k) <= a(k+1) loop rather than quietly trimming behind it.")
        w("        ln = int(len(values), int64)")
        w("        allocate(buf(1)%offsets(n + 1_int64))")
        w("        total = 0_int64")
        w("        do k = 1_int64, n + 1_int64")
        w("            buf(1)%offsets(k) = total")
        w("            total = total + ln")
        w("        end do")
        w("        buf(1)%offsets(n + 1_int64) = n * ln")
        w("        allocate(buf(1)%data(max(n * ln, 1_int64)))")
        w("        pos = 0_int64")
        w("        do k = 1_int64, n")
        w("            do j = 1_int64, ln")
        w("                buf(1)%data(pos + j) = values(k)(j:j)")
        w("            end do")
        w("            pos = pos + ln")
        w("        end do")
    elif family == "ts":
        w("        ! A timestamp is TWO integer keys: folding (seconds, nanoseconds) into one int64")
        w("        ! as s*10**9 + ns overflows outside roughly 1678-2262, well inside the range this")
        w("        ! library handles. Seconds lead, nanoseconds break their ties, and both carry the")
        w("        ! same validity so the pair can never disagree about which rows are null.")
        w("        allocate(buf(2))")
        w("        buf(:)%family = SK_INT")
        w("        buf(:)%descending = descending")
        w("        buf(:)%nulls_first = nulls_first")
        w("        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))")
        w("        buf(1)%ints = 0_int64")
        w("        buf(2)%ints = 0_int64")
        w("        allocate(mask(max(n, 1_int64)))")
        w("        mask = .true.")
        w("        do k = 1_int64, n")
        w("            call values(k)%get_raw(s, ns)")
        w("            buf(1)%ints(k) = s")
        w("            buf(2)%ints(k) = int(ns, int64)")
        w("            mask(k) = .not. values(k)%is_null()")
        w("        end do")
        w("        call valid_from_mask(mask, n, proc, buf(1)%valid)")
        w("        if (allocated(buf(1)%valid)) buf(2)%valid = buf(1)%valid")
    elif family == "strcol":
        w("        allocate(buf(1))")
        w("        buf(1)%family = SK_STR")
        w("        buf(1)%descending = descending")
        w("        buf(1)%nulls_first = nulls_first")
        w("        allocate(buf(1)%offsets(n + 1_int64))")
        w("        buf(1)%offsets(1) = 0_int64")
        w("        total = 0_int64")
        w("        do k = 1_int64, n")
        w("            total = total + values%length(k)")
        w("            buf(1)%offsets(k + 1_int64) = total")
        w("        end do")
        w("        allocate(buf(1)%data(max(total, 1_int64)))")
        w("        pos = 0_int64")
        w("        any_null = .false.")
        w("        do k = 1_int64, n")
        w("            if (values%is_null(k)) any_null = .true.")
        w("            call values%get(k, s, allow_null=.true.)")
        w("            ln = int(len(s), int64)")
        w("            do j = 1_int64, ln")
        w("                buf(1)%data(pos + j) = s(j:j)")
        w("            end do")
        w("            pos = pos + ln")
        w("        end do")
        w("        if (any_null) then")
        w("            allocate(buf(1)%valid(max(n, 1_int64)))")
        w("            buf(1)%valid = 1_c_int8_t")
        w("            do k = 1_int64, n")
        w("                if (values%is_null(k)) buf(1)%valid(k) = 0_c_int8_t")
        w("            end do")
        w("        end if")
    elif family == "col":
        w("        ! One column, whose element type is only known at runtime -- so this reduces to")
        w("        ! whichever of the four extractors above matches, and a PK_TIMESTAMP column")
        w("        ! produces two keys exactly as the bare type does.")
        w("        kind = values%kindof()")
        w("        if (values%colwidth() /= 1) then")
        w("            error stop EP // proc // \": a vector column cannot be a sort key; there is no \" // &")
        w("                \"defined order on a whole vector row\"")
        w("        end if")
        w("        select case (kind)")
        w("        case (PK_INT32, PK_INT64, PK_LOGICAL, PK_DATE, PK_TIME)")
        w("            call extract_col_integer(values, n, descending, nulls_first, buf)")
        w("        case (PK_FLOAT32, PK_FLOAT64)")
        w("            call extract_col_real(values, n, descending, nulls_first, buf)")
        w("        case (PK_STRING)")
        w("            call extract_col_string(values, n, descending, nulls_first, buf)")
        w("        case (PK_TIMESTAMP)")
        w("            call extract_col_timestamp(values, n, descending, nulls_first, buf)")
        w("        case default")
        w("            block")
        w("                character(len=:), allocatable :: kname")
        w("                call parquet_kind_name(kind, kname)")
        w("                error stop EP // proc // \": a \" // kname // \" column cannot be a sort key\"")
        w("            end block")
        w("        end select")
        w("        call col_valid_flags(values, n, buf)")

    if nulls == "arg":
        w("        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)")
    w(f"    end procedure extract_{tag}")
    w("    !")


def gen_keys():
    o = []
    w = o.append
    w(BANNER)
    w('''!> Turns each supported element type into the canonical key form the C++ engine takes, and
!! implements `pf_sort_keys`.
!!
!! **This file decides nothing about order.** It extracts values, says which rows are null, and
!! passes the caller's `descending`/`nulls_first` flags through -- every ordering decision is made
!! in one place (`sort_compare_key`, `src/parquet_wrapper.cpp`), which is what stops a raw-array
!! sort, a table sort and a read-time `sort_by=` from ever disagreeing.
!!
!! The canonical form is deliberately narrow: an integer key, a real key, or a packed
!! (offsets, data) string key, each with an optional per-row validity array. Everything else
!! reduces to one of those three -- a `logical` and every temporal kind order exactly as their
!! stored integers do, and a `parquet_timestamp` becomes two integer keys rather than one.
submodule (parquet_sorting) parquet_sorting_keys
    implicit none
    !
contains
    !''')
    for t in TYPES:
        emit_extract(w, t)

    # ---- parquet_column sub-extractors (plain contained procedures) ----
    w('''    !> Reads every integer-valued scalar kind of a column as int64 -- including logical and the
    !! two date/time kinds, whose stored values order exactly as the values they represent.
    subroutine extract_col_integer(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int64) :: k
        integer(int32) :: v32
        logical :: b
        type(parquet_date) :: d
        type(parquet_time) :: tm
        !
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            select case (col%kindof())
            case (PK_INT32)
                call col%get_at(k, v32)
                buf(1)%ints(k) = int(v32, int64)
            case (PK_INT64)
                call col%get_at(k, buf(1)%ints(k))
            case (PK_LOGICAL)
                call col%get_at(k, b)
                buf(1)%ints(k) = merge(1_int64, 0_int64, b)
            case (PK_DATE)
                call col%get_at(k, d)
                buf(1)%ints(k) = int(d%raw(), int64)
            case default
                call col%get_at(k, tm)
                buf(1)%ints(k) = tm%raw()
            end select
        end do
    end subroutine extract_col_integer
    !
    !> Reads a float32 or float64 column as real64. NaNs pass straight through: the engine tiers
    !! them itself, exactly as it does for a read-time sort.
    subroutine extract_col_real(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int64) :: k
        real(real32) :: r32
        !
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        buf(1)%reals = 0.0_real64
        do k = 1_int64, n
            if (col%kindof() == PK_FLOAT32) then
                call col%get_at(k, r32)
                buf(1)%reals(k) = real(r32, real64)
            else
                call col%get_at(k, buf(1)%reals(k))
            end if
        end do
    end subroutine extract_col_real
    !
    !> Packs a string column into the (offsets, data) pair the engine takes: row k occupies
    !! `data(offsets(k)+1 : offsets(k+1))`, with `offsets` 0-based because the C++ side indexes
    !! with it directly.
    subroutine extract_col_string(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        character(len=:), allocatable :: s
        integer(int64) :: k, total, pos, j
        !
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%offsets(n + 1_int64))
        buf(1)%offsets(1) = 0_int64
        total = 0_int64
        do k = 1_int64, n
            call col%get_at(k, s)
            total = total + int(len(s), int64)
            buf(1)%offsets(k + 1_int64) = total
        end do
        allocate(buf(1)%data(max(total, 1_int64)))
        pos = 0_int64
        do k = 1_int64, n
            call col%get_at(k, s)
            do j = 1_int64, int(len(s), int64)
                buf(1)%data(pos + j) = s(j:j)
            end do
            pos = pos + int(len(s), int64)
        end do
    end subroutine extract_col_string
    !
    !> Splits a timestamp column into its (seconds, nanoseconds) pair of integer keys -- see
    !! `extract_ts` for why a timestamp becomes two keys rather than one.
    subroutine extract_col_timestamp(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives two keys.
        integer(int64) :: k, s
        integer(int32) :: ns
        type(parquet_timestamp) :: ts
        !
        allocate(buf(2))
        buf(:)%family = SK_INT
        buf(:)%descending = descending
        buf(:)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        buf(2)%ints = 0_int64
        do k = 1_int64, n
            call col%get_at(k, ts)
            call ts%get_raw(s, ns)
            buf(1)%ints(k) = s
            buf(2)%ints(k) = int(ns, int64)
        end do
    end subroutine extract_col_timestamp
    !
    !> Copies a column's own per-row validity onto every key extracted from it, leaving it
    !! UNALLOCATED for a null-free column -- which is the engine's no-nulls fast path, and the same
    !! convention `%row_validity` itself uses by leaving its own result unallocated.
    !!
    !! **Deliberately built from per-row `%is_null` rather than `%row_validity`**, which would be
    !! the obvious choice: `%row_validity` is `intent(inout)` (a temporal column rescans and
    !! caches its null count there), so reaching it from a public `pf_argsort(column)` would mean
    !! either making that argument `intent(inout)` -- wrong for a query, and it would stop a caller
    !! passing their own `intent(in)` dummy -- or taking a copy of the column, which is a full deep
    !! copy of every value on the way to sorting it. `%is_null` is `intent(in)` and is always
    !! correct without consulting any cache, so it costs one extra O(n) pass in front of an
    !! O(n log n) sort and nothing else.
    subroutine col_valid_flags(col, n, buf)
        type(parquet_column), intent(in) :: col                 !! the key column.
        integer(int64), intent(in) :: n                         !! row count.
        type(sort_key_buf), intent(inout) :: buf(:)             !! the keys extracted from it.
        integer(int64) :: k
        integer :: ik
        logical :: any_null
        !
        any_null = .false.
        do k = 1_int64, n
            if (col%is_null(k)) then
                any_null = .true.
                exit
            end if
        end do
        if (.not. any_null) return
        allocate(buf(1)%valid(max(n, 1_int64)))
        buf(1)%valid = 1_c_int8_t
        do k = 1_int64, n
            if (col%is_null(k)) buf(1)%valid(k) = 0_c_int8_t
        end do
        do ik = 2, size(buf)
            buf(ik)%valid = buf(1)%valid
        end do
    end subroutine col_valid_flags
    !''')

    # ---- pf_sort_keys %add specifics ----
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"    module procedure add_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        extra = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_sort_keys%add\"{extra})")
        w("        call keys_append(self, buf, \"pf_sort_keys%add\")")
        w(f"    end procedure add_{tag}")
        w("    !")

    w('''    module procedure keys_count
        n = self%nkeys
    end procedure keys_count
    !
    module procedure keys_clear
        self%nkeys = 0
        self%nrows = -1_int64
        if (allocated(self%keys)) deallocate(self%keys)
    end procedure keys_clear
    !
    module procedure keys_append
        type(sort_key_buf), allocatable :: bigger(:)
        integer(int64) :: n
        integer :: ik
        character(len=32) :: got_str, want_str
        !
        n = key_rows(buf(1))
        if (self%nkeys == 0) then
            self%nrows = n
        else if (n /= self%nrows) then
            write (got_str, "(i0)") n
            write (want_str, "(i0)") self%nrows
            error stop EP // proc // ": every key must describe the same number of rows; this " // &
                "one has " // trim(got_str) // " where the first key has " // trim(want_str)
        end if
        ! Grown exactly, not geometrically: a key list is a handful of entries, and each entry
        ! holds only allocatable descriptors (the value buffers themselves are moved, not copied).
        if (.not. allocated(self%keys)) allocate(self%keys(0))
        allocate(bigger(self%nkeys + size(buf)))
        do ik = 1, self%nkeys
            call move_key(self%keys(ik), bigger(ik))
        end do
        do ik = 1, size(buf)
            call move_key(buf(ik), bigger(self%nkeys + ik))
        end do
        self%nkeys = self%nkeys + size(buf)
        call move_alloc(bigger, self%keys)
        deallocate(buf)
    end procedure keys_append
    !
    !> Moves one key's buffers from `src` to `dst` without copying them.
    subroutine move_key(src, dst)
        type(sort_key_buf), intent(inout) :: src !! the key to move from; left empty.
        type(sort_key_buf), intent(inout) :: dst !! the key to move into.
        dst%family = src%family
        dst%descending = src%descending
        dst%nulls_first = src%nulls_first
        if (allocated(src%ints)) call move_alloc(src%ints, dst%ints)
        if (allocated(src%reals)) call move_alloc(src%reals, dst%reals)
        if (allocated(src%offsets)) call move_alloc(src%offsets, dst%offsets)
        if (allocated(src%data)) call move_alloc(src%data, dst%data)
        if (allocated(src%valid)) call move_alloc(src%valid, dst%valid)
    end subroutine move_key
    !
    !> How many rows one extracted key describes.
    pure function key_rows(buf) result(n)
        type(sort_key_buf), intent(in) :: buf !! the key.
        integer(int64) :: n                   !! its row count.
        select case (buf%family)
        case (SK_REAL)
            n = size(buf%reals, kind=int64)
        case (SK_STR)
            n = size(buf%offsets, kind=int64) - 1_int64
        case default
            n = size(buf%ints, kind=int64)
        end select
    end function key_rows
    !
    module procedure valid_from_mask
        integer(int64) :: k, m
        character(len=32) :: got_str, want_str
        !
        m = size(mask, kind=int64)
        if (m /= n) then
            write (got_str, "(i0)") m
            write (want_str, "(i0)") n
            error stop EP // proc // ": is_valid has " // trim(got_str) // " elements but the " // &
                "values have " // trim(want_str)
        end if
        ! Left UNALLOCATED when nothing is null: the caller turns that into a null pointer, which
        ! the engine reads as "no nulls" and takes its own fast path for. Answering `.true.` for
        ! every row instead would be correct and measurably slower.
        if (all(mask(1:n))) return
        allocate(valid(max(n, 1_int64)))
        valid = 1_c_int8_t
        do k = 1_int64, n
            if (.not. mask(k)) valid(k) = 0_c_int8_t
        end do
    end procedure valid_from_mask
    !
    module procedure drive_engine
        type(c_ptr) :: builder
        integer(int64) :: status
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once"
        end if
        ! Sized EXACTLY, never max(nrows, 1): a zero-row sort must hand back a zero-length
        ! permutation, or `size(perm)` lies and a caller's `do k = 1, size(perm)` reads element 1
        ! of an empty array. The engine is not called at all below two rows, so nothing downstream
        ! needs the one-element floor the extraction buffers use.
        allocate(perm(nrows))
        do ik = 1_int64, nrows
            perm(ik) = ik
        end do
        if (nrows < 2_int64) return
        if (size(keys) == 1) then
            ! One key needs no builder at all: the one-shot entry points BORROW the buffer that
            ! was just extracted, so this saves a handle allocation and a second copy of every
            ! value. Multi-key has to go through the builder, which owns its keys.
            call engine_one_shot(keys(1), nrows, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build(builder, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects -- kept
            ! because silently ignoring a nonzero status is how a real failure goes unnoticed.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine
    !
    module procedure engine_is_sorted
        type(c_ptr) :: builder
        integer(int64) :: res
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once"
        end if
        answer = .true.
        if (nrows < 2_int64) return
        if (size(keys) == 1) then
            call engine_one_shot_is_sorted(keys(1), nrows, answer)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        res = parquet_sort_builder_is_sorted(builder)
        call parquet_sort_builder_free(builder)
        if (res < 0_int64) then
            error stop EP // proc // ": the sort engine had no key to test" ! GCOVR_EXCL_LINE
        end if
        answer = res == 1_int64
    end procedure engine_is_sorted
    !
    !> Argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot(key, nrows, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(inout) :: perm(:)      !! receives the 1-based permutation.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_argsort_double(nrows, key%reals, vp, df, nf, perm)
        case (SK_STR)
            call parquet_sort_argsort_string(nrows, key%offsets, key%data, vp, df, nf, perm)
        case default
            call parquet_sort_argsort_int64(nrows, key%ints, vp, df, nf, perm)
        end select
    end subroutine engine_one_shot
    !
    !> Tests one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_is_sorted(key, nrows, answer)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        logical, intent(out) :: answer                !! .true. when already in order.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        integer(int64) :: res
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            res = parquet_sort_is_sorted_double(nrows, key%reals, vp, df, nf)
        case (SK_STR)
            res = parquet_sort_is_sorted_string(nrows, key%offsets, key%data, vp, df, nf)
        case default
            res = parquet_sort_is_sorted_int64(nrows, key%ints, vp, df, nf)
        end select
        answer = res == 1_int64
    end subroutine engine_one_shot_is_sorted
    !
    !> Adds one already-extracted key to a C++ builder.
    subroutine engine_add_key(builder, key, nrows)
        type(c_ptr), intent(in) :: builder            !! the builder handle.
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_builder_add_key_double(builder, key%reals, vp, df, nf)
        case (SK_STR)
            call parquet_sort_builder_add_key_string(builder, key%offsets, key%data, vp, df, nf)
        case default
            call parquet_sort_builder_add_key_int64(builder, key%ints, vp, df, nf)
        end select
    end subroutine engine_add_key
    !
    !> The three scalars every engine call needs: a pointer to the validity array (or a null
    !! pointer when the key has no nulls) and the two order flags as int8.
    subroutine key_flags(key, valid_ptr, desc_flag, nulls_flag)
        type(sort_key_buf), intent(in), target :: key   !! the key.
        type(c_ptr), intent(out) :: valid_ptr           !! its validity array, or C_NULL_PTR.
        integer(c_int8_t), intent(out) :: desc_flag     !! nonzero for descending.
        integer(c_int8_t), intent(out) :: nulls_flag    !! nonzero to place nulls first.
        !
        valid_ptr = c_null_ptr
        if (allocated(key%valid)) valid_ptr = c_loc(key%valid)
        desc_flag = merge(1_c_int8_t, 0_c_int8_t, key%descending)
        nulls_flag = merge(1_c_int8_t, 0_c_int8_t, key%nulls_first)
    end subroutine key_flags
    !
    module procedure check_permutation
        integer(int8), allocatable :: seen(:)
        integer(int64) :: k, v, word
        character(len=32) :: a_str, b_str
        !
        if (size(perm, kind=int64) /= n) then
            write (a_str, "(i0)") size(perm, kind=int64)
            write (b_str, "(i0)") n
            error stop EP // proc // ": perm has " // trim(a_str) // " elements but the values " // &
                "have " // trim(b_str)
        end if
        if (n < 1_int64) return
        ! A BIT-PACKED seen-set, not a LOGICAL array: gfortran's default LOGICAL is 32 bits, so a
        ! plain seen(n) would cost 4n bytes of scratch to validate a permutation whose own payload
        ! is 8n -- a 50% overhead on an operation whose whole point is to be cheap. This is n/8.
        allocate(seen((n + 7_int64) / 8_int64))
        seen = 0_int8
        do k = 1_int64, n
            v = perm(k)
            if (v < 1_int64 .or. v > n) then
                write (a_str, "(i0)") k
                write (b_str, "(i0)") v
                error stop EP // proc // ": perm(" // trim(a_str) // ") is " // trim(b_str) // &
                    ", which is outside the valid index range"
            end if
            word = (v - 1_int64) / 8_int64 + 1_int64
            if (btest(seen(word), int(mod(v - 1_int64, 8_int64)))) then
                write (a_str, "(i0)") v
                error stop EP // proc // ": perm is not a permutation -- the index " // &
                    trim(a_str) // " appears more than once"
            end if
            seen(word) = ibset(seen(word), int(mod(v - 1_int64, 8_int64)))
        end do
    end procedure check_permutation
    !
    module procedure narrow_perm
        integer(int64) :: n
        character(len=32) :: n_str
        !
        n = size(perm64, kind=int64)
        if (n > int(huge(1_int32), int64)) then
            write (n_str, "(i0)") n
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, which " // &
                "does not fit an int32 permutation; declare perm as integer(int64)"
        end if
        allocate(perm32(n))
        perm32 = int(perm64, int32)
    end procedure narrow_perm
    !
end submodule parquet_sorting_keys ! GCOVR_EXCL_LINE''')
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_argsort.f90 -- pf_argsort and pf_sort
# --------------------------------------------------------------------------------------
def rows_expr(t):
    """Expression giving the row count of `values` for type `t`."""
    family = t[3]
    if family == "strcol":
        return "values%size()"
    if family == "col":
        return "values%length()"
    return "size(values, kind=int64)"


def gen_argsort():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_argsort` (the permutation that would sort an array) and `pf_sort` (an independent sorted
!! copy), over every supported element type.
!!
!! Both are thin: they extract the values into the engine's canonical key form
!! (`parquet_sorting_keys`), run the engine, and -- for `pf_sort` -- gather the result. No ordering
!! decision is made here.
!!
!! **The int32 permutation forms exist because a caller with a default-kind `INTEGER` should not be
!! forced to widen one** (CLAUDE.md's "Public numeric arguments"). They compute in int64 and narrow
!! at the end, aborting rather than truncating when the array is longer than `huge(1_int32)`.
submodule (parquet_sorting) parquet_sorting_argsort
    implicit none
    !
contains
    !''')

    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure argsort_{tag}_{ik}")
            w("        type(sort_key_buf), allocatable :: buf(:)")
            w("        logical :: desc, nlo")
            if ik == "i32":
                w("        integer(int64), allocatable :: perm64(:)")
            w("        !")
            w("        desc = .false.")
            w("        if (present(descending)) desc = descending")
            w("        nlo = .false.")
            w("        if (present(nulls_first)) nlo = nulls_first")
            w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_argsort\"{iv})")
            if ik == "i32":
                w(f"        call drive_engine(buf, {rows_expr(t)}, \"pf_argsort\", perm64)")
                w("        call narrow_perm(perm64, \"pf_argsort\", perm)")
            else:
                w(f"        call drive_engine(buf, {rows_expr(t)}, \"pf_argsort\", perm)")
            w(f"    end procedure argsort_{tag}_{ik}")
            w("    !")

    for ik, idecl, iname in IDX_KINDS:
        w(f"    module procedure argsort_keys_{ik}")
        if ik == "i32":
            w("        integer(int64), allocatable :: perm64(:)")
        w("        !")
        w("        if (keys%nkeys < 1) then")
        w("            error stop EP // \"pf_argsort: this pf_sort_keys has no key; \" // &")
        w("                \"call keys%add(...) at least once before sorting\"")
        w("        end if")
        if ik == "i32":
            w("        call drive_engine(keys%keys(1:keys%nkeys), keys%nrows, \"pf_argsort\", perm64)")
            w("        call narrow_perm(perm64, \"pf_argsort\", perm)")
        else:
            w("        call drive_engine(keys%keys(1:keys%nkeys), keys%nrows, \"pf_argsort\", perm)")
        w(f"    end procedure argsort_keys_{ik}")
        w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        if not has_sort:
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure sort_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64), allocatable :: perm(:)")
        w("        integer(int64) :: k, n")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w(f"        n = {rows_expr(t)}")
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_sort\"{iv})")
        w("        call drive_engine(buf, n, \"pf_sort\", perm)")
        if family == "chr":
            w("        allocate(character(len=len(values)) :: sorted(n))")
        else:
            w("        allocate(sorted(n))")
        w("        do k = 1_int64, n")
        w("            sorted(k) = values(perm(k))")
        w("        end do")
        w(f"    end procedure sort_{tag}")
        w("    !")

    w("end submodule parquet_sorting_argsort ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_permute.f90 -- pf_permute and pf_is_sorted
# --------------------------------------------------------------------------------------
def gen_permute():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_permute` (apply a permutation in place) and `pf_is_sorted` (test an existing order).
!!
!! **`pf_permute` gathers into a fresh array and moves it back**, rather than following cycles in
!! place. Cycle-following would save the temporary, but it needs a "visited" bit per element and
!! would have to be written once per type anyway; the gather is one obvious pass, and for the two
!! container types the work is delegated to their own `%reindex` regardless.
!!
!! **`perm` is validated before anything is written.** An invalid permutation does not fail -- it
!! silently duplicates some elements and drops others, which is precisely the wrong-answer-with-no-
!! symptom class this project guards hardest against. The check is O(n) in front of an O(n)
!! operation, so it is a constant factor rather than a change of complexity, which is why it is on
!! by default; `assume_valid=.true.` skips it for a permutation that came straight from
!! `pf_argsort`.
submodule (parquet_sorting) parquet_sorting_permute
    implicit none
    !
contains
    !''')

    for t in TYPES:
        tag, decl, what, family, nulls, _, how = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure permute_{tag}_{ik}")
            w("        integer(int64), allocatable :: p64(:)")
            if how == "gather":
                if family == "chr":
                    w("        character(len=len(values)), allocatable :: tmp(:)")
                else:
                    w(f"        {decl}, allocatable :: tmp(:)")
                w("        integer(int64) :: k, n")
            w("        logical :: skip")
            w("        !")
            w("        skip = .false.")
            w("        if (present(assume_valid)) skip = assume_valid")
            w("        allocate(p64(size(perm, kind=int64)))")
            w("        p64 = int(perm, int64)")
            if how == "gather":
                w(f"        n = {rows_expr(t)}")
                w("        if (.not. skip) call check_permutation(p64, n, \"pf_permute\")")
                if family == "chr":
                    w("        allocate(character(len=len(values)) :: tmp(n))")
                else:
                    w("        allocate(tmp(n))")
                w("        do k = 1_int64, n")
                w("            tmp(k) = values(p64(k))")
                w("        end do")
                w("        do k = 1_int64, n")
                w("            values(k) = tmp(k)")
                w("        end do")
            else:
                w("        ! `assume_valid` is deliberately ignored here: %reindex validates")
                w("        ! unconditionally, and adding an unvalidated back door into a column's")
                w("        ! storage is not worth saving one O(n) pass.")
                w("        call values%reindex(p64)")
            w(f"    end procedure permute_{tag}_{ik}")
            w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure is_sorted_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_is_sorted\"{iv})")
        w(f"        call engine_is_sorted(buf, {rows_expr(t)}, \"pf_is_sorted\", answer)")
        w(f"    end procedure is_sorted_{tag}")
        w("    !")

    w("end submodule parquet_sorting_permute ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="compare the committed files with a fresh generation; exit 1 on drift")
    ap.add_argument("--self-test", action="store_true",
                    help="verify the imported kind table still has the shape this script assumes")
    args = ap.parse_args()

    if args.self_test:
        scalar = _load_scalar_kinds()
        tags = [k[0] for k in scalar]
        if tags != EXPECTED_SCALAR_TAGS:
            print("generate_parquet_sorting.py: the columns generator's scalar kind table has "
                  "changed.\n  expected: %s\n  found:    %s\n"
                  "Update TYPES (and EXPECTED_SCALAR_TAGS) here to match, then re-run --check."
                  % (EXPECTED_SCALAR_TAGS, tags), file=sys.stderr)
            return 1
        our_tags = [t[0] for t in TYPES]
        missing = [t for t in tags if t not in our_tags and t != "str"]
        if missing:
            print("generate_parquet_sorting.py: scalar kind(s) %s are not in TYPES." % missing,
                  file=sys.stderr)
            return 1
        print("generate_parquet_sorting.py: self-test OK (%d scalar kinds imported, %d types emitted)."
              % (len(scalar), len(TYPES)))
        return 0

    outputs = {
        REPO_ROOT / "src" / "parquet_sorting.f90": gen_spec(),
        REPO_ROOT / "src" / "parquet_sorting_keys.f90": gen_keys(),
        REPO_ROOT / "src" / "parquet_sorting_argsort.f90": gen_argsort(),
        REPO_ROOT / "src" / "parquet_sorting_permute.f90": gen_permute(),
    }

    if args.check:
        drift = []
        for path, text in outputs.items():
            if not path.exists() or path.read_text() != text:
                drift.append(path.relative_to(REPO_ROOT))
        if drift:
            print("generate_parquet_sorting.py --check: %d file(s) differ from a fresh generation:"
                  % len(drift), file=sys.stderr)
            for d in drift:
                print("  %s" % d, file=sys.stderr)
            print("Re-run tools/generate_parquet_sorting.py to regenerate.", file=sys.stderr)
            return 1
        print("generate_parquet_sorting.py --check: %d file(s) are current." % len(outputs))
        return 0

    for path, text in outputs.items():
        path.write_text(text)
        print("wrote %s (%d lines)" % (path.relative_to(REPO_ROOT), text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
