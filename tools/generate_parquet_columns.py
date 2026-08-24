#!/usr/bin/env python3
"""Generate the per-kind blocks of the `parquet_columns` foundation module.

`parquet_column` (see feature_table.md D2/§2) is a type-erased column-value container
discriminated by a `PK_*` kind constant. Its value access, value mutation and storage
bookkeeping are the same handful of statements repeated over 18 kinds, which is what this
script emits -- by hand that is several hundred near-identical procedures, each needing its
own `!>`/`!!` doc-comments.

Emitted (all three are COMMITTED to the repository, exactly like the module
`tools/generate_parquet_maml.sh` emits -- nothing is generated at build time, so the fpm
build stays dependency-free):

  src/parquet_columns.f90           module spec: PK_* constants, the type, its bindings, the
                                    interface bodies for every submodule-implemented
                                    procedure, and the private bitmap/guard helpers that every
                                    submodule reaches by host association.
  src/parquet_columns_access.f90    get_at / set_at / set_all / data_ptr, for the 16 array kinds.
  src/parquet_columns_mutate.f90    append_values for the 16 array kinds, plus the three
                                    kind-dispatched storage helpers (gather / grow / copy / paste)
                                    that the hand-written structural operations are built on.

NOT emitted (hand-written, and never touched by this script):

  src/parquet_columns_validity.f90    kind-dispatched validity (RF8)
  src/parquet_columns_structural.f90  init/clear/append/append_nulls/paste/delete_by_mask/reindex/deep_copy
  src/parquet_columns_string.f90      the string kinds, which delegate to parquet_string_column (DD1)

Usage:  tools/generate_parquet_columns.py [--check]

  --check  regenerate into memory and compare with the committed files; exit 1 on any
           difference (so CI, or a reviewer, can prove the committed output is current).

Regenerates the per-kind blocks of the `parquet_columns`
foundation module: `src/parquet_columns.f90` (the module spec), `src/parquet_columns_access.f90`
and `src/parquet_columns_mutate.f90`. Its output is committed, exactly like
`tools/generate_parquet_maml.sh`'s, so nothing is generated at build time and the fpm build stays
dependency-free. Re-run it after editing the kind table at the top of the script -- for example
when a new column kind is added -- and commit the regenerated files;
`tools/generate_parquet_columns.py --check` re-derives the output and fails if the committed files
have drifted, which is the cheap way to catch a forgotten regeneration. The other four
`parquet_columns_*.f90` files (`util`, `validity`, `structural`, `string`) are hand-written and
the script never touches them. Maintainer-only (stripped from the fpm-published package, see
`tools/prep_fpm_publish.sh`).

It also takes `--bench-guards`, which is for measurement branches only and never for `main`: it
wraps `get_at`/`set_at`'s two guard calls in cpp `#ifdef`s so one source tree can be built three
ways -- as shipped, with the comparisons inlined (`-DPF_BENCH_INLINE_GUARDS`, every check still
performed, only the two cross-submodule calls gone), or with the guards removed
(`-DPF_BENCH_NO_GUARDS`). It exists because that measurement cannot be replicated inside a
benchmark program: the question is what a call across a program-unit boundary costs, and a local
copy in one file would be inlined, measuring the opposite. Without the flag the generator emits
byte-identical output to what is committed, so `--check` and CI are unaffected; restore with `git
checkout src/parquet_columns_access.f90`. `tools/benchmark_colindex.sh` refuses to run its non-
default `GUARDS` values against an unscaffolded tree rather than silently building the shipped
binary under another name.
"""

import argparse
import pathlib
import sys

# --------------------------------------------------------------------------------------
# The kind table. Everything this script emits is driven by these rows.
#
#   tag    short name used in every specific's name (get_at_<tag>, ...)
#   pk     the PK_* discriminator constant
#   decl   the Fortran declaration of one element
#   comp   the storage component inside parquet_column
#   rank   1 = scalar column (values(nrows)), 2 = vector column (values(width, nrows))
#   cat    'num' (numeric/logical: validity lives in the column bitmap)
#          'tmp' (temporal: validity lives INSIDE the element -- see RF8)
#          'str' (string: delegates to an embedded parquet_string_column -- DD1)
# --------------------------------------------------------------------------------------
KINDS = [
    # tag,    pk,                 decl,                      comp,   rank, cat
    ("i32",   "PK_INT32",         "integer(int32)",          "i32",   1, "num"),
    ("i64",   "PK_INT64",         "integer(int64)",          "i64",   1, "num"),
    ("f32",   "PK_FLOAT32",       "real(real32)",            "f32",   1, "num"),
    ("f64",   "PK_FLOAT64",       "real(real64)",            "f64",   1, "num"),
    ("bool",  "PK_LOGICAL",       "logical",                 "bool",  1, "num"),
    ("str",   "PK_STRING",        "character(len=*)",        "str",   1, "str"),
    ("date",  "PK_DATE",          "type(parquet_date)",      "dt",    1, "tmp"),
    ("time",  "PK_TIME",          "type(parquet_time)",      "tm",    1, "tmp"),
    ("ts",    "PK_TIMESTAMP",     "type(parquet_timestamp)", "ts",    1, "tmp"),
    ("i32v",  "PK_INT32_VEC",     "integer(int32)",          "i32v",  2, "num"),
    ("i64v",  "PK_INT64_VEC",     "integer(int64)",          "i64v",  2, "num"),
    ("f32v",  "PK_FLOAT32_VEC",   "real(real32)",            "f32v",  2, "num"),
    ("f64v",  "PK_FLOAT64_VEC",   "real(real64)",            "f64v",  2, "num"),
    ("boolv", "PK_LOGICAL_VEC",   "logical",                 "boolv", 2, "num"),
    ("strv",  "PK_STRING_VEC",    "character(len=*)",        "str",   2, "str"),
    ("datev", "PK_DATE_VEC",      "type(parquet_date)",      "dtv",   2, "tmp"),
    ("timev", "PK_TIME_VEC",      "type(parquet_time)",      "tmv",   2, "tmp"),
    ("tsv",   "PK_TIMESTAMP_VEC", "type(parquet_timestamp)", "tsv",   2, "tmp"),
]

# The array kinds -- everything except the two string kinds, which are hand-written because
# they delegate to parquet_string_column rather than owning a Fortran array (DD1).
ARRAY_KINDS = [k for k in KINDS if k[5] != "str"]

# The vector kinds -- the only ones with more than one value per row, and so the only ones for
# which "one element of one row" is a different operation from "row i". `get_elem`/`set_elem`
# exist for these and nothing else.
VEC_KINDS = [k for k in KINDS if k[4] == 2]

# --------------------------------------------------------------------------------------
# The TYPED per-cell accessor tier (feature_ifx.md).
#
# Every per-cell accessor's body lives behind a `type(parquet_column)` dummy, and the type-bound
# binding is a one-line forwarder onto it. These are the only names `parquet_tables` may use to
# reach storage -- see check_no_type_bound_column_access in tools/check_source_conventions.py.
#
# Only the GENERICS are public (a generic may be public while every specific stays private, as
# `parquet_get_metadata` in parquet_core.f90 already does); `src/parquet.f90` privatises them
# again, so nothing here reaches a `use parquet` program.
# --------------------------------------------------------------------------------------
TYPED_PREFIX = "parquet_column_"

#: generic name -> the specific TAGS it covers. `data_ptr` excludes the string kinds, which own
#: no Fortran array; `get_elem`/`set_elem` exist only for the vector kinds.
TYPED_FAMILIES = [
    ("get_at", [k[0] for k in KINDS]),
    ("set_at", [k[0] for k in KINDS]),
    ("get_elem", [k[0] for k in VEC_KINDS]),
    ("set_elem", [k[0] for k in VEC_KINDS]),
    ("data_ptr", [k[0] for k in ARRAY_KINDS]),
    ("is_null", ["row", "elem"]),
    ("set_null", ["row", "elem"]),
    ("clear_null", ["row", "elem"]),
]

#: Typed procedures that belong to no family and so stay solo public names.
TYPED_SOLO = ["string_column"]

#: Every public name this tier adds, in the order the module declares them.
TYPED_PUBLIC_NAMES = [TYPED_PREFIX + base for base, _ in TYPED_FAMILIES] + \
                     [TYPED_PREFIX + base for base in TYPED_SOLO]

#: The guards, which stay PRIVATE -- only this module and its submodules call them.
TYPED_GUARDS = ["check_kind", "check_index", "check_element", "check_width"]

# Set by --bench-guards. MEASUREMENT BRANCHES ONLY -- see cell_guards() below.
BENCH_GUARDS = False


def cell_guards(pk, proc, indent=8, obj="col", pfx=TYPED_PREFIX):
    """The kind and index guards for one per-element accessor (`get_at`/`set_at`).

    `obj`/`pfx` select which tier the guards are emitted for: the typed implementations pass the
    defaults (`col`, and the typed guards), while a class-dummy body would pass `self` and no
    prefix. A typed body MUST call the typed guards -- the class ones take `class(parquet_column)`,
    so calling one from a typed body reintroduces the whole descriptor block (feature_ifx.md §7.2).

    Normally two plain calls into `parquet_columns_util`, which is exactly what ships. Under
    `--bench-guards` the same two guards are emitted three ways behind cpp `#ifdef`s, so one
    source tree can be built as three binaries and the CALL can be measured apart from the CHECK:

      (default)                  two calls -- as shipped
      -DPF_BENCH_INLINE_GUARDS   the comparisons inline, the call kept only for the failing path
      -DPF_BENCH_NO_GUARDS       neither

    The middle arm is the interesting one: every check still happens, so it is free of any safety
    cost, and the gap to the default is purely what the two cross-submodule calls cost. See
    `feature_colindex.md` Q9 and the run sheet `feature_benchmark_colindex.md`.

    This exists because that measurement CANNOT be replicated inside a benchmark program: the
    question is what a call across a program-unit boundary costs, and a local copy in one file
    would be inlined, measuring the opposite. It must be the real source.
    """
    sp = " " * indent
    plain = (f'{sp}call {pfx}check_kind({obj}, {pk}, "{proc}")\n'
             f'{sp}call {pfx}check_index({obj}, i, "{proc}")')
    if not BENCH_GUARDS:
        return plain
    return (f'#if defined(PF_BENCH_NO_GUARDS)\n'
            f'#elif defined(PF_BENCH_INLINE_GUARDS)\n'
            f'{sp}if ({obj}%kind /= {pk}) call {pfx}check_kind({obj}, {pk}, "{proc}")\n'
            f'{sp}if (i < 1_int64 .or. i > {obj}%nrows) call {pfx}check_index({obj}, i, "{proc}")\n'
            f'#else\n'
            f'{plain}\n'
            f'#endif')

PK_VALUES = [
    ("PK_NONE", 0, "no kind assigned yet (a default-initialized column)"),
    ("PK_INT32", 1, "32-bit integer scalar column"),
    ("PK_INT64", 2, "64-bit integer scalar column"),
    ("PK_FLOAT32", 3, "32-bit real scalar column"),
    ("PK_FLOAT64", 4, "64-bit real scalar column"),
    ("PK_LOGICAL", 5, "logical scalar column"),
    ("PK_STRING", 6, "variable-length string scalar column"),
    ("PK_DATE", 7, "date scalar column"),
    ("PK_TIME", 8, "time scalar column"),
    ("PK_TIMESTAMP", 9, "timestamp scalar column"),
    ("PK_INT32_VEC", 11, "32-bit integer vector column (width values per row)"),
    ("PK_INT64_VEC", 12, "64-bit integer vector column"),
    ("PK_FLOAT32_VEC", 13, "32-bit real vector column"),
    ("PK_FLOAT64_VEC", 14, "64-bit real vector column"),
    ("PK_LOGICAL_VEC", 15, "logical vector column"),
    ("PK_STRING_VEC", 16, "string vector column (one flat string store, stride width)"),
    ("PK_DATE_VEC", 17, "date vector column"),
    ("PK_TIME_VEC", 18, "time vector column"),
    ("PK_TIMESTAMP_VEC", 19, "timestamp vector column"),
    ("PK_LIST", 21, "reserved for a variable-length list column (feature_map_list_struct.md)"),
    ("PK_MAP", 22, "reserved for a map column (feature_map_list_struct.md)"),
    ("PK_STRUCT", 23, "reserved for a struct column (feature_map_list_struct.md)"),
]

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_columns.py
! The kind table lives in that script; edit it there, not here.
!"""


def storage_decl(k):
    """The storage component declaration for one array kind."""
    tag, pk, decl, comp, rank, cat = k
    dims = "(:)" if rank == 1 else "(:,:)"
    what = "scalar" if rank == 1 else "vector (width, nrows)"
    return f"        {decl}, allocatable :: {comp}{dims}   !! {pk} storage, {what}."


# --------------------------------------------------------------------------------------
# src/parquet_columns.f90 -- the module spec
# --------------------------------------------------------------------------------------
def gen_typed_interfaces():
    """The typed per-cell tier's interfaces, plus the generics that make it callable.

    Grouped by the submodule that implements each block, per CLAUDE.md's interface-block rule.
    The array kinds are generated into `parquet_columns_access`; the string kinds and the
    validity forms are hand-written (`parquet_columns_string`, `parquet_columns_validity`), and
    the guards are hand-written in `parquet_columns_util` and stay private.
    """
    o = []
    w = o.append
    w("""    ! ---- Typed per-cell access: the NON-POLYMORPHIC implementation tier (feature_ifx.md) ----
    !
    ! Every per-cell accessor's body lives here, behind a `type(parquet_column)` dummy. The
    ! type-bound bindings declared above are one-line forwarders onto these, and `parquet_tables`
    ! calls these directly instead of going through a binding.
    !
    ! The direction is the whole point and must never be flipped. A `class` actual passed to a
    ! `type` dummy hands over the declared-type part for nothing; a `type` actual passed to a
    ! `class` dummy makes ifx construct a runtime type descriptor in the CALLER's prologue --
    ! 178 stores, emitted unconditionally ahead of any branch, ~35 ns on every call -- because
    ! `parquet_column` has 20 allocatable components and one finalizable component. Re-homing the
    ! bodies down here removes that conversion from every per-cell path in the table layer;
    ! re-homing them the other way would silently restore it, with no test failure and no warning.
    ! `check_no_type_bound_column_access` (tools/check_source_conventions.py) is what enforces it.
    !
    ! ---- Typed value access per kind (parquet_columns_access, GENERATED) ----
    interface""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dim1 = "" if rank == 1 else "(:)"
        dim2 = "(:)" if rank == 1 else "(:,:)"
        rowdoc = "element" if rank == 1 else "row's vector"
        plural = "s" if rank == 2 else ""
        w(f"""        !> Typed `get_at` for a {pk} column: reads row `i`'s {rowdoc}.
        module subroutine {TYPED_PREFIX}get_at_{tag}(col, i, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            {decl}, intent(out) :: value{dim1}   !! receives the value{plural}.
        end subroutine {TYPED_PREFIX}get_at_{tag}
        !> Typed `set_at` for a {pk} column: writes row `i`'s {rowdoc}.
        module subroutine {TYPED_PREFIX}set_at_{tag}(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            {decl}, intent(in) :: value{dim1}     !! the new value{plural}.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine {TYPED_PREFIX}set_at_{tag}""")
        if rank == 2:
            w(f"""        !> Typed `get_elem` for a {pk} column: reads ONE element of row `i`'s vector.
        module subroutine {TYPED_PREFIX}get_elem_{tag}(col, i, e, value)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            {decl}, intent(out) :: value       !! receives the element's value.
        end subroutine {TYPED_PREFIX}get_elem_{tag}
        !> Typed `set_elem` for a {pk} column: writes ONE element of row `i`'s vector.
        module subroutine {TYPED_PREFIX}set_elem_{tag}(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            {decl}, intent(in) :: value           !! the new value.
        end subroutine {TYPED_PREFIX}set_elem_{tag}""")
        w(f"""        !> Typed `data_ptr` for a {pk} column: zero-copy pointer to the live storage.
        module subroutine {TYPED_PREFIX}data_ptr_{tag}(col, p)
            type(parquet_column), intent(in), target :: col !! the column.
            {decl}, pointer, intent(out) :: p{dim2}    !! alias to the live storage.
        end subroutine {TYPED_PREFIX}data_ptr_{tag}""")
    w("    end interface")
    w("    !")
    w(f"""    ! ---- Typed string-kind access (parquet_columns_string) ----
    interface
        !> Typed `string_column`: pointer to the embedded string store (PK_STRING/PK_STRING_VEC).
        module subroutine {TYPED_PREFIX}string_column(col, p)
            type(parquet_column), intent(in), target :: col         !! the column.
            type(parquet_string_column), pointer, intent(out) :: p  !! alias to the string store.
        end subroutine {TYPED_PREFIX}string_column
        !> Typed `get_at` for PK_STRING: reads element `i` into an allocatable string.
        module subroutine {TYPED_PREFIX}get_at_str(col, i, value)
            type(parquet_column), intent(in) :: col             !! the column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            character(len=:), allocatable, intent(out) :: value !! the element's value.
        end subroutine {TYPED_PREFIX}get_at_str
        !> Typed `get_at` for PK_STRING_VEC: reads row `i`'s whole string vector, blank-padded.
        module subroutine {TYPED_PREFIX}get_at_strv(col, i, value)
            type(parquet_column), intent(in) :: col   !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            character(len=*), intent(out) :: value(:) !! receives width values, blank-padded.
        end subroutine {TYPED_PREFIX}get_at_strv
        !> Typed `get_elem` for PK_STRING_VEC: reads ONE element, sized to the stored value.
        module subroutine {TYPED_PREFIX}get_elem_strv(col, i, e, value)
            type(parquet_column), intent(in) :: col             !! the column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            integer(int64), intent(in) :: e                     !! 1-based element index in the row.
            character(len=:), allocatable, intent(out) :: value !! the element's value.
        end subroutine {TYPED_PREFIX}get_elem_strv
        !> Typed `set_at` for PK_STRING. `value` is a SCALAR, so it is stored verbatim.
        module subroutine {TYPED_PREFIX}set_at_str(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            character(len=*), intent(in) :: value      !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine {TYPED_PREFIX}set_at_str
        !> Typed `set_at` for PK_STRING_VEC. `value` is an ARRAY, so trailing blanks are trimmed.
        module subroutine {TYPED_PREFIX}set_at_strv(col, i, value, modify_nulls)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            character(len=*), intent(in) :: value(:)   !! width values for row i.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine {TYPED_PREFIX}set_at_strv
        !> Typed `set_elem` for PK_STRING_VEC. `value` is a SCALAR, so it is stored verbatim.
        module subroutine {TYPED_PREFIX}set_elem_strv(col, i, e, value)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
            character(len=*), intent(in) :: value      !! the new value.
        end subroutine {TYPED_PREFIX}set_elem_strv
    end interface
    !
    ! ---- Typed validity access (parquet_columns_validity) ----
    interface
        !> Typed `is_null` row form: .true. when ANY element of row `i` is null.
        module function {TYPED_PREFIX}is_null_row(col, i) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            logical :: res                          !! .true. when any element of the row is null.
        end function {TYPED_PREFIX}is_null_row
        !> Typed `is_null` element form: the null state of element `e` of row `i` alone.
        module function {TYPED_PREFIX}is_null_elem(col, i, e) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! 1-based row index.
            integer(int64), intent(in) :: e         !! 1-based element index within the row.
            logical :: res                          !! .true. when that element is null.
        end function {TYPED_PREFIX}is_null_elem
        !> Typed `set_null` row form: marks EVERY element of row `i` null.
        module subroutine {TYPED_PREFIX}set_null_row(col, i)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
        end subroutine {TYPED_PREFIX}set_null_row
        !> Typed `set_null` element form: marks element `e` of row `i` null.
        module subroutine {TYPED_PREFIX}set_null_elem(col, i, e)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
        end subroutine {TYPED_PREFIX}set_null_elem
        !> Typed `clear_null` row form: clears the null flag of EVERY element of row `i`.
        module subroutine {TYPED_PREFIX}clear_null_row(col, i)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
        end subroutine {TYPED_PREFIX}clear_null_row
        !> Typed `clear_null` element form: clears the null flag of element `e` of row `i`.
        module subroutine {TYPED_PREFIX}clear_null_elem(col, i, e)
            type(parquet_column), intent(inout) :: col !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            integer(int64), intent(in) :: e            !! 1-based element index within the row.
        end subroutine {TYPED_PREFIX}clear_null_elem
    end interface
    !
    ! ---- Typed guards (parquet_columns_util) ----
    !
    ! PRIVATE, unlike the accessors above: only this module and its submodules call them. The
    ! `class`-dummy guards declared further up are one-line forwarders onto these, so a body that
    ! still takes a polymorphic passed object keeps working unchanged.
    interface
        !> Typed `check_kind`: aborts unless the column's active kind is `expected`.
        module subroutine {TYPED_PREFIX}check_kind(col, expected, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer, intent(in) :: expected         !! the PK_* kind the caller requires.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine {TYPED_PREFIX}check_kind
        !> Typed `check_index`: aborts unless `i` is a valid 1-based row index.
        module subroutine {TYPED_PREFIX}check_index(col, i, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: i         !! the offending 1-based row index.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine {TYPED_PREFIX}check_index
        !> Typed `check_element`: aborts unless `1 <= e <= width`.
        module subroutine {TYPED_PREFIX}check_element(col, e, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: e         !! the offending 1-based element index.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine {TYPED_PREFIX}check_element
        !> Typed `check_width`: aborts unless `n` matches the column's own vector width.
        module subroutine {TYPED_PREFIX}check_width(col, n, proc)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64), intent(in) :: n         !! the supplied element count per row.
            character(len=*), intent(in) :: proc    !! calling procedure name (for the message).
        end subroutine {TYPED_PREFIX}check_width
    end interface
    !""")
    w("    ! ---- The typed tier's generics: the ONLY names parquet_tables uses (feature_ifx.md) ----")
    for base, tags in TYPED_FAMILIES:
        w(f"    interface {TYPED_PREFIX}{base}")
        for tag in tags:
            w(f"        module procedure {TYPED_PREFIX}{base}_{tag}")
        w(f"    end interface {TYPED_PREFIX}{base}")
    w("    !")
    return "\n".join(o)


def gen_spec():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Type-erased, whole-column value storage shared by `parquet_table` and (later) the
!! list/map/struct container column types.
!!
!! `parquet_column` holds the values of ONE column of ONE table: a `PK_*` kind discriminator
!! plus exactly one allocated storage array (the active kind's), a sparse null bitmap, an
!! optional unit string, and the row count. Only the active kind's array is ever allocated, so
!! a column costs one array plus a handful of scalars regardless of how many kinds exist.
!!
!! Three things are worth knowing before using it:
!!
!! * **Validity is kind-dispatched, not uniform.** Numeric and logical kinds carry a
!!   column-level null bitmap; the string kinds delegate to the embedded
!!   `parquet_string_column`'s own validity; the temporal kinds carry their null state INSIDE
!!   each element (matching `parquet_temporal`'s deliberate design). `is_null`/`set_null`/
!!   `any_null` hide this, and there is no public `has_nulls`.
!! * **Validity is sparse.** A column with no nulls allocates NO bitmap at all -- only a
!!   logical scalar. The bitmap appears on the first `set_null`/`append_nulls` and disappears
!!   again on a whole-column `set_all` (or an explicit `compact_validity`).
!! * **Validity is per ELEMENT, and a vector row is not one bit.** The bitmap is
!!   `width * nrows` bits, and every query and mutation comes in a row form and an `(i, e)`
!!   element form. The two are deliberately asymmetric where they differ: a row QUERY
!!   (`is_null(i)`, `row_validity`) answers "any element of the row is null", while a whole-row
!!   MUTATION (`set_null(i)`, `clear_null(i)`, `append_nulls`) acts on every element of it.
!!   `modify_nulls=.false.` skips individual null ELEMENTS, not whole rows.
!! * **Row indices are `integer(int64)` throughout.** This type is internal to the library and
!!   never sees a caller's default-kind `INTEGER`, so it deliberately does not carry the
!!   int32/int64 specific pairs the public API uses.
!!
!! Depends only on `iso_fortran_env` plus the two element-domain modules (`parquet_strings`,
!! `parquet_temporal`) -- never on `parquet` itself, so the container column types can consume
!! it symmetrically without a circular dependency.
module parquet_columns
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    !
    implicit none
    private
    !
    public :: parquet_column
    public :: parquet_kind_name""")
    for name, _, _ in PK_VALUES:
        w(f"    public :: {name}")
    w("""    !
    ! The typed per-cell accessor tier (feature_ifx.md). INTERNAL API: public only because
    ! `parquet_tables` is a different module and this type's components are private, so there is
    ! no other way for it to reach storage without a type-bound call -- which is the thing being
    ! avoided. `src/parquet.f90` privatises every one of these again, so none reaches a
    ! `use parquet` program, and none is covered by the library's semantic-versioning promise or
    ! listed in README's API overview. Only the generics are public; every specific is private.""")
    for name in TYPED_PUBLIC_NAMES:
        w(f"    public :: {name}")
    w("""    !
    ! INTERNAL API, for the same reason and on the same terms as the tier above: a NON-MUTATING
    ! `any_null`, so that a table read accessor holding its table by `intent(in)` can ask the
    ! question without writing to the column. The type-bound `%any_null()` refreshes the temporal
    ! null cache and so cannot be called from a shared column -- see its own interface below.
    ! `src/parquet.f90` privatises this too; a user calls `%any_null()`.
    public :: parquet_column_any_null
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_columns: "
    !
    !> Bits per validity-bitmap block. The bitmap is a hand-rolled `integer(int64)` array
    !! (1 = null, 0 = valid): measured 1.6-2x faster than stdlib's `bitset_large` on random
    !! set/test and on the reindex rebuild, at identical memory, with no int32 bit-index
    !! ceiling, which is why this library takes no stdlib dependency for it.
    integer(int64), parameter :: BITS_PER_BLOCK = 64_int64
    !
    !> `BITS_PER_BLOCK`, published.
    !!
    !! **Exposed for exactly one reason: so that nobody has to copy it.** A caller filling one
    !! column from several threads -- `parquet_tables`' parallel single-column read is the one that
    !! exists -- must keep two threads off the same bitmap block, because updating a block is a
    !! read-modify-write and a lost update silently moves a null to the wrong row. Computing that
    !! alignment needs this number, and a second copy of it living in another module could drift
    !! from this one with nothing to report it. This is a fact about the storage layout, not a knob:
    !! it is a `parameter`, and changing it changes only how the same bits are grouped.
    integer(int64), parameter, public :: parquet_validity_block_bits = BITS_PER_BLOCK
    !""")
    w("    ! ---- Column kind discriminators ----")
    for name, val, doc in PK_VALUES:
        w(f"    integer, parameter :: {name} = {val} !! {doc}")
    w("""    !
    !> One column's values: a kind discriminator, one active storage array, sparse validity,
    !! an optional unit, and the row/width geometry.
    type :: parquet_column
        private
        integer :: kind = PK_NONE                      !! active PK_* discriminator.
        integer(int64) :: nrows = 0                    !! number of rows stored.
        !> Rows the active storage is ALLOCATED for; always >= nrows, 0 when nothing is allocated.
        !!
        !! Growth is geometric (1.5x, `ensure_capacity`), so appending row by row is amortised O(1)
        !! instead of the O(n^2) an exact-fit realloc per append would cost. The slack is invisible
        !! to every caller because every read of a storage array is bounded by `1:nrows` -- an
        !! UNBOUNDED read would return uninitialised tail elements, which is the one silent failure
        !! this component introduces (see feature_risks.md).
        integer(int64) :: cap = 0
        integer(int32) :: width = 1                    !! values per row; > 1 only for *_VEC kinds.
        logical :: has_nulls = .false.                 !! .true. while the bitmap is materialized.
        logical :: nulls_dirty = .true.                !! temporal kinds: the null cache needs a rescan.
        logical :: nulls_cached = .false.              !! temporal kinds: cached "column has >= 1 null".
        integer(int64), allocatable :: validity(:)     !! null bitmap (1 = null); allocated iff has_nulls.
        character(len=:), allocatable :: unit          !! unit string from the MAML `unit:` key (D10).
        type(parquet_string_column), allocatable :: str !! PK_STRING / PK_STRING_VEC storage (DD1).""")
    for k in ARRAY_KINDS:
        w(storage_decl(k))
    w("""        class(*), allocatable :: container(:)          !! reserved payload for PK_LIST/PK_MAP/PK_STRUCT.
    contains
        ! --- lifecycle ---
        procedure :: init                              !! Set kind/geometry and allocate empty storage.
        procedure :: clear                             !! Release all storage and reset to PK_NONE.
        procedure :: deep_copy                         !! Independent copy of values, validity and unit.
        procedure :: move_from                         !! Take over another column's storage, leaving it empty.
        ! --- queries ---
        procedure :: kindof                            !! The active PK_* discriminator.
        procedure :: length                            !! Number of rows stored.
        procedure :: capacity                          !! Rows the storage is allocated for (>= length()).
        procedure :: colwidth                          !! Values per row (1 for scalar kinds).
        procedure :: validity_bytes                    !! Bytes the null bitmap occupies (0 when sparse).
        procedure :: has_validity_storage               !! Whether nulling would still have to allocate.
        procedure :: ensure_validity                    !! Allocate the validity storage up front.
        procedure :: unit_string                       !! Copy out the unit string ("" when unset).
        procedure :: set_unit                          !! Set (or clear) the unit string.
        procedure :: any_null                          !! Whether the column holds at least one null.
        procedure, private :: is_null_row               !! is_null specific taking a row index alone.
        procedure, private :: is_null_elem              !! is_null specific taking a row and an element.
        !> Whether row `i` (or, with `e`, element `e` of row `i`) is null. On a vector kind the
        !! row form answers "ANY element of the row is null"; see the module doc.
        generic :: is_null => is_null_row, is_null_elem
        procedure :: row_validity                      !! Build the whole per-row validity mask at once.
        procedure :: element_validity                  !! Build the whole per-ELEMENT validity mask at once.
        ! --- validity mutation (sparse: see the module doc) ---
        procedure, private :: set_null_row              !! set_null specific taking a row index alone.
        procedure, private :: set_null_elem             !! set_null specific taking a row and an element.
        !> Marks row `i` null, or with `e` just element `e` of it.
        generic :: set_null => set_null_row, set_null_elem
        procedure, private :: clear_null_row            !! clear_null specific taking a row index alone.
        procedure, private :: clear_null_elem           !! clear_null specific taking a row and an element.
        !> Marks row `i` valid, or with `e` just element `e` of it.
        generic :: clear_null => clear_null_row, clear_null_elem
        procedure, private :: set_validity_elems       !! set_validity specific, per-ELEMENT (width, nrows) mask.
        procedure, private :: set_validity_rows        !! set_validity specific, per-ROW mask.
        !> Writes a whole validity mask in one pass. A rank-2 `(width, nrows)` mask is per ELEMENT;
        !! a rank-1 `(nrows)` mask is per ROW, marking every element of a `.false.` row null -- the
        !! same row/element pairing `%set_null` and `%is_null` already use.
        generic :: set_validity => set_validity_elems, set_validity_rows
        procedure :: compact_validity                  !! Drop the bitmap when no nulls remain.
        ! --- capacity ---
        procedure, private :: reserve_i32              !! int32 specific of reserve.
        procedure, private :: reserve_i64              !! int64 specific of reserve.
        !> Grows the storage capacity to hold at least `n` rows without changing the row count.
        generic :: reserve => reserve_i32, reserve_i64
        procedure :: shrink_to_fit                     !! Release capacity beyond the rows stored.
        ! --- structural mutation ---
        procedure :: append                            !! Append another column of identical kind/width.
        procedure :: append_row_of                     !! Append one row of another column (internal; see below).
        procedure :: append_nulls                      !! Append n all-null rows.
        procedure :: paste                             !! Overwrite an existing row range from another column.
        procedure :: delete_by_mask                    !! Keep only rows whose mask entry is .true.
        procedure :: reindex                           !! Reorder rows by a permutation.
        !> INTERNAL -- reindex without the duplicate/range scan, for a permutation the caller has
        !! already established is one. Public only because Fortran offers no narrower visibility;
        !! see the interface below.
        procedure :: reindex_trusted
        procedure, private :: gather_i32               !! int32 specific of gather.
        procedure, private :: gather_i64               !! int64 specific of gather.
        !> Keeps the listed rows, in the listed order. Unlike `reindex` the list may be any length,
        !! and unlike `delete_by_mask` it may reorder -- see the interface below.
        generic :: gather => gather_i32, gather_i64
        ! --- string-kind storage access (PK_STRING / PK_STRING_VEC) ---
        procedure :: string_column                     !! Pointer to the embedded string store.""")
    # generic groups
    for gname, doc in (("get_at", "Read element i (or row i's vector) out."),
                       ("set_at", "Write element i (or row i's vector)."),
                       ("get_elem", "Read ONE element of row i's vector, without materialising the row."),
                       ("set_elem", "Write ONE element of row i's vector, without materialising the row."),
                       ("set_all", "Replace every value in the column."),
                       ("adopt", "Take ownership of an array outright, without copying it."),
                       ("data_ptr", "Zero-copy typed pointer to the active storage."),
                       ("append_values", "Append values, growing the column.")):
        if gname in ("get_elem", "set_elem"):
            tags = [k[0] for k in VEC_KINDS]
        else:
            tags = [k[0] for k in (KINDS if gname in ("get_at", "set_at", "set_all", "append_values")
                                   else ARRAY_KINDS)]
        w(f"        ! --- {gname} ---")
        for t in tags:
            w(f"        procedure, private :: {gname}_{t}   !! {gname} specific for the {t} kind.")
        # Wrap the generic list over several continuation lines. A trailing `!!` would have to
        # sit on the last continuation line, where it both reads badly and blows the 132-column
        # limit -- so these use the leading `!>` form instead, the exception CLAUDE.md already
        # grants to long multi-line continuation bindings (see parquet_column_info's
        # add_metadata generic).
        w(f"        !> {doc}")
        lines = []
        cur = f"        generic :: {gname} => "
        for i, t in enumerate(tags):
            piece = f"{gname}_{t}" + ("" if i == len(tags) - 1 else ", ")
            if len(cur) + len(piece) > 110:
                lines.append(cur + "&")
                cur = "            "
            cur += piece
        lines.append(cur)
        for ln in lines:
            w(ln)
    w("""    end type parquet_column
    !""")

    # ---- interface blocks -------------------------------------------------------------
    w("    ! ---- Lifecycle, queries and unit (parquet_columns_structural) ----")
    w("    interface")
    w("""        !> Sets the column's kind and geometry and allocates empty storage for that kind.
        !! Any previous contents are released first. `width` is required (and must be > 1) for a
        !! *_VEC kind and must be 1 (or absent) otherwise. `nrows` rows are allocated; their values
        !! are unspecified for numeric kinds and null for temporal/string kinds.
        module subroutine init(self, kind, nrows, width, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            integer, intent(in) :: kind                  !! PK_* discriminator to activate.
            integer(int64), intent(in) :: nrows          !! initial row count (>= 0).
            integer(int32), intent(in), optional :: width !! values per row for a *_VEC kind.
            character(len=*), intent(in), optional :: unit !! unit string to store (D10).
        end subroutine init
        !> Releases every storage array and resets the column to PK_NONE with zero rows.
        module subroutine clear(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine clear
        !> Produces an independent copy: values, validity, unit and geometry. Mutating either
        !! column afterwards leaves the other unchanged.
        module subroutine deep_copy(self, out)
            class(parquet_column), intent(in) :: self       !! the source column.
            type(parquet_column), intent(out) :: out        !! the copy.
        end subroutine deep_copy
        !> Takes over `src`'s storage outright, leaving `src` empty (PK_NONE, zero rows).
        !!
        !! The move counterpart of `deep_copy`: every allocatable component is handed over with
        !! `move_alloc` rather than copied, so relocating a column costs a handful of descriptor
        !! assignments instead of a full copy of its data. Use it wherever a column is being
        !! RELOCATED rather than duplicated -- `%drop_column` shifting the tail of a table's slot
        !! array down over the dropped one is the case that motivated it, where intrinsic
        !! assignment memcpy'd every remaining column's values.
        !!
        !! `self` is cleared first, so whatever it held is released.
        module subroutine move_from(self, src)
            class(parquet_column), intent(inout) :: self    !! the column receiving the storage.
            type(parquet_column), intent(inout) :: src      !! the column giving it up; left empty.
        end subroutine move_from
        !> Copies the unit string out ("" when no unit is set).
        module subroutine unit_string(self, u)
            class(parquet_column), intent(in) :: self          !! the column.
            character(len=:), allocatable, intent(out) :: u    !! the unit string, or "".
        end subroutine unit_string
        !> Sets the unit string; passing "" clears it.
        module subroutine set_unit(self, u)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: u            !! the unit string ("" clears).
        end subroutine set_unit
        !> int32 form of `reserve`; converts and delegates.
        module subroutine reserve_i32(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: n              !! rows to make room for.
        end subroutine reserve_i32
        !> Grows the storage capacity to hold at least `n` rows, without changing the row count or
        !! any value. A no-op when the capacity is already sufficient.
        !!
        !! The point of it is that the `n - length()` appends that follow perform **no** allocation
        !! at all, turning even the amortised geometric growth into a single up-front one. Pair it
        !! with `%shrink_to_fit` (reserve, fill, shrink) when the final row count is known ahead of
        !! time.
        !!
        !! **It does not allocate exactly `n`.** Capacity goes to `max(n, 1.5*capacity())`, the same
        !! rule an append follows, so reserving slightly more than the column already holds does not
        !! shrink the allocation -- `%shrink_to_fit` is what does that. This matches
        !! `parquet_string_column%reserve`, which shares the same growth primitive.
        !!
        !! Invalidates any pointer previously obtained from `%data_ptr` if it actually reallocates.
        module subroutine reserve_i64(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! rows to make room for.
        end subroutine reserve_i64
        !> Releases capacity beyond the rows actually stored, so the column occupies exactly what it
        !! holds. A no-op when there is no slack.
        !!
        !! Slack only ever comes from an append: every rebuild (`reindex`, `gather`,
        !! `delete_by_mask`) allocates exact-fit, and so does reading a column from a file. So on a
        !! column that has not been appended to this does nothing at all.
        !!
        !! Invalidates any pointer previously obtained from `%data_ptr` if it actually reallocates.
        !!
        !! `released` reports whether it did. The caller cannot work that out for itself without
        !! knowing which of the three storage mechanisms the kind uses -- a string column's spare
        !! capacity lives in its embedded store's offsets AND its byte payload, neither of which is
        !! `cap` -- so the answer is produced here, where the kind is known. `parquet_table%compact`
        !! needs it to decide whether to advance `%generation()`.
        module subroutine shrink_to_fit(self, released)
            class(parquet_column), intent(inout) :: self  !! the column.
            logical, intent(out), optional :: released    !! .true. if anything was reallocated.
        end subroutine shrink_to_fit
        !> Appends every row of `other`, which must have identical kind and width.
        module subroutine append(self, other)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
        end subroutine append
        !> Appends row `irow` of `other` as this column's next row, carrying that row's validity.
        !!
        !! **INTERNAL plumbing, public only because Fortran offers no narrower visibility** --
        !! `parquet_tables` is a different module and cannot reach `parquet_column`'s private
        !! components, and `%append(row)` needs exactly this to append one row without building a
        !! one-row column and a one-row table first. Same reasoning as `reindex_trusted`, and it is
        !! likewise absent from README.md's API overview.
        !!
        !! Kind and width are checked on every call even though the table layer has already
        !! validated them: two integer comparisons against a call that copies a whole row, on a
        !! procedure a caller who validated nothing can still reach.
        module subroutine append_row_of(self, other, irow)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
            integer(int64), intent(in) :: irow           !! 1-based row of `other` to append.
        end subroutine append_row_of
        !> Appends `n` all-null rows (allocating the bitmap if this is the first null).
        module subroutine append_nulls(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! number of null rows to append (>= 0).
        end subroutine append_nulls
        !> Overwrites the already-allocated rows `at .. at+count-1` with `count` rows of `src`,
        !! starting at `src` row `from`. Kind and width must match; nothing is reallocated and
        !! `nrows` does not change.
        !!
        !! This is the counterpart of `append` for a column whose final row count is known up
        !! front: `init` it once at full size, then paste each piece into place. Assembling a
        !! column from k pieces with `append` instead costs O(k^2) copying, because every append
        !! reallocates the whole column exact-fit and copies everything already in it (see
        !! `grow_storage`) -- which is why `parquet_table`'s slice regime uses this.
        !!
        !! `from`/`count` default to 1 and `src%nrows`, i.e. all of `src`. Passing them copies a
        !! sub-range directly, so a caller trimming a piece to a row window does not need to build
        !! a mask and call `delete_by_mask` first.
        !!
        !! Validity follows the same kind-dispatched rules as `append`: bitmap-backed kinds copy
        !! `src`'s null bits into the destination positions (materializing this column's bitmap
        !! only if `src` actually has nulls), and the temporal kinds carry their null state inside
        !! the pasted elements. **The string kinds are not supported** and abort: their store is
        !! variable-length, so a row range cannot be overwritten in place -- and they already grow
        !! geometrically under `append`, so there is nothing to gain (see `parquet_string_column`).
        module subroutine paste(self, src, at, from, count)
            class(parquet_column), intent(inout) :: self       !! the destination column.
            type(parquet_column), intent(in) :: src            !! the source column (unchanged).
            integer(int64), intent(in) :: at                   !! 1-based first destination row.
            integer(int64), intent(in), optional :: from       !! 1-based first source row (default 1).
            integer(int64), intent(in), optional :: count      !! rows to copy (default all of `src`).
        end subroutine paste
        !> Keeps only the rows whose `keep` entry is .true., preserving order.
        module subroutine delete_by_mask(self, keep)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: keep(:)               !! .true. for every row to retain.
        end subroutine delete_by_mask
        !> Reorders rows so row k of the result is the row that was at `perm(k)`. `perm` must be
        !! a true permutation of 1..nrows and is fully validated before anything is modified.
        module subroutine reindex(self, perm)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: perm(:)        !! 1-based permutation of 1..nrows.
        end subroutine reindex
        !> **INTERNAL.** `reindex` without the O(nrows) range/duplicate scan, for a permutation the
        !! caller has already established is one. The O(1) length check still runs, since it guards
        !! a different invariant and costs nothing.
        !!
        !! Public only because Fortran has no narrower visibility: `parquet_column`'s components are
        !! private to this module, so `parquet_tables` cannot reach them, and `%sort_by` needs this
        !! to stop re-validating one permutation once per column -- measured at a third of its total
        !! time on a 24-column table. Deliberately absent from README.md's API overview. A caller who
        !! passes a non-permutation gets silently duplicated and dropped rows; a library can refuse
        !! accidents, not deliberate misuse of something documented as internal.
        module subroutine reindex_trusted(self, perm)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: perm(:)        !! 1-based permutation of 1..nrows, unchecked.
        end subroutine reindex_trusted
        !> int32 form of `gather` -- see the int64 form below, which does the work.
        module subroutine gather_i32(self, idx)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int32), intent(in) :: idx(:)         !! 1-based source row per destination row.
        end subroutine gather_i32
        !> Keeps the rows `idx` lists, in the order it lists them: row `k` of the result is the row
        !! that was at `idx(k)`, and the column ends up `size(idx)` rows long.
        !!
        !! This is the subset-and-reorder primitive the other two do not provide between them:
        !! `reindex` demands a permutation of the whole column, and `delete_by_mask` keeps the
        !! existing order. It is what makes `parquet_table%top_n` cost O(n) per column rather than
        !! O(nrows) -- a keep mask would make every column pay for a full scan to keep a handful of
        !! rows.
        !!
        !! **It is a gather, not a permutation.** Any row in 1..nrows, in any order, and a row may be
        !! named more than once, so the result may be shorter than, as long as, or longer than the
        !! column it replaces. Only the range is checked: refusing repeats would need a seen-set
        !! sized by the row count on every call, which is the very cost this exists to avoid, and a
        !! caller that needs distinctness can check it once for itself.
        !!
        !! Validity travels with the rows it belongs to, per element for a vector column.
        module subroutine gather_i64(self, idx)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: idx(:)         !! 1-based source row per destination row.
        end subroutine gather_i64
    end interface
    !
    ! ---- Validity, kind-dispatched (parquet_columns_validity) ----
    interface
        !> Whether the column holds at least one null. Cheap for bitmap-backed and string kinds;
        !! for temporal kinds (whose null state lives inside each element) the answer is cached,
        !! and a rescan is needed after a mutation -- which is why `self` is `intent(inout)`
        !! despite this being a query.
        !!
        !! **That `intent(inout)` is real: this WRITES to the column, so it must not be called on
        !! one another thread can reach.** The temporal rescan ends by clearing the dirty flag,
        !! which silently discards a `set_null` that another thread raised during the scan -- and
        !! the column then reports "no nulls" for good, with nothing to notice. Being reached
        !! through the table's `cache` POINTER is what makes that possible from a table accessor
        !! declared `intent(in)`, so the compiler cannot object either. Anything on a read path
        !! must call `parquet_column_any_null` below instead; `%has_nulls` learned this the hard
        !! way. See feature_risks.md Risk-136.
        module function any_null(self) result(res)
            class(parquet_column), intent(inout) :: self !! the column (null cache may be refreshed).
            logical :: res                               !! .true. when at least one row is null.
        end function any_null
        !> Whether the column holds at least one null, WITHOUT writing to it -- the read path's
        !! form of `any_null`, and the one every shared reader must use.
        !!
        !! Identical answer, and identical cost on every kind but one. A temporal column's cache
        !! is read when it is clean and simply re-scanned when it is dirty, rather than being
        !! refreshed: the memoisation is an optimisation for `any_null`, never part of the answer,
        !! so giving it up costs a dirty column one O(n) walk per call and buys the property that
        !! any number of threads may ask at once.
        !!
        !! `type(parquet_column)`, not `class`, for the reason the per-cell tier above records --
        !! and a `class` actual may be passed to it freely, which is how the two bulk validity
        !! walks in this module reach it while holding `self` by `intent(in)`.
        module function parquet_column_any_null(col) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            logical :: res                          !! .true. when at least one row is null.
        end function parquet_column_any_null
        !> Whether row `i` is null. On a *_VEC kind that means **any element** of the row is null.
        !!
        !! The row forms of the validity API are deliberately asymmetric, and the asymmetry is the
        !! useful one: a QUERY answers about the row as a whole ("is anything here missing?"),
        !! while a whole-row MUTATION (`set_null(i)`/`clear_null(i)`) acts on every element. Use
        !! the `(i, e)` forms whenever a single element is what is meant.
        module function is_null_row(self, i) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            logical :: res                            !! .true. when any element of the row is null.
        end function is_null_row
        !> Whether element `e` of row `i` is null.
        !!
        !! Defined for every kind, scalar included -- on a scalar column `width` is 1, so `e` can
        !! only be 1 and the answer equals the row form. That is deliberate: generic code (the
        !! generated table accessors, a caller's own loop) can use one shape for both without
        !! branching on the kind.
        module function is_null_elem(self, i, e) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            logical :: res                            !! .true. when that element is null.
        end function is_null_elem
        !> Fills `valid` with one entry per row: `.true.` where the row is not null.
        !!
        !! The bulk counterpart of `is_null`, and worth having as its own entry point rather than
        !! a loop at the call site for two reasons. It is inside the module that owns the bitmap,
        !! so it can walk that bitmap a 64-bit word at a time and skip whole runs of valid rows,
        !! where an outside loop can only ask one row at a time through a call the compiler cannot
        !! inline. And a null-free column returns an UNALLOCATED `valid` -- which, passed on to an
        !! `optional` dummy such as `parquet_write_column`'s `is_valid=`, counts as an absent
        !! argument (F2018 15.5.2.12), so the common no-nulls case costs no allocation and no scan
        !! at all rather than a mask that is uniformly `.true.`. Callers must therefore test
        !! `allocated(valid)` and not assume a mask came back.
        !! **`intent(in)`, deliberately.** It used to be `intent(inout)` so that the temporal kinds'
        !! null cache could be refreshed in passing, and that single word made the whole procedure
        !! unreachable from anything holding a column by `intent(in)` -- which includes
        !! `pf_argsort(column)` and every other read-only consumer, each of which was left calling
        !! `is_null(i)` once per row instead. The cache is an optimisation for `any_null`, never a
        !! correctness requirement, so this reads it when it is clean and scans when it is dirty
        !! rather than writing it. Widening an argument's intent this way is source-compatible:
        !! every call that compiled before still does.
        module subroutine row_validity(self, valid)
            class(parquet_column), intent(in) :: self     !! the column.
            logical, allocatable, intent(out) :: valid(:) !! per-row mask, or unallocated when no nulls.
        end subroutine row_validity
        !> Fills `valid` with one entry per ELEMENT, shaped `(width, nrows)`: the column's true
        !! validity state, without the row summary `row_validity` applies.
        !!
        !! Same contract as `row_validity` in every other respect, and for the same reasons: a
        !! null-free column returns an UNALLOCATED `valid` (so it reaches an `optional` dummy as an
        !! absent argument and costs nothing), and the bitmap is walked a 64-bit word at a time
        !! rather than a call per element. This is the shape `parquet_write_column` takes for a
        !! vector column, so a table write can hand it straight on.
        !!
        !! **Note the memory**: `LOGICAL` is 4 bytes under gfortran, so this is `4*width*nrows`
        !! bytes -- for a wide column, orders of magnitude more than the bitmap it is built from.
        !! Ask for it when the whole mask is genuinely needed; use `is_null(i, e)` for a few
        !! elements.
        !!
        !! `intent(in)` for the same reason `row_validity` is -- see its note.
        module subroutine element_validity(self, valid)
            class(parquet_column), intent(in) :: self       !! the column.
            logical, allocatable, intent(out) :: valid(:,:) !! (element, row) mask, or unallocated when no nulls.
        end subroutine element_validity
        !> Writes a whole per-ELEMENT validity mask in one pass: `.false.` marks that element null.
        !!
        !! The bulk counterpart of `set_null(i, e)`, and the reason the read path can stop widening
        !! without paying for it: replaying a mask element by element would cost `width*nrows`
        !! type-bound calls, where this writes the bitmap directly. `valid` must be shaped
        !! `(width, nrows)` exactly.
        !!
        !! Only ever ADDS nulls -- an element whose entry is `.true.` is left exactly as it is, so
        !! this composes with a mask describing only part of what the caller knows, and never
        !! resurrects a value that was already null.
        module subroutine set_validity_elems(self, valid)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: valid(:,:)            !! (element, row); .false. marks that element null.
        end subroutine set_validity_elems
        !> Writes a whole per-ROW validity mask in one pass: `.false.` marks every element of that
        !! row null, exactly as `set_null(i)` does for one row.
        !!
        !! The rank-1 form of `set_validity`, and the bulk counterpart of `set_null(i)`. It exists
        !! because replaying a row mask with one type-bound call per null row is what every
        !! materializer and every `%set(..., is_valid=)` was doing; this writes the bitmap a word at
        !! a time instead. `valid` must have exactly `nrows` entries.
        !!
        !! Only ever ADDS nulls, on the same terms as the rank-2 form.
        module subroutine set_validity_rows(self, valid)
            class(parquet_column), intent(inout) :: self !! the column.
            logical, intent(in) :: valid(:)              !! one entry per row; .false. marks the row null.
        end subroutine set_validity_rows
        !> Marks every element of row `i` null, allocating the bitmap on first use (R2).
        module subroutine set_null_row(self, i)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine set_null_row
        !> Marks element `e` of row `i` null, allocating the bitmap on first use (R2).
        module subroutine set_null_elem(self, i, e)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
        end subroutine set_null_elem
        !> Marks every element of row `i` valid. The values behind them are unspecified until written.
        module subroutine clear_null_row(self, i)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine clear_null_row
        !> Marks element `e` of row `i` valid. The value behind it is unspecified until written.
        module subroutine clear_null_elem(self, i, e)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
        end subroutine clear_null_elem
        !> Scans for remaining nulls and releases the bitmap when none are found (R2 iv).
        module subroutine compact_validity(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine compact_validity
    end interface
    !
    ! ---- String kinds, delegating to parquet_string_column (parquet_columns_string) ----
    interface
        !> Pointer to the embedded string store (PK_STRING / PK_STRING_VEC only). The column must
        !! outlive the pointer; any structural mutation invalidates it.
        module subroutine string_column(self, p)
            class(parquet_column), intent(in), target :: self       !! the column.
            type(parquet_string_column), pointer, intent(out) :: p  !! alias to the string store.
        end subroutine string_column
        !> Reads string element `i` into an allocatable string (PK_STRING).
        module subroutine get_at_str(self, i, value)
            class(parquet_column), intent(in) :: self             !! the column.
            integer(int64), intent(in) :: i                       !! 1-based row index.
            character(len=:), allocatable, intent(out) :: value    !! the element's value.
        end subroutine get_at_str
        !> Reads row `i`'s whole string vector (PK_STRING_VEC), one array element per position.
        module subroutine get_at_strv(self, i, value)
            class(parquet_column), intent(in) :: self  !! the column.
            integer(int64), intent(in) :: i            !! 1-based row index.
            character(len=*), intent(out) :: value(:)  !! receives width values, blank-padded.
        end subroutine get_at_strv
        !> Reads ONE element of row `i`'s string vector (PK_STRING_VEC) into an allocatable string.
        !!
        !! Unlike `get_at_strv`, nothing here is blank-padded to a caller-declared width and no
        !! width-long array is built: the result is exactly as long as the stored value.
        module subroutine get_elem_strv(self, i, e, value)
            class(parquet_column), intent(in) :: self           !! the column.
            integer(int64), intent(in) :: i                     !! 1-based row index.
            integer(int64), intent(in) :: e                     !! 1-based element index within the row.
            character(len=:), allocatable, intent(out) :: value !! the element's value.
        end subroutine get_elem_strv
        !> Writes ONE element of row `i`'s string vector (PK_STRING_VEC), clearing THAT element's
        !! null. `value` is a SCALAR, so it is stored verbatim -- the array forms' trimming rule is
        !! about one declared length shared by every element, and one element has no such length.
        module subroutine set_elem_strv(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            character(len=*), intent(in) :: value        !! the new value.
        end subroutine set_elem_strv
        !> Writes string element `i` (PK_STRING). `value` is a SCALAR, so it is stored verbatim,
        !! trailing blanks included -- a scalar is exactly as long as the caller wrote it. Every
        !! character ARRAY entry point below trims instead; see `set_all_str`.
        module subroutine set_at_str(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            character(len=*), intent(in) :: value        !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_str
        !> Writes row `i`'s whole string vector (PK_STRING_VEC). Trailing blanks are trimmed --
        !! `value` is an array, so see `set_all_str` for why that differs from `set_at_str`.
        module subroutine set_at_strv(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            character(len=*), intent(in) :: value(:)     !! width values for row i.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_strv
        !> Replaces every value in a PK_STRING column. **Trailing blanks are trimmed.**
        !!
        !! Every element of a `character(len=*)` array shares one declared length, so a shorter
        !! value is blank-padded by Fortran and those blanks carry nothing the caller could have
        !! meant. That is why every character ARRAY entry point here trims while the SCALAR
        !! `set_at_str` stores its value verbatim, and why `parquet_string_column`'s own API --
        !! which takes bytes the caller controls exactly -- trims only when asked with `trim=`.
        module subroutine set_all_str(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:)    !! exactly nrows values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_str
        !> Replaces every value in a PK_STRING_VEC column, shaped (width, nrows). Trailing blanks
        !! are trimmed, as in `set_all_str`.
        module subroutine set_all_strv(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:,:)  !! (width, nrows) values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_strv
        !> Appends string rows to a PK_STRING column. Trailing blanks are trimmed, as in
        !! `set_all_str`, so a column filled by `%append` holds the same bytes as one filled by
        !! `%set_all`.
        module subroutine append_values_str(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:)    !! rows to append.
        end subroutine append_values_str
        !> Appends string vector rows to a PK_STRING_VEC column, shaped (width, n). Trailing
        !! blanks are trimmed, as in `set_all_str`.
        module subroutine append_values_strv(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:,:)  !! (width, n) rows to append.
        end subroutine append_values_strv
    end interface
    !""")

    # generated per-kind interfaces
    w("    ! ---- Value access per kind (parquet_columns_access, GENERATED) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dim1 = "" if rank == 1 else "(:)"
        dim2 = "(:)" if rank == 1 else "(:,:)"
        rowdoc = "element" if rank == 1 else "row's vector"
        w(f"""        !> Reads row `i`'s {rowdoc} from a {pk} column.
        module subroutine get_at_{tag}(self, i, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            {decl}, intent(out) :: value{dim1}   !! receives the value{'s' if rank == 2 else ''}.
        end subroutine get_at_{tag}
        !> Writes row `i`'s {rowdoc} in a {pk} column.
        module subroutine set_at_{tag}(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            {decl}, intent(in) :: value{dim1}       !! the new value{'s' if rank == 2 else ''}.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_{tag}""")
        if rank == 2:
            w(f"""        !> Reads ONE element of row `i`'s vector from a {pk} column.
        !!
        !! The point is what it does NOT do: `get_at` fills a width-long array, so reading one
        !! element through it costs the caller an allocation per access.
        module subroutine get_elem_{tag}(self, i, e, value)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            integer(int64), intent(in) :: e           !! 1-based element index within the row.
            {decl}, intent(out) :: value         !! receives the element's value.
        end subroutine get_elem_{tag}
        !> Writes ONE element of row `i`'s vector in a {pk} column, clearing THAT element's null.
        !!
        !! Writing through `%data_ptr` instead would leave the column's own null bookkeeping
        !! behind -- for a temporal kind that is a cached answer this type recomputes lazily, so
        !! a bypassed write shows up later as a wrong `%any_null()` and nowhere near its cause.
        !! That bookkeeping is private to this type, which is why the operation belongs here.
        module subroutine set_elem_{tag}(self, i, e, value)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            {decl}, intent(in) :: value             !! the new value.
        end subroutine set_elem_{tag}""")
        w(f"""        !> Replaces every value in a {pk} column.
        module subroutine set_all_{tag}(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            {decl}, intent(in) :: values{dim2}      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_{tag}
        !> Makes `values` this column's storage as a {pk} column, WITHOUT copying it.
        !!
        !! `init` + `set_all` is the copying equivalent: it allocates the column's own array and
        !! then assigns into it, so filling a column from an array you already hold costs a second
        !! full pass and, transiently, twice the memory. This hands the allocation over instead
        !! (`move_alloc`), leaving `values` deallocated -- which is why it is `intent(inout)` and not
        !! `intent(in)`. Prefer it wherever the source array is a temporary the caller is about to
        !! discard; that is exactly the shape of every `parquet_table` materialize.
        !!
        !! The column's kind, width and row count are taken from `values` itself, and any previous
        !! contents (including the validity bitmap) are cleared, so this replaces `init` rather than
        !! following it.
        module subroutine adopt_{tag}(self, values, unit)
            class(parquet_column), intent(inout) :: self !! the column.
            {decl}, allocatable, intent(inout) :: values{dim2} !! array to take over; deallocated on return.
            character(len=*), intent(in), optional :: unit !! unit string to store ("" or absent for none).
        end subroutine adopt_{tag}
        !> Zero-copy typed pointer to a {pk} column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_{tag}(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            {decl}, pointer, intent(out) :: p{dim2}      !! alias to the live storage.
        end subroutine data_ptr_{tag}""")
    w("    end interface")
    w("    !")
    w(gen_typed_interfaces())
    w("    ! ---- Value append per kind + storage helpers (parquet_columns_mutate, GENERATED) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dim2 = "(:)" if rank == 1 else "(:,:)"
        w(f"""        !> Appends rows to a {pk} column, growing its storage.
        module subroutine append_values_{tag}(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            {decl}, intent(in) :: values{dim2}      !! rows to append.
        end subroutine append_values_{tag}""")
    w("""        !> Rebuilds the active storage so row k becomes the row that was at `idx(k)`. Serves both
        !! `reindex` (a permutation) and `delete_by_mask` (a subset, in order). Values only --
        !! validity is the caller's business.
        module subroutine gather_storage(self, idx)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: idx(:)         !! source row index per destination row.
        end subroutine gather_storage
        !> Ensures the active storage is allocated for at least `need_rows` rows, preserving the
        !! rows already stored. A no-op when `cap` is already sufficient.
        !!
        !! **The ONLY place capacity ever grows**, which is what keeps one growth policy in the
        !! library rather than one per caller: `grow_storage` and `reserve` are both two lines on
        !! top of it. Growth is geometric at 1.5x (`max(need_rows, cap + cap/2)`), so appending row
        !! by row is amortised O(1) rather than the O(n^2) an exact-fit realloc per append costs.
        !! `cap + cap/2` rather than `(3*cap)/2` cannot overflow on a very large capacity, and is
        !! the same integer form `parquet_strings`' own `ensure_offsets_cap` uses.
        !!
        !! Does NOT touch `nrows`, and does not resize the validity bitmap -- `grow_storage` calls
        !! `ensure_bitmap` after updating `nrows`, and `ensure_bitmap` sizes itself from `cap`.
        module subroutine ensure_capacity(self, need_rows)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: need_rows      !! rows the storage must hold.
        end subroutine ensure_capacity
        !> Reallocates the active storage down to exactly `nrows` rows, releasing any spare
        !! capacity. A no-op when there is none, and on the string kinds (whose own store carries
        !! its capacity). The counterpart of `ensure_capacity`, and the only place capacity shrinks.
        module subroutine shrink_storage(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine shrink_storage
        !> Grows the active storage by `n` rows, preserving existing values. New rows hold
        !! unspecified values for numeric kinds and null elements for temporal kinds.
        module subroutine grow_storage(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! number of rows to add (>= 0).
        end subroutine grow_storage
        !> Appends every row of `other`'s active storage to `self`'s (values only; the caller
        !! settles validity). Both columns must already have the same kind and width.
        module subroutine append_storage(self, other)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
        end subroutine append_storage
        !> Overwrites rows `at .. at+n-1` of `self`'s active storage with rows `from .. from+n-1`
        !! of `src`'s (values only; the caller settles validity). Both columns must already have
        !! the same kind and width, and every index must already be in range -- `paste` checks all
        !! of that before calling this. Aborts on the string kinds, which cannot be overwritten in
        !! place.
        module subroutine paste_storage(self, src, at, from, n)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: src      !! the source column (unchanged).
            integer(int64), intent(in) :: at             !! 1-based first destination row.
            integer(int64), intent(in) :: from           !! 1-based first source row.
            integer(int64), intent(in) :: n              !! rows to copy (> 0).
        end subroutine paste_storage
        !> Copies the active storage (values only) into `out`, which must already have the same
        !! kind and geometry.
        module subroutine copy_storage(self, out)
            class(parquet_column), intent(in) :: self  !! the source column.
            type(parquet_column), intent(inout) :: out !! the destination column.
        end subroutine copy_storage
    end interface
    !
    ! ---- Shared internal helpers (parquet_columns_util) ----
    ! Declared here rather than contained in this module so that EVERY submodule can call them:
    ! a procedure contained in one submodule is invisible to its siblings, and one contained in
    ! the module itself is reported as unused when this file is compiled on its own.
    interface
        !> Aborts unless the column's active kind is `expected`.
        module subroutine check_kind(self, expected, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer, intent(in) :: expected           !! the PK_* kind the caller requires.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_kind
        !> Aborts unless `i` is a valid 1-based row index.
        module subroutine check_index(self, i, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! the offending 1-based row index.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_index
        !> Aborts unless `e` is a valid 1-based element index WITHIN a row, i.e. `1 <= e <= width`.
        !!
        !! The companion to `check_index` for the element forms of the validity API. Keeping the
        !! two separate is what makes a flat element index passed where a row was meant fail with
        !! a message about the right axis: on a scalar column `width` is 1, so anything but 1 is
        !! rejected immediately rather than silently addressing another row's storage.
        module subroutine check_element(self, e, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: e           !! the offending 1-based element index.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_element
        !> Aborts unless `n` matches the column's own row count.
        module subroutine check_nrows(self, n, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: n           !! the supplied row count.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_nrows
        !> Aborts unless `n` matches the column's own vector width.
        module subroutine check_width(self, n, proc)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: n           !! the supplied element count per row.
            character(len=*), intent(in) :: proc      !! calling procedure name (for the message).
        end subroutine check_width
        !> Number of validity bits the column needs: one per ELEMENT, so `nrows*width` for a
        !! vector kind and `nrows` for a scalar kind (RF8).
        !!
        !! `type`, not `class`, so that a procedure in the typed tier may call it -- a `class`
        !! dummy here would hand a `type` actual to a `class` one and rebuild the descriptor block
        !! that tier exists to remove (the same conversion `ensure_bitmap` needed). Widening it
        !! this way is source-compatible: a `class` actual passes to a `type` dummy freely, so
        !! every existing caller still compiles unchanged.
        pure module function bits_needed(col) result(res)
            type(parquet_column), intent(in) :: col !! the column.
            integer(int64) :: res                   !! required bit count.
        end function bits_needed
        !> Number of int64 blocks needed to hold `nbits` bits.
        pure module function blocks_for(nbits) result(res)
            integer(int64), intent(in) :: nbits !! bit count.
            integer(int64) :: res               !! block count.
        end function blocks_for
        !> Whether bit `b` (1-based) of a bitmap is set. An unallocated bitmap has no set bits.
        pure module function bit_test(map, b) result(res)
            integer(int64), allocatable, intent(in) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b                   !! 1-based bit index.
            logical :: res                                    !! .true. when the bit is set.
        end function bit_test
        !> Sets bit `b` (1-based) of an allocated bitmap.
        !!
        !! **NOT `pure`, and that is load-bearing rather than an oversight.** The body updates the
        !! bitmap word under `!$omp atomic`, and gfortran rejects an OpenMP directive inside a
        !! `pure` procedure outright ("OpenMP directive is not pure and thus may not appear in a
        !! PURE procedure"). Restoring `pure` therefore means removing the atomic, which silently
        !! reintroduces a lost-update race: validity is packed `BITS_PER_BLOCK` elements to one
        !! `integer(int64)`, so two threads writing *different rows* of one column collide whenever
        !! those rows share a block -- see feature_risks.md Risk-135. Nothing here needs purity;
        !! no caller is `pure` or `elemental`, and no `do concurrent` reaches this.
        module subroutine bit_set(map, b)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b         !! 1-based bit index.
        end subroutine bit_set
        !> Clears bit `b` (1-based) of an allocated bitmap. Not `pure`, for the reason `bit_set`
        !! records.
        module subroutine bit_clear(map, b)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b         !! 1-based bit index.
        end subroutine bit_clear
        !> Sets every bit in the 1-based inclusive range `lo .. hi`, a word at a time.
        !!
        !! The bulk counterpart of `bit_set`, for the several places that mark a *contiguous run* of
        !! elements null. Doing it one bit at a time costs a call that cannot be inlined plus a
        !! divide and a `mod` per bit; here the interior words are stored whole and only the two
        !! ragged ends are masked. A range that does not start or end on a word boundary is the
        !! normal case, not an edge case -- `width` need not divide 64 -- so both ends are handled,
        !! and a range lying inside one word is handled by the first end alone.
        !!
        !! **Still `pure`, unlike `bit_set`/`bit_clear`, because nothing reaches it concurrently.**
        !! Its only callers are `grow_rows` and `insert_null_rows`
        !! (`src/parquet_columns_structural.f90`), both row-structural operations that the table
        !! layer refuses outright on a shared table. Give it the same atomic treatment as `bit_set`
        !! if that ever stops being true -- its two ragged-end words are read-modify-writes and
        !! would race exactly as a single bit does.
        pure module subroutine bits_set_range(map, lo, hi)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: lo        !! first 1-based bit to set.
            integer(int64), intent(in) :: hi        !! last 1-based bit to set; < lo sets nothing.
        end subroutine bits_set_range
        !> Clears every bit in the 1-based inclusive range `lo .. hi`, a word at a time. The
        !! counterpart of `bits_set_range`, kept separate for the same reason `bit_clear` is
        !! separate from `bit_set`.
        pure module subroutine bits_clear_range(map, lo, hi)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: lo        !! first 1-based bit to clear.
            integer(int64), intent(in) :: hi        !! last 1-based bit to clear; < lo clears nothing.
        end subroutine bits_clear_range
        !> Copies the bit run `src_lo .. src_lo+nbits-1` of `src` onto `dst_lo ..` of `dst`,
        !! REPLACING the destination bits rather than merging into them.
        !!
        !! The two runs need not be aligned to each other: the destination is walked a word at a
        !! time and each word is assembled from the one or two source words that overlap it, which
        !! is what makes this useful for `append`/`paste`, where the destination offset is whatever
        !! the existing row count happens to be. `merge_only_set` keeps the destination's own set
        !! bits (an OR rather than a replace), which is what `append` wants and `paste` does not --
        !! see their own doc-comments for why those two differ.
        pure module subroutine bits_copy_range(dst, dst_lo, src, src_lo, nbits, merge_only_set)
            integer(int64), intent(inout) :: dst(:) !! destination bitmap.
            integer(int64), intent(in) :: dst_lo    !! first 1-based destination bit.
            integer(int64), intent(in) :: src(:)    !! source bitmap.
            integer(int64), intent(in) :: src_lo    !! first 1-based source bit.
            integer(int64), intent(in) :: nbits     !! how many bits to copy; <= 0 copies nothing.
            logical, intent(in) :: merge_only_set   !! .true. ORs into the destination instead of replacing.
        end subroutine bits_copy_range
        !> Ensures the bitmap exists and covers every element, zero-filling new blocks (0 = valid),
        !! and marks the column bitmap-backed.
        !!
        !! Takes a NON-polymorphic dummy, unlike its neighbours here: the typed `set_null` forms
        !! call it, and a typed body handing a `type(parquet_column)` to a `class` dummy rebuilds
        !! the whole descriptor block the typed tier exists to remove (feature_ifx.md). It is
        !! private plumbing and never a binding, so it needs no polymorphic form at all -- every
        !! existing caller passes a `class` actual, which a `type` dummy accepts for free.
        module subroutine ensure_bitmap(col)
            type(parquet_column), intent(inout) :: col !! the column.
        end subroutine ensure_bitmap
        !> Whether this column's validity storage already exists, i.e. whether nulling an element
        !! would still have to ALLOCATE something.
        !!
        !! The answer depends on which of the three validity mechanisms the kind uses (see
        !! `set_null`): a bitmap kind answers whether its bitmap is allocated, a string kind asks
        !! its `parquet_string_column`, and a temporal kind is **always** `.true.` -- its null state
        !! lives in the element itself, so there is nothing to allocate and never was.
        !!
        !! Exists for concurrency: nulling elements of the same column from two threads races on
        !! that lazy allocation, and this plus `ensure_validity` is how a caller (or the table
        !! layer's own guard) removes the race rather than detecting it.
        module function has_validity_storage(self) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            logical :: res                            !! .true. when nulling would allocate nothing.
        end function has_validity_storage
        !> Materializes this column's validity storage now, leaving every element valid.
        !!
        !! Idempotent, and a no-op for a temporal kind (which has nothing to allocate). Changes no
        !! value and no null state -- only *when* the allocation happens. See
        !! `has_validity_storage` for why that matters.
        module subroutine ensure_validity(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine ensure_validity
        !> Releases the bitmap in O(1): every row becomes valid and a null-free column costs one
        !! scalar again (R2).
        module subroutine drop_bitmap(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine drop_bitmap
        !> Whether a kind stores its null state inside each element (the temporal kinds).
        pure module function is_temporal_kind(kind) result(res)
            integer, intent(in) :: kind !! a PK_* discriminator.
            logical :: res              !! .true. for the date/time/timestamp kinds.
        end function is_temporal_kind
        !> Whether a kind delegates its storage and validity to parquet_string_column.
        pure module function is_string_kind(kind) result(res)
            integer, intent(in) :: kind !! a PK_* discriminator.
            logical :: res              !! .true. for PK_STRING and PK_STRING_VEC.
        end function is_string_kind
        !> Fixed-length kind name, used to build this module's error messages.
        pure module function kind_text(kind) result(res)
            integer, intent(in) :: kind !! a PK_* discriminator.
            character(len=16) :: res    !! the kind's name, blank-padded.
        end function kind_text
    end interface
    !
contains
    !
    ! ==================================================================================
    ! Shared private helpers -- host-associated by every submodule.
    ! ==================================================================================
    !
    !> The column's active kind discriminator. `PK_NONE` until `init` is called.
    pure function kindof(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer :: res                            !! the active PK_* constant.
        res = self%kind
    end function kindof
    !
    !> Number of rows the column currently holds.
    pure function length(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int64) :: res                     !! the row count.
        res = self%nrows
    end function length
    !
    !> Rows the active storage is allocated for. Always >= `length()`; the difference is spare
    !! capacity that `%append` can fill without reallocating, and that `%shrink_to_fit` releases.
    !!
    !! **Reported in ROWS for every kind**, which is what makes `capacity() >= length()` a
    !! meaningful comparison whatever the column holds. The two string kinds forward to the
    !! embedded `parquet_string_column`, whose own capacity counts ELEMENTS -- so it is divided by
    !! the width to get rows, exactly as `length()` counts rows rather than the `nrows*width`
    !! elements a vector string column stores (RF6). A column with no kind yet reports 0.
    !!
    !! **Not `pure`, unlike `length`**, only because the string store's own `%capacity` is not.
    function capacity(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int64) :: res                     !! rows the storage is allocated for.
        if (is_string_kind(self%kind)) then
            res = 0_int64
            if (allocated(self%str)) res = self%str%capacity()/int(self%width, int64)
        else
            res = self%cap
        end if
    end function capacity
    !
    !> Bytes currently occupied by the null bitmap — **0 for a null-free column**, which is the
    !! whole point of the sparse representation (R2): nothing is allocated until the column
    !! actually holds a null. Also the cheapest way for a test or a memory report to prove that.
    !! Always 0 for the string and temporal kinds, whose null state is not a column bitmap.
    pure function validity_bytes(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int64) :: res                     !! bitmap size in bytes.
        if (allocated(self%validity)) then
            res = size(self%validity, kind=int64)*8_int64
        else
            res = 0_int64
        end if
    end function validity_bytes
    !
    !> Values per row: 1 for every scalar kind, the vector width for a *_VEC kind.
    pure function colwidth(self) result(res)
        class(parquet_column), intent(in) :: self !! the column.
        integer(int32) :: res                     !! the column width.
        res = self%width
    end function colwidth
    !
    !> Copies a human-readable name for a PK_* kind out (for error messages and callers that
    !! dispatch on `kindof`).
    subroutine parquet_kind_name(kind, name)
        integer, intent(in) :: kind                        !! a PK_* discriminator.
        character(len=:), allocatable, intent(out) :: name !! the kind's name.
        name = trim(kind_text(kind))
    end subroutine parquet_kind_name
    !
    !
end module parquet_columns ! GCOVR_EXCL_LINE""")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_columns_access.f90
# --------------------------------------------------------------------------------------
def gen_access():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind value access for `parquet_column`: `get_at`, `set_at`, `set_all` and `data_ptr`
!! over every array kind. The string kinds live in `parquet_columns_string` instead, because
!! they delegate to `parquet_string_column` (DD1).
!!
!! Validity handling follows the RF9 rule table: writing a value CLEARS that row's null bit, so
!! a default `set_all` drops the bitmap outright (O(1), no scan); `modify_nulls=.false.` leaves
!! the null entries and the bitmap untouched. Temporal kinds carry their null state inside
!! the element, so they only invalidate the cached null flag.
!!
!! **`modify_nulls=.false.` skips null ELEMENTS, not whole rows.** On a vector kind it writes
!! every element whose own bit is clear and leaves the null ones alone, rather than refusing the
!! whole row because one element of it is null -- matching the rule that each operation acts at
!! the granularity the caller named. The default (`.true.`) path is untouched by this and stays a
!! single whole-array assignment with no per-element work, so the common case costs nothing.
submodule (parquet_columns) parquet_columns_access
    implicit none
contains""")
    P = TYPED_PREFIX
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        temporal = cat == "tmp"
        if rank == 1:
            w(f"""    !
    module procedure {P}get_at_{tag}
{cell_guards(pk, "get_at")}
        value = col%{comp}(i)
    end procedure {P}get_at_{tag}
    !
    module procedure get_at_{tag}
        call {P}get_at_{tag}(self, i, value)
    end procedure get_at_{tag}
    !
    module procedure {P}set_at_{tag}
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
{cell_guards(pk, "set_at")}
        if (.not. mod_nulls) then
            if ({P}is_null(col, i)) return
        end if
        col%{comp}(i) = value""")
            if temporal:
                w("        col%nulls_dirty = .true.")
            else:
                w("        if (col%has_nulls) call bit_clear(col%validity, i)")
            w(f"""    end procedure {P}set_at_{tag}
    !
    module procedure set_at_{tag}
        call {P}set_at_{tag}(self, i, value, modify_nulls)
    end procedure set_at_{tag}
    !
    module procedure set_all_{tag}
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, {pk}, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%{comp} is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%{comp}(1:self%nrows) = values""")
            if temporal:
                w("            self%nulls_dirty = .true.")
            else:
                w("            call drop_bitmap(self)")
            w("""        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%""" + comp + """(k) = values(k)
            end do""")
            if temporal:
                w("            self%nulls_dirty = .true.")
            w(f"""        end if
    end procedure set_all_{tag}
    !
    module procedure adopt_{tag}
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = {pk}
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%{comp})""")
            if temporal:
                w("        self%nulls_dirty = .true.")
            w(f"""    end procedure adopt_{tag}
    !
    module procedure {P}data_ptr_{tag}
        call {P}check_kind(col, {pk}, "data_ptr")
        p => col%{comp}(1:col%nrows)
    end procedure {P}data_ptr_{tag}
    !
    module procedure data_ptr_{tag}
        call {P}data_ptr_{tag}(self, p)
    end procedure data_ptr_{tag}""")
        else:
            w(f"""    !
    module procedure {P}get_at_{tag}
{cell_guards(pk, "get_at")}
        call {P}check_width(col, size(value, kind=int64), "get_at")
        value = col%{comp}(:, i)
    end procedure {P}get_at_{tag}
    !
    module procedure get_at_{tag}
        call {P}get_at_{tag}(self, i, value)
    end procedure get_at_{tag}
    !
    module procedure {P}set_at_{tag}
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call {P}check_kind(col, {pk}, "set_at")
        call {P}check_index(col, i, "set_at")
        call {P}check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if ({P}is_null(col, i, e)) cycle
                col%{comp}(e, i) = value(e)
            end do
            return
        end if
        col%{comp}(:, i) = value""")
            if temporal:
                w("        col%nulls_dirty = .true.")
            else:
                w("""        if (col%has_nulls) then
            base = (i - 1_int64)*int(col%width, int64)
            do e = 1_int64, int(col%width, int64)
                call bit_clear(col%validity, base + e)
            end do
        end if""")
            w(f"""    end procedure {P}set_at_{tag}
    !
    module procedure set_at_{tag}
        call {P}set_at_{tag}(self, i, value, modify_nulls)
    end procedure set_at_{tag}
    !
    module procedure {P}get_elem_{tag}
{cell_guards(pk, "get_elem")}
        call {P}check_element(col, e, "get_elem")
        value = col%{comp}(e, i)
    end procedure {P}get_elem_{tag}
    !
    module procedure get_elem_{tag}
        call {P}get_elem_{tag}(self, i, e, value)
    end procedure get_elem_{tag}
    !
    module procedure {P}set_elem_{tag}
        call {P}check_kind(col, {pk}, "set_elem")
        call {P}check_index(col, i, "set_elem")
        call {P}check_element(col, e, "set_elem")
        col%{comp}(e, i) = value""")
            if temporal:
                # A temporal element IS its own null state, so assigning it is what makes it valid
                # -- or null, if the caller assigned a default-initialised one. Either way the
                # cached answer no longer describes the data, hence the flag rather than a guess.
                w("        col%nulls_dirty = .true.")
            else:
                w("        if (col%has_nulls) call bit_clear(col%validity, "
                  "(i - 1_int64)*int(col%width, int64) + e)")
            w(f"""    end procedure {P}set_elem_{tag}
    !
    module procedure set_elem_{tag}
        call {P}set_elem_{tag}(self, i, e, value)
    end procedure set_elem_{tag}
    !
    module procedure set_all_{tag}
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, {pk}, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%{comp} is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%{comp}(:, 1:self%nrows) = values""")
            if temporal:
                w("            self%nulls_dirty = .true.")
            else:
                w("            call drop_bitmap(self)")
            w("""        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%""" + comp + """(e, k) = values(e, k)
                end do
            end do""")
            if temporal:
                w("            self%nulls_dirty = .true.")
            w(f"""        end if
    end procedure set_all_{tag}
    !
    module procedure adopt_{tag}
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = {pk}
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%{comp})""")
            if temporal:
                w("        self%nulls_dirty = .true.")
            w(f"""    end procedure adopt_{tag}
    !
    module procedure {P}data_ptr_{tag}
        call {P}check_kind(col, {pk}, "data_ptr")
        p => col%{comp}(:, 1:col%nrows)
    end procedure {P}data_ptr_{tag}
    !
    module procedure data_ptr_{tag}
        call {P}data_ptr_{tag}(self, p)
    end procedure data_ptr_{tag}""")
    w("    !")
    w("end submodule parquet_columns_access ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_columns_mutate.f90
# --------------------------------------------------------------------------------------
def gen_mutate():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind value append for `parquet_column`, plus the four kind-dispatched storage
!! helpers (`gather_storage`, `grow_storage`, `copy_storage`, `paste_storage`) that the
!! hand-written structural operations in `parquet_columns_structural` are built on -- so
!! `reindex`, `delete_by_mask`, `append`, `append_nulls`, `paste` and `deep_copy` each exist
!! ONCE, kind-agnostically, instead of eighteen times.
submodule (parquet_columns) parquet_columns_mutate
    implicit none
contains""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""    !
    module procedure append_values_{tag}
        integer(int64) :: n, old
        call check_kind(self, {pk}, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%{comp}(old+1_int64:old+n) = values
    end procedure append_values_{tag}""")
        else:
            w(f"""    !
    module procedure append_values_{tag}
        integer(int64) :: n, old
        call check_kind(self, {pk}, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%{comp}(:, old+1_int64:old+n) = values
    end procedure append_values_{tag}""")

    # gather_storage
    w("""    !
    module procedure gather_storage
        integer(int64) :: n, k""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dims = "(:)" if rank == 1 else "(:,:)"
        w(f"        {decl}, allocatable :: new_{comp}{dims}")
    w("""        n = size(idx, kind=int64)
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            allocate(new_{comp}(max(n, 1_int64)))
            do k = 1_int64, n
                new_{comp}(k) = self%{comp}(idx(k))
            end do
            call move_alloc(new_{comp}, self%{comp})""")
        else:
            w(f"""        case ({pk})
            allocate(new_{comp}(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_{comp}(:, k) = self%{comp}(:, idx(k))
            end do
            call move_alloc(new_{comp}, self%{comp})""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            ! the string store is reordered by its own reindex/delete_by_mask (DD1)
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"gather_storage: column has no active storage"
        end select
        ! A rebuild allocates EXACT-FIT and says so. Only grow_storage ever creates slack, which is
        ! what makes %shrink_to_fit (and parquet_table's %compact) a no-op on any column that has
        ! not been appended to -- so a %filter_rows or a %sort_by hands the memory back on its own
        ! rather than waiting for a call the caller may never make.
        self%cap = n
    end procedure gather_storage
    !
    module procedure ensure_capacity
        integer(int64) :: old, newcap""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dims = "(:)" if rank == 1 else "(:,:)"
        w(f"        {decl}, allocatable :: tmp_{comp}{dims}")
    w("""        if (need_rows <= self%cap) return
        old = self%nrows
        ! Geometric, not exact-fit: see the interface's own doc-comment for why, and why the
        ! `cap + cap/2` form is the one to keep.
        newcap = max(need_rows, self%cap + self%cap/2_int64)
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            allocate(tmp_{comp}(newcap))
            if (old > 0_int64) tmp_{comp}(1:old) = self%{comp}(1:old)
            call move_alloc(tmp_{comp}, self%{comp})""")
        else:
            w(f"""        case ({pk})
            allocate(tmp_{comp}(self%width, newcap))
            if (old > 0_int64) tmp_{comp}(:, 1:old) = self%{comp}(:, 1:old)
            call move_alloc(tmp_{comp}, self%{comp})""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            ! the string store carries its own capacity (parquet_strings' ensure_*_cap), so `cap`
            ! is meaningless here and %capacity/%reserve/%shrink_to_fit forward to it instead
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"ensure_capacity: column has no active storage"
        end select
        self%cap = newcap
    end procedure ensure_capacity
    !
    module procedure shrink_storage
        integer(int64) :: n""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dims = "(:)" if rank == 1 else "(:,:)"
        w(f"        {decl}, allocatable :: tmp_{comp}{dims}")
    w("""        if (self%cap <= self%nrows) return
        n = self%nrows
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            allocate(tmp_{comp}(n))
            if (n > 0_int64) tmp_{comp}(1:n) = self%{comp}(1:n)
            call move_alloc(tmp_{comp}, self%{comp})""")
        else:
            w(f"""        case ({pk})
            allocate(tmp_{comp}(self%width, n))
            if (n > 0_int64) tmp_{comp}(:, 1:n) = self%{comp}(:, 1:n)
            call move_alloc(tmp_{comp}, self%{comp})""")
    w("""        case default
            ! Nothing to shrink: the string kinds carry capacity in their own store, and a column
            ! with no kind has no storage. Neither is an error -- %shrink_to_fit is a request to
            ! release what can be released, not an assertion that there is something to release.
            return
        end select
        self%cap = n
    end procedure shrink_storage
    !
    module procedure grow_storage
        if (n < 0_int64) error stop EP//"grow_storage: negative row count"
        if (n == 0_int64) return
        ! Two lines on top of ensure_capacity, which is the only place capacity grows. The string
        ! kinds reach here too and ensure_capacity is a no-op for them -- their storage grows
        ! through parquet_string_column's own append path (DD1) -- but nrows must still advance.
        call ensure_capacity(self, self%nrows + n)
        self%nrows = self%nrows + n
        if (self%has_nulls) call ensure_bitmap(self)
    end procedure grow_storage
    !
    module procedure copy_storage
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        idx = "1:self%nrows" if rank == 1 else ":, 1:self%nrows"
        w(f"""        case ({pk})
            if (allocated(self%{comp})) out%{comp}({idx}) = self%{comp}({idx})""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            if (allocated(self%str)) then
                if (.not. allocated(out%str)) allocate(out%str)
                out%str = self%str
            end if
        case (PK_NONE)
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"copy_storage: column has no active storage"
        end select
    end procedure copy_storage
    !
    module procedure append_storage
        integer(int64) :: old, n
        if (self%kind /= other%kind) error stop EP//"append_storage: column kinds differ"
        if (self%width /= other%width) error stop EP//"append_storage: column widths differ"
        n = other%nrows
        if (n == 0_int64) return
        old = self%nrows
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            call grow_storage(self, n)
            self%{comp}(old+1_int64:old+n) = other%{comp}(1:n)""")
        else:
            w(f"""        case ({pk})
            call grow_storage(self, n)
            self%{comp}(:, old+1_int64:old+n) = other%{comp}(:, 1:n)""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            call self%str%append_column(other%str)
            self%nrows = old + n
        case default
            error stop EP//"append_storage: column has no active storage"
        end select
    end procedure append_storage
    !
    module procedure append_row_of
        integer(int64) :: at, w, e, src_base, dst_base
        if (self%kind /= other%kind) error stop EP//"append_row_of: column kinds differ"
        if (self%width /= other%width) error stop EP//"append_row_of: column widths differ"
        if (irow < 1_int64 .or. irow > other%nrows) then
            error stop EP//"append_row_of: source row index out of range"
        end if
        w = int(self%width, int64)
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            call grow_storage(self, 1_int64)
            self%{comp}(self%nrows) = other%{comp}(irow)""")
        else:
            w(f"""        case ({pk})
            call grow_storage(self, 1_int64)
            self%{comp}(:, self%nrows) = other%{comp}(:, irow)""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            ! One flat store of nrows*width elements, row i at (i-1)*width + 1 .. i*width (RF6).
            ! append_from copies one element without materializing it as a Fortran string.
            do e = 1_int64, w
                call self%str%append_from(other%str, (irow - 1_int64)*w + e)
            end do
            self%nrows = self%nrows + 1_int64
            return
        case default
            error stop EP//"append_row_of: column has no active storage"
        end select
        ! Validity, element by element, exactly as `append` carries it for a whole column: the
        ! temporal kinds hold their null state inside the element (so the value copy above already
        ! moved it, and only the cache needs invalidating), while a bitmap kind has to copy bits --
        ! and only when the source row actually has one, so a null-free append allocates nothing.
        if (is_temporal_kind(self%kind)) then
            self%nulls_dirty = .true.
        else if (other%has_nulls) then
            src_base = (irow - 1_int64)*w
            dst_base = (self%nrows - 1_int64)*w
            do e = 1_int64, w
                if (bit_test(other%validity, src_base + e)) then
                    call ensure_bitmap(self)
                    call bit_set(self%validity, dst_base + e)
                end if
            end do
        end if
    end procedure append_row_of
    !
    module procedure paste_storage
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            self%{comp}(at:at+n-1_int64) = src%{comp}(from:from+n-1_int64)""")
        else:
            w(f"""        case ({pk})
            self%{comp}(:, at:at+n-1_int64) = src%{comp}(:, from:from+n-1_int64)""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            ! Unreachable through paste, which rejects the string kinds before it gets here --
            ! kept so this select is exhaustive over every storable kind, like its siblings above.
            error stop EP//"paste_storage: the string kinds cannot be overwritten in place" ! GCOVR_EXCL_LINE
        case default
            error stop EP//"paste_storage: column has no active storage"
        end select
    end procedure paste_storage
    !
end submodule parquet_columns_mutate ! GCOVR_EXCL_LINE""")
    return "\n".join(o) + "\n"


def main():
    global BENCH_GUARDS
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true",
                    help="compare with the committed files instead of writing; exit 1 on drift")
    ap.add_argument("--bench-guards", action="store_true",
                    help="MEASUREMENT BRANCH ONLY: emit get_at/set_at's guards inside cpp "
                         "#ifdefs so a build can select shipped / PF_BENCH_INLINE_GUARDS / "
                         "PF_BENCH_NO_GUARDS. Never commit this to main -- see "
                         "feature_benchmark_colindex.md")
    args = ap.parse_args()
    BENCH_GUARDS = args.bench_guards
    if BENCH_GUARDS and args.check:
        print("generate_parquet_columns.py: --check and --bench-guards together compare the "
              "committed files against the BENCHMARK variant, which is only meaningful on a "
              "measurement branch.", file=sys.stderr)

    root = pathlib.Path(__file__).resolve().parent.parent
    outputs = {
        root / "src" / "parquet_columns.f90": gen_spec(),
        root / "src" / "parquet_columns_access.f90": gen_access(),
        root / "src" / "parquet_columns_mutate.f90": gen_mutate(),
    }

    # Guard the project's hard 132-column limit at generation time: a template edit that pushes
    # one emitted line over the limit would otherwise only surface as a CI style failure.
    bad = []
    for path, text in outputs.items():
        for n, line in enumerate(text.splitlines(), start=1):
            if len(line) > 132:
                bad.append(f"{path.name}:{n}: {len(line)} columns")
    if bad:
        print("generate_parquet_columns.py: emitted lines exceed the 132-column limit:",
              file=sys.stderr)
        for b in bad:
            print("  " + b, file=sys.stderr)
        return 1

    if args.check:
        drift = False
        for path, text in outputs.items():
            current = path.read_text() if path.exists() else ""
            if current != text:
                print(f"generate_parquet_columns.py: {path.name} is out of date", file=sys.stderr)
                drift = True
        if drift:
            print("Re-run tools/generate_parquet_columns.py and commit the result.", file=sys.stderr)
            return 1
        print("generate_parquet_columns.py: committed output is up to date")
        return 0

    for path, text in outputs.items():
        path.write_text(text)
        print(f"generate_parquet_columns.py: wrote {path.relative_to(root)} "
              f"({len(text.splitlines())} lines)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
