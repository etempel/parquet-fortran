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
                                    kind-dispatched storage helpers (gather / grow / copy) that
                                    the hand-written structural operations are built on.

NOT emitted (hand-written, and never touched by this script):

  src/parquet_columns_validity.f90    kind-dispatched validity (RF8)
  src/parquet_columns_structural.f90  init/clear/append/append_nulls/delete_by_mask/reindex/deep_copy
  src/parquet_columns_string.f90      the string kinds, which delegate to parquet_string_column (DD1)

Usage:  tools/generate_parquet_columns.py [--check]

  --check  regenerate into memory and compare with the committed files; exit 1 on any
           difference (so CI, or a reviewer, can prove the committed output is current).
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
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_columns: "
    !
    !> Bits per validity-bitmap block. The bitmap is a hand-rolled `integer(int64)` array
    !! (1 = null, 0 = valid): measured 1.6-2x faster than stdlib's `bitset_large` on random
    !! set/test and on the reindex rebuild, at identical memory, with no int32 bit-index
    !! ceiling -- see feature_table_stage1_columns.md "Spike results".
    integer(int64), parameter :: BITS_PER_BLOCK = 64_int64
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
        ! --- queries ---
        procedure :: kindof                            !! The active PK_* discriminator.
        procedure :: length                            !! Number of rows stored.
        procedure :: colwidth                          !! Values per row (1 for scalar kinds).
        procedure :: validity_bytes                    !! Bytes the null bitmap occupies (0 when sparse).
        procedure :: unit_string                       !! Copy out the unit string ("" when unset).
        procedure :: set_unit                          !! Set (or clear) the unit string.
        procedure :: any_null                          !! Whether the column holds at least one null.
        procedure :: is_null                           !! Whether element i is null.
        procedure :: row_validity                      !! Build the whole per-row validity mask at once.
        ! --- validity mutation (sparse: see the module doc) ---
        procedure :: set_null                          !! Mark element i null.
        procedure :: clear_null                        !! Mark element i valid (value left unspecified).
        procedure :: compact_validity                  !! Drop the bitmap when no nulls remain.
        ! --- structural mutation ---
        procedure :: append                            !! Append another column of identical kind/width.
        procedure :: append_nulls                      !! Append n all-null rows.
        procedure :: delete_by_mask                    !! Keep only rows whose mask entry is .true.
        procedure :: reindex                           !! Reorder rows by a permutation.
        ! --- string-kind storage access (PK_STRING / PK_STRING_VEC) ---
        procedure :: string_column                     !! Pointer to the embedded string store.""")
    # generic groups
    for gname, doc in (("get_at", "Read element i (or row i's vector) out."),
                       ("set_at", "Write element i (or row i's vector)."),
                       ("set_all", "Replace every value in the column."),
                       ("data_ptr", "Zero-copy typed pointer to the active storage."),
                       ("append_values", "Append values, growing the column.")):
        tags = [k[0] for k in (KINDS if gname in ("get_at", "set_at", "set_all", "append_values") else ARRAY_KINDS)]
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
        !> Appends every row of `other`, which must have identical kind and width.
        module subroutine append(self, other)
            class(parquet_column), intent(inout) :: self !! the destination column.
            type(parquet_column), intent(in) :: other    !! the source column (unchanged).
        end subroutine append
        !> Appends `n` all-null rows (allocating the bitmap if this is the first null).
        module subroutine append_nulls(self, n)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: n              !! number of null rows to append (>= 0).
        end subroutine append_nulls
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
    end interface
    !
    ! ---- Validity, kind-dispatched (parquet_columns_validity) ----
    interface
        !> Whether the column holds at least one null. Cheap for bitmap-backed and string kinds;
        !! for temporal kinds (whose null state lives inside each element) the answer is cached,
        !! and a rescan is needed after a mutation -- which is why `self` is `intent(inout)`
        !! despite this being a query. The column is reached through the table's `cache` pointer,
        !! so this does not stop a table read-accessor from staying `intent(in)`.
        module function any_null(self) result(res)
            class(parquet_column), intent(inout) :: self !! the column (null cache may be refreshed).
            logical :: res                               !! .true. when at least one row is null.
        end function any_null
        !> Whether element `i` is null. For a *_VEC kind this is row `i`'s FIRST element; use
        !! `is_null_element` semantics via the bitmap when per-element nulls matter.
        module function is_null(self, i) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64), intent(in) :: i           !! 1-based row index.
            logical :: res                            !! .true. when the row is null.
        end function is_null
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
        module subroutine row_validity(self, valid)
            class(parquet_column), intent(inout) :: self  !! the column (null cache may be refreshed).
            logical, allocatable, intent(out) :: valid(:) !! per-row mask, or unallocated when no nulls.
        end subroutine row_validity
        !> Marks row `i` null, allocating the bitmap on first use (R2).
        module subroutine set_null(self, i)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine set_null
        !> Marks row `i` valid. The value behind it is unspecified until written.
        module subroutine clear_null(self, i)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine clear_null
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
        !> Writes string element `i` (PK_STRING).
        module subroutine set_at_str(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            character(len=*), intent(in) :: value        !! the new value.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_str
        !> Writes row `i`'s whole string vector (PK_STRING_VEC).
        module subroutine set_at_strv(self, i, value, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            integer(int64), intent(in) :: i              !! 1-based row index.
            character(len=*), intent(in) :: value(:)     !! width values for row i.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_at_strv
        !> Replaces every value in a PK_STRING column.
        module subroutine set_all_str(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:)    !! exactly nrows values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_str
        !> Replaces every value in a PK_STRING_VEC column, shaped (width, nrows).
        module subroutine set_all_strv(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:,:)  !! (width, nrows) values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_strv
        !> Appends string rows to a PK_STRING column.
        module subroutine append_values_str(self, values)
            class(parquet_column), intent(inout) :: self !! the column.
            character(len=*), intent(in) :: values(:)    !! rows to append.
        end subroutine append_values_str
        !> Appends string vector rows to a PK_STRING_VEC column, shaped (width, n).
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
        end subroutine set_at_{tag}
        !> Replaces every value in a {pk} column.
        module subroutine set_all_{tag}(self, values, modify_nulls)
            class(parquet_column), intent(inout) :: self !! the column.
            {decl}, intent(in) :: values{dim2}      !! exactly the column's own shape.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_all_{tag}
        !> Zero-copy typed pointer to a {pk} column's storage. The kind must match EXACTLY (no
        !! widening, DD2). Any structural mutation invalidates the pointer.
        module subroutine data_ptr_{tag}(self, p)
            class(parquet_column), intent(in), target :: self !! the column.
            {decl}, pointer, intent(out) :: p{dim2}      !! alias to the live storage.
        end subroutine data_ptr_{tag}""")
    w("    end interface")
    w("    !")
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
        pure module function bits_needed(self) result(res)
            class(parquet_column), intent(in) :: self !! the column.
            integer(int64) :: res                     !! required bit count.
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
        pure module subroutine bit_set(map, b)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b         !! 1-based bit index.
        end subroutine bit_set
        !> Clears bit `b` (1-based) of an allocated bitmap.
        pure module subroutine bit_clear(map, b)
            integer(int64), intent(inout) :: map(:) !! the bitmap.
            integer(int64), intent(in) :: b         !! 1-based bit index.
        end subroutine bit_clear
        !> Ensures the bitmap exists and covers every element, zero-filling new blocks (0 = valid),
        !! and marks the column bitmap-backed.
        module subroutine ensure_bitmap(self)
            class(parquet_column), intent(inout) :: self !! the column.
        end subroutine ensure_bitmap
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
end module parquet_columns""")
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
!! both the null rows and the bitmap untouched. Temporal kinds carry their null state inside
!! the element, so they only invalidate the cached null flag.
submodule (parquet_columns) parquet_columns_access
    implicit none
contains""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        temporal = cat == "tmp"
        if rank == 1:
            w(f"""    !
    module procedure get_at_{tag}
        call check_kind(self, {pk}, "get_at")
        call check_index(self, i, "get_at")
        value = self%{comp}(i)
    end procedure get_at_{tag}
    !
    module procedure set_at_{tag}
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, {pk}, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%{comp}(i) = value""")
            if temporal:
                w("        self%nulls_dirty = .true.")
            else:
                w("        if (self%has_nulls) call bit_clear(self%validity, i)")
            w(f"""    end procedure set_at_{tag}
    !
    module procedure set_all_{tag}
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, {pk}, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%{comp}(1:self%nrows) = values""")
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
    module procedure data_ptr_{tag}
        call check_kind(self, {pk}, "data_ptr")
        p => self%{comp}(1:self%nrows)
    end procedure data_ptr_{tag}""")
        else:
            w(f"""    !
    module procedure get_at_{tag}
        call check_kind(self, {pk}, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%{comp}(:, i)
    end procedure get_at_{tag}
    !
    module procedure set_at_{tag}
        logical :: mod_nulls{"" if temporal else chr(10) + "        integer(int64) :: e, base"}
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, {pk}, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%{comp}(:, i) = value""")
            if temporal:
                w("        self%nulls_dirty = .true.")
            else:
                w("""        if (self%has_nulls) then
            base = (i - 1_int64)*int(self%width, int64)
            do e = 1_int64, int(self%width, int64)
                call bit_clear(self%validity, base + e)
            end do
        end if""")
            w(f"""    end procedure set_at_{tag}
    !
    module procedure set_all_{tag}
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, {pk}, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%{comp}(:, 1:self%nrows) = values""")
            if temporal:
                w("            self%nulls_dirty = .true.")
            else:
                w("            call drop_bitmap(self)")
            w("""        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%""" + comp + """(:, k) = values(:, k)
            end do""")
            if temporal:
                w("            self%nulls_dirty = .true.")
            w(f"""        end if
    end procedure set_all_{tag}
    !
    module procedure data_ptr_{tag}
        call check_kind(self, {pk}, "data_ptr")
        p => self%{comp}(:, 1:self%nrows)
    end procedure data_ptr_{tag}""")
    w("    !")
    w("end submodule parquet_columns_access")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_columns_mutate.f90
# --------------------------------------------------------------------------------------
def gen_mutate():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind value append for `parquet_column`, plus the three kind-dispatched storage
!! helpers (`gather_storage`, `grow_storage`, `copy_storage`) that the hand-written structural
!! operations in `parquet_columns_structural` are built on -- so `reindex`, `delete_by_mask`,
!! `append`, `append_nulls` and `deep_copy` each exist ONCE, kind-agnostically, instead of
!! eighteen times.
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
    end procedure gather_storage
    !
    module procedure grow_storage
        integer(int64) :: old, new""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        dims = "(:)" if rank == 1 else "(:,:)"
        w(f"        {decl}, allocatable :: tmp_{comp}{dims}")
    w("""        if (n < 0_int64) error stop EP//"grow_storage: negative row count"
        if (n == 0_int64) return
        old = self%nrows
        new = old + n
        select case (self%kind)""")
    for k in ARRAY_KINDS:
        tag, pk, decl, comp, rank, cat = k
        if rank == 1:
            w(f"""        case ({pk})
            allocate(tmp_{comp}(new))
            if (old > 0_int64) tmp_{comp}(1:old) = self%{comp}(1:old)
            call move_alloc(tmp_{comp}, self%{comp})""")
        else:
            w(f"""        case ({pk})
            allocate(tmp_{comp}(self%width, new))
            if (old > 0_int64) tmp_{comp}(:, 1:old) = self%{comp}(:, 1:old)
            call move_alloc(tmp_{comp}, self%{comp})""")
    w("""        case (PK_STRING, PK_STRING_VEC)
            ! the string store grows through its own append path (DD1)
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"grow_storage: column has no active storage"
        end select
        self%nrows = new
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
end submodule parquet_columns_mutate""")
    return "\n".join(o) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true",
                    help="compare with the committed files instead of writing; exit 1 on drift")
    args = ap.parse_args()

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
