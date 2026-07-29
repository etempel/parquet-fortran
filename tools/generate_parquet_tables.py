#!/usr/bin/env python3
"""Generate the per-kind blocks of the `parquet_tables` table layer.

`parquet_table` (see feature_table.md §4 and feature_table_stage3a_table_core.md) exposes the
same handful of operations over all 18 column kinds -- a zero-copy pointer accessor, a
widening copy-out, a copy-back, a from-scratch column add, and "read one file column into a
parquet_column". Written by hand that is several hundred near-identical procedures, each
needing its own `!>`/`!!` doc-comments, so they are emitted from the kind table instead.

The kind table itself is IMPORTED from tools/generate_parquet_columns.py rather than copied:
one list of kinds serves both generators, so adding a kind cannot leave the two layers
disagreeing about what exists (feature_table_stage3a_table_core.md §12b).

Emitted (all COMMITTED to the repository, exactly like the parquet_columns generator's own
output -- nothing is generated at build time, so the fpm build stays dependency-free):

  src/parquet_tables.f90              module spec: the three types, REGIME_*/RES_* constants,
                                      every type-bound binding, and the interface bodies for
                                      every submodule-implemented procedure (hand-written ones
                                      included -- Fortran requires the interface to live in the
                                      parent module).
  src/parquet_tables_access.f90       col_ptr_* / get_arr_* / set_arr_*
  src/parquet_tables_addcol.f90       add_column_*
  src/parquet_tables_materialize.f90  read one file column of each kind into a parquet_column

NOT emitted (hand-written, and never touched by this script):

  src/parquet_tables_lifecycle.f90    open/new/finalize/assignment guard/slot bookkeeping
  src/parquet_tables_query.f90        nrows/ncols/column_names/kind/width/unit/is_null/...
  src/parquet_tables_read.f90         kind resolution + materialize-all orchestration + release
  src/parquet_tables_write.f90        parquet_write_table

Usage:  tools/generate_parquet_tables.py [--check]

  --check  regenerate into memory and compare with the committed files; exit 1 on any
           difference (so CI, or a reviewer, can prove the committed output is current).
"""

import argparse
import importlib.util
import pathlib
import sys

_HERE = pathlib.Path(__file__).resolve().parent


def _load_kind_table():
    """Import KINDS/ARRAY_KINDS from the columns generator -- one source of truth."""
    spec = importlib.util.spec_from_file_location(
        "generate_parquet_columns", _HERE / "generate_parquet_columns.py")
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod.KINDS, mod.ARRAY_KINDS


KINDS, ARRAY_KINDS = _load_kind_table()

# --------------------------------------------------------------------------------------
# Table-layer facts the columns generator's own table does not need.
#
#   widen   PK_* discriminators a get_arr_<tag> accepts BESIDES its own, with the
#           parquet_column storage tag to read them through. This is the D11 widening rule,
#           and it is deliberately the SAME set the reader already applies (int32 -> int64,
#           float32 -> float64) -- the copy path widens, the pointer path never does (DD2).
#   ftype   the token parquet_get_column_type reports for a file column of this kind.
# --------------------------------------------------------------------------------------
WIDEN = {
    "i64": [("PK_INT32", "i32")],
    "f64": [("PK_FLOAT32", "f32")],
    "i64v": [("PK_INT32_VEC", "i32v")],
    "f64v": [("PK_FLOAT32_VEC", "f32v")],
}

FTYPE = {
    "i32": "int32", "i64": "int64", "f32": "float32", "f64": "float64",
    "bool": "boolean", "str": "string", "date": "date", "time": "time", "ts": "timestamp",
}

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!"""

# Kinds with a zero-copy pointer accessor: everything except the two string kinds, whose
# storage is a parquet_string_column rather than a plain Fortran array, so there is nothing
# kind-matched to alias (the same reason parquet_column has no data_ptr for them).
PTR_KINDS = ARRAY_KINDS


def dims(rank):
    return "(:)" if rank == 1 else "(:,:)"


def elem_decl(k):
    """Declaration of one element for a caller-facing dummy argument."""
    tag, pk, decl, comp, rank, cat = k
    return "character(len=:)" if cat == "str" else decl


def shape_comment(rank):
    return "one value per row" if rank == 1 else "(element, row), shaped (width, nrows)"


def decl_line(indent, code, comment, align=60):
    """One declaration plus its trailing `!!` doc tag, aligned but never past 132 columns.

    The alignment column is cosmetic; the 132-column limit is not (CLAUDE.md), so a long
    declaration simply gets a single space before its comment instead of pushing the line over.
    """
    left = " " * indent + code
    pad = max(1, align - len(left))
    if len(left) + pad + len(comment) > 132:
        pad = 1
    return left + " " * pad + comment


# --------------------------------------------------------------------------------------
# src/parquet_tables.f90 -- the module spec
# --------------------------------------------------------------------------------------
def gen_spec():
    o = []
    w = o.append
    w(BANNER)
    w("""!> A whole parquet file as one in-memory table: `parquet_table`.
!!
!! `parquet_table` sits on top of the `parquet` reader/writer rather than replacing it. Opening
!! one reads every supported column into the type-erased `parquet_column` store (`parquet_columns`)
!! and frees the Arrow-side buffers as it goes, so the table owns the sole Fortran copy of each
!! column. From there a column is reached either by a zero-copy typed pointer (`%col`, exact kind)
!! or by a widening copy (`%get`), a table can be built from scratch in memory (`parquet_new_table`
!! + `%add_column`), and the whole thing is written back out through an ordinary `parquet_schema`
!! (`parquet_write_table`).
!!
!! Four things are worth knowing before using it:
!!
!! * **`%get` is the friendly path; `%col` is the fast one.** `%get` copies the column into an
!!   allocatable array of the caller's own kind, widening int32 -> int64 and float32 -> float64
!!   on the way, so a caller who just wants the numbers never has to ask what type the file used.
!!   `%col` hands back a pointer straight into the store -- zero copy, writable -- but the pointer
!!   kind must match the stored kind EXACTLY, so it is for code that already knows the type (or
!!   has asked `%kind`).
!! * **Everything is read at open time.** This is the eager, read-only, single-threaded core:
!!   there is no lazy first touch and no row-scope narrowing yet, so opening a wide file loads all
!!   of it. `%residency` reports what is resident.
!! * **Assignment is blocked.** The column store lives behind a pointer, so `b = a` would leave two
!!   tables sharing (and later double-freeing) one store. `b = a` is a hard error rather than a
!!   silent corruption; copying a table comes with `%clone` in a later milestone.
!! * **A table is NOT thread-safe.** Nothing here takes a lock. Reading an already-open table from
!!   several threads is fine; opening, adding columns and writing are single-threaded operations.
!!
!! Depends on `parquet_columns` (the value store) and `parquet` (the reader/writer it drives).
module parquet_tables
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
    use parquet_columns
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use parquet, only : parquet_reader, parquet_writer, parquet_schema, parquet_column_type, &
        parquet_open_reader, parquet_close_reader, parquet_get_nrows, parquet_get_col_size, &
        parquet_get_column_names, parquet_get_column_type, parquet_column_exists, &
        parquet_release_column, parquet_read_column, parquet_get_metadata, parquet_get_string_length, &
        parquet_open_writer, parquet_write_column, parquet_close_writer, parquet_write_row_mask
    !
    implicit none
    private
    !
    public :: parquet_table
    public :: parquet_open_table
    public :: parquet_new_table
    public :: parquet_write_table
    public :: REGIME_FULL, REGIME_SLICE
    public :: RES_EMPTY, RES_PARTIAL, RES_FULL
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_table: "
    !
    !> Spare descriptor slots allocated beyond the file's own column count, so the common
    !! "open a file, add a few computed columns" case never reallocates `cols(:)` (RF16). This
    !! is an optimization only -- the documented contract stays the broad one: ANY column- or
    !! row-structural mutation invalidates every outstanding pointer into the table.
    integer, parameter :: COL_HEADROOM = 8
    !
    ! ---- Row-scope regimes (D14) ----
    integer, parameter :: REGIME_FULL = 0  !! the table covers every row of the file.
    integer, parameter :: REGIME_SLICE = 1 !! reserved: a contiguous row slice (a later milestone).
    !
    ! ---- Column residency (D14/RF20) ----
    integer, parameter :: RES_EMPTY = 0   !! no values held (never read, or an unsupported type).
    integer, parameter :: RES_PARTIAL = 1 !! reserved: some row groups resident (a later milestone).
    integer, parameter :: RES_FULL = 2    !! the whole column, across the table's row scope, is held.
    !
    !> One column slot: its identity and provenance, plus the values themselves.
    type :: parquet_table_column
        character(len=:), allocatable :: name      !! internal/logical name -- ALWAYS the lookup key.
        character(len=:), allocatable :: file_name !! physical name in the file (== name for now).
        integer :: declared_kind = PK_NONE         !! PK_* this slot holds, or PK_NONE if unsupported.
        logical :: file_source = .false.           !! .true. iff a backing file column exists.
        logical :: predefined = .false.            !! reserved: a generated accessor exists for it.
        logical :: user_populated = .false.        !! .true. once user values were written into it.
        logical :: supported = .true.              !! .false. for a foreign/container column type.
        integer :: residency = RES_EMPTY           !! RES_EMPTY or RES_FULL; RES_PARTIAL reserved.
        logical, allocatable :: rg_loaded(:)       !! reserved for per-row-group residency.
        type(parquet_column) :: values             !! the value store (parquet_columns).
    end type parquet_table_column
    !
    !> Everything a read may have to mutate, held behind ONE pointer so that read accessors can
    !! stay `intent(in)`: allocating through a pointer's target is allowed on an `intent(in)`
    !! dummy, whereas allocating an `allocatable` component of one is not. This is also what
    !! keeps `%col`'s returned pointer valid without the caller declaring the table `target` --
    !! see the module doc and `col_ptr_*`.
    type :: parquet_table_cache
        type(parquet_table_column), allocatable :: cols(:) !! descriptor slots; `ncols` are live.
        integer :: ncols = 0                               !! live slot count (cols may be longer).
        type(parquet_reader), allocatable :: reader        !! present iff the table is file-backed.
        logical :: reads_started = .false.                 !! reserved: locks read-time transforms.
    end type parquet_table_cache
    !""")
    w(gen_table_type())
    w("    !")
    w(gen_spec_interfaces())
    w("    !")
    w("end module parquet_tables")
    return "\n".join(o) + "\n"


def gen_table_type():
    o = []
    w = o.append
    w("""    !> A whole table: a column store plus the row scope and provenance describing it.
    !! Declared by the caller (`type(parquet_table) :: t`), filled by `parquet_open_table` or
    !! `parquet_new_table`, and freed automatically when it goes out of scope.
    type :: parquet_table
        private
        logical :: file_backed = .false.            !! .true. if opened from a parquet file.
        logical :: detached = .false.               !! reserved: set by a row-structural mutation.
        character(len=:), allocatable :: source_file !! the file this table was opened from.
        integer :: regime = REGIME_FULL             !! REGIME_FULL; REGIME_SLICE reserved.
        integer(int64) :: row_lo = 1                !! first row of the scope (1 in the full regime).
        integer(int64) :: row_hi = -1               !! last row of the scope (nrows in the full regime).
        integer(int64) :: row_count = 0             !! rows every column in this table holds.
        type(parquet_table_cache), pointer :: cache => null() !! the column store (see its own doc).
    contains
        ! --- introspection ---
        procedure :: nrows => table_nrows            !! Number of rows every column holds.
        procedure :: ncols => table_ncols            !! Number of columns the table has.
        procedure :: column_names => table_column_names !! Copy out every column name, in order.
        procedure :: has_column => table_has_column  !! Whether a column of this name exists.
        procedure :: kind => table_column_kind       !! A column's PK_* kind discriminator.
        procedure :: width => table_column_width     !! A column's values-per-row (1 if scalar).
        procedure :: unit => table_column_unit       !! Copy out a column's unit string.
        procedure :: residency => table_column_residency !! A column's RES_* residency state.
        procedure :: is_null => table_is_null        !! Whether row i of a column is null.
        procedure :: is_detached => table_is_detached !! Whether the table has left its file behind.
        procedure :: is_supported => table_is_supported !! Whether a column's type can be read.
        procedure :: filename => table_filename      !! Copy out the file this table came from.
        procedure :: get_file_metadata => table_get_file_metadata !! One key from the file's metadata.""")
    # pointer accessors
    w("        ! --- zero-copy pointer access (exact kind) ---")
    for k in PTR_KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_ptr_{tag} !! %col specific for the {tag} kind.")
    w("        !> Points `p` at a column's storage: zero copy, writable, and the pointer kind must")
    w("        !! match the stored kind exactly (ask %kind first if you do not know it).")
    w("        generic :: col => " + wrap_list([f"col_ptr_{k[0]}" for k in PTR_KINDS], 12,
                                                first_prefix=len("        generic :: col => ")))
    # get
    w("        ! --- copy out (widens int32->int64, float32->float64) ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: get_arr_{tag} !! %get specific for the {tag} kind.")
    w("        procedure, private :: get_arr_str  !! %get specific returning a parquet_string_column.")
    w("        procedure, private :: get_arr_chr  !! %get specific returning a character array.")
    w("        procedure, private :: get_arr_chrv !! %get specific returning a character (elem, row) array.")
    w("        !> Copies a column into a freshly allocated array of the caller's own kind.")
    w("        generic :: get => " + wrap_list(
        [f"get_arr_{k[0]}" for k in ARRAY_KINDS] + ["get_arr_str", "get_arr_chr", "get_arr_chrv"], 12,
        first_prefix=len("        generic :: get => ")))
    # set
    w("        ! --- copy back (same length, exact kind) ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: set_arr_{tag} !! %set specific for the {tag} kind.")
    w("        procedure, private :: set_arr_chr  !! %set specific taking a character array.")
    w("        procedure, private :: set_arr_chrv !! %set specific taking a character (elem, row) array.")
    w("        !> Replaces every value of an existing column from an array of the same length.")
    w("        generic :: set => " + wrap_list(
        [f"set_arr_{k[0]}" for k in ARRAY_KINDS] + ["set_arr_chr", "set_arr_chrv"], 12,
        first_prefix=len("        generic :: set => ")))
    # add_column
    w("        ! --- from-scratch construction ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: add_column_{tag} !! %add_column specific for the {tag} kind.")
    w("        procedure, private :: add_column_chr  !! %add_column specific taking a character array.")
    w("        procedure, private :: add_column_chrv !! %add_column specific taking a character (elem, row) array.")
    w("        !> Appends a new column, taking its values (and so its kind, width and row count).")
    w("        generic :: add_column => " + wrap_list(
        [f"add_column_{k[0]}" for k in ARRAY_KINDS] + ["add_column_chr", "add_column_chrv"], 12,
        first_prefix=len("        generic :: add_column => ")))
    w("""        ! --- lifecycle ---
        !> Blocks intrinsic assignment: the store lives behind a pointer, so a default `b = a`
        !! would leave two tables sharing one store and double-freeing it.
        generic :: assignment(=) => table_assign_guard
        procedure, private :: table_assign_guard !! The blocking defined assignment.
        final :: table_finalize                  !! Frees the store; never fails, never validates.
    end type parquet_table""")
    return "\n".join(o)


def wrap_list(names, indent, first_prefix=0):
    """Fortran continuation-wrapped comma list, kept under the 132-column limit.

    `first_prefix` is the width of whatever the caller already put on the first line (e.g.
    "        generic :: col => "), which the budget must include or that line alone overflows.
    """
    pad = " " * indent
    out, line = [], ""
    budget = 132 - 2  # room for the trailing " &"
    for i, n in enumerate(names):
        piece = n + ("" if i == len(names) - 1 else ", ")
        used = (first_prefix if not out else indent) + len(line)
        if used + len(piece) > budget:
            out.append(line.rstrip() + " &")
            line = ""
        line += piece
    out.append(line.rstrip())
    return ("\n" + pad).join(out)


# --------------------------------------------------------------------------------------
# Interface bodies (module spec)
# --------------------------------------------------------------------------------------
def gen_spec_interfaces():
    o = []
    w = o.append
    w("""    ! ---- Lifecycle (parquet_tables_lifecycle) ----
    interface
        !> Opens `filename` and reads every supported column into `table`, freeing each column's
        !! Arrow buffers as it goes. Columns whose physical type this library cannot read (a
        !! foreign decimal/uint32 column, a LIST or MAP) do NOT stop the open: their slot is
        !! created and marked unsupported, they still appear in %column_names, and only an
        !! attempt to read one is an error. `table` is intent(out), so reopening the same
        !! variable frees the previous table first.
        module subroutine parquet_open_table(table, filename)
            type(parquet_table), intent(out) :: table !! the table to fill.
            character(len=*), intent(in) :: filename  !! parquet file to open.
        end subroutine parquet_open_table
        !> Prepares an empty in-memory table with no columns and no rows. The first %add_column
        !! fixes the row count; every later one must match it.
        module subroutine parquet_new_table(table)
            type(parquet_table), intent(out) :: table !! the table to initialize.
        end subroutine parquet_new_table
        !> Always error stops: see the `assignment(=)` binding.
        module subroutine table_assign_guard(lhs, rhs)
            class(parquet_table), intent(out) :: lhs !! unused -- this procedure never returns.
            type(parquet_table), intent(in) :: rhs   !! unused -- this procedure never returns.
        end subroutine table_assign_guard
        !> Frees the column store and abandons the reader. Runs at scope exit and on an
        !! intent(out) reopen, so it must always succeed silently -- it validates nothing.
        module subroutine table_finalize(self)
            type(parquet_table), intent(inout) :: self !! the table being destroyed.
        end subroutine table_finalize
        !> Appends an empty slot named `name` and returns its index, growing `cols(:)` if the
        !! headroom is used up. error stops if the name is already taken and `force` is absent.
        module subroutine table_new_slot(self, name, force, idx)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the new column's name.
            logical, intent(in), optional :: force      !! .true. replaces an existing same-named column.
            integer, intent(out) :: idx                 !! 1-based index of the slot to fill.
        end subroutine table_new_slot
        !> Fixes or checks the table's row count when a column of `n` rows is added: the first
        !! column sets it, every later one must match. error stops on a mismatch.
        module subroutine table_fix_nrows(self, name, n)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column being added (for the message).
            integer(int64), intent(in) :: n             !! that column's row count.
        end subroutine table_fix_nrows
    end interface
    !
    ! ---- Introspection (parquet_tables_query) ----
    interface
        !> Number of rows every column of this table holds.
        module function table_nrows(self) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64) :: n                      !! row count.
        end function table_nrows
        !> Number of columns this table has.
        module function table_ncols(self) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            integer :: n                             !! column count.
        end function table_ncols
        !> Copies out every column's name, in file/insertion order, blank-padded to the longest.
        module subroutine table_column_names(self, names)
            class(parquet_table), intent(in) :: self                  !! the table.
            character(len=:), allocatable, intent(out) :: names(:)    !! one entry per column.
        end subroutine table_column_names
        !> Whether a column of this name exists (supported or not).
        module function table_has_column(self, name) result(found)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            logical :: found                         !! .true. if the table has it.
        end function table_has_column
        !> A column's PK_* kind discriminator (PK_NONE for an unsupported column).
        module function table_column_kind(self, name, found) result(k)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: k                              !! the PK_* constant.
        end function table_column_kind
        !> A column's values-per-row: 1 for a scalar kind, the vector width for a *_VEC kind.
        module function table_column_width(self, name, found) result(wdt)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: wdt                            !! values per row.
        end function table_column_width
        !> Copies out a column's unit string ("" when it has none).
        module subroutine table_column_unit(self, name, u, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            character(len=:), allocatable, intent(out) :: u      !! the unit, or "".
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine table_column_unit
        !> A column's residency: RES_FULL once read, RES_EMPTY for an unsupported column.
        module function table_column_residency(self, name, found) result(r)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: r                              !! the RES_* constant.
        end function table_column_residency
        !> Whether a column's physical type is one this library can read.
        module function table_is_supported(self, name, found) result(ok)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            logical :: ok                             !! .true. if readable.
        end function table_is_supported
        !> Whether the table has been detached from its file by a row-structural mutation.
        module function table_is_detached(self) result(d)
            class(parquet_table), intent(in) :: self !! the table.
            logical :: d                             !! .true. once detached.
        end function table_is_detached
        !> Copies out the file this table was opened from ("" for an in-memory table).
        module subroutine table_filename(self, fname)
            class(parquet_table), intent(in) :: self            !! the table.
            character(len=:), allocatable, intent(out) :: fname !! the file name, or "".
        end subroutine table_filename
        !> Reads one key from the source file's table metadata. `found` reports a missing key.
        module subroutine table_get_file_metadata(self, key, value, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: key                  !! metadata key.
            character(len=:), allocatable, intent(out) :: value  !! the value, or "".
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine table_get_file_metadata
        !> Whether row `i` of a column is null.
        module function table_is_null(self, name, i) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int64), intent(in) :: i          !! 1-based row index.
            logical :: isnull                        !! .true. if that element is null.
        end function table_is_null
        !> Resolves `name` to its 1-based slot index, or 0 when absent. The single lookup every
        !! accessor goes through, so a rename or remap only has to change one place.
        module function table_find(self, name) result(idx)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer :: idx                           !! slot index, or 0.
        end function table_find
        !> Resolves `name` for a value access: aborts (or reports through `found`) when the
        !! column is missing, unsupported or not resident.
        module subroutine table_resolve(self, name, proc, idx, found)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            character(len=*), intent(in) :: proc      !! calling procedure, for the message.
            integer, intent(out) :: idx               !! slot index, or 0 when `found` is present.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
        end subroutine table_resolve
        !> Builds the "(file 'x.parquet', column 'y')" suffix every error message carries.
        module subroutine table_context_suffix(self, name, suffix)
            class(parquet_table), intent(in) :: self              !! the table.
            character(len=*), intent(in) :: name                  !! column name ("" to omit it).
            character(len=:), allocatable, intent(out) :: suffix  !! the message suffix.
        end subroutine table_context_suffix
        !> error stops unless `self%cache` is associated -- the guard every accessor runs first.
        module subroutine table_check_open(self, proc)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_check_open
        !> error stops unless slot `idx` holds exactly `kind`. The exact-kind rule the pointer
        !! path and the copy-back path both enforce (the copy-OUT path widens instead).
        module subroutine table_require_kind(self, idx, kind, proc)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: idx               !! slot index.
            integer, intent(in) :: kind              !! required PK_* discriminator.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_require_kind
        !> error stops unless slot `idx` holds exactly `n` rows -- a %set replaces values, never
        !! the row set, so a different length is a row-structural change and not allowed here.
        module subroutine table_require_length(self, idx, n, proc)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: idx               !! slot index.
            integer(int64), intent(in) :: n          !! the caller's array length.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_require_length
    end interface
    !
    ! ---- Materialization orchestration (parquet_tables_read) ----
    interface
        !> Maps a file column's canonical type token and element count onto a PK_* kind.
        !! `ok` is .false. for a token this library cannot read.
        module subroutine table_kind_from_type(type_name, col_size, kind, ok)
            character(len=*), intent(in) :: type_name !! canonical token from parquet_get_column_type.
            integer, intent(in) :: col_size           !! elements per row (1 for a scalar column).
            integer, intent(out) :: kind              !! the resolved PK_* constant.
            logical, intent(out) :: ok                !! .false. if the token is not readable.
        end subroutine table_kind_from_type
        !> Reads one file column into slot `idx`'s value store and marks it RES_FULL.
        module subroutine table_materialize(self, idx)
            class(parquet_table), intent(in) :: self !! the table (mutates through %cache).
            integer, intent(in) :: idx               !! slot to fill.
        end subroutine table_materialize
        !> Reads every supported column of a freshly opened table, releasing each column's Arrow
        !! buffers as it goes so peak memory stays one column above the Fortran store.
        module subroutine table_materialize_all(self)
            class(parquet_table), intent(in) :: self !! the table (mutates through %cache).
        end subroutine table_materialize_all
    end interface
    !
    ! ---- Write-out (parquet_tables_write) ----
    interface
        !> Writes `table` to `filename` using `schema` to choose and name the output columns:
        !! every enabled schema field is looked up in the table BY ITS INTERNAL NAME, and written
        !! under its own output name (a col_map: rename is honoured automatically). A schema field
        !! with no matching table column is an error; a table column the schema does not name is
        !! simply not written. `row_mask` writes a row subset without changing the table.
        module subroutine parquet_write_table(table, filename, schema, row_mask)
            type(parquet_table), intent(in) :: table   !! the table to write.
            character(len=*), intent(in) :: filename   !! output parquet file.
            type(parquet_schema), intent(inout) :: schema !! output schema (chooses/renames columns).
            logical, intent(in), optional :: row_mask(:)  !! per-row write mask.
        end subroutine parquet_write_table
    end interface""")
    w("    !")
    w("    ! ---- Zero-copy pointer access (parquet_tables_access) ----")
    w("    interface")
    for k in PTR_KINDS:
        w(ptr_iface(k))
    w("    end interface")
    w("    !")
    w("    ! ---- Copy out (parquet_tables_access) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        w(get_iface(k))
    w(get_str_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- Copy back (parquet_tables_access) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        w(set_iface(k))
    w(set_str_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- From-scratch construction (parquet_tables_addcol) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        w(add_iface(k))
    w(add_str_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- Per-kind materialization (parquet_tables_materialize) ----")
    w("    interface")
    for k in KINDS:
        w(mat_iface(k))
    w("""        !> Dispatches one file column's read to the specific matching `kind`.
        module subroutine table_materialize_kind(kind, reader, name, col, nrows, wdt, unit)
            integer, intent(in) :: kind                  !! PK_* discriminator to read as.
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine table_materialize_kind""")
    w("    end interface")
    return "\n".join(o)


def ptr_iface(k):
    tag, pk, decl, comp, rank, cat = k
    return f"""        !> Points `p` at a {pk} column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_{tag}(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
{decl_line(12, f"{decl}, pointer, intent(out) :: p{dims(rank)}", "!! alias to the live storage.")}
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_{tag}"""


def get_iface(k):
    tag, pk, decl, comp, rank, cat = k
    widen = ""
    if tag in WIDEN:
        srcs = ", ".join(p for p, _ in WIDEN[tag])
        widen = f"\n        !! Also accepts a {srcs} column, widening on the way."
    return f"""        !> Copies a {pk} column out into a freshly allocated array.{widen}
        module subroutine get_arr_{tag}(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
{decl_line(12, f"{decl}, allocatable, intent(out) :: arr{dims(rank)}", f"!! {shape_comment(rank)}.")}
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_{tag}"""


def get_str_iface():
    return """        !> Copies a PK_STRING column out as a parquet_string_column (offsets+data+validity).
        module subroutine get_arr_str(self, name, arr, found)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            type(parquet_string_column), intent(inout) :: arr      !! cleared, then filled.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine get_arr_str
        !> Copies a PK_STRING column out as a fixed-width character array, sized to the longest
        !! element present. A null element comes back blank -- gate on %is_null to tell a null
        !! from a genuinely empty string.
        module subroutine get_arr_chr(self, name, arr, found)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            character(len=:), allocatable, intent(out) :: arr(:)   !! one value per row.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine get_arr_chr
        !> Copies a PK_STRING_VEC column out as a fixed-width character (element, row) array.
        module subroutine get_arr_chrv(self, name, arr, found)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            character(len=:), allocatable, intent(out) :: arr(:,:) !! (element, row) values.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine get_arr_chrv"""


def set_iface(k):
    tag, pk, decl, comp, rank, cat = k
    return f"""        !> Replaces every value of a {pk} column. The array must have the column's own shape.
        module subroutine set_arr_{tag}(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
{decl_line(12, f"{decl}, intent(in) :: arr{dims(rank)}", f"!! {shape_comment(rank)}.")}
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_{tag}"""


def set_str_iface():
    return """        !> Replaces every value of a PK_STRING column from a character array.
        module subroutine set_arr_chr(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: arr(:)       !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_chr
        !> Replaces every value of a PK_STRING_VEC column from a character (element, row) array.
        module subroutine set_arr_chrv(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: arr(:,:)     !! (element, row) values.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_chrv"""


def add_iface(k):
    tag, pk, decl, comp, rank, cat = k
    return f"""        !> Appends a new {pk} column holding `values`.
        module subroutine add_column_{tag}(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
{decl_line(12, f"{decl}, intent(in) :: values{dims(rank)}", f"!! {shape_comment(rank)}.")}
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_{tag}"""


def add_str_iface():
    return """        !> Appends a new PK_STRING column holding `values` (trailing blanks are trimmed).
        module subroutine add_column_chr(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            character(len=*), intent(in) :: values(:)    !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_chr
        !> Appends a new PK_STRING_VEC column holding `values` (trailing blanks are trimmed).
        module subroutine add_column_chrv(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            character(len=*), intent(in) :: values(:,:)  !! (element, row) values.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_chrv"""


def mat_iface(k):
    tag, pk, decl, comp, rank, cat = k
    return f"""        !> Reads a {pk} file column into `col`, carrying its nulls across.
        module subroutine mat_{tag}(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_{tag}"""


def gen_dispatch():
    """The kind-dispatch entry point the read submodule calls, emitted INTO a submodule.

    It must not be a module-contained procedure: gfortran does not emit such a procedure when
    every caller lives in a submodule, which links cleanly right up until it doesn't (see
    parquet_columns_util.f90 for the same lesson).
    """
    o = []
    w = o.append
    w("""    module procedure table_materialize_kind
        select case (kind)""")
    for k in KINDS:
        tag, pk = k[0], k[1]
        w(f"        case ({pk})")
        w(f"            call mat_{tag}(reader, name, col, nrows, wdt, unit)")
    w("""        case default
            error stop EP // "internal: no materializer for this column kind"
        end select
    end procedure table_materialize_kind
    !""")
    return "\n".join(o)


# --------------------------------------------------------------------------------------
# src/parquet_tables_access.f90
# --------------------------------------------------------------------------------------
def gen_access():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind value access for `parquet_table`: the zero-copy pointer path (`%col`), the
!! widening copy-out (`%get`) and the copy-back (`%set`).
submodule (parquet_tables) parquet_tables_access
    implicit none
    !
contains
    !""")
    for k in PTR_KINDS:
        w(ptr_impl(k))
    for k in ARRAY_KINDS:
        w(get_impl(k))
    w(get_str_impl())
    for k in ARRAY_KINDS:
        w(set_impl(k))
    w(set_str_impl())
    w("end submodule parquet_tables_access")
    return "\n".join(o) + "\n"


def ptr_impl(k):
    tag, pk, decl, comp, rank, cat = k
    return f"""    module procedure col_ptr_{tag}
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= {pk}) then
            call table_context_suffix(self, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_{tag}
    !"""


def get_impl(k):
    tag, pk, decl, comp, rank, cat = k
    lines = [f"    module procedure get_arr_{tag}",
             "        integer :: idx",
             "        character(len=:), allocatable :: sfx, kname",
             f"        {decl}, pointer :: p{dims(rank)}"]
    for _, src in WIDEN.get(tag, []):
        srcdecl = next(kk[2] for kk in KINDS if kk[0] == src)
        lines.append(f"        {srcdecl}, pointer :: p_{src}{dims(rank)}")
    lines += ["        !",
              '        call table_resolve(self, name, "get", idx, found)',
              "        if (idx == 0) then",
              f"            allocate(arr{'(0)' if rank == 1 else '(0,0)'})",
              "            return",
              "        end if",
              "        select case (self%cache%cols(idx)%values%kindof())",
              f"        case ({pk})",
              "            call self%cache%cols(idx)%values%data_ptr(p)",
              f"            allocate(arr{'(size(p))' if rank == 1 else '(size(p,1), size(p,2))'})",
              "            arr = p"]
    for srcpk, src in WIDEN.get(tag, []):
        lines += [f"        case ({srcpk})",
                  f"            call self%cache%cols(idx)%values%data_ptr(p_{src})",
                  f"            allocate(arr{f'(size(p_{src}))' if rank == 1 else f'(size(p_{src},1), size(p_{src},2))'})",
                  f"            arr = p_{src}"]
    lines += ["        case default",
              "            call table_context_suffix(self, name, sfx)",
              "            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)",
              '            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx',
              "        end select",
              f"    end procedure get_arr_{tag}",
              "    !"]
    return "\n".join(lines)


def get_str_impl():
    return """    module procedure get_arr_str
        integer :: idx
        type(parquet_string_column), pointer :: src
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get")
        call self%cache%cols(idx)%values%string_column(src)
        arr = src%clone()
    end procedure get_arr_str
    !
    module procedure get_arr_chr
        integer :: idx, maxlen
        integer(int64) :: i, n
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING, "get")
        n = self%cache%cols(idx)%values%length()
        ! Two passes: the width must be the longest element present, and a fixed-length array
        ! cannot be grown per element. A null reads back as "" and so contributes length 0.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do i = 1, n
            call store%get(i, s, allow_null=.true.)
            if (len(s) > maxlen) maxlen = len(s)
        end do
        allocate(character(len=maxlen) :: arr(n))
        do i = 1, n
            call store%get(i, s, allow_null=.true.)
            arr(i) = s
        end do
    end procedure get_arr_chr
    !
    module procedure get_arr_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: i, n, flat
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0,0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING_VEC, "get")
        n = self%cache%cols(idx)%values%length()
        wdt = self%cache%cols(idx)%values%colwidth()
        ! A vector string column is ONE flat string store of width*nrows elements, element
        ! (e, i) living at (i-1)*width + e -- reaching it directly is what lets each element
        ! come back as an allocatable string, which the two-pass width measurement needs.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, s, allow_null=.true.)
                if (len(s) > maxlen) maxlen = len(s)
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, n))
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, s, allow_null=.true.)
                arr(e, i) = s
            end do
        end do
    end procedure get_arr_chrv
    !"""


def set_impl(k):
    tag, pk, decl, comp, rank, cat = k
    size_expr = "size(arr, kind=int64)" if rank == 1 else "size(arr, 2, kind=int64)"
    return f"""    module procedure set_arr_{tag}
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, {pk}, "set")
        call table_require_length(self, idx, {size_expr}, "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_{tag}
    !"""


def set_str_impl():
    return """    module procedure set_arr_chr
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chr
    !
    module procedure set_arr_chrv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_STRING_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chrv
    !"""


# --------------------------------------------------------------------------------------
# src/parquet_tables_addcol.f90
# --------------------------------------------------------------------------------------
def gen_addcol():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind `%add_column` for `parquet_table`: appends a new in-memory column, taking its
!! kind, width and row count from the values it is given.
submodule (parquet_tables) parquet_tables_addcol
    implicit none
    !
contains
    !""")
    for k in ARRAY_KINDS:
        w(add_impl(k))
    w(add_str_impl())
    w("end submodule parquet_tables_addcol")
    return "\n".join(o) + "\n"


def add_impl(k):
    tag, pk, decl, comp, rank, cat = k
    n_expr = "size(values, kind=int64)" if rank == 1 else "size(values, 2, kind=int64)"
    w_expr = "1_int32" if rank == 1 else "int(size(values, 1), int32)"
    return f"""    module procedure add_column_{tag}
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, {n_expr})
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init({pk}, {n_expr}, {w_expr}, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = {pk}
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_{tag}
    !"""


def add_str_impl():
    return """    module procedure add_column_chr
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_STRING, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_STRING
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_chr
    !
    module procedure add_column_chrv
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_STRING_VEC, size(values, 2, kind=int64), &
            int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_STRING_VEC
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_chrv
    !"""


# --------------------------------------------------------------------------------------
# src/parquet_tables_materialize.f90
# --------------------------------------------------------------------------------------
def gen_materialize():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind "read one file column into a `parquet_column`" for `parquet_table`.
!!
!! Every numeric/logical/string read passes `is_valid=`, which is what stops the reader from
!! aborting on a Null-containing column: the mask comes back here and is replayed onto the
!! column's own sparse validity. The temporal kinds take no mask -- their null state lives
!! inside each element, which the reader fills directly.
submodule (parquet_tables) parquet_tables_materialize
    implicit none
    !
contains
    !""")
    for k in KINDS:
        w(mat_impl(k))
    w(gen_dispatch())
    w("end submodule parquet_tables_materialize")
    return "\n".join(o) + "\n"


def mat_impl(k):
    tag, pk, decl, comp, rank, cat = k
    if cat == "str":
        return mat_str_impl(k)
    if cat == "tmp":
        # Temporal: no is_valid, nulls live in the elements.
        alloc = "tmp(nrows)" if rank == 1 else "tmp(wdt, nrows)"
        return f"""    module procedure mat_{tag}
        {decl}, allocatable :: tmp{dims(rank)}
        !
        allocate({alloc})
        call parquet_read_column(reader, name, tmp)
        call col%init({pk}, nrows, wdt, unit)
        call col%set_all(tmp)
    end procedure mat_{tag}
    !"""
    alloc = "tmp(nrows), valid(nrows)" if rank == 1 else "tmp(wdt, nrows), valid(wdt, nrows)"
    if rank == 1:
        null_loop = """        do i = 1, nrows
            if (.not. valid(i)) call col%set_null(i)
        end do"""
        decls = "        integer(int64) :: i"
    else:
        # parquet_column's validity is ROW-granular even for a vector kind: set_null(i) nulls
        # every element of row i, and the index is bounded by nrows, not nrows*width. Arrow
        # reports per-ELEMENT validity, so a row is marked null when ANY of its elements is --
        # per-element nulls inside a vector column cannot be represented and are widened to the
        # whole row. Passing a flat (i-1)*width+e index here instead aborts on the row-index
        # bounds check, which is exactly how this was found.
        null_loop = """        do i = 1, nrows
            if (.not. all(valid(:, i))) call col%set_null(i)
        end do"""
        decls = "        integer(int64) :: i"
    return f"""    module procedure mat_{tag}
        {decl}, allocatable :: tmp{dims(rank)}
        logical, allocatable :: valid{dims(rank)}
{decls}
        !
        allocate({alloc})
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init({pk}, nrows, wdt, unit)
        call col%set_all(tmp)
{null_loop}
    end procedure mat_{tag}
    !"""


def mat_str_impl(k):
    tag, pk, decl, comp, rank, cat = k
    if rank == 1:
        return """    module procedure mat_str
        type(parquet_string_column) :: tmp
        type(parquet_string_column), pointer :: dest
        !
        ! The compact offsets+data+validity path: nulls travel with the buffers, so unlike
        ! every other kind this one needs no separate validity replay.
        call parquet_read_column(reader, name, tmp)
        call col%init(PK_STRING, nrows, wdt, unit)
        call col%string_column(dest)
        dest = tmp%clone()
    end procedure mat_str
    !"""
    return """    module procedure mat_strv
        character(len=:), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        integer :: e, slen
        !
        ! There is no compact buffer path for a rank-2 string column, so this goes through the
        ! legacy fixed-width reader -- which means trailing blanks cannot be distinguished from
        ! padding and are trimmed. Documented as a known limitation of vector string columns.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        allocate(valid(wdt, nrows))
        call parquet_read_column(reader, name, tmp, is_valid=valid)
        call col%init(PK_STRING_VEC, nrows, wdt, unit)
        call col%set_all(tmp)
        do i = 1, nrows
            do e = 1, wdt
                if (.not. valid(e, i)) call col%set_null((i - 1) * int(wdt, int64) + int(e, int64))
            end do
        end do
    end procedure mat_strv
    !"""


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--check", action="store_true",
                    help="verify the committed output is current; do not write")
    args = ap.parse_args()

    root = _HERE.parent
    outputs = {
        root / "src" / "parquet_tables.f90": gen_spec(),
        root / "src" / "parquet_tables_access.f90": gen_access(),
        root / "src" / "parquet_tables_addcol.f90": gen_addcol(),
        root / "src" / "parquet_tables_materialize.f90": gen_materialize(),
    }

    # Guard the project's hard 132-column limit at generation time: a template edit that pushes
    # one emitted line over the limit would otherwise only surface as a CI style failure.
    bad = []
    for path, text in outputs.items():
        for n, line in enumerate(text.splitlines(), start=1):
            if len(line) > 132:
                bad.append(f"{path.name}:{n}: {len(line)} columns")
    if bad:
        print("generate_parquet_tables.py: emitted lines exceed the 132-column limit:",
              file=sys.stderr)
        for b in bad:
            print("  " + b, file=sys.stderr)
        return 1

    if args.check:
        drift = False
        for path, text in outputs.items():
            current = path.read_text() if path.exists() else ""
            if current != text:
                print(f"generate_parquet_tables.py: {path.name} is out of date", file=sys.stderr)
                drift = True
        if drift:
            print("Re-run tools/generate_parquet_tables.py and commit the result.", file=sys.stderr)
            return 1
        print("generate_parquet_tables.py: committed output is up to date")
        return 0

    for path, text in outputs.items():
        path.write_text(text)
        print(f"generate_parquet_tables.py: wrote {path.relative_to(root)} "
              f"({len(text.splitlines())} lines)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
