#!/usr/bin/env python3
"""Generate the per-kind blocks of the `parquet_tables` table layer.

`parquet_table` exposes the
same handful of operations over all 18 column kinds -- a zero-copy pointer accessor, a
widening copy-out, a copy-back, a from-scratch column add, and "read one file column into a
parquet_column". Written by hand that is several hundred near-identical procedures, each
needing its own `!>`/`!!` doc-comments, so they are emitted from the kind table instead.

The kind table itself is IMPORTED from tools/generate_parquet_columns.py rather than copied:
one list of kinds serves both generators, so adding a kind cannot leave the two layers
disagreeing about what exists.

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
  src/parquet_tables_predefined.f90   %bind_predefined, for generated table types

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


def mask_dims(rank):
    """Shape of an `is_valid=` mask accompanying values of this rank.

    The rule is one line and applies to every paired accessor: **the mask has the same shape as
    the values it describes.** A vector column's validity is per element, so a rank-2 `arr`/`p`
    takes a rank-2 mask -- there is deliberately no rank-1 form that would widen a row's worth of
    elements into one bit (CLAUDE.md, "Validity is per ELEMENT, and a vector row is not one bit").
    """
    return "(:)" if rank == 1 else "(:,:)"


def mask_comment(rank, text_row, text_elem):
    return text_row if rank == 1 else text_elem


def valid_out_comment(rank):
    """Trailing doc tag for an `intent(out)` validity mask."""
    return mask_comment(rank,
                        "!! present: per-row validity, .true. = value.",
                        "!! present: per-element validity, (width, nrows).")


def valid_in_comment(rank):
    """Trailing doc tag for an `intent(in)` validity mask."""
    return mask_comment(rank,
                        "!! present: rows marked .false. become null.",
                        "!! present: elements marked .false. become null.")


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
!! one reads the file's SCHEMA and nothing else; each column's values are read into the
!! type-erased `parquet_column` store (`parquet_columns`) the first time something asks for them,
!! and the Arrow-side buffers are freed as soon as that copy exists, so the table owns the sole
!! Fortran copy of each column it holds. From there a column is reached either by a zero-copy
!! typed pointer (`%col`, exact kind) or by a widening copy (`%get`), a table can be built from
!! scratch in memory (`parquet_new_table` + `%add_column`), and the whole thing is written back
!! out through an ordinary `parquet_schema` (`parquet_write_table`).
!!
!! Five things are worth knowing before using it:
!!
!! * **`%get` is the friendly path; `%col` is the fast one.** `%get` copies the column into an
!!   allocatable array of the caller's own kind, widening int32 -> int64 and float32 -> float64
!!   on the way, so a caller who just wants the numbers never has to ask what type the file used.
!!   `%col` hands back a pointer straight into the store -- zero copy, writable -- but the pointer
!!   kind must match the stored kind EXACTLY, so it is for code that already knows the type (or
!!   has asked `%kind`).
!! * **Reads happen on first touch.** `%nrows`/`%kind`/`%width`/`%column_names` answer from the
!!   schema and read nothing; a value access reads that column, whole, across the table's row
!!   scope. `%residency` reports what is held, `%prefetch`/`%materialize_all` read ahead of time,
!!   and `%reload` goes back to the file.
!! * **A table can cover part of a file.** `parquet_open_table(t, file, row_lo, row_hi)` reads
!!   only the row groups covering that range, which is how a file bigger than memory is worked
!!   through and how a parallel program gives each thread its own share.
!! * **Assignment is blocked.** The column store lives behind a pointer, so `b = a` would leave two
!!   tables sharing (and later double-freeing) one store. `b = a` is a hard error rather than a
!!   silent corruption; copying a table comes with `%clone` in a later milestone.
!! * **Concurrency is enforced, not merely documented.** Reading an already-resident column is
!!   free -- no lock, no atomic, any number of threads -- and %append into a shared table is
!!   serialised by the table's own lock, so a parallel producer needs no !$omp critical. Every
!!   other change to a table another thread may be using is a hard error naming what to do
!!   instead: a lazy first touch, any structural change, nulling a column whose validity storage
!!   does not exist yet, and any write to a string column (whose rows share one packed store). A
!!   table a thread opened ITSELF inside the region is thread-private and exempt from all of them.
!!   See parquet_tables_parallel.f90 and doc/pages/thread-safety.md.
!!
!! Depends on `parquet_columns` (the value store) and `parquet_core` (the reader/writer it drives).
module parquet_tables
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
#ifdef _OPENMP
    ! Only omp_lock_kind is needed at module scope, for parquet_table_cache's own lock component.
    ! The procedures that operate on it import omp_lib themselves, exactly as unsafe_first_touch
    ! and record_open_thread already do.
    use omp_lib, only : omp_lock_kind
#endif
    use parquet_columns
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use parquet_core, only : parquet_reader, parquet_writer, parquet_schema, parquet_column_type, &
        parquet_open_reader, parquet_close_reader, parquet_get_nrows, parquet_get_col_size, &
        parquet_get_column_names, parquet_get_column_type, parquet_column_exists, &
        parquet_release_column, parquet_read_column, parquet_get_metadata, parquet_get_string_length, &
        parquet_get_num_row_groups, parquet_get_chunk_size, parquet_read_column_chunk, &
        parquet_open_writer, parquet_write_column, parquet_close_writer, parquet_write_row_mask, &
        parquet_measure_list_width, parquet_column_width_needs_data, parquet_column_has_nulls, &
        parquet_load_qc_maml_file, parquet_filter, parquet_sortkey, parquet_read_qc, &
        parquet_compose_read_qc, parquet_reader_set_filter, parquet_parse_maml, &
        parquet_get_metadata_items, parquet_get_qc_columns, parquet_get_physical_row_indices, &
        parquet_get_column_time_info, parquet_size_auto, &
        parquet_unit_millis, parquet_unit_micros, parquet_unit_nanos
    ! The table layer's two solicited printers (%print_stat) and its own warnings go through the
    ! library's output channels rather than printing directly, so verbosity/message_stream apply
    ! here as everywhere -- see tools/check_source_conventions.py's `no direct printing` check.
    use parquet_settings, only : parquet_emit_warning, parquet_output_is_suppressed
    !
    implicit none
    private
    !
    public :: parquet_table
    public :: parquet_table_row
    public :: parquet_slice
    public :: parquet_slice_range
    public :: parquet_slice_list
    public :: parquet_open_table
    public :: parquet_new_table
    public :: parquet_write_table
    public :: parquet_table_row_group_bounds
    public :: PARQUET_ROW_INDEX
    public :: REGIME_FULL, REGIME_SLICE
    public :: RES_EMPTY, RES_PARTIAL, RES_FULL
    !> TEST-ONLY debug hook; deliberately NOT in README.md's API overview. See its own
    !! doc-comment for why it has to be public at all.
    public :: parquet_debug_table_set_inflight
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
    integer, parameter :: REGIME_SLICE = 1 !! the table covers one contiguous row range of the file.
    !
    !> Opens a file-backed table, over the whole file or over one contiguous row slice.
    !!
    !! Given `row_lo`/`row_hi` (1-based, inclusive, in either integer kind) the table covers only
    !! those rows, and reads only the row groups covering them -- this is how a file bigger than
    !! memory is worked through, and how a parallel program gives each thread its own share
    !! (`parquet_table_row_group_bounds` reports where the natural boundaries are). Row indices
    !! everywhere else, `%row(i)` included, are then relative to the slice, not to the file.
    !!
    !! A slice accepts the same read-time transform the whole-file form does, with ONE exception:
    !! there is no `sort` argument on the slice forms at all. A sort reorders rows across the whole
    !! file, so "the 1000th row" would no longer name anything a slice could be cut along -- and
    !! omitting the argument makes that a compile error rather than a runtime one. (A `maml=` whose
    !! `extra: sort:` list is non-empty is the same rejection, necessarily at runtime.)
    !!
    !! **A filtered or sampled slice does not have `row_hi - row_lo + 1` rows.** `%nrows()` is the
    !! number of rows of `[row_lo, row_hi]` that survive the transform, and every row index the
    !! table takes or reports counts those survivors -- row 1 is the first surviving row, not file
    !! row `row_lo`. Without a filter and without `sample_fraction=` nothing changes: the slice is
    !! trimmed out of the covering row groups in memory, exactly as it always was.
    interface parquet_open_table
        module procedure open_table_full
        module procedure open_table_slice_i32
        module procedure open_table_slice_i64
    end interface parquet_open_table
    !
    !> The name of the automatic column holding each row's PHYSICAL row number in the source
    !! parquet file. Declared as a constant so a program can name it without hard-coding the
    !! string, and so a collision test has something to compare against.
    character(len=*), parameter :: PARQUET_ROW_INDEX = "parquet_row_index"
    !
    ! ---- Column residency (D14/RF20) ----
    integer, parameter :: RES_EMPTY = 0   !! no values held (never read, or an unsupported type).
    integer, parameter :: RES_PARTIAL = 1 !! reserved: some row groups resident (a later milestone).
    integer, parameter :: RES_FULL = 2    !! the whole column, across the table's row scope, is held.
    !
    !> One column slot: its identity, its shape, its provenance, and the values themselves.
    !!
    !! `declared_kind`, `width` and `supported` are settled at OPEN time, from the file schema
    !! alone -- they must be answerable before any data is read, because `%kind` is how a caller
    !! decides which `%col` specific to call in the first place. `values` stays empty until the
    !! column is first touched.
    type :: parquet_table_column
        character(len=:), allocatable :: name      !! internal/logical name -- ALWAYS the lookup key.
        character(len=:), allocatable :: file_name !! physical name in the file (== name for now).
        integer :: declared_kind = PK_NONE         !! PK_* this slot holds, or PK_NONE if unsupported.
        integer :: width = 1                       !! values per row: 1 scalar, col_size for a *_VEC.
        !> .true. while this column's kind and width are still UNKNOWN, which happens for exactly
        !! one column type: a plain LIST/LARGE_LIST from a foreign writer, whose per-row width is
        !! a property of the data rather than the schema. While set, `declared_kind` is PK_NONE and
        !! `width` is 0, and anything that needs either must call table_resolve_width first.
        !! Deferring this is what keeps parquet_open_table schema-only -- measuring at open meant
        !! decoding every such column just to classify it.
        logical :: width_pending = .false.
        !> .true. while a `%cast` on a column nothing has read yet is still waiting for the read
        !! that will carry it out. `%cast` on a non-resident file-backed column only rewrites
        !! `declared_kind` and sets this, so the first touch decodes STRAIGHT into the target
        !! kind -- one pass instead of read-then-convert, using the reader's own numeric
        !! conversions. Cleared by `table_materialize`, and by anything that gives the slot values
        !! some other way. A second `%cast` while it is set materializes first, so that a chain of
        !! casts cannot silently forget the intermediate one (see `table_cast`).
        logical :: cast_pending = .false.
        !> The TIME/TIMESTAMP resolution this column is stored at in the file, as a parquet_unit_*
        !! selector, or 0 for every other kind (and for a column with no file behind it). Recorded
        !! at classification time because it is recoverable from nowhere else afterwards: a
        !! `parquet_timestamp` holds seconds+nanoseconds and carries no unit of its own, so once
        !! the reader is gone the table would have no way to know it read a `timestamp[ns]` column
        !! rather than a `timestamp[us]` one. A schema-less write needs it to declare the matching
        !! data_type token -- without it the writer defaults to microseconds and a nanosecond
        !! column fails the write outright (`to_unix` aborts rather than truncating).
        integer :: time_unit = 0
        !> .true. when this column is a TIMESTAMP stored with a timezone (the MAML `,utc` token).
        !! Recorded and used for the same reason as `time_unit`.
        logical :: time_utc = .false.
        !> Unit declared for this column by the read-in MAML (`fields:`' `unit:` key), looked up
        !! by the column's FILE name at open. Unallocated when no MAML was given, or when it
        !! declares nothing for this column. It is held here rather than only on `values` because
        !! a lazy column has no values yet, and %unit must answer for a column nothing has read;
        !! table_materialize copies it onto the values as they arrive, so everything downstream
        !! (%append's unit check, %clone, the writer) sees one unit rather than two.
        character(len=:), allocatable :: unit
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
    !!
    !! The source file's identity lives here rather than on `parquet_table` for the same reason
    !! the reader does: a first touch, and the error message it may raise, must be reachable from
    !! the cache alone, because that is all a `parquet_table_row` handle holds.
    type :: parquet_table_cache
        type(parquet_table_column), allocatable :: cols(:) !! descriptor slots; `ncols` are live.
        integer :: ncols = 0                               !! live slot count (cols may be longer).
        type(parquet_reader), allocatable :: reader        !! present iff the table is file-backed.
        logical :: reads_started = .false.                 !! .true. once any column has been read.
        !> Bumped by every structural change, so a caller can tell whether a pointer it holds may
        !! have been invalidated (%generation). On the CACHE rather than on `parquet_table`, like
        !! all other table state -- see this type's own note above.
        integer(int64) :: generation = 0_int64
        logical :: file_backed = .false.                   !! .true. if opened from a parquet file.
        character(len=:), allocatable :: source_file       !! the file this table was opened from.
        ! --- the source file's key/value metadata, copied ONCE at open.
        !
        !     Snapshotted rather than read through `reader` on demand, because `reader` does not
        !     survive a row mutation (table_detach releases it) and a file's metadata is a
        !     property of where the rows came from, which detaching does not change. Both arrays
        !     are allocated for every file-backed open, zero-size when the file carries no
        !     metadata at all -- so `allocated(meta_keys)` answers "did this table come from a
        !     file", which is the question %get_file_metadata has to ask once file_backed can no
        !     longer be trusted to mean it.
        character(len=:), allocatable :: meta_keys(:)      !! metadata keys, blank-padded; file order.
        character(len=:), allocatable :: meta_values(:)    !! the matching values, same order.
        ! --- row-group geometry, slice regime only. TWO coordinate systems, and which one a given
        !     array is in is the whole reason there are two of them:
        !
        !     `rg_bounds` is always in the coordinates `row_lo`/`row_hi`/`%row(i)` use, i.e. the
        !     TABLE's own row numbering, because that is what materialize_slice and
        !     resolve_width_row_groups compare their scope against. On an unfiltered slice those
        !     ARE the file's rows, so it holds physical bounds and `rg_bounds_physical` stays
        !     unallocated. On a filtered or sampled slice the table counts survivors instead, so
        !     `rg_bounds` holds each row group's surviving rows and the file's own numbering is
        !     kept separately -- captured before the mask is attached, since afterwards the reader
        !     reports survivors and the physical numbering is simply no longer askable.
        integer(int64), allocatable :: rg_bounds(:,:)      !! (2, nrg) row-group ranges, TABLE rows.
        integer(int64), allocatable :: rg_bounds_physical(:,:) !! the same, FILE rows; masked slice only.
        ! --- physical row geometry, captured at open so both survive a detach. `%nrows()` counts
        !     what the table HOLDS; these two count what it came from, which no other query can
        !     answer once a filter is active (the reader then reports survivors) or once the
        !     table has cut its file loose.
        !> .true. once the automatic parquet_row_index column has been asked for and given a real
        !! slot. Until then the column is VIRTUAL: %has_column answers for it, but it occupies no
        !! slot, is not listed by %column_names or counted by %ncols, and costs nothing -- 8 bytes
        !! a row is 8 GB at a billion rows, so materializing it at open would undo the laziness
        !! the whole type is built on.
        logical :: row_index_live = .false.
        !> .true. when the source file has its own column called parquet_row_index that a read-in
        !! MAML did NOT remap. That column is unreachable (the reserved name resolves to the
        !! automatic one), which parquet_open_table warns about at open.
        logical :: row_index_shadowed = .false.
        integer(int64) :: unfiltered_rows = 0              !! rows before filter/sample: the slice's length, or the file's.
        integer(int64) :: rg_extent_rows = 0               !! summed length of the row groups this table covers.
        integer(int64) :: slice_row_lo = 0                 !! slice's first FILE row (0 = not a slice).
        integer(int64) :: slice_row_hi = 0                 !! slice's last FILE row (0 = not a slice).
        logical :: opened_in_parallel = .false.            !! .true. if opened inside a parallel region.
        integer :: owner_thread = -1                       !! OpenMP thread that opened it (-1 if serial).
        ! --- the table's own lock, and the two counters the read/append guards read ---
        !     The lock serialises %append into a SHARED table, so a parallel producer region needs
        !     no !$omp critical of its own and cannot wrap the wrong statement. It is an OpenMP
        !     SIMPLE lock, which is not recursive: `table_append_table` is the only procedure that
        !     may take it, and every internal caller goes to `append_table_worker` instead (see its
        !     own doc-comment). Taking it twice on one thread deadlocks rather than failing to
        !     build.
        !
        !     A lock is a HANDLE, not a value: %clone must initialise a fresh one rather than copy
        !     the source's, and `table_finalize` must destroy it exactly once.
#ifdef _OPENMP
        integer(omp_lock_kind) :: lock                     !! serialises %append on a shared table.
#endif
        logical :: lock_ready = .false.                    !! .true. between omp_init_lock and omp_destroy_lock.
        !> Nonzero while some thread is inside %append. Every table READ entry point checks it and
        !! aborts, because the parallel append region is append-only: a reader inside a value array
        !! while the appender reallocates it is reading freed memory. Read/written with
        !! `!$omp atomic`, and deliberately an integer rather than a logical so the atomic update
        !! is an increment (nested/overlapping appends stay correct without a second flag).
        integer :: append_active = 0
        !> Table read entry points currently in flight. %append checks it under the lock and
        !! aborts rather than reallocating storage another thread is reading. Only the COARSE
        !! entry points maintain it (%get, %col, %get_slice, %row, %get_valid_mask, %prefetch,
        !! %materialize_all) -- the per-element accessors deliberately do not, since two atomics
        !! per cell would dominate a %get_element loop over a large column, and they still take
        !! the cheap `append_active` check above. Do not "fix" that asymmetry.
        integer :: readers_active = 0
        ! --- read-time transform, composed ONCE at parquet_open_table time and retained only so
        !     %clone can reattach the same one when it reopens the file. Already translated to
        !     FILE names and already merged with whatever the read-in MAML declared, so nothing
        !     downstream has to redo either step. Each stays unallocated when nothing was supplied,
        !     which is how a clone tells "nothing to reattach" from "an empty filter was composed".
        !
        !     THESE BELONG HERE, NOT ON parquet_table ITSELF, and that is not a filing decision:
        !     `parquet_table` is deliberately five scalars and one pointer, with NO allocatable
        !     components at all, so its intent(out) entry and its FINAL do no recursive walk over
        !     nested derived types. Hanging a `type(parquet_schema), allocatable` off it (maml +
        !     cinfo + metadata, each holding allocatable arrays of derived types with their own
        !     allocatable components) makes every `parquet_open_table` entry perform exactly such a
        !     walk -- and this project already has two confirmed compiler bugs in that machinery on
        !     this very type (an OpenMP private() copy left uninitialized, and %detached surviving
        !     an intent(out) reset; see parquet_tables_lifecycle.f90 and CLAUDE.md). Doing it
        !     anyway segfaulted ifx inside the RTL's own recursive descriptor walker, on a
        !     block-local table opened inside a parallel region. Keep new transform state here.
        type(parquet_filter), allocatable :: read_filter    !! composed row filter, file names.
        type(parquet_sortkey), allocatable :: read_sort     !! composed sort keys, file names.
        type(parquet_schema), allocatable :: read_qc_schema !! merged qc schema, file names.
        logical :: read_qc_soft = .false.                   !! qc_soft as given at open.
        real(real64), allocatable :: read_sample_fraction   !! sample_fraction as given at open.
        integer(int32), allocatable :: read_sample_seed     !! sample_seed as given at open.
    end type parquet_table_cache
    !
    !> Which rows to pick out of a column: `1:`, `1:10`, `1:10:2` or an explicit list.
    !!
    !! Fortran cannot overload `t%col('x')(1:10:2)` on an arbitrary column expression, so the
    !! selection has to be an object the copy path can be handed. Build one with
    !! `parquet_slice_range` or `parquet_slice_list` and pass it to `%get_slice`.
    !!
    !! The rows it selects are rows of THIS TABLE -- in the slice regime, index 1 is the table's
    !! first row, not the file's. That is a different thing from the table-level slice regime,
    !! which decides how much of the file the table covers in the first place.
    type :: parquet_slice
        private
        logical :: strided = .true.       !! .true. for start:stop:step, .false. for a list.
        integer(int64) :: start = 1       !! first row (strided form).
        integer(int64) :: stop = -1       !! last row, or -1 meaning "to the end" (strided form).
        integer(int64) :: step = 1        !! stride, may be negative, never 0 (strided form).
        integer(int64), allocatable :: indices(:) !! explicit row list (list form).
    end type parquet_slice
    !
    !> Builds a `start:stop:step` slice. `stop` defaults to the table's last row (resolved when
    !! the slice is USED, not when it is built, so one slice object can outlive a row count),
    !! `step` to 1. A negative step counts down; a zero step is an error.
    interface parquet_slice_range
        module procedure slice_range_i32
        module procedure slice_range_i64
    end interface parquet_slice_range
    !
    !> Builds a slice from an explicit list of 1-based row indices, in the order given --
    !! repeats and non-monotone order are both allowed, since this is a gather, not a range.
    interface parquet_slice_list
        module procedure slice_list_i32
        module procedure slice_list_i64
    end interface parquet_slice_list
    !
    !> The table-level state a first touch needs, grouped so it can be passed as one argument:
    !! which rows the table covers, and whether it still has a file to read them from.
    !!
    !! It exists because a first touch has two callers with nothing else in common -- the table
    !! itself, and a `parquet_table_row` handle, which by design holds only the cache pointer
    !! plus by-value copies of exactly these scalars (RF3). Grouping them keeps that contract in
    !! one place instead of five arguments repeated down the call chain.
    type :: table_scope
        integer :: regime = REGIME_FULL   !! REGIME_FULL or REGIME_SLICE.
        integer(int64) :: row_lo = 1      !! first file row this table covers.
        integer(int64) :: row_hi = -1     !! last file row this table covers.
        integer(int64) :: nrows = 0       !! rows the table has (row_hi - row_lo + 1).
        logical :: detached = .false.     !! .true. once a row-structural mutation cut the file loose.
    end type table_scope
    !""")
    w(gen_table_type())
    w("    !")
    w(gen_row_type())
    w("    !")
    w(gen_spec_interfaces())
    w("    !")
    w("end module parquet_tables ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


def gen_table_type():
    o = []
    w = o.append
    w("""    !> A whole table: a column store plus the row scope and provenance describing it.
    !! Declared by the caller (`type(parquet_table) :: t`), filled by `parquet_open_table` or
    !! `parquet_new_table`, and freed automatically when it goes out of scope.
    type :: parquet_table
        private
        logical :: detached = .false.               !! reserved: set by a row-structural mutation.
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
        procedure :: has_nulls => table_has_nulls    !! Whether a column holds (or may hold) nulls.
        procedure, private :: table_get_valid_mask     !! %get_valid_mask specific, per-row mask.
        procedure, private :: table_get_valid_mask_elem !! %get_valid_mask specific, per-element mask.
        !> Copy out a column's validity as a plain `logical` array. A rank-1 `mask` gives one entry
        !! per row (on a *_VEC column: "any element of the row is null"); a rank-2 `mask` gives the
        !! true `(width, nrows)` per-element state.
        generic :: get_valid_mask => table_get_valid_mask, table_get_valid_mask_elem
        procedure :: generation => table_generation  !! Counter bumped by every structural change.
        procedure :: has_column => table_has_column  !! Whether a column of this name exists.
        procedure :: kind => table_column_kind       !! A column's PK_* kind discriminator.
        procedure :: width => table_column_width     !! A column's values-per-row (1 if scalar).
        procedure :: unit => table_column_unit       !! Copy out a column's unit string.
        procedure :: residency => table_column_residency !! A column's RES_* residency state.
        procedure, private :: is_null_i32 => table_is_null_i32 !! %is_null specific, int32 row index.
        procedure, private :: is_null_i64 => table_is_null_i64 !! %is_null specific, int64 row index.
        procedure, private :: is_null_e32 => table_is_null_e32 !! %is_null specific, int32 row + element.
        procedure, private :: is_null_e64 => table_is_null_e64 !! %is_null specific, int64 row + element.
        !> Whether row `i` of a column is null, or -- given `e` as well -- element `e` of it.
        !!
        !! On a *_VEC column the row form answers "ANY element of the row is null"; the element
        !! form answers about that one element. Defined on a scalar column too, where `e` can only
        !! be 1 and the two agree.
        generic :: is_null => is_null_i32, is_null_i64, is_null_e32, is_null_e64
        procedure :: is_detached => table_is_detached !! Whether the table has left its file behind.
        procedure :: is_supported => table_is_supported !! Whether a column's type can be read.
        procedure :: filename => table_filename      !! Copy out the file this table came from.
        procedure :: get_file_metadata => table_get_file_metadata !! One key from the file's metadata.
        ! --- residency control ---
        procedure, private :: prefetch_one  !! %prefetch specific taking one column name.
        procedure, private :: prefetch_many !! %prefetch specific taking an array of names.
        !> Reads the named column(s) now, instead of on first touch. Required before a parallel
        !! region: a first touch inside one is a hard error, since it would mutate shared state.
        generic :: prefetch => prefetch_one, prefetch_many
        procedure :: materialize_all => table_materialize_every !! Read every column not yet read.
        procedure :: reload => table_reload           !! Re-read one column, discarding local edits.
        procedure :: evict_column => table_evict_column !! Drop a column's VALUES, keeping the slot.
        procedure :: validate_qc => table_validate_qc !! Check every qc-declaring column, holding none.
        procedure :: print_stat => table_print_stat  !! Print what the table holds, to stdout.
        procedure :: nrows_unfiltered => table_nrows_unfiltered !! Rows before filter=/sample_fraction=.
        procedure :: row_group_extent => table_row_group_extent !! Rows in the row groups this table covers.
        procedure :: row_group_bounds => table_row_group_bounds !! Row-group row ranges, this table's rows or the file's.
        ! --- row view ---
        procedure, private :: row_at_i32 !! %row specific taking an int32 index.
        procedure, private :: row_at_i64 !! %row specific taking an int64 index.
        !> A handle on one row, for code that works a row at a time rather than a column at a
        !! time. The index is 1-based within THIS table -- in the slice regime, row 1 is the
        !! slice's first row, not the file's.
        generic :: row => row_at_i32, row_at_i64""")
    # get_slice
    w("        ! --- copy out a row selection ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: get_slice_{tag} !! %get_slice specific for the {tag} kind.")
    w("        procedure, private :: get_slice_str  !! %get_slice specific returning a parquet_string_column.")
    w("        procedure, private :: get_slice_chr  !! %get_slice specific returning a character array.")
    w("        procedure, private :: get_slice_chrv !! %get_slice specific returning a character (elem, row) array.")
    w("        !> Copies the rows a `parquet_slice` selects into a freshly allocated array of")
    w("        !! the caller's own kind, widening on the way exactly as %get does.")
    w("        generic :: get_slice => " + wrap_list(
        [f"get_slice_{k[0]}" for k in ARRAY_KINDS] + ["get_slice_str", "get_slice_chr", "get_slice_chrv"], 12,
        first_prefix=len("        generic :: get_slice => ")))
    w("        ! --- write a row selection back ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: set_slice_{tag} !! %set_slice specific for the {tag} kind.")
    w("        procedure, private :: set_slice_chr  !! %set_slice specific taking a character array.")
    w("        procedure, private :: set_slice_chrv !! %set_slice specific taking a character (elem, row) array.")
    w("        !> Writes values into the rows a `parquet_slice` selects -- %get_slice's counterpart.")
    w("        !! The kind must match the column's exactly, and the array must have one value per")
    w("        !! selected row.")
    w("        generic :: set_slice => " + wrap_list(
        [f"set_slice_{k[0]}" for k in ARRAY_KINDS] + ["set_slice_chr", "set_slice_chrv"], 12,
        first_prefix=len("        generic :: set_slice => ")))
    # pointer accessors
    w("        ! --- zero-copy pointer access (exact kind) ---")
    for k in PTR_KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_ptr_{tag} !! %col specific for the {tag} kind.")
    w("        procedure, private :: col_ptr_strcol !! %col specific aliasing the compact string store.")
    w("        !> Points `p` at a column's storage: zero copy, writable, and the pointer kind must")
    w("        !! match the stored kind exactly (ask %kind first if you do not know it). A")
    w("        !! `parquet_string_column` pointer aliases a PK_STRING column's packed store: read")
    w("        !! it and edit its values in place, but do NOT change its length or element count")
    w("        !! through the pointer -- the column's own row count would no longer describe it.")
    w("        !!")
    w("        !! For the same reason, do NOT REORDER a column through this pointer -- in particular")
    w("        !! `call pf_permute(p, perm)`, which compiles and runs happily. It reorders that one")
    w("        !! column and leaves every other column where it was, silently breaking the row")
    w("        !! correspondence, and nothing detects it: the row count is unchanged and every later")
    w("        !! read returns values that are individually valid and jointly wrong. Use `%sort_by`,")
    w("        !! which reorders every column together.")
    w("        generic :: col => " + wrap_list([f"col_ptr_{k[0]}" for k in PTR_KINDS] + ["col_ptr_strcol"], 12,
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
    w("        procedure, private :: set_arr_strcol !! %set specific taking a parquet_string_column.")
    w("        generic :: set => " + wrap_list(
        [f"set_arr_{k[0]}" for k in ARRAY_KINDS] + ["set_arr_chr", "set_arr_chrv", "set_arr_strcol"], 12,
        first_prefix=len("        generic :: set => ")))
    # add_column
    w("        ! --- from-scratch construction ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: add_column_{tag} !! %add_column specific for the {tag} kind.")
    w("        procedure, private :: add_column_chr  !! %add_column specific taking a character array.")
    w("        procedure, private :: add_column_chrv !! %add_column specific taking a character (elem, row) array.")
    w("        !> Appends a new column, taking its values (and so its kind, width and row count).")
    w("        procedure, private :: add_column_strcol !! %add_column specific taking a parquet_string_column.")
    w("        generic :: add_column => " + wrap_list(
        [f"add_column_{k[0]}" for k in ARRAY_KINDS]
        + ["add_column_chr", "add_column_chrv", "add_column_strcol"], 12,
        first_prefix=len("        generic :: add_column => ")))
    # set_element + validity
    w("        ! --- mutation: one cell at a time (never changes the row set) ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        for ik in ("i32", "i64"):
            w(f"        procedure, private :: set_element_{tag}_{ik} "
              f"!! %set_element specific, {tag} kind, {ik} row index.")
    for tag in ("chr", "chrv"):
        for ik in ("i32", "i64"):
            w(f"        procedure, private :: set_element_{tag}_{ik} "
              f"!! %set_element specific, character {tag} form, {ik} row index.")
    for k in ARRAY_KINDS:
        tag = k[0]
        for ik in ("i32", "i64"):
            w(f"        procedure, private :: get_element_{tag}_{ik} "
              f"!! %get_element specific, {tag} kind, {ik} row index.")
    for t in ("chr", "chrv"):
        for ik in ("i32", "i64"):
            w(f"        procedure, private :: get_element_{t}_{ik} "
              f"!! %get_element specific, character {t} form, {ik} row index.")
    w("        !> Reads one row's value out of a column, widening into the caller's variable")
    w("        !! exactly as %get does -- the one-call form of `r = t%row(i)` then `r%get(name, v)`.")
    w("        !! On a *_VEC column the value is that row's whole vector.")
    w("        generic :: get_element => " + wrap_list(
        [f"get_element_{k[0]}_{ik}" for k in ARRAY_KINDS for ik in ("i32", "i64")]
        + [f"get_element_{t}_{ik}" for t in ("chr", "chrv") for ik in ("i32", "i64")], 12,
        first_prefix=len("        generic :: get_element => ")))
    w("        !> Writes one row's value in place. The kind must match the column's exactly (as")
    w("        !! %set does), and writing a value CLEARS that row's null -- use %set_null to put")
    w("        !! one back. On a *_VEC column the value is that row's whole vector.")
    w("        generic :: set_element => " + wrap_list(
        [f"set_element_{k[0]}_{ik}" for k in ARRAY_KINDS for ik in ("i32", "i64")]
        + [f"set_element_{t}_{ik}" for t in ("chr", "chrv") for ik in ("i32", "i64")], 12,
        first_prefix=len("        generic :: set_element => ")))
    w("""        procedure, private :: set_null_i32   !! %set_null specific taking an int32 row index.
        procedure, private :: set_null_i64   !! %set_null specific taking an int64 row index.
        procedure, private :: set_null_e32   !! %set_null specific taking an int32 row + element.
        procedure, private :: set_null_e64   !! %set_null specific taking an int64 row + element.
        procedure, private :: set_null_mask  !! %set_null specific taking a per-row mask.
        procedure, private :: set_null_mask_elem !! %set_null specific taking a per-element mask.
        !> Marks null: row `i` of a column, element `e` of row `i`, or every entry a `logical`
        !! mask marks `.false.`.
        !!
        !! The row form is whole-row even on a *_VEC column -- naming only a row says the row is
        !! missing. Name `e` to null one element. The mask form takes either shape: one entry per
        !! row (whole rows), or a `(width, nrows)` mask (individual elements).
        generic :: set_null => set_null_i32, set_null_i64, set_null_e32, set_null_e64, &
            set_null_mask, set_null_mask_elem
        procedure, private :: clear_null_i32 !! %clear_null specific taking an int32 row index.
        procedure, private :: clear_null_i64 !! %clear_null specific taking an int64 row index.
        procedure, private :: clear_null_e32 !! %clear_null specific taking an int32 row + element.
        procedure, private :: clear_null_e64 !! %clear_null specific taking an int64 row + element.
        !> Marks row `i` -- or, given `e`, element `e` of it -- valid without saying what its value
        !! is. Only useful when a value is already there or is about to be written; %set_element
        !! clears the null itself.
        generic :: clear_null => clear_null_i32, clear_null_i64, clear_null_e32, clear_null_e64
        procedure :: compact_validity => table_compact_validity !! Drop a null bitmap that no longer has nulls.
        procedure :: ensure_validity => table_ensure_validity !! Allocate validity storage up front, for concurrent nulling.
        ! --- mutation: whole columns (never changes the row set) ---
        procedure :: drop_column => table_drop_column     !! Remove a column; force= for a predefined one.
        procedure :: rename_column => table_rename_column !! Change the name a column is looked up by.
        procedure :: copy_column => table_copy_column     !! Add a copy of a column, optionally of another kind.
        procedure :: cast => table_cast                   !! Convert a column to another kind, in place.
        ! --- mutation: the row set itself -- every one of these DETACHES the table ---
        procedure :: filter_rows => table_filter_rows !! Keep only the rows a mask selects.
        procedure :: sort_by => table_sort_by         !! Reorder rows by one or more key columns.
        procedure :: top_n => table_top_n             !! Keep only the n best rows, in key order.
        ! --- the ORDER, without applying it: read-only, and they do NOT detach ---
        procedure, private :: table_argsort_by_i32    !! %argsort_by specific, int32 permutation.
        procedure, private :: table_argsort_by_i64    !! %argsort_by specific, int64 permutation.
        !> The row order the keys imply, without reordering anything. Unlike %sort_by the table
        !! stays attached, so this is how to read rows in an order while keeping the file.
        generic :: argsort_by => table_argsort_by_i32, table_argsort_by_i64
        procedure, private :: table_argsort_partial_i32 !! %argsort_partial specific, int32 perm.
        procedure, private :: table_argsort_partial_i64 !! %argsort_partial specific, int64 perm.
        !> The `n` best rows in order, by selection rather than a full sort. Also non-mutating.
        generic :: argsort_partial => table_argsort_partial_i32, table_argsort_partial_i64
        procedure :: is_sorted_by => table_is_sorted_by !! Whether the rows are already in that order.
        procedure, private :: table_delete_rows_i32   !! %delete_rows specific, int32 indices.
        procedure, private :: table_delete_rows_i64   !! %delete_rows specific, int64 indices.
        !> Removes the listed rows. A thin convenience over %filter_rows, and like it, detaching.
        generic :: delete_rows => table_delete_rows_i32, table_delete_rows_i64
        procedure, private :: table_truncate_i32      !! %truncate specific, int32 count.
        procedure, private :: table_truncate_i64      !! %truncate specific, int64 count.
        !> Keeps only the first n rows. Detaching, like every row-structural change.
        generic :: truncate => table_truncate_i32, table_truncate_i64
        procedure, private :: table_append_table      !! %append specific taking another table.
        procedure, private :: table_append_row        !! %append specific taking one row handle.
        !> Appends rows: a whole table's worth, or one row. Detaching, like every row-structural
        !! change. The bulk idiom is %clone_structure -> fill -> %append(batch).
        generic :: append => table_append_table, table_append_row
        procedure, private :: table_append_null_rows_i32 !! %append_null_rows specific, int32 count.
        procedure, private :: table_append_null_rows_i64 !! %append_null_rows specific, int64 count.
        !> Appends n all-null rows, to be filled in afterwards. Detaching.
        generic :: append_null_rows => table_append_null_rows_i32, table_append_null_rows_i64
        ! --- copying ---
        procedure :: clone => table_clone                     !! Independent deep copy of this table.
        procedure :: clone_structure => table_clone_structure !! Empty table with the same columns.
        procedure :: clone_extra => table_clone_extra !! Hook: copies an EXTENDING type's own components.
        ! --- generated table types (see doc/pages/generated-tables.md) ---
        procedure :: bind_predefined => table_bind_predefined !! Binds a generated type's predefined columns.""")
    w("""        ! --- lifecycle ---
        !> Blocks intrinsic assignment: the store lives behind a pointer, so a default `b = a`
        !! would leave two tables sharing one store and double-freeing it.
        generic :: assignment(=) => table_assign_guard
        procedure, private :: table_assign_guard !! The blocking defined assignment.
        final :: table_finalize                  !! Frees the store; never fails, never validates.
    end type parquet_table""")
    return "\n".join(o)


def gen_row_type():
    o = []
    w = o.append
    w("""    !> One row of a table, as a lightweight handle: `r = t%row(i)`.
    !!
    !! Non-owning and cheap to make, so it is the natural thing to pass to a procedure that
    !! works on a single row, or to build inside a loop over rows. It resolves the column by
    !! name and the row by index on EVERY access, so it survives anything that merely reallocates
    !! a column's values -- and it triggers the same lazy first touch that `%get` on the table
    !! does, so a handle can reach a column nothing has read yet.
    !!
    !! It points at the table's column STORE, not at the table, which is what lets `t%row(i)`
    !! return a usable handle without the caller declaring the table `target` (a pointer to a
    !! dummy's target would be undefined the moment the function returned).
    !!
    !! Invalidated by anything that changes the row set, by dropping a column it reads, and by
    !! the table going out of scope. None of those is detectable from the handle, so treat it as
    !! short-lived: make it, use it, let it go.
    type :: parquet_table_row
        private
        type(parquet_table_cache), pointer :: cache => null() !! the table's column store.
        integer(int64) :: irow = 0                            !! this row's 1-based index.
        type(table_scope) :: scope                            !! the table's row scope, by value.
    contains""")
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_get_{tag} !! %get specific for the {tag} kind.")
    w("        !> Copies this row's value for a column into the caller's own variable, widening")
    w("        !! int32 -> int64 and float32 -> float64 exactly as the table's own %get does.")
    w("        generic :: get => " + wrap_list([f"row_get_{k[0]}" for k in KINDS], 12,
                                               first_prefix=len("        generic :: get => ")))
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_set_{tag} !! %set specific for the {tag} kind.")
    w("        !> Writes this row's value for a column. The kind must match the column's exactly")
    w("        !! (a write never widens), and writing a value CLEARS that row's null. The TABLE is")
    w("        !! updated -- a handle is a view of it, not a copy.")
    w("        generic :: set => " + wrap_list([f"row_set_{k[0]}" for k in KINDS], 12,
                                               first_prefix=len("        generic :: set => ")))
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_ref_{tag} !! %ref specific for the {tag} kind.")
    w("        !> Points `p` at this row's storage: zero copy, writable, exact kind. A scalar")
    w("        !! column gives a scalar pointer, a vector column a pointer to that row's whole")
    w("        !! vector. The two string kinds have no %ref -- a packed variable-length store has")
    w("        !! no fixed slot to point at -- and the pointer dies with any structural change,")
    w("        !! exactly as the table's own %col pointers do.")
    w("        generic :: ref => " + wrap_list([f"row_ref_{k[0]}" for k in ARRAY_KINDS], 12,
                                               first_prefix=len("        generic :: ref => ")))
    w("""        procedure, private :: row_is_null      !! %is_null specific asking about the whole row.
        procedure, private :: row_is_null_elem !! %is_null specific asking about one element.
        !> Whether this row is null in a column, or -- given `e` -- element `e` of it.
        generic :: is_null => row_is_null, row_is_null_elem
        procedure :: index => row_index     !! This row's 1-based index within the table.
        final :: row_finalize               !! Drops the pointer; owns nothing, frees nothing.
    end type parquet_table_row""")
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
        !!
        !! `filter=`, `sort=` and `qc=` name columns in the table's own INTERNAL vocabulary -- the
        !! names %col/%get use -- which for a remapped column is NOT what the file calls it. They
        !! are translated to file names, and merged with whatever `maml=` declares, before the
        !! reader is opened. A MAML's own `extra: filter:`/`extra: sort:`/`fields: qc:` are in FILE
        !! names, because a read-in MAML describes the physical file and travels with it.
        module subroutine open_table_full(table, filename, maml, filter, sort, qc, qc_soft, use_threads, &
                sample_fraction, sample_seed)
            type(parquet_table), intent(out) :: table !! the table to fill.
            character(len=*), intent(in) :: filename  !! parquet file to open.
            character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML file describing `filename`.
            type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
            type(parquet_sortkey), intent(in), optional :: sort !! sort keys, in INTERNAL column names.
            type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
            logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
            logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
            real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
            integer(int32), intent(in), optional :: sample_seed !! seed for that draw; omitted = nondeterministic.
        end subroutine open_table_full
        !> Slice-regime open, int32 row bounds -- see the `parquet_open_table` generic above, which
        !! also explains why there is no `sort` argument here and what `filter=`/`sample_fraction=`
        !! do to the slice's row count.
        module subroutine open_table_slice_i32(table, filename, row_lo, row_hi, maml, filter, qc, &
                qc_soft, use_threads, sample_fraction, sample_seed)
            type(parquet_table), intent(out) :: table !! the table to fill.
            character(len=*), intent(in) :: filename  !! parquet file to open.
            integer(int32), intent(in) :: row_lo      !! first file row to cover (1-based).
            integer(int32), intent(in) :: row_hi      !! last file row to cover (inclusive).
            character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML file describing `filename`.
            type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
            type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
            logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
            logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
            real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
            integer(int32), intent(in), optional :: sample_seed !! seed for that draw; omitted = nondeterministic.
        end subroutine open_table_slice_i32
        !> Slice-regime open, int64 row bounds -- see the `parquet_open_table` generic above.
        module subroutine open_table_slice_i64(table, filename, row_lo, row_hi, maml, filter, qc, &
                qc_soft, use_threads, sample_fraction, sample_seed)
            type(parquet_table), intent(out) :: table !! the table to fill.
            character(len=*), intent(in) :: filename  !! parquet file to open.
            integer(int64), intent(in) :: row_lo      !! first file row to cover (1-based).
            integer(int64), intent(in) :: row_hi      !! last file row to cover (inclusive).
            character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML file describing `filename`.
            type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
            type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
            logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
            logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
            real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
            integer(int32), intent(in), optional :: sample_seed !! seed for that draw; omitted = nondeterministic.
        end subroutine open_table_slice_i64
        !> Opens `table%cache%reader` on `filename` with whatever read-time transform the table
        !! carries in its `read_*` components already attached. One helper rather than two open
        !! calls, so that %clone's reopen cannot drift from parquet_open_table's own.
        !!
        !! Each half is passed through an ALLOCATABLE local left unallocated when that half is
        !! empty: an unallocated allocatable actual makes an optional dummy absent (F2018
        !! 15.5.2.12), so one unconditional call covers every combination -- and a table with no
        !! transform at all reaches parquet_open_reader with exactly the arguments it always did.
        !!
        !! **A masked slice is the one case where the filter is NOT a constructor argument**: it
        !! carries the slice's own row range, which only `parquet_reader_set_filter` can express,
        !! so it is attached immediately after the open instead. That whole sequence lives here
        !! rather than in the caller for the same reason the rest does -- so `%clone`'s reopen
        !! cannot produce a reader in a different state from the one `parquet_open_table` built.
        !! Requires `cache%slice_row_lo`/`slice_row_hi` and `cache%rg_bounds_physical` to be set
        !! already, and leaves `cache%rg_bounds` holding the resulting per-row-group survivor
        !! counts.
        module subroutine table_open_reader_with_transform(table, filename, use_threads)
            type(parquet_table), intent(inout) :: table !! table whose (allocated) reader is opened.
            character(len=*), intent(in) :: filename    !! parquet file to open.
            logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
        end subroutine table_open_reader_with_transform
        !> The inclusive 1-based row-group range covering rows `row_lo..row_hi` of `bounds`, or
        !! 0/0 when no row group intersects that range at all.
        !!
        !! One helper rather than a scan written out at each site: the same walk decides which row
        !! groups a slice's filter is scoped to and which ones a deferred column's width is
        !! measured over, and the two must agree.
        module subroutine rg_covering_range(bounds, row_lo, row_hi, rg_lo, rg_hi)
            integer(int64), intent(in) :: bounds(:,:) !! (2, nrg) row ranges, in the same coordinates as row_lo/hi.
            integer(int64), intent(in) :: row_lo      !! first row wanted.
            integer(int64), intent(in) :: row_hi      !! last row wanted.
            integer(int64), intent(out) :: rg_lo      !! first covering row group, or 0 for none.
            integer(int64), intent(out) :: rg_hi      !! last covering row group, or 0 for none.
        end subroutine rg_covering_range
        !> Prepares an empty in-memory table with no columns and no rows. The first %add_column
        !! fixes the row count; every later one must match it.
        module subroutine parquet_new_table(table)
            type(parquet_table), intent(out) :: table !! the table to initialize.
        end subroutine parquet_new_table
        !> Reports each row group of `filename` as the inclusive 1-based row range it covers:
        !! `bounds(1, rg)` is its first row and `bounds(2, rg)` its last. Together they partition
        !! 1..nrows exactly.
        !!
        !! Standalone on purpose: this is the PLANNING call, made before any table exists, so
        !! that each thread can work out which slice to open. It opens and closes a reader
        !! internally, which costs only a footer read -- no column data is touched.
        module subroutine parquet_table_row_group_bounds(filename, bounds)
            character(len=*), intent(in) :: filename                     !! parquet file to inspect.
            integer(int64), allocatable, intent(out) :: bounds(:,:)      !! (2, num_row_groups).
        end subroutine parquet_table_row_group_bounds
        !> The same row ranges for an already-open table, without reopening the file, in THIS
        !! TABLE's own row numbering -- or, with `physical=.true.`, in the file's.
        !!
        !! The two differ only when the table does not hold every row of the file: a slice, a
        !! filter, a sample. Then `bounds(1, rg)`/`bounds(2, rg)` are the rows OF THIS TABLE that
        !! came from row group `rg`, which is what relates a row index in hand to the row group it
        !! was read from; `physical=.true.` answers in the file's numbering instead, which is what
        !! `parquet_table_row_group_bounds` and every slice bound are expressed in, and so is what
        !! to use for planning the next slice.
        !!
        !! Both forms have ONE ENTRY PER PHYSICAL ROW GROUP and are index-aligned, so the two can be
        !! read side by side ("row group 7 holds file rows A..B, which are my rows C..D"). A row
        !! group contributing no rows to this table -- outside the slice, or filtered away entirely
        !! -- is reported as an EMPTY range, `bounds(1, rg) > bounds(2, rg)`, rather than dropped;
        !! dropping it would break the alignment that makes the pairing possible.
        module subroutine table_row_group_bounds(self, bounds, physical)
            class(parquet_table), intent(in) :: self                     !! the table.
            integer(int64), allocatable, intent(out) :: bounds(:,:)      !! (2, num_row_groups).
            logical, intent(in), optional :: physical                    !! .true. for the file's own row numbering.
        end subroutine table_row_group_bounds
        !> Fills `bounds` from an open reader: the shared walk both public forms sit on.
        module subroutine reader_row_group_bounds(reader, bounds)
            type(parquet_reader), intent(in) :: reader                   !! open reader.
            integer(int64), allocatable, intent(out) :: bounds(:,:)      !! (2, num_row_groups).
        end subroutine reader_row_group_bounds
        !> Always error stops: see the `assignment(=)` binding.
        !!
        !! `rhs` is `class`, not `type`, so that an EXTENDING type's own `b = a` reaches this guard
        !! too. With a `type(parquet_table)` dummy the match relies on generic resolution against a
        !! non-polymorphic dummy for an actual of an extended type; gfortran 15.2 and flang 22.1
        !! both resolve it, but the polymorphic form removes the question -- and the failure it
        !! would hide is the worst kind, two tables sharing one store and double-freeing it.
        module subroutine table_assign_guard(lhs, rhs)
            class(parquet_table), intent(out) :: lhs !! unused -- this procedure never returns.
            class(parquet_table), intent(in) :: rhs  !! unused -- this procedure never returns.
        end subroutine table_assign_guard
        !> Frees the column store and abandons the reader. Runs at scope exit and on an
        !! intent(out) reopen, so it must always succeed silently -- it validates nothing.
        module subroutine table_finalize(self)
            type(parquet_table), intent(inout) :: self !! the table being destroyed.
        end subroutine table_finalize
        !> Moves one descriptor slot's contents into another, leaving the source slot empty.
        !!
        !! The metadata fields are plain scalars and short allocatable strings, so they are
        !! assigned; `values` is handed over with `%move_from`, which is the whole reason this
        !! exists -- intrinsic assignment on a `parquet_table_column` deep-copies the column's
        !! entire storage, so relocating a slot used to cost a full copy of its data.
        module subroutine move_table_column(dst, src)
            type(parquet_table_column), intent(inout) :: dst !! the slot receiving the column.
            type(parquet_table_column), intent(inout) :: src !! the slot giving it up.
        end subroutine move_table_column
        !> Appends an empty slot named `name` and returns its index, growing `cols(:)` if the
        !! headroom is used up. error stops if the name is already taken and `force` is absent.
        module subroutine table_new_slot(self, name, force, idx)
            !> Deliberately `intent(in)`, not `intent(inout)`: every change it makes is to
            !! `self%cache`, which is a POINTER component, so it needs no more than this -- and
            !! the automatic `parquet_row_index` column has to be created from the lazy read
            !! path, where `self` is `intent(in)` like every other first-touch entry point.
            class(parquet_table), intent(in) :: self
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
        !> This table's row scope and detach state, as the single value a first touch takes.
        pure module function table_scope_of(self) result(sc)
            class(parquet_table), intent(in) :: self !! the table.
            type(table_scope) :: sc                  !! its scope.
        end function table_scope_of
        !> Number of rows every column of this table holds.
        module function table_nrows(self) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64) :: n                      !! row count.
        end function table_nrows
        !> Rows this table covers BEFORE `filter=`/`sample_fraction=` removed any -- the slice's
        !! own length in the slice regime, and the file's row count for a whole-file table.
        !!
        !! `%nrows()` is what the table holds; this is what it was cut from, which nothing else
        !! can report once a transform is active: a filtered reader answers in survivors, so
        !! asking it afterwards gives the same number `%nrows()` already gave. Captured when the
        !! table opens, so it keeps answering after a row mutation has detached the table.
        !!
        !! Equal to `%nrows()` when no filter or sample is in play. 0 for a table built in memory,
        !! which was not cut from anything.
        module function table_nrows_unfiltered(self) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64) :: n                      !! rows before the transform.
        end function table_nrows_unfiltered
        !> Rows in the row groups this table covers, which is what reading it actually costs.
        !!
        !! A slice is read by row group, so a slice of 10 rows straddling two 100k-row groups
        !! decodes 200k rows to produce them. This reports that number, so a caller choosing
        !! slice boundaries can see when a slice is not paying for itself.
        !!
        !! Equal to `%nrows_unfiltered()` when the slice lines up with row-group boundaries, and
        !! for a whole-file table always. 0 for a table built in memory.
        module function table_row_group_extent(self) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64) :: n                      !! rows in the covering row groups.
        end function table_row_group_extent
        !> Number of columns this table has, or only the resident ones with `resident_only`.
        module function table_ncols(self, resident_only) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            logical, intent(in), optional :: resident_only !! .true.: count only columns already read.
            integer :: n                             !! column count.
        end function table_ncols
        !> Copies out every column's name, in file/insertion order, blank-padded to the longest.
        !!
        !! `resident_only=.true.` reports only the columns that have been read, in the same order,
        !! which is how a caller finds out what a lazy table is actually holding.
        module subroutine table_column_names(self, names, resident_only)
            class(parquet_table), intent(in) :: self                  !! the table.
            character(len=:), allocatable, intent(out) :: names(:)    !! one entry per column.
            logical, intent(in), optional :: resident_only            !! .true.: only columns already read.
        end subroutine table_column_names
        !> Whether a column holds any null value.
        !!
        !! Answered as cheaply as the column's state allows, which is the point of having it: a
        !! RESIDENT column answers from its own validity state, and a non-resident file-backed one
        !! answers from the FILE'S FOOTER STATISTICS, reading no column data at all. The footer
        !! answer is conservative -- `.false.` is a guarantee, `.true.` means "may have nulls",
        !! since a file written without statistics cannot say -- and reading the column afterwards
        !! may therefore turn a `.true.` into a `.false.`.
        module function table_has_nulls(self, name, found) result(any_null)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            logical :: any_null                       !! .true. if it holds (or may hold) a null.
        end function table_has_nulls
        !> Copies out a column's per-ROW validity as a plain logical array: .true. where the row
        !! holds a value, .false. where it is null.
        !!
        !! One entry per row of the table. On a *_VEC column this is the SUMMARY -- a row is
        !! `.false.` when any element of it is null -- which is a genuinely useful question
        !! ("which rows are complete?") and is why the rank-1 form is kept alongside the rank-2
        !! one. For the true per-element state, declare `mask` rank-2 instead.
        !!
        !! A column with no nulls at all comes back all `.true.` rather than unallocated, so a
        !! caller never has to test for that case. Triggers the same lazy first touch any other
        !! value access does.
        module subroutine table_get_valid_mask(self, name, mask, found)
            class(parquet_table), intent(in) :: self               !! the table (fills through %cache).
            character(len=*), intent(in) :: name                   !! column name.
            logical, allocatable, intent(out) :: mask(:)           !! one entry per row; .true. = value.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine table_get_valid_mask
        !> Copies out a column's per-ELEMENT validity as a `(width, nrows)` logical array.
        !!
        !! The rank-2 counterpart of the above, and the column's actual state rather than a row
        !! summary. On a scalar column `width` is 1, so the two agree.
        !!
        !! **Note the memory**: `LOGICAL` is 4 bytes under gfortran, so a wide column's mask is
        !! `4*width*nrows` bytes. Use `%is_null(name, i, e)` to ask about a few elements.
        module subroutine table_get_valid_mask_elem(self, name, mask, found)
            class(parquet_table), intent(in) :: self               !! the table (fills through %cache).
            character(len=*), intent(in) :: name                   !! column name.
            logical, allocatable, intent(out) :: mask(:,:)         !! (element, row); .true. = value.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine table_get_valid_mask_elem
        !> A counter bumped by every structural change to this table, for a caller holding a
        !! pointer across a call that might have invalidated it.
        !!
        !! **A stale `%col` pointer cannot be detected by Fortran, and this library cannot detect
        !! it either.** What it can do is tell you whether anything structural happened: take the
        !! generation before, compare it after, and re-fetch the pointer if it moved. The counter
        !! is deliberately conservative -- every column- and row-structural entry point bumps it,
        !! whether or not that particular call actually relocated anything -- because a missed bump
        !! would give false confidence, while a spare one only costs a re-fetch. A call that
        !! changes nothing at all (see the no-detach rule) does not bump it.
        module function table_generation(self) result(g)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64) :: g                      !! current generation.
        end function table_generation
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
        !> Whether row `i` of a column is null (int32 row index).
        module function table_is_null_i32(self, name, i, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int32), intent(in) :: i          !! 1-based row index.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
            logical :: isnull                        !! .true. if that row is null (.false. on a miss).
        end function table_is_null_i32
        !> Whether element `e` of row `i` is null (int32 indices).
        module function table_is_null_e32(self, name, i, e, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int32), intent(in) :: i          !! 1-based row index.
            integer(int32), intent(in) :: e          !! 1-based element index within the row.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
            logical :: isnull                        !! .true. if that element is null (.false. on a miss).
        end function table_is_null_e32
        !> Whether element `e` of row `i` is null (int64 indices).
        module function table_is_null_e64(self, name, i, e, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int64), intent(in) :: i          !! 1-based row index.
            integer(int64), intent(in) :: e          !! 1-based element index within the row.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
            logical :: isnull                        !! .true. if that element is null (.false. on a miss).
        end function table_is_null_e64
        !> Whether row `i` of a column is null (int64 row index).
        module function table_is_null_i64(self, name, i, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int64), intent(in) :: i          !! 1-based row index.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
            logical :: isnull                        !! .true. if that row is null (.false. on a miss).
        end function table_is_null_i64
        !> error stops unless `i` is a valid 1-based row index for this table. Shared by every
        !! per-row entry point so they all report the same way.
        module subroutine table_require_row(self, i, proc)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64), intent(in) :: i          !! the row index to check.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_require_row
        !> Shared front half of every soft-failing query: resolves `name`, honouring `found=` and
        !! otherwise aborting. Unlike `table_resolve` this does NOT require the column to hold
        !! values -- asking a column's kind, or dropping it, must work precisely when it has none.
        module subroutine table_lookup_or_fail(self, name, proc, idx, found)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
            integer, intent(out) :: idx              !! slot index, or 0 on a reported miss.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
        end subroutine table_lookup_or_fail
        !> Resolves `name` to its 1-based slot index, or 0 when absent. The single lookup every
        !! accessor goes through, so a rename or remap only has to change one place.
        module function table_find(self, name) result(idx)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer :: idx                           !! slot index, or 0.
        end function table_find
        !> Resolves `name` for a value access: aborts (or reports through `found`) when the
        !! column is missing, unsupported or not resident.
        module subroutine table_resolve(self, name, proc, idx, found, writing)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            character(len=*), intent(in) :: proc      !! calling procedure, for the message.
            integer, intent(out) :: idx               !! slot index, or 0 when `found` is present.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            !> .true. when the caller is about to WRITE values into the column, which brings
            !! `table_check_shared_write`'s string-column rule with it. Passed by every %set and
            !! %set_element specific, so a new one inherits the rule by copying its neighbour. The
            !! null-writing entry points call that guard directly instead, with `nulling=.true.`,
            !! since only they can trigger the lazy validity allocation.
            logical, intent(in), optional :: writing
        end subroutine table_resolve
        !> Builds the "(file 'x.parquet', column 'y')" suffix every error message carries.
        !! Takes the cache rather than the table so that a `parquet_table_row` handle, which
        !! holds nothing else, can raise messages with the same context.
        module subroutine table_context_suffix(cache, name, suffix)
            type(parquet_table_cache), intent(in) :: cache        !! the column store.
            character(len=*), intent(in) :: name                  !! column name ("" to omit it).
            character(len=:), allocatable, intent(out) :: suffix  !! the message suffix.
        end subroutine table_context_suffix
        !> error stops unless `self%cache` is associated -- the guard every accessor runs first.
        module subroutine table_check_open(self, proc)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_check_open
        !> error stops when the table has been detached from its file by a row-structural change.
        !! The guard every path that would READ from the file runs first: once the row set has
        !! changed, a column still in the file can never be lined up with the columns already in
        !! memory, so reading one would hand back silently misaligned data.
        !!
        !! Takes the cache and the scope rather than the table, so a `parquet_table_row` handle
        !! and the internal read helpers can run the identical guard.
        module subroutine table_check_not_detached(cache, sc, name, proc)
            type(parquet_table_cache), intent(in) :: cache !! the column store, for the message.
            type(table_scope), intent(in) :: sc            !! rows this table covers; carries `detached`.
            character(len=*), intent(in) :: name           !! column name ("" to omit it).
            character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        end subroutine table_check_not_detached
        !> Fills `mask` with slot `idx`'s per-ROW validity: one entry per row, .true. where the
        !! row holds a value. Always allocated, even for a column with no nulls at all, so a
        !! caller never has to test allocated() before using it.
        module subroutine table_valid_mask_of(cache, idx, mask)
            type(parquet_table_cache), intent(in) :: cache !! the table's store.
            integer, intent(in) :: idx                     !! slot index.
            logical, allocatable, intent(out) :: mask(:)   !! one entry per row.
        end subroutine table_valid_mask_of
        !> The same, for an arbitrary list of rows -- what `%get_slice(is_valid=)` needs, since a
        !! selection may be strided, reversed or repeated.
        module subroutine table_valid_mask_rows(cache, idx, rows, mask)
            type(parquet_table_cache), intent(in) :: cache !! the table's store.
            integer, intent(in) :: idx                     !! slot index.
            integer(int64), intent(in) :: rows(:)          !! the selected rows, in order.
            logical, allocatable, intent(out) :: mask(:)   !! one entry per selected row.
        end subroutine table_valid_mask_rows
        !> Applies a caller-supplied validity mask to slot `idx`: every row marked .false. becomes
        !! null. Only ever ADDS nulls -- a .true. entry says nothing about a row that is already
        !! null, and clearing it would invert a call the caller did not make. A wrong-length mask
        !! is an error naming both counts.
        module subroutine table_apply_valid(self, idx, is_valid, name, proc)
            class(parquet_table), intent(inout) :: self !! the table.
            integer, intent(in) :: idx                  !! slot index.
            logical, intent(in) :: is_valid(:)          !! one entry per row; .false. marks it null.
            character(len=*), intent(in) :: name        !! column name, for the message.
            character(len=*), intent(in) :: proc        !! calling procedure, for the message.
        end subroutine table_apply_valid
        !> Applies a caller-supplied validity mask to a SELECTION of rows: every selected row
        !! whose entry is .false. becomes null. Only ever adds nulls, exactly as the whole-column
        !! form does. A mask whose length is not the selection's is an error.
        module subroutine table_apply_valid_rows(self, idx, rows, is_valid, name, proc)
            class(parquet_table), intent(inout) :: self !! the table.
            integer, intent(in) :: idx                  !! slot index.
            integer(int64), intent(in) :: rows(:)       !! the selected rows, in order.
            logical, intent(in) :: is_valid(:)          !! one entry per selected row.
            character(len=*), intent(in) :: name        !! column name, for the message.
            character(len=*), intent(in) :: proc        !! calling procedure, for the message.
        end subroutine table_apply_valid_rows
        !> Fills `mask` with slot `idx`'s per-ELEMENT validity, shaped (width, nrows).
        !!
        !! The rank-2 counterpart of `table_valid_mask_of`, and what a vector column's
        !! `%get`/`%col`/`%get_slice` hand back: a vector column's validity is per element, so
        !! summarising it to one bit per row would be a different (and lossier) answer.
        module subroutine table_valid_mask_of_elem(cache, idx, mask)
            type(parquet_table_cache), intent(in) :: cache  !! the table's store.
            integer, intent(in) :: idx                      !! slot index.
            logical, allocatable, intent(out) :: mask(:,:)  !! (element, row).
        end subroutine table_valid_mask_of_elem
        !> The same, for an arbitrary list of rows -- the rank-2 `%get_slice(is_valid=)` form.
        module subroutine table_valid_mask_rows_elem(cache, idx, rows, mask)
            type(parquet_table_cache), intent(in) :: cache  !! the table's store.
            integer, intent(in) :: idx                      !! slot index.
            integer(int64), intent(in) :: rows(:)           !! the selected rows, in order.
            logical, allocatable, intent(out) :: mask(:,:)  !! (element, selected row).
        end subroutine table_valid_mask_rows_elem
        !> Applies a caller-supplied per-ELEMENT validity mask to slot `idx`: every element marked
        !! .false. becomes null. Only ever ADDS nulls, exactly as the row form does. A mask whose
        !! shape is not (width, nrows) is an error naming both shapes.
        module subroutine table_apply_valid_elem(self, idx, is_valid, name, proc)
            class(parquet_table), intent(inout) :: self !! the table.
            integer, intent(in) :: idx                  !! slot index.
            logical, intent(in) :: is_valid(:,:)        !! (element, row); .false. marks it null.
            character(len=*), intent(in) :: name        !! column name, for the message.
            character(len=*), intent(in) :: proc        !! calling procedure, for the message.
        end subroutine table_apply_valid_elem
        !> Applies a per-ELEMENT validity mask to a SELECTION of rows. Only ever adds nulls.
        module subroutine table_apply_valid_rows_elem(self, idx, rows, is_valid, name, proc)
            class(parquet_table), intent(inout) :: self !! the table.
            integer, intent(in) :: idx                  !! slot index.
            integer(int64), intent(in) :: rows(:)       !! the selected rows, in order.
            logical, intent(in) :: is_valid(:,:)        !! (element, selected row).
            character(len=*), intent(in) :: name        !! column name, for the message.
            character(len=*), intent(in) :: proc        !! calling procedure, for the message.
        end subroutine table_apply_valid_rows_elem
        !> error stops unless an array being written into a row selection has one value per
        !! selected row. Its own procedure because the two counts come from different places --
        !! the caller's array and the resolved selection -- and naming both is what makes the
        !! message useful.
        module subroutine table_require_slice_size(self, n_arr, n_rows, name, proc)
            class(parquet_table), intent(in) :: self !! the table.
            integer(int64), intent(in) :: n_arr      !! values the caller supplied.
            integer(int64), intent(in) :: n_rows     !! rows the selection resolves to.
            character(len=*), intent(in) :: name     !! column name, for the message.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_require_slice_size
        !> A column's min and max as display text, for %print_stat.
        !!
        !! Over the VALUES only: a null row contributes nothing and is counted separately, which
        !! is the sort engine's rule too. An all-null column has neither, and both come back as
        !! "-". A `logical` column reports true/false counts instead of an ordering, and a vector
        !! column's statistic is over all of its elements, flattened.
        module subroutine table_column_stat_text(values, min_s, max_s)
            type(parquet_column), intent(in) :: values          !! the column, which must be resident.
            character(len=:), allocatable, intent(out) :: min_s !! smallest value as text, or "-".
            character(len=:), allocatable, intent(out) :: max_s !! largest value as text, or "-".
        end subroutine table_column_stat_text
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
    ! ---- Read-in (Role-B) MAML (parquet_tables_maml) ----
    interface
        !> Parses a read-in MAML's `extra: remap:` block into two parallel name arrays:
        !! `internal(i)` is the table-facing name the caller will use, `physical(i)` the column it
        !! actually reads from the file. Both are returned trimmed and unquoted, in declaration
        !! order; `n` is 0 (and the arrays are allocated empty) when the MAML declares no remapping.
        !! A repeated internal name is rejected here, since that is malformed MAML on its own terms
        !! and needs no parquet file to detect.
        !!
        !! Takes the loaded `parquet_schema` rather than its `%maml%lines` so that callers can pass
        !! `parquet_load_qc_maml_file(...)` straight in as the actual argument, which is how every
        !! other consumer of that function in this project uses it -- assigning its (derived-type,
        !! allocatable-component) result to a local variable first is a shape nothing else here
        !! relies on.
        !!
        !! A deliberately separate, narrow parser rather than a reuse of the write-side
        !! `parquet_parse_col_map`: that one is validated against a base schema a generic table does
        !! not have, and the two serve opposite directions (this relabels for READING, `col_map:`
        !! for writing). It reads only lines already loaded from disk, so the shared
        !! `parquet_read_maml_source_lines` guarantees (line-length cap, CRLF stripping) come with
        !! them via `parquet_load_qc_maml_file`.
        module subroutine parse_read_maml_remap(schema, internal, physical, n, maml_file)
            type(parquet_schema), intent(in) :: schema !! the loaded read-in MAML.
            character(len=:), allocatable, intent(out) :: internal(:) !! table-facing names, in order.
            character(len=:), allocatable, intent(out) :: physical(:) !! file column each one reads.
            integer, intent(out) :: n                !! entries found; 0 if there is no remap: block.
            character(len=*), intent(in) :: maml_file !! the MAML's own path, for error messages.
        end subroutine parse_read_maml_remap
        !> Validates a parsed remap against the file's own column names and fills `cache%cols`
        !! accordingly -- the enumeration that would otherwise be one slot per physical column.
        !!
        !! Every physical column is visited in file order and expanded into the internal names that
        !! claim it, so file order (and therefore struct-leaf adjacency, which the batch release
        !! policy depends on) is preserved and two internal names reading one column land in
        !! adjacent slots. A physical column whose own name is claimed as an INTERNAL name by some
        !! remap entry is skipped: that is the deliberate shadow, not an error.
        !> Parses one plain YAML string list nested under the MAML's `extra:` section -- the shape
        !! both `extra: filter:` and `extra: sort:` have -- into `items(1:n)`, unquoted and in list
        !! order. `n` is 0 (and `items` is allocated empty) when the MAML has no such key.
        !!
        !! One parser for both keys rather than one each: the two differ only in what the strings
        !! MEAN, and neither this procedure nor the MAML format cares. Every string is passed on to
        !! parquet_filter%add / parquet_sortkey%add verbatim, so both grammars stay defined in
        !! exactly one place.
        module subroutine parse_read_maml_string_list(schema, key, items, n)
            type(parquet_schema), intent(in) :: schema !! the loaded read-in MAML.
            character(len=*), intent(in) :: key        !! nested key to read, with its colon ("filter:").
            character(len=:), allocatable, intent(out) :: items(:) !! the list's entries, in order.
            integer, intent(out) :: n                  !! entries found; 0 if the key is absent.
        end subroutine parse_read_maml_string_list
        !> Splits an `extra: sort:` entry into the key text parquet_sortkey%add takes and the
        !! per-key null placement, consuming an optional trailing `nulls_first`/`nulls_last` word
        !! (case-insensitive). Absent, it defaults to `nulls_last`, matching %add's own default.
        !!
        !! This trailing token is the one place this stage EXTENDS an existing text grammar rather
        !! than reusing it verbatim, and it is deliberately MAML-only: a plain YAML string list has
        !! nowhere else to carry what the Fortran API expresses as %add(key, nulls_first=.true.).
        !! Anything else in that position is left in `key_text` for %add to reject with its own
        !! message, so there is still only one implementation of the direction grammar.
        module subroutine split_sort_nulls_token(entry, key_text, nulls_first)
            character(len=*), intent(in) :: entry !! one raw extra: sort: list entry.
            character(len=:), allocatable, intent(out) :: key_text !! "<column> [asc|desc]", token removed.
            logical, intent(out) :: nulls_first   !! .true. if the entry asked for nulls first.
        end subroutine split_sort_nulls_token
        !> Composes the whole read-time transform, ONCE, before the parquet file is opened: loads
        !! the read-in MAML (if any), parses its `extra: remap:`/`filter:`/`sort:` blocks, translates
        !! the caller's internal-name `filter`/`sort`/`qc` into file names using that remap, and
        !! merges each with its MAML counterpart.
        !!
        !! Composition rules, all from the stage design: filter is AND (code rules first, then the
        !! MAML's -- order is immaterial for AND); sort is ORDER-SENSITIVE, MAML keys first as the
        !! primary ones and code keys appended as tie-breakers; qc is a per-COLUMN override handled
        !! by parquet_compose_read_qc. `out_filter%n`/`out_sort%n`/`n_qc` are zero when that half of
        !! the transform is empty, which is the caller's signal not to pass it to the reader at all.
        !!
        !! Doing this before the reader opens is what lets the full regime hand everything to
        !! parquet_open_reader as constructor arguments, and it means a malformed MAML aborts with
        !! no reader -- and so no live Arrow object -- anywhere in scope.
        module subroutine compose_read_transform(sliced, maml_file, filter, sort, qc, internal, physical, &
                n_remap, out_filter, out_sort, out_qc, n_qc, unit_cols, unit_vals, n_units)
            logical, intent(in) :: sliced          !! .true. for a slice-regime open, which forbids sorting.
            character(len=*), intent(in), optional :: maml_file !! read-in MAML path, if one was given.
            type(parquet_filter), intent(in), optional :: filter !! code filter, INTERNAL names.
            type(parquet_sortkey), intent(in), optional :: sort  !! code sort keys, INTERNAL names.
            type(parquet_read_qc), intent(in), optional :: qc    !! code qc, INTERNAL names.
            character(len=:), allocatable, intent(out) :: internal(:) !! remap: table-facing names.
            character(len=:), allocatable, intent(out) :: physical(:) !! remap: file column each one reads.
            integer, intent(out) :: n_remap        !! live remap entries.
            type(parquet_filter), intent(out) :: out_filter !! composed filter, FILE names.
            type(parquet_sortkey), intent(out) :: out_sort  !! composed sort keys, FILE names.
            type(parquet_schema), intent(out) :: out_qc     !! merged qc schema, FILE names.
            integer, intent(out) :: n_qc           !! columns `out_qc` declares qc for.
            character(len=:), allocatable, intent(out) :: unit_cols(:) !! MAML fields declaring a unit, FILE names.
            character(len=:), allocatable, intent(out) :: unit_vals(:) !! the unit each one declares.
            integer, intent(out) :: n_units        !! live entries in `unit_cols`/`unit_vals`.
        end subroutine compose_read_transform
        !> Fills the cache's column slots from the file's own column names, expanding each physical
        !! column into whatever `extra: remap:` claims it (two internal names may target one
        !! physical column, so slots can outnumber the file's columns). Walks the file's order, so
        !! a struct's leaves stay adjacent for the batch release policy.
        module subroutine table_enumerate_columns(cache, names, internal, physical, n_remap, filename)
            type(parquet_table_cache), intent(inout) :: cache !! the column store to fill.
            character(len=*), intent(in) :: names(:) !! the file's own column names, in file order.
            character(len=*), intent(in) :: internal(:) !! remap: table-facing names.
            character(len=*), intent(in) :: physical(:) !! remap: the file column each one reads.
            integer, intent(in) :: n_remap           !! live entries in internal/physical.
            character(len=*), intent(in) :: filename !! source file, for error messages.
        end subroutine table_enumerate_columns
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
        !> Settles slot `idx`'s kind, width and supported flag from the file SCHEMA alone,
        !! reading no column data. Run once per column at open time, so that %kind/%width can
        !! answer before anything has been materialized.
        module subroutine table_classify(cache, idx)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            integer, intent(in) :: idx                        !! slot to classify.
        end subroutine table_classify
        !> Reads one already-classified file column into slot `idx`'s value store and marks it
        !! RES_FULL. Does NOT release the Arrow buffers -- that is the caller's policy choice,
        !! since a struct's array is shared by all its leaves (see `table_materialize_all`).
        module subroutine table_materialize(cache, sc, idx, rdr)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            type(table_scope), intent(in) :: sc               !! rows this table covers.
            integer, intent(in) :: idx                        !! slot to fill.
            !> Reader to decode through, INSTEAD of the store's own. Present only on the
            !! internally-parallel prefetch path, where each thread must drive a reader nothing
            !! else is touching -- a shared parquet_reader entered from two threads at once is
            !! caught by the C++ ConcurrencyGuard and aborts the process. Absent everywhere else,
            !! which is the ordinary single-reader path.
            type(parquet_reader), intent(inout), optional :: rdr
        end subroutine table_materialize
        !> Reads every supported, file-backed column that is not resident yet, releasing each
        !! column's Arrow buffers as it goes so peak memory stays one column above the Fortran
        !! store. Columns already resident are left untouched.
        module subroutine table_materialize_all(cache, sc)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            type(table_scope), intent(in) :: sc               !! rows this table covers.
        end subroutine table_materialize_all
        !> Frees the reader-side Arrow buffers behind column path `name`, which for a dotted
        !! struct leaf means the whole struct's array. Releasing a name twice, or one that was
        !! never read, is a quiet no-op.
        module subroutine table_release_one(cache, name, rdr)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            character(len=*), intent(in) :: name              !! column path to release.
            type(parquet_reader), intent(inout), optional :: rdr !! reader to release from; see table_materialize.
        end subroutine table_release_one
        !> Whether a lazy first touch on this store would be unsafe right now.
        !!
        !! It is unsafe in exactly one situation: the caller is inside an OpenMP parallel region
        !! AND the table was opened outside it, so other threads may be reading the same store
        !! while this one allocates into it. A table a thread opened INSIDE the region is
        !! thread-private by construction -- that is how a parallel per-slice program is written
        !! -- so its first touch is left alone.
        !!
        !! Always .false. when the library itself is built without `-fopenmp`: under fpm the
        !! whole tree is built with one flag set, so a program using OpenMP gets the real answer,
        !! but a library compiled without it cannot see its caller's regions at all.
        module function unsafe_first_touch(cache) result(unsafe)
            type(parquet_table_cache), intent(in) :: cache !! the column store.
            logical :: unsafe                              !! .true. if a first touch must be refused.
        end function unsafe_first_touch
        !> Records, at open time, whether this store was created inside a parallel region and by
        !! which thread -- the two facts `unsafe_first_touch` needs later.
        module subroutine record_open_thread(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine record_open_thread
        !> Whether a STRUCTURAL change to this store would be unsafe right now.
        !!
        !! Same ownership test `unsafe_first_touch` applies to a first touch, and deliberately the
        !! same one rather than a second concept: a table this very thread opened inside the region
        !! is thread-private, so mutating it is the caller's own business, while any other table
        !! visible inside a parallel region may be shared. Every column- and row-structural entry
        !! point routes through this; %append is the one exception, because the table's own lock
        !! makes it safe (see table_lock).
        module function unsafe_shared_mutation(cache) result(unsafe)
            type(parquet_table_cache), intent(in) :: cache !! the column store.
            logical :: unsafe                              !! .true. if a structural change must be refused.
        end function unsafe_shared_mutation
        !> Initialises the store's lock. Called once, immediately after the cache is allocated.
        module subroutine table_init_lock(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine table_init_lock
        !> Destroys the store's lock, if it has one. Safe to call twice and safe to call from a
        !! finalizer: it validates nothing and can never abort.
        module subroutine table_destroy_lock(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine table_destroy_lock
        !> Takes the store's lock, blocking until it is free. ONLY `table_append_table` may call
        !! this -- an OpenMP simple lock is not recursive, so a second acquisition on one thread
        !! deadlocks rather than failing to build.
        module subroutine table_lock(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine table_lock
        !> Releases the store's lock.
        module subroutine table_unlock(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine table_unlock
        !> TEST-ONLY: forces this table's "an append is in flight"/"a read is in flight" counters,
        !! so the two concurrency aborts can be provoked from ONE thread, deterministically.
        !!
        !! **This is a debug hook, not API.** It exists because the guards it drives
        !! (`table_check_no_append`, and `%append`'s own reader check) can otherwise only be
        !! triggered by two threads overlapping on demand, and a timing-dependent test is worse than
        !! no test -- it fails on a busy machine and gets disabled. Unlike the C++ `parquet_debug_*`
        !! hooks, which a test reaches through its own local `bind(C)` interface, a Fortran-side hook
        !! has no such escape hatch: these counters live on `parquet_table_cache`, whose components
        !! are private to this module, so forcing them requires a public procedure here. That cost
        !! was accepted deliberately (feature_risks.md Risk-6); it is excluded from README.md's API
        !! overview and no library code calls it.
        !!
        !! Both arguments are optional and independent: `appending=.true.` makes every READ on this
        !! table abort, `reading=.true.` makes every `%append` abort. Pass `.false.` to clear.
        module subroutine parquet_debug_table_set_inflight(table, appending, reading)
            type(parquet_table), intent(in) :: table   !! the table whose counters to force.
            logical, intent(in), optional :: appending !! .true.: pretend an append is in flight.
            logical, intent(in), optional :: reading   !! .true.: pretend a read is in flight.
        end subroutine parquet_debug_table_set_inflight
        !> Aborts if another thread is inside %append on this store.
        !!
        !! The cheap half of the append/read contract, and the one every read entry point takes:
        !! a single atomic read of a counter, against a call that was going to copy a column
        !! anyway. Detection is best-effort by construction -- a read starting fractionally before
        !! the appender publishes its flag is not caught -- so this is a safety net over the
        !! documented append-only contract, not a replacement for it.
        module subroutine table_check_no_append(cache, proc)
            type(parquet_table_cache), intent(in) :: cache !! the column store.
            character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        end subroutine table_check_no_append
        !> `table_check_no_append`, plus registering this read as in flight so a concurrent %append
        !! can refuse. Only the COARSE read entry points pair this with `table_read_exit`; the
        !! per-element accessors take `table_check_no_append` alone -- see parquet_table_cache's
        !! `readers_active` comment for why that asymmetry is deliberate.
        module subroutine table_read_enter(cache, proc)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            character(len=*), intent(in) :: proc              !! calling procedure, for the message.
        end subroutine table_read_enter
        !> Ends a read registered by `table_read_enter`. Must run on every exit path from it.
        module subroutine table_read_exit(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine table_read_exit
        !> Aborts if a caller is about to WRITE into a column another thread may be using, in one
        !! of the two ways that is unsafe even though writing values generally is not.
        !!
        !! Writing values into a resident column is normally free of any concurrency concern --
        !! different columns are different allocations, and disjoint row ranges of one fixed-width
        !! column are ordinary Fortran element writes. Two cases break that, and both are silent:
        !!
        !!   1. **A string column's rows are not independent.** `parquet_string_column` is a packed
        !!      variable-length store, so writing any element can move the whole payload -- "disjoint
        !!      row ranges" is not a meaningful division of it. Any table-level write to a string
        !!      column on a possibly-shared table is refused.
        !!   2. **The first null allocates.** Validity storage is lazy (that is the sparse-validity
        !!      property a null-free column depends on), so two threads nulling elements of the same
        !!      previously null-free column race on the allocation. Refused, naming
        !!      `%ensure_validity`, which is the way to make the allocation happen up front and let
        !!      the concurrent nulling proceed.
        !!
        !! A temporal column is exempt from (2) by construction: its null state lives in the element,
        !! so nulling allocates nothing and never could.
        module subroutine table_check_shared_write(self, idx, proc, nulling)
            class(parquet_table), intent(in) :: self !! the table being written to.
            integer, intent(in) :: idx               !! 1-based slot index of the target column.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
            logical, intent(in) :: nulling           !! .true. if the write can create a null.
        end subroutine table_check_shared_write
        !> Materializes a column's validity storage up front, so concurrent nulling allocates
        !! nothing. See `table_check_shared_write`.
        module subroutine table_ensure_validity(self, name, found)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in), optional :: name !! column to prepare; absent = every resident one.
            logical, intent(out), optional :: found  !! .false. if `name` is not a column here.
        end subroutine table_ensure_validity
        !> Aborts if a caller is about to mutate a store another thread may be using.
        !!
        !! The shared refusal behind every structural entry point: `unsafe_shared_mutation` plus a
        !! message naming `proc` and the table.
        module subroutine table_check_not_shared(self, proc)
            class(parquet_table), intent(in) :: self !! the table being changed.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine table_check_not_shared
        !> Resolves a `width_pending` column's kind and width, then clears the flag. A no-op for
        !! every other column, so callers can invoke it unconditionally.
        !!
        !! `proven` is the whole design in one argument. `.true.` (the `%kind`/`%width` path) walks
        !! the covered row groups to PROVE the width, since answering a metadata query with a
        !! guess would silently mis-type the column. `.false.` (the read path) takes the footer
        !! screen's unproven candidate and lets the read itself settle it -- the C++ reader checks
        !! every row's length against the width it was given and aborts on a mismatch, so a wrong
        !! candidate fails loudly instead of quietly, and the read that would have happened anyway
        !! doubles as the proof. That is what keeps `%prefetch` on such a column to ONE pass over
        !! the data.
        !!
        !! Measuring is scoped to the row groups `sc` actually covers, so a slice pays for its own
        !! rows only -- and a file that is ragged overall may therefore present a uniform width
        !! within one slice.
        module subroutine table_resolve_width(cache, sc, idx, proven, proc)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            type(table_scope), intent(in) :: sc               !! rows this table covers.
            integer, intent(in) :: idx                        !! slot to resolve.
            logical, intent(in) :: proven                     !! .true.: prove it; .false.: candidate only.
            character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        end subroutine table_resolve_width
        !> Makes slot `idx` resident if it is not already: the lazy first touch every value
        !! accessor goes through. Returns immediately for a column that is already RES_FULL --
        !! that path takes no lock and is what a parallel loop over resident data runs on.
        !!
        !! Takes the cache and the scope rather than the table, so that a `parquet_table_row`
        !! handle (which holds exactly those two things) triggers an identical first touch.
        module subroutine table_touch(cache, sc, idx, proc)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            type(table_scope), intent(in) :: sc               !! rows this table covers.
            integer, intent(in) :: idx                        !! slot to make resident.
            character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        end subroutine table_touch
        !> Reads one named column now rather than on first touch. A column already resident is
        !! left alone; an unsupported one is an error, since asking to read something unreadable
        !! is a mistake worth hearing about.
        !!
        !! `name` may also be a STRUCT's own name, with no dot: every leaf under `name.` is then
        !! read in ONE pass, which is what makes it worth having -- the reader decodes a struct as
        !! one array shared by all its leaves, so reading them one at a time decodes it once per
        !! leaf. A real column of that exact name always wins over the prefix reading, and a name
        !! matching neither is a missing column reported the usual way.
        module subroutine prefetch_one(self, name, found)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
            character(len=*), intent(in) :: name     !! column to read.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
        end subroutine prefetch_one
        !> Reads several named columns now, in one pass, so that a struct whose leaves are all
        !! named is decoded once rather than once per leaf.
        module subroutine prefetch_many(self, names, found)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
            character(len=*), intent(in) :: names(:) !! columns to read.
            logical, intent(out), optional :: found  !! present: .false. if ANY name was missing.
        end subroutine prefetch_many
        !> Reads every supported, file-backed column that is not resident yet -- the one-call way
        !! to make a whole table safe to use from a parallel region.
        module subroutine table_materialize_every(self)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
        end subroutine table_materialize_every
        !> Prints what this table holds, to standard output: one header line and one line per
        !! column, with each column's kind, width, row count, null count and min/max.
        !!
        !! **Materialized columns only, by default** -- the header says how many of the table's
        !! columns those are, so a lazy table reports what it is actually holding rather than what
        !! its file contains. `all=.true.` lists every column, with `-` where a column that has
        !! not been read has nothing to report.
        !!
        !! **Printing never reads anything.** A deferred plain-`LIST` column, whose width is only
        !! knowable from its data, prints as `pending` rather than being resolved -- a diagnostic
        !! that changes what it is diagnosing is worse than one that admits it does not know.
        !!
        !! These are statistics of the values IN MEMORY, computed here by a plain Fortran scan.
        !! They are not the file's own footer statistics, and they are the only ones available for
        !! a column built with %add_column, which has no footer at all. The scan is O(rows) per
        !! column, so this is not a call to put in a loop over a large table.
        module subroutine table_print_stat(self, all)
            class(parquet_table), intent(in) :: self !! the table.
            logical, intent(in), optional :: all     !! .true.: list every column, not just the resident ones.
        end subroutine table_print_stat
        !> .true. when this table's read-time transform REMOVES rows, so the reader's own row
        !! numbering is the surviving rows rather than the file's.
        !!
        !! A sort does not count: it reorders rows without removing any. Declared here rather than
        !! kept private to one submodule because more than one needs it, and two copies of "does
        !! this transform narrow?" would be exactly the kind of predicate that drifts apart.
        module function table_transform_narrows(cache) result(narrows)
            type(parquet_table_cache), intent(in) :: cache !! the table's store, with its transform.
            logical :: narrows                             !! .true. if rows are removed.
        end function table_transform_narrows
        !> Gives the automatic `parquet_row_index` column a real slot and fills it.
        !!
        !! Where the values come from depends only on what the table is: `i` for a whole file with
        !! no transform, `row_lo + i - 1` for an unfiltered slice, and -- for a filtered, sampled
        !! or sorted table -- the reader's own account of which file rows survived and in what
        !! order, which nothing else can reconstruct.
        !!
        !! Private: reached through the ordinary column API, which resolves the reserved name to
        !! this on first use.
        module subroutine table_make_row_index(self)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
        end subroutine table_make_row_index
        !> Checks this table's read-time qc against the file, WITHOUT leaving the columns resident.
        !!
        !! qc is enforced when a column is read, so on a lazy table a declared bound is only
        !! checked once something asks for that column -- which means a program that reads two of
        !! forty columns never finds out whether the other thirty-eight satisfy their bounds. This
        !! reads exactly the columns that declare a bound, letting the reader check them, and then
        !! releases the ones it had to read.
        !!
        !! **What it leaves behind is the point.** Residency is recorded BEFORE anything is read,
        !! and only the columns this call made resident are released afterwards -- a column that
        !! was already in memory stays there, values and all. So it can be called at any time
        !! without disturbing what the program is working on.
        !!
        !! A violation is reported the way it would be on an ordinary read: an abort, or a warning
        !! under `qc_soft=`. A table with no qc declared at all, and one built in memory, are
        !! no-ops -- there is nothing to check and no file to check it against.
        module subroutine table_validate_qc(self)
            class(parquet_table), intent(inout) :: self !! the table.
        end subroutine table_validate_qc
        !> Releases one column's VALUES while keeping its slot -- the honest counterpart of
        !! %prefetch, and the way to give a column's memory back without losing the column.
        !!
        !! The difference from %drop_column is what survives: an evicted column still appears in
        !! %column_names, still answers %kind/%width/%unit, and is READ AGAIN on the next touch.
        !! A dropped one is gone. So %drop_column is for a column you are finished with, and
        !! %evict_column for one you are finished with FOR NOW.
        !!
        !! Only a file-backed column of an attached table can be evicted, and that restriction is
        !! the whole safety story: everywhere else the values are the only copy that exists, so
        !! evicting them would be silent data loss rather than a memory saving. A column built
        !! with %add_column, and any column of a detached table, is therefore an error naming what
        !! is wrong. Evicting a column that is not resident is a no-op.
        !!
        !! Eviction is user-driven only. Nothing in this library evicts on its own -- no LRU, no
        !! memory budget -- so what a table holds stays predictable from the calls you wrote.
        module subroutine table_evict_column(self, name, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column to release.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine table_evict_column
        !> Re-reads one column from the file, discarding whatever is in the store -- the escape
        !! hatch back to the file's own values after %set has changed them locally. Only valid
        !! for a file-backed column of a table that has not been detached.
        module subroutine table_reload(self, name, found)
            class(parquet_table), intent(in) :: self !! the table (refills through %cache).
            character(len=*), intent(in) :: name     !! column to re-read.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
        end subroutine table_reload
    end interface
    !
    ! ---- Write-out (parquet_tables_write) ----
    interface
        !> Writes `table` to `filename` using `schema` to choose and name the output columns:
        !! every enabled schema field is looked up in the table BY ITS INTERNAL NAME, and written
        !! under its own output name (a col_map: rename is honoured automatically). A schema field
        !! with no matching table column is an error; a table column the schema does not name is
        !! simply not written. `row_mask` writes a row subset without changing the table.
        !!
        !! **`schema` is optional.** Without one the write is *schema-less*: every column that is
        !! currently RESIDENT is written, in slot order, under its own internal name -- a quick
        !! path for a small or temporary table that reads nothing and needs no schema built for
        !! it. A table with nothing resident writes a valid empty file. The automatic
        !! `parquet_row_index` column is never written by a schema-less write, even when it is
        !! resident; name it in a schema to write it. Without a schema there is no `col_map:` and
        !! no `qc:`, so output names are the internal names and writer-side qc is off.
        !!
        !! A schema built with `%init`/`%add_field` and never parsed is parsed here, so calling
        !! `parquet_parse_maml` first is optional. That is why `schema` is `intent(inout)`: the
        !! caller's schema is parsed on return.
        !!
        !! `copy_metadata=.true.` carries every key/value metadata entry of the table's SOURCE FILE
        !! into the output; `metadata_keys=` carries only the listed keys (and error stops on one
        !! the source does not have). The two are mutually exclusive. A key the schema itself
        !! declares wins and is not overwritten -- the schema is the explicit statement. Both work
        !! after the table has detached, since the metadata was snapshotted at open, and neither
        !! adds anything to the caller's own schema.
        !!
        !! `write_maml`, `qc`, `compression`, `compression_level`, `chunk_size`, `use_threads` and
        !! `overwrite` are pass-throughs to `parquet_open_writer` with its own defaults and no
        !! reinterpretation, so a table write and the equivalent hand-written open stay the same
        !! calls. `chunk_size` is default-kind `integer` on purpose: a row group cannot hold more
        !! than int32 rows, so there is no `int64` form to add.
        !!
        !! `release` (default `.true.`) leaves the table in the residency state it started in: a
        !! column this write had to materialize is evicted again once it has been written, while a
        !! column the caller had already read is left alone. Releasing frees storage a `%col`
        !! pointer could alias, so the generation counter advances when at least one column was
        !! actually released -- possibly a false alarm (nothing you can hold a pointer to is ever
        !! released), never a missed one.
        module subroutine parquet_write_table(table, filename, schema, row_mask, copy_metadata,   &
                metadata_keys, write_maml, qc, compression, compression_level, chunk_size,        &
                use_threads, overwrite, release)
            class(parquet_table), intent(in) :: table  !! the table to write (any extending type too).
            character(len=*), intent(in) :: filename   !! output parquet file.
            type(parquet_schema), intent(inout), optional :: schema !! output schema; absent = schema-less write.
            logical, intent(in), optional :: row_mask(:)  !! per-row write mask.
            logical, intent(in), optional :: copy_metadata !! .true.: carry every source-file metadata entry.
            character(len=*), intent(in), optional :: metadata_keys(:) !! carry only these source-file keys.
            logical, intent(in), optional :: write_maml !! also save a sidecar .maml next to filename.
            logical, intent(in), optional :: qc !! run the schema's qc: checks on write; defaults to on.
            character(len=*), intent(in), optional :: compression !! Arrow compression codec name (e.g. "snappy").
            integer, intent(in), optional :: compression_level !! codec-specific compression level.
            integer, intent(in), optional :: chunk_size !! Parquet row-group size, in rows.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded writer.
            logical, intent(in), optional :: overwrite !! allow truncating an existing file; default .true.
            logical, intent(in), optional :: release !! evict columns this write materialized; default .true.
        end subroutine parquet_write_table
    end interface""")
    w("    !")
    w("    ! ---- Zero-copy pointer access (parquet_tables_access) ----")
    w("    interface")
    for k in PTR_KINDS:
        w(ptr_iface(k))
    w(ptr_str_iface())
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
    w(set_strcol_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- From-scratch construction (parquet_tables_addcol) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        w(add_iface(k))
    w(add_str_iface())
    w(add_strcol_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- Single-cell mutation (the per-kind writers in ..._access, the rest in ..._mutate) ----")
    w("    interface")
    for k in ARRAY_KINDS:
        w(getelem_iface(k))
    w(getelem_str_iface())
    for k in ARRAY_KINDS:
        w(setelem_iface(k))
    w(setelem_str_iface())
    w("""        !> Marks row `i` of a column null (int32 row index).
        module subroutine set_null_i32(self, name, i, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i             !! 1-based row index.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_null_i32
        !> Marks row `i` of a column null (int64 row index).
        module subroutine set_null_i64(self, name, i, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i             !! 1-based row index.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_null_i64
        !> Marks element `e` of row `i` null, leaving the row's other elements alone (int32).
        module subroutine set_null_e32(self, name, i, e, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i             !! 1-based row index.
            integer(int32), intent(in) :: e             !! 1-based element index within the row.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_null_e32
        !> Marks element `e` of row `i` null, leaving the row's other elements alone (int64).
        module subroutine set_null_e64(self, name, i, e, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i             !! 1-based row index.
            integer(int64), intent(in) :: e             !! 1-based element index within the row.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_null_e64
        !> Marks null every ELEMENT whose `is_valid` entry is .false., in one call.
        !!
        !! The rank-2 counterpart of the row form below, taking a `(width, nrows)` mask. Only ever
        !! ADDS nulls, on exactly the same terms.
        module subroutine set_null_mask_elem(self, name, is_valid, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            logical, intent(in) :: is_valid(:,:)        !! (element, row); .false. marks it null.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_null_mask_elem
        !> Marks null every row whose `is_valid` entry is .false., in one call.
        !!
        !! The mask is the same shape `%get_valid_mask` hands back and `is_valid=` takes elsewhere:
        !! one entry per row, `.true.` meaning the row holds a value. Rows marked `.true.` are left
        !! exactly as they are -- this only ever ADDS nulls, so it composes with a mask that
        !! describes only part of what the caller knows. A wrong-length mask is an error.
        module subroutine set_null_mask(self, name, is_valid, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            logical, intent(in) :: is_valid(:)          !! one entry per row; .false. marks it null.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_null_mask
        !> Marks row `i` of a column valid, leaving its value unspecified (int32 row index).
        module subroutine clear_null_i32(self, name, i, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i             !! 1-based row index.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine clear_null_i32
        !> Marks row `i` of a column valid, leaving its value unspecified (int64 row index).
        module subroutine clear_null_i64(self, name, i, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i             !! 1-based row index.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine clear_null_i64
        !> Marks element `e` of row `i` valid, leaving its value unspecified (int32 indices).
        module subroutine clear_null_e32(self, name, i, e, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i             !! 1-based row index.
            integer(int32), intent(in) :: e             !! 1-based element index within the row.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine clear_null_e32
        !> Marks element `e` of row `i` valid, leaving its value unspecified (int64 indices).
        module subroutine clear_null_e64(self, name, i, e, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i             !! 1-based row index.
            integer(int64), intent(in) :: e             !! 1-based element index within the row.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine clear_null_e64
        !> Drops a column's null bitmap when it no longer holds any null, so a column that HAD
        !! nulls and no longer does stops paying for the bitmap. Scans the column, so it is not
        !! free -- a whole-column %set already compacts on its own and does not need this.
        !! Idempotent: calling it on an already-compact column is a cheap no-op.
        module subroutine table_compact_validity(self, name, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine table_compact_validity
    end interface""")
    w("    !")
    w("""    ! ---- Column-structural mutation (parquet_tables_mutate) ----
    interface
        !> Removes a column from the table. Cheap, and it does NOT detach: dropping a column
        !! leaves every remaining column the same length, so the table can still read the ones it
        !! has not read yet. Dropping a column that was never read is the memory-reclaiming case
        !! and reads nothing.
        module subroutine table_drop_column(self, name, force, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column to remove.
            logical, intent(in), optional :: force      !! .true. to drop a PREDEFINED column.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine table_drop_column
        !> Changes a column's name. Only the name a caller looks it up by changes -- a
        !! file-backed column that has not been read yet still reads from the same physical
        !! column afterwards. A predefined column cannot be renamed at all (its accessor is bound
        !! to the name at compile time), and there is no `force=` for it.
        module subroutine table_rename_column(self, old_name, new_name, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: old_name    !! the column to rename.
            character(len=*), intent(in) :: new_name    !! its new name; must not already exist.
            logical, intent(out), optional :: found     !! present: report a missing SOURCE column instead of aborting.
        end subroutine table_rename_column
        !> Adds a NEW column holding a copy of `name`'s values, leaving the source column
        !! untouched. With `to_kind` absent it is a plain deep copy and works for EVERY kind the
        !! library can read -- string, temporal, logical and the vector kinds included. With
        !! `to_kind` given it copies and converts, and the conversion rules are `%cast`'s.
        !!
        !! `exact` defaults to `.true.` here, the opposite of `%cast`: a copy is usually taken to
        !! keep something, so a value that would not survive the round trip is an error naming the
        !! row and the value rather than a silent truncation. Every value is checked before
        !! anything is written, so a rejected copy leaves the table exactly as it was. The unit
        !! carries over unchanged -- a kind conversion is not a unit change.
        module subroutine table_copy_column(self, name, new_name, to_kind, exact, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the source column.
            character(len=*), intent(in) :: new_name    !! the column to create.
            integer, intent(in), optional :: to_kind    !! target PK_* kind; absent keeps the source's.
            logical, intent(in), optional :: exact      !! .false. to allow lossy narrowing (default .true.).
            logical, intent(out), optional :: found     !! present: report a missing SOURCE column instead of aborting.
        end subroutine table_copy_column
        !> Converts a column to `to_kind` IN PLACE, so that `%col` can be called with a kind the
        !! calling code has decided on rather than the one the file happens to hold. A column
        !! already of `to_kind` is left alone.
        !!
        !! The conversions allowed are exactly those the reader and writer already perform
        !! between numeric kinds: int32 <-> int64, float32 <-> float64, and either integer kind
        !! to or from either real kind, scalar or vector, never changing a column's width. An
        !! integer overflow, and a real value with a fractional part converted to an integer
        !! kind, are errors naming the row and the value. Anything else -- logical, string,
        !! temporal, or a conversion that would change a column's rank -- is refused outright.
        !!
        !! `exact` defaults to `.false.`: precision loss (real64 to real32, or a large integer to
        !! a real kind) is silent, exactly as it is on the read path. Pass `.true.` to make any
        !! value that would not survive the round trip an error instead.
        !!
        !! **This invalidates any pointer previously taken from `%col`** for this column, which
        !! Fortran cannot detect -- take the pointer again afterwards.
        module subroutine table_cast(self, name, to_kind, exact, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the column to convert.
            integer, intent(in) :: to_kind              !! target PK_* kind.
            logical, intent(in), optional :: exact      !! .true. to refuse any precision loss.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine table_cast
        !> Appends an already-built `parquet_column` as a new column. The kind-generic
        !! `%add_column` covers every case a user has; this is the internal path for code that
        !! already holds a column of the right shape and only needs it slotted in.
        module subroutine table_put_column(self, name, col)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the new column's name.
            type(parquet_column), intent(in) :: col     !! the column to copy in.
        end subroutine table_put_column
    end interface""")
    w("    !")
    w("""    ! ---- Row-structural mutation -- detaches whenever it changes the row set (parquet_tables_rowmutate) ----
    interface
        !> Keeps only the rows whose `keep` entry is .true., dropping the rest from EVERY column.
        !!
        !! Row-structural, so it DETACHES the table from its file: after it, a column that was
        !! never read can never be read, because the file's rows no longer line up with the rows
        !! in memory. Materialize what you need first (`%prefetch`/`%materialize_all`). An
        !! all-`.true.` mask removes no row, so it changes nothing and does not detach.
        module subroutine table_filter_rows(self, keep)
            class(parquet_table), intent(inout) :: self !! the table.
            logical, intent(in) :: keep(:)              !! one entry per row; .true. to retain it.
        end subroutine table_filter_rows
        !> Reorders every column's rows by one or more key columns, in memory.
        !!
        !! Runs the library's own C++ sort engine -- the same one a read-time `sort_by=` uses, so
        !! the two cannot order the same keys differently. Keys apply in the order given, the
        !! first being the primary. `descending`/`nulls_first`, when given, carry one entry per
        !! key. Nulls and NaNs are placed absolutely and are never flipped by `descending`.
        !!
        !! A key column that is not resident yet is READ, by the same lazy first touch every value
        !! accessor uses -- so sorting a freshly opened table needs no `%prefetch` first. Only the
        !! key columns are read; the rest stay as they were.
        !!
        !! Row-structural, so it DETACHES -- unless the rows were already in that order, in which
        !! case nothing moves and nothing is detached.
        module subroutine table_sort_by(self, keys, descending, nulls_first)
            class(parquet_table), intent(inout) :: self       !! the table.
            character(len=*), intent(in) :: keys(:)           !! key columns, primary first.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_sort_by
        !> Keeps only the `n` rows a `%sort_by` on the same keys would put first, in that order.
        !!
        !! Selection rather than a full sort, which is the point: asking a 100-million-row table for
        !! its best 100 costs O(nrows) instead of O(nrows log nrows), and each column is gathered
        !! straight to `n` rows rather than reindexed in full and then shrunk. `%sort_by` followed by
        !! `%truncate` gives the same answer and does neither of those things.
        !!
        !! Same keys, same engine and same refusals as `%sort_by`, including the lazy first touch of
        !! a key column that has not been read yet. "The last `n`" is `descending=`, not a separate
        !! binding.
        !!
        !! Row-structural, so it DETACHES -- except when `n` is at or above the row count, where this
        !! IS `%sort_by` and inherits its rule that a table already in that order is left alone.
        !!
        !! Deliberately the only way to reduce a table to a chosen set of rows in a chosen order:
        !! there is no `%take(indices)` taking a caller-supplied permutation, because applying one
        !! partially, or applying a stale one, breaks row correspondence with nothing to report it
        !! (`feature_risks.md` Risk-33). Here the indices are produced inside the call, from this
        !! table's own columns, and applied to every column together.
        module subroutine table_top_n(self, keys, n, descending, nulls_first)
            class(parquet_table), intent(inout) :: self       !! the table.
            character(len=*), intent(in) :: keys(:)           !! key columns, primary first.
            !> how many rows to keep, CLAMPED to the row count rather than checked, so an `n`
            !! derived from a fraction or a config value needs no `min()` of its own; a negative `n`
            !! is an error. Deliberately a plain default-kind `integer` and not also an int64 form:
            !! an `n` that large is not a top-N but a whole sort, which is what this delegates to.
            integer, intent(in) :: n
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_top_n""")

    for ik, idecl, aname in (("i32", "integer(int32)", "int32"), ("i64", "integer(int64)", "int64")):
        w(f"""        !> The 1-based row order `keys` implies, WITHOUT applying it, as {aname} indices.
        !!
        !! Same keys, same engine and same refusals as `%sort_by`; the difference is that nothing
        !! moves. That matters because `%sort_by` DETACHES: this is the only way to read a table's
        !! rows in some order and still have the file behind it. `%get_slice(name,
        !! parquet_slice_list(perm), values)` is how the permutation is consumed.
        !!
        !! **The permutation describes the table AS IT WAS.** Nothing links the two afterwards, so
        !! any row-structural change (`%sort_by`, `%top_n`, `%filter_rows`, `%delete_rows`, `%truncate`,
        !! `%append`) silently invalidates it -- against a table that has since shrunk the indices
        !! stay in range and name the wrong rows. `%generation()` is bumped by every such change:
        !! record it beside a permutation you intend to keep, and compare before reusing.
        module subroutine table_argsort_by_{ik}(self, keys, perm, descending, nulls_first, &
                group_offsets, group_nkeys)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys(:)           !! key columns, primary first.
            {idecl}, allocatable, intent(out) :: perm(:) !! the 1-based row order.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
            !> where each run of rows equal under the grouping keys begins, as offsets INTO `perm`:
            !! length `ngroups + 1`, last entry the sentinel `nrows + 1`, so group g is
            !! `perm(o(g) : o(g+1) - 1)` for every g. Always allocated when asked for -- an empty
            !! table gives `[1]`. All nulls form one group and all NaNs form one group.
            {idecl}, allocatable, intent(out), optional :: group_offsets(:)
            !> how many LEADING keys must be equal for two rows to share a group; absent means all
            !! of them. Counts key NAMES, so `group_nkeys=1` over `["field", "mag"]` gives one group
            !! per field with the rows inside each ordered by mag. Requires `group_offsets`.
            integer, intent(in), optional :: group_nkeys
        end subroutine table_argsort_by_{ik}
        !> The first `n` rows of the order `keys` implies, as {aname} indices, length `n`.
        !!
        !! Selection rather than a full sort, which is the point: asking a 100M-row table for its
        !! best 100 costs O(nrows) instead of O(nrows log nrows), and reorders nothing. `n` is
        !! CLAMPED to the row count rather than checked, so a derived `n` needs no `min()` of its
        !! own. "The last n" is `descending=`, not a separate binding.
        !!
        !! Hands out row indices, so it carries `%argsort_by`'s staleness rule verbatim.
        module subroutine table_argsort_partial_{ik}(self, keys, perm, n, descending, nulls_first)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys(:)           !! key columns, primary first.
            {idecl}, allocatable, intent(out) :: perm(:) !! the first `n` 1-based row indices.
            integer, intent(in) :: n                          !! rows to order; clamped to %nrows().
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_argsort_partial_{ik}""")

    w("""        !> Whether the rows are ALREADY in the order `keys` describes.
        !!
        !! O(nrows) with an early exit and no permutation built, where asking `%sort_by` the same
        !! question costs a full sort. Same keys, same refusals, same null and NaN placement, so a
        !! `.true.` here means `%sort_by` with those arguments would move nothing.
        module function table_is_sorted_by(self, keys, descending, nulls_first) result(answer)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys(:)           !! key columns, primary first.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
            logical :: answer                                 !! .true. when already in that order.
        end function table_is_sorted_by
        !> Removes the listed rows (int32 indices). Repeats are harmless -- a row named twice is
        !! removed once. Row-structural, so it DETACHES; an empty index list removes nothing and
        !! does not.
        module subroutine table_delete_rows_i32(self, indices)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int32), intent(in) :: indices(:)    !! 1-based row indices to remove.
        end subroutine table_delete_rows_i32
        !> Removes the listed rows (int64 indices). Row-structural, so it DETACHES; an empty
        !! index list removes nothing and does not.
        module subroutine table_delete_rows_i64(self, indices)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int64), intent(in) :: indices(:)    !! 1-based row indices to remove.
        end subroutine table_delete_rows_i64
        !> Keeps only the first `n` rows (int32 count). `n` beyond the row count is a no-op and
        !! does not detach; 0 empties the table. Row-structural otherwise, so it DETACHES.
        module subroutine table_truncate_i32(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int32), intent(in) :: n             !! rows to keep.
        end subroutine table_truncate_i32
        !> Keeps only the first `n` rows (int64 count). Row-structural, so it DETACHES, unless
        !! `n` is at least the row count, which keeps every row and changes nothing.
        module subroutine table_truncate_i64(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int64), intent(in) :: n             !! rows to keep.
        end subroutine table_truncate_i64
        !> Appends every row of another table. `other`'s columns must be a SUBSET of this
        !! table's, with matching kinds, widths and units; a column this table has and `other`
        !! does not is filled with nulls. A column `other` has and this table does not is an
        !! error rather than being silently dropped. Row-structural, so it DETACHES -- but a
        !! zero-row `other` adds no row, so it is still checked for compatibility and then does
        !! nothing at all.
        module subroutine table_append_table(self, other)
            class(parquet_table), intent(inout) :: self !! the table to grow.
            class(parquet_table), intent(in) :: other   !! the table whose rows are appended.
        end subroutine table_append_table
        !> Appends one row, taken from a row handle on another (or the same) table. Convenient,
        !! but slow in bulk -- build a batch with `%clone_structure` and append that instead.
        !! Row-structural, so it DETACHES.
        module subroutine table_append_row(self, r)
            class(parquet_table), intent(inout) :: self !! the table to grow.
            type(parquet_table_row), intent(in) :: r    !! the row to append.
        end subroutine table_append_row
        !> Appends `n` all-null rows (int32 count) to every column, so they can be filled in
        !! afterwards. Row-structural, so it DETACHES; `n = 0` appends nothing and does not.
        module subroutine table_append_null_rows_i32(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int32), intent(in) :: n             !! rows to append.
        end subroutine table_append_null_rows_i32
        !> Appends `n` all-null rows (int64 count). Row-structural, so it DETACHES; `n = 0`
        !! appends nothing and does not.
        module subroutine table_append_null_rows_i64(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int64), intent(in) :: n             !! rows to append.
        end subroutine table_append_null_rows_i64
        !> The shared back half of every row-structural mutation: applies `keep` to every
        !! resident column, updates the row count, and detaches. Returns without touching
        !! anything when `keep` retains every row. Private to the implementation.
        module subroutine table_apply_keep(self, keep, proc)
            class(parquet_table), intent(inout) :: self !! the table.
            logical, intent(in) :: keep(:)              !! one entry per row; .true. to retain it.
            character(len=*), intent(in) :: proc        !! calling procedure, for messages.
        end subroutine table_apply_keep
        !> Cuts the table loose from its file: sets `detached`, releases the reader (it can never
        !! be read from again) and rewrites the row scope, since the surviving rows are no longer
        !! a contiguous range of file rows. Keeps `source_file`, which `%filename` and `%clone`
        !! still need.
        module subroutine table_detach(self)
            class(parquet_table), intent(inout) :: self !! the table.
        end subroutine table_detach
        !> Whether slot `idx` takes part in a row-structural mutation: it must hold values, so a
        !! column that was never read, or whose type this library cannot read at all, does not.
        !!
        !! Such a column is skipped rather than making the mutation an error, which is what makes
        !! it possible to filter or sort a table without first reading every column it has (the
        !! whole point of a lazy table). The cost is that the skipped column can never be read
        !! afterwards -- detaching sees to that -- so it is left behind deliberately, and the
        !! detach guard is what reports it if anyone reaches for it later.
        module function table_mutable_column(self, idx) result(ok)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: idx               !! slot index.
            logical :: ok                            !! .true. if the mutation applies to it.
        end function table_mutable_column
    end interface""")
    w("    !")
    w("""    ! ---- Copying a whole table (parquet_tables_clone) ----
    interface
        !> Makes `out` an independent deep copy of this table -- the way to keep a version to go
        !! back to, since mutation is in place and there is no undo.
        !!
        !! Columns already read are copied; columns not yet read stay unread, so a clone costs
        !! what the table actually holds rather than what its file contains. A live file-backed
        !! table's clone opens its OWN reader on the same file and stays lazy; a detached or
        !! in-memory table's clone has no reader, like its source.
        !!
        !! `out` must be declared as the same concrete type as `self`.
        module subroutine table_clone(self, out)
            class(parquet_table), intent(in) :: self  !! the table to copy.
            class(parquet_table), intent(out) :: out  !! receives the copy.
        end subroutine table_clone
        !> Makes `out` an EMPTY table with this table's columns: same names, kinds, widths and
        !! units, zero rows, no file behind it.
        !!
        !! This is the first half of the bulk-append idiom -- `%clone_structure` a batch, fill it
        !! with `%add_column`/`%set`, then `%append` it -- which is how a program adds many rows
        !! without appending one at a time. A batch made this way structurally cannot have the
        !! wrong column set, and abandoning a half-filled one is just a variable going out of
        !! scope. Columns whose type this library cannot read are left out.
        !!
        !! `resident_only=.true.` copies only the columns that have been read, which is also the
        !! way to clone the structure of a table whose other columns are still deferred.
        module subroutine table_clone_structure(self, out, resident_only)
            class(parquet_table), intent(in) :: self  !! the table to take the shape of.
            class(parquet_table), intent(out) :: out  !! receives the empty table.
            logical, intent(in), optional :: resident_only !! .true.: only columns already read.
        end subroutine table_clone_structure
        !> Copies the components an EXTENDING type added, which `%clone` cannot know about.
        !!
        !! `parquet_table` is designed to be extended -- a generated table type
        !! (`doc/pages/generated-tables.md`) does exactly that, and so may hand-written code. But
        !! `table_clone` only knows `parquet_table`'s own components, so anything the extension
        !! declared would arrive default-initialized and nothing would report it. Overriding this
        !! hook is how an extension copies its own state; `%clone` and `%clone_structure` each call
        !! it as their LAST action, dispatching on `self`, so the override runs for free wherever
        !! either is used.
        !!
        !! **A concrete-typed override of `%clone` itself is not possible** -- an overriding
        !! procedure must keep every dummy argument's characteristics, so `out` cannot be narrowed
        !! from `class(parquet_table)`. This hook is the supported substitute, and it keeps one name
        !! for one operation rather than adding a second spelling of "clone" that the first one
        !! silently gets wrong.
        !!
        !! The default implementation does nothing, which is correct for `parquet_table` itself.
        !! An override reaches `out`'s own components through `select type` -- use `class is`, not
        !! `type is`, so that a further extension still gets this level's copy. `%clone` has already
        !! checked that `self` and `out` have the same dynamic type, so the guarded branch always
        !! matches.
        module subroutine table_clone_extra(self, out, structure_only)
            class(parquet_table), intent(in) :: self    !! the table being copied.
            class(parquet_table), intent(inout) :: out  !! the copy, already holding the base state.
            logical, intent(in) :: structure_only       !! .true. when called from %clone_structure.
        end subroutine table_clone_extra
    end interface""")
    w("    !")
    w("""    ! ---- Predefined columns, for generated table types (parquet_tables_predefined) ----
    interface
        !> Binds the predefined columns a GENERATED table type declares: checks each one, converts
        !! it to the kind the schema declared, reads it, and marks the slot `predefined`.
        !!
        !! This is the one library call a generated type's `%init` makes
        !! (`doc/pages/generated-tables.md`); it is public only because a generated module is a
        !! DIFFERENT module and `parquet_table`'s components are private, so there is no other way
        !! in. Hand-written code rarely needs it -- a table opened with `parquet_open_table` already
        !! reaches every column by name.
        !!
        !! Per column, in `names` order:
        !!
        !! * a `from_file` column must exist (after any `remap:`), or this aborts naming it;
        !! * its width must equal the declared `widths` entry, or this aborts;
        !! * if its kind differs from the declared one, `%cast` converts it -- and a conversion that
        !!   can lose information (a narrowing, or an integer wider than the target real's mantissa)
        !!   emits a warning naming the table and column, since the declaration is a contract the
        !!   file does not have to honour exactly;
        !! * every `from_file` column is then read in ONE `%prefetch`, after the casts, so a
        !!   converted column decodes straight into its declared kind rather than being read twice;
        !! * a column with `from_file` `.false.` -- a `source: computed` field, or any column of a
        !!   from-scratch table -- is created with `%nrows()` all-null rows instead of being looked
        !!   for in the file.
        !!
        !! Passing `from_file` all `.false.` is exactly what an in-memory generated table does, so
        !! the same procedure serves a file-backed and a from-scratch construction.
        !!
        !! `units` FILLS IN a unit the column does not already have; it never overwrites one. A
        !! read-in MAML describes the physical file and is authoritative about what a file column
        !! holds, so a declaration must not override it -- but a computed column has no other
        !! source of a unit at all, and a file opened without a MAML has none either, which is
        !! where the declared one belongs.
        module subroutine table_bind_predefined(self, names, kinds, widths, from_file, context, exact, units)
            class(parquet_table), intent(inout) :: self   !! the table, already opened or created.
            character(len=*), intent(in) :: names(:)      !! internal column names, in declaration order.
            integer, intent(in) :: kinds(:)               !! declared PK_* kind per name.
            integer, intent(in) :: widths(:)              !! declared col_size per name (1 if scalar).
            logical, intent(in) :: from_file(:)           !! .false. for a computed/from-scratch column.
            character(len=*), intent(in), optional :: context !! schema name, for error messages.
            logical, intent(in), optional :: exact !! refuse a lossy kind conversion; forwarded
            !! verbatim to %cast, whose own default applies when this is absent.
            character(len=*), intent(in), optional :: units(:) !! declared unit per name ("" for none).
        end subroutine table_bind_predefined
    end interface""")
    w("    !")
    w("""    ! ---- Sort key extraction (parquet_tables_sort) ----
    interface
        !> Builds the 1-based row permutation `keys` implies, without applying it. Split out from
        !! `%sort_by` so the key extraction and the mutation can be reasoned about separately.
        module subroutine table_build_sort_permutation(self, keys, descending, nulls_first, perm)
            class(parquet_table), intent(in) :: self        !! the table.
            character(len=*), intent(in) :: keys(:)         !! key columns, primary first.
            logical, intent(in), optional :: descending(:)  !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:) !! per key: .true. to put nulls first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
        end subroutine table_build_sort_permutation
        !> Builds the 1-based indices of the `n` rows `keys` would put first, without applying them.
        !!
        !! Split out from `%top_n` for the same reason `table_build_sort_permutation` is split out
        !! from `%sort_by`, and for one more: `sort_collect_keys` and `sort_partial_check_n` are
        !! contained procedures of the `parquet_tables_sort` submodule, which the sibling submodule
        !! holding the mutation cannot reach.
        module subroutine table_build_top_n_permutation(self, keys, n, descending, nulls_first, perm)
            class(parquet_table), intent(in) :: self        !! the table.
            character(len=*), intent(in) :: keys(:)         !! key columns, primary first.
            integer, intent(in) :: n                        !! rows to select; clamped to the row count.
            logical, intent(in), optional :: descending(:)  !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:) !! per key: .true. to put nulls first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the selected rows, in key order.
        end subroutine table_build_top_n_permutation
    end interface""")
    w("    !")
    w("    ! ---- Row selection (parquet_tables_slice, and the per-kind copies in ..._access) ----")
    w("""    interface
        !> int32 form of parquet_slice_range -- see the generic interface above.
        module function slice_range_i32(start, stop, step) result(s)
            integer(int32), intent(in) :: start           !! first row (1-based).
            integer(int32), intent(in), optional :: stop  !! last row; default the table's last.
            integer(int32), intent(in), optional :: step  !! stride; default 1, never 0.
            type(parquet_slice) :: s                      !! the slice.
        end function slice_range_i32
        !> int64 form of parquet_slice_range -- see the generic interface above.
        module function slice_range_i64(start, stop, step) result(s)
            integer(int64), intent(in) :: start           !! first row (1-based).
            integer(int64), intent(in), optional :: stop  !! last row; default the table's last.
            integer(int64), intent(in), optional :: step  !! stride; default 1, never 0.
            type(parquet_slice) :: s                      !! the slice.
        end function slice_range_i64
        !> int32 form of parquet_slice_list -- see the generic interface above.
        module function slice_list_i32(indices) result(s)
            integer(int32), intent(in) :: indices(:)      !! 1-based row indices, in order.
            type(parquet_slice) :: s                      !! the slice.
        end function slice_list_i32
        !> int64 form of parquet_slice_list -- see the generic interface above.
        module function slice_list_i64(indices) result(s)
            integer(int64), intent(in) :: indices(:)      !! 1-based row indices, in order.
            type(parquet_slice) :: s                      !! the slice.
        end function slice_list_i64
        !> Turns a slice into the explicit list of rows it selects, validated against `nrows`.
        !! Every index must land inside 1..nrows and a zero step is rejected, so a caller of the
        !! per-kind copies can assume the list is safe to index with.
        module subroutine slice_resolve(s, nrows, rows, proc)
            type(parquet_slice), intent(in) :: s                  !! the slice.
            integer(int64), intent(in) :: nrows                   !! the table's row count.
            integer(int64), allocatable, intent(out) :: rows(:)   !! selected rows, in order.
            character(len=*), intent(in) :: proc                  !! caller, for the message.
        end subroutine slice_resolve
        !> Reports a sliced read whose column kind cannot be copied into the caller's array.
        module subroutine slice_kind_error(self, name, idx)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer, intent(in) :: idx               !! slot index.
        end subroutine slice_kind_error
    end interface""")
    w("    !")
    w("    ! ---- Row view (parquet_tables_row, and the per-kind getters in ..._access) ----")
    w("    interface")
    w("""        !> Builds a handle on row `i` (int32 index) -- see the %row generic.
        module function row_at_i32(self, i) result(r)
            class(parquet_table), intent(in), target :: self !! the table.
            integer(int32), intent(in) :: i                  !! 1-based row index.
            type(parquet_table_row) :: r                     !! the handle.
        end function row_at_i32
        !> Builds a handle on row `i` (int64 index) -- see the %row generic.
        module function row_at_i64(self, i) result(r)
            class(parquet_table), intent(in), target :: self !! the table.
            integer(int64), intent(in) :: i                  !! 1-based row index.
            type(parquet_table_row) :: r                     !! the handle.
        end function row_at_i64
        !> Whether this row is null in the named column.
        module function row_is_null(self, name) result(isnull)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            logical :: isnull                            !! .true. if this row is null there.
        end function row_is_null
        !> Whether element `e` of this row is null in a column.
        module function row_is_null_elem(self, name, e) result(isnull)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            logical :: isnull                            !! .true. if that element is null.
        end function row_is_null_elem
        !> This row's 1-based index within its table.
        pure module function row_index(self) result(i)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            integer(int64) :: i                          !! the row index.
        end function row_index
        !> Drops the store pointer. The handle owns nothing, so nothing is freed.
        module subroutine row_finalize(self)
            type(parquet_table_row), intent(inout) :: self !! the handle being destroyed.
        end subroutine row_finalize
        !> Resolves `name` for a row-handle access, triggering the same lazy first touch the
        !! table's own accessors do. Aborts on a missing, unsupported or unreadable column.
        module subroutine row_resolve(self, name, proc, idx)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
            integer, intent(out) :: idx                  !! slot index.
        end subroutine row_resolve
        !> Resolves `name` to its 1-based slot index in `cache`, or 0 when absent. The one place
        !! a name becomes an index, shared by the table and by a row handle.
        module function cache_find(cache, name) result(idx)
            type(parquet_table_cache), intent(in) :: cache !! the column store.
            character(len=*), intent(in) :: name           !! column name.
            integer :: idx                                 !! slot index, or 0.
        end function cache_find
        !> error stops unless slot `idx` holds exactly `kind` -- the row handle's counterpart of
        !! `table_require_kind`, for the string kinds, which have no widening to fall back on.
        module subroutine row_require_kind(self, name, idx, kind)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer, intent(in) :: idx                   !! slot index.
            integer, intent(in) :: kind                  !! required PK_* discriminator.
        end subroutine row_require_kind
        !> Reports a row read whose column kind cannot be copied into the caller's variable.
        module subroutine row_kind_error(self, name, idx)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer, intent(in) :: idx                   !! slot index.
        end subroutine row_kind_error""")
    for k in KINDS:
        w(rowget_iface(k))
    for k in KINDS:
        w(rowset_iface(k))
    for k in ARRAY_KINDS:
        w(rowref_iface(k))
    for k in ARRAY_KINDS:
        w(getslice_iface(k))
    w(getslice_str_iface())
    for k in ARRAY_KINDS:
        w(setslice_iface(k))
    w(setslice_str_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- Per-kind materialization (parquet_tables_materialize) ----")
    w("    interface")
    for k in KINDS:
        w(mat_iface(k))
    for k in KINDS:
        w(matchunk_iface(k))
    w("""        !> Dispatches one file column's read to the specific matching `kind`.
        module subroutine table_materialize_kind(kind, reader, name, col, nrows, wdt, unit)
            integer, intent(in) :: kind                  !! PK_* discriminator to read as.
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine table_materialize_kind
        !> Dispatches ONE ROW GROUP of a column to the specific matching `kind`. Same contract
        !! as `table_materialize_kind`, scoped to a single row group -- the primitive the slice
        !! regime assembles a column from.
        module subroutine table_materialize_chunk_kind(kind, reader, name, rg, col, nrows, wdt, unit)
            integer, intent(in) :: kind                  !! PK_* discriminator to read as.
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine table_materialize_chunk_kind""")
    w("    end interface")
    return "\n".join(o)


def ptr_iface(k):
    tag, pk, decl, comp, rank, cat = k
    mdims, mdoc = mask_dims(rank), valid_out_comment(rank)
    return f"""        !> Points `p` at a {pk} column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_{tag}(self, name, p, is_valid, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
{decl_line(12, f"{decl}, pointer, intent(out) :: p{dims(rank)}", "!! alias to the live storage.")}
            logical, allocatable, intent(out), optional :: is_valid{mdims} {mdoc}
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_{tag}"""


def ptr_str_iface():
    return """        !> Points `p` at a PK_STRING column's packed store (offsets + data + validity).
        !!
        !! The compact counterpart of the typed `%col` pointers, and the one pointer this layer
        !! hands out that a caller could use to change the column's SHAPE. Reading and in-place
        !! value edits are supported; appending to it, or otherwise changing how many elements it
        !! holds, is not -- the column's own row count is kept separately and would stop matching.
        module subroutine col_ptr_strcol(self, name, p, found)
            class(parquet_table), intent(in), target :: self          !! the table.
            character(len=*), intent(in) :: name                      !! column name.
            type(parquet_string_column), pointer, intent(out) :: p    !! alias to the packed store.
            logical, intent(out), optional :: found                   !! present: report a miss instead of aborting.
        end subroutine col_ptr_strcol"""


def get_iface(k):
    tag, pk, decl, comp, rank, cat = k
    widen = ""
    if tag in WIDEN:
        srcs = ", ".join(p for p, _ in WIDEN[tag])
        widen = f"\n        !! Also accepts a {srcs} column, widening on the way."
    mdims, mdoc = mask_dims(rank), valid_out_comment(rank)
    return f"""        !> Copies a {pk} column out into a freshly allocated array.{widen}
        module subroutine get_arr_{tag}(self, name, arr, is_valid, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
{decl_line(12, f"{decl}, allocatable, intent(out) :: arr{dims(rank)}", f"!! {shape_comment(rank)}.")}
            logical, allocatable, intent(out), optional :: is_valid{mdims} {mdoc}
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_{tag}"""


def get_str_iface():
    return """        !> Copies a PK_STRING column out as a parquet_string_column (offsets+data+validity).
        module subroutine get_arr_str(self, name, arr, is_valid, found)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            type(parquet_string_column), intent(inout) :: arr      !! cleared, then filled.
            logical, allocatable, intent(out), optional :: is_valid(:) !! present: per-row validity, .true. = value.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine get_arr_str
        !> Copies a PK_STRING column out as a fixed-width character array, sized to the longest
        !! element present. A null element comes back blank -- gate on %is_null to tell a null
        !! from a genuinely empty string.
        module subroutine get_arr_chr(self, name, arr, is_valid, found)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            character(len=:), allocatable, intent(out) :: arr(:)   !! one value per row.
            logical, allocatable, intent(out), optional :: is_valid(:) !! present: per-row validity, .true. = value.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine get_arr_chr
        !> Copies a PK_STRING_VEC column out as a fixed-width character (element, row) array.
        module subroutine get_arr_chrv(self, name, arr, is_valid, found)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            character(len=:), allocatable, intent(out) :: arr(:,:) !! (element, row) values.
            logical, allocatable, intent(out), optional :: is_valid(:,:) !! present: per-element validity, (width, nrows).
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine get_arr_chrv"""


def set_iface(k):
    tag, pk, decl, comp, rank, cat = k
    mdims, mdoc = mask_dims(rank), valid_in_comment(rank)
    return f"""        !> Replaces every value of a {pk} column. The array must have the column's own shape.
        module subroutine set_arr_{tag}(self, name, arr, is_valid, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
{decl_line(12, f"{decl}, intent(in) :: arr{dims(rank)}", f"!! {shape_comment(rank)}.")}
            logical, intent(in), optional :: is_valid{mdims} {mdoc}
            logical, intent(in), optional :: modify_nulls !! .false. leaves null entries untouched.
        end subroutine set_arr_{tag}"""


def set_str_iface():
    return """        !> Replaces every value of a PK_STRING column from a character array. **Trailing blanks
        !! are trimmed** -- every element of a `character(len=*)` array shares one declared length,
        !! so a shorter value is blank-padded by Fortran and those blanks carry nothing the caller
        !! could have meant. `%set_element`, which takes a scalar, stores its value verbatim.
        module subroutine set_arr_chr(self, name, arr, is_valid, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: arr(:)       !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! present: rows marked .false. become null.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_chr
        !> Replaces every value of a PK_STRING_VEC column from a character (element, row) array.
        !! Trailing blanks are trimmed, as in the rank-1 form above.
        module subroutine set_arr_chrv(self, name, arr, is_valid, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: arr(:,:)     !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! present: elements marked .false. become null.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_chrv"""


def setslice_iface(k):
    tag, pk, decl, comp, rank, cat = k
    mdims = mask_dims(rank)
    mdoc = mask_comment(rank, "!! present: selected rows marked .false. become null.",
                        "!! present: selected elements marked .false. become null.")
    return f"""        !> Writes `arr` into the rows `s` selects of a {pk} column.
        !!
        !! The mirror of %get_slice: same selection object, same order, and the array must have
        !! exactly one value (or one vector) per selected row. The kind must match EXACTLY, as it
        !! does for %set -- a copy INTO the table never widens.
        module subroutine set_slice_{tag}(self, name, s, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to write.
            {decl}, intent(in) :: arr{dims(rank)}{' ' * max(1, 21 - len(decl))}!! one value per selected row.
            logical, intent(in), optional :: is_valid{mdims} {mdoc}
            logical, intent(in), optional :: modify_nulls !! .false. leaves a selected null entry null.
            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine set_slice_{tag}"""


def setslice_str_iface():
    return """        !> Writes a character array into the rows `s` selects of a PK_STRING column. Trailing
        !! blanks are trimmed, as in `%set`.
        module subroutine set_slice_chr(self, name, s, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to write.
            character(len=*), intent(in) :: arr(:)       !! one value per selected row.
            logical, intent(in), optional :: is_valid(:) !! present: selected rows marked .false. become null.
            logical, intent(in), optional :: modify_nulls !! .false. leaves a selected null row null.
            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine set_slice_chr
        !> Writes a character (element, row) array into the rows `s` selects of a PK_STRING_VEC column.
        module subroutine set_slice_chrv(self, name, s, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to write.
            character(len=*), intent(in) :: arr(:,:)     !! (element, selected row) values.
            logical, intent(in), optional :: is_valid(:,:) !! present: selected elements marked .false. become null.
            logical, intent(in), optional :: modify_nulls !! .false. leaves a selected null row null.
            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine set_slice_chrv"""


def set_strcol_iface():
    return """        !> Replaces every value of a PK_STRING column from a compact parquet_string_column.
        !!
        !! The counterpart of `%get(name, packed)`: an independent copy is taken, so the caller's
        !! own column and the table's do not share storage afterwards. The row count must match,
        !! exactly as it must for the character-array form.
        module subroutine set_arr_strcol(self, name, arr, is_valid, found)
            class(parquet_table), intent(inout) :: self         !! the table.
            character(len=*), intent(in) :: name                !! column name.
            type(parquet_string_column), intent(in) :: arr      !! one value per row.
            logical, intent(in), optional :: is_valid(:)        !! present: rows marked .false. become null.
            logical, intent(out), optional :: found             !! present: report a miss instead of aborting.
        end subroutine set_arr_strcol"""


def add_strcol_iface():
    return """        !> Appends a new PK_STRING column holding a compact parquet_string_column's values.
        !!
        !! Unlike the character-array form nothing is trimmed: a parquet_string_column already
        !! stores each value at its own length, which is the reason to build one.
        module subroutine add_column_strcol(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self       !! the table.
            character(len=*), intent(in) :: name              !! the new column's name.
            type(parquet_string_column), intent(in) :: values !! one value per row.
            character(len=*), intent(in), optional :: unit    !! unit string to store.
            logical, intent(in), optional :: force            !! .true. replaces an existing same-named column.
        end subroutine add_column_strcol"""


def getelem_iface(k):
    """One row's value, by row index, for one column -- %set_element's read counterpart."""
    tag, pk, decl, comp, rank, cat = k
    widen_note = ""
    if tag in WIDEN:
        widen_note = ("\n        !! Also accepts a "
                      + ", ".join(w[0] for w in WIDEN[tag]) + " column, widening on the way out.")
    out = []
    for ik, ityp in (("i32", "integer(int32)"), ("i64", "integer(int64)")):
        if rank == 1:
            val = f"{decl}, intent(out) :: value"
            what = "receives the value"
        else:
            val = f"{decl}, allocatable, intent(out) :: value(:)"
            what = "receives that row's width values"
        out.append(f"""        !> Reads one row of a {pk} column ({ik} row index).{widen_note}
        module subroutine get_element_{tag}_{ik}(self, name, i, value, found)
            class(parquet_table), intent(in) :: self    !! the table (fills through %cache).
            character(len=*), intent(in) :: name        !! column name.
{decl_line(12, f"{ityp}, intent(in) :: i", "!! 1-based row index.")}
{decl_line(12, val, f"!! {what}.")}
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine get_element_{tag}_{ik}""")
    return "\n".join(out)


def getelem_str_iface():
    out = []
    for ik, ityp in (("i32", "integer(int32)"), ("i64", "integer(int64)")):
        out.append(f"""        !> Reads one row of a PK_STRING column into an allocatable character ({ik} row index).
        module subroutine get_element_chr_{ik}(self, name, i, value, found)
            class(parquet_table), intent(in) :: self             !! the table (fills through %cache).
            character(len=*), intent(in) :: name                 !! column name.
{decl_line(12, f"{ityp}, intent(in) :: i", "!! 1-based row index.")}
            character(len=:), allocatable, intent(out) :: value  !! receives the value ("" when null).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_element_chr_{ik}""")
        out.append(f"""        !> Reads one row of a PK_STRING_VEC column, one array element per position ({ik} row index).
        module subroutine get_element_chrv_{ik}(self, name, i, value, found)
            class(parquet_table), intent(in) :: self                !! the table (fills through %cache).
            character(len=*), intent(in) :: name                    !! column name.
{decl_line(12, f"{ityp}, intent(in) :: i", "!! 1-based row index.")}
            character(len=:), allocatable, intent(out) :: value(:)  !! receives width values.
            logical, intent(out), optional :: found                 !! present: report a miss instead of aborting.
        end subroutine get_element_chrv_{ik}""")
    return "\n".join(out)


def setelem_iface(k):
    tag, pk, decl, comp, rank, cat = k
    val = f"{decl}, intent(in) :: value" + ("(:)" if rank == 2 else "")
    what = "that row's whole vector" if rank == 2 else "the new value"
    out = []
    for ik, ityp in (("i32", "integer(int32)"), ("i64", "integer(int64)")):
        out.append(f"""        !> Writes one row of a {pk} column ({ik} row index).
        module subroutine set_element_{tag}_{ik}(self, name, i, value, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
{decl_line(12, f"{ityp}, intent(in) :: i", "!! 1-based row index.")}
{decl_line(12, val, f"!! {what}.")}
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_element_{tag}_{ik}""")
    return "\n".join(out)


def setelem_str_iface():
    out = []
    for tag, pk, val, what in (
        ("chr", "PK_STRING", "character(len=*), intent(in) :: value", "the new value"),
        ("chrv", "PK_STRING_VEC", "character(len=*), intent(in) :: value(:)", "that row's whole vector"),
    ):
        for ik, ityp in (("i32", "integer(int32)"), ("i64", "integer(int64)")):
            out.append(f"""        !> Writes one row of a {pk} column from a character value ({ik} row index).
        module subroutine set_element_{tag}_{ik}(self, name, i, value, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
{decl_line(12, f"{ityp}, intent(in) :: i", "!! 1-based row index.")}
{decl_line(12, val, f"!! {what}.")}
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine set_element_{tag}_{ik}""")
    return "\n".join(out)


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


def rowset_iface(k):
    """Write one row's value for one column, through a row handle."""
    tag, pk, decl, comp, rank, cat = k
    if cat == "str":
        if rank == 1:
            return f"""        !> Writes this row's string value into a {pk} column.
        module subroutine row_set_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: value        !! the new value.
        end subroutine row_set_{tag}"""
        return f"""        !> Writes this row's whole string vector into a {pk} column.
        module subroutine row_set_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: value(:)     !! width values.
        end subroutine row_set_{tag}"""
    val = f"{decl}, intent(in) :: value" + ("(:)" if rank == 2 else "")
    what = "that row's whole vector" if rank == 2 else "the new value"
    return f"""        !> Writes this row's value into a {pk} column.
        module subroutine row_set_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
{decl_line(12, val, f"!! {what}.")}
        end subroutine row_set_{tag}"""


def rowref_iface(k):
    """A zero-copy pointer to one row's storage in one column."""
    tag, pk, decl, comp, rank, cat = k
    ptr = f"{decl}, pointer, intent(out) :: p" + ("(:)" if rank == 2 else "")
    what = "alias to this row's vector" if rank == 2 else "alias to this row's value"
    return f"""        !> Points `p` at this row's storage in a {pk} column. The kind must match EXACTLY.
        module subroutine row_ref_{tag}(self, name, p)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
{decl_line(12, ptr, f"!! {what}.")}
        end subroutine row_ref_{tag}"""


def rowget_iface(k):
    """One row's value for one column: a scalar for a scalar kind, a width-long array for a
    vector kind, and an allocatable character for either string kind."""
    tag, pk, decl, comp, rank, cat = k
    # The widening note goes on its own continuation line: appended to the summary line it
    # pushes the longer vector kinds past the 132-column limit (CLAUDE.md).
    widen_note = ""
    if tag in WIDEN:
        widen_note = ("\n        !! Also accepts a "
                      + ", ".join(w[0] for w in WIDEN[tag]) + " column, widening on the way out.")
    if cat == "str":
        if rank == 1:
            return f"""        !> This row's string value from a {pk} column.{widen_note}
        module subroutine row_get_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self          !! the row handle.
            character(len=*), intent(in) :: name                  !! column name.
            character(len=:), allocatable, intent(out) :: value   !! receives the value.
        end subroutine row_get_{tag}"""
        return f"""        !> This row's string vector from a {pk} column, one array element per position.
        module subroutine row_get_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self             !! the row handle.
            character(len=*), intent(in) :: name                     !! column name.
            character(len=:), allocatable, intent(out) :: value(:)   !! receives width values.
        end subroutine row_get_{tag}"""
    if rank == 1:
        return f"""        !> This row's value from a {pk} column.{widen_note}
        module subroutine row_get_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            {decl}, intent(out) :: value{' ' * max(1, 24 - len(decl))}!! receives the value.
        end subroutine row_get_{tag}"""
    return f"""        !> This row's vector from a {pk} column, one array element per position.{widen_note}
        module subroutine row_get_{tag}(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            {decl}, allocatable, intent(out) :: value(:){' ' * max(1, 8 - len(decl))}!! receives width values.
        end subroutine row_get_{tag}"""


def getslice_iface(k):
    tag, pk, decl, comp, rank, cat = k
    widen_note = ""
    if tag in WIDEN:
        widen_note = ("\n        !! Also accepts a "
                      + ", ".join(w[0] for w in WIDEN[tag]) + " column, widening on the way out.")
    mdims = mask_dims(rank)
    mdoc = mask_comment(rank, "!! present: validity of the selected rows.",
                        "!! present: per-element validity of the picked rows.")
    return f"""        !> Copies the rows `s` selects from a {pk} column into `arr`.{widen_note}
        module subroutine get_slice_{tag}(self, name, s, arr, is_valid, found)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            {decl}, allocatable, intent(out) :: arr{dims(rank)}{' ' * max(1, 8 - len(decl))}!! {shape_comment(rank)}.
            logical, allocatable, intent(out), optional :: is_valid{mdims} {mdoc}\n            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine get_slice_{tag}"""


def getslice_str_iface():
    return """        !> Copies the rows `s` selects from a PK_STRING column into a compact string column.
        module subroutine get_slice_str(self, name, s, arr, is_valid, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_slice), intent(in) :: s                 !! rows to pick.
            type(parquet_string_column), intent(out) :: arr      !! the selected elements.
            logical, allocatable, intent(out), optional :: is_valid(:) !! present: validity of the selected rows.
            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine get_slice_str
        !> Copies the rows `s` selects from a PK_STRING column into a character array, sized to
        !! the longest element selected.
        module subroutine get_slice_chr(self, name, s, arr, is_valid, found)
            class(parquet_table), intent(in) :: self                 !! the table.
            character(len=*), intent(in) :: name                     !! column name.
            type(parquet_slice), intent(in) :: s                     !! rows to pick.
            character(len=:), allocatable, intent(out) :: arr(:)     !! one value per selected row.
            logical, allocatable, intent(out), optional :: is_valid(:) !! present: validity of the selected rows.
            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine get_slice_chr
        !> Copies the rows `s` selects from a PK_STRING_VEC column, shaped (width, selected).
        module subroutine get_slice_chrv(self, name, s, arr, is_valid, found)
            class(parquet_table), intent(in) :: self                 !! the table.
            character(len=*), intent(in) :: name                     !! column name.
            type(parquet_slice), intent(in) :: s                     !! rows to pick.
            character(len=:), allocatable, intent(out) :: arr(:,:)   !! (element, selected row).
            logical, allocatable, intent(out), optional :: is_valid(:,:) !! present: per-element validity of the picked rows.
            logical, intent(out), optional :: found      !! present: report a miss instead of aborting.
        end subroutine get_slice_chrv"""


def matchunk_iface(k):
    tag, pk, decl, comp, rank, cat = k
    return f"""        !> Reads ONE ROW GROUP of a {pk} file column into `col`, carrying its nulls across.
        module subroutine matchunk_{tag}(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_{tag}"""


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
            ! table_classify only ever assigns a slot one of the supported PK_* kinds handled
            ! above (an unsupported column stays PK_NONE and is never routed to a materializer),
            ! so this branch guards an internal invariant with no path reachable through the
            ! public API -- there is no way to feed it a value that would actually take it.
            error stop EP // "internal: no materializer for this column kind" ! GCOVR_EXCL_LINE
        end select
    end procedure table_materialize_kind
    !""")
    w("""    module procedure table_materialize_chunk_kind
        select case (kind)""")
    for k in KINDS:
        tag, pk = k[0], k[1]
        w(f"        case ({pk})")
        w(f"            call matchunk_{tag}(reader, name, rg, col, nrows, wdt, unit)")
    w("""        case default
            ! Same internal invariant as table_materialize_kind's own case default above -- not
            ! reachable through the public API.
            error stop EP // "internal: no row-group materializer for this column kind" ! GCOVR_EXCL_LINE
        end select
    end procedure table_materialize_chunk_kind
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
    w(ptr_str_impl())
    w(set_strcol_impl())
    w(stat_dispatch())
    for k in KINDS:
        w(stat_impl(k))
    for k in ARRAY_KINDS:
        w(get_impl(k))
    w(get_str_impl())
    for k in ARRAY_KINDS:
        w(set_impl(k))
    w(set_str_impl())
    for k in ARRAY_KINDS:
        w(getelem_impl(k))
    w(getelem_str_impl())
    for k in ARRAY_KINDS:
        w(setelem_impl(k))
    w(setelem_str_impl())
    for k in KINDS:
        w(rowget_impl(k))
    for k in KINDS:
        w(rowset_impl(k))
    for k in ARRAY_KINDS:
        w(rowref_impl(k))
    for k in ARRAY_KINDS:
        w(getslice_impl(k))
    w(getslice_str_impl())
    for k in ARRAY_KINDS:
        w(setslice_impl(k))
    w(setslice_str_impl())
    w("end submodule parquet_tables_access ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


def getslice_impl(k):
    """Gather the selected rows one at a time through get_at.

    A row-by-row gather rather than an array section, because a slice may be strided, reversed
    or an arbitrary list -- none of which is a contiguous section of the store.
    """
    tag, pk, decl, comp, rank, cat = k
    msuf = "" if rank == 1 else "_elem"
    lines = [f"    module procedure get_slice_{tag}",
             "        integer :: idx",
             "        integer(int64) :: k",
             "        integer(int64), allocatable :: rows(:)"]
    for _, src in WIDEN.get(tag, []):
        srcdecl = next(kk[2] for kk in KINDS if kk[0] == src)
        if rank == 1:
            lines.append(f"        {srcdecl} :: v_{src}")
        else:
            lines.append(f"        {srcdecl}, allocatable :: v_{src}(:)")
    lines += ["        !",
              '        call table_resolve(self, name, "get_slice", idx, found)',
              "        if (idx == 0) then",
              f"            allocate(arr{'(0)' if rank == 1 else '(0,0)'})",
              f"            if (present(is_valid)) allocate(is_valid{'(0)' if rank == 1 else '(0,0)'})",
              "            return",
              "        end if",
              '        call slice_resolve(s, self%row_count, rows, "get_slice")',
              f"        if (present(is_valid)) call table_valid_mask_rows{msuf}(self%cache, idx, rows, is_valid)",
              "        select case (self%cache%cols(idx)%declared_kind)",
              f"        case ({pk})"]
    if rank == 1:
        lines += ["            allocate(arr(size(rows)))",
                  "            do k = 1, size(rows, kind=int64)",
                  "                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))",
                  "            end do"]
    else:
        lines += ["            allocate(arr(self%cache%cols(idx)%width, size(rows)))",
                  "            do k = 1, size(rows, kind=int64)",
                  "                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))",
                  "            end do"]
    for srcpk, src in WIDEN.get(tag, []):
        lines.append(f"        case ({srcpk})")
        if rank == 1:
            lines += ["            allocate(arr(size(rows)))",
                      "            do k = 1, size(rows, kind=int64)",
                      f"                call self%cache%cols(idx)%values%get_at(rows(k), v_{src})",
                      f"                arr(k) = v_{src}",
                      "            end do"]
        else:
            lines += ["            allocate(arr(self%cache%cols(idx)%width, size(rows)))",
                      f"            allocate(v_{src}(self%cache%cols(idx)%width))",
                      "            do k = 1, size(rows, kind=int64)",
                      f"                call self%cache%cols(idx)%values%get_at(rows(k), v_{src})",
                      f"                arr(:, k) = v_{src}",
                      "            end do"]
    lines += ["        case default",
              "            call slice_kind_error(self, name, idx)",
              "        end select",
              f"    end procedure get_slice_{tag}",
              "    !"]
    return "\n".join(lines)


def setslice_impl(k):
    """Scatter the given values into the selected rows, one at a time through set_at.

    Row by row for the same reason %get_slice gathers row by row: a selection may be strided,
    reversed or an arbitrary list, none of which is a contiguous section of the store.
    """
    tag, pk, decl, comp, rank, cat = k
    n_arr = "size(arr, kind=int64)" if rank == 1 else "size(arr, 2, kind=int64)"
    val = "arr(k)" if rank == 1 else "arr(:, k)"
    msuf = "" if rank == 1 else "_elem"
    return f"""    module procedure set_slice_{tag}
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, {pk}, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, {n_arr}, size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), {val}, modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows{msuf}(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_{tag}
    !"""


def setslice_str_impl():
    out = []
    for tag, pk, val, n_arr, msuf in (("chr", "PK_STRING", "arr(k)", "size(arr, kind=int64)", ""),
                                      ("chrv", "PK_STRING_VEC", "arr(:, k)", "size(arr, 2, kind=int64)", "_elem")):
        out.append(f"""    module procedure set_slice_{tag}
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, {pk}, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, {n_arr}, size(rows, kind=int64), name, "set_slice")
        ! One row at a time through set_at, which the string store supports in place -- unlike
        ! %paste, which cannot overwrite a packed variable-length store's range wholesale.
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), {val}, modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows{msuf}(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_{tag}
    !""")
    return "\n".join(out)


def getslice_str_impl():
    return """    module procedure get_slice_str
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call self%cache%cols(idx)%values%string_column(store)
        ! Built element by element rather than copied and trimmed: a gather has no contiguous
        ! source range to clone from, and appending keeps the result compact.
        do k = 1, size(rows, kind=int64)
            if (store%is_null(rows(k))) then
                call arr%append_null()
            else
                call store%get(rows(k), sv)
                call arr%append_string(sv)
            end if
        end do
    end procedure get_slice_str
    !
    module procedure get_slice_chr
        integer :: idx, maxlen
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call self%cache%cols(idx)%values%string_column(store)
        ! Two passes: a fixed-length array's width must be the longest element SELECTED, which
        ! is not known until every selected row has been looked at.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            call store%get(rows(k), sv, allow_null=.true.)
            if (len(sv) > maxlen) maxlen = len(sv)
        end do
        allocate(character(len=maxlen) :: arr(size(rows)))
        do k = 1, size(rows, kind=int64)
            call store%get(rows(k), sv, allow_null=.true.)
            arr(k) = sv
        end do
    end procedure get_slice_chr
    !
    module procedure get_slice_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: k, flat
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        wdt = self%cache%cols(idx)%width
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, sv, allow_null=.true.)
                if (len(sv) > maxlen) maxlen = len(sv)
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, size(rows)))
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, sv, allow_null=.true.)
                arr(e, k) = sv
            end do
        end do
    end procedure get_slice_chrv
    !"""


def rowset_impl(k):
    """Write one row's value through a handle: exact kind, and it changes the TABLE.

    Exact-kind rather than widening, unlike row_get: widening is a rule for copying OUT into the
    caller's variable, while a write would have to change what the column stores.
    """
    tag, pk, decl, comp, rank, cat = k
    return f"""    module procedure row_set_{tag}
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, {pk})
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_{tag}
    !"""


def rowref_impl(k):
    """A pointer to one row's storage: the whole-column pointer, narrowed to this row."""
    tag, pk, decl, comp, rank, cat = k
    if rank == 1:
        body = """        call self%cache%cols(idx)%values%data_ptr(store)
        p => store(self%irow)"""
        decl_store = f"        {decl}, pointer :: store(:)"
    else:
        body = """        call self%cache%cols(idx)%values%data_ptr(store)
        p => store(:, self%irow)"""
        decl_store = f"        {decl}, pointer :: store(:,:)"
    return f"""    module procedure row_ref_{tag}
        integer :: idx
{decl_store}
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, {pk})
{body}
    end procedure row_ref_{tag}
    !"""


def rowget_impl(k):
    """One row's value for one column, widening exactly as the table's own %get does.

    Widening is a `select case` inside each specific rather than extra specifics: generic
    resolution picks the specific from the CALLER's variable, and the stored kind is only known
    at run time.
    """
    tag, pk, decl, comp, rank, cat = k
    if cat == "str":
        if rank == 1:
            return """    module procedure row_get_str
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING)
        call self%cache%cols(idx)%values%string_column(store)
        ! allow_null keeps a null row from aborting: it reads back as "", and %is_null is how a
        ! caller tells the two apart.
        call store%get(self%irow, value, allow_null=.true.)
    end procedure row_get_str
    !"""
        return """    module procedure row_get_strv
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING_VEC)
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, i) at
        ! (i-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%get(flat, s, allow_null=.true.)
            if (len(s) > maxlen) maxlen = len(s)
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%get(flat, s, allow_null=.true.)
            value(e) = s
        end do
    end procedure row_get_strv
    !"""
    lines = [f"    module procedure row_get_{tag}", "        integer :: idx"]
    for _, src in WIDEN.get(tag, []):
        srcdecl = next(kk[2] for kk in KINDS if kk[0] == src)
        if rank == 1:
            lines.append(f"        {srcdecl} :: v_{src}")
        else:
            lines.append(f"        {srcdecl}, allocatable :: v_{src}(:)")
    lines += ["        !",
              '        call row_resolve(self, name, "get", idx)',
              "        select case (self%cache%cols(idx)%declared_kind)",
              f"        case ({pk})"]
    if rank == 1:
        lines.append("            call self%cache%cols(idx)%values%get_at(self%irow, value)")
    else:
        lines += ["            allocate(value(self%cache%cols(idx)%width))",
                  "            call self%cache%cols(idx)%values%get_at(self%irow, value)"]
    for srcpk, src in WIDEN.get(tag, []):
        lines.append(f"        case ({srcpk})")
        if rank == 1:
            lines += [f"            call self%cache%cols(idx)%values%get_at(self%irow, v_{src})",
                      f"            value = v_{src}"]
        else:
            lines += [f"            allocate(v_{src}(self%cache%cols(idx)%width))",
                      f"            call self%cache%cols(idx)%values%get_at(self%irow, v_{src})",
                      "            allocate(value(self%cache%cols(idx)%width))",
                      f"            value = v_{src}"]
    lines += ["        case default",
              "            call row_kind_error(self, name, idx)",
              "        end select",
              f"    end procedure row_get_{tag}",
              "    !"]
    return "\n".join(lines)


def ptr_impl(k):
    tag, pk, decl, comp, rank, cat = k
    msuf = "" if rank == 1 else "_elem"
    mzero = "(0)" if rank == 1 else "(0,0)"
    return f"""    module procedure col_ptr_{tag}
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid{mzero})
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of{msuf}(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= {pk}) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_{tag}
    !"""


def ptr_str_impl():
    return """    module procedure col_ptr_strcol
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "col")
        call self%cache%cols(idx)%values%string_column(p)
    end procedure col_ptr_strcol
    !"""


def set_strcol_impl():
    return """    module procedure set_arr_strcol
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, arr%size(), "set")
        ! Replaces the packed store wholesale with an independent copy, so the caller's own column
        ! and the table's do not end up sharing storage. %set is a value replacement, exactly as
        ! the character-array form is; it is not a way to hand ownership over.
        call self%cache%cols(idx)%values%string_column(store)
        store = arr%clone()
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_strcol
    !"""


def add_strcol_impl():
    return """    module procedure add_column_strcol
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, values%size())
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_STRING, values%size(), 1_int32, unit)
        call self%cache%cols(idx)%values%string_column(store)
        store = values%clone()
        self%cache%cols(idx)%declared_kind = PK_STRING
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_strcol
    !"""


NUMFMT = {
    "i32": "(I0)", "i64": "(I0)", "f32": "(G0.6)", "f64": "(G0.6)",
}


def stat_impl(k):
    """One kind's min/max, as text, over the rows that are not null.

    A statistic is over the VALUES: a null row contributes nothing, and the null count is
    reported separately. That is the sort engine's rule too, so the library has one story about
    where nulls sit rather than two. An all-null column has no min or max and prints "-".

    A vector kind's statistic is over ALL of its elements, flattened -- the same convention the
    reader's own print_stat uses for a vector column's null count.
    """
    tag, pk, decl, comp, rank, cat = k
    if cat == "num" and tag.startswith("bool"):
        # A logical column's useful summary is how many of each, not an ordering.
        inner = ("                do e = 1, size(p, 1)\n"
                 "                    if (p(e, i)) then\n"
                 "                        nt = nt + 1\n"
                 "                    else\n"
                 "                        nf = nf + 1\n"
                 "                    end if\n"
                 "                end do") if rank == 2 else (
                 "                if (p(i)) then\n"
                 "                    nt = nt + 1\n"
                 "                else\n"
                 "                    nf = nf + 1\n"
                 "                end if")
        bool_edecl = "        integer :: e\n" if rank == 2 else ""
        return f"""    !> {pk}: true/false counts rather than an ordering.
    subroutine stat_{tag}(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! "T:<n>".
        character(len=:), allocatable, intent(out) :: max_s    !! "F:<n>".
        {decl}, pointer :: p{dims(rank)}
        integer(int64) :: i, nt, nf
{bool_edecl}        character(len=32) :: buf
        !
        nt = 0_int64
        nf = 0_int64
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
{inner}
        end do
        write(buf, "(I0)") nt
        min_s = "T:" // trim(buf)
        write(buf, "(I0)") nf
        max_s = "F:" // trim(buf)
    end subroutine stat_{tag}
"""
    if cat == "num":
        fmt = NUMFMT[tag.replace("v", "") if tag.endswith("v") else tag]
        if rank == 1:
            body = """                if (first) then
                    mn = p(i)
                    mx = p(i)
                    first = .false.
                else
                    mn = min(mn, p(i))
                    mx = max(mx, p(i))
                end if"""
        else:
            body = """                do e = 1, size(p, 1)
                    if (first) then
                        mn = p(e, i)
                        mx = p(e, i)
                        first = .false.
                    else
                        mn = min(mn, p(e, i))
                        mx = max(mx, p(e, i))
                    end if
                end do"""
        edecl = "        integer :: e\n" if rank == 2 else ""
        return f"""    !> {pk}: smallest and largest value, over the rows that hold one.
    subroutine stat_{tag}(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        {decl}, pointer :: p{dims(rank)}
        {decl} :: mn, mx
        integer(int64) :: i
{edecl}        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
{body}
        end do
        if (first) return
        write(buf, "{fmt}") mn
        min_s = trim(adjustl(buf))
        write(buf, "{fmt}") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_{tag}
"""
    if cat == "tmp":
        if rank == 1:
            body = """                if (first) then
                    mn = p(i)
                    mx = p(i)
                    first = .false.
                else
                    if (p(i) < mn) mn = p(i)
                    if (mx < p(i)) mx = p(i)
                end if"""
        else:
            body = """                do e = 1, size(p, 1)
                    if (p(e, i)%is_null()) cycle
                    if (first) then
                        mn = p(e, i)
                        mx = p(e, i)
                        first = .false.
                    else
                        if (p(e, i) < mn) mn = p(e, i)
                        if (mx < p(e, i)) mx = p(e, i)
                    end if
                end do"""
        edecl = "        integer :: e\n" if rank == 2 else ""
        # A temporal element carries its own null, so a row that is not null can still hold one
        # on the scalar path -- checked before the value is used, since < aborts on a null.
        guard = ("            if (p(i)%is_null()) cycle\n" if rank == 1 else "")
        return f"""    !> {pk}: earliest and latest value, in ISO-8601 form.
    subroutine stat_{tag}(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        {decl}, pointer :: p{dims(rank)}
        {decl} :: mn, mx
        integer(int64) :: i
{edecl}        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
{guard}{body}
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_{tag}
"""
    # string kinds: lexicographic, over the flat store
    n_elems = "values%length()" if rank == 1 else "values%length() * int(values%colwidth(), int64)"
    return f"""    !> {pk}: lexicographically smallest and largest value.
    subroutine stat_{tag}(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        type(parquet_string_column), pointer :: store
        character(len=:), allocatable :: sv
        integer(int64) :: i, n
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%string_column(store)
        n = {n_elems}
        do i = 1_int64, n
            if (store%is_null(i)) cycle
            call store%get(i, sv)
            ! Trimmed for display only: a vector string column stores its values blank-padded to
            ! the widest element, and printing that padding says nothing. Fortran's own comparison
            ! blank-pads the shorter operand anyway, so trimming cannot change which value wins.
            sv = trim(sv)
            if (first) then
                min_s = sv
                max_s = sv
                first = .false.
            else
                if (sv < min_s) min_s = sv
                if (max_s < sv) max_s = sv
            end if
        end do
    end subroutine stat_{tag}
"""


def stat_dispatch():
    arms = []
    for k in KINDS:
        arms.append(f"        case ({k[1]})\n            call stat_{k[0]}(values, min_s, max_s)")
    return """    module procedure table_column_stat_text
        min_s = "-"
        max_s = "-"
        select case (values%kindof())
""" + "\n".join(arms) + """
        case default
            ! PK_NONE, and the reserved container kinds: nothing to summarize.
            return
        end select
    end procedure table_column_stat_text
    !"""


def get_impl(k):
    tag, pk, decl, comp, rank, cat = k
    msuf = "" if rank == 1 else "_elem"
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
              f"            if (present(is_valid)) allocate(is_valid{'(0)' if rank == 1 else '(0,0)'})",
              "            return",
              "        end if",
              f"        if (present(is_valid)) call table_valid_mask_of{msuf}(self%cache, idx, is_valid)",
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
              "            call table_context_suffix(self%cache, name, sfx)",
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
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING, "get")
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
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
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
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
    msuf = "" if rank == 1 else "_elem"
    size_expr = "size(arr, kind=int64)" if rank == 1 else "size(arr, 2, kind=int64)"
    return f"""    module procedure set_arr_{tag}
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, writing=.true.)
        call table_require_kind(self, idx, {pk}, "set")
        call table_require_length(self, idx, {size_expr}, "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid{msuf}(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_{tag}
    !"""


def _setelem_pair(tag, pk):
    """One kind's two %set_element specifics: the int64 worker and its int32 delegation.

    The int32 form exists so a caller with a plain default-kind INTEGER loop variable can write
    `call t%set_element("x", i, v)` without an int() cast -- the project-wide dual-kind rule for
    a public numeric argument.
    """
    return f"""    module procedure set_element_{tag}_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_{tag}_i32
    !
    module procedure set_element_{tag}_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, {pk}, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_{tag}_i64
    !"""


ZERO = {
    "i32": "0_int32", "i64": "0_int64", "f32": "0.0_real32", "f64": "0.0_real64",
    "bool": ".false.",
}


def getelem_impl(k):
    """One row's value by index, widening exactly as the row handle's %get does.

    The int32 specific forwards to the int64 one; the int64 one carries the body. Widening is a
    `select case` on the stored kind for the same reason it is in row_get: generic resolution
    picks the specific from the CALLER's variable, and what the column holds is a run-time fact.
    """
    tag, pk, decl, comp, rank, cat = k
    out = [f"""    module procedure get_element_{tag}_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_{tag}_i32
    !"""]
    lines = [f"    module procedure get_element_{tag}_i64", "        integer :: idx"]
    for _, src in WIDEN.get(tag, []):
        srcdecl = next(kk[2] for kk in KINDS if kk[0] == src)
        if rank == 1:
            lines.append(f"        {srcdecl} :: v_{src}")
        else:
            lines.append(f"        {srcdecl}, allocatable :: v_{src}(:)")
    lines.append("        !")
    # A miss has to leave `value` defined, since it is intent(out): a scalar gets its type's
    # zero, an allocatable result stays unallocated (the same "nothing here" %get already uses).
    if rank == 1 and cat == "num":
        miss = f"            value = {ZERO[tag]}"
    elif rank == 1:
        lines.append(f"        {decl} :: blank")
        miss = "            value = blank"
    else:
        miss = "            ! `value` stays unallocated, which is how %get reports a miss too."
    lines += ['        call table_resolve(self, name, "get_element", idx, found)',
              "        if (idx == 0) then",
              miss,
              "            return",
              "        end if",
              '        call table_require_row(self, i, "get_element")',
              "        select case (self%cache%cols(idx)%declared_kind)",
              f"        case ({pk})"]
    if rank == 1:
        lines.append("            call self%cache%cols(idx)%values%get_at(i, value)")
    else:
        lines += ["            allocate(value(self%cache%cols(idx)%width))",
                  "            call self%cache%cols(idx)%values%get_at(i, value)"]
    for srcpk, src in WIDEN.get(tag, []):
        lines.append(f"        case ({srcpk})")
        if rank == 1:
            lines += [f"            call self%cache%cols(idx)%values%get_at(i, v_{src})",
                      f"            value = v_{src}"]
        else:
            lines += [f"            allocate(v_{src}(self%cache%cols(idx)%width))",
                      f"            call self%cache%cols(idx)%values%get_at(i, v_{src})",
                      "            allocate(value(self%cache%cols(idx)%width))",
                      f"            value = v_{src}"]
    lines += ["        case default",
              # Always fails here, which is the point: it produces the same "column kind is X,
              # not Y" message every other exact-kind path in this layer produces.
              f'            call table_require_kind(self, idx, {pk}, "get_element")',
              "        end select",
              f"    end procedure get_element_{tag}_i64",
              "    !"]
    out.append("\n".join(lines))
    return "\n".join(out)


def getelem_str_impl():
    return """    module procedure get_element_chr_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chr_i32
    !
    module procedure get_element_chr_i64
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        value = ""
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_element")
        call table_require_row(self, i, "get_element")
        call self%cache%cols(idx)%values%string_column(store)
        ! allow_null keeps a null row from aborting: it reads back as "", and %is_null is how a
        ! caller tells the two apart -- the same rule the row handle's %get follows.
        call store%get(i, value, allow_null=.true.)
    end procedure get_element_chr_i64
    !
    module procedure get_element_chrv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chrv_i32
    !
    module procedure get_element_chrv_i64
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        character(len=:), allocatable :: str1
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "get_element")
        call table_require_row(self, i, "get_element")
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, row) at
        ! (row-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%get(flat, str1, allow_null=.true.)
            if (len(str1) > maxlen) maxlen = len(str1)
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%get(flat, str1, allow_null=.true.)
            value(e) = str1
        end do
    end procedure get_element_chrv_i64
    !"""


def setelem_impl(k):
    return _setelem_pair(k[0], k[1])


def setelem_str_impl():
    return "\n".join([_setelem_pair("chr", "PK_STRING"), _setelem_pair("chrv", "PK_STRING_VEC")])


def set_str_impl():
    return """    module procedure set_arr_chr
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, writing=.true.)
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chr
    !
    module procedure set_arr_chrv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, writing=.true.)
        call table_require_kind(self, idx, PK_STRING_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
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
    w(add_strcol_impl())
    w("end submodule parquet_tables_addcol ! GCOVR_EXCL_LINE")
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
        self%cache%cols(idx)%width = {w_expr}
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
        self%cache%cols(idx)%width = 1
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
        self%cache%cols(idx)%width = size(values, 1)
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
    for k in KINDS:
        w(matchunk_impl(k))
    w(gen_dispatch())
    w("end submodule parquet_tables_materialize ! GCOVR_EXCL_LINE")
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
        ! adopt, not init+set_all: the column takes over `tmp`'s allocation instead of allocating
        ! its own and copying into it, which saves a full pass over the data and a second live copy
        ! of the column. `tmp` is a temporary this procedure was about to discard, which is exactly
        ! what adopt is for.
        call col%adopt(tmp, unit)
    end procedure mat_{tag}
    !"""
    tmp_alloc = "tmp(nrows)" if rank == 1 else "tmp(wdt, nrows)"
    valid_alloc = "valid(nrows)" if rank == 1 else "valid(wdt, nrows)"
    if rank == 1:
        null_loop = """                do i = 1, nrows
                    if (.not. valid(i)) call col%set_null(i)
                end do"""
        decls = "        integer(int64) :: i"
    else:
        # Arrow reports per-ELEMENT validity for a vector column, and parquet_column stores it
        # that way too, so the mask is handed over WHOLE rather than collapsed to one bit per
        # row. set_validity writes the bitmap in a single pass; replaying it through
        # set_null(i, e) would cost width*nrows type-bound calls to record the same thing.
        null_loop = "                call col%set_validity(valid)"
        decls = ""
    return f"""    module procedure mat_{tag}
        {decl}, allocatable :: tmp{dims(rank)}
        logical, allocatable :: valid{dims(rank)}
{decls}
        !
        allocate({tmp_alloc})
        ! The file's own statistics answer this from the footer, without reading a byte of the
        ! column -- and when they say there are no Nulls, the whole validity pipeline is skipped:
        ! no int8 buffer, no LOGICAL mask (four bytes per row, four times the buffer it is built
        ! from), no per-element scan in C++, and no replay loop below. That is the common case for
        ! every column of an ordinary file. parquet_column_has_nulls answers .true. whenever it
        ! cannot be sure, so the slow path is the safe default.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate({valid_alloc})
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            ! Guarded rather than unconditional: statistics describe the FILE, so a column that
            ! reports Nulls may still have none among the rows actually read (a filter can remove
            ! them), and one vectorised all() beats nrows type-bound calls that do nothing.
            if (.not. all(valid)) then
{null_loop}
            end if
        else
            call parquet_read_column(reader, name, tmp)
            call col%adopt(tmp, unit)
        end if
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
        integer :: slen
        !
        ! There is no compact buffer path for a rank-2 string column, so this goes through the
        ! legacy fixed-width reader -- which means trailing blanks cannot be distinguished from
        ! padding and are trimmed. Documented as a known limitation of vector string columns.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        ! No adopt for the string kinds (they own a parquet_string_column, not a plain array), but
        ! the validity work is skipped the same way every other kind skips it -- see mat_f64.
        if (parquet_column_has_nulls(reader, name, 0_int64, 0_int64)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column(reader, name, tmp, is_valid=valid)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
            ! Per ELEMENT, exactly like every other vector kind: the mask is handed over whole
            ! rather than collapsed to one bit per row. set_all above wrote every element, which
            ! cleared the all-null state a freshly initialized string store starts in, so this
            ! records exactly the nulls Arrow reported and nothing else.
            if (.not. all(valid)) call col%set_validity(valid)
        else
            call parquet_read_column(reader, name, tmp)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
        end if
    end procedure mat_strv
    !"""


def matchunk_impl(k):
    """One row group's worth of a column, as its own parquet_column.

    Deliberately the same shape as mat_impl with a chunked read swapped in, so the two cannot
    drift on validity handling -- which is exactly where the whole-column form has been wrong
    before (a flat element index into a row-granular API, see mat_strv).
    """
    tag, pk, decl, comp, rank, cat = k
    if cat == "str" and rank == 1:
        return """    module procedure matchunk_str
        type(parquet_string_column) :: tmp
        type(parquet_string_column), pointer :: dest
        !
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%init(PK_STRING, nrows, wdt, unit)
        call col%string_column(dest)
        dest = tmp%clone()
    end procedure matchunk_str
    !"""
    if cat == "str":
        return """    module procedure matchunk_strv
        character(len=:), allocatable :: tmp(:,:)
        logical, allocatable :: valid(:,:)
        integer(int64) :: i
        integer :: slen
        !
        ! parquet_get_string_length is a whole-column question, so this asks for the longest
        ! element anywhere in the column rather than in this row group. That over-allocates the
        ! buffer slightly and is otherwise harmless: the padded path trims on the way in anyway.
        call parquet_get_string_length(reader, name, slen)
        if (slen < 1) slen = 1
        allocate(character(len=slen) :: tmp(wdt, nrows))
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate(valid(wdt, nrows))
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
            ! Per element, as in mat_strv -- see its note.
            if (.not. all(valid)) call col%set_validity(valid)
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%init(PK_STRING_VEC, nrows, wdt, unit)
            call col%set_all(tmp)
        end if
    end procedure matchunk_strv
    !"""
    if cat == "tmp":
        alloc = "tmp(nrows)" if rank == 1 else "tmp(wdt, nrows)"
        return f"""    module procedure matchunk_{tag}
        {decl}, allocatable :: tmp{dims(rank)}
        !
        allocate({alloc})
        call parquet_read_column_chunk(reader, name, rg, tmp)
        call col%adopt(tmp, unit)
    end procedure matchunk_{tag}
    !"""
    tmp_alloc = "tmp(nrows)" if rank == 1 else "tmp(wdt, nrows)"
    valid_alloc = "valid(nrows)" if rank == 1 else "valid(wdt, nrows)"
    # Rank 2 hands the per-ELEMENT mask over whole; see mat_*'s own note for why.
    null_loop = ("                do i = 1, nrows\n"
                 "                    if (.not. valid(i)) call col%set_null(i)\n"
                 "                end do") if rank == 1 else (
                 "                call col%set_validity(valid)")
    idecl = "        integer(int64) :: i\n" if rank == 1 else ""
    return f"""    module procedure matchunk_{tag}
        {decl}, allocatable :: tmp{dims(rank)}
        logical, allocatable :: valid{dims(rank)}
{idecl}        !
        allocate({tmp_alloc})
        ! Scoped to THIS row group, unlike mat_{tag}'s whole-file question: a file with Nulls
        ! somewhere else must not force the mask pipeline onto a clean row group. See mat_{tag}
        ! for what skipping it saves.
        if (parquet_column_has_nulls(reader, name, rg, rg)) then
            allocate({valid_alloc})
            call parquet_read_column_chunk(reader, name, rg, tmp, is_valid=valid)
            call col%adopt(tmp, unit)
            if (.not. all(valid)) then
{null_loop}
            end if
        else
            call parquet_read_column_chunk(reader, name, rg, tmp)
            call col%adopt(tmp, unit)
        end if
    end procedure matchunk_{tag}
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
