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

Regenerates the per-kind blocks of the `parquet_tables` table
layer: `src/parquet_tables.f90` (the module spec), `src/parquet_tables_access.f90`,
`src/parquet_tables_addcol.f90` and `src/parquet_tables_materialize.f90`. It imports its kind
table from `tools/generate_parquet_columns.py` rather than keeping a second copy, so a new column
kind is declared in exactly one place and the two layers cannot drift apart. Output is committed
and `--check` verifies it, exactly as for the columns generator. The other four
`parquet_tables_*.f90` files (`lifecycle`, `query`, `read`, `write`) are hand-written and the
script never touches them. Maintainer-only (stripped from the fpm-published package).
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
!!   See parquet_tables_parallel.f90 and doc/pages/operating/thread-safety.md.
!!
!! Depends on `parquet_columns` (the value store) and `parquet_core` (the reader/writer it drives).
module parquet_tables
    ! int8 is here for ONE caller: the packed leaf arrays and the per-row Kleene verdicts
    ! %row_mask exchanges with parquet_core's filter evaluator are int8, and
    ! parquet_tables_filter.f90 has to declare them.
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
#ifdef _OPENMP
    ! Only omp_lock_kind is needed at module scope, for parquet_table_cache's own lock component.
    ! The procedures that operate on it import omp_lib themselves, exactly as unsafe_first_touch
    ! and record_open_thread already do.
    use omp_lib, only : omp_lock_kind
#endif
    use parquet_columns
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    ! The three container element types. They cost this module's compile footprint NOTHING -- all
    ! three are already in it through parquet_core, which imports them for the reader and writer --
    ! so this is a namespace import, not a new dependency (tools/check_module_footprints.sh is the
    ! check that keeps that true).
    use parquet_list, only : parquet_list_column
    use parquet_map, only : parquet_map_column
    use parquet_struct, only : parquet_struct_column
    use parquet_core, only : parquet_reader, parquet_writer, parquet_schema, &
        parquet_open_reader, parquet_close_reader, parquet_get_nrows, parquet_get_col_size, &
        parquet_get_column_names, parquet_get_column_type, parquet_column_exists, &
        parquet_get_column_shape, parquet_get_map_value_type, &
        parquet_release_column, parquet_read_column, parquet_get_string_length, &
        parquet_get_num_row_groups, parquet_get_chunk_size, parquet_read_column_chunk, &
        parquet_open_writer, parquet_write_column, parquet_close_writer, parquet_write_row_mask, &
        parquet_measure_list_width, parquet_column_width_needs_data, parquet_column_has_nulls, &
        parquet_load_qc_maml_file, parquet_filter, parquet_sortkey, parquet_read_qc, &
        parquet_compose_read_qc, parquet_reader_set_filter, parquet_parse_maml, &
        parquet_get_metadata_items, parquet_get_qc_columns, parquet_get_physical_row_indices, &
        parquet_get_column_time_info, parquet_size_auto, parquet_reader_adopt_transform, &
        parquet_unit_millis, parquet_unit_micros, parquet_unit_nanos, &
        parquet_split_name_list, parquet_parse_sort_key, &
        parquet_parse_filter_rules, parquet_eval_filter_leaf, parquet_eval_filter_program, &
        filter_leaf_name_len, filter_leaf_op_len, filter_leaf_value_len
    ! The table layer's two solicited printers (%print_stat) and its own warnings go through the
    ! library's output channels rather than printing directly, so verbosity/message_stream apply
    ! here as everywhere -- see tools/check_source_conventions.py's `no direct printing` check.
    use parquet_settings, only : parquet_emit_warning, parquet_output_is_suppressed, &
        parquet_get_arrow_version
    ! The one binding this layer reaches for directly, and it needs no reader: the seed for a
    ! sample_fraction= open is settled BEFORE the table's reader is created, so that reader and
    ! every later one (a clone's, a per-thread one) draw the identical rows. It stays out of the
    ! `use parquet` namespace via this module's default-private accessibility.
    use parquet_random, only : pf_random_seed
    !
    implicit none
    private
    !
    public :: parquet_table
    public :: parquet_table_row
    public :: parquet_table_col
    public :: parquet_slice
    public :: parquet_slice_range
    public :: parquet_slice_list
    public :: parquet_open_table
    public :: parquet_new_table
    public :: parquet_write_table
    public :: parquet_table_row_group_bounds
    !> Re-exported from parquet_settings so that a `use parquet_tables` program can report
    !! which Arrow/Parquet C++ it is linked against without a second import. The library's
    !! OWN version is not re-exported here -- that is `parquet_get_version`, in the leaf
    !! module parquet_version, carried only by the `parquet` facade.
    public :: parquet_get_arrow_version
    public :: PARQUET_ROW_INDEX
    ! REGIME_FULL/REGIME_SLICE are deliberately NOT public. `regime` is a private component of
    ! parquet_table and no binding exposes it, so a caller could never obtain a value to compare
    ! against either constant -- publishing them advertised a distinction the public API cannot
    ! express. The submodules that assign `regime` reach both by host association, which is why
    ! nothing inside the tier changes. Row 30's code issue 1.
    public :: RES_EMPTY, RES_PARTIAL, RES_FULL
    !> TEST-ONLY debug hook; deliberately NOT in README.md's API overview. See its own
    !! doc-comment for why it has to be public at all.
    public :: parquet_debug_table_set_inflight
    public :: parquet_debug_colread_block_rows
    public :: parquet_debug_table_drop_name_index
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
    !!
    !! **`list_columns=` decides what a variable-length `LIST` column becomes**, and it takes a
    !! token rather than a logical so a third policy can be added later without a second argument:
    !!
    !! - `"auto"` (the default, and what every earlier release did) measures the column. A `LIST`
    !!   whose rows all happen to hold the same number of elements becomes an ordinary VECTOR
    !!   column of that width; a ragged one becomes a `parquet_list_column`. The measurement is
    !!   over the rows the table covers, so **a slice and the whole file can legitimately disagree**
    !!   about the same column -- a file that is ragged overall may be uniform inside one slice.
    !! - `"container"` makes every `LIST` column a `parquet_list_column`, from the schema alone,
    !!   with nothing measured and nothing data-dependent. That is what a program wants when it is
    !!   going to treat the column as a list whatever the data happens to look like, and it is the
    !!   only way to be sure two tables over one file agree.
    !!
    !! It governs the plain-`LIST` case and nothing else: a `FIXED_SIZE_LIST` column -- which is
    !! what every column this library writes is -- carries its width in the schema and is a vector
    !! column under both tokens.
    !!
    !! **`bounded=.true.` reads a file larger than memory.** By default a `filter=` is installed by
    !! decoding every filter column over the whole file at once, and each payload column is then
    !! decoded whole and filtered -- both proportional to the FILE, however few rows survive. Under
    !! `bounded=.true.` the filter is evaluated one row group at a time and every column is
    !! assembled from per-row-group chunks, so the peak is one row group's worth of one column
    !! rather than one column. Three things to know:
    !!
    !! - **It is opt-in and never automatic.** Nothing is cached and the filter is evaluated per
    !!   row group, so on a file that fits in memory it is never faster than the default and is
    !!   often slower. The library cannot know how much memory the caller has, so it does not guess.
    !! - **`sort=` is refused with it**, because a sort reorders rows across the whole file and no
    !!   row-group-scoped read can be assembled under one. Open without `sort=`, then
    !!   `%materialize_all` and `%sort_by`.
    !! - **It is accepted and does nothing on the two slice forms**, which assemble their columns
    !!   from row-group chunks already.
    !!
    !! What is NOT bounded by it: the filter's own row mask, one bit per row of every row group the
    !! statistics screen did not prune, held for as long as the table's reader is open.
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
    ! ---- Per-column work a row-structural mutation hands to `table_colwork` ----
    ! Private: an op code is how one parallel region serves three operations without an abstract
    ! interface in this spec and an indirect call inside the region. One branch per column.
    integer, parameter :: PCW_REINDEX_TRUSTED = 1 !! %reindex_trusted(rows) -- the sort's replay.
    integer, parameter :: PCW_DELETE_MASK = 2     !! %delete_by_mask(keep) -- filter/delete/truncate.
    integer, parameter :: PCW_GATHER = 3          !! %gather(rows) -- %top_n's selection.
    private :: PCW_REINDEX_TRUSTED, PCW_DELETE_MASK, PCW_GATHER
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
        !> The live slots' indices ordered by column NAME, so `cache_find` can bisect instead of
        !! scanning; the ordering covers `cols(1:ncols)`. Every value accessor resolves a name
        !! through `cache_find`, so on a wide table that scan is most of the cost of a
        !! `%get_element` call.
        !!
        !! **Maintained EAGERLY, by the mutations that change the column set** -- never rebuilt
        !! lazily inside a lookup. `cache_find` takes the cache `intent(in)` precisely so that
        !! concurrent readers of a shared table need no synchronisation to read it (see
        !! `readers_active`); a lookup that rebuilt this would be a write on that path, and two
        !! threads reallocating one array is heap corruption rather than a stale answer.
        !!
        !! **A stale or absent index can never produce a wrong answer**, only a slow one:
        !! `cache_find` re-checks the name at the slot it lands on and falls back to the linear
        !! scan if it does not match. That is what makes a forgotten rebuild site a performance
        !! bug a benchmark catches, rather than a silently wrong column.
        integer, allocatable :: name_order(:)
        !> `name_order`'s entries' sort keys: the first 7 bytes of each name, packed big-endian
        !! into an integer and blank-padded, so the bisection above compares INTEGERS out of one
        !! contiguous array instead of chasing a deferred-length `character` allocation per probe.
        !! That is the difference between beating the linear scan and merely matching it: the scan
        !! walks `cols` in order and prefetches well, while a bisection over the names alone jumps
        !! about and misses. Seven bytes rather than eight keeps every key positive, so a plain
        !! signed comparison orders them; names sharing a 7-byte prefix tie, and the search falls
        !! back to comparing the names themselves for those.
        integer(int64), allocatable :: name_key(:)
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
        !> Long reads currently in flight -- the ones that would still be reading storage when a
        !! concurrent %append reallocated it. %append checks this under the lock and aborts
        !! rather than reallocating underneath one.
        !!
        !! **Maintained around exactly TWO windows, and neither is "a coarse accessor":** the lazy
        !! FIRST TOUCH inside table_resolve_slot -- whichever accessor triggered it, %get and
        !! %get_element alike -- and %prefetch/%materialize_all's bulk materialize
        !! (materialize_marked). Both read from the file and take real time.
        !!
        !! **No accessor of any kind maintains it for a column that is already RESIDENT.** Two
        !! atomics per cell would dominate a %get_element loop over a large column, and a resident
        !! read is a memcpy over memory no %append can be reallocating -- the cheap
        !! `append_active` check above already covers that shorter window, and every accessor
        !! takes it, resident or not. Do not "fix" that asymmetry.
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
        !> The seed every reader this table opens will sample with. Allocated and POSITIVE whenever
        !! `read_sample_fraction` is allocated -- an unseeded open settles one at open time
        !! (`pf_random_seed`) rather than leaving each reader to draw its own. That invariant is
        !! what makes a `%clone`, a reopen and a per-thread reader all keep the same rows; before
        !! it existed, an unseeded clone redrew and silently held a different sample.
        integer(int64), allocatable :: read_sample_seed
        !> How a variable-length `LIST` column is classified: `"auto"` (the default) or
        !! `"container"`. Settled once at open from `parquet_open_table`'s `list_columns=`
        !! argument and never changed afterwards.
        !!
        !! **It is on the cache because every reader this table opens must agree with it.** `%clone`
        !! reopens the file and the internally-parallel `%prefetch` opens a reader per thread; a
        !! policy held anywhere else would let two readers over one table disagree about whether a
        !! column is a `PK_LIST` or a `*_VEC`, which is `feature_risks.md` Risk-152's
        !! whole-file-versus-slice disagreement reappearing INSIDE one table.
        !!
        !! Unallocated is read as `"auto"` everywhere, so a cache built by `parquet_new_table` --
        !! which has no file and classifies nothing -- needs no initialiser and no special case.
        character(len=:), allocatable :: list_columns
        !> .true. when this table was opened with `bounded=.true.` over the WHOLE file: its filter
        !! was installed with the row-group-scoped engine and every column is assembled from
        !! per-row-group chunks instead of read whole.
        !!
        !! **Set only for a whole-file open** (`bounded .and. .not. sliced`), which is what lets
        !! every reader of this flag test it alone. A slice is chunked already -- that is what the
        !! slice regime IS -- so `bounded=` on a slice form is accepted and changes nothing rather
        !! than being a second spelling of the same thing.
        !!
        !! On the cache rather than on `parquet_table` for the reason the block above gives at
        !! length: `parquet_table` is finalizable, and this project has three confirmed compiler
        !! bugs in the machinery that walks such a type's components.
        !!
        !! It travels with a `%clone` (`parquet_tables_clone.f90`), because a clone reopens the
        !! file through the same helper and would otherwise silently reattach through the caching
        !! engine.
        logical :: bounded_read = .false.
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
    # The tag is not decoration: an `end module` line carries no executable statement, yet gcov
    # credits it with a large hit count (1690 in one full run here), so without the phrase it is
    # reported forever as a candidate stale exclusion. See CLAUDE.md, "Fortran gcov attribution
    # artifacts".
    # table_finalize is bound as `final ::` and its body is emitted HERE, module-contained,
    # rather than in parquet_tables_lifecycle.f90 with the rest of the lifecycle code. Not a style
    # choice: nagfor 7.2 panics with `Panic: Cannot find scope id 0` -- an internal compiler error
    # naming no line -- when compiling ANY submodule of a separately compiled module that declares
    # a FINAL binding whose target is a SEPARATE MODULE PROCEDURE, under -C=undefined. An empty
    # submodule is enough, so every parquet_tables_* file failed to compile. Keeping the target
    # module-contained is the documented ingredient that avoids it. table_destroy_lock stays a
    # separate module procedure (module -> submodule is the legal direction and it has an
    # interface). See feature_nag_ice_scope_id.md. Do not move this back into a submodule, and
    # give any FUTURE finalizer on this type the same treatment.
    w("contains")
    w("    !")
    w("    !> FINAL procedure: frees the column store and abandons the reader. Runs at scope exit")
    w("    !! and on an intent(out) reopen, so it must always succeed silently -- it validates")
    w("    !! nothing. Module-contained on purpose; see the note in the generator that emits it.")
    w("    subroutine table_finalize(self)")
    w("        type(parquet_table), intent(inout) :: self !! the table being destroyed.")
    w("        ! An implicit finalizer runs at unpredictable points -- scope exit, an intent(out)")
    w("        ! reopen, an early return -- with no caller able to see or handle a failure, so it")
    w("        ! must always succeed silently and validate nothing (CLAUDE.md). Deallocating the")
    w("        ! cache runs parquet_reader's own finalizer, which abandons rather than closes it.")
    w("        if (associated(self%cache)) then")
    w("            ! The lock is an OpenMP handle rather than a value, so it has to be destroyed")
    w("            ! explicitly -- deallocating the cache would otherwise leak whatever the")
    w("            ! runtime allocated for it. table_destroy_lock validates nothing and cannot")
    w("            ! abort, which is what makes it safe to call from here.")
    w("            call table_destroy_lock(self%cache)")
    w("            deallocate(self%cache)")
    w("            nullify(self%cache)")
    w("        end if")
    w("    end subroutine table_finalize")
    w("    !")
    w("end module parquet_tables ! GCOVR_EXCL_LINE -- gcov attribution artifact")
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
        procedure :: column_index => table_column_index !! A column's 1-based position, 0 when absent.
        procedure :: column_name => table_column_name !! Copy out the name at a 1-based position.
        procedure, private :: column_by_name !! %column specific taking a column name.
        procedure, private :: column_by_index !! %column specific taking a 1-based position.
        !> A resolved handle on one column, by name or by 1-based position. Resolves once -- the
        !! lookup, the lazy first touch and the kind -- so a per-element loop over that column
        !! stops paying for a name lookup on every access. See `parquet_table_col`.
        generic :: column => column_by_name, column_by_index
        procedure, private :: has_nulls_name => table_has_nulls !! %has_nulls specific, by name.
        procedure, private :: has_nulls_at => table_has_nulls_at !! %has_nulls specific, by position.
        !> Whether a column holds (or may hold) nulls -- named, or by 1-based position.
        generic :: has_nulls => has_nulls_name, has_nulls_at
        procedure, private :: table_get_valid_mask     !! %get_valid_mask specific, per-row mask.
        procedure, private :: table_get_valid_mask_elem !! %get_valid_mask specific, per-element mask.
        !> Copy out a column's validity as a plain `logical` array. A rank-1 `mask` gives one entry
        !! per row (on a *_VEC column: "any element of the row is null"); a rank-2 `mask` gives the
        !! true `(width, nrows)` per-element state.
        generic :: get_valid_mask => table_get_valid_mask, table_get_valid_mask_elem
        procedure :: generation => table_generation  !! Counter bumped by every structural change.
        procedure :: has_column => table_has_column  !! Whether a column of this name exists.
        procedure, private :: missing_columns_string !! %missing_columns specific, separated string.
        procedure, private :: missing_columns_array  !! %missing_columns specific, array of names.
        !> Which of these columns the table does NOT have, as a packed array (zero-size when it
        !! has them all). The non-aborting half of %require_columns.
        generic :: missing_columns => missing_columns_string, missing_columns_array
        procedure, private :: require_columns_string !! %require_columns specific, separated string.
        procedure, private :: require_columns_array  !! %require_columns specific, array of names.
        !> Aborts unless the table has every one of these columns, naming EVERY missing one --
        !! not just the first, which is what a hand-written loop reports.
        generic :: require_columns => require_columns_string, require_columns_array
        procedure, private :: kind_name => table_column_kind !! %kind specific, by name.
        procedure, private :: kind_at => table_column_kind_at !! %kind specific, by position.
        !> A column's PK_* kind discriminator -- named, or by 1-based position.
        generic :: kind => kind_name, kind_at
        procedure, private :: width_name => table_column_width !! %width specific, by name.
        procedure, private :: width_at => table_column_width_at !! %width specific, by position.
        !> A column's values-per-row (1 if scalar) -- named, or by 1-based position.
        generic :: width => width_name, width_at
        procedure, private :: unit_name => table_column_unit !! %unit specific, by name.
        procedure, private :: unit_at => table_column_unit_at !! %unit specific, by position.
        !> Copy out a column's unit string -- named, or by 1-based position.
        generic :: unit => unit_name, unit_at
        procedure, private :: residency_name => table_column_residency !! %residency specific, by name.
        procedure, private :: residency_at => table_column_residency_at !! %residency specific, by position.
        !> A column's RES_* residency state -- named, or by 1-based position.
        generic :: residency => residency_name, residency_at
        procedure, private :: is_null_i32 => table_is_null_i32 !! %is_null specific, int32 row index.
        procedure, private :: is_null_i64 => table_is_null_i64 !! %is_null specific, int64 row index.
        procedure, private :: is_null_e32 => table_is_null_e32 !! %is_null specific, int32 row + element.
        procedure, private :: is_null_e64 => table_is_null_e64 !! %is_null specific, int64 row + element.
        procedure, private :: is_null_at_i32 => table_is_null_at_i32 !! %is_null by position, int32 row.
        procedure, private :: is_null_at_i64 => table_is_null_at_i64 !! %is_null by position, int64 row.
        procedure, private :: is_null_at_e32 => table_is_null_at_e32 !! %is_null by position, int32 row + element.
        procedure, private :: is_null_at_e64 => table_is_null_at_e64 !! %is_null by position, int64 row + element.
        !> Whether row `i` of a column is null, or -- given `e` as well -- element `e` of it.
        !!
        !! On a *_VEC column the row form answers "ANY element of the row is null"; the element
        !! form answers about that one element. Defined on a scalar column too, where `e` can only
        !! be 1 and the two agree.
        generic :: is_null => is_null_i32, is_null_i64, is_null_e32, is_null_e64, &
            is_null_at_i32, is_null_at_i64, is_null_at_e32, is_null_at_e64
        procedure :: is_detached => table_is_detached !! Whether the table has left its file behind.
        procedure, private :: is_supported_name => table_is_supported !! %is_supported specific, by name.
        procedure, private :: is_supported_at => table_is_supported_at !! %is_supported specific, by position.
        !> Whether a column's type can be read -- named, or by 1-based position.
        generic :: is_supported => is_supported_name, is_supported_at
        procedure :: filename => table_filename      !! Copy out the file this table came from.
        procedure :: get_file_metadata => table_get_file_metadata !! One key from the file's metadata.
        ! --- residency control ---
        procedure, private :: prefetch_string !! %prefetch specific taking a separated name string.
        procedure, private :: prefetch_array  !! %prefetch specific taking an array of names.
        !> Reads the named column(s) now, instead of on first touch. Required before a parallel
        !! region: a first touch inside one is a hard error, since it would mutate shared state.
        generic :: prefetch => prefetch_string, prefetch_array
        !> The same call as %prefetch, under the name that pairs with %materialize_all. Reaching
        !! for the definitive-sounding %materialize_all when only a few columns are wanted reads
        !! the whole file, silently; %materialize(names) is the one to find first.
        generic :: materialize => prefetch_string, prefetch_array
        procedure :: materialize_all => table_materialize_every !! Read every column not yet read.
        procedure :: reload => table_reload           !! Re-read one column; force= to discard local edits.
        procedure :: evict_column => table_evict_column !! Drop a column's VALUES; force= if it holds local edits.
        procedure :: set_user_populated => table_set_user_populated !! Claim a column's values as the caller's own, or unclaim.
        procedure :: is_user_populated => table_is_user_populated !! Whether a column is claimed as holding the caller's values.
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
    w("        !!")
    w("        !! **The selection is taken in the order given, duplicates included.** A")
    w("        !! `parquet_slice_list` may name a row more than once and may name rows in any")
    w("        !! order, so the result has one entry per SELECTION -- not per distinct row -- and")
    w("        !! its order is the selection's, never the table's.")
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
    w("        !!")
    w("        !! **The selection is written in the order given, duplicates included, so a row")
    w("        !! named twice ends up holding the LAST value written to it.** Selections are")
    w("        !! applied one after another rather than merged or de-duplicated, which is what")
    w("        !! makes `%set_slice` the exact inverse of `%get_slice` for a selection that names")
    w("        !! each row once.")
    w("        generic :: set_slice => " + wrap_list(
        [f"set_slice_{k[0]}" for k in ARRAY_KINDS] + ["set_slice_chr", "set_slice_chrv"], 12,
        first_prefix=len("        generic :: set_slice => ")))
    # pointer accessors
    w("        ! --- zero-copy pointer access (exact kind) ---")
    for k in PTR_KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_ptr_{tag} !! %col specific for the {tag} kind.")
    w("        procedure, private :: col_ptr_strcol !! %col specific aliasing the compact string store.")
    for tag, pk, ctype in CONTAINERS:
        w(f"        procedure, private :: col_ptr_{tag}col !! %col specific aliasing a {pk} container.")
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
    w("        generic :: col => " + wrap_list(
        [f"col_ptr_{k[0]}" for k in PTR_KINDS] + ["col_ptr_strcol"]
        + [f"col_ptr_{c[0]}col" for c in CONTAINERS], 12,
        first_prefix=len("        generic :: col => ")))
    # get
    w("        ! --- copy out (widens int32->int64, float32->float64) ---")
    for k in ARRAY_KINDS:
        tag = k[0]
        w(f"        procedure, private :: get_arr_{tag} !! %get specific for the {tag} kind.")
    w("        procedure, private :: get_arr_str  !! %get specific returning a parquet_string_column.")
    for tag, pk, ctype in CONTAINERS:
        w(f"        procedure, private :: get_arr_{tag}col !! %get specific returning a {ctype}.")
    w("        procedure, private :: get_arr_chr  !! %get specific returning a character array.")
    w("        procedure, private :: get_arr_chrv !! %get specific returning a character (elem, row) array.")
    w("        !> Copies a column into a freshly allocated array of the caller's own kind.")
    w("        generic :: get => " + wrap_list(
        [f"get_arr_{k[0]}" for k in ARRAY_KINDS] + ["get_arr_str", "get_arr_chr", "get_arr_chrv"]
        + [f"get_arr_{c[0]}col" for c in CONTAINERS], 12,
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
    for tag, pk, ctype in CONTAINERS:
        w(f"        procedure, private :: set_arr_{tag}col !! %set specific taking a {ctype}.")
    w("        generic :: set => " + wrap_list(
        [f"set_arr_{k[0]}" for k in ARRAY_KINDS] + ["set_arr_chr", "set_arr_chrv", "set_arr_strcol"]
        + [f"set_arr_{c[0]}col" for c in CONTAINERS], 12,
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
    for tag, pk, ctype in CONTAINERS:
        w(f"        procedure, private :: add_column_{tag}col !! %add_column specific taking a {ctype}.")
    w("        procedure, private :: add_column_col  !! %add_column specific taking a whole parquet_column.")
    w("        generic :: add_column => " + wrap_list(
        [f"add_column_{k[0]}" for k in ARRAY_KINDS]
        + ["add_column_chr", "add_column_chrv", "add_column_strcol", "add_column_col"]
        + [f"add_column_{c[0]}col" for c in CONTAINERS], 12,
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
        ! --- reads that answer a filter expression without changing anything ---
        procedure, private :: table_row_mask_expr     !! %row_mask specific, expression text.
        procedure, private :: table_row_mask_filter   !! %row_mask specific, parquet_filter object.
        !> The row mask a filter expression selects, without applying it. Never detaches.
        generic :: row_mask => table_row_mask_expr, table_row_mask_filter
        ! --- mutation: the row set itself -- every one of these DETACHES the table ---
        procedure, private :: table_filter_rows        !! %filter_rows specific, logical mask.
        procedure, private :: table_filter_rows_expr   !! %filter_rows specific, expression text.
        procedure, private :: table_filter_rows_filter !! %filter_rows specific, parquet_filter object.
        !> Keeps only the rows a mask, or a filter expression, selects. Detaching.
        generic :: filter_rows => table_filter_rows, table_filter_rows_expr, table_filter_rows_filter
        procedure, private :: table_sort_by           !! %sort_by specific, array of key names.
        procedure, private :: table_sort_by_string    !! %sort_by specific, separated key string.
        !> Reorders rows by one or more key columns. Detaching.
        generic :: sort_by => table_sort_by, table_sort_by_string
        procedure, private :: table_top_n             !! %top_n specific, array of key names.
        procedure, private :: table_top_n_string      !! %top_n specific, separated key string.
        !> Keeps only the n best rows, in key order. Detaching.
        generic :: top_n => table_top_n, table_top_n_string
        procedure, private :: table_join              !! %join specific, array of key names.
        procedure, private :: table_join_max_i32      !! %join specific, array names, int32 max_rows.
        procedure, private :: table_join_max_i64      !! %join specific, array names, int64 max_rows.
        procedure, private :: table_join_string       !! %join specific, separated key string.
        procedure, private :: table_join_string_max_i32 !! %join specific, key string, int32 max_rows.
        procedure, private :: table_join_string_max_i64 !! %join specific, key string, int64 max_rows.
        !> Matches another table's rows against this one's and brings its columns over. Detaching,
        !! unless every row of this table survives exactly once and in place.
        generic :: join => table_join, table_join_max_i32, table_join_max_i64, &
            table_join_string, table_join_string_max_i32, table_join_string_max_i64
        ! --- the ORDER, without applying it: read-only, and they do NOT detach ---
        procedure, private :: table_argsort_by_i32    !! %argsort_by specific, int32 permutation.
        procedure, private :: table_argsort_by_i64    !! %argsort_by specific, int64 permutation.
        procedure, private :: table_argsort_by_string_i32 !! %argsort_by specific, key string, int32.
        procedure, private :: table_argsort_by_string_i64 !! %argsort_by specific, key string, int64.
        !> The row order the keys imply, without reordering anything. Unlike %sort_by the table
        !! stays attached, so this is how to read rows in an order while keeping the file.
        generic :: argsort_by => table_argsort_by_i32, table_argsort_by_i64, &
                                 table_argsort_by_string_i32, table_argsort_by_string_i64
        procedure, private :: table_argsort_partial_i32 !! %argsort_partial specific, int32 perm.
        procedure, private :: table_argsort_partial_i64 !! %argsort_partial specific, int64 perm.
        procedure, private :: table_argsort_partial_string_i32 !! %argsort_partial, key string, int32.
        procedure, private :: table_argsort_partial_string_i64 !! %argsort_partial, key string, int64.
        !> The `n` best rows in order, by selection rather than a full sort. Also non-mutating.
        generic :: argsort_partial => table_argsort_partial_i32, table_argsort_partial_i64, &
                                      table_argsort_partial_string_i32, table_argsort_partial_string_i64
        procedure, private :: table_is_sorted_by        !! %is_sorted_by specific, array of key names.
        procedure, private :: table_is_sorted_by_string !! %is_sorted_by specific, key string.
        !> Whether the rows are already in that order.
        generic :: is_sorted_by => table_is_sorted_by, table_is_sorted_by_string
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
        ! --- capacity ---
        procedure :: compact => table_compact         !! Release capacity appends left behind.
        procedure, private :: table_reserve_i32       !! %reserve specific, int32 count.
        procedure, private :: table_reserve_i64       !! %reserve specific, int64 count.
        !> Makes room for n rows in every resident column, so the appends that follow do not
        !! reallocate. %compact's counterpart; neither changes the row set, so neither detaches.
        generic :: reserve => table_reserve_i32, table_reserve_i64
        !> Makes room for n COLUMNS, so that the %add_column calls that follow relocate nothing
        !! and leave an outstanding %col pointer valid. See its own doc-comment for the guarantee.
        procedure :: reserve_columns => table_reserve_columns
        procedure :: column_capacity => table_column_capacity !! Column slots allocated, or spare.
        ! --- copying ---
        procedure :: clone => table_clone                     !! Independent deep copy of this table.
        procedure :: clone_structure => table_clone_structure !! Empty table with the same columns.
        procedure :: clone_extra => table_clone_extra !! Hook -- copies an EXTENDING type's own components.
        ! --- generated table types (see doc/pages/utilities/generated-tables.md) ---
        procedure :: bind_predefined => table_bind_predefined !! Binds a generated type's predefined columns.""")
    w("""        ! --- lifecycle ---
        !> Blocks intrinsic assignment: the store lives behind a pointer, so a default `b = a`
        !! would leave two tables sharing one store and double-freeing it.
        !!
        !! **The binding is named `assign_guard`, and the name's ALPHABETICAL POSITION is
        !! load-bearing -- do not "tidy" it to `table_assign_guard` to match its implementation.**
        !! flang builds a type's binding table sorted by name and stores a SPECIAL binding's index
        !! (a defined assignment, here) in a single byte. `parquet_table` has ~285 bindings, so a
        !! guard sorting under "t" lands past index 255 and flang dies with an internal compiler
        !! error -- `CHECK(bindingIndex <= 255)` in runtime-type-info.cpp, which names neither this
        !! type nor this line. Sorting under "a" keeps it far below the limit. gfortran and ifx are
        !! indifferent, so nothing here will warn if this is undone.
        generic :: assignment(=) => assign_guard
        procedure, private :: assign_guard => table_assign_guard !! The blocking defined assignment.
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
    !! **A handle does not survive a structural change**, and says so rather than reading the wrong
    !! row: it stamps the table's `%generation()` when it is made and refuses once they differ.
    !! `%is_valid()` is the non-aborting way to ask. The rule, the stamp and the message are the
    !! same ones `parquet_table_col` uses -- two handles with one rule between them.
    !!
    !! Still treat it as short-lived: a handle is cheap to make and the refusal is deliberately
    !! conservative, so re-fetching inside the loop is the shape to reach for rather than working
    !! out which mutations a particular handle could have survived.
    !!
    !! **No finalizer, deliberately** -- and this once had one, `row_finalize`, which nullified the
    !! pointer, removed on the argument rather than on a measurement. It protected nothing: it ran
    !! at scope exit (the object is already dead), and on `r = t%row(i)` it nullified a pointer the
    !! copy overwrote a moment later. It could not catch the case that matters either -- a cache
    !! freed with its table leaves `associated()` answering `.true.`, not `.false.`. Meanwhile it
    !! cost two finalizer calls per assignment, on the shape this API makes idiomatic, and put the
    !! type in the class CLAUDE.md says never to give to OpenMP's `private()`.
    !!
    !! `parquet_string` still carries the equivalent nullify-only finalizer, so the library is not
    !! uniform here. That is accepted rather than overlooked: it is a released type, its finalizer
    !! has no measured cost, and the consistency that matters is between this handle and
    !! `parquet_table_col`, which the guide presents as its mirror image.
    type :: parquet_table_row
        private
        type(parquet_table_cache), pointer :: cache => null() !! the table's column store.
        integer(int64) :: irow = 0                            !! this row's 1-based index.
        integer(int64) :: gen = -1_int64                      !! cache%generation when this handle was made.
        type(table_scope) :: scope                            !! the table's row scope, by value.
    contains""")
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_get_{tag} !! %get specific for the {tag} kind.")
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_get_col_{tag} !! %get specific, {tag} kind, column by handle.")
    w("        !> Copies this row's value for a column into the caller's own variable, widening")
    w("        !! int32 -> int64 and float32 -> float64 exactly as the table's own %get does. The")
    w("        !! column may be named by a string or by a `parquet_table_col` handle; the handle")
    w("        !! form does no name lookup, which is what a loop over rows would otherwise repeat.")
    w("        generic :: get => " + wrap_list(
        [f"row_get_{k[0]}" for k in KINDS] + [f"row_get_col_{k[0]}" for k in KINDS], 12,
        first_prefix=len("        generic :: get => ")))
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_set_{tag} !! %set specific for the {tag} kind.")
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: row_set_col_{tag} !! %set specific, {tag} kind, column by handle.")
    w("        !> Writes this row's value for a column. The kind must match the column's exactly")
    w("        !! (a write never widens), and writing a value CLEARS that row's null. The TABLE is")
    w("        !! updated -- a handle is a view of it, not a copy. The column may be named by a")
    w("        !! string or by a `parquet_table_col` handle.")
    w("        generic :: set => " + wrap_list(
        [f"row_set_{k[0]}" for k in KINDS] + [f"row_set_col_{k[0]}" for k in KINDS], 12,
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
        procedure :: index => row_index       !! This row's 1-based index within the table.
        procedure :: is_valid => row_is_valid !! Whether the handle is attached AND still current.
        ! NO `final` -- see the type's own doc-comment. This is a decision, not an omission.
    end type parquet_table_row
    !
    !> A resolved handle on ONE column of a `parquet_table`: the slot, its kind and the table's row
    !! scope, captured once so a per-element loop stops resolving a name on every access.
    !!
    !! Made with `t%column(name)` or `t%column(j)` and used as `call c%get(i, value)`. It is a
    !! VIEW, not a copy -- writes through it change the table -- and it points at the table's
    !! column store, never at the table itself, so the table needs no `target` attribute.
    !!
    !! **A handle does not survive a structural change**, and says so rather than reading the wrong
    !! column: it stamps the table's `%generation()` when it is made and refuses once they differ.
    !! `%is_valid()` is the non-aborting way to ask. Re-fetching costs one lookup.
    !!
    !! **No finalizer, deliberately** -- the same decision `parquet_table_row` now carries, and for
    !! the same reasons. The handle owns nothing and frees nothing, and a finalizer could not catch
    !! the case that matters (a cache freed with its table leaves `associated()` answering `.true.`,
    !! not `.false.`). It is not free either: intrinsic assignment to or from a finalizable type
    !! runs the finalizer twice, which `c = t%column(name)` would pay on every handle it makes.
    !!
    !! **No allocatable components, mandatory.** A per-thread handle declared in a `block` inside a
    !! parallel region is an obvious thing to write, and this project has recorded both an ifx
    !! segfault and a gfortran uninitialised-`private()` bug for types in that position that carry
    !! one. See CLAUDE.md's "New `parquet_table` state goes on the CACHE".
    type :: parquet_table_col
        private
        type(parquet_table_cache), pointer :: cache => null() !! the table's column store.
        integer :: slot = 0                        !! 1-based index into cache%cols.
        integer :: colkind = PK_NONE               !! the kind resolved when the handle was made.
        integer(int64) :: gen = -1_int64           !! cache%generation when this handle was made.
        type(table_scope) :: scope                 !! the table's row scope, by value.
    contains
""")
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_get_{tag}_i32 !! %get specific, {tag} value, int32 row index.")
        w(f"        procedure, private :: col_get_{tag}_i64 !! %get specific, {tag} value, int64 row index.")
    for k in VEC_KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_get_{tag}_e32 !! %get specific, one {tag} element, int32 indices.")
        w(f"        procedure, private :: col_get_{tag}_e64 !! %get specific, one {tag} element, int64 indices.")
    w("        !> Copies one row's value into the caller's variable, widening exactly as the table's")
    w("        !! own `%get_element` does. No name, no lookup -- the handle already knows the slot.")
    w("        !! Given `e` as well, copies ONE ELEMENT of that row without materialising the rest,")
    w("        !! which the name form cannot do at all.")
    w("        generic :: get => " + wrap_list(
        [f"col_get_{k[0]}_{ik}" for k in KINDS for ik in ("i32", "i64")]
        + [f"col_get_{k[0]}_{ik}" for k in VEC_KINDS for ik in ("e32", "e64")], 12,
        first_prefix=len("        generic :: get => ")))
    for k in KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_set_{tag}_i32 !! %set specific, {tag} value, int32 row index.")
        w(f"        procedure, private :: col_set_{tag}_i64 !! %set specific, {tag} value, int64 row index.")
    for k in VEC_KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_set_{tag}_e32 !! %set specific, one {tag} element, int32 indices.")
        w(f"        procedure, private :: col_set_{tag}_e64 !! %set specific, one {tag} element, int64 indices.")
    w("        !> Writes one row's value. The kind must match the column's exactly (a write never")
    w("        !! widens), and writing a value CLEARS that row's null. The TABLE is updated -- a")
    w("        !! handle is a view of it, not a copy. Given `e` as well, writes ONE ELEMENT and")
    w("        !! clears that element's null rather than the whole row's.")
    w("        generic :: set => " + wrap_list(
        [f"col_set_{k[0]}_{ik}" for k in KINDS for ik in ("i32", "i64")]
        + [f"col_set_{k[0]}_{ik}" for k in VEC_KINDS for ik in ("e32", "e64")], 12,
        first_prefix=len("        generic :: set => ")))
    w("""        procedure, private :: col_is_null_i32  !! %is_null specific, whole row, int32 index.
        procedure, private :: col_is_null_i64  !! %is_null specific, whole row, int64 index.
        procedure, private :: col_is_null_e32  !! %is_null specific, one element, int32 indices.
        procedure, private :: col_is_null_e64  !! %is_null specific, one element, int64 indices.
        !> Whether row `i` of this column is null, or -- given `e` as well -- element `e` of it.
        !! On a *_VEC column the row form answers "ANY element of the row is null".
        generic :: is_null => col_is_null_i32, col_is_null_i64, col_is_null_e32, col_is_null_e64
        procedure, private :: col_set_null_i32  !! %set_null specific, whole row, int32 index.
        procedure, private :: col_set_null_i64  !! %set_null specific, whole row, int64 index.
        procedure, private :: col_set_null_e32  !! %set_null specific, one element, int32 indices.
        procedure, private :: col_set_null_e64  !! %set_null specific, one element, int64 indices.
        !> Marks row `i` null, or -- given `e` -- element `e` of it. Naming only a row marks
        !! every element of it, exactly as the table's own %set_null does.
        generic :: set_null => col_set_null_i32, col_set_null_i64, col_set_null_e32, col_set_null_e64
        procedure, private :: col_clear_null_i32  !! %clear_null specific, whole row, int32 index.
        procedure, private :: col_clear_null_i64  !! %clear_null specific, whole row, int64 index.
        procedure, private :: col_clear_null_e32  !! %clear_null specific, one element, int32 indices.
        procedure, private :: col_clear_null_e64  !! %clear_null specific, one element, int64 indices.
        !> Clears row `i`'s null, or -- given `e` -- element `e` of it. The stored VALUE is
        !! whatever was there; clearing a null does not write one.
        generic :: clear_null => col_clear_null_i32, col_clear_null_i64, col_clear_null_e32, col_clear_null_e64""")
    w("""
""")
    for k in PTR_KINDS:
        tag = k[0]
        w(f"        procedure, private :: col_ref_{tag} !! %ref specific, {tag} storage.")
    w("        procedure, private :: col_ref_strcol !! %ref specific, the packed string store.")
    for tag, pk, ctype in CONTAINERS:
        w(f"        procedure, private :: col_ref_{tag}col !! %ref specific, a {pk} container.")
    w("        !> Points `p` at this column's live storage -- the `%col` pointer, without the name")
    w("        !! lookup. Same rules: the kind must match exactly (a pointer never widens), a write")
    w("        !! through `p` changes the table, and REORDERING one column through its pointer")
    w("        !! breaks the table's row alignment with nothing to report it.")
    w("        generic :: ref => " + wrap_list(
        [f"col_ref_{k[0]}" for k in PTR_KINDS] + ["col_ref_strcol"]
        + [f"col_ref_{c[0]}col" for c in CONTAINERS], 12,
        first_prefix=len("        generic :: ref => ")))
    w("""        procedure :: is_valid => col_is_valid   !! Whether the handle is attached AND still current.
        procedure :: index => col_index         !! This column's 1-based position in the table.
        procedure :: kind => col_kind           !! This column's PK_* kind.
        procedure :: name => col_name           !! This column's name.
        procedure :: width => col_width         !! This column's values per row (1 for a scalar kind).
        procedure :: unit => col_unit           !! This column's unit string, or "".
        procedure :: residency => col_residency !! Whether this column is RES_EMPTY/RES_PARTIAL/RES_FULL.
        procedure :: set_user_populated => col_set_user_populated !! Claim this column's values as the caller's own, or unclaim.
        procedure :: is_user_populated => col_is_user_populated !! Whether this column is claimed as holding the caller's values.
        ! NO `final` -- see the type's own doc-comment. This is a decision, not an omission.
    end type parquet_table_col""")
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
def join_interface(name, rank, mkind):
    """One `%join` specific's interface body.

    Six of them differ only in how `on` is spelled (an array of names or one separated string)
    and whether they carry a `max_rows=` ceiling -- and, if they do, in its integer kind. That
    three-by-two shape is forced rather than chosen: CLAUDE.md's dual-kind rule wants both kinds
    for a row count, and an OPTIONAL dummy differing only by kind cannot disambiguate a generic
    (a call omitting it matches both), so the ceiling is REQUIRED in four of the six and the
    no-ceiling case is a specific of its own. Emitting them from one function is what keeps the
    other eleven arguments from being written out six times and drifting.
    """
    o = []
    a = o.append
    if mkind is None and rank == "array":
        a("""        !> Matches `other`'s rows against this table's on one or more key columns, keeps the rows
        !! `how` asks for, and brings `other`'s columns over.
        !!
        !! **`%join` MUTATES this table**, and detaches it from its file unless the result holds
        !! every one of its rows exactly once and in its original order -- which is a property of
        !! the DATA, not of `how`: a left join keeps every left row, but keeps it once only when
        !! each matched key is unique in `other`. On that path nothing is rewritten, no unread
        !! column becomes unreadable, the file stays open, and `%generation()` moves only if the
        !! new columns had to grow the slot array, which `%reserve_columns` lets a caller avoid --
        !! so an outstanding `%col` pointer can survive the whole operation. `%is_detached()`
        !! reports which of the two happened. `call t%clone(out)` first is the non-mutating form
        !! and needs no separate API -- a clone copies only the columns that are resident, so on a
        !! lazy table it is nearly free.
        !!
        !! **`how=` chooses which rows survive.** `"inner"` keeps only the rows that found a
        !! counterpart; `"left"` keeps every row of this table; `"right"` keeps every row of
        !! `other`; `"outer"` keeps both; `"semi"` keeps the rows of this table that matched and
        !! `"anti"` the ones that did not. All six are case-insensitive.
        !!
        !! **`"right"` and `"outer"` can emit a row with no counterpart HERE**, so this table's
        !! own columns are filled with nulls at those rows -- which means every column here has to
        !! be null-fillable, and a container column (list, map, struct) is refused under those two
        !! `how` values for the same reason an incoming one always is. Where `on` and `other_on`
        !! name the same column, the merged key takes `other`'s value at such a row, since this
        !! table has none to give.
        !!
        !! **`"semi"` and `"anti"` bring NOTHING across.** They answer a question about this
        !! table's own rows -- did each one find a counterpart -- so no column of `other` is
        !! carried, no name can clash, and `columns=` is refused rather than ignored.
        !!
        !! **Keys are NAMES, not sort keys.** `on` takes one column name per key, primary first,
        !! and a direction token (`"-id"`, `"id desc"`) is refused: a join is an equality test, and
        !! ordering the keys is the engine's business. `other_on` names the right-hand columns when
        !! they differ; absent means the same names. Two keys must have exactly the same kind and
        !! width -- nothing is promoted, because an identifier above 2**53 does not survive being
        !! promoted to a real, so cast one side with `%cast` first.
        !!
        !! **A null key matches nothing, including another null**, on either side; a NaN key is an
        !! ordinary value and matches every other NaN, which for a column full of NaNs is a
        !! cartesian product. Both are what `%sort_by` already means by "equal", because this runs
        !! the same engine over the same keys.
        !!
        !! **`columns=` absent carries every column of `other` that is already RESIDENT -- not
        !! every column it has**, on the four `how` values that carry anything at all. A freshly
        !! opened right table has none, so a join given neither
        !! `columns=` nor a prior `%materialize_all()` brings no payload across at all. Naming a
        !! column DOES read it, by the ordinary lazy first touch. The same rule governs THIS
        !! table on a join that DETACHES: a column of it that has not been read is skipped rather
        !! than read, and is then gone -- so materialize what you need before one. A join that
        !! keeps every row where it was reads and skips nothing, and leaves every unread column
        !! exactly as readable as it was.
        !!
        !! **The key column appears once**, taken from this table, when `on` and `other_on` name
        !! the same thing. When they differ both are kept, because the right key really is a
        !! different column. Any other incoming name that clashes takes `other_suffix` (default
        !! `"_2"`), and only the INCOMING column is renamed -- renaming a caller's own column as a
        !! side effect of adding one is a surprise a mutating API has no business springing. A
        !! suffixed name that still clashes is an error naming both.
        !!
        !! **`require=` is the assertion worth reaching for**, and it is read LEFT-SIDE-FIRST:
        !! `require="m:1"` says many rows here may share a key but each of them finds at most one
        !! row in `other`, which is the lookup-table annotation and the one join a caller is most
        !! likely to get silently wrong. It is checked before anything is allocated, so a
        !! duplicated row in a lookup table becomes a named error rather than a result 40 times
        !! the expected size. `max_rows=` is the weaker form for a caller who knows only the
        !! scale, and refuses the join on the count alone -- also before the allocation.
        !!
        !! **`matched=` is the diagnostic**: one entry per row of this table AS IT WAS ON ENTRY,
        !! which is the only coordinate system in which "did my object find a counterpart?" still
        !! has an answer once the join has mutated the table. `count(matched)` is what a
        !! cross-match script prints.
        !!
        !! **`pairs=` and `other_pairs=` hand back the match itself** -- one entry each per output
        !! row, naming the row of this table (numbered as on entry, like `matched=`) and the row of
        !! `other` that produced it, with **0 for "no counterpart on that side"**. That is the whole
        !! intermediate representation the join reasons over, and it is what applies the same match
        !! to something this table does not hold: an array of a derived type, a second table keyed
        !! the same way. It is the shape `pf_spatial_index%pairs_within` already returns for the
        !! positional case, and it costs nothing when not asked for.
        !!
        !! Rows come out in this table's original order, and within each, that row's matches in
        !! `other`'s original order, unless `order="key"` asks for the engine's own (key) order
        !! instead. An incoming column keeps its own `%unit`; `%get_file_metadata` still answers
        !! about this table's own file.""")
    elif rank == "array":
        a("""        !> %join with an `integer(<KIND>)` `max_rows=`; see `table_join` for everything else.
        !!
        !! A specific of its own because the ceiling is REQUIRED here: an optional dummy differing
        !! only by kind cannot disambiguate a generic, so `table_join` carries the no-ceiling case
        !! and these two carry the kinds.""".replace("<KIND>", mkind))
    elif mkind is None:
        a("""        !> %join over a separated key string ("id" or "ra,dec"); see `table_join` for everything
        !! else. `other_on` is a separated string here too, with one name per `on` name.""")
    else:
        a("""        !> %join over a separated key string with an `integer(<KIND>)` `max_rows=`; see
        !! `table_join` for everything else, and `table_join_max_i32` for why the ceiling is a
        !! specific rather than an optional argument.""".replace("<KIND>", mkind))
    args = ["self", "other", "on", "other_on", "how", "columns", "other_suffix", "require",
            "order"]
    if mkind is not None:
        args.append("max_rows")
    args += ["matched", "pairs", "other_pairs", "threads"]
    head = "        module subroutine " + name + "("
    a(head + wrap_list(args, 16, first_prefix=len(head)) + ")")
    a("            class(parquet_table), intent(inout) :: self !! the LEFT table; mutated in place.")
    a("            class(parquet_table), intent(in) :: other   !! the RIGHT table; only read from.")
    if rank == "array":
        a("            character(len=*), intent(in) :: on(:)       !! left key columns, primary first.")
        a("            !> right key columns; absent means the same names as `on`. One entry per `on` entry.")
        a("            character(len=*), intent(in), optional :: other_on(:)")
    else:
        a("            character(len=*), intent(in) :: on          !! left key columns, separated; primary first.")
        a("            character(len=*), intent(in), optional :: other_on !! right key columns, separated.")
    if mkind is None and rank == "array":
        a("""            !> `"inner"` (the default), `"left"`, `"right"`, `"outer"`, `"semi"` or `"anti"`,
            !! case-insensitive. See above for which rows each keeps.
            character(len=*), intent(in), optional :: how
            !> which of `other`'s non-key columns to bring over, separated by commas and/or
            !! semicolons. Absent carries the ones already resident; see above, because this is
            !! the argument a caller is most likely to be caught by. Refused with `how="semi"`
            !! and `how="anti"`, which carry no columns at all.
            character(len=*), intent(in), optional :: columns
            !> suffix for an incoming column whose name clashes with one here. Default `"_2"`.
            character(len=*), intent(in), optional :: other_suffix
            !> `"m:m"` (the default, no assertion), `"1:1"`, `"1:m"` or `"m:1"`, case-insensitive
            !! and read left-side-first -- so `"m:1"` asserts that `other`'s key is unique.
            character(len=*), intent(in), optional :: require
            !> `"left"` (the default: this table's rows in their own order) or `"key"` (the sort's
            !! own order), case-insensitive.
            character(len=*), intent(in), optional :: order""")
    else:
        a("            character(len=*), intent(in), optional :: how      !! join kind; see `table_join`.")
        a("            character(len=*), intent(in), optional :: columns  !! payload columns; see `table_join`.")
        a("            character(len=*), intent(in), optional :: other_suffix !! clash suffix; default \"_2\".")
        a("            character(len=*), intent(in), optional :: require  !! cardinality assertion; see `table_join`.")
        a("            character(len=*), intent(in), optional :: order    !! output ordering; see `table_join`.")
    if mkind is not None:
        a("            !> refuse the join rather than build a result with more rows than this. Checked")
        a("            !! against the counted output size before anything proportional to it is allocated.")
        a("            integer(<KIND>), intent(in) :: max_rows".replace("<KIND>", mkind))
    if mkind is None and rank == "array":
        a("""            !> per row of this table AS IT WAS ON ENTRY: .true. when that row found at least one
            !! counterpart in `other`. The only coordinate system the question still has an answer
            !! in once the join has run.
            logical, allocatable, intent(out), optional :: matched(:)
            !> per output row: the row of THIS table that produced it, numbered as the rows were on
            !! entry, or 0 when the output row has no counterpart here. The join's own
            !! intermediate representation; see above.
            integer(int64), allocatable, intent(out), optional :: pairs(:)
            !> per output row: the row of `other` that produced it, or 0 when there is none.
            !! `pairs`'s counterpart, and the same length.
            integer(int64), allocatable, intent(out), optional :: other_pairs(:)""")
    else:
        a("            logical, allocatable, intent(out), optional :: matched(:) !! see `table_join`.")
        a("            integer(int64), allocatable, intent(out), optional :: pairs(:) !! see `table_join`.")
        a("            !> per output row: `other`'s row, or 0; see `table_join`.")
        a("            integer(int64), allocatable, intent(out), optional :: other_pairs(:)")
    a("""            !> team size for the sort that builds the pair list, and for nothing else -- the
            !! column work that follows answers to `parquet_set_table_threads`, as every other
            !! row-structural mutation does. Absent means the automatic rule (`omp_get_max_threads()`
            !! capped by `parquet_set_sort_threads` and by the processors available); `threads=1`
            !! forces that sort serial. It never changes which rows come out, or their order.
            integer, intent(in), optional :: threads""")
    a("        end subroutine " + name)
    return "\n".join(o)


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
                sample_fraction, sample_seed, list_columns, bounded)
            type(parquet_table), intent(out) :: table !! the table to fill.
            character(len=*), intent(in) :: filename  !! parquet file to open.
            character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML file describing `filename`.
            type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
            type(parquet_sortkey), intent(in), optional :: sort !! sort keys, in INTERNAL column names.
            type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
            logical, intent(in), optional :: qc_soft !! warn on a qc violation instead of aborting.
            logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
            real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
            integer(int64), intent(in), optional :: sample_seed !! seed for that draw; omitted = nondeterministic.
            !! `integer(int64)` only, as everywhere in this library: a literal is `42_int64`.
            character(len=*), intent(in), optional :: list_columns !! `"auto"` (default) or `"container"`.
            logical, intent(in), optional :: bounded !! read one row group at a time; default .false.
            !! See the `parquet_open_table` generic above for what it does and what it refuses.
        end subroutine open_table_full
        !> Slice-regime open, int32 row bounds -- see the `parquet_open_table` generic above, which
        !! also explains why there is no `sort` argument here and what `filter=`/`sample_fraction=`
        !! do to the slice's row count.
        module subroutine open_table_slice_i32(table, filename, row_lo, row_hi, maml, filter, qc, &
                qc_soft, use_threads, sample_fraction, sample_seed, list_columns, bounded)
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
            integer(int64), intent(in), optional :: sample_seed !! seed for that draw; omitted = nondeterministic.
            !! `integer(int64)` only, as everywhere in this library: a literal is `42_int64`.
            character(len=*), intent(in), optional :: list_columns !! `"auto"` (default) or `"container"`.
            logical, intent(in), optional :: bounded !! accepted and inert: a slice is chunked already.
        end subroutine open_table_slice_i32
        !> Slice-regime open, int64 row bounds -- see the `parquet_open_table` generic above.
        module subroutine open_table_slice_i64(table, filename, row_lo, row_hi, maml, filter, qc, &
                qc_soft, use_threads, sample_fraction, sample_seed, list_columns, bounded)
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
            integer(int64), intent(in), optional :: sample_seed !! seed for that draw; omitted = nondeterministic.
            !! `integer(int64)` only, as everywhere in this library: a literal is `42_int64`.
            character(len=*), intent(in), optional :: list_columns !! `"auto"` (default) or `"container"`.
            logical, intent(in), optional :: bounded !! accepted and inert: a slice is chunked already.
        end subroutine open_table_slice_i64
        !> Opens a reader on `filename` with whatever read-time transform the table carries in its
        !! `read_*` components already attached -- `cache%reader` itself, or, when `rdr` is given,
        !! some other reader over the same file. One helper rather than several open calls, so that
        !! %clone's reopen and the parallel prefetch's per-thread readers cannot drift from
        !! parquet_open_table's own. That is what makes the prefetch gate's central claim -- *a
        !! freshly opened reader sees exactly what the table's reader sees* -- true by construction
        !! rather than by argument.
        !!
        !! **`rdr` is optional rather than required, and that is forced rather than chosen.**
        !! Passing `t%cache` and `t%cache%reader` to one call would associate two dummy arguments
        !! with overlapping storage, which Fortran forbids as soon as either is defined. So the
        !! cache's own reader is reached through the cache, and `rdr` names any OTHER reader.
        !!
        !! **`cache%rg_bounds` is written only when `rdr` is absent**, and the two conditions are
        !! deliberately the same one rather than a separate argument. Those bounds describe the
        !! cache's own reader, so only the open that creates that reader may write them; a
        !! per-thread open must not, both because the bounds are already correct and because
        !! several threads writing one component of a shared cache is a data race. Deriving it from
        !! `present(rdr)` rather than taking a `set_bounds` flag removes the only way to get it
        !! wrong.
        !!
        !! Each half is passed through an ALLOCATABLE local left unallocated when that half is
        !! empty: an unallocated allocatable actual makes an optional dummy absent (F2018
        !! 15.5.2.12), so one unconditional call covers every combination -- and a table with no
        !! transform at all reaches parquet_open_reader with exactly the arguments it always did.
        !!
        !! **A BOUNDED whole-file table is the second case where the filter is not a constructor
        !! argument**, for a different reason with the same shape: passing `filter=` to
        !! `parquet_open_reader` selects the CACHING whole-file engine, and `bounded=.true.` asks
        !! for the row-group-scoped one, so the filter is attached afterwards with
        !! `parquet_reader_set_filter(r, filter, 0, 0)` -- `0, 0` being "every row group", which is
        !! a whole-file table's scope. Unlike the masked slice it leaves `cache%rg_bounds`
        !! UNALLOCATED: those bounds are a slice-regime object that `%row_group_bounds` and
        !! `resolve_width_row_groups` branch on, so `materialize_slice` builds its own instead.
        !!
        !! **A masked slice is the one case where the filter is NOT a constructor argument**: it
        !! carries the slice's own row range, which only `parquet_reader_set_filter` can express,
        !! so it is attached immediately after the open instead. That whole sequence lives here
        !! rather than in the caller for the same reason the rest does -- so `%clone`'s reopen
        !! cannot produce a reader in a different state from the one `parquet_open_table` built.
        !! Requires `cache%slice_row_lo`/`slice_row_hi` and `cache%rg_bounds_physical` to be set
        !! already, and leaves `cache%rg_bounds` holding the resulting per-row-group survivor
        !! counts.
        module subroutine table_open_reader_with_transform(cache, filename, rdr, use_threads)
            type(parquet_table_cache), intent(inout), target :: cache !! the table's store, holding the transform.
            character(len=*), intent(in) :: filename    !! parquet file to open.
            type(parquet_reader), intent(inout), optional, target :: rdr !! reader to open instead of cache%reader.
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
        !> Whether a column holds (or may hold) nulls, by 1-based position.
        module function table_has_nulls_at(self, j, found) result(any_null)
            class(parquet_table), intent(in) :: self  !! the table.
            integer, intent(in) :: j                  !! 1-based column position.
            logical, intent(out), optional :: found   !! present: report an out-of-range j instead of aborting.
            logical :: any_null                       !! .true. if it holds (or may hold) a null.
        end function table_has_nulls_at
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
        !> Which of `names` the table does not have, as a packed array sized to the longest one
        !! reported -- zero-size, `len=1`, when nothing is missing, so `size(absent) == 0` is the
        !! test. Reads nothing: this is a metadata query, and a column's existence is a property
        !! of the descriptor.
        !!
        !! Matching is EXACT, as `%has_column`'s is, never by struct-path prefix the way
        !! `%prefetch` accepts one -- a required column is a specific column. Name a struct leaf
        !! in full, or give it an internal name with a MAML `col_map:` remap.
        !!
        !! `parquet_row_index` counts as present on a file-backed table even before anything has
        !! asked for it, exactly as `%has_column` reports it: the question is "can I use this
        !! name?", and the answer is yes until the table detaches.
        module subroutine missing_columns_array(self, names, absent)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: names(:)               !! column names to look for.
            character(len=:), allocatable, intent(out) :: absent(:) !! the ones that are not there.
        end subroutine missing_columns_array
        !> %missing_columns over one string of names separated by commas and/or semicolons
        !! ("ra;dec, mag"), split by the same tokenizer %prefetch and parquet_prefetch_columns use.
        module subroutine missing_columns_string(self, names, absent)
            class(parquet_table), intent(in) :: self               !! the table.
            character(len=*), intent(in) :: names                  !! names, comma/semicolon separated.
            character(len=:), allocatable, intent(out) :: absent(:) !! the ones that are not there.
        end subroutine missing_columns_string
        !> Aborts unless the table has every one of `names`, naming **every** missing column
        !! rather than only the first -- which is the whole point, since a hand-written
        !! `%has_column` loop reports one name per run and a caller then fixes them one at a time.
        !!
        !! The message also echoes what was ASKED for, which is how it carries context without
        !! taking a caller-supplied message string; both lists are truncated to a preview, because
        !! interpolating unbounded caller text into an `error stop` corrupts the heap on ifx.
        !!
        !! Same exact matching and same `parquet_row_index` rule as `%missing_columns`, which does
        !! the work.
        module subroutine require_columns_array(self, names)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: names(:) !! column names that must all exist.
        end subroutine require_columns_array
        !> %require_columns over one string of names separated by commas and/or semicolons.
        module subroutine require_columns_string(self, names)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: names    !! names, comma/semicolon separated.
        end subroutine require_columns_string
        !> A column's 1-based position among the table's columns, or 0 when there is no such
        !! column. The inverse of `%column_name`, and the cheap way to hoist a lookup out of a
        !! loop that then queries the same column by position.
        module function table_column_index(self, name, found) result(j)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: j                              !! 1-based position, or 0 on a reported miss.
        end function table_column_index
        !> Copies out the name of the column at 1-based position `j`. The inverse of
        !! `%column_index`, and what makes a `do j = 1, t%ncols()` sweep able to report itself.
        module subroutine table_column_name(self, j, nm, found)
            class(parquet_table), intent(in) :: self             !! the table.
            integer, intent(in) :: j                             !! 1-based column position.
            character(len=:), allocatable, intent(out) :: nm     !! the column's name, or "".
            logical, intent(out), optional :: found              !! present: report an out-of-range j instead of aborting.
        end subroutine table_column_name
        !> A column's PK_* kind discriminator (PK_NONE for an unsupported column).
        module function table_column_kind(self, name, found) result(k)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: k                              !! the PK_* constant.
        end function table_column_kind
        !> A column's PK_* kind discriminator, by 1-based position.
        module function table_column_kind_at(self, j, found) result(k)
            class(parquet_table), intent(in) :: self  !! the table.
            integer, intent(in) :: j                  !! 1-based column position.
            logical, intent(out), optional :: found   !! present: report an out-of-range j instead of aborting.
            integer :: k                              !! the PK_* constant.
        end function table_column_kind_at
        !> A column's values-per-row: 1 for a scalar kind, the vector width for a *_VEC kind.
        module function table_column_width(self, name, found) result(wdt)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: wdt                            !! values per row.
        end function table_column_width
        !> A column's values-per-row, by 1-based position.
        module function table_column_width_at(self, j, found) result(wdt)
            class(parquet_table), intent(in) :: self  !! the table.
            integer, intent(in) :: j                  !! 1-based column position.
            logical, intent(out), optional :: found   !! present: report an out-of-range j instead of aborting.
            integer :: wdt                            !! values per row.
        end function table_column_width_at
        !> Copies out a column's unit string ("" when it has none).
        module subroutine table_column_unit(self, name, u, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            character(len=:), allocatable, intent(out) :: u      !! the unit, or "".
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine table_column_unit
        !> Copies out a column's unit string, by 1-based position.
        module subroutine table_column_unit_at(self, j, u, found)
            class(parquet_table), intent(in) :: self             !! the table.
            integer, intent(in) :: j                             !! 1-based column position.
            character(len=:), allocatable, intent(out) :: u      !! the unit, or "".
            logical, intent(out), optional :: found              !! present: report an out-of-range j instead of aborting.
        end subroutine table_column_unit_at
        !> A column's residency: RES_FULL once read, RES_EMPTY for an unsupported column.
        module function table_column_residency(self, name, found) result(r)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            integer :: r                              !! the RES_* constant.
        end function table_column_residency
        !> A column's RES_* residency state, by 1-based position.
        module function table_column_residency_at(self, j, found) result(r)
            class(parquet_table), intent(in) :: self  !! the table.
            integer, intent(in) :: j                  !! 1-based column position.
            logical, intent(out), optional :: found   !! present: report an out-of-range j instead of aborting.
            integer :: r                              !! the RES_* constant.
        end function table_column_residency_at
        !> Whether a column's physical TYPE is one this library can read. It answers from the
        !> schema alone and reads no data, so for a plain LIST/LARGE_LIST column it is a
        !> statement about the element type, not a promise that the read will succeed: such a
        !> column carries no width in the schema, and whether one uniform width exists is a
        !> property of the DATA. A `list<int32>` whose rows all hold 3 elements is an ordinary
        !> vector column of width 3; one whose rows differ in length is rejected when it is read.
        !> Both answer .true. here. Every other type is settled by the schema, so for them this
        !> does predict the read. Use %width (which resolves a deferred width for real) when the
        !> distinction matters.
        module function table_is_supported(self, name, found) result(ok)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            logical :: ok                             !! .true. if the column's TYPE is readable.
        end function table_is_supported
        !> Whether a column's type can be read, by 1-based position -- see table_is_supported
        !> above for the plain-LIST caveat.
        module function table_is_supported_at(self, j, found) result(ok)
            class(parquet_table), intent(in) :: self  !! the table.
            integer, intent(in) :: j                  !! 1-based column position.
            logical, intent(out), optional :: found   !! present: report an out-of-range j instead of aborting.
            logical :: ok                             !! .true. if the column's TYPE is readable.
        end function table_is_supported_at
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
        !> Whether row `i` of the column at 1-based position `j` is null.
        module function table_is_null_at_i32(self, j, i, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: j                 !! 1-based column position.
            integer(int32), intent(in) :: i          !! 1-based row index.
            logical, intent(out), optional :: found  !! present: report an out-of-range j instead of aborting.
            logical :: isnull                        !! .true. if null (.false. on a miss).
        end function table_is_null_at_i32
        !> Whether row `i` of the column at 1-based position `j` is null.
        module function table_is_null_at_i64(self, j, i, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: j                 !! 1-based column position.
            integer(int64), intent(in) :: i          !! 1-based row index.
            logical, intent(out), optional :: found  !! present: report an out-of-range j instead of aborting.
            logical :: isnull                        !! .true. if null (.false. on a miss).
        end function table_is_null_at_i64
        !> Whether element `e` of row `i` of the column at 1-based position `j` is null.
        module function table_is_null_at_e32(self, j, i, e, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: j                 !! 1-based column position.
            integer(int32), intent(in) :: i          !! 1-based row index.
            integer(int32), intent(in) :: e          !! 1-based element index within the row.
            logical, intent(out), optional :: found  !! present: report an out-of-range j instead of aborting.
            logical :: isnull                        !! .true. if null (.false. on a miss).
        end function table_is_null_at_e32
        !> Whether element `e` of row `i` of the column at 1-based position `j` is null.
        module function table_is_null_at_e64(self, j, i, e, found) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: j                 !! 1-based column position.
            integer(int64), intent(in) :: i          !! 1-based row index.
            integer(int64), intent(in) :: e          !! 1-based element index within the row.
            logical, intent(out), optional :: found  !! present: report an out-of-range j instead of aborting.
            logical :: isnull                        !! .true. if null (.false. on a miss).
        end function table_is_null_at_e64
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
        !> A resolved handle on the named column. Honours `found=` exactly as every other
        !! name-taking query does; on a reported miss the handle comes back detached, so
        !! `%is_valid()` is `.false.` and using it aborts.
        module subroutine column_by_name(self, name, c, found)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: name              !! column name.
            type(parquet_table_col), intent(out) :: c         !! the resolved handle.
            logical, intent(out), optional :: found           !! present: report a miss instead of aborting.
        end subroutine column_by_name
        !> A resolved handle on the column at 1-based position `j`.
        module subroutine column_by_index(self, j, c, found)
            class(parquet_table), intent(in) :: self          !! the table.
            integer, intent(in) :: j                          !! 1-based column position.
            type(parquet_table_col), intent(out) :: c         !! the resolved handle.
            logical, intent(out), optional :: found           !! present: report an out-of-range j instead of aborting.
        end subroutine column_by_index
        !> Resolves `name` straight into a handle, carrying the CALLER's procedure name so the
        !! messages a delegating `%get_element` produces are the ones it always produced. This is
        !! the entry `%column` itself uses, with `proc` = "column".
        module subroutine table_resolve_to_handle(self, name, proc, c, found)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: name              !! column name.
            character(len=*), intent(in) :: proc              !! calling procedure, for the message.
            type(parquet_table_col), intent(out) :: c         !! the resolved handle.
            logical, intent(out), optional :: found           !! present: report a miss instead of aborting.
        end subroutine table_resolve_to_handle
        !> Whether the handle is attached to a table AND still current -- one predicate, because
        !! a caller can do nothing useful with a handle that is one and not the other.
        module function col_is_valid(self) result(ok)
            class(parquet_table_col), intent(in) :: self !! the handle.
            logical :: ok                                !! .true. when it can still be used.
        end function col_is_valid
        !> This column's 1-based position in the table.
        module function col_index(self) result(j)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer :: j                                 !! 1-based position.
        end function col_index
        !> This column's PK_* kind, as resolved when the handle was made.
        module function col_kind(self) result(k)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer :: k                                 !! the PK_* constant.
        end function col_kind
        !> Whether that row is null (handle form, i32 indices).
        module function col_is_null_i32(self, i) result(isnull)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int32), intent(in) :: i              !! 1-based row index.
            logical :: isnull                        !! .true. if null.
        end function col_is_null_i32
        !> Whether that row is null (handle form, i64 indices).
        module function col_is_null_i64(self, i) result(isnull)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! 1-based row index.
            logical :: isnull                        !! .true. if null.
        end function col_is_null_i64
        !> Whether that element is null (handle form, e32 indices).
        module function col_is_null_e32(self, i, e) result(isnull)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int32), intent(in) :: i              !! 1-based row index.
            integer(int32), intent(in) :: e              !! 1-based element index within the row.
            logical :: isnull                        !! .true. if null.
        end function col_is_null_e32
        !> Whether that element is null (handle form, e64 indices).
        module function col_is_null_e64(self, i, e) result(isnull)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
            logical :: isnull                        !! .true. if null.
        end function col_is_null_e64
        !> Marks that row's null (handle form, i32 indices).
        module subroutine col_set_null_i32(self, i)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int32), intent(in) :: i              !! 1-based row index.
        end subroutine col_set_null_i32
        !> Marks that row's null (handle form, i64 indices).
        module subroutine col_set_null_i64(self, i)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine col_set_null_i64
        !> Marks that element's null (handle form, e32 indices).
        module subroutine col_set_null_e32(self, i, e)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int32), intent(in) :: i              !! 1-based row index.
            integer(int32), intent(in) :: e              !! 1-based element index within the row.
        end subroutine col_set_null_e32
        !> Marks that element's null (handle form, e64 indices).
        module subroutine col_set_null_e64(self, i, e)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
        end subroutine col_set_null_e64
        !> Clears that row's null (handle form, i32 indices).
        module subroutine col_clear_null_i32(self, i)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int32), intent(in) :: i              !! 1-based row index.
        end subroutine col_clear_null_i32
        !> Clears that row's null (handle form, i64 indices).
        module subroutine col_clear_null_i64(self, i)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! 1-based row index.
        end subroutine col_clear_null_i64
        !> Clears that element's null (handle form, e32 indices).
        module subroutine col_clear_null_e32(self, i, e)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int32), intent(in) :: i              !! 1-based row index.
            integer(int32), intent(in) :: e              !! 1-based element index within the row.
        end subroutine col_clear_null_e32
        !> Clears that element's null (handle form, e64 indices).
        module subroutine col_clear_null_e64(self, i, e)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! 1-based row index.
            integer(int64), intent(in) :: e              !! 1-based element index within the row.
        end subroutine col_clear_null_e64
        !> Aborts unless the handle is attached and current, naming the remedy -- the cause of a
        !! stale handle is usually several statements away from where it is noticed.
        module subroutine col_resolve(self, proc)
            class(parquet_table_col), intent(in) :: self !! the handle.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
        end subroutine col_resolve
        !> This column's name.
        module subroutine col_name(self, nm)
            class(parquet_table_col), intent(in) :: self     !! the handle.
            character(len=:), allocatable, intent(out) :: nm !! receives the name.
        end subroutine col_name
        !> This column's values per row -- 1 for a scalar kind, the vector length otherwise.
        module function col_width(self) result(wdt)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer :: wdt                               !! values per row.
        end function col_width
        !> This column's unit string, or "" when it has none.
        module subroutine col_unit(self, u)
            class(parquet_table_col), intent(in) :: self    !! the handle.
            character(len=:), allocatable, intent(out) :: u !! receives the unit, or "".
        end subroutine col_unit
        !> Whether this column is RES_EMPTY, RES_PARTIAL or RES_FULL.
        module function col_residency(self) result(r)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer :: r                                 !! one of the RES_* constants.
        end function col_residency
        !> Marks this column as holding values the CALLER wrote, or clears that mark.
        !!
        !! The handle form of `%set_user_populated`, and the one the case it exists for actually
        !! reaches for: `%ref` is a handle method, so a caller who edits through the pointer it
        !! hands out already has the handle. See parquet_table%set_user_populated for the rule.
        module subroutine col_set_user_populated(self, flag)
            class(parquet_table_col), intent(in) :: self !! the handle.
            logical, intent(in) :: flag                  !! .true. = the caller's own values; .false. = the file's.
        end subroutine col_set_user_populated
        !> Whether this column is marked as holding values the caller wrote rather than the file's.
        module function col_is_user_populated(self) result(ok)
            class(parquet_table_col), intent(in) :: self !! the handle.
            logical :: ok                                !! .true. when the slot holds the caller's own writes.
        end function col_is_user_populated
        !> Aborts unless `i` is a valid 1-based row index for the handle's own row scope.
        module subroutine col_require_row(self, i, proc)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer(int64), intent(in) :: i              !! the row index to check.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
        end subroutine col_require_row
        !> Aborts unless `e` is a valid 1-based element index within one of this column's rows.
        !!
        !! Not declared here on purpose: `parquet_column%get_elem`/`%set_elem` run `check_element`
        !! themselves, and its message already names the ELEMENT axis rather than reading like an
        !! out-of-range row. A second check in this layer would be a second copy of the column's
        !! own width rule.
        !> Reports that this column's kind cannot serve the caller's variable.
        module subroutine col_kind_error(self, want, proc)
            class(parquet_table_col), intent(in) :: self !! the handle.
            integer, intent(in) :: want                  !! the PK_* the caller asked for.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
        end subroutine col_kind_error
""")
    for k in KINDS:
        tag, _pk, decl, _comp, _rank, _cat = k
        w(f"        !> The shared {tag} body behind both `%get_element(name, i, v)` and a column")
        w("        !! handle's `%get(i, v)`: the widening set, the kind error and the null rule,")
        w("        !! once. Takes the resolved pieces rather than a handle -- building one purely")
        w("        !! to pass it measured +16.3% on `%get_element`. `proc` is the CALLER's name, so")
        w("        !! each entry point keeps the messages it always produced.")
        w(f"        module subroutine col_fetch_{tag}(cache, slot, colkind, i, value, proc)")
        w(f"            type(parquet_table_cache), intent(in){fetch_cache_attr(k)} :: cache "
          "!! the table's column store.")
        w("            integer, intent(in) :: slot                    !! validated slot index.")
        w("            integer, intent(in) :: colkind                 !! that slot's PK_* kind.")
        w("            integer(int64), intent(in) :: i                !! validated 1-based row index.")
        w(f"            {fetch_val_decl(k)} !! receives the value.")
        w("            character(len=*), intent(in) :: proc           !! calling procedure, for the message.")
        w(f"        end subroutine col_fetch_{tag}")
        w(f"        !> The shared {tag} body behind both `%set_element(name, i, v)` and a column")
        w("        !! handle's `%set(i, v)`. Exact kind, never widening -- a write that silently")
        w("        !! converted would lose information the caller did not agree to lose.")
        w(f"        module subroutine col_store_{tag}(cache, slot, i, value, proc)")
        w("            type(parquet_table_cache), intent(inout) :: cache !! the table's column store.")
        w("            integer, intent(in) :: slot                    !! validated slot index.")
        w("            integer(int64), intent(in) :: i                !! validated 1-based row index.")
        w(f"            {store_val_decl(k)} !! the value to write.")
        w("            character(len=*), intent(in) :: proc           !! calling procedure, for the message.")
        w(f"        end subroutine col_store_{tag}")
    for k in KINDS:
        tag, _pk, decl, _comp, _rank, _cat = k
        for ik, ikdecl in (("i32", "integer(int32)"), ("i64", "integer(int64)")):
            w(f"        !> Writes one row's {tag} value through a handle ({ik} row index).")
            w(f"        module subroutine col_set_{tag}_{ik}(self, i, value)")
            w("            class(parquet_table_col), intent(in) :: self !! the handle.")
            w(f"            {ikdecl}, intent(in) :: i               !! 1-based row index.")
            w(f"            {store_val_decl(k)} !! the value to write.")
            w(f"        end subroutine col_set_{tag}_{ik}")
    w("""
""")
    for k in KINDS:
        tag, _pk, decl, _comp, _rank, _cat = k
        for ik, ikdecl in (("i32", "integer(int32)"), ("i64", "integer(int64)")):
            w(f"        !> One row's {tag} value through a handle ({ik} row index).")
            w(f"        module subroutine col_get_{tag}_{ik}(self, i, value)")
            w("            class(parquet_table_col), intent(in) :: self !! the handle.")
            w(f"            {ikdecl}, intent(in) :: i              !! 1-based row index.")
            w(f"            {fetch_val_decl(k)} !! receives the value.")
            w(f"        end subroutine col_get_{tag}_{ik}")
    for k in VEC_KINDS:
        tag, _pk, decl, _comp, _rank, cat = k
        rd = "character(len=:), allocatable" if cat == "str" else decl
        wr = "character(len=*)" if cat == "str" else decl
        for ik, ikdecl in (("e32", "integer(int32)"), ("e64", "integer(int64)")):
            w(f"        !> ONE ELEMENT of one row of a {tag} column, without materialising the")
            w("        !! row ({ik} indices). The table has no name-taking counterpart -- reading"
              .replace("{ik}", ik))
            w("        !! a single element of a vector row is new capability, not a faster spelling.")
            w(f"        module subroutine col_get_{tag}_{ik}(self, i, e, value)")
            w("            class(parquet_table_col), intent(in) :: self !! the handle.")
            w(f"            {ikdecl}, intent(in) :: i              !! 1-based row index.")
            w(f"            {ikdecl}, intent(in) :: e              !! 1-based element index within the row.")
            w(f"            {rd}, intent(out) :: value             !! receives the value.")
            w(f"        end subroutine col_get_{tag}_{ik}")
            w(f"        !> Writes ONE ELEMENT of one row of a {tag} column ({ik} indices). Exact")
            w("        !! kind, and the write clears that element's null -- not the whole row's.")
            w(f"        module subroutine col_set_{tag}_{ik}(self, i, e, value)")
            w("            class(parquet_table_col), intent(in) :: self !! the handle.")
            w(f"            {ikdecl}, intent(in) :: i              !! 1-based row index.")
            w(f"            {ikdecl}, intent(in) :: e              !! 1-based element index within the row.")
            w(f"            {wr}, intent(in) :: value              !! the value to write.")
            w(f"        end subroutine col_set_{tag}_{ik}")
    w("""
        !> The TAIL of `table_resolve`, on a slot that is already known good: the unsupported-type
        !! refusal, the lazy first touch, and the shared-write rule. Split out so that a caller
        !! holding a slot index -- `table_resolve` after its name lookup, and anything reaching a
        !! column by position -- runs exactly the same checks in exactly the same order, without a
        !! second copy of them existing to drift.
        !!
        !! **Assumes `table_check_open` and `table_check_no_append` have already run** and that
        !! `idx` is in 1..ncols. Those live in the callers because `table_resolve` must do them
        !! before its lookup, and repeating them here would put a redundant test on the per-cell
        !! path this split exists not to slow down.
        module subroutine table_resolve_slot(self, idx, proc, found, writing)
            class(parquet_table), intent(in) :: self  !! the table.
            integer, intent(inout) :: idx             !! validated slot index in; 0 out on a reported refusal.
            character(len=*), intent(in) :: proc      !! calling procedure, for the message.
            logical, intent(out), optional :: found   !! present: report a refusal instead of aborting.
            logical, intent(in), optional :: writing  !! .true. when the caller is about to write.
        end subroutine table_resolve_slot
        !> The by-POSITION twin of `table_lookup_or_fail`: validates that `j` is a 1-based column
        !! position this table has, honouring `found=` and otherwise aborting. Shared by every
        !! index-form introspection query so they all bounds-check and report the same way.
        module subroutine table_slot_or_fail(self, j, proc, idx, found)
            class(parquet_table), intent(in) :: self !! the table.
            integer, intent(in) :: j                 !! 1-based column position to validate.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
            integer, intent(out) :: idx              !! the slot index (== j), or 0 on a reported miss.
            logical, intent(out), optional :: found  !! present: report an out-of-range j instead of aborting.
        end subroutine table_slot_or_fail
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
        !!
        !! `cache` is intent(inout) only because `parquet_column%row_validity` is: a temporal
        !! kind refreshes its cached null flag while answering. Nothing here changes the store.
        module subroutine table_valid_mask_of(cache, idx, mask)
            type(parquet_table_cache), intent(inout) :: cache !! the table's store.
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
        !!
        !! `cache` is intent(inout) for the same reason `table_valid_mask_of`'s is.
        module subroutine table_valid_mask_of_elem(cache, idx, mask)
            type(parquet_table_cache), intent(inout) :: cache !! the table's store.
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
        !> The CACHE-and-slot twin of `table_check_shared_write`, for callers holding a resolved
        !! slot rather than a table. `table_check_shared_write` delegates to it, so a handle and a
        !! name enforce the same two concurrency rules -- the string-store rule and the
        !! first-null-allocates-validity rule -- from one body.
        module subroutine cache_check_shared_write(cache, idx, proc, nulling)
            type(parquet_table_cache), intent(in) :: cache !! the table's column store.
            integer, intent(in) :: idx               !! slot index.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
            logical, intent(in) :: nulling           !! .true. when the write would create a null.
        end subroutine cache_check_shared_write
        !> The CACHE-and-slot twin of `table_require_kind`, for callers that hold a resolved slot
        !! rather than a table -- a column handle, and every shared `col_fetch_*`/`col_store_*`
        !! body. `table_require_kind` delegates to it, so there is exactly one wording of this
        !! message: a handle and a name must report a kind mismatch identically or the shared body
        !! has not actually stopped the two forms diverging.
        module subroutine cache_require_kind(cache, idx, kind, proc)
            type(parquet_table_cache), intent(in) :: cache !! the table's column store.
            integer, intent(in) :: idx               !! slot index.
            integer, intent(in) :: kind              !! required PK_* discriminator.
            character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        end subroutine cache_require_kind
        !> Aborts unless slot `idx`'s STORED kind is exactly `kind`, with the POINTER path's own
        !! message rather than `cache_require_kind`'s.
        !!
        !! Separate from `cache_require_kind` because the remedy is different and worth saying: a
        !! copying accessor would have widened here, and the caller reaching for a pointer needs to
        !! be told that is why this one will not. Shared by the table's `%col` and a column
        !! handle's `%ref`, which are the same operation reached two ways.
        module subroutine cache_require_ptr_kind(cache, idx, kind, proc)
            type(parquet_table_cache), intent(in) :: cache !! the table's column store.
            integer, intent(in) :: idx                     !! validated slot index.
            integer, intent(in) :: kind                    !! the PK_* the pointer requires.
            character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        end subroutine cache_require_ptr_kind
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
        !! a struct's leaves stay adjacent for the batch release policy, and two internal names
        !! reading one column land in adjacent slots.
        !!
        !! A physical column whose own name is claimed as an INTERNAL name by some remap entry is
        !! skipped: that is the deliberate shadow, not an error.
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
        !> TEST-ONLY -- forces this table's "an append is in flight"/"a read is in flight" counters,
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
        !> Exposes the parallel single-column read's validity-block alignment arithmetic for testing.
        !!
        !! **This is a debug hook, not API**, public for the same reason
        !! `parquet_debug_table_set_inflight` is: the procedure it forwards to lives in a submodule
        !! and the property it computes cannot be observed from outside.
        !!
        !! What it computes is which rows of `lo..hi` occupy WHOLE validity-bitmap blocks, and it is
        !! the one thing standing between the parallel column read and a silent wrong answer -- two
        !! threads pasting adjacent row groups share a bitmap block unless their ranges are trimmed
        !! to this. That race is a few instructions wide, so an end-to-end test cannot be relied on
        !! to catch a mistake in it; this makes the rule itself assertable, exactly as
        !! `parquet_debug_string_row_ranges` does for `parquet_string_column`'s byte-aligned split.
        !! `mid_lo > mid_hi` reports that no whole block exists in the range.
        module subroutine parquet_debug_colread_block_rows(lo, hi, width, mid_lo, mid_hi)
            integer(int64), intent(in) :: lo      !! first row of the range.
            integer(int64), intent(in) :: hi      !! last row of the range.
            integer(int64), intent(in) :: width   !! elements per row.
            integer(int64), intent(out) :: mid_lo !! first row occupying a whole block.
            integer(int64), intent(out) :: mid_hi !! last such row; < mid_lo when there is none.
        end subroutine parquet_debug_colread_block_rows
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
        !! can refuse. Paired with `table_read_exit` around the two LONG windows only -- a lazy
        !! first touch and the bulk materialize -- never around a read of a column that is already
        !! resident, whichever accessor asked for it; see parquet_table_cache's `readers_active`
        !! comment for why that asymmetry is deliberate.
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
        !> Collects the slots a row-structural mutation will actually rewrite, in slot order.
        !!
        !! Exists so the serial loop, the parallel loop and the caller that hoists the first column
        !! out of it cannot disagree about which columns are touched -- the same reason
        !! `materialize_wanted` is factored out of the prefetch paths. The predicate is
        !! `table_mutable_column` and nothing else.
        module subroutine table_mutable_slots(self, slots)
            class(parquet_table), intent(in) :: self          !! the table.
            integer, allocatable, intent(out) :: slots(:)     !! the rewritable slot indices, in order.
        end subroutine table_mutable_slots
        !> Runs one row-structural operation over `slots`, on several threads when that is worth
        !! doing and serially otherwise. The single entry point for the whole parallel-mutation
        !! path: the caller never sees the gate and there is no second copy of the serial loop.
        !!
        !! `op` selects the operation (`PCW_*`), and the two optional arrays carry its argument:
        !! `rows` for `PCW_REINDEX_TRUSTED` (a permutation) and `PCW_GATHER` (a selection), `keep`
        !! for `PCW_DELETE_MASK`. Exactly one is expected per op.
        !!
        !! **Every column in `slots` is rewritten, and nothing else is touched** -- no counter, no
        !! flag, no cache-level field. The `generation` bump and the detach stay with the caller,
        !! after this returns, exactly as they were around the serial loop.
        module subroutine table_colwork(cache, op, slots, rows, keep)
            type(parquet_table_cache), intent(inout) :: cache      !! the column store.
            integer, intent(in) :: op                              !! which operation; a PCW_* constant.
            integer, intent(in) :: slots(:)                        !! slots to rewrite, from table_mutable_slots.
            integer(int64), intent(in), optional :: rows(:)        !! permutation or selection, per `op`.
            logical, intent(in), optional :: keep(:)               !! per-row keep mask, per `op`.
        end subroutine table_colwork
        !> Deep-copies `slots` from one column store into another, on several threads when that is
        !! worth doing and serially otherwise. `%clone`'s counterpart to `table_colwork`.
        !!
        !! **A separate entry point rather than another `PCW_*` op, because a clone has TWO stores
        !! and every other operation has one.** Adding an optional destination to `table_colwork`
        !! would put an argument on an op-code dispatcher that exactly one op uses, and a future op
        !! that ignored it would be a silent wrong answer rather than a compile error.
        !!
        !! The caller has already copied every descriptor and is responsible for deciding which
        !! slots are resident; this copies values and nothing else.
        module subroutine table_colwork_clone(src, dst, slots)
            type(parquet_table_cache), intent(in) :: src           !! the source column store.
            type(parquet_table_cache), intent(inout) :: dst        !! the destination column store.
            integer, intent(in) :: slots(:)                        !! resident slots to copy, in order.
        end subroutine table_colwork_clone
        !> Carries `sslots` of one column store over into `dslots` of another, gathering each by
        !! `idx` and marking the rows `valid` says have no counterpart null. The JOIN's per-column
        !! rewrite, and `table_colwork_clone`'s sibling: two stores again, and again a separate
        !! entry point rather than another `PCW_*` op for the reason given there.
        !!
        !! Three shipped bindings, in this order, and no new per-kind primitive at all: a
        !! `deep_copy` into the destination slot, a `%gather` naming the source row for each
        !! output row, and -- only where something is unmatched -- a `%set_validity` marking those
        !! rows null. `%set_validity` never CLEARS a null, so the source column's own nulls
        !! survive the gather untouched and no read-back of the mask is needed.
        !!
        !! `idx` therefore names a real source row for EVERY output row, unmatched ones included
        !! (they are given row 1, whose values the mask then declares null and which nothing may
        !! read). The one case that cannot express is a source column with no rows at all, where
        !! there is no row 1 to name: those columns are built by appending `size(idx)` null rows
        !! to the empty copy instead.
        !!
        !! The destination slots must already exist -- `table_new_slot` reallocates the slot
        !! array, which cannot happen while threads hold descriptors into it.
        module subroutine table_colwork_join(src, dst, sslots, dslots, idx, valid)
            type(parquet_table_cache), intent(in) :: src           !! the source (right) column store.
            type(parquet_table_cache), intent(inout) :: dst        !! the destination (left) column store.
            integer, intent(in) :: sslots(:)                       !! source slots to carry, in order.
            integer, intent(in) :: dslots(:)                       !! destination slot for each, same size.
            integer(int64), intent(in) :: idx(:)                   !! per output row: source row, never 0.
            !> per output row: .false. where that row has no counterpart and must become null.
            !! Pass an UNALLOCATED array when nothing is unmatched -- an unallocated allocatable
            !! actual makes an optional dummy absent (F2018 15.5.2.12), which is how an inner join
            !! skips the whole `%set_validity` pass without the caller branching.
            logical, intent(in), optional :: valid(:)
        end subroutine table_colwork_join
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
        !> Reads the named columns now rather than on first touch. A column already resident is
        !! left alone; an unsupported one is an error, since asking to read something unreadable
        !! is a mistake worth hearing about.
        !!
        !! `names` may list SEVERAL columns separated by commas and/or semicolons
        !! ("ra;dec, mag"), which is the same spelling parquet_prefetch_columns has taken since
        !! 1.0.0 and goes through the same tokenizer. Blanks around a name are trimmed and an
        !! empty token is ignored, so a trailing or repeated separator is harmless. Prefer this
        !! over a fixed-length array constructor: a too-short declared length there silently
        !! TRUNCATES a name rather than failing.
        !!
        !! Each token may also be a STRUCT's own name, with no dot: every leaf under `name.` is
        !! then read in ONE pass, which is what makes it worth having -- the reader decodes a
        !! struct as one array shared by all its leaves, so reading them one at a time decodes it
        !! once per leaf. A real column of that exact name always wins over the prefix reading,
        !! and a name matching neither is a missing column reported the usual way.
        !!
        !! A column whose own name contains a comma or a semicolon is reachable through the array
        !! form only.
        module subroutine prefetch_string(self, names, found)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
            character(len=*), intent(in) :: names    !! column(s) to read, comma/semicolon separated.
            logical, intent(out), optional :: found  !! present: .false. if ANY name was missing.
        end subroutine prefetch_string
        !> Reads several named columns now, in one pass, so that a struct whose leaves are all
        !! named is decoded once rather than once per leaf.
        module subroutine prefetch_array(self, names, found)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
            character(len=*), intent(in) :: names(:) !! columns to read.
            logical, intent(out), optional :: found  !! present: .false. if ANY name was missing.
        end subroutine prefetch_array
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
        !!
        !! `proc` is the name the CALLER was invoked under -- `get`, `col`, `prefetch` -- because
        !! every abort here happens while the user is asking for a column, never while adding one.
        !! Without it the shared-table refusal came out of `table_new_slot` naming `add_column`,
        !! which is a procedure the caller did not invoke and cannot find in their own code.
        module subroutine table_make_row_index(self, proc)
            class(parquet_table), intent(in) :: self !! the table (fills through %cache).
            character(len=*), intent(in) :: proc     !! calling procedure name (for the message).
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
        !!
        !! A column holding values the CALLER wrote (%user_populated) is refused as well, because
        !! the file's own values would come back on the next read and the edits would be gone with
        !! nothing to notice -- pass force=.true. to discard them on purpose. Note the protection
        !! covers what the value-setting API wrote, NOT a write made through a %col/%ref pointer:
        !! the library cannot tell such a write from a read, so a caller who edits that way marks
        !! the column with %set_user_populated themselves.
        module subroutine table_evict_column(self, name, force, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column to release.
            logical, intent(in), optional :: force      !! .true. to evict a column holding local edits.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine table_evict_column
        !> Re-reads one column from the file, discarding whatever is in the store -- the escape
        !! hatch back to the file's own values after %set has changed them locally. Only valid
        !! for a file-backed column of a table that has not been detached.
        !!
        !! Discarding local edits is what this is FOR, so it says so rather than assuming: a
        !! column holding values the caller wrote (%user_populated) is refused unless force=.true.
        !! is passed, which puts %reload and %evict_column under one rule instead of two. A
        !! forced reload clears the mark, since the slot then holds the file's values again.
        module subroutine table_reload(self, name, force, found)
            class(parquet_table), intent(in) :: self !! the table (refills through %cache).
            character(len=*), intent(in) :: name     !! column to re-read.
            logical, intent(in), optional :: force   !! .true. to discard local edits and re-read.
            logical, intent(out), optional :: found  !! present: report a miss instead of aborting.
        end subroutine table_reload
        !> Marks a column as holding values the CALLER wrote, or clears that mark.
        !!
        !! The library sets this itself for every value-setting call (%set, %set_element,
        !! %set_slice, a row or column handle's %set, %set_null, %add_column, ...), and
        !! %evict_column and %reload then refuse that column unless force=.true. is passed. It
        !! cannot set it for a write made through the pointer %col/%ref hands out, because it
        !! cannot tell such a write from a read -- so a caller who edits a column that way marks
        !! it here, and gets the same protection. Clearing it says the opposite: the slot's values
        !! are the file's again, and may be discarded without force=.
        !!
        !! Marking a column that holds no values is refused: there is nothing to protect, and the
        !! mark would outlive the read that filled the slot. Clearing is always allowed.
        module subroutine table_set_user_populated(self, name, flag, found)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column to mark.
            logical, intent(in) :: flag                 !! .true. = the caller's own values; .false. = the file's.
            logical, intent(out), optional :: found     !! present: report a miss instead of aborting.
        end subroutine table_set_user_populated
        !> Whether a column is marked as holding values the caller wrote rather than the file's.
        !!
        !! Reads no values and never triggers a read. A column that has not been read is never
        !! marked, so this answers .false. for one -- see %set_user_populated for what sets it.
        module function table_is_user_populated(self, name, found) result(ok)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column to ask about.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
            logical :: ok                             !! .true. when the slot holds the caller's own writes.
        end function table_is_user_populated
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
    for k in PTR_KINDS:
        w(ref_iface(k))
    w(ref_str_iface())
    w("    end interface")
    w("    !")
    w("    ! ---- Container column access (parquet_tables_container) ----")
    w("    interface")
    w(container_ifaces())
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
    w(add_col_iface())
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
        !> Keeps only the rows one filter EXPRESSION selects -- the reader's own grammar, applied
        !! to the rows already in memory.
        !!
        !! `t%filter_rows("n_obs >= 8 and score > 3")` is `t%row_mask(...)` followed by the mask
        !! form above, so everything the mask form does applies: an all-`.true.` result removes no
        !! row, changes nothing and does not detach.
        !!
        !! The expression is parsed by the SAME parser a read-time `filter=` uses and each clause
        !! is answered by the same rules, so a rule selects the same rows here as it would at the
        !! reader -- including the null rule (a Null row is excluded unless the clause is
        !! `is_null`), the NaN rule (a NaN is a value, so it survives `/=` but no ordering
        !! comparison) and every message a bad rule raises. A column the expression names is READ
        !! if it is not resident yet, by the same lazy touch `%get` performs.
        !!
        !! A set-valued clause (`in`/`not_in`) works with a literal list -- `t%filter_rows("id in
        !! (3, 5, 9)")` -- but a `@name` clause needs the set that is bound to it, so pass the
        !! `parquet_filter` itself for that.
        module subroutine table_filter_rows_expr(self, expr)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: expr        !! one filter expression, the reader's grammar.
        end subroutine table_filter_rows_expr
        !> Keeps only the rows a `parquet_filter` selects, bound sets included.
        !!
        !! The object form of the call above, and the only one that can answer an `in @name`
        !! clause, since the set lives on the filter. Several `%add` rules are AND-combined exactly
        !! as they are at the reader.
        module subroutine table_filter_rows_filter(self, filter)
            class(parquet_table), intent(inout) :: self !! the table.
            type(parquet_filter), intent(in) :: filter  !! the filter whose rules select the rows.
        end subroutine table_filter_rows_filter
        !> The row mask a filter expression selects, WITHOUT applying it.
        !!
        !! The non-mutating half of `%filter_rows`: nothing is dropped, nothing is detached, and
        !! `count(keep)` says how many rows would survive. That is what a caller wants before
        !! deciding whether to filter at all, and it is how a selection the grammar cannot fully
        !! express is combined with one it can (`keep = keep .and. my_own_test`).
        !!
        !! `keep` must have one entry per row of the table. A column the expression names is READ
        !! if it is not resident yet -- the same lazy touch `%get` performs -- so this is a read of
        !! the table, not merely of its metadata.
        module subroutine table_row_mask_expr(self, expr, keep)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: expr     !! one filter expression, the reader's grammar.
            logical, intent(out) :: keep(:)          !! one entry per row; .true. for a selected row.
        end subroutine table_row_mask_expr
        !> The row mask a `parquet_filter` selects, without applying it. Bound sets included.
        module subroutine table_row_mask_filter(self, filter, keep)
            class(parquet_table), intent(in) :: self   !! the table.
            type(parquet_filter), intent(in) :: filter !! the filter whose rules select the rows.
            logical, intent(out) :: keep(:)            !! one entry per row; .true. for a selected row.
        end subroutine table_row_mask_filter
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
        ! ---- Key lists written as ONE string ("ra,-dec") -------------------------------------
        !
        !> Every key-taking binding above also accepts its keys as a single string instead of an
        !! array, which is what removes the `[character(len=9) :: "object_id", "filter", "mjd"]`
        !! constructor from the commonest call in this whole layer. That constructor is not just
        !! verbose: guessing its declared length too short SILENTLY TRUNCATES a name.
        !!
        !! **Separators** are commas and/or semicolons, interchangeably, with blanks around a key
        !! trimmed and empty tokens ignored -- the same tokenizer, and so the same rules, as
        !! `%prefetch` and `parquet_prefetch_columns`.
        !!
        !! **A key may carry its own direction**, in exactly the grammar a read-time
        !! `parquet_sortkey%add` key uses (one parser backs both): `"<column> [asc|desc]"`,
        !! case-insensitive, with a leading `-` as shorthand for descending. So
        !! `t%sort_by("ra,-dec")` orders `ra` ascending and `dec` descending, and
        !! `"ra asc, dec desc"` says the same thing longhand.
        !!
        !! **A direction token and `descending=` together are an error**, for the whole call --
        !! including a redundant `asc`. The two are ways of saying one thing and can disagree; a
        !! per-key rule would leave `descending(1)` governing one key and a token governing
        !! another, which no reader can follow. Without any token, `descending=` works exactly as
        !! it does on the array form.
        !!
        !! `nulls_first=` is orthogonal and always accepted: the grammar has no null-placement
        !! token, matching `parquet_sortkey`.
        !!
        !! A column whose own name contains a comma, a semicolon, a leading `-` or a trailing
        !! " asc"/" desc" is reachable through the array form only.
        module subroutine table_sort_by_string(self, keys, descending, nulls_first)
            class(parquet_table), intent(inout) :: self       !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_sort_by_string
        !> %top_n over a string key list; see `table_sort_by_string` for the grammar.
        module subroutine table_top_n_string(self, keys, n, descending, nulls_first)
            class(parquet_table), intent(inout) :: self       !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            integer, intent(in) :: n                          !! rows to keep; clamped to %nrows().
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_top_n_string
        !> %argsort_by over a string key list, int32 permutation; see `table_sort_by_string`.
        module subroutine table_argsort_by_string_i32(self, keys, perm, descending, nulls_first, &
                                                      group_offsets, group_nkeys)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based row order.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
                !! run boundaries: group g is perm(group_offsets(g) : group_offsets(g+1) - 1).
            integer, intent(in), optional :: group_nkeys      !! leading keys a group is defined by.
        end subroutine table_argsort_by_string_i32
        !> %argsort_by over a string key list, int64 permutation; see `table_sort_by_string`.
        module subroutine table_argsort_by_string_i64(self, keys, perm, descending, nulls_first, &
                                                      group_offsets, group_nkeys)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based row order.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
                !! run boundaries: group g is perm(group_offsets(g) : group_offsets(g+1) - 1).
            integer, intent(in), optional :: group_nkeys      !! leading keys a group is defined by.
        end subroutine table_argsort_by_string_i64
        !> %argsort_partial over a string key list, int32 permutation; see `table_sort_by_string`.
        module subroutine table_argsort_partial_string_i32(self, keys, perm, n, descending, nulls_first)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based row indices.
            integer, intent(in) :: n                          !! rows to order; clamped to %nrows().
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_argsort_partial_string_i32
        !> %argsort_partial over a string key list, int64 permutation; see `table_sort_by_string`.
        module subroutine table_argsort_partial_string_i64(self, keys, perm, n, descending, nulls_first)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based row indices.
            integer, intent(in) :: n                          !! rows to order; clamped to %nrows().
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_argsort_partial_string_i64
        !> %is_sorted_by over a string key list; see `table_sort_by_string` for the grammar.
        module function table_is_sorted_by_string(self, keys, descending, nulls_first) result(answer)
            class(parquet_table), intent(in) :: self          !! the table.
            character(len=*), intent(in) :: keys              !! key columns, separated; primary first.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
            logical :: answer                                 !! .true. when already in that order.
        end function table_is_sorted_by_string
        !> Splits a string key list into key NAMES plus the per-key direction its tokens asked
        !! for, refusing the token-plus-`descending=` conflict. Shared by all seven string
        !! specifics so that one grammar, one conflict rule and one set of messages back every
        !! spelling. `proc` names the caller in every message.
        !!
        !! `descending` comes back **unallocated when no token carried a direction**, which is how
        !! each specific decides what to forward: an unallocated allocatable passed on as an
        !! optional actual makes that dummy absent (F2018 15.5.2.12), so the caller's own
        !! `descending=` is forwarded untouched in that case and the parsed one otherwise. The two
        !! can never both apply -- that is the conflict this refuses.
        module subroutine table_split_key_list(self, keys, proc, have_descending, names, descending)
            class(parquet_table), intent(in) :: self             !! the table, for error context.
            character(len=*), intent(in) :: keys                 !! the raw key string.
            character(len=*), intent(in) :: proc                 !! calling binding, for messages.
            logical, intent(in) :: have_descending               !! caller's present(descending).
            character(len=:), allocatable, intent(out) :: names(:) !! one key name per token.
            logical, allocatable, intent(out) :: descending(:)
                !! parsed direction per name; unallocated when no token asked for one.
        end subroutine table_split_key_list
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
        !> Releases the spare storage capacity that appending left behind, so every resident
        !! column occupies exactly the rows it holds.
        !!
        !! **A no-op on a table that has not been appended to.** Capacity only ever comes from an
        !! append: reading a column from a file allocates exact-fit, and so does every rebuild
        !! (`%filter_rows`, `%sort_by`, `%top_n`, `%delete_rows`, `%truncate`), which hand their
        !! memory back on their own. So this is safe to call unconditionally -- before
        !! `parquet_write_table`, say -- and costs nothing when there is nothing to release. It is
        !! not NEEDED before a write: the writer reads through a pointer that is already bounded by
        !! the row count, so slack costs a write nothing.
        !!
        !! **It INVALIDATES every pointer obtained from `%col` and every row handle**, because it
        !! reallocates storage. That makes it the one procedure in `parquet_tables_mutate` that
        !! does so -- everything else there leaves the row set and the storage alone. It does NOT
        !! detach: the row set is unchanged, so the table keeps its file and a column not yet read
        !! can still be read afterwards.
        !!
        !! **`%generation()` advances only if something was actually released**, which is what
        !! makes the no-op above observable: a caller following the documented
        !! take-generation/compare/re-fetch pattern re-fetches only when a pointer really did die.
        !!
        !! Refused on a shared table, like every other mutation except `%append`.
        module subroutine table_compact(self)
            class(parquet_table), intent(inout) :: self !! the table.
        end subroutine table_compact
        !> int32 form of `%reserve`; converts and delegates.
        module subroutine table_reserve_i32(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int32), intent(in) :: n             !! rows to make room for.
        end subroutine table_reserve_i32
        !> Makes room for `n` rows in every resident column without changing the row count, so the
        !! appends that follow perform no allocation at all.
        !!
        !! The intended shape for building a table incrementally is `%reserve(n)` -> append ->
        !! `%compact()`: growth is amortised O(1) without it, but reserving turns even that into a
        !! single up-front allocation. `n` is the TOTAL row count to make room for, not an
        !! increment, so reserving less than the table already holds does nothing.
        !!
        !! Shares `%compact`'s rules: it invalidates `%col` pointers and row handles if it
        !! reallocates, advances `%generation()` only then, does not detach, and is refused on a
        !! shared table.
        module subroutine table_reserve_i64(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer(int64), intent(in) :: n             !! total rows to make room for.
        end subroutine table_reserve_i64
        !> Makes room for `n` COLUMNS, and in doing so gives `%add_column` a guarantee it does not
        !! otherwise have:
        !!
        !! **While spare column capacity remains, adding a column under a NEW name relocates no
        !! existing column's storage, moves no existing column's slot position, and does not
        !! advance `%generation()`.** A pointer taken from `%col`, a `parquet_table_col` handle
        !! and a `parquet_table_row` handle all stay valid across such a call. A reservation
        !! survives `%clone` and `%clone_structure`, so a copy starts with the same spare
        !! capacity. Replacing an existing column (`force=.true.`) is **not** covered -- it frees
        !! that column's storage -- and neither is any row-structural mutation.
        !!
        !! That is what makes the commonest derived-column idiom safe rather than merely
        !! lucky. Without a reservation, an `%add_column` that happens to fill the slot array
        !! reallocates it, and Fortran leaves a pointer's association status UNDEFINED across the
        !! `MOVE_ALLOC` that does it -- code that usually works and is not permitted to:
        !!
        !!```fortran
        !! call t%reserve_columns(t%ncols() + 2)   ! two derived columns coming
        !! call t%col("mag_g", g)                  ! pointers taken up front...
        !! call t%col("mag_r", r)
        !! call t%add_column("g_minus_r", g - r)   ! ...and still valid here, by contract
        !!```
        !!
        !! **Reserve first, take pointers second** -- the same rule `%reserve` (rows) follows.
        !! Growing the capacity is itself a relocation, so this call invalidates every outstanding
        !! pointer and advances `%generation()` when it actually grows; below the current capacity
        !! it is a no-op, exactly as `%reserve` below the current row count is.
        !!
        !! `n` is the TOTAL capacity to make room for, not an increment. Deliberately a plain
        !! default `integer` and not also an int64 form: a column count cannot exceed int32 -- Arrow's
        !! own `Schema::num_fields()` is an `int32_t` and the writer already guards that ceiling --
        !! so the dual-kind rule in CLAUDE.md does not apply. Refused on a shared table, like every
        !! other structural change; does not detach.
        module subroutine table_reserve_columns(self, n)
            class(parquet_table), intent(inout) :: self !! the table.
            integer, intent(in) :: n                    !! total column slots to make room for.
        end subroutine table_reserve_columns
        !> How many column slots are allocated (`%ncols()` of them in use), or -- with
        !! `free=.true.` -- how many are spare.
        !!
        !! This is what `%reserve_columns` acts on, and the two are inverses: after
        !! `call t%reserve_columns(n)` with `n` above the current capacity, `%column_capacity()`
        !! is `n`. Capacity only ever grows: `%compact` releases row storage rather than slots,
        !! and `%drop_column` keeps the slot it vacated.
        !!
        !! A metadata query -- it reads no column data.
        module function table_column_capacity(self, free) result(n)
            class(parquet_table), intent(in) :: self !! the table.
            logical, intent(in), optional :: free    !! .true.: report the SPARE slots instead.
            integer :: n                             !! slots allocated, or spare.
        end function table_column_capacity
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
        !! (`doc/pages/utilities/generated-tables.md`) does exactly that, and so may hand-written code. But
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
        !! (`doc/pages/utilities/generated-tables.md`); it is public only because a generated module is a
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
        !> Resolves ONE key column name to its slot, refusing every column that cannot be a key.
        !!
        !! A thin separate-module-procedure relay onto `sort_lookup_key`, which is a contained
        !! procedure of `parquet_tables_sort` and so unreachable from a sibling submodule. It
        !! exists so the JOIN resolves its key names through exactly the same code the sorts do
        !! -- which brings the refusal of every unorderable kind, the lazy first touch of a key
        !! column, and the file-naming message suffix with it. Two copies of "which columns can
        !! be a key" is precisely the drift that would let a join accept a column `%sort_by`
        !! rejects.
        module subroutine table_lookup_sort_key(self, name, proc, idx, key_kind)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! the key column's name.
            character(len=*), intent(in) :: proc     !! calling procedure, for messages.
            integer, intent(out) :: idx              !! its slot index.
            !> what the message calls the key, and what it advises instead: `"sort"` (the default)
            !! or `"join"`. A join is an equality test rather than an ordering, so telling its
            !! caller a column "cannot be a sort key" and to "sort by a scalar column" names an
            !! operation they did not ask for.
            character(len=*), intent(in), optional :: key_kind
        end subroutine table_lookup_sort_key
    end interface""")
    w("    !")
    w("""    ! ---- The join (parquet_tables_join -- HAND-WRITTEN, not generated) ----
    interface""")
    for spec, rank, mkind in (("table_join", "array", None),
                              ("table_join_max_i32", "array", "int32"),
                              ("table_join_max_i64", "array", "int64"),
                              ("table_join_string", "string", None),
                              ("table_join_string_max_i32", "string", "int32"),
                              ("table_join_string_max_i64", "string", "int64")):
        w(join_interface(spec, rank, mkind))
    w("""        !> Builds the join's PAIR LIST: which left row meets which right row, and how many rows
        !! the joined table will have. Does not touch a single value column.
        !!
        !! **This is the whole of the join's reasoning**, and it is deliberately separable from
        !! the column rewrite that follows it: everything that can silently produce a wrong
        !! ANSWER -- the null rule, the cardinality assertion, the output count, the ordering --
        !! is decided here, over nothing but row indices, where it can be asserted directly.
        !!
        !! `il(o)` and `ir(o)` are the left and right rows contributing to output row `o`, with
        !! **0 meaning "no counterpart on that side"**. `n_out` is `size(il)`, computed before
        !! anything proportional to it is allocated, which is what lets `max_rows=` refuse a
        !! join rather than run out of memory during one.
        module subroutine table_join_pairs(self, other, on, other_on, how, require, order, &
                max_rows, il, ir, n_out, matched, threads)
            class(parquet_table), intent(in) :: self  !! the LEFT table.
            class(parquet_table), intent(in) :: other !! the RIGHT table.
            character(len=*), intent(in) :: on(:)     !! left key columns, primary first.
            !> right key columns; absent means the same names as `on`. One entry per `on` entry.
            character(len=*), intent(in), optional :: other_on(:)
            !> "inner" (default), "left", "right", "outer", "semi" or "anti", case-insensitive.
            character(len=*), intent(in), optional :: how
            !> "m:m" (default, no assertion), "1:1", "1:m" or "m:1" -- read left-side-first, so
            !! "m:1" is the lookup-table annotation and asserts the RIGHT key is unique.
            character(len=*), intent(in), optional :: require
            !> "left" (default: left rows in their own order) or "key" (the sort's own order).
            character(len=*), intent(in), optional :: order
            !> abort rather than build an output larger than this. Absent means no limit.
            integer(int64), intent(in), optional :: max_rows
            integer(int64), allocatable, intent(out) :: il(:) !! per output row: left row, or 0.
            integer(int64), allocatable, intent(out) :: ir(:) !! per output row: right row, or 0.
            integer(int64), intent(out) :: n_out              !! output rows; equals size(il).
            !> per PRE-join left row: .true. when it found at least one counterpart. The only
            !! coordinate system in which that question still has an answer after the mutation.
            logical, allocatable, intent(out), optional :: matched(:)
            integer, intent(in), optional :: threads !! forwarded to pf_argsort; absent = auto.
        end subroutine table_join_pairs
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
        !!
        !! Deliberately `pure`, and so the one query that does NOT check the stamp: it answers
        !! about the HANDLE ("which row was I made for"), not about the table, and that answer is
        !! still true after a mutation even though reading through the handle is refused. Its
        !! column-handle counterpart `%index()` does check, because a slot number is about the
        !! table and slots renumber.
        pure module function row_index(self) result(i)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            integer(int64) :: i                          !! the row index.
        end function row_index
        !> Whether the handle is attached to a table AND still current -- one predicate, for the
        !! reason its `parquet_table_col` twin gives: a caller can do nothing useful with a handle
        !! that is one and not the other.
        module function row_is_valid(self) result(ok)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            logical :: ok                                !! .true. when it can still be used.
        end function row_is_valid
        !> Aborts unless the row handle is attached and current, naming the remedy.
        module subroutine row_check_current(self, proc)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
        end subroutine row_check_current
        !> Resolves `name` for a row-handle access, triggering the same lazy first touch the
        !! table's own accessors do. Aborts on a missing, unsupported or unreadable column.
        module subroutine row_resolve(self, name, proc, idx)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
            integer, intent(out) :: idx                  !! slot index.
        end subroutine row_resolve
        !> Rebuilds `cache%name_order` from scratch, for a change that can reorder or renumber
        !! slots -- a drop, a rename, a clone, a reset. An append uses `cache_name_index_insert`
        !! instead, because rebuilding per appended column would make opening an n-column file
        !! cost O(n^2 log n) name comparisons.
        module subroutine cache_name_index_rebuild(cache)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
        end subroutine cache_name_index_rebuild
        !> Inserts slot `slot` into `cache%name_order`, keeping it sorted by name. O(log n)
        !! comparisons plus an integer shift, so building the index one column at a time as a file
        !! is opened stays O(n log n) comparisons overall.
        module subroutine cache_name_index_insert(cache, slot)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            integer, intent(in) :: slot                       !! the newly added slot index.
        end subroutine cache_name_index_insert
        !> Grows `cache%name_order`/`name_key` to hold at least `cap` entries, keeping whatever is
        !! already indexed. Called by `%reserve_columns` so that the appends a reservation exists
        !! to make cheap do not each reallocate the index instead. Cost only: the index holds no
        !! column storage, so it has no bearing on a `%col` pointer's validity.
        module subroutine cache_name_index_reserve(cache, cap)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            integer, intent(in) :: cap                        !! entries to make room for.
        end subroutine cache_name_index_reserve
        !> Resolves `name` to its 1-based slot index in `cache`, or 0 when absent. The one place
        !! a name becomes an index, shared by the table and by a row handle.
        module function cache_find(cache, name) result(idx)
            type(parquet_table_cache), intent(in) :: cache !! the column store.
            character(len=*), intent(in) :: name           !! column name.
            integer :: idx                                 !! slot index, or 0.
        end function cache_find
        !> TEST-ONLY -- deallocates this table's name index, so the next lookup has to take
        !! `cache_find`'s linear-scan fallback.
        !!
        !! **This is a debug hook, not API**, public for the same reason
        !! `parquet_debug_table_set_inflight` is: the index lives on `parquet_table_cache`, whose
        !! components are private to this module, so nothing outside can reach it. It is excluded
        !! from README.md's API overview and no library code calls it.
        !!
        !! It exists because that fallback is a SAFETY NET with no route to it through the public
        !! API: every mutation maintains the index eagerly, so a correct library never reaches the
        !! scan, and it was measured executing zero times across the whole suite and every error
        !! scenario. What it protects against is a future mutation that forgets to maintain the
        !! index -- which would otherwise turn into wrong-column answers rather than a slower
        !! lookup. Untested, the net could rot away and nothing would say so.
        !!
        !! `had_index` is not optional on purpose: dropping the index is invisible from outside, so
        !! a test that did not check it would pass just as happily against a hook that did nothing.
        !! It reports whether an index was there to drop, which makes the before/after states
        !! assertable -- `.true.` on the first call, `.false.` on a second one.
        module subroutine parquet_debug_table_drop_name_index(table, had_index)
            type(parquet_table), intent(in) :: table !! the table whose index to drop.
            logical, intent(out) :: had_index        !! .true. when an index was present.
        end subroutine parquet_debug_table_drop_name_index
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
        end subroutine row_kind_error
        !> Aborts unless column handle `c` can be used to reach a cell of THIS row.
        !!
        !! Three checks, and the third is the one a caller cannot make for itself: the row handle
        !! must be attached, `c` must be attached and current (`col_resolve`), and the two must
        !! name the SAME table. A handle from another table would otherwise read that table's
        !! column at this row's index -- a wrong answer rather than an error, since both tables
        !! are perfectly valid objects.
        !!
        !! It then checks the row index against `c`'s scope. Both handles are current by that point,
        !! so the two scopes agree; taking `c`'s keeps the bounds check reading from the handle
        !! validated most recently.
        module subroutine row_require_col(self, c, proc)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            type(parquet_table_col), intent(in) :: c     !! the column handle to validate.
            character(len=*), intent(in) :: proc         !! calling procedure, for the message.
        end subroutine row_require_col""")
    for k in KINDS:
        w(rowget_iface(k))
    for k in KINDS:
        w(rowset_iface(k))
    for k in KINDS:
        w(rowget_col_iface(k))
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
    for c in READ_CONTAINERS:
        w(mat_container_iface(c))
    for c in READ_CONTAINERS:
        w(matchunk_container_iface(c))
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


def ref_iface(k):
    tag, pk, decl, comp, rank, cat = k
    mdims, mdoc = mask_dims(rank), valid_out_comment(rank)
    return f"""        !> Points `p` at a {pk} column's storage, from an already-resolved handle.
        !!
        !! The handle twin of `%col`, with the same warning: `p` aliases the LIVE column, so a
        !! write through it changes the table and nothing revalidates the result. Reordering one
        !! column through its pointer breaks the table's row alignment silently. The stored kind
        !! must match EXACTLY -- the pointer path never widens.
        module subroutine col_ref_{tag}(self, p, is_valid)
            class(parquet_table_col), intent(in) :: self     !! the handle.
{decl_line(12, f"{decl}, pointer, intent(out) :: p{dims(rank)}", "!! alias to the live storage.")}
            logical, allocatable, intent(out), optional :: is_valid{mdims} {mdoc}
        end subroutine col_ref_{tag}"""


def ref_str_iface():
    return """        !> Points `p` at a PK_STRING column's packed store, from an already-resolved handle.
        !!
        !! The handle twin of `%col`'s string form, carrying its warning verbatim: this is the one
        !! pointer this layer hands out that a caller could use to change the column's SHAPE.
        !! Reading and in-place value edits are supported; appending to it, or otherwise changing
        !! how many elements it holds, is not -- the column's own row count is kept separately and
        !! would stop matching.
        module subroutine col_ref_strcol(self, p)
            class(parquet_table_col), intent(in) :: self              !! the handle.
            type(parquet_string_column), pointer, intent(out) :: p    !! alias to the packed store.
        end subroutine col_ref_strcol"""


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
        module subroutine set_arr_{tag}(self, name, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
{decl_line(12, f"{decl}, intent(in) :: arr{dims(rank)}", f"!! {shape_comment(rank)}.")}
            logical, intent(in), optional :: is_valid{mdims} {mdoc}
            logical, intent(in), optional :: modify_nulls !! .false. leaves null entries untouched.
            logical, intent(out), optional :: found       !! present: report a miss instead of aborting.
        end subroutine set_arr_{tag}"""


def set_str_iface():
    return """        !> Replaces every value of a PK_STRING column from a character array. **Trailing blanks
        !! are trimmed** -- every element of a `character(len=*)` array shares one declared length,
        !! so a shorter value is blank-padded by Fortran and those blanks carry nothing the caller
        !! could have meant. `%set_element`, which takes a scalar, stores its value verbatim.
        module subroutine set_arr_chr(self, name, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: arr(:)       !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! present: rows marked .false. become null.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
            logical, intent(out), optional :: found       !! present: report a miss instead of aborting.
        end subroutine set_arr_chr
        !> Replaces every value of a PK_STRING_VEC column from a character (element, row) array.
        !! Trailing blanks are trimmed, as in the rank-1 form above.
        module subroutine set_arr_chrv(self, name, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            character(len=*), intent(in) :: arr(:,:)     !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! present: elements marked .false. become null.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
            logical, intent(out), optional :: found       !! present: report a miss instead of aborting.
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
        module subroutine set_arr_strcol(self, name, arr, is_valid, modify_nulls, found)
            class(parquet_table), intent(inout) :: self         !! the table.
            character(len=*), intent(in) :: name                !! column name.
            type(parquet_string_column), intent(in) :: arr      !! one value per row.
            logical, intent(in), optional :: is_valid(:)        !! present: rows marked .false. become null.
            logical, intent(in), optional :: modify_nulls       !! .false. keeps a row null if it was.
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


def add_col_iface():
    return """        !> Appends a new column holding a copy of an already-built `parquet_column`, taking its
        !! kind, width and row count from the column itself.
        !!
        !! This is the one `%add_column` form that covers EVERY kind and width through a single
        !! call, because it reads all three off the column rather than from the shape of a Fortran
        !! array. What it is for is a column that could not be handed over as a plain array: one
        !! grown a row at a time with `%append_values` when the final length was not known up
        !! front, one carrying per-ELEMENT nulls on a vector kind, or one derived from another with
        !! `%gather`/`%delete_by_mask`/`%reindex`.
        !!
        !! **The column is COPIED, and the caller keeps its own.** That matches every other
        !! `%add_column` form, none of which disturbs what it is given -- so a single built column
        !! can be added to several tables, and one that was built with `%adopt` to avoid a copy
        !! does pay for one here.
        !!
        !! `unit=` overrides whatever unit the column carries; omitted, the column's own is kept.
        !!
        !! A column that has never been given a kind (no `%init`, `%adopt` or `%append_values`) is
        !! refused rather than added as an unusable `PK_NONE` slot -- the one failure mode the
        !! array forms cannot have, since they take their kind from the type they are handed.
        module subroutine add_column_col(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self    !! the table.
            character(len=*), intent(in) :: name           !! the new column's name.
            type(parquet_column), intent(in) :: values     !! the column to copy in.
            character(len=*), intent(in), optional :: unit !! unit string to store, overriding the column's own.
            logical, intent(in), optional :: force         !! .true. replaces an existing same-named column.
        end subroutine add_column_col"""


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


# The three container column kinds, as (tag, PK_*, concrete type).
#
# Deliberately NOT rows of KINDS. A KINDS row carries an element declaration, a storage component
# and a rank, and a container has none of the three: its storage is the concrete container object
# itself, reached through parquet_column_container rather than through a typed array component. So
# everything container-shaped is emitted from this table by its own helper, and no KINDS-driven
# emitter has to grow a special case.
CONTAINERS = [
    ("list", "PK_LIST", "parquet_list_column"),
    ("map", "PK_MAP", "parquet_map_column"),
    ("struct", "PK_STRUCT", "parquet_struct_column"),
]

# The container kinds a FILE column can be classified as, in the order the dispatchers list them.
#
# `struct` is absent, and that is a property of the file format's own addressing rather than a gap
# here: parquet_get_column_names EXPANDS a top-level struct into one dotted path per leaf, so a
# struct column is never enumerated under its own name and table_classify never sees one. A
# PK_STRUCT column therefore only ever exists in memory -- built with %add_column or %set -- and is
# already RES_FULL, so no materializer could run for it. Adding one would be dead code; see the
# case default arms in gen_dispatch, which say so where a reader would look.
READ_CONTAINERS = [c for c in CONTAINERS if c[0] != "struct"]


def mat_container_iface(c):
    tag, pk, ctype = c
    return f"""        !> Reads a {pk} file column into `col`, carrying its row nulls across.
        module subroutine mat_{tag}(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows the column holds (unused: the
            !! container reports its own row count, and adopt_container takes it from there).
            integer(int32), intent(in) :: wdt            !! values per row (unused: a container row
            !! has no fixed width -- that is what makes it a container).
            character(len=*), intent(in) :: unit         !! unit string (unused: a container's unit
            !! belongs to its payload column, which the reader has already set).
        end subroutine mat_{tag}"""


def matchunk_container_iface(c):
    tag, pk, ctype = c
    return f"""        !> Reads one row group of a {pk} file column into `col`.
        module subroutine matchunk_{tag}(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows in that row group (unused, as in mat_{tag}).
            integer(int32), intent(in) :: wdt            !! values per row (unused, as in mat_{tag}).
            character(len=*), intent(in) :: unit         !! unit string (unused, as in mat_{tag}).
        end subroutine matchunk_{tag}"""


def mat_container_impl(c):
    tag, pk, ctype = c
    return f"""    module procedure mat_{tag}
        class(parquet_container_column), allocatable :: tmp
        !
        ! Allocated as the ABSTRACT class holding a {ctype}, rather than read into a concrete
        ! local and copied into a box afterwards: adopt_container MOVES what it is given, so this
        ! shape hands the reader's own allocation straight to the column with no second live copy
        ! of the payload. `allocate(box, source=concrete)` would deep-copy the whole column.
        allocate({ctype} :: tmp)
        select type (dest => tmp)
        type is ({ctype})
            call parquet_read_column(reader, name, dest)
        end select
        ! nrows/wdt/unit are deliberately unread here -- see this procedure's interface.
        call col%adopt_container(tmp)
    end procedure mat_{tag}
    !"""


def matchunk_container_impl(c):
    tag, pk, ctype = c
    return f"""    module procedure matchunk_{tag}
        class(parquet_container_column), allocatable :: tmp
        !
        allocate({ctype} :: tmp)
        select type (dest => tmp)
        type is ({ctype})
            call parquet_read_column_chunk(reader, name, rg, dest)
        end select
        call col%adopt_container(tmp)
    end procedure matchunk_{tag}
    !"""


def container_ifaces():
    """The fifteen container accessors' interfaces: %col, %get, %set, %add_column and %ref.

    Exactly the five specifics `parquet_string_column` has, and no others. There is deliberately no
    `%get_slice`/`%set_slice`, no `%get_element`/`%set_element` and no `%row%get`/`%set`: all three
    address a fixed-width CELL, and a container row is a variable-length object with no such shape.
    The packed string store declines the same three for the same reason.

    NO `is_valid=` on any of them, matching the temporal kinds rather than the array kinds: a
    container carries its own per-row nullness inside itself (`is_null_row`), so a second mask
    beside it would be the divergent-second-answer `adopt_container` exists to prevent. `%get`'s
    caller reads nullness off the returned container; `%get_valid_mask` still answers rank-1 for a
    container column, which is the table-level way to ask.
    """
    o = []
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""        !> Points `p` at a {pk} column's live container.
        !!
        !! The container counterpart of `%col`'s typed pointers, and it carries their warning: `p`
        !! aliases the LIVE column, a write through it changes the table, and nothing revalidates
        !! the result. It is also subject to the same detach rule as every other `%col` pointer --
        !! any row-structural mutation (`%sort_by`, `%filter_rows`, `%delete_rows`, `%truncate`,
        !! `%append`) rebuilds the storage and leaves `p` dangling; `%generation()` is how a caller
        !! finds out.
        !!
        !! Changing how many ROWS the container holds through this pointer is not supported: the
        !! table keeps its own row count, which would stop matching.
        module subroutine col_ptr_{tag}col(self, name, p, found)
            class(parquet_table), intent(in), target :: self       !! the table.
            character(len=*), intent(in) :: name                   !! column name.
            type({ctype}), pointer, intent(out) :: p               !! alias to the live container.
            logical, intent(out), optional :: found                !! present: report a miss instead of aborting.
        end subroutine col_ptr_{tag}col""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""        !> Copies a {pk} column out as an independent `{ctype}`.
        !!
        !! A genuine copy, exactly as every other `%get` is: `arr` shares no storage with the table
        !! and outlives any mutation. `%col` is the zero-copy route.
        module subroutine get_arr_{tag}col(self, name, arr, found)
            class(parquet_table), intent(in) :: self        !! the table.
            character(len=*), intent(in) :: name            !! column name.
            type({ctype}), intent(out) :: arr               !! independent copy of the column.
            logical, intent(out), optional :: found         !! present: report a miss instead of aborting.
        end subroutine get_arr_{tag}col""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""        !> Replaces a {pk} column's values with an independent copy of `arr`.
        !!
        !! The row count must already match -- `%set` never changes a table's shape. Row nullness
        !! comes across with the container, since that is where a container's nullness lives; there
        !! is no `is_valid=` and no `modify_nulls=` for the same reason.
        module subroutine set_arr_{tag}col(self, name, arr, found)
            class(parquet_table), intent(inout) :: self     !! the table.
            character(len=*), intent(in) :: name            !! column name.
            type({ctype}), intent(in) :: arr                !! values to copy in.
            logical, intent(out), optional :: found         !! present: report a miss instead of aborting.
        end subroutine set_arr_{tag}col""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""        !> Appends a new {pk} column holding an independent copy of `values`.
        !!
        !! No `unit=`: a container's unit belongs to its PAYLOAD column, which the container itself
        !! already carries, so a second one on the outer column could only disagree with it.
        module subroutine add_column_{tag}col(self, name, values, force)
            class(parquet_table), intent(inout) :: self     !! the table.
            character(len=*), intent(in) :: name            !! the new column's name.
            type({ctype}), intent(in) :: values             !! values to copy in.
            logical, intent(in), optional :: force          !! .true. replaces an existing same-named column.
        end subroutine add_column_{tag}col""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""        !> Points `p` at a {pk} column's live container, from an already-resolved handle.
        !!
        !! The handle twin of `%col`'s container form, carrying its warnings verbatim.
        module subroutine col_ref_{tag}col(self, p)
            class(parquet_table_col), intent(in) :: self    !! the handle.
            type({ctype}), pointer, intent(out) :: p        !! alias to the live container.
        end subroutine col_ref_{tag}col""")
    return "\n".join(o)


def container_impls():
    """The fifteen container accessors' bodies.

    Every one of them resolves through `table_resolve` (or `col_resolve` for the handle form), and
    that is not a stylistic choice: it is what puts `table_check_not_detached` and the append guard
    in front of the access. A row-structural mutation SKIPS a column that is not resident, leaving
    it unreadable for good, and the detach guard is the only thing that reports it -- so a specific
    written any other way would silently read a skipped container column instead of aborting.
    See feature_container_phase6.md's D6.
    """
    o = []
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""    module procedure col_ptr_{tag}col
        integer :: idx
        class(parquet_container_column), pointer :: c
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, {pk}, "col")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_{tag}(c, p)
    end procedure col_ptr_{tag}col
    !""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""    module procedure get_arr_{tag}col
        integer :: idx
        class(parquet_container_column), pointer :: c
        type({ctype}), pointer :: src
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, {pk}, "get")
        call parquet_column_container(self%cache%cols(idx)%values, c)
        call container_as_{tag}(c, src)
        ! clone_into rather than a component-wise copy: it is the container's own deep copy, so a
        ! future component is carried across without this file having to learn about it.
        call src%clone_into(copy)
        select type (copy)
        type is ({ctype})
            call arr%move_from(copy)
        end select
    end procedure get_arr_{tag}col
    !""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""    module procedure set_arr_{tag}col
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, {pk}, "set")
        call table_require_length(self, idx, arr%nrows(), "set")
        ! An independent copy, then adopt_container MOVES it in -- so the caller's own container
        ! and the table's never share storage, and no second live copy of the payload survives the
        ! call. %set is a value replacement, not a way to hand ownership over.
        call arr%clone_into(copy)
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_{tag}col
    !""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""    module procedure add_column_{tag}col
        integer :: idx
        class(parquet_container_column), allocatable :: copy
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, values%nrows())
        call table_new_slot(self, name, force, idx)
        call values%clone_into(copy)
        ! adopt_container is the ONLY writer of a container kind, and it settles the kind, the row
        ! count and the width from the container itself -- so unlike every other %add_column form
        ! there is no %init call here to keep in step with it.
        call self%cache%cols(idx)%values%adopt_container(copy)
        self%cache%cols(idx)%declared_kind = {pk}
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_{tag}col
    !""")
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""    module procedure col_ref_{tag}col
        class(parquet_container_column), pointer :: c
        !
        call col_resolve(self, "ref")
        nullify(p)
        call cache_require_kind(self%cache, self%slot, {pk}, "ref")
        call parquet_column_container(self%cache%cols(self%slot)%values, c)
        call container_as_{tag}(c, p)
    end procedure col_ref_{tag}col
    !""")
    return "\n".join(o)


def container_downcasts():
    """One `select type` downcast per container type, shared by `%col` and `%ref`.

    The downcast has to happen SOMEWHERE -- `parquet_column_container` hands back the abstract
    class, and `%col` promises a concrete pointer (which is the whole point: a caller wants
    `%length(i)`, not the eleven bindings the base declares). Doing it once per type here rather
    than inline at each of the six call sites keeps the `class default` arm, and its reasoning,
    in one place.
    """
    o = []
    for tag, pk, ctype in CONTAINERS:
        o.append(f"""    !> Downcasts an abstract container pointer to a `{ctype}`.
    !!
    !! Both callers run `table_require_kind`/`cache_require_kind` first, so by the time this is
    !! reached the column's declared kind is already {pk} and `adopt_container` -- the only writer
    !! of a container kind -- takes the kind FROM the container it is given. The two therefore
    !! cannot disagree, which is what makes the `class default` arm below unreachable rather than
    !! merely unlikely. It still aborts rather than leaving `p` null, because a null pointer that
    !! a caller then dereferences is a worse failure than an abort naming the cause.
    subroutine container_as_{tag}(c, p)
        class(parquet_container_column), pointer, intent(in) :: c !! the abstract container.
        type({ctype}), pointer, intent(out) :: p                  !! the same object, concretely typed.
        !
        nullify(p)
        select type (c)
        type is ({ctype})
            p => c
        class default ! GCOVR_EXCL_START -- unreachable, see this procedure's own doc-comment.
            error stop EP // "internal: a {pk} column does not hold a {ctype}"
        end select ! GCOVR_EXCL_STOP
    end subroutine container_as_{tag}
    !""")
    return "\n".join(o)


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


def rowget_col_iface(k):
    """`r%get(c, value)` -- the same read, with the column named by a HANDLE rather than a
    string, so a row-major loop stops resolving the name on every access."""
    tag, pk, decl, comp, rank, cat = k
    return f"""        !> This row's value from a {pk} column named by an already-resolved handle.
        !!
        !! The same operation as `%get(name, value)` over the same body -- what it saves is the
        !! name lookup, which a loop over rows would otherwise pay once per access. The handle
        !! must belong to THIS row's table and must still be current.
        module subroutine row_get_col_{tag}(self, c, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            type(parquet_table_col), intent(in) :: c     !! a handle on the column to read.
            {fetch_val_decl(k)} !! receives the value.
        end subroutine row_get_col_{tag}
        !> Writes this row's value in a {pk} column named by an already-resolved handle.
        module subroutine row_set_col_{tag}(self, c, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            type(parquet_table_col), intent(in) :: c     !! a handle on the column to write.
            {store_val_decl(k)} !! the value to write.
        end subroutine row_set_col_{tag}"""


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
    for c in READ_CONTAINERS:
        tag, pk = c[0], c[1]
        w(f"        case ({pk})")
        w(f"            call mat_{tag}(reader, name, col, nrows, wdt, unit)")
    w("""        case default
            ! table_classify only ever assigns a slot one of the supported PK_* kinds handled
            ! above (an unsupported column stays PK_NONE and is never routed to a materializer),
            ! so this branch guards an internal invariant with no path reachable through the
            ! public API -- there is no way to feed it a value that would actually take it.
            !
            ! PK_STRUCT is the one supported kind deliberately absent from the list above, and it
            ! is unreachable here for a reason worth knowing before adding an arm: a struct column
            ! is never CLASSIFIED, because parquet_get_column_names expands a top-level struct into
            ! one dotted path per leaf and so never emits the struct's own name. A PK_STRUCT column
            ! therefore only ever exists in memory (%add_column, %set), where it is RES_FULL from
            ! birth and nothing materializes it.
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
    for c in READ_CONTAINERS:
        tag, pk = c[0], c[1]
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
    for k in KINDS:
        w(rowget_col_impl(k))
    for k in KINDS:
        w(rowset_col_impl(k))
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
        lines += ["            allocate(arr(size(rows, kind=int64)))",
                  "            do k = 1, size(rows, kind=int64)",
                  "                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))",
                  "            end do"]
    else:
        lines += ["            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))",
                  "            do k = 1, size(rows, kind=int64)",
                  "                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))",
                  "            end do"]
    for srcpk, src in WIDEN.get(tag, []):
        lines.append(f"        case ({srcpk})")
        if rank == 1:
            lines += ["            allocate(arr(size(rows, kind=int64)))",
                      "            do k = 1, size(rows, kind=int64)",
                      f"                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), v_{src})",
                      f"                arr(k) = v_{src}",
                      "            end do"]
        else:
            lines += ["            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))",
                      f"            allocate(v_{src}(self%cache%cols(idx)%width))",
                      "            do k = 1, size(rows, kind=int64)",
                      f"                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), v_{src})",
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
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), {val}, modify_nulls)
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
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), {val}, modify_nulls)
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
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Built element by element rather than copied and trimmed: a gather has no contiguous
        ! source range to clone from, and appending keeps the result compact.
        ! `%append_from` carries both the bytes and the null state, so the is_null fork this
        ! replaced is redundant and no per-row string is materialized.
        do k = 1, size(rows, kind=int64)
            call arr%append_from(store, rows(k))
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
        if (idx == 0) then
            allocate(character(len=1) :: arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Two passes: a fixed-length array's width must be the longest element SELECTED, which
        ! is not known until every selected row has been looked at.
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            if (int(store%length(rows(k))) > maxlen) maxlen = int(store%length(rows(k)))
        end do
        allocate(character(len=maxlen) :: arr(size(rows, kind=int64)))
        do k = 1, size(rows, kind=int64)
            call store%copy_to(rows(k), arr(k), allow_null=.true.)
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
        if (idx == 0) then
            allocate(character(len=1) :: arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING_VEC, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        wdt = self%cache%cols(idx)%width
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, size(rows, kind=int64)))
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%copy_to(flat, arr(e, k), allow_null=.true.)
            end do
        end do
    end procedure get_slice_chrv
    !"""


def rowget_col_impl(k):
    """`r%get(c, value)` -- the row handle taking a COLUMN HANDLE in place of a name.

    This is what makes a row-major loop (`do i; r = t%row(i); ...`) as cheap as the column-major
    one: the row handle resolves the column by name on every access, and a handle hoisted out of
    the loop resolves it never. It runs the same `col_fetch_<tag>` body both other spellings do.
    """
    tag = k[0]
    return f"""    module procedure row_get_col_{tag}
        call row_require_col(self, c, "get")
        call col_fetch_{tag}(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_{tag}
    !"""


def rowset_col_impl(k):
    """`r%set(c, value)`. Exact kind, like every other write in this layer."""
    tag = k[0]
    return f"""    module procedure row_set_col_{tag}
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_{tag}(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_{tag}
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
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_{tag}
    !"""


def rowref_impl(k):
    """A pointer to one row's storage: the whole-column pointer, narrowed to this row."""
    tag, pk, decl, comp, rank, cat = k
    if rank == 1:
        body = """        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)"""
        decl_store = f"        {decl}, pointer :: store(:)"
    else:
        body = """        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
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
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
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
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%copy_to(flat, value(e), allow_null=.true.)
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
        lines.append("            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)")
    else:
        lines += ["            allocate(value(self%cache%cols(idx)%width))",
                  "            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)"]
    for srcpk, src in WIDEN.get(tag, []):
        lines.append(f"        case ({srcpk})")
        if rank == 1:
            lines += [f"            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, v_{src})",
                      f"            value = v_{src}"]
        else:
            lines += [f"            allocate(v_{src}(self%cache%cols(idx)%width))",
                      f"            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, v_{src})",
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
        call cache_require_ptr_kind(self%cache, idx, {pk}, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_{tag}
    !"""


def col_ref_impl(k):
    """The same pointer, from an already-resolved handle.

    A drop-in for `%col`: same kind rule, same `is_valid=` snapshot, same warning about what a
    pointer lets a caller do. What it does not have is `found=` -- a handle that could not be
    resolved never came into existence, so there is nothing left to report here.
    """
    tag, pk, decl, comp, rank, cat = k
    msuf = "" if rank == 1 else "_elem"
    return f"""    module procedure col_ref_{tag}
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of{msuf}(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, {pk}, "ref")
        call parquet_column_data_ptr(self%cache%cols(self%slot)%values, p)
    end procedure col_ref_{tag}
    !"""


def ptr_str_impl():
    return """    module procedure col_ptr_strcol
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "col")
        call parquet_column_string_column(self%cache%cols(idx)%values, p)
    end procedure col_ptr_strcol
    !"""


def col_ref_str_impl():
    """The string store, from an already-resolved handle -- `%ref`'s 17th specific."""
    return """    module procedure col_ref_strcol
        call col_resolve(self, "ref")
        nullify(p)
        call cache_require_kind(self%cache, self%slot, PK_STRING, "ref")
        call parquet_column_string_column(self%cache%cols(self%slot)%values, p)
    end procedure col_ref_strcol
    !"""


def set_strcol_impl():
    return """    module procedure set_arr_strcol
        integer :: idx
        integer(int64) :: n, k
        logical :: mod_nulls
        logical, allocatable :: was_null(:)
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, arr%size(), "set")
        ! Replaces the packed store wholesale with an independent copy, so the caller's own column
        ! and the table's do not end up sharing storage. %set is a value replacement, exactly as
        ! the character-array form is; it is not a way to hand ownership over.
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! modify_nulls = .false. keeps a row null if it was null HERE, on top of whatever the
        ! source says -- the union of the two, not one replacing the other. The character-array
        ! sibling (`refill_string_store`) can simply restore the destination's mask because its
        ! source is a plain array with no validity of its own; this source carries nulls, and
        ! discarding them would throw away something the caller explicitly supplied. Where the
        ! source is null-free the two rules agree, which is every case the sibling covers.
        ! Captured BEFORE the overwrite, or there is nothing left to read it from.
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        if (.not. mod_nulls) then
            n = store%size()
            allocate(was_null(n))
            do k = 1_int64, n
                was_null(k) = store%is_null(k)
            end do
        end if
        store = arr%clone()
        if (.not. mod_nulls) then
            do k = 1_int64, n
                if (was_null(k)) call store%set_null(k)
            end do
        end if
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
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        store = values%clone()
        self%cache%cols(idx)%declared_kind = PK_STRING
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_strcol
    !"""


def add_col_impl():
    return """    module procedure add_column_col
        integer :: idx
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, "add_column")
        ! Every other %add_column form takes its kind from the TYPE of the values it is given, so
        ! it cannot be kindless. This one reads the kind off the column, and a column that was
        ! never given one would land in the table as a PK_NONE slot that nothing can read or write
        ! -- failing later, at the first %get, with nothing to say where it came from. Checked
        ! before table_fix_nrows and table_new_slot, so a refused call changes nothing.
        if (values%kindof() == PK_NONE) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "add_column: this parquet_column has no kind yet, so there is " // &
                "nothing to add; give it one with %init, %adopt or %append_values first" // sfx
        end if
        call table_fix_nrows(self, name, values%length())
        call table_new_slot(self, name, force, idx)
        ! Copied, not moved: `values` is intent(in) like every other %add_column form's, so the
        ! caller's column is left intact and can be added to a second table.
        call values%deep_copy(self%cache%cols(idx)%values)
        ! The copy already carries the source column's unit, so this only has to run when the
        ! caller asked for a different one.
        if (present(unit)) call self%cache%cols(idx)%values%set_unit(unit)
        ! Read off the column rather than named by the caller -- which is what lets one specific
        ! stand in for all eighteen of the per-kind ones.
        self%cache%cols(idx)%declared_kind = values%kindof()
        self%cache%cols(idx)%width = values%colwidth()
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_col
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

    **A float kind excludes a NaN from the ordering, and the reason is a trap rather than taste.**
    `min`/`max` over a NaN is not merely processor-dependent in Fortran, it is a fatal signal: both
    compile to x86 `minsd`/`maxsd`, which raise IEEE_INVALID for a QUIET-NaN operand, and nagfor
    unmasks the IEEE traps by default (`-ieee=stop`) -- so `%print_info` on a column holding a NaN
    killed the process there, in an optimised build only. Screening the NaN out is also the answer
    `pf_minmax` gives (`skipnan` defaults to excluding it) and the one Parquet's own column
    statistics record, so the three now agree. A column whose every value is NaN reports NaN rather
    than "-", which is reserved for a column with no value to report at all.
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
        base = tag[:-1] if tag.endswith("v") else tag
        fmt = NUMFMT[base]
        # A float kind screens the NaN out before it can reach `min`/`max`; see this function's
        # own docstring for why that is a trap and not a preference. An integer kind cannot hold
        # one, so it keeps the shorter body and gains neither the test nor the two extra locals.
        isreal = base in ("f32", "f64")
        elem = "p(i)" if rank == 1 else "p(e, i)"
        if isreal:
            core = f"""v = {elem}
if (v /= v) then
    if (.not. saw_nan) then
        nanv = v
        saw_nan = .true.
    end if
else if (first) then
    mn = v
    mx = v
    first = .false.
else
    mn = min(mn, v)
    mx = max(mx, v)
end if"""
        else:
            core = f"""if (first) then
    mn = {elem}
    mx = {elem}
    first = .false.
else
    mn = min(mn, {elem})
    mx = max(mx, {elem})
end if"""
        # A rank-2 kind runs the same core once per element, one indent level deeper.
        pad = " " * (16 if rank == 1 else 20)
        core = "\n".join(pad + ln for ln in core.split("\n"))
        if rank == 1:
            body = core
        else:
            body = ("                do e = 1, size(p, 1)\n"
                    + core + "\n"
                    + "                end do")
        edecl = "        integer :: e\n" if rank == 2 else ""
        nan_decl = f"        {decl} :: v, nanv\n" if isreal else ""
        nan_init = "        saw_nan = .false.\n" if isreal else ""
        nan_flag = ", saw_nan" if isreal else ""
        # Two FORD rules shape the float kinds' doc-comment, and each was learned by breaking it:
        # a continuation line is `!!`, never a second `!>` (FORD starts a new doc-comment at every
        # `!>` and parses its first line for metadata), and the first line of a MULTI-line comment
        # must not open with a bare `word:` -- so the float arm says "Smallest and largest
        # PK_FLOAT32 value" where the single-line kinds keep "PK_INT32: smallest and largest
        # value". `ford docs.md` reports "Ignoring unknown Ford metadata", once per kind, if either
        # is reverted. See CLAUDE.md's "FORD doc-comment conventions".
        nan_note = ("\n    !!\n    !! A NaN never enters the ordering. It is excluded, as "
                    "`pf_minmax` excludes it and as\n    !! Parquet's own statistics do, and a "
                    "column whose every value is NaN reports NaN.") if isreal else ""
        headline = ("Smallest and largest %s value, over the rows that hold one." % pk) if isreal \
            else ("%s: smallest and largest value, over the rows that hold one." % pk)
        if isreal:
            tail = """        if (first) then
            ! "-" is reserved for a column with nothing to report. Values that are all NaN are
            ! values, so they report NaN -- and `nanv` carries one of the column's own rather
            ! than building a fresh one, which nagfor would trap on (`0.0/0.0` raises).
            if (.not. saw_nan) return
            mn = nanv
            mx = nanv
        end if"""
        else:
            tail = "        if (first) return"
        return f"""    !> {headline}{nan_note}
    subroutine stat_{tag}(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        {decl}, pointer :: p{dims(rank)}
        {decl} :: mn, mx
{nan_decl}        integer(int64) :: i
{edecl}        logical :: first{nan_flag}
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
{nan_init}        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
{body}
        end do
{tail}
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
        integer(int64) :: i, n, imin, imax
        !
        min_s = "-"
        max_s = "-"
        call values%string_column(store)
        n = {n_elems}
        imin = 0_int64
        imax = 0_int64
        do i = 1_int64, n
            if (store%is_null(i)) cycle
            if (imin == 0_int64) then
                imin = i
                imax = i
            else
                ! Compared by INDEX, never by value. `%compare` orders two elements without
                ! materializing either, where `%get` allocates a deferred-length string per row --
                ! about 0.11 s per 4 M elements, for a scan that only ever keeps two of them
                ! (feature_risks.md Risk-60). Its ordering is Fortran's own `<`, blanks and all,
                ! so this picks exactly the winners the previous value comparison did.
                if (store%compare(i, imin) < 0) imin = i
                if (store%compare(i, imax) > 0) imax = i
            end if
        end do
        if (imin == 0_int64) return
        ! Only the two winners are materialized. Trimmed for display only: a vector string column
        ! stores its values blank-padded to the widest element, and printing that padding says
        ! nothing. Fortran's own comparison blank-pads the shorter operand anyway, so trimming
        ! cannot change which value won.
        call store%get(imin, sv)
        min_s = trim(sv)
        call store%get(imax, sv)
        max_s = trim(sv)
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
        case (PK_LIST, PK_MAP)
            ! A list or a map has no min/max VALUE -- there is no order on a whole row -- so the
            ! stat columns report the shortest and longest ROW instead, which is the one summary a
            ! reader of a %print_stat table actually wants from a ragged column.
            call stat_container_lengths(values, min_s, max_s)
        case default
            ! PK_NONE, and PK_STRUCT: nothing to summarize. A struct's rows all carry the same
            ! field count by construction, so a length extreme would print the same number twice.
            return
        end select
    end procedure table_column_stat_text
    !
    !> Shortest and longest ROW of a container column, excluding null rows.
    !!
    !! **Reads nothing.** The lengths come from the container's own offsets, which are resident
    !! whenever the column is -- %print_stat's documented contract is that it leaves a lazy table
    !! lazy, and a stat routine that triggered a read would break it silently (the table's own
    !! %print_stat test is what asserts that, not this procedure).
    !!
    !! **A NULL row has no length and is excluded from both extremes**, rather than counted as
    !! zero: a column of mostly nulls would otherwise report `min = 0` for rows that do not exist.
    !! An all-null column reports "-" for both, exactly as an unsummarizable kind does. Note this
    !! is a different question from a row of length zero, which is a real, present, empty list and
    !! IS counted -- `test/fixtures/list_widths.parquet`'s `with_empty` column has both.
    subroutine stat_container_lengths(values, min_s, max_s)
        type(parquet_column), intent(in) :: values         !! the column to summarize.
        character(len=:), allocatable, intent(out) :: min_s !! shortest present row, or "-".
        character(len=:), allocatable, intent(out) :: max_s !! longest present row, or "-".
        class(parquet_container_column), pointer :: c
        integer(int64) :: k, n, lo, hi, len_k
        logical :: seen
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        call parquet_column_container(values, c)
        if (.not. associated(c)) return
        n = c%nrows()
        seen = .false.
        lo = 0_int64
        hi = 0_int64
        do k = 1_int64, n
            if (c%is_null_row(k)) cycle
            call container_row_length(c, k, len_k)
            if (.not. seen) then
                lo = len_k
                hi = len_k
                seen = .true.
            else
                lo = min(lo, len_k)
                hi = max(hi, len_k)
            end if
        end do
        if (.not. seen) return
        write (buf, '(i0)') lo
        min_s = trim(buf)
        write (buf, '(i0)') hi
        max_s = trim(buf)
    end subroutine stat_container_lengths
    !
    !> The number of elements (list) or entries (map) in row `k` of a container.
    !!
    !! A `select type` rather than a twelfth deferred binding on the abstract base: a row length is
    !! a display feature, and the base's bindings are the ones every structural operation needs.
    !! Adding one there would oblige every future container type to implement it for a `%print_stat`
    !! column -- see feature_container_phase6.md's Q4.
    subroutine container_row_length(c, k, n)
        class(parquet_container_column), intent(in) :: c !! the container.
        integer(int64), intent(in) :: k                  !! 1-based row.
        integer(int64), intent(out) :: n                 !! elements/entries in that row.
        !
        n = 0_int64
        select type (c)
        type is (parquet_list_column)
            n = c%length(k)
        type is (parquet_map_column)
            n = c%length(k)
        class default ! GCOVR_EXCL_START -- only PK_LIST and PK_MAP reach here; PK_STRUCT is
            ! handled by table_column_stat_text's own case default above.
            n = 0_int64
        end select ! GCOVR_EXCL_STOP
    end subroutine container_row_length
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
              "            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)",
              f"            allocate(arr{'(size(p, kind=int64))' if rank == 1 else '(size(p, 1, kind=int64), size(p, 2, kind=int64))'})",
              "            arr = p"]
    for srcpk, src in WIDEN.get(tag, []):
        lines += [f"        case ({srcpk})",
                  f"            call parquet_column_data_ptr(self%cache%cols(idx)%values, p_{src})",
                  f"            allocate(arr{f'(size(p_{src}, kind=int64))' if rank == 1 else f'(size(p_{src}, 1, kind=int64), size(p_{src}, 2, kind=int64))'})",
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
        call parquet_column_string_column(self%cache%cols(idx)%values, src)
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
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! `%length` measures without allocating and `%copy_to` fills a fixed-length slot without
        ! allocating, so neither pass materializes a string. `maxlen` starts at 1 so that an
        ! all-empty column still yields `character(len=1)` rather than `len=0`.
        maxlen = 1
        do i = 1, n
            if (int(store%length(i)) > maxlen) maxlen = int(store%length(i))
        end do
        allocate(character(len=maxlen) :: arr(n))
        do i = 1, n
            call store%copy_to(i, arr(i), allow_null=.true.)
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
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, n))
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%copy_to(flat, arr(e, i), allow_null=.true.)
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
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
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
    a public numeric argument. The worker runs `col_store_<tag>`, the same body a column handle's
    `%set` runs; `tag` is the accessor's name and BODY_TAG maps the two string ones onto theirs.
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
        call table_require_row(self, i, "set_element")
        call col_store_{BODY_TAG.get(tag, tag)}(self%cache, idx, i, value, "set_element")
    end procedure set_element_{tag}_i64
    !"""


ZERO = {
    "i32": "0_int32", "i64": "0_int64", "f32": "0.0_real32", "f64": "0.0_real64",
    "bool": ".false.",
}


def getelem_impl(k):
    """One row's value by name, over the SAME body a column handle's `%get` runs.

    The int32 specific forwards to the int64 one; the int64 one resolves the name, checks the
    row, and hands the resolved pieces to `col_fetch_<tag>`. Every rule about this operation --
    the widening set, the kind error, the null convention -- lives in that body and nowhere else,
    which is what stops the two spellings of one operation from answering differently
    (feature_colindex.md 6.3). It is the FIVE-ARGUMENT variant, not the handle-delegating sketch:
    building a `parquet_table_col` here purely to delegate through it measured +16.3% on this
    accessor, three times criterion (3)'s bar, against +2.16% for this shape.
    """
    tag, pk, decl, comp, rank, cat = k
    out = [f"""    module procedure get_element_{tag}_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_{tag}_i32
    !"""]
    # The miss path must leave `value` DEFINED and EMPTY -- the same rule %get and %get_slice
    # follow, so that a program which ignores `found` reads an empty result rather than something
    # undefined. A scalar numeric kind gets its type's zero; a scalar temporal one gets a
    # default-initialised element, which IS its null state (CLAUDE.md's parquet_temporal note), so
    # there is no zero constant to use; a vector kind gets a zero-length array, exactly as
    # %get's own vector forms allocate arr(0,0) on a miss.
    #
    # The allocation has to sit INSIDE the miss branch, not before the lookup: on a hit,
    # `col_fetch_<tag>` allocates `value` itself, and allocating it here first would abort.
    miss_decl = ""
    if rank == 2:
        miss = "            allocate(value(0))"
    elif cat == "num":
        miss = f"            value = {ZERO[tag]}"
    else:
        miss_decl, miss = f"        {decl} :: blank\n", "            value = blank"
    out.append(f"""    module procedure get_element_{tag}_i64
        integer :: idx
{miss_decl}        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
{miss}
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_{tag}(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_{tag}_i64""")
    return "\n".join(out)


def _getelem_str_pair(tag, body):
    """One string kind's two `%get_element` specifics. `tag` is the accessor's name (`chr`), and
    `body` the kind table's (`str`) -- see BODY_TAG."""
    # Same miss rule as the typed forms above: defined and empty, inside the branch.
    blank = ('            value = ""' if body == "str"
             else "            allocate(character(len=1) :: value(0))")
    return f"""    module procedure get_element_{tag}_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_{tag}_i32
    !
    module procedure get_element_{tag}_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
{blank}
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_{body}(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_{tag}_i64
    !"""


def getelem_str_impl():
    return "\n".join([_getelem_str_pair("chr", "str"), _getelem_str_pair("chrv", "strv")])


def setelem_impl(k):
    return _setelem_pair(k[0], k[1])


def setelem_str_impl():
    return "\n".join([_setelem_pair("chr", "PK_STRING"), _setelem_pair("chrv", "PK_STRING_VEC")])


def set_str_impl():
    return """    module procedure set_arr_chr
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
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
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
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
# src/parquet_tables_colaccess.f90 -- the column handle's per-kind accessors, and the SHARED
# bodies the table's own %get_element calls.
#
# All 18 kinds have a shared body, and every table-level `%get_element`/`%set_element` is a
# caller of one -- there is no second copy of the widening set, the kind error or the null rule
# anywhere in this layer. Stage 3a built the 8 allocation-free scalar kinds first (where the
# measured speed case lives) and 3b added the 10 allocating ones; the split is gone now that
# both halves exist.
# --------------------------------------------------------------------------------------
# Kinds whose element can be reached one at a time WITHIN a row: the 9 rank-2 kinds. This is new
# capability rather than a faster spelling -- the table has no `%get_element(name, i, e, v)` to
# wrap, because reading one element of a vector row has never been expressible without
# materialising the whole row (feature_colindex.md 4.1).
VEC_KINDS = [k for k in KINDS if k[4] == 2]

# Table-level `%get_element`/`%set_element` name the two string kinds `chr`/`chrv`, while the
# kind table -- and therefore every shared body -- names them `str`/`strv`. One map rather than a
# second naming convention.
BODY_TAG = {"chr": "str", "chrv": "strv"}


def fetch_val_decl(k):
    """Declaration of `value` on a READ: the shape the caller receives.

    A vector kind hands back an allocatable array (the body allocates it to the column's width),
    and a string kind an allocatable of deferred length, because neither length nor width is
    something the caller can be asked to know in advance.
    """
    tag, pk, decl, comp, rank, cat = k
    if cat == "str":
        return "character(len=:), allocatable, intent(out) :: value" + ("(:)" if rank == 2 else "")
    if rank == 2:
        return f"{decl}, allocatable, intent(out) :: value(:)"
    return f"{decl}, intent(out) :: value"


def store_val_decl(k):
    """Declaration of `value` on a WRITE. A character ARRAY is trimmed on the way in and a
    character SCALAR is not -- `parquet_column`'s rule, inherited rather than restated here."""
    tag, pk, decl, comp, rank, cat = k
    base = "character(len=*)" if cat == "str" else decl
    return f"{base}, intent(in) :: value" + ("(:)" if rank == 2 else "")


def fetch_cache_attr(k):
    """`, target` for the one body that takes a POINTER into the cache.

    `col_fetch_strv` needs `%string_column(store)`, and a pointer to a subobject of a plain dummy
    is undefined the moment that dummy goes out of scope. Only that body needs it, so only that
    body pays whatever the attribute costs the optimiser.
    """
    return ", target" if k[0] == "strv" else ""


def col_fetch_impl(k):
    """The SHARED body behind both `%get_element(name, i, v)` and a handle's `%get(i, v)`.

    Takes the resolved pieces rather than a handle: building a `parquet_table_col` purely to
    delegate through it measured +16.3% on `%get_element`, against criterion (3)'s 5% bar, while
    this shape measured +2.16% (feature_colindex.md 2-ii). `proc` is the CALLER's name so each
    entry point keeps the messages it always produced.
    """
    tag, pk, decl, comp, rank, cat = k
    if cat == "str":
        return col_fetch_str_impl(k)
    lines = [f"    module procedure col_fetch_{tag}"]
    for _, src in WIDEN.get(tag, []):
        srcdecl = next(kk[2] for kk in KINDS if kk[0] == src)
        lines.append(f"        {srcdecl} :: v_{src}" if rank == 1
                     else f"        {srcdecl}, allocatable :: v_{src}(:)")
    lines += ["        !",
              "        select case (colkind)",
              f"        case ({pk})"]
    if rank == 1:
        lines.append("            call parquet_column_get_at(cache%cols(slot)%values, i, value)")
    else:
        lines += ["            allocate(value(cache%cols(slot)%width))",
                  "            call parquet_column_get_at(cache%cols(slot)%values, i, value)"]
    for srcpk, src in WIDEN.get(tag, []):
        lines.append(f"        case ({srcpk})")
        if rank == 1:
            lines += [f"            call parquet_column_get_at(cache%cols(slot)%values, i, v_{src})",
                      f"            value = v_{src}"]
        else:
            lines += [f"            allocate(v_{src}(cache%cols(slot)%width))",
                      f"            call parquet_column_get_at(cache%cols(slot)%values, i, v_{src})",
                      "            allocate(value(cache%cols(slot)%width))",
                      f"            value = v_{src}"]
    lines += ["        case default",
              # The SAME wording a name-form accessor produces, via the same helper. Inventing a
              # second one here is exactly the drift a shared body exists to prevent, and it is
              # invisible until someone compares two messages side by side.
              f'            call cache_require_kind(cache, slot, {pk}, proc)',
              "        end select",
              f"    end procedure col_fetch_{tag}",
              "    !"]
    return "\n".join(lines)


def col_fetch_str_impl(k):
    """The two string kinds' shared read bodies. No widening -- there is nothing to widen to."""
    if k[4] == 1:
        return """    module procedure col_fetch_str
        call cache_require_kind(cache, slot, PK_STRING, proc)
        ! %get_at reads with allow_null, so a null row comes back as "" rather than aborting;
        ! %is_null is how a caller tells an empty string from a missing one.
        call parquet_column_get_at(cache%cols(slot)%values, i, value)
    end procedure col_fetch_str
    !"""
    return """    module procedure col_fetch_strv
        integer :: e, wdt, maxlen
        integer(int64) :: flat
        type(parquet_string_column), pointer :: store
        !
        call cache_require_kind(cache, slot, PK_STRING_VEC, proc)
        wdt = cache%cols(slot)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, row) at
        ! (row-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call parquet_column_string_column(cache%cols(slot)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%copy_to(flat, value(e), allow_null=.true.)
        end do
    end procedure col_fetch_strv
    !"""


def col_get_impl(k):
    """The handle's own `%get`: validate the handle and the row, then the shared body."""
    tag = k[0]
    return f"""    module procedure col_get_{tag}_i32
        call self%get(int(i, int64), value)
    end procedure col_get_{tag}_i32
    !
    module procedure col_get_{tag}_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_{tag}(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_{tag}_i64
    !"""


def col_store_impl(k):
    """The SHARED body behind both `%set_element(name, i, v)` and a handle's `%set(i, v)`.

    Exact kind, never widening -- a write that silently converted would lose information the
    caller did not agree to lose. Mirrors `col_fetch_*`'s five-argument shape for the same
    measured reason.
    """
    tag, pk = k[0], k[1]
    return f"""    module procedure col_store_{tag}
        call cache_require_kind(cache, slot, {pk}, proc)
        call parquet_column_set_at(cache%cols(slot)%values, i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_{tag}
    !"""


def col_set_impl(k):
    """The handle's own `%set`: validate the handle, the row and the shared-write rule, then the
    shared body. `nulling=.false.` -- writing a VALUE cannot be the first null."""
    tag = k[0]
    return f"""    module procedure col_set_{tag}_i32
        call self%set(int(i, int64), value)
    end procedure col_set_{tag}_i32
    !
    module procedure col_set_{tag}_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_{tag}(self%cache, self%slot, i, value, "set")
    end procedure col_set_{tag}_i64
    !"""


def col_getelem_impl(k):
    """One ELEMENT of one row, through a handle.

    New capability rather than a faster spelling: the table has no name-taking counterpart,
    because reading a single element of a vector row has never been expressible without
    materialising the whole row (feature_colindex.md 4.1).

    It runs `parquet_column%get_elem`, which was added for this and is where the bounds check and
    the storage layout live. Reaching around it through `%data_ptr` would have put a second copy
    of that column's own rules in this module -- and on the write side, silently wrong ones.
    """
    tag, pk, decl, comp, rank, cat = k
    lines = [f"""    module procedure col_get_{tag}_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_{tag}_e32
    !""",
             f"    module procedure col_get_{tag}_e64"]
    for _, src in WIDEN.get(tag, []):
        srcdecl = next(kk[2] for kk in KINDS if kk[0] == src)
        lines.append(f"        {srcdecl} :: v_{src}")
    lines += ["        !",
              '        call col_resolve(self, "get")',
              '        call col_require_row(self, i, "get")',
              "        select case (self%colkind)",
              f"        case ({pk})",
              "            call parquet_column_get_elem(self%cache%cols(self%slot)%values, i, e, value)"]
    for srcpk, src in WIDEN.get(tag, []):
        lines += [f"        case ({srcpk})",
                  f"            call parquet_column_get_elem(self%cache%cols(self%slot)%values, i, e, v_{src})",
                  f"            value = v_{src}"]
    lines += ["        case default",
              f'            call cache_require_kind(self%cache, self%slot, {pk}, "get")',
              "        end select",
              f"    end procedure col_get_{tag}_e64",
              "    !"]
    return "\n".join(lines)


def col_setelem_impl(k):
    """Writes one ELEMENT of one row, through a handle.

    `parquet_column%set_elem` carries the validity rule, and that rule is not one rule: a numeric
    element's null lives in the column's bitmap, a temporal element IS its own null state, and a
    string element's lives in the string store. All three are private to that type, and a write
    that bypassed them would be silent -- the value stored, the cell still reading back as
    missing, or `%any_null()` answering from a cache nothing invalidated.
    """
    tag, pk = k[0], k[1]
    return f"""    module procedure col_set_{tag}_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_{tag}_e32
    !
    module procedure col_set_{tag}_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, {pk}, "set")
        call parquet_column_set_elem(self%cache%cols(self%slot)%values, i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_{tag}_e64
    !"""


def gen_colaccess():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Per-kind value access for `parquet_table_col`, and the shared bodies the table's own
!! `%get_element` calls so the two forms cannot answer differently.
!!
!! Every `col_fetch_<tag>` takes the resolved PIECES -- cache, slot, kind -- rather than a handle,
!! because building a handle purely to pass it costs more than the body costs to run. The handle's
!! own `%get` validates itself and then calls exactly the same body.
submodule (parquet_tables) parquet_tables_colaccess
    implicit none
    !
contains
    !""")
    for k in KINDS:
        w(col_fetch_impl(k))
    for k in KINDS:
        w(col_get_impl(k))
    for k in KINDS:
        w(col_store_impl(k))
    for k in KINDS:
        w(col_set_impl(k))
    for k in VEC_KINDS:
        w(col_getelem_impl(k))
    for k in VEC_KINDS:
        w(col_setelem_impl(k))
    for k in PTR_KINDS:
        w(col_ref_impl(k))
    w(col_ref_str_impl())
    w("end submodule parquet_tables_colaccess ! GCOVR_EXCL_LINE")
    return "\n".join(o)


# --------------------------------------------------------------------------------------
# src/parquet_tables_addcol.f90
# --------------------------------------------------------------------------------------
def gen_container():
    o = []
    w = o.append
    w(BANNER)
    w("""!> Container-column access for `parquet_table`: the `%col`, `%get`, `%set`, `%add_column` and
!! `%ref` specifics for a `parquet_list_column`, a `parquet_map_column` and a
!! `parquet_struct_column`.
!!
!! **Five specifics per type, and deliberately no more.** They mirror exactly what
!! `parquet_string_column` gets, and decline the same three families for the same reason:
!! `%get_slice`/`%set_slice`, `%get_element`/`%set_element` and `%row%get`/`%set` all address a
!! fixed-width CELL, and a container row is a variable-length object with no such shape. A caller
!! that wants one row reaches the container through `%col` and asks it directly.
!!
!! **None of them takes an `is_valid=` mask**, matching the temporal kinds rather than the array
!! kinds: a container holds its own per-row nullness (`is_null_row`), and `adopt_container`
!! deliberately leaves the surrounding `parquet_column`'s own bitmap unallocated so that there is
!! exactly one answer to "is row i null?". A mask argument here would be the second one.
!! `%get_valid_mask` still answers for a container column, in its rank-1 form -- that is the
!! table-level way to ask, and it reads through to the container.
submodule (parquet_tables) parquet_tables_container
    implicit none
    !
contains
    !""")
    w(container_downcasts())
    w(container_impls())
    w("end submodule parquet_tables_container ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


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
    w(add_col_impl())
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
    for c in READ_CONTAINERS:
        w(mat_container_impl(c))
    for c in READ_CONTAINERS:
        w(matchunk_container_impl(c))
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
        # The rank-1 (per-ROW) form of %set_validity, which writes the bitmap a word at a time.
        # The loop this replaces was one type-bound call per null row, each redoing check_index
        # and the kind select case before touching a single bit.
        null_loop = "                call col%set_validity(valid)"
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
    # Both ranks now hand the whole mask over: rank 1 is per ROW, rank 2 per ELEMENT.
    null_loop = "                call col%set_validity(valid)"
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
        root / "src" / "parquet_tables_colaccess.f90": gen_colaccess(),
        root / "src" / "parquet_tables_addcol.f90": gen_addcol(),
        root / "src" / "parquet_tables_materialize.f90": gen_materialize(),
        root / "src" / "parquet_tables_container.f90": gen_container(),
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
