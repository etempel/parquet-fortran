!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> A whole parquet file as one in-memory table: `parquet_table`.
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
!! * **Mutation is NOT thread-safe.** Reading already-resident columns from several threads is
!!   fine and takes no lock, and each thread may open and read its own table. A first touch on a
!!   table shared across a parallel region is a hard error -- prefetch before the region instead.
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
        parquet_get_num_row_groups, parquet_get_chunk_size, parquet_read_column_chunk, &
        parquet_open_writer, parquet_write_column, parquet_close_writer, parquet_write_row_mask, &
        parquet_measure_list_width, parquet_column_width_needs_data, parquet_column_has_nulls, &
        parquet_load_qc_maml_file, parquet_filter, parquet_sortkey, parquet_read_qc, &
        parquet_compose_read_qc, parquet_reader_set_filter, parquet_parse_maml
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
        logical :: file_backed = .false.                   !! .true. if opened from a parquet file.
        character(len=:), allocatable :: source_file       !! the file this table was opened from.
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
        integer(int64) :: slice_row_lo = 0                 !! slice's first FILE row (0 = not a slice).
        integer(int64) :: slice_row_hi = 0                 !! slice's last FILE row (0 = not a slice).
        logical :: opened_in_parallel = .false.            !! .true. if opened inside a parallel region.
        integer :: owner_thread = -1                       !! OpenMP thread that opened it (-1 if serial).
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
    !
    !> A whole table: a column store plus the row scope and provenance describing it.
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
        procedure :: has_column => table_has_column  !! Whether a column of this name exists.
        procedure :: kind => table_column_kind       !! A column's PK_* kind discriminator.
        procedure :: width => table_column_width     !! A column's values-per-row (1 if scalar).
        procedure :: unit => table_column_unit       !! Copy out a column's unit string.
        procedure :: residency => table_column_residency !! A column's RES_* residency state.
        procedure, private :: is_null_i32 => table_is_null_i32 !! %is_null specific, int32 row index.
        procedure, private :: is_null_i64 => table_is_null_i64 !! %is_null specific, int64 row index.
        !> Whether row `i` of a column is null. ROW-granular: on a *_VEC column it answers for the
        !! whole row, since a single element of a vector row cannot be null on its own.
        generic :: is_null => is_null_i32, is_null_i64
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
        procedure :: row_group_bounds => table_row_group_bounds !! Row-group row ranges, this table's rows or the file's.
        ! --- row view ---
        procedure, private :: row_at_i32 !! %row specific taking an int32 index.
        procedure, private :: row_at_i64 !! %row specific taking an int64 index.
        !> A handle on one row, for code that works a row at a time rather than a column at a
        !! time. The index is 1-based within THIS table -- in the slice regime, row 1 is the
        !! slice's first row, not the file's.
        generic :: row => row_at_i32, row_at_i64
        ! --- copy out a row selection ---
        procedure, private :: get_slice_i32 !! %get_slice specific for the i32 kind.
        procedure, private :: get_slice_i64 !! %get_slice specific for the i64 kind.
        procedure, private :: get_slice_f32 !! %get_slice specific for the f32 kind.
        procedure, private :: get_slice_f64 !! %get_slice specific for the f64 kind.
        procedure, private :: get_slice_bool !! %get_slice specific for the bool kind.
        procedure, private :: get_slice_date !! %get_slice specific for the date kind.
        procedure, private :: get_slice_time !! %get_slice specific for the time kind.
        procedure, private :: get_slice_ts !! %get_slice specific for the ts kind.
        procedure, private :: get_slice_i32v !! %get_slice specific for the i32v kind.
        procedure, private :: get_slice_i64v !! %get_slice specific for the i64v kind.
        procedure, private :: get_slice_f32v !! %get_slice specific for the f32v kind.
        procedure, private :: get_slice_f64v !! %get_slice specific for the f64v kind.
        procedure, private :: get_slice_boolv !! %get_slice specific for the boolv kind.
        procedure, private :: get_slice_datev !! %get_slice specific for the datev kind.
        procedure, private :: get_slice_timev !! %get_slice specific for the timev kind.
        procedure, private :: get_slice_tsv !! %get_slice specific for the tsv kind.
        procedure, private :: get_slice_str  !! %get_slice specific returning a parquet_string_column.
        procedure, private :: get_slice_chr  !! %get_slice specific returning a character array.
        procedure, private :: get_slice_chrv !! %get_slice specific returning a character (elem, row) array.
        !> Copies the rows a `parquet_slice` selects into a freshly allocated array of
        !! the caller's own kind, widening on the way exactly as %get does.
        generic :: get_slice => get_slice_i32, get_slice_i64, get_slice_f32, get_slice_f64, get_slice_bool, get_slice_date, &
            get_slice_time, get_slice_ts, get_slice_i32v, get_slice_i64v, get_slice_f32v, get_slice_f64v, get_slice_boolv, &
            get_slice_datev, get_slice_timev, get_slice_tsv, get_slice_str, get_slice_chr, get_slice_chrv
        ! --- zero-copy pointer access (exact kind) ---
        procedure, private :: col_ptr_i32 !! %col specific for the i32 kind.
        procedure, private :: col_ptr_i64 !! %col specific for the i64 kind.
        procedure, private :: col_ptr_f32 !! %col specific for the f32 kind.
        procedure, private :: col_ptr_f64 !! %col specific for the f64 kind.
        procedure, private :: col_ptr_bool !! %col specific for the bool kind.
        procedure, private :: col_ptr_date !! %col specific for the date kind.
        procedure, private :: col_ptr_time !! %col specific for the time kind.
        procedure, private :: col_ptr_ts !! %col specific for the ts kind.
        procedure, private :: col_ptr_i32v !! %col specific for the i32v kind.
        procedure, private :: col_ptr_i64v !! %col specific for the i64v kind.
        procedure, private :: col_ptr_f32v !! %col specific for the f32v kind.
        procedure, private :: col_ptr_f64v !! %col specific for the f64v kind.
        procedure, private :: col_ptr_boolv !! %col specific for the boolv kind.
        procedure, private :: col_ptr_datev !! %col specific for the datev kind.
        procedure, private :: col_ptr_timev !! %col specific for the timev kind.
        procedure, private :: col_ptr_tsv !! %col specific for the tsv kind.
        !> Points `p` at a column's storage: zero copy, writable, and the pointer kind must
        !! match the stored kind exactly (ask %kind first if you do not know it).
        generic :: col => col_ptr_i32, col_ptr_i64, col_ptr_f32, col_ptr_f64, col_ptr_bool, col_ptr_date, col_ptr_time, &
            col_ptr_ts, col_ptr_i32v, col_ptr_i64v, col_ptr_f32v, col_ptr_f64v, col_ptr_boolv, col_ptr_datev, col_ptr_timev, &
            col_ptr_tsv
        ! --- copy out (widens int32->int64, float32->float64) ---
        procedure, private :: get_arr_i32 !! %get specific for the i32 kind.
        procedure, private :: get_arr_i64 !! %get specific for the i64 kind.
        procedure, private :: get_arr_f32 !! %get specific for the f32 kind.
        procedure, private :: get_arr_f64 !! %get specific for the f64 kind.
        procedure, private :: get_arr_bool !! %get specific for the bool kind.
        procedure, private :: get_arr_date !! %get specific for the date kind.
        procedure, private :: get_arr_time !! %get specific for the time kind.
        procedure, private :: get_arr_ts !! %get specific for the ts kind.
        procedure, private :: get_arr_i32v !! %get specific for the i32v kind.
        procedure, private :: get_arr_i64v !! %get specific for the i64v kind.
        procedure, private :: get_arr_f32v !! %get specific for the f32v kind.
        procedure, private :: get_arr_f64v !! %get specific for the f64v kind.
        procedure, private :: get_arr_boolv !! %get specific for the boolv kind.
        procedure, private :: get_arr_datev !! %get specific for the datev kind.
        procedure, private :: get_arr_timev !! %get specific for the timev kind.
        procedure, private :: get_arr_tsv !! %get specific for the tsv kind.
        procedure, private :: get_arr_str  !! %get specific returning a parquet_string_column.
        procedure, private :: get_arr_chr  !! %get specific returning a character array.
        procedure, private :: get_arr_chrv !! %get specific returning a character (elem, row) array.
        !> Copies a column into a freshly allocated array of the caller's own kind.
        generic :: get => get_arr_i32, get_arr_i64, get_arr_f32, get_arr_f64, get_arr_bool, get_arr_date, get_arr_time, &
            get_arr_ts, get_arr_i32v, get_arr_i64v, get_arr_f32v, get_arr_f64v, get_arr_boolv, get_arr_datev, get_arr_timev, &
            get_arr_tsv, get_arr_str, get_arr_chr, get_arr_chrv
        ! --- copy back (same length, exact kind) ---
        procedure, private :: set_arr_i32 !! %set specific for the i32 kind.
        procedure, private :: set_arr_i64 !! %set specific for the i64 kind.
        procedure, private :: set_arr_f32 !! %set specific for the f32 kind.
        procedure, private :: set_arr_f64 !! %set specific for the f64 kind.
        procedure, private :: set_arr_bool !! %set specific for the bool kind.
        procedure, private :: set_arr_date !! %set specific for the date kind.
        procedure, private :: set_arr_time !! %set specific for the time kind.
        procedure, private :: set_arr_ts !! %set specific for the ts kind.
        procedure, private :: set_arr_i32v !! %set specific for the i32v kind.
        procedure, private :: set_arr_i64v !! %set specific for the i64v kind.
        procedure, private :: set_arr_f32v !! %set specific for the f32v kind.
        procedure, private :: set_arr_f64v !! %set specific for the f64v kind.
        procedure, private :: set_arr_boolv !! %set specific for the boolv kind.
        procedure, private :: set_arr_datev !! %set specific for the datev kind.
        procedure, private :: set_arr_timev !! %set specific for the timev kind.
        procedure, private :: set_arr_tsv !! %set specific for the tsv kind.
        procedure, private :: set_arr_chr  !! %set specific taking a character array.
        procedure, private :: set_arr_chrv !! %set specific taking a character (elem, row) array.
        !> Replaces every value of an existing column from an array of the same length.
        generic :: set => set_arr_i32, set_arr_i64, set_arr_f32, set_arr_f64, set_arr_bool, set_arr_date, set_arr_time, &
            set_arr_ts, set_arr_i32v, set_arr_i64v, set_arr_f32v, set_arr_f64v, set_arr_boolv, set_arr_datev, set_arr_timev, &
            set_arr_tsv, set_arr_chr, set_arr_chrv
        ! --- from-scratch construction ---
        procedure, private :: add_column_i32 !! %add_column specific for the i32 kind.
        procedure, private :: add_column_i64 !! %add_column specific for the i64 kind.
        procedure, private :: add_column_f32 !! %add_column specific for the f32 kind.
        procedure, private :: add_column_f64 !! %add_column specific for the f64 kind.
        procedure, private :: add_column_bool !! %add_column specific for the bool kind.
        procedure, private :: add_column_date !! %add_column specific for the date kind.
        procedure, private :: add_column_time !! %add_column specific for the time kind.
        procedure, private :: add_column_ts !! %add_column specific for the ts kind.
        procedure, private :: add_column_i32v !! %add_column specific for the i32v kind.
        procedure, private :: add_column_i64v !! %add_column specific for the i64v kind.
        procedure, private :: add_column_f32v !! %add_column specific for the f32v kind.
        procedure, private :: add_column_f64v !! %add_column specific for the f64v kind.
        procedure, private :: add_column_boolv !! %add_column specific for the boolv kind.
        procedure, private :: add_column_datev !! %add_column specific for the datev kind.
        procedure, private :: add_column_timev !! %add_column specific for the timev kind.
        procedure, private :: add_column_tsv !! %add_column specific for the tsv kind.
        procedure, private :: add_column_chr  !! %add_column specific taking a character array.
        procedure, private :: add_column_chrv !! %add_column specific taking a character (elem, row) array.
        !> Appends a new column, taking its values (and so its kind, width and row count).
        generic :: add_column => add_column_i32, add_column_i64, add_column_f32, add_column_f64, add_column_bool, &
            add_column_date, add_column_time, add_column_ts, add_column_i32v, add_column_i64v, add_column_f32v, add_column_f64v, &
            add_column_boolv, add_column_datev, add_column_timev, add_column_tsv, add_column_chr, add_column_chrv
        ! --- mutation: one cell at a time (never changes the row set) ---
        procedure, private :: set_element_i32_i32 !! %set_element specific, i32 kind, i32 row index.
        procedure, private :: set_element_i32_i64 !! %set_element specific, i32 kind, i64 row index.
        procedure, private :: set_element_i64_i32 !! %set_element specific, i64 kind, i32 row index.
        procedure, private :: set_element_i64_i64 !! %set_element specific, i64 kind, i64 row index.
        procedure, private :: set_element_f32_i32 !! %set_element specific, f32 kind, i32 row index.
        procedure, private :: set_element_f32_i64 !! %set_element specific, f32 kind, i64 row index.
        procedure, private :: set_element_f64_i32 !! %set_element specific, f64 kind, i32 row index.
        procedure, private :: set_element_f64_i64 !! %set_element specific, f64 kind, i64 row index.
        procedure, private :: set_element_bool_i32 !! %set_element specific, bool kind, i32 row index.
        procedure, private :: set_element_bool_i64 !! %set_element specific, bool kind, i64 row index.
        procedure, private :: set_element_date_i32 !! %set_element specific, date kind, i32 row index.
        procedure, private :: set_element_date_i64 !! %set_element specific, date kind, i64 row index.
        procedure, private :: set_element_time_i32 !! %set_element specific, time kind, i32 row index.
        procedure, private :: set_element_time_i64 !! %set_element specific, time kind, i64 row index.
        procedure, private :: set_element_ts_i32 !! %set_element specific, ts kind, i32 row index.
        procedure, private :: set_element_ts_i64 !! %set_element specific, ts kind, i64 row index.
        procedure, private :: set_element_i32v_i32 !! %set_element specific, i32v kind, i32 row index.
        procedure, private :: set_element_i32v_i64 !! %set_element specific, i32v kind, i64 row index.
        procedure, private :: set_element_i64v_i32 !! %set_element specific, i64v kind, i32 row index.
        procedure, private :: set_element_i64v_i64 !! %set_element specific, i64v kind, i64 row index.
        procedure, private :: set_element_f32v_i32 !! %set_element specific, f32v kind, i32 row index.
        procedure, private :: set_element_f32v_i64 !! %set_element specific, f32v kind, i64 row index.
        procedure, private :: set_element_f64v_i32 !! %set_element specific, f64v kind, i32 row index.
        procedure, private :: set_element_f64v_i64 !! %set_element specific, f64v kind, i64 row index.
        procedure, private :: set_element_boolv_i32 !! %set_element specific, boolv kind, i32 row index.
        procedure, private :: set_element_boolv_i64 !! %set_element specific, boolv kind, i64 row index.
        procedure, private :: set_element_datev_i32 !! %set_element specific, datev kind, i32 row index.
        procedure, private :: set_element_datev_i64 !! %set_element specific, datev kind, i64 row index.
        procedure, private :: set_element_timev_i32 !! %set_element specific, timev kind, i32 row index.
        procedure, private :: set_element_timev_i64 !! %set_element specific, timev kind, i64 row index.
        procedure, private :: set_element_tsv_i32 !! %set_element specific, tsv kind, i32 row index.
        procedure, private :: set_element_tsv_i64 !! %set_element specific, tsv kind, i64 row index.
        procedure, private :: set_element_chr_i32 !! %set_element specific, character chr form, i32 row index.
        procedure, private :: set_element_chr_i64 !! %set_element specific, character chr form, i64 row index.
        procedure, private :: set_element_chrv_i32 !! %set_element specific, character chrv form, i32 row index.
        procedure, private :: set_element_chrv_i64 !! %set_element specific, character chrv form, i64 row index.
        !> Writes one row's value in place. The kind must match the column's exactly (as
        !! %set does), and writing a value CLEARS that row's null -- use %set_null to put
        !! one back. On a *_VEC column the value is that row's whole vector.
        generic :: set_element => set_element_i32_i32, set_element_i32_i64, set_element_i64_i32, set_element_i64_i64, &
            set_element_f32_i32, set_element_f32_i64, set_element_f64_i32, set_element_f64_i64, set_element_bool_i32, &
            set_element_bool_i64, set_element_date_i32, set_element_date_i64, set_element_time_i32, set_element_time_i64, &
            set_element_ts_i32, set_element_ts_i64, set_element_i32v_i32, set_element_i32v_i64, set_element_i64v_i32, &
            set_element_i64v_i64, set_element_f32v_i32, set_element_f32v_i64, set_element_f64v_i32, set_element_f64v_i64, &
            set_element_boolv_i32, set_element_boolv_i64, set_element_datev_i32, set_element_datev_i64, set_element_timev_i32, &
            set_element_timev_i64, set_element_tsv_i32, set_element_tsv_i64, set_element_chr_i32, set_element_chr_i64, &
            set_element_chrv_i32, set_element_chrv_i64
        procedure, private :: set_null_i32   !! %set_null specific taking an int32 row index.
        procedure, private :: set_null_i64   !! %set_null specific taking an int64 row index.
        !> Marks row `i` of a column null. ROW-granular even on a *_VEC column, where it nulls
        !! every element of the row -- a single element of a vector row cannot be nulled.
        generic :: set_null => set_null_i32, set_null_i64
        procedure, private :: clear_null_i32 !! %clear_null specific taking an int32 row index.
        procedure, private :: clear_null_i64 !! %clear_null specific taking an int64 row index.
        !> Marks row `i` of a column valid without saying what its value is. Only useful when a
        !! value is already there or is about to be written; %set_element clears the null itself.
        generic :: clear_null => clear_null_i32, clear_null_i64
        procedure :: compact_validity => table_compact_validity !! Drop a null bitmap that no longer has nulls.
        ! --- mutation: whole columns (never changes the row set) ---
        procedure :: drop_column => table_drop_column     !! Remove a column; force= for a predefined one.
        procedure :: rename_column => table_rename_column !! Change the name a column is looked up by.
        procedure :: copy_column => table_copy_column     !! Add a copy of a column, optionally of another kind.
        procedure :: cast => table_cast                   !! Convert a column to another kind, in place.
        ! --- mutation: the row set itself -- every one of these DETACHES the table ---
        procedure :: filter_rows => table_filter_rows !! Keep only the rows a mask selects.
        procedure :: sort_by => table_sort_by         !! Reorder rows by one or more key columns.
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
        ! --- lifecycle ---
        !> Blocks intrinsic assignment: the store lives behind a pointer, so a default `b = a`
        !! would leave two tables sharing one store and double-freeing it.
        generic :: assignment(=) => table_assign_guard
        procedure, private :: table_assign_guard !! The blocking defined assignment.
        final :: table_finalize                  !! Frees the store; never fails, never validates.
    end type parquet_table
    !
    !> One row of a table, as a lightweight handle: `r = t%row(i)`.
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
    contains
        procedure, private :: row_get_i32 !! %get specific for the i32 kind.
        procedure, private :: row_get_i64 !! %get specific for the i64 kind.
        procedure, private :: row_get_f32 !! %get specific for the f32 kind.
        procedure, private :: row_get_f64 !! %get specific for the f64 kind.
        procedure, private :: row_get_bool !! %get specific for the bool kind.
        procedure, private :: row_get_str !! %get specific for the str kind.
        procedure, private :: row_get_date !! %get specific for the date kind.
        procedure, private :: row_get_time !! %get specific for the time kind.
        procedure, private :: row_get_ts !! %get specific for the ts kind.
        procedure, private :: row_get_i32v !! %get specific for the i32v kind.
        procedure, private :: row_get_i64v !! %get specific for the i64v kind.
        procedure, private :: row_get_f32v !! %get specific for the f32v kind.
        procedure, private :: row_get_f64v !! %get specific for the f64v kind.
        procedure, private :: row_get_boolv !! %get specific for the boolv kind.
        procedure, private :: row_get_strv !! %get specific for the strv kind.
        procedure, private :: row_get_datev !! %get specific for the datev kind.
        procedure, private :: row_get_timev !! %get specific for the timev kind.
        procedure, private :: row_get_tsv !! %get specific for the tsv kind.
        !> Copies this row's value for a column into the caller's own variable, widening
        !! int32 -> int64 and float32 -> float64 exactly as the table's own %get does.
        generic :: get => row_get_i32, row_get_i64, row_get_f32, row_get_f64, row_get_bool, row_get_str, row_get_date, &
            row_get_time, row_get_ts, row_get_i32v, row_get_i64v, row_get_f32v, row_get_f64v, row_get_boolv, row_get_strv, &
            row_get_datev, row_get_timev, row_get_tsv
        procedure :: is_null => row_is_null !! Whether this row is null in a column.
        procedure :: index => row_index     !! This row's 1-based index within the table.
        final :: row_finalize               !! Drops the pointer; owns nothing, frees nothing.
    end type parquet_table_row
    !
    ! ---- Lifecycle (parquet_tables_lifecycle) ----
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
        !> Whether row `i` of a column is null (int32 row index).
        module function table_is_null_i32(self, name, i) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int32), intent(in) :: i          !! 1-based row index.
            logical :: isnull                        !! .true. if that row is null.
        end function table_is_null_i32
        !> Whether row `i` of a column is null (int64 row index).
        module function table_is_null_i64(self, name, i) result(isnull)
            class(parquet_table), intent(in) :: self !! the table.
            character(len=*), intent(in) :: name     !! column name.
            integer(int64), intent(in) :: i          !! 1-based row index.
            logical :: isnull                        !! .true. if that row is null.
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
        module subroutine table_resolve(self, name, proc, idx, found)
            class(parquet_table), intent(in) :: self  !! the table.
            character(len=*), intent(in) :: name      !! column name.
            character(len=*), intent(in) :: proc      !! calling procedure, for the message.
            integer, intent(out) :: idx               !! slot index, or 0 when `found` is present.
            logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
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
                n_remap, out_filter, out_sort, out_qc, n_qc)
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
        end subroutine compose_read_transform
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
        module subroutine table_materialize(cache, sc, idx)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            type(table_scope), intent(in) :: sc               !! rows this table covers.
            integer, intent(in) :: idx                        !! slot to fill.
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
        module subroutine table_release_one(cache, name)
            type(parquet_table_cache), intent(inout) :: cache !! the column store.
            character(len=*), intent(in) :: name              !! column path to release.
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
        !! A schema built with `%init`/`%add_field` and never parsed is parsed here, so calling
        !! `parquet_parse_maml` first is optional. That is why `schema` is `intent(inout)`: the
        !! caller's schema is parsed on return.
        module subroutine parquet_write_table(table, filename, schema, row_mask)
            type(parquet_table), intent(in) :: table   !! the table to write.
            character(len=*), intent(in) :: filename   !! output parquet file.
            type(parquet_schema), intent(inout) :: schema !! output schema (chooses/renames columns).
            logical, intent(in), optional :: row_mask(:)  !! per-row write mask.
        end subroutine parquet_write_table
    end interface
    !
    ! ---- Zero-copy pointer access (parquet_tables_access) ----
    interface
        !> Points `p` at a PK_INT32 column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_i32(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            integer(int32), pointer, intent(out) :: p(:)    !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_i32
        !> Points `p` at a PK_INT64 column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_i64(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            integer(int64), pointer, intent(out) :: p(:)    !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_i64
        !> Points `p` at a PK_FLOAT32 column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_f32(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            real(real32), pointer, intent(out) :: p(:)      !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_f32
        !> Points `p` at a PK_FLOAT64 column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_f64(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            real(real64), pointer, intent(out) :: p(:)      !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_f64
        !> Points `p` at a PK_LOGICAL column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_bool(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            logical, pointer, intent(out) :: p(:)           !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_bool
        !> Points `p` at a PK_DATE column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_date(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            type(parquet_date), pointer, intent(out) :: p(:) !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_date
        !> Points `p` at a PK_TIME column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_time(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            type(parquet_time), pointer, intent(out) :: p(:) !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_time
        !> Points `p` at a PK_TIMESTAMP column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_ts(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            type(parquet_timestamp), pointer, intent(out) :: p(:) !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_ts
        !> Points `p` at a PK_INT32_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_i32v(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            integer(int32), pointer, intent(out) :: p(:,:)  !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_i32v
        !> Points `p` at a PK_INT64_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_i64v(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            integer(int64), pointer, intent(out) :: p(:,:)  !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_i64v
        !> Points `p` at a PK_FLOAT32_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_f32v(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            real(real32), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_f32v
        !> Points `p` at a PK_FLOAT64_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_f64v(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            real(real64), pointer, intent(out) :: p(:,:)    !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_f64v
        !> Points `p` at a PK_LOGICAL_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_boolv(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            logical, pointer, intent(out) :: p(:,:)         !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_boolv
        !> Points `p` at a PK_DATE_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_datev(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            type(parquet_date), pointer, intent(out) :: p(:,:) !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_datev
        !> Points `p` at a PK_TIME_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_timev(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            type(parquet_time), pointer, intent(out) :: p(:,:) !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_timev
        !> Points `p` at a PK_TIMESTAMP_VEC column's storage. The stored kind must match EXACTLY.
        module subroutine col_ptr_tsv(self, name, p, found)
            class(parquet_table), intent(in), target :: self !! the table.
            character(len=*), intent(in) :: name             !! column name.
            type(parquet_timestamp), pointer, intent(out) :: p(:,:) !! alias to the live storage.
            logical, intent(out), optional :: found          !! present: report a miss instead of aborting.
        end subroutine col_ptr_tsv
    end interface
    !
    ! ---- Copy out (parquet_tables_access) ----
    interface
        !> Copies a PK_INT32 column out into a freshly allocated array.
        module subroutine get_arr_i32(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            integer(int32), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_i32
        !> Copies a PK_INT64 column out into a freshly allocated array.
        !! Also accepts a PK_INT32 column, widening on the way.
        module subroutine get_arr_i64(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            integer(int64), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_i64
        !> Copies a PK_FLOAT32 column out into a freshly allocated array.
        module subroutine get_arr_f32(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            real(real32), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_f32
        !> Copies a PK_FLOAT64 column out into a freshly allocated array.
        !! Also accepts a PK_FLOAT32 column, widening on the way.
        module subroutine get_arr_f64(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            real(real64), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_f64
        !> Copies a PK_LOGICAL column out into a freshly allocated array.
        module subroutine get_arr_bool(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            logical, allocatable, intent(out) :: arr(:)     !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_bool
        !> Copies a PK_DATE column out into a freshly allocated array.
        module subroutine get_arr_date(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_date), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_date
        !> Copies a PK_TIME column out into a freshly allocated array.
        module subroutine get_arr_time(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_time), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_time
        !> Copies a PK_TIMESTAMP column out into a freshly allocated array.
        module subroutine get_arr_ts(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_timestamp), allocatable, intent(out) :: arr(:) !! one value per row.
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_ts
        !> Copies a PK_INT32_VEC column out into a freshly allocated array.
        module subroutine get_arr_i32v(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            integer(int32), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_i32v
        !> Copies a PK_INT64_VEC column out into a freshly allocated array.
        !! Also accepts a PK_INT32_VEC column, widening on the way.
        module subroutine get_arr_i64v(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            integer(int64), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_i64v
        !> Copies a PK_FLOAT32_VEC column out into a freshly allocated array.
        module subroutine get_arr_f32v(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            real(real32), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_f32v
        !> Copies a PK_FLOAT64_VEC column out into a freshly allocated array.
        !! Also accepts a PK_FLOAT32_VEC column, widening on the way.
        module subroutine get_arr_f64v(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            real(real64), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_f64v
        !> Copies a PK_LOGICAL_VEC column out into a freshly allocated array.
        module subroutine get_arr_boolv(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            logical, allocatable, intent(out) :: arr(:,:)   !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_boolv
        !> Copies a PK_DATE_VEC column out into a freshly allocated array.
        module subroutine get_arr_datev(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_date), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_datev
        !> Copies a PK_TIME_VEC column out into a freshly allocated array.
        module subroutine get_arr_timev(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_time), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_timev
        !> Copies a PK_TIMESTAMP_VEC column out into a freshly allocated array.
        module subroutine get_arr_tsv(self, name, arr, found)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_timestamp), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(out), optional :: found              !! present: report a miss instead of aborting.
        end subroutine get_arr_tsv
        !> Copies a PK_STRING column out as a parquet_string_column (offsets+data+validity).
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
        end subroutine get_arr_chrv
    end interface
    !
    ! ---- Copy back (parquet_tables_access) ----
    interface
        !> Replaces every value of a PK_INT32 column. The array must have the column's own shape.
        module subroutine set_arr_i32(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            integer(int32), intent(in) :: arr(:)            !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_i32
        !> Replaces every value of a PK_INT64 column. The array must have the column's own shape.
        module subroutine set_arr_i64(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            integer(int64), intent(in) :: arr(:)            !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_i64
        !> Replaces every value of a PK_FLOAT32 column. The array must have the column's own shape.
        module subroutine set_arr_f32(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            real(real32), intent(in) :: arr(:)              !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_f32
        !> Replaces every value of a PK_FLOAT64 column. The array must have the column's own shape.
        module subroutine set_arr_f64(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            real(real64), intent(in) :: arr(:)              !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_f64
        !> Replaces every value of a PK_LOGICAL column. The array must have the column's own shape.
        module subroutine set_arr_bool(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            logical, intent(in) :: arr(:)                   !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_bool
        !> Replaces every value of a PK_DATE column. The array must have the column's own shape.
        module subroutine set_arr_date(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_date), intent(in) :: arr(:)        !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_date
        !> Replaces every value of a PK_TIME column. The array must have the column's own shape.
        module subroutine set_arr_time(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_time), intent(in) :: arr(:)        !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_time
        !> Replaces every value of a PK_TIMESTAMP column. The array must have the column's own shape.
        module subroutine set_arr_ts(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_timestamp), intent(in) :: arr(:)   !! one value per row.
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_ts
        !> Replaces every value of a PK_INT32_VEC column. The array must have the column's own shape.
        module subroutine set_arr_i32v(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            integer(int32), intent(in) :: arr(:,:)          !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_i32v
        !> Replaces every value of a PK_INT64_VEC column. The array must have the column's own shape.
        module subroutine set_arr_i64v(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            integer(int64), intent(in) :: arr(:,:)          !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_i64v
        !> Replaces every value of a PK_FLOAT32_VEC column. The array must have the column's own shape.
        module subroutine set_arr_f32v(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            real(real32), intent(in) :: arr(:,:)            !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_f32v
        !> Replaces every value of a PK_FLOAT64_VEC column. The array must have the column's own shape.
        module subroutine set_arr_f64v(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            real(real64), intent(in) :: arr(:,:)            !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_f64v
        !> Replaces every value of a PK_LOGICAL_VEC column. The array must have the column's own shape.
        module subroutine set_arr_boolv(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            logical, intent(in) :: arr(:,:)                 !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_boolv
        !> Replaces every value of a PK_DATE_VEC column. The array must have the column's own shape.
        module subroutine set_arr_datev(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_date), intent(in) :: arr(:,:)      !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_datev
        !> Replaces every value of a PK_TIME_VEC column. The array must have the column's own shape.
        module subroutine set_arr_timev(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_time), intent(in) :: arr(:,:)      !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_timev
        !> Replaces every value of a PK_TIMESTAMP_VEC column. The array must have the column's own shape.
        module subroutine set_arr_tsv(self, name, arr, modify_nulls)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_timestamp), intent(in) :: arr(:,:) !! (element, row), shaped (width, nrows).
            logical, intent(in), optional :: modify_nulls !! .false. leaves null rows untouched.
        end subroutine set_arr_tsv
        !> Replaces every value of a PK_STRING column from a character array.
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
        end subroutine set_arr_chrv
    end interface
    !
    ! ---- From-scratch construction (parquet_tables_addcol) ----
    interface
        !> Appends a new PK_INT32 column holding `values`.
        module subroutine add_column_i32(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            integer(int32), intent(in) :: values(:)         !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_i32
        !> Appends a new PK_INT64 column holding `values`.
        module subroutine add_column_i64(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            integer(int64), intent(in) :: values(:)         !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_i64
        !> Appends a new PK_FLOAT32 column holding `values`.
        module subroutine add_column_f32(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            real(real32), intent(in) :: values(:)           !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_f32
        !> Appends a new PK_FLOAT64 column holding `values`.
        module subroutine add_column_f64(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            real(real64), intent(in) :: values(:)           !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_f64
        !> Appends a new PK_LOGICAL column holding `values`.
        module subroutine add_column_bool(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            logical, intent(in) :: values(:)                !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_bool
        !> Appends a new PK_DATE column holding `values`.
        module subroutine add_column_date(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            type(parquet_date), intent(in) :: values(:)     !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_date
        !> Appends a new PK_TIME column holding `values`.
        module subroutine add_column_time(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            type(parquet_time), intent(in) :: values(:)     !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_time
        !> Appends a new PK_TIMESTAMP column holding `values`.
        module subroutine add_column_ts(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            type(parquet_timestamp), intent(in) :: values(:) !! one value per row.
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_ts
        !> Appends a new PK_INT32_VEC column holding `values`.
        module subroutine add_column_i32v(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            integer(int32), intent(in) :: values(:,:)       !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_i32v
        !> Appends a new PK_INT64_VEC column holding `values`.
        module subroutine add_column_i64v(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            integer(int64), intent(in) :: values(:,:)       !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_i64v
        !> Appends a new PK_FLOAT32_VEC column holding `values`.
        module subroutine add_column_f32v(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            real(real32), intent(in) :: values(:,:)         !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_f32v
        !> Appends a new PK_FLOAT64_VEC column holding `values`.
        module subroutine add_column_f64v(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            real(real64), intent(in) :: values(:,:)         !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_f64v
        !> Appends a new PK_LOGICAL_VEC column holding `values`.
        module subroutine add_column_boolv(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            logical, intent(in) :: values(:,:)              !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_boolv
        !> Appends a new PK_DATE_VEC column holding `values`.
        module subroutine add_column_datev(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            type(parquet_date), intent(in) :: values(:,:)   !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_datev
        !> Appends a new PK_TIME_VEC column holding `values`.
        module subroutine add_column_timev(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            type(parquet_time), intent(in) :: values(:,:)   !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_timev
        !> Appends a new PK_TIMESTAMP_VEC column holding `values`.
        module subroutine add_column_tsv(self, name, values, unit, force)
            class(parquet_table), intent(inout) :: self  !! the table.
            character(len=*), intent(in) :: name         !! the new column's name.
            type(parquet_timestamp), intent(in) :: values(:,:) !! (element, row), shaped (width, nrows).
            character(len=*), intent(in), optional :: unit !! unit string to store.
            logical, intent(in), optional :: force        !! .true. replaces an existing same-named column.
        end subroutine add_column_tsv
        !> Appends a new PK_STRING column holding `values` (trailing blanks are trimmed).
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
        end subroutine add_column_chrv
    end interface
    !
    ! ---- Single-cell mutation (the per-kind writers in ..._access, the rest in ..._mutate) ----
    interface
        !> Writes one row of a PK_INT32 column (i32 row index).
        module subroutine set_element_i32_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            integer(int32), intent(in) :: value             !! the new value.
        end subroutine set_element_i32_i32
        !> Writes one row of a PK_INT32 column (i64 row index).
        module subroutine set_element_i32_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            integer(int32), intent(in) :: value             !! the new value.
        end subroutine set_element_i32_i64
        !> Writes one row of a PK_INT64 column (i32 row index).
        module subroutine set_element_i64_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            integer(int64), intent(in) :: value             !! the new value.
        end subroutine set_element_i64_i32
        !> Writes one row of a PK_INT64 column (i64 row index).
        module subroutine set_element_i64_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            integer(int64), intent(in) :: value             !! the new value.
        end subroutine set_element_i64_i64
        !> Writes one row of a PK_FLOAT32 column (i32 row index).
        module subroutine set_element_f32_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            real(real32), intent(in) :: value               !! the new value.
        end subroutine set_element_f32_i32
        !> Writes one row of a PK_FLOAT32 column (i64 row index).
        module subroutine set_element_f32_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            real(real32), intent(in) :: value               !! the new value.
        end subroutine set_element_f32_i64
        !> Writes one row of a PK_FLOAT64 column (i32 row index).
        module subroutine set_element_f64_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            real(real64), intent(in) :: value               !! the new value.
        end subroutine set_element_f64_i32
        !> Writes one row of a PK_FLOAT64 column (i64 row index).
        module subroutine set_element_f64_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            real(real64), intent(in) :: value               !! the new value.
        end subroutine set_element_f64_i64
        !> Writes one row of a PK_LOGICAL column (i32 row index).
        module subroutine set_element_bool_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            logical, intent(in) :: value                    !! the new value.
        end subroutine set_element_bool_i32
        !> Writes one row of a PK_LOGICAL column (i64 row index).
        module subroutine set_element_bool_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            logical, intent(in) :: value                    !! the new value.
        end subroutine set_element_bool_i64
        !> Writes one row of a PK_DATE column (i32 row index).
        module subroutine set_element_date_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            type(parquet_date), intent(in) :: value         !! the new value.
        end subroutine set_element_date_i32
        !> Writes one row of a PK_DATE column (i64 row index).
        module subroutine set_element_date_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            type(parquet_date), intent(in) :: value         !! the new value.
        end subroutine set_element_date_i64
        !> Writes one row of a PK_TIME column (i32 row index).
        module subroutine set_element_time_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            type(parquet_time), intent(in) :: value         !! the new value.
        end subroutine set_element_time_i32
        !> Writes one row of a PK_TIME column (i64 row index).
        module subroutine set_element_time_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            type(parquet_time), intent(in) :: value         !! the new value.
        end subroutine set_element_time_i64
        !> Writes one row of a PK_TIMESTAMP column (i32 row index).
        module subroutine set_element_ts_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value    !! the new value.
        end subroutine set_element_ts_i32
        !> Writes one row of a PK_TIMESTAMP column (i64 row index).
        module subroutine set_element_ts_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value    !! the new value.
        end subroutine set_element_ts_i64
        !> Writes one row of a PK_INT32_VEC column (i32 row index).
        module subroutine set_element_i32v_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            integer(int32), intent(in) :: value(:)          !! that row's whole vector.
        end subroutine set_element_i32v_i32
        !> Writes one row of a PK_INT32_VEC column (i64 row index).
        module subroutine set_element_i32v_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            integer(int32), intent(in) :: value(:)          !! that row's whole vector.
        end subroutine set_element_i32v_i64
        !> Writes one row of a PK_INT64_VEC column (i32 row index).
        module subroutine set_element_i64v_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            integer(int64), intent(in) :: value(:)          !! that row's whole vector.
        end subroutine set_element_i64v_i32
        !> Writes one row of a PK_INT64_VEC column (i64 row index).
        module subroutine set_element_i64v_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            integer(int64), intent(in) :: value(:)          !! that row's whole vector.
        end subroutine set_element_i64v_i64
        !> Writes one row of a PK_FLOAT32_VEC column (i32 row index).
        module subroutine set_element_f32v_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            real(real32), intent(in) :: value(:)            !! that row's whole vector.
        end subroutine set_element_f32v_i32
        !> Writes one row of a PK_FLOAT32_VEC column (i64 row index).
        module subroutine set_element_f32v_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            real(real32), intent(in) :: value(:)            !! that row's whole vector.
        end subroutine set_element_f32v_i64
        !> Writes one row of a PK_FLOAT64_VEC column (i32 row index).
        module subroutine set_element_f64v_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            real(real64), intent(in) :: value(:)            !! that row's whole vector.
        end subroutine set_element_f64v_i32
        !> Writes one row of a PK_FLOAT64_VEC column (i64 row index).
        module subroutine set_element_f64v_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            real(real64), intent(in) :: value(:)            !! that row's whole vector.
        end subroutine set_element_f64v_i64
        !> Writes one row of a PK_LOGICAL_VEC column (i32 row index).
        module subroutine set_element_boolv_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            logical, intent(in) :: value(:)                 !! that row's whole vector.
        end subroutine set_element_boolv_i32
        !> Writes one row of a PK_LOGICAL_VEC column (i64 row index).
        module subroutine set_element_boolv_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            logical, intent(in) :: value(:)                 !! that row's whole vector.
        end subroutine set_element_boolv_i64
        !> Writes one row of a PK_DATE_VEC column (i32 row index).
        module subroutine set_element_datev_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            type(parquet_date), intent(in) :: value(:)      !! that row's whole vector.
        end subroutine set_element_datev_i32
        !> Writes one row of a PK_DATE_VEC column (i64 row index).
        module subroutine set_element_datev_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            type(parquet_date), intent(in) :: value(:)      !! that row's whole vector.
        end subroutine set_element_datev_i64
        !> Writes one row of a PK_TIME_VEC column (i32 row index).
        module subroutine set_element_timev_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            type(parquet_time), intent(in) :: value(:)      !! that row's whole vector.
        end subroutine set_element_timev_i32
        !> Writes one row of a PK_TIME_VEC column (i64 row index).
        module subroutine set_element_timev_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            type(parquet_time), intent(in) :: value(:)      !! that row's whole vector.
        end subroutine set_element_timev_i64
        !> Writes one row of a PK_TIMESTAMP_VEC column (i32 row index).
        module subroutine set_element_tsv_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value(:) !! that row's whole vector.
        end subroutine set_element_tsv_i32
        !> Writes one row of a PK_TIMESTAMP_VEC column (i64 row index).
        module subroutine set_element_tsv_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            type(parquet_timestamp), intent(in) :: value(:) !! that row's whole vector.
        end subroutine set_element_tsv_i64
        !> Writes one row of a PK_STRING column from a character value (i32 row index).
        module subroutine set_element_chr_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            character(len=*), intent(in) :: value           !! the new value.
        end subroutine set_element_chr_i32
        !> Writes one row of a PK_STRING column from a character value (i64 row index).
        module subroutine set_element_chr_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            character(len=*), intent(in) :: value           !! the new value.
        end subroutine set_element_chr_i64
        !> Writes one row of a PK_STRING_VEC column from a character value (i32 row index).
        module subroutine set_element_chrv_i32(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i                 !! 1-based row index.
            character(len=*), intent(in) :: value(:)        !! that row's whole vector.
        end subroutine set_element_chrv_i32
        !> Writes one row of a PK_STRING_VEC column from a character value (i64 row index).
        module subroutine set_element_chrv_i64(self, name, i, value)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i                 !! 1-based row index.
            character(len=*), intent(in) :: value(:)        !! that row's whole vector.
        end subroutine set_element_chrv_i64
        !> Marks row `i` of a column null (int32 row index).
        module subroutine set_null_i32(self, name, i)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i             !! 1-based row index.
        end subroutine set_null_i32
        !> Marks row `i` of a column null (int64 row index).
        module subroutine set_null_i64(self, name, i)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i             !! 1-based row index.
        end subroutine set_null_i64
        !> Marks row `i` of a column valid, leaving its value unspecified (int32 row index).
        module subroutine clear_null_i32(self, name, i)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int32), intent(in) :: i             !! 1-based row index.
        end subroutine clear_null_i32
        !> Marks row `i` of a column valid, leaving its value unspecified (int64 row index).
        module subroutine clear_null_i64(self, name, i)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
            integer(int64), intent(in) :: i             !! 1-based row index.
        end subroutine clear_null_i64
        !> Drops a column's null bitmap when it no longer holds any null, so a column that HAD
        !! nulls and no longer does stops paying for the bitmap. Scans the column, so it is not
        !! free -- a whole-column %set already compacts on its own and does not need this.
        !! Idempotent: calling it on an already-compact column is a cheap no-op.
        module subroutine table_compact_validity(self, name)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column name.
        end subroutine table_compact_validity
    end interface
    !
    ! ---- Column-structural mutation (parquet_tables_mutate) ----
    interface
        !> Removes a column from the table. Cheap, and it does NOT detach: dropping a column
        !! leaves every remaining column the same length, so the table can still read the ones it
        !! has not read yet. Dropping a column that was never read is the memory-reclaiming case
        !! and reads nothing.
        module subroutine table_drop_column(self, name, force)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! column to remove.
            logical, intent(in), optional :: force      !! .true. to drop a PREDEFINED column.
        end subroutine table_drop_column
        !> Changes a column's name. Only the name a caller looks it up by changes -- a
        !! file-backed column that has not been read yet still reads from the same physical
        !! column afterwards. A predefined column cannot be renamed at all (its accessor is bound
        !! to the name at compile time), and there is no `force=` for it.
        module subroutine table_rename_column(self, old_name, new_name)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: old_name    !! the column to rename.
            character(len=*), intent(in) :: new_name    !! its new name; must not already exist.
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
        module subroutine table_copy_column(self, name, new_name, to_kind, exact)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the source column.
            character(len=*), intent(in) :: new_name    !! the column to create.
            integer, intent(in), optional :: to_kind    !! target PK_* kind; absent keeps the source's.
            logical, intent(in), optional :: exact      !! .false. to allow lossy narrowing (default .true.).
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
        module subroutine table_cast(self, name, to_kind, exact)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the column to convert.
            integer, intent(in) :: to_kind              !! target PK_* kind.
            logical, intent(in), optional :: exact      !! .true. to refuse any precision loss.
        end subroutine table_cast
        !> Appends an already-built `parquet_column` as a new column. The kind-generic
        !! `%add_column` covers every case a user has; this is the internal path for code that
        !! already holds a column of the right shape and only needs it slotted in.
        module subroutine table_put_column(self, name, col)
            class(parquet_table), intent(inout) :: self !! the table.
            character(len=*), intent(in) :: name        !! the new column's name.
            type(parquet_column), intent(in) :: col     !! the column to copy in.
        end subroutine table_put_column
    end interface
    !
    ! ---- Row-structural mutation -- detaches whenever it changes the row set (parquet_tables_rowmutate) ----
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
        !! Every key column must already be resident: sorting will not read one implicitly.
        !! Row-structural, so it DETACHES -- unless the rows were already in that order, in which
        !! case nothing moves and nothing is detached.
        module subroutine table_sort_by(self, keys, descending, nulls_first)
            class(parquet_table), intent(inout) :: self       !! the table.
            character(len=*), intent(in) :: keys(:)           !! key columns, primary first.
            logical, intent(in), optional :: descending(:)    !! per key: .true. for descending.
            logical, intent(in), optional :: nulls_first(:)   !! per key: .true. to put nulls first.
        end subroutine table_sort_by
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
    end interface
    !
    ! ---- Copying a whole table (parquet_tables_clone) ----
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
        module subroutine table_clone_structure(self, out)
            class(parquet_table), intent(in) :: self  !! the table to take the shape of.
            class(parquet_table), intent(out) :: out  !! receives the empty table.
        end subroutine table_clone_structure
    end interface
    !
    ! ---- Sort key extraction (parquet_tables_sort) ----
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
    end interface
    !
    ! ---- Row selection (parquet_tables_slice, and the per-kind copies in ..._access) ----
    interface
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
    end interface
    !
    ! ---- Row view (parquet_tables_row, and the per-kind getters in ..._access) ----
    interface
        !> Builds a handle on row `i` (int32 index) -- see the %row generic.
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
        end subroutine row_kind_error
        !> This row's value from a PK_INT32 column.
        module subroutine row_get_i32(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer(int32), intent(out) :: value          !! receives the value.
        end subroutine row_get_i32
        !> This row's value from a PK_INT64 column.
        !! Also accepts a PK_INT32 column, widening on the way out.
        module subroutine row_get_i64(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer(int64), intent(out) :: value          !! receives the value.
        end subroutine row_get_i64
        !> This row's value from a PK_FLOAT32 column.
        module subroutine row_get_f32(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            real(real32), intent(out) :: value            !! receives the value.
        end subroutine row_get_f32
        !> This row's value from a PK_FLOAT64 column.
        !! Also accepts a PK_FLOAT32 column, widening on the way out.
        module subroutine row_get_f64(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            real(real64), intent(out) :: value            !! receives the value.
        end subroutine row_get_f64
        !> This row's value from a PK_LOGICAL column.
        module subroutine row_get_bool(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            logical, intent(out) :: value                 !! receives the value.
        end subroutine row_get_bool
        !> This row's string value from a PK_STRING column.
        module subroutine row_get_str(self, name, value)
            class(parquet_table_row), intent(in) :: self          !! the row handle.
            character(len=*), intent(in) :: name                  !! column name.
            character(len=:), allocatable, intent(out) :: value   !! receives the value.
        end subroutine row_get_str
        !> This row's value from a PK_DATE column.
        module subroutine row_get_date(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_date), intent(out) :: value      !! receives the value.
        end subroutine row_get_date
        !> This row's value from a PK_TIME column.
        module subroutine row_get_time(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_time), intent(out) :: value      !! receives the value.
        end subroutine row_get_time
        !> This row's value from a PK_TIMESTAMP column.
        module subroutine row_get_ts(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_timestamp), intent(out) :: value !! receives the value.
        end subroutine row_get_ts
        !> This row's vector from a PK_INT32_VEC column, one array element per position.
        module subroutine row_get_i32v(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer(int32), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_i32v
        !> This row's vector from a PK_INT64_VEC column, one array element per position.
        !! Also accepts a PK_INT32_VEC column, widening on the way out.
        module subroutine row_get_i64v(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            integer(int64), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_i64v
        !> This row's vector from a PK_FLOAT32_VEC column, one array element per position.
        module subroutine row_get_f32v(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            real(real32), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_f32v
        !> This row's vector from a PK_FLOAT64_VEC column, one array element per position.
        !! Also accepts a PK_FLOAT32_VEC column, widening on the way out.
        module subroutine row_get_f64v(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            real(real64), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_f64v
        !> This row's vector from a PK_LOGICAL_VEC column, one array element per position.
        module subroutine row_get_boolv(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            logical, allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_boolv
        !> This row's string vector from a PK_STRING_VEC column, one array element per position.
        module subroutine row_get_strv(self, name, value)
            class(parquet_table_row), intent(in) :: self             !! the row handle.
            character(len=*), intent(in) :: name                     !! column name.
            character(len=:), allocatable, intent(out) :: value(:)   !! receives width values.
        end subroutine row_get_strv
        !> This row's vector from a PK_DATE_VEC column, one array element per position.
        module subroutine row_get_datev(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_date), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_datev
        !> This row's vector from a PK_TIME_VEC column, one array element per position.
        module subroutine row_get_timev(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_time), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_timev
        !> This row's vector from a PK_TIMESTAMP_VEC column, one array element per position.
        module subroutine row_get_tsv(self, name, value)
            class(parquet_table_row), intent(in) :: self !! the row handle.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_timestamp), allocatable, intent(out) :: value(:) !! receives width values.
        end subroutine row_get_tsv
        !> Copies the rows `s` selects from a PK_INT32 column into `arr`.
        module subroutine get_slice_i32(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            integer(int32), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_i32
        !> Copies the rows `s` selects from a PK_INT64 column into `arr`.
        !! Also accepts a PK_INT32 column, widening on the way out.
        module subroutine get_slice_i64(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            integer(int64), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_i64
        !> Copies the rows `s` selects from a PK_FLOAT32 column into `arr`.
        module subroutine get_slice_f32(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            real(real32), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_f32
        !> Copies the rows `s` selects from a PK_FLOAT64 column into `arr`.
        !! Also accepts a PK_FLOAT32 column, widening on the way out.
        module subroutine get_slice_f64(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            real(real64), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_f64
        !> Copies the rows `s` selects from a PK_LOGICAL column into `arr`.
        module subroutine get_slice_bool(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            logical, allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_bool
        !> Copies the rows `s` selects from a PK_DATE column into `arr`.
        module subroutine get_slice_date(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            type(parquet_date), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_date
        !> Copies the rows `s` selects from a PK_TIME column into `arr`.
        module subroutine get_slice_time(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            type(parquet_time), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_time
        !> Copies the rows `s` selects from a PK_TIMESTAMP column into `arr`.
        module subroutine get_slice_ts(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            type(parquet_timestamp), allocatable, intent(out) :: arr(:) !! one value per row.
        end subroutine get_slice_ts
        !> Copies the rows `s` selects from a PK_INT32_VEC column into `arr`.
        module subroutine get_slice_i32v(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            integer(int32), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_i32v
        !> Copies the rows `s` selects from a PK_INT64_VEC column into `arr`.
        !! Also accepts a PK_INT32_VEC column, widening on the way out.
        module subroutine get_slice_i64v(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            integer(int64), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_i64v
        !> Copies the rows `s` selects from a PK_FLOAT32_VEC column into `arr`.
        module subroutine get_slice_f32v(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            real(real32), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_f32v
        !> Copies the rows `s` selects from a PK_FLOAT64_VEC column into `arr`.
        !! Also accepts a PK_FLOAT32_VEC column, widening on the way out.
        module subroutine get_slice_f64v(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            real(real64), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_f64v
        !> Copies the rows `s` selects from a PK_LOGICAL_VEC column into `arr`.
        module subroutine get_slice_boolv(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            logical, allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_boolv
        !> Copies the rows `s` selects from a PK_DATE_VEC column into `arr`.
        module subroutine get_slice_datev(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            type(parquet_date), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_datev
        !> Copies the rows `s` selects from a PK_TIME_VEC column into `arr`.
        module subroutine get_slice_timev(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            type(parquet_time), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_timev
        !> Copies the rows `s` selects from a PK_TIMESTAMP_VEC column into `arr`.
        module subroutine get_slice_tsv(self, name, s, arr)
            class(parquet_table), intent(in) :: self     !! the table.
            character(len=*), intent(in) :: name         !! column name.
            type(parquet_slice), intent(in) :: s         !! rows to pick.
            type(parquet_timestamp), allocatable, intent(out) :: arr(:,:) !! (element, row), shaped (width, nrows).
        end subroutine get_slice_tsv
        !> Copies the rows `s` selects from a PK_STRING column into a compact string column.
        module subroutine get_slice_str(self, name, s, arr)
            class(parquet_table), intent(in) :: self             !! the table.
            character(len=*), intent(in) :: name                 !! column name.
            type(parquet_slice), intent(in) :: s                 !! rows to pick.
            type(parquet_string_column), intent(out) :: arr      !! the selected elements.
        end subroutine get_slice_str
        !> Copies the rows `s` selects from a PK_STRING column into a character array, sized to
        !! the longest element selected.
        module subroutine get_slice_chr(self, name, s, arr)
            class(parquet_table), intent(in) :: self                 !! the table.
            character(len=*), intent(in) :: name                     !! column name.
            type(parquet_slice), intent(in) :: s                     !! rows to pick.
            character(len=:), allocatable, intent(out) :: arr(:)     !! one value per selected row.
        end subroutine get_slice_chr
        !> Copies the rows `s` selects from a PK_STRING_VEC column, shaped (width, selected).
        module subroutine get_slice_chrv(self, name, s, arr)
            class(parquet_table), intent(in) :: self                 !! the table.
            character(len=*), intent(in) :: name                     !! column name.
            type(parquet_slice), intent(in) :: s                     !! rows to pick.
            character(len=:), allocatable, intent(out) :: arr(:,:)   !! (element, selected row).
        end subroutine get_slice_chrv
    end interface
    !
    ! ---- Per-kind materialization (parquet_tables_materialize) ----
    interface
        !> Reads a PK_INT32 file column into `col`, carrying its nulls across.
        module subroutine mat_i32(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_i32
        !> Reads a PK_INT64 file column into `col`, carrying its nulls across.
        module subroutine mat_i64(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_i64
        !> Reads a PK_FLOAT32 file column into `col`, carrying its nulls across.
        module subroutine mat_f32(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_f32
        !> Reads a PK_FLOAT64 file column into `col`, carrying its nulls across.
        module subroutine mat_f64(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_f64
        !> Reads a PK_LOGICAL file column into `col`, carrying its nulls across.
        module subroutine mat_bool(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_bool
        !> Reads a PK_STRING file column into `col`, carrying its nulls across.
        module subroutine mat_str(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_str
        !> Reads a PK_DATE file column into `col`, carrying its nulls across.
        module subroutine mat_date(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_date
        !> Reads a PK_TIME file column into `col`, carrying its nulls across.
        module subroutine mat_time(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_time
        !> Reads a PK_TIMESTAMP file column into `col`, carrying its nulls across.
        module subroutine mat_ts(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_ts
        !> Reads a PK_INT32_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_i32v(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_i32v
        !> Reads a PK_INT64_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_i64v(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_i64v
        !> Reads a PK_FLOAT32_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_f32v(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_f32v
        !> Reads a PK_FLOAT64_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_f64v(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_f64v
        !> Reads a PK_LOGICAL_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_boolv(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_boolv
        !> Reads a PK_STRING_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_strv(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_strv
        !> Reads a PK_DATE_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_datev(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_datev
        !> Reads a PK_TIME_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_timev(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_timev
        !> Reads a PK_TIMESTAMP_VEC file column into `col`, carrying its nulls across.
        module subroutine mat_tsv(reader, name, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! rows to read.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine mat_tsv
        !> Reads ONE ROW GROUP of a PK_INT32 file column into `col`, carrying its nulls across.
        module subroutine matchunk_i32(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_i32
        !> Reads ONE ROW GROUP of a PK_INT64 file column into `col`, carrying its nulls across.
        module subroutine matchunk_i64(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_i64
        !> Reads ONE ROW GROUP of a PK_FLOAT32 file column into `col`, carrying its nulls across.
        module subroutine matchunk_f32(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_f32
        !> Reads ONE ROW GROUP of a PK_FLOAT64 file column into `col`, carrying its nulls across.
        module subroutine matchunk_f64(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_f64
        !> Reads ONE ROW GROUP of a PK_LOGICAL file column into `col`, carrying its nulls across.
        module subroutine matchunk_bool(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_bool
        !> Reads ONE ROW GROUP of a PK_STRING file column into `col`, carrying its nulls across.
        module subroutine matchunk_str(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_str
        !> Reads ONE ROW GROUP of a PK_DATE file column into `col`, carrying its nulls across.
        module subroutine matchunk_date(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_date
        !> Reads ONE ROW GROUP of a PK_TIME file column into `col`, carrying its nulls across.
        module subroutine matchunk_time(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_time
        !> Reads ONE ROW GROUP of a PK_TIMESTAMP file column into `col`, carrying its nulls across.
        module subroutine matchunk_ts(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_ts
        !> Reads ONE ROW GROUP of a PK_INT32_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_i32v(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_i32v
        !> Reads ONE ROW GROUP of a PK_INT64_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_i64v(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_i64v
        !> Reads ONE ROW GROUP of a PK_FLOAT32_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_f32v(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_f32v
        !> Reads ONE ROW GROUP of a PK_FLOAT64_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_f64v(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_f64v
        !> Reads ONE ROW GROUP of a PK_LOGICAL_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_boolv(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_boolv
        !> Reads ONE ROW GROUP of a PK_STRING_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_strv(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_strv
        !> Reads ONE ROW GROUP of a PK_DATE_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_datev(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_datev
        !> Reads ONE ROW GROUP of a PK_TIME_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_timev(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_timev
        !> Reads ONE ROW GROUP of a PK_TIMESTAMP_VEC file column into `col`, carrying its nulls across.
        module subroutine matchunk_tsv(reader, name, rg, col, nrows, wdt, unit)
            type(parquet_reader), intent(in) :: reader   !! open reader.
            character(len=*), intent(in) :: name         !! file column name.
            integer(int64), intent(in) :: rg             !! 1-based row group.
            type(parquet_column), intent(inout) :: col   !! value store to fill.
            integer(int64), intent(in) :: nrows          !! that row group's own row count.
            integer(int32), intent(in) :: wdt            !! values per row.
            character(len=*), intent(in) :: unit         !! unit string to store ("" for none).
        end subroutine matchunk_tsv
        !> Dispatches one file column's read to the specific matching `kind`.
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
        end subroutine table_materialize_chunk_kind
    end interface
    !
end module parquet_tables
