!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Public API for reading and writing Apache Parquet files: schema
!> construction/validation from MAML, writers/readers, row filtering,
!> quality-control (qc:) enforcement, and flat key-value table metadata.
!> Submodules parquet_write.f90/parquet_read.f90/parquet_metadata.f90 hold the
!> bodies of the module procedures declared in the interface block below.
module parquet
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64, real32, real64
    use parquet_bindings
    use parquet_maml_base, only: parquet_maml_file, parquet_maml_missing_column, parquet_maml_col_map_entry
    implicit none
    private
    !
    character(len=*),parameter:: cversion = "v0.9.7 (2026-07-14)" !! version info
#ifndef RELEASE_VERSION
#  define RELEASE_VERSION 0.1
#endif

    !> Valid values for a field's data_type in a MAML file, checked by
    !> parquet_validate_maml (parquet_metadata_validate.f90) and schema%add_field
    !> (parquet_metadata.f90) alike -- add new supported types here as needed.
    !> Declared here (rather than in either submodule) so both can see it.
    character(len=7), parameter :: valid_maml_data_types(6) = [character(len=7) :: &
        "int32", "int64", "string", "boolean", "float32", "float64"]

    !> One schema field's write-time metadata and QC bounds, parsed from a
    !> MAML fields: entry (or built via schema%add_field); one array element
    !> per field, held in parquet_column_info%col(:).
    type parquet_column_type
        logical :: is_set = .false. !! true if this column is currently enabled to be written; toggled by
        !! set_column_available/set_column_unavailable.
        logical :: is_deactivated = .false. !! true for columns merged in from a base MAML that the user's MAML
        !! excluded; protects is_set from being changed by set_column_available/set_column_unavailable (bulk or
        !! by name).
        logical :: is_protected = .false. !! true if this column's name is listed under extra: protected_cols: in
        !! whichever MAML built this cinfo; parquet_write_column error stops if an is_valid mask with any
        !! .false. entry is passed for such a column.
        logical :: has_qc_min = .false. !! true if a qc: min: bound was declared for this field.
        logical :: has_qc_max = .false. !! true if a qc: max: bound was declared for this field.
        character(len=2) :: qc_min_op = ">=" !! one of ">", ">="; default when qc: min: has no operator prefix.
        character(len=2) :: qc_max_op = "<=" !! one of "<", "<="; default when qc: max: has no operator prefix.
        character(len=:), allocatable :: qc_min_raw !! The qc: min: bound text, operator prefix already stripped/
        !! trimmed; numeric for int32/int64/float32/float64, literal for string.
        character(len=:), allocatable :: qc_max_raw !! qc: max: bound text, same convention as qc_min_raw.
        character(len=:), allocatable :: name      !! The name of the field [required]; always the internal/
        !! canonical name, i.e. what parquet_write_column/set_column_available/etc. use -- never affected by a
        !! col_map: rename (see output_name).
        character(len=:), allocatable :: unit      !! The unit of measurement for the field.
        character(len=:), allocatable :: info      !! A short description of the field.
        character(len=:), allocatable :: ucd       !! Unified Content Descriptor for IVOA (can have many).
        character(len=:), allocatable :: data_type !! The data type of the field [required].
        integer :: array_size = 1 !! Maximum length of character strings.
        integer :: col_size = 1   !! The number of elements in the vector column.
        character(len=:), allocatable :: output_name !! The name actually written to the parquet file/VOTable
        !! header. Equal to `name` unless a col_map: entry in the MAML that declared this field renamed it
        !! (col_map: maps internal_name -> output_name; `name` is then set to the internal_name and
        !! `output_name` keeps the field's own declared name from that MAML's fields: section).
    end type parquet_column_type

    !> Column-level schema state for one MAML: an array of parquet_column_type,
    !> one per declared field, plus lookups/toggles over it. Embedded in
    !> parquet_schema%cinfo; parquet_schema's own type-bound procedures are
    !> flat passthroughs to the ones here.
    type parquet_column_info
        type(parquet_column_type), allocatable :: col(:) !! One entry per declared field, in MAML source order.
    contains
        procedure :: get_column_index !! 1-based index of a column by name, or 0 if not found.
        procedure :: get_num_fields !! Total number of declared fields.
        procedure :: get_field_name !! Field name at a given 1-based MAML source position.
        ! Exposed as set_column_available/set_column_unavailable to match the
        ! same-named methods on parquet_schema (the primary public API); the
        ! backing module procedures keep the shorter set_available/set_unavailable
        ! names only to avoid colliding with parquet_schema's own impls.
        procedure :: set_column_unavailable => set_unavailable !! Disables a column, or every column if no name is given.
        procedure :: set_column_available => set_available !! Enables a column, or every column if no name is given.
    end type parquet_column_info

    !> One flat key-value table-metadata entry (a keyarray: item on the write
    !> side, or one row of parquet_reader%metadata on the read side).
    type parquet_metadata_entry
        character(len=:), allocatable :: key !! Metadata key.
        character(len=:), allocatable :: value !! Value, always stored as plain text (see add_metadata).
        character(len=:), allocatable :: description !! Optional free-text description; never populated on read.
    end type parquet_metadata_entry

    !> Flat key-value table metadata: one array of parquet_metadata_entry plus
    !> the add_metadata family of type-bound procedures that append to it (one
    !> specific per supported type/kind, dispatched via the add_metadata generic).
    type parquet_table_metadata
        type(parquet_metadata_entry), allocatable :: items(:) !! One entry per add_metadata call (or MAML keyarray: item).
        !> Verbatim source MAML lines, populated by parquet_read_maml; used by
        !> parquet_open_writer(..., write_maml=.true.) to save a sidecar .maml
        !> file next to the .parquet output. add_metadata calls made after
        !> parquet_read_maml append a new keyarray: entry to these lines (see
        !> parquet_append_keyarray_line), so the sidecar reflects them; it is
        !> NOT kept in sync with which columns end up enabled/written, though.
        character(len=:), allocatable :: source_maml_lines(:)
    contains
        procedure :: add_metadata_int32 !! int32 specific.
        procedure :: add_metadata_int64 !! int64 specific.
        procedure :: add_metadata_float32 !! float32 specific.
        procedure :: add_metadata_float64 !! float64 specific.
        procedure :: add_metadata_logical !! logical specific.
        procedure :: add_metadata_string !! string specific.
        procedure :: add_metadata_int32_array !! int32 array specific.
        procedure :: add_metadata_int64_array !! int64 array specific.
        procedure :: add_metadata_float32_array !! float32 array specific.
        procedure :: add_metadata_float64_array !! float64 array specific.
        procedure :: add_metadata_logical_array !! logical array specific.
        procedure :: add_metadata_string_array !! string array specific.
        !> Appends one flat key-value metadata entry; dispatched by value's type/kind/rank.
        generic :: add_metadata => add_metadata_int32, add_metadata_int64, add_metadata_float32, &
                                    add_metadata_float64, add_metadata_logical, add_metadata_string, &
                                    add_metadata_int32_array, add_metadata_int64_array, add_metadata_float32_array, &
                                    add_metadata_float64_array, add_metadata_logical_array, add_metadata_string_array
    end type parquet_table_metadata

    !> Bundles a parsed MAML source together with the column schema (cinfo)
    !> and table metadata (metadata) that parquet_parse_maml derives from it,
    !> so a single variable carries everything parquet_open_writer needs.
    !> %cinfo and %metadata are public: their fields (col(:), items(:)) and
    !> own type-bound procedures stay directly reachable, and the procedures
    !> below are flat convenience passthroughs (schema%set_column_available("id")
    !> instead of schema%cinfo%set_column_available("id")).
    !>
    !> On the read side a qc-maml is populated into %maml alone -- either by
    !> parquet_load_qc_maml_file or schema%add_col_qc -- and is never run
    !> through parquet_parse_maml (a qc-maml has no data_type and would fail
    !> the full schema validation), so %cinfo/%metadata stay empty;
    !> parquet_open_reader consumes only %maml.
    type parquet_schema
        type(parquet_maml_file)      :: maml     !! Raw MAML source plus (once parsed) missing-columns/col_map.
        type(parquet_column_info)    :: cinfo    !! Per-field schema/QC state, derived from %maml by parquet_parse_maml.
        type(parquet_table_metadata) :: metadata !! Flat key-value table metadata, derived from %maml's keyarray:.
        !> Set by %init: guards %add_field, which otherwise has no "fields:"
        !> header (or even a %maml at all) to append to. Only %init and
        !> %add_field are meaningful for a schema being built from scratch in
        !> memory -- a schema populated via parquet_parse_maml (from a real
        !> MAML file/object) never calls %init and never needs %add_field.
        logical, private :: is_initialized = .false.
    contains
        procedure :: init => schema_init !! Initializes a from-scratch schema (table: key + optional metadata).
        procedure :: add_field => schema_add_field !! Appends one fields: entry to a from-scratch schema.
        procedure :: set_column_available !! Enables a column, or every column if no name is given.
        procedure :: set_column_unavailable !! Disables a column, or every column if no name is given.
        procedure :: get_column_index => schema_get_column_index !! 1-based index of a column by name, or 0 if not found.
        procedure :: get_num_fields => schema_get_num_fields !! Total number of declared fields.
        procedure :: get_field_name => schema_get_field_name !! Field name at a given 1-based MAML source position.
        ! add_col_qc/get_col_qc build a read-time qc-maml. Two intentional
        ! naming choices here: (1) "col_qc" is a deliberate domain abbreviation
        ! for "column quality-control" (the qc: block of a fields: entry) --
        ! kept short because it appears in every qc-building call. (2) get_col_qc
        ! is deliberately named get_ even though it MUTATES the schema (it
        ! appends the entry, like add_col_qc): the get_ form exists only so the
        ! parsed column name can be assigned back in place (col = schema%get_col_qc(col)),
        ! which the subroutine form cannot do without aliasing an intent(out)
        ! argument. It is a builder that also returns the name, not a pure query.
        procedure :: add_col_qc => schema_add_col_qc !! Appends one qc: field entry from a compact string.
        procedure :: get_col_qc => schema_get_col_qc !! Function form of %add_col_qc; returns the parsed name.
        procedure :: schema_add_metadata_int32 !! int32 specific.
        procedure :: schema_add_metadata_int64 !! int64 specific.
        procedure :: schema_add_metadata_float32 !! float32 specific.
        procedure :: schema_add_metadata_float64 !! float64 specific.
        procedure :: schema_add_metadata_logical !! logical specific.
        procedure :: schema_add_metadata_string !! string specific.
        procedure :: schema_add_metadata_int32_array !! int32 array specific.
        procedure :: schema_add_metadata_int64_array !! int64 array specific.
        procedure :: schema_add_metadata_float32_array !! float32 array specific.
        procedure :: schema_add_metadata_float64_array !! float64 array specific.
        procedure :: schema_add_metadata_logical_array !! logical array specific.
        procedure :: schema_add_metadata_string_array !! string array specific.
        !> Appends one flat key-value metadata entry; dispatched by value's type/kind/rank.
        generic :: add_metadata => schema_add_metadata_int32, schema_add_metadata_int64, &
                                    schema_add_metadata_float32, schema_add_metadata_float64, &
                                    schema_add_metadata_logical, schema_add_metadata_string, &
                                    schema_add_metadata_int32_array, schema_add_metadata_int64_array, &
                                    schema_add_metadata_float32_array, schema_add_metadata_float64_array, &
                                    schema_add_metadata_logical_array, schema_add_metadata_string_array
    end type parquet_schema

    !> Overrides the default structure constructor so a schema can be built
    !> in one expression (my_maml = parquet_schema(table="my_table")) as an
    !> alternative to call my_maml%init(table="my_table"); both call
    !> parquet_schema_new/schema_init under the hood.
    interface parquet_schema
        module procedure parquet_schema_new
    end interface parquet_schema

    !> parquet_writer/parquet_reader own a handle to a C++-side Arrow/Parquet
    !> object with no automatic Fortran cleanup. Always prefer an explicit
    !> parquet_close_writer/parquet_close_reader call; the FINAL procedures
    !> below are only a safety net for a handle that's still open when its
    !> variable goes out of scope or is overwritten (e.g. an early RETURN
    !> between open and close), not a substitute for closing normally.
    !>
    !> Do not copy a parquet_writer/parquet_reader (`w2 = w1`, passing one as
    !> a function result, etc.): the handle is a plain c_ptr, so a copy
    !> aliases the same underlying C++ object without any reference counting.
    !> Whichever copy is finalized/closed first frees it out from under the
    !> other, which would then double-free/use-after-free when it is itself
    !> later closed or finalized. Always use a single named writer/reader,
    !> passed by reference (as every procedure in this module already does).
    !> Fully opaque from outside this module: every component is an
    !> implementation detail (the raw C handle, schema bookkeeping, write
    !> tracking) that parquet_write_column/parquet_open_writer/etc. manage
    !> internally. Never referenced directly by any test or consumer -- only
    !> parquet.f90's own submodules (parquet_write, parquet_read,
    !> parquet_metadata) need access, which `private` here still allows.
    type parquet_writer
        private
        type(c_ptr) :: handle = c_null_ptr !! Opaque C++ Arrow/Parquet writer handle; c_null_ptr until opened.
        type(parquet_column_type), allocatable :: all_columns(:) !! Every column the schema declares, enabled or not.
        type(parquet_column_type), allocatable :: enabled_columns(:) !! Subset of all_columns currently enabled (is_set).
        integer, allocatable :: write_counts(:) !! Per-enabled-column count of parquet_write_column calls so far.
        logical :: is_schema_enforced = .false. !! true when opened with a schema (vs. a schema-less writer).
        logical :: qc = .false. !! set from parquet_open_writer(..., qc=); when true, parquet_write_column checks
        !! each column's qc: min/max (if declared) against its valid (is_valid) elements
        !! and prints a WARNING (never an error) on violation. No-op without a schema.
        integer(c_long_long) :: expected_nrows = -1 !! set by the first parquet_write_column call; every later
        !! call must supply this same row count (see parquet_check_row_count), since
        !! Arrow/Parquet requires every column in a table to have equal length.
        character(len=256), allocatable :: written_names(:) !! Schema-less writer only (no cinfo, so
        !! enabled_columns/write_counts below don't exist): every name
        !! parquet_write_column has already written, so a repeat can still be
        !! caught -- see parquet_mark_column_written.
        character(len=:), allocatable :: maml_name !! Set from schema%maml%name by parquet_open_writer
        !! when a schema is given (unallocated for a schema-less writer, or if
        !! the schema's own %maml%name was never set); solely so
        !! parquet_write_column's "column not defined"/"type mismatch" errors
        !! can name which maml the schema came from -- see writer_maml_suffix.
        !> Set by parquet_open_writer from its own `filename` argument, solely
        !> so parquet_close_writer's missing-write error can name the output
        !> file; not used for anything else.
        character(len=:), allocatable :: filename
    contains
        final :: writer_finalize !! Safety-net close if the writer is still open when it goes out of scope.
    end type parquet_writer

    !> Opaque handle for an open parquet file being read; see parquet_writer's
    !> doc comment above for the shared handle-ownership/no-copy rules.
    type parquet_reader
        private
        type(c_ptr) :: handle = c_null_ptr !! Opaque C++ Arrow/Parquet reader handle; c_null_ptr until opened.
        !> Set by parquet_open_reader from its own `filename` argument, solely
        !> so parquet_get_nrows(..., check_positive=.true.) can name the file
        !> in its error message; not used for anything else.
        character(len=:), allocatable :: filename
        !> Flat key-value table metadata (whatever add_metadata wrote on the
        !> write side), copied once by parquet_open_reader from the underlying
        !> Arrow schema's KeyValueMetadata -- see parquet_reader_get_table_metadata_count
        !> and friends in parquet_bindings.f90. Every parquet_get_metadata call
        !> only scans this in-memory copy; it never re-reads the file. %description
        !> is never populated on this side (Arrow's flat KV store has no
        !> description field), only %key/%value.
        type(parquet_table_metadata) :: metadata
    contains
        final :: reader_finalize !! Safety-net close if the reader is still open when it goes out of scope.
    end type parquet_reader

    !> A row filter for parquet_open_reader: each %add call is one AND-combined
    !> clause, "<column> <op> [value]" (e.g. "ra > 180", "id is_not_null"),
    !> validated (column exists, is a scalar column, value is well-formed for
    !> that column's type) once parquet_open_reader actually applies it --
    !> %add itself just accumulates the raw rule text. See "Row filtering with
    !> parquet_filter" in the README for the full rule syntax.
    type parquet_filter
        character(len=512), allocatable :: rules(:) !! Raw, unvalidated "<column> <op> [value]" rule text, one per %add call.
        integer :: n = 0 !! Number of rules actually in use (rules(:) may be over-allocated).
    contains
        procedure :: add => parquet_filter_add !! Appends one AND-combined "<column> <op> [value]" rule clause.
    end type parquet_filter

    !> Internal plumbing only (not part of the public API): one column's
    !> read-time QC declaration, parsed from a qc-maml's fields: entries by
    !> parquet_parse_qc_maml. min_text/max_text carry the qc: min:/max: value
    !> verbatim (operator prefix, if any, already stripped and captured in
    !> min_op/max_op by parquet_set_qc_bound) -- parsed against the actual
    !> Arrow column type only when a check runs, in parquet_wrapper.cpp's
    !> run_qc_range_check, never against a maml-declared data_type (this
    !> maml doesn't even require one). See "Row filtering" for the analogous
    !> convention parquet_filter already uses for its own rule values.
    type :: parquet_qc_rule
        character(len=64) :: name = "" !! Column this rule applies to.
        !> True only if this field had a qc: sub-block at all (even an empty
        !> one) -- a field with just a name: and no qc: gets no rule at all,
        !> the same as a field never mentioned in the maml (see
        !> parquet_parse_qc_maml); parquet_apply_qc filters these out before
        !> ever reaching parquet_reader_set_qc.
        logical :: has_qc_block = .false.
        logical :: has_min = .false. !! true if a qc: min: bound was declared.
        character(len=2) :: min_op = ">=" !! one of ">", ">=".
        character(len=256) :: min_text = "" !! qc: min: value, verbatim, operator prefix stripped.
        logical :: has_max = .false. !! true if a qc: max: bound was declared.
        character(len=2) :: max_op = "<=" !! one of "<", "<=".
        character(len=256) :: max_text = "" !! qc: max: value, verbatim, operator prefix stripped.
        logical :: null_values_allowed = .false. !! true only if this field's qc: miss: was Null/NA (case-insensitive).
    end type parquet_qc_rule

    !> Writes one column's values to an open parquet_writer, dispatched by
    !> the actual/declared type/kind of `values` (scalar or matrix/vector
    !> column). is_valid (optional) marks per-element nulls; a .false. entry
    !> for a protected column (see parquet_column_type%is_protected) error stops.
    interface parquet_write_column
        module procedure parquet_write_int32_column
        module procedure parquet_write_int32_matrix_column
        module procedure parquet_write_int64_column
        module procedure parquet_write_int64_matrix_column
        module procedure parquet_write_float32_column
        module procedure parquet_write_float32_matrix_column
        module procedure parquet_write_float64_column
        module procedure parquet_write_float64_matrix_column
        module procedure parquet_write_logical_column
        module procedure parquet_write_logical_matrix_column
        module procedure parquet_write_string_column
        module procedure parquet_write_string_matrix_column
    end interface parquet_write_column

    !> Parses a MAML into a parquet_schema (its %maml, %cinfo and %metadata).
    !> The file form loads the .maml from disk first; the object form parses
    !> a schema whose %maml has already been populated (e.g. built in memory).
    interface parquet_parse_maml
        module procedure parquet_parse_maml_from_file
        module procedure parquet_parse_maml_from_object
    end interface parquet_parse_maml

    !> Validates either a parquet_maml_file (already loaded, e.g. via
    !> parquet_load_maml_file or built in memory) or a MAML filename (loaded
    !> from disk first). See parquet_validate_maml_internal/_file for behavior.
    interface parquet_validate_maml
        module procedure parquet_validate_maml_internal
        module procedure parquet_validate_maml_file
    end interface parquet_validate_maml

    !> Reads one column's values from an open parquet_reader into `values`,
    !> dispatched by its actual/declared type/kind and rank (scalar 1-D
    !> column_1d specifics, or a full 2-D array_full specific for a vector
    !> column). null_value (optional) fills missing entries; is_valid
    !> (optional) reports which elements were actually present.
    interface parquet_read_column
        module procedure parquet_read_int32_column_1d
        module procedure parquet_read_int64_column_1d
        module procedure parquet_read_float32_column_1d
        module procedure parquet_read_float64_column_1d
        module procedure parquet_read_logical_column_1d
        module procedure parquet_read_string_column_1d
        module procedure parquet_read_int32_array_full
        module procedure parquet_read_int64_array_full
        module procedure parquet_read_float32_array_full
        module procedure parquet_read_float64_array_full
        module procedure parquet_read_logical_array_full
        module procedure parquet_read_string_array_full
    end interface parquet_read_column

    !> Reads one row of a vector (array) column: `values` receives that row's
    !> full element vector, selected by the 1-based `row_index`. Dispatched by
    !> `values`' actual/declared type/kind. See parquet_read_array_element_mode
    !> for the complementary "one element across all rows" access pattern.
    interface parquet_read_array_row_mode
        module procedure parquet_read_int32_array_row_mode
        module procedure parquet_read_int64_array_row_mode
        module procedure parquet_read_float32_array_row_mode
        module procedure parquet_read_float64_array_row_mode
        module procedure parquet_read_logical_array_row_mode
        module procedure parquet_read_string_array_row_mode
    end interface parquet_read_array_row_mode

    !> Reads one element position of a vector (array) column across all rows:
    !> `values` receives that element (selected by the 1-based `elem_index`)
    !> from every row. Dispatched by `values`' actual/declared type/kind. See
    !> parquet_read_array_row_mode for the complementary "one row" access pattern.
    interface parquet_read_array_element_mode
        module procedure parquet_read_int32_array_element_mode
        module procedure parquet_read_int64_array_element_mode
        module procedure parquet_read_float32_array_element_mode
        module procedure parquet_read_float64_array_element_mode
        module procedure parquet_read_logical_array_element_mode
        module procedure parquet_read_string_array_element_mode
    end interface parquet_read_array_element_mode

    !> Returns the open reader's post-filter row count in `nrows`, dispatched
    !> by its integer(int32)/integer(int64) kind. check_positive (optional,
    !> default .false.): if .true., error stops instead of returning 0 rows.
    interface parquet_get_nrows
        module procedure parquet_get_nrows_int64
        module procedure parquet_get_nrows_int32
    end interface parquet_get_nrows

    !> Opens `filename` for reading into `reader`, optionally applying a
    !> parquet_filter/qc schema and prefetching columns; see
    !> parquet_open_reader_base's own doc comment below for the full argument
    !> list. nrows= is generic over integer(int32)/integer(int64)
    !> (parquet_open_reader_nrows_int32/_int64), plus the original nrows-less
    !> form (parquet_open_reader_base) for when nrows isn't wanted at all.
    !> This mirrors parquet_get_nrows's own int32/int64 overload above;
    !> nrows is required (not optional) in the two typed specifics -- an
    !> optional dummy that may be absent can't be the sole thing
    !> distinguishing two specific procedures in a generic interface (a call
    !> omitting it would be ambiguous), so parquet_open_reader_base carries
    !> the nrows-absent case as a separate specific instead.
    interface parquet_open_reader
        module procedure parquet_open_reader_base
        module procedure parquet_open_reader_nrows_int64
        module procedure parquet_open_reader_nrows_int32
    end interface parquet_open_reader

    !> Reads back one key's value from the flat key-value table metadata a
    !> parquet_writer wrote via add_metadata (see parquet_reader%metadata,
    !> populated once by parquet_open_reader). `value`'s declared type/kind
    !> selects the specific procedure, so it also selects which stored
    !> representation is expected -- the stored string (always written by
    !> add_metadata as plain text, see parquet_metadata.f90) is parsed back
    !> into that type. Any key is allowed, including the writer's own
    !> reserved/internal keys (e.g. "DATE", "column.<name>.unit").
    !>
    !> If `key` is missing: returns `default` if given (printing a WARNING
    !> unless warn=.false.), else error stops. If `key` is present but its
    !> stored text cannot be converted to the requested type (including a
    !> stored integer that overflows a 32-bit target): always prints a
    !> WARNING, then falls back to `default` if given, else error stops.
    !> `default` must be the same type/kind as `value`; `warn` defaults to
    !> .true. and only affects the "key missing, default used" case.
    interface parquet_get_metadata
        module procedure parquet_get_metadata_int32
        module procedure parquet_get_metadata_int64
        module procedure parquet_get_metadata_float32
        module procedure parquet_get_metadata_float64
        module procedure parquet_get_metadata_logical
        module procedure parquet_get_metadata_string
        module procedure parquet_get_metadata_int32_array
        module procedure parquet_get_metadata_int64_array
        module procedure parquet_get_metadata_float32_array
        module procedure parquet_get_metadata_float64_array
        module procedure parquet_get_metadata_logical_array
        module procedure parquet_get_metadata_string_array
    end interface parquet_get_metadata

    !> Returns the total element count of a vector (array) column across every
    !> row (i.e. nrows * col_size) in `total_elements`, dispatched by its
    !> integer(int32)/integer(int64) kind.
    interface parquet_get_column_total_elements
        module procedure parquet_get_column_total_elements_int64
        module procedure parquet_get_column_total_elements_int32
    end interface parquet_get_column_total_elements

    !> Reads and caches the named column(s) right away rather than lazily on
    !> first request. parquet_prefetch_columns accepts either an array of
    !> column names (each element sharing one declared length -- pad shorter
    !> names with blanks) or a single scalar string listing the names
    !> separated by commas and/or semicolons ("ra;dec,mag"). The scalar form
    !> avoids the fixed-length array pitfall where a too-short declared
    !> length silently truncates a name.
    interface parquet_prefetch_columns
        module procedure parquet_prefetch_columns_array
        module procedure parquet_prefetch_columns_string
    end interface parquet_prefetch_columns

    public :: parquet_writer
    public :: parquet_reader
    public :: parquet_filter
    public :: parquet_column_info
    public :: parquet_column_type
    public :: parquet_table_metadata
    public :: parquet_schema
    public :: parquet_maml_file
    public :: parquet_open_writer
    public :: parquet_write_column
    public :: parquet_close_writer
    public :: parquet_get_version
    public :: parquet_parse_maml
    public :: parquet_load_maml_file
    public :: parquet_load_qc_maml_file
    public :: parquet_validate_maml
    public :: parquet_validate_user_maml
    public :: parquet_open_reader
    public :: parquet_close_reader
    public :: parquet_prefetch_columns
    public :: parquet_get_nrows
    public :: parquet_get_col_size
    public :: parquet_get_column_total_elements
    public :: parquet_get_string_length
    public :: parquet_get_metadata
    public :: parquet_read_column
    public :: parquet_read_array_row_mode
    public :: parquet_read_array_element_mode
    public :: parquet_set_max_threads

    interface
        !> 1-based index of `name` in writer%enabled_columns (currently
        !> enabled/set columns only), or 0 if not found among them.
        module integer function parquet_get_enabled_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer !! open writer to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_get_enabled_column_index

        !> 1-based index of `name` in writer%all_columns (every declared
        !> column, enabled or not), or 0 if not found among them.
        module integer function parquet_get_defined_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer !! open writer to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_get_defined_column_index

        !> True if a parquet_write_column call declaring `expected_type`
        !> (the actual/declared Fortran type/kind of `values`) is compatible
        !> with the schema's own `actual_type` for that column -- exact match,
        !> or a numeric widening (e.g. int32/int64 values written into a
        !> float32/float64 schema column).
        module logical function parquet_is_type_compatible(actual_type, expected_type)
            character(len=*), intent(in) :: actual_type !! schema-declared data_type for the column.
            character(len=*), intent(in) :: expected_type !! data_type implied by the write call's own values.
        end function parquet_is_type_compatible

        !> Parses `raw` (a qc: min:/max: bound, operator already stripped) as
        !> a real64 number appropriate for `data_type`: for float32/float64,
        !> just requires a finite (non-NaN/non-Inf) parse; for int32/int64,
        !> additionally requires the parsed value to be an exact integer
        !> within that type's representable range. Returns .false. (value
        !> undefined) if parsing fails or any of these checks fail. Not
        !> meaningful for "string" (no numeric bound) or "boolean" (qc: is
        !> never enforced there) -- callers should not invoke this for those.
        module logical function parquet_qc_numeric_bound(raw, data_type, value)
            character(len=*), intent(in) :: raw !! qc: min:/max: text, operator prefix already stripped.
            character(len=*), intent(in) :: data_type !! field's declared data_type; selects the parsing rule.
            real(real64), intent(out) :: value !! parsed bound value; only meaningful when the function returns .true.
        end function parquet_qc_numeric_bound

        !> Parses one qc: min:/max: value (already unquoted or not) into an
        !> operator + bound-text pair: a leading ">=", "<=", ">", or "<" (checked
        !> in that order, so the two-char operators are never mistaken for the
        !> one-char ones) is stripped and used as the operator; otherwise
        !> `default_op` applies (">=" for min:, "<=" for max:, matching the MAML
        !> format's documented inclusive-by-default convention). The remaining
        !> text is kept verbatim (not yet converted to a number) -- numeric
        !> parsing/validity is deferred to parquet_validate_maml_internal and to
        !> the write-time qc check, since it depends on the field's data_type,
        !> which may not be known yet at this point in parsing.
        module subroutine parquet_set_qc_bound(has_flag, op, raw, cvalue, default_op)
            logical, intent(out) :: has_flag !! .true. once set (a qc: min:/max: value was present).
            character(len=2), intent(out) :: op !! parsed operator (">=", "<=", "> ", or "< ").
            character(len=:), allocatable, intent(out) :: raw !! bound text, verbatim, operator prefix stripped.
            character(len=*), intent(in) :: cvalue !! raw qc: min:/max: value text (possibly quoted).
            character(len=*), intent(in) :: default_op !! operator to use when cvalue has no explicit prefix.
        end subroutine parquet_set_qc_bound

        !> Parses a `protected_cols:` entry nested inside `extra:`, e.g.:
        !>   extra:
        !>     protected_cols: col1;col2; col3
        !> or, equivalently:
        !>   extra:
        !>     protected_cols:
        !>     - col1
        !>     - col2
        !>     - col3
        !> `names` is a zero-size array if there is no extra:/protected_cols:
        !> section. Names are trimmed and unquoted; empty tokens (e.g. a stray
        !> ";;" or trailing ";") are skipped. Matching against declared field
        !> names (by output_name) is done by the caller -- this subroutine only
        !> extracts the raw name list. Subroutine (not a function returning
        !> names) to sidestep a gfortran 15.2.0 ICE with a submodule-implemented
        !> module function returning a deferred-length character array result.
        module subroutine parquet_parse_protected_cols(lines, names)
            character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
            character(len=:), allocatable, intent(out) :: names(:) !! trimmed, unquoted protected column names.
        end subroutine parquet_parse_protected_cols

        !> Parses a `col_map:` block nested inside `extra:` (col_map: is NOT a
        !> valid top-level MAML section) into (internal_name -> output_name)
        !> entries. Each list item is a single "<internal_name>: <output_name>"
        !> line (with its leading "- "), e.g.:
        !>   extra:
        !>     col_map:
        !>     - col_internal: col_user
        !> Unlike every other map-list section (fields:, keyarray:, ...), the key
        !> here IS the data (an arbitrary internal column name) rather than a
        !> fixed sub-key label, so this is a dedicated parser rather than a
        !> generic one. extra:'s own content is otherwise entirely unvalidated
        !> (see the "extra" entry in allowed_maml_sections), so this is a
        !> narrow, specific lookup rather than a generically-validated section.
        !> Returns a zero-size array if there is no extra:/col_map: section.
        module function parquet_parse_col_map(lines) result(col_map)
            character(len=*), intent(in) :: lines(:) !! raw MAML source lines to scan.
            type(parquet_maml_col_map_entry), allocatable :: col_map(:) !! parsed (internal_name, output_name) entries.
        end function parquet_parse_col_map

        !> Error stops if `name` is not a defined column, or is defined with a
        !> data_type incompatible with `expected_type` (see parquet_is_type_compatible).
        module subroutine parquet_assert_column_type(writer, name, expected_type)
            type(parquet_writer), intent(in) :: writer !! open writer to check against.
            character(len=*), intent(in) :: name !! column name being written.
            character(len=*), intent(in) :: expected_type !! data_type implied by the write call's own values.
        end subroutine parquet_assert_column_type

        !> Records that `name` has now been written at least once, growing
        !> writer%written_names (schema-less writer only) so a later repeat
        !> write to the same name can be detected.
        module subroutine parquet_mark_column_written(writer, name)
            type(parquet_writer), intent(inout) :: writer !! open (schema-less) writer being written to.
            character(len=*), intent(in) :: name !! column name just written.
        end subroutine parquet_mark_column_written

        !> True if `name` is a currently enabled/set column of a schema-
        !> enforced writer (see parquet_column_type%is_set); always .true.
        !> for a schema-less writer once the column has been defined.
        module logical function parquet_is_column_enabled(writer, name)
            type(parquet_writer), intent(in) :: writer !! open writer to check.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_is_column_enabled

        !> The declared col_size (vector-column element count) for `name`,
        !> from the writer's schema; error stops if `name` is not defined.
        module integer function parquet_get_column_col_size(writer, name)
            type(parquet_writer), intent(in) :: writer !! open (schema-enforced) writer to check.
            character(len=*), intent(in) :: name !! column name to look up.
        end function parquet_get_column_col_size

        !> Every column in a file must have the same number of rows (Arrow/Parquet
        !> requirement). Called by every parquet_write_column variant with that
        !> call's own row count: the first call for a given writer fixes the
        !> expected row count, every later call must match it or error stop.
        module subroutine parquet_check_row_count(writer, name, nrows)
            type(parquet_writer), intent(inout) :: writer !! open writer whose expected_nrows this call checks/sets.
            character(len=*), intent(in) :: name !! column being written; named only in the error-stop message.
            integer(c_long_long), intent(in) :: nrows !! row count of this write call's own values.
        end subroutine parquet_check_row_count

        !> Creates `filename` and opens `writer` for writing. Without `schema`,
        !> the writer is schema-less: columns are inferred from the first
        !> parquet_write_column call for each name, with no field metadata/QC.
        !> With `schema`, every column/type/QC rule is fixed up front and
        !> enforced on every write.
        module subroutine parquet_open_writer(writer, filename, schema, write_maml, qc, &
                compression, compression_level, chunk_size, use_threads)
            type(parquet_writer), intent(out) :: writer !! writer to open.
            character(len=*), intent(in) :: filename !! output .parquet path.
            type(parquet_schema), intent(in), optional :: schema !! schema to enforce; schema-less writer if absent.
            logical, intent(in), optional :: write_maml !! also save a sidecar .maml next to filename (needs schema).
            logical, intent(in), optional :: qc !! enable qc: min/max WARNING checks on write (needs schema).
            character(len=*), intent(in), optional :: compression !! Arrow compression codec name (e.g. "snappy", "zstd").
            integer, intent(in), optional :: compression_level !! codec-specific compression level.
            integer, intent(in), optional :: chunk_size !! Parquet row-group size.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded writer.
        end subroutine parquet_open_writer

        !> Declares one column on a schema-less writer (%is_schema_enforced
        !> .false.), mirroring the field metadata a MAML fields: entry would
        !> otherwise supply. Called internally the first time each column
        !> name is written; not part of the public API.
        module subroutine parquet_add_column_info(writer, name, unit, description, ucd, data_type, array_size, col_size)
            type(parquet_writer), intent(inout) :: writer !! schema-less writer gaining this column.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: unit !! unit of measurement.
            character(len=*), intent(in) :: description !! short free-text description.
            character(len=*), intent(in) :: ucd !! IVOA Unified Content Descriptor.
            character(len=*), intent(in) :: data_type !! column's data type.
            integer, intent(in) :: array_size !! maximum string length (string columns only).
            integer, intent(in) :: col_size !! vector-column element count (1 for a scalar column).
        end subroutine parquet_add_column_info

        !> Writes a scalar int32 column; see the parquet_write_column generic
        !> interface above for the shared behavior of this whole family.
        module subroutine parquet_write_int32_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_int32_column

        !> Writes a vector (matrix) int32 column, one row per column of
        !> `values`; see parquet_write_column above.
        module subroutine parquet_write_int32_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_int32_matrix_column

        !> Writes a scalar int64 column; see parquet_write_column above.
        module subroutine parquet_write_int64_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_int64_column

        !> Writes a vector (matrix) int64 column; see parquet_write_column above.
        module subroutine parquet_write_int64_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_int64_matrix_column

        !> Writes a scalar float32 column; see parquet_write_column above.
        module subroutine parquet_write_float32_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_float32_column

        !> Writes a vector (matrix) float32 column; see parquet_write_column above.
        module subroutine parquet_write_float32_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_float32_matrix_column

        !> Writes a scalar float64 column; see parquet_write_column above.
        module subroutine parquet_write_float64_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_float64_column

        !> Writes a vector (matrix) float64 column; see parquet_write_column above.
        module subroutine parquet_write_float64_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_float64_matrix_column

        !> Writes a scalar logical (boolean) column; see parquet_write_column above.
        module subroutine parquet_write_logical_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_logical_column

        !> Writes a vector (matrix) logical (boolean) column; see parquet_write_column above.
        module subroutine parquet_write_logical_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_logical_matrix_column

        !> Writes a scalar string column; see parquet_write_column above.
        module subroutine parquet_write_string_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: values(:) !! one value per row.
            logical, intent(in), optional :: is_valid(:) !! per-row validity mask (.false. => null).
        end subroutine parquet_write_string_column

        !> Writes a vector (matrix) string column; see parquet_write_column above.
        module subroutine parquet_write_string_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer !! open writer.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(in) :: values(:,:) !! (element, row) values.
            logical, intent(in), optional :: is_valid(:,:) !! per-element validity mask (.false. => null).
        end subroutine parquet_write_string_matrix_column

        !> Flushes and closes `writer`; error stops if any declared/enabled
        !> column was never written (schema-enforced writer only).
        module subroutine parquet_close_writer(writer)
            type(parquet_writer), intent(inout) :: writer !! writer to close.
        end subroutine parquet_close_writer

        !> FINAL procedure: safety-net close for a writer whose variable goes
        !> out of scope (or is overwritten) still open; see parquet_writer's
        !> own doc comment for why this is not a substitute for %close.
        module subroutine writer_finalize(this)
            type(parquet_writer), intent(inout) :: this !! writer being finalized.
        end subroutine writer_finalize

        !> File form of parquet_parse_maml: loads `filename` from disk, then
        !> parses/validates it into `schema` (%maml, %cinfo, %metadata).
        module subroutine parquet_parse_maml_from_file(filename, schema)
            character(len=*), intent(in) :: filename !! .maml file path.
            type(parquet_schema), intent(out) :: schema !! fully parsed schema.
        end subroutine parquet_parse_maml_from_file

        !> Loads `filename`'s raw lines from disk into a parquet_maml_file,
        !> running the full parquet_validate_maml checks (table:, at least
        !> one field, valid data_type, ...); not parsed into a schema yet
        !> (see parquet_parse_maml).
        module function parquet_load_maml_file(filename) result(maml)
            character(len=*), intent(in) :: filename !! .maml file path.
            type(parquet_maml_file) :: maml !! validated raw MAML.
        end function parquet_load_maml_file

        !> Loads a qc-maml's raw lines from disk WITHOUT running the full
        !> parquet_validate_maml checks parquet_load_maml_file always applies
        !> (table:, at least one field, valid data_type, ...) -- a qc-maml
        !> only needs name + qc: min:/max:/miss: per field, and doesn't need
        !> data_type at all. Its own (lighter) validation happens later, in
        !> parquet_parse_qc_maml, when parquet_open_reader(..., schema=) uses it.
        !> Returns a parquet_schema with only %maml populated (the raw qc-maml
        !> lines); %cinfo/%metadata stay empty, since a qc-maml is never parsed.
        module function parquet_load_qc_maml_file(filename) result(schema)
            character(len=*), intent(in) :: filename !! qc-maml file path.
            type(parquet_schema) :: schema !! schema with only %maml populated (raw qc-maml lines).
        end function parquet_load_qc_maml_file

        !> Validates and parses a qc-maml's fields: entries into `rules`, one
        !> entry per field that has at least a name (data_type/unit/ucd/etc.
        !> are irrelevant here and never required) -- error stops on a
        !> duplicate field name, an unknown top-level section/sub-key, or an
        !> unrecognized qc: miss: value (anything other than Null/NA,
        !> case-insensitive, or empty). Table-level metadata is ignored
        !> entirely. See parquet_qc_rule's own doc comment for min_text/max_text.
        module subroutine parquet_parse_qc_maml(maml, rules)
            type(parquet_maml_file), intent(in) :: maml !! raw qc-maml (e.g. from parquet_load_qc_maml_file).
            type(parquet_qc_rule), allocatable, intent(out) :: rules(:) !! one entry per field with at least a name.
        end subroutine parquet_parse_qc_maml

        !> Validates `user_maml` against `base_maml` (the full base schema):
        !> populates user_maml%missing_columns/%col_map, error stops on any
        !> field user_maml declares that base_maml doesn't.
        module subroutine parquet_validate_user_maml(base_maml, user_maml)
            type(parquet_maml_file), intent(in) :: base_maml !! the full base schema to validate against.
            type(parquet_maml_file), intent(inout) :: user_maml !! user-supplied MAML being validated.
        end subroutine parquet_validate_user_maml

        !> Runs the full parquet_validate_maml checks (table:, at least one
        !> field, valid data_type, ...) against an already-loaded MAML.
        module subroutine parquet_validate_maml_internal(maml)
            type(parquet_maml_file), intent(in) :: maml !! raw MAML to validate.
        end subroutine parquet_validate_maml_internal

        !> Checks every top-level section name (and, for map-list sections
        !> like fields:/keyarray:, their items' sub-keys) in `lines` against
        !> the allowed_maml_sections/allowed_maml_nested_sections schema
        !> (src/parquet_metadata_sections.f90); appends one message per
        !> violation to `errors` (key presence only, not semantic content).
        !> Called by both parquet_validate_maml_internal (full schema mamls)
        !> and parquet_parse_qc_maml (qc-mamls, a strict subset of the schema).
        module subroutine parquet_validate_maml_sections(lines, errors)
            character(len=*), intent(in) :: lines(:) !! raw MAML source lines to check.
            character(len=:), allocatable, intent(inout) :: errors !! accumulated error messages; appended to, not reset.
        end subroutine parquet_validate_maml_sections

        !> File form of parquet_validate_maml: loads `maml` from disk first,
        !> then runs the same checks as parquet_validate_maml_internal.
        module subroutine parquet_validate_maml_file(maml)
            character(len=*), intent(in) :: maml !! .maml file path.
        end subroutine parquet_validate_maml_file

        !> Object form of parquet_parse_maml: parses/validates a schema whose
        !> %maml has already been populated (e.g. built in memory), filling
        !> in %cinfo/%metadata in place.
        module subroutine parquet_parse_maml_from_object(schema)
            type(parquet_schema), intent(inout) :: schema !! schema with %maml already populated.
        end subroutine parquet_parse_maml_from_object

        !> Initializes a from-scratch parquet_schema: sets the (required)
        !> table: key and any of the optional scalar top-level MAML keys
        !> given, and marks this schema ready for %add_field. Error stops if
        !> called twice on the same schema. List-shaped top-level sections
        !> (coauthors:, comments:, keywords:, DOIs:, depends:, keyarray:,
        !> extra:) are out of scope here -- keyarray: already has its own
        !> API (add_metadata); the others aren't supported by %init/%add_field
        !> at all yet.
        module subroutine schema_init(this, table, survey, dataset, version, date, author, description, license, &
                maml_version)
            class(parquet_schema), intent(inout) :: this !! schema being initialized (must not already be initialized).
            character(len=*), intent(in) :: table !! required table: key.
            character(len=*), intent(in), optional :: survey !! optional survey: key.
            character(len=*), intent(in), optional :: dataset !! optional dataset: key.
            character(len=*), intent(in), optional :: version !! optional version: key.
            character(len=*), intent(in), optional :: date !! optional date: key.
            character(len=*), intent(in), optional :: author !! optional author: key.
            character(len=*), intent(in), optional :: description !! optional description: key.
            character(len=*), intent(in), optional :: license !! optional license: key.
            character(len=*), intent(in), optional :: maml_version !! optional MAML_version: key.
        end subroutine schema_init

        !> Structure-constructor form of %init: builds and returns an
        !> initialized parquet_schema in one expression instead of
        !> declaring the variable and calling %init separately. Same
        !> arguments and error-stop conditions as schema_init.
        module function parquet_schema_new(table, survey, dataset, version, date, author, description, license, &
                maml_version) result(this)
            character(len=*), intent(in) :: table !! required table: key.
            character(len=*), intent(in), optional :: survey !! optional survey: key.
            character(len=*), intent(in), optional :: dataset !! optional dataset: key.
            character(len=*), intent(in), optional :: version !! optional version: key.
            character(len=*), intent(in), optional :: date !! optional date: key.
            character(len=*), intent(in), optional :: author !! optional author: key.
            character(len=*), intent(in), optional :: description !! optional description: key.
            character(len=*), intent(in), optional :: license !! optional license: key.
            character(len=*), intent(in), optional :: maml_version !! optional MAML_version: key.
            type(parquet_schema) :: this !! newly initialized schema.
        end function parquet_schema_new

        !> Appends one fields: entry to a schema built from scratch (%init
        !> must be called first). name/data_type are required (data_type
        !> must be one of the supported types); unit/info/ucd/array_size/
        !> col_size and qc_min/qc_max/qc_miss are optional, the latter three
        !> forming an optional qc: sub-block (same min:/max: operator-direction
        !> and miss: Null/NA rules %add_col_qc enforces, checked independently
        !> here -- %add_field is a distinct API from %add_col_qc, not built on
        !> top of it: %add_col_qc is for read-time qc-mamls (name + qc: only,
        !> no data_type) and explicitly rejects a column already declared as a
        !> field, so it cannot be layered onto a field %add_field just added).
        !> Validates eagerly (name/data_type/duplicate/qc all checked here,
        !> not deferred to parquet_validate_maml).
        module subroutine schema_add_field(this, name, data_type, unit, info, ucd, array_size, col_size, &
                qc_min, qc_max, qc_miss)
            class(parquet_schema), intent(inout) :: this !! schema being built (%init must have been called first).
            character(len=*), intent(in) :: name !! field name.
            character(len=*), intent(in) :: data_type !! field's data type (must be one of the supported types).
            character(len=*), intent(in), optional :: unit !! unit of measurement.
            character(len=*), intent(in), optional :: info !! short description.
            character(len=*), intent(in), optional :: ucd !! IVOA Unified Content Descriptor.
            integer, intent(in), optional :: array_size !! maximum string length (string fields only).
            integer, intent(in), optional :: col_size !! vector-column element count.
            character(len=*), intent(in), optional :: qc_min !! qc: min: bound (operator prefix allowed).
            character(len=*), intent(in), optional :: qc_max !! qc: max: bound (operator prefix allowed).
            character(len=*), intent(in), optional :: qc_miss !! qc: miss: value (Null/NA, case-insensitive).
        end subroutine schema_add_field

        ! Flat convenience passthroughs on parquet_schema -- each forwards to
        ! the matching procedure on %cinfo, %metadata or %maml.
        !> Forwards to %cinfo%set_column_available; enables `name`, or every
        !> non-deactivated column if `name` is absent.
        module subroutine set_column_available(this, name)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in), optional :: name !! column to enable; every column if absent.
        end subroutine set_column_available

        !> Forwards to %cinfo%set_column_unavailable; disables `name`, or
        !> every non-deactivated column if `name` is absent.
        module subroutine set_column_unavailable(this, name)
            class(parquet_schema), intent(inout) :: this !! schema whose cinfo is updated.
            character(len=*), intent(in), optional :: name !! column to disable; every column if absent.
        end subroutine set_column_unavailable

        !> Forwards to %cinfo%get_column_index.
        module integer function schema_get_column_index(this, name)
            class(parquet_schema), intent(in) :: this !! schema to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function schema_get_column_index

        !> Forwards to %cinfo%get_num_fields.
        module integer function schema_get_num_fields(this)
            class(parquet_schema), intent(in) :: this !! schema to query.
        end function schema_get_num_fields

        !> Forwards to %cinfo%get_field_name.
        module function schema_get_field_name(this, index) result(name)
            class(parquet_schema), intent(in) :: this !! schema to query.
            integer, intent(in) :: index !! 1-based field position in MAML source order.
            character(len=:), allocatable :: name !! field name at that position.
        end function schema_get_field_name

        !> Subroutine form of %add_col_qc: forwards to %maml%add_col_qc. See
        !> parquet_maml_add_col_qc (parquet_maml_base_add_col_qc.f90) for the
        !> "col, min, max, miss" input syntax and validation rules.
        module subroutine schema_add_col_qc(this, qc_input, col_name)
            class(parquet_schema), intent(inout) :: this !! schema whose %maml gains one fields: entry.
            character(len=*), intent(in) :: qc_input !! compact "col, min, max, miss" string.
            character(len=:), allocatable, intent(out), optional :: col_name !! parsed column name.
        end subroutine schema_add_col_qc

        !> Function form of %add_col_qc: forwards to %maml%get_col_qc, so the
        !> parsed name can be assigned back in place (col = schema%get_col_qc(col)).
        module function schema_get_col_qc(this, qc_input) result(col_name)
            class(parquet_schema), intent(inout) :: this !! schema whose %maml gains one fields: entry.
            character(len=*), intent(in) :: qc_input !! compact "col, min, max, miss" string.
            character(len=:), allocatable :: col_name !! parsed column name.
        end function schema_get_col_qc

        !> int32 specific of %add_metadata; forwards to %metadata%add_metadata
        !> (see the parquet_get_metadata generic interface above for the
        !> read-side counterpart and its stored-representation semantics).
        module subroutine schema_add_metadata_int32(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_int32

        !> int64 specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_int64(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_int64

        !> float32 specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float32(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine schema_add_metadata_float32

        !> float64 specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float64(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine schema_add_metadata_float64

        !> logical specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_logical(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_logical

        !> string specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_string(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_string

        !> int32 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_int32_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_int32_array

        !> int64 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_int64_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_int64_array

        !> float32 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float32_array(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine schema_add_metadata_float32_array

        !> float64 array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_float64_array(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine schema_add_metadata_float64_array

        !> logical array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_logical_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_logical_array

        !> string array specific of %add_metadata; see schema_add_metadata_int32.
        module subroutine schema_add_metadata_string_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this !! schema whose %metadata gains one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine schema_add_metadata_string_array

        !> Parses raw MAML source `lines` into `cinfo` (per-field schema/QC)
        !> and `metadata` (flat key-value table metadata); the shared worker
        !> behind parquet_parse_maml's file/object specifics.
        module subroutine parquet_parse_maml_lines(lines, cinfo, metadata)
            character(len=*), intent(in) :: lines(:) !! raw MAML source, one array element per line.
            type(parquet_column_info), intent(out) :: cinfo !! parsed per-field schema/QC state.
            type(parquet_table_metadata), intent(out) :: metadata !! parsed flat key-value table metadata.
        end subroutine parquet_parse_maml_lines

        !> 1-based index of `name` in this%col, or 0 if not found.
        module integer function get_column_index(this, name)
            class(parquet_column_info), intent(in) :: this !! column_info to search.
            character(len=*), intent(in) :: name !! column name to look up.
        end function get_column_index

        !> Total number of fields defined in this column_info, in maml source
        !> order, with no filtering by is_set/is_deactivated -- i.e. every
        !> field that was ever declared (via a fields: entry or %add_field).
        module integer function get_num_fields(this)
            class(parquet_column_info), intent(in) :: this !! column_info to query.
        end function get_num_fields

        !> Name of the field at the given 1-based position in maml source
        !> order (same order get_num_fields counts). index must be between 1
        !> and get_num_fields(this); anything outside that range fails with
        !> error stop.
        module function get_field_name(this, index) result(name)
            class(parquet_column_info), intent(in) :: this !! column_info to query.
            integer, intent(in) :: index !! 1-based field position in MAML source order.
            character(len=:), allocatable :: name !! field name at that position.
        end function get_field_name

        !> Backs parquet_schema%set_column_unavailable (see set_column_unavailable
        !> in the parquet_schema block above); disables `name`, or every
        !> non-deactivated column if `name` is absent. Error stops if `name`
        !> is a deactivated column.
        module subroutine set_unavailable(this, name)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in), optional :: name !! column to disable; every column if absent.
        end subroutine set_unavailable

        !> Backs parquet_schema%set_column_available; enables `name`, or every
        !> non-deactivated column if `name` is absent. Error stops if `name`
        !> is a deactivated column.
        module subroutine set_available(this, name)
            class(parquet_column_info), intent(inout) :: this !! column_info being updated.
            character(len=*), intent(in), optional :: name !! column to enable; every column if absent.
        end subroutine set_available

        !> Appends `line` to `lines` at 1-based position `n`, growing the
        !> array if needed; internal MAML-source-lines plumbing.
        module subroutine parquet_append_line(lines, n, line)
            character(len=1024), allocatable, intent(inout) :: lines(:) !! line buffer being appended to.
            integer, intent(in) :: n !! number of lines already in use before this call.
            character(len=*), intent(in) :: line !! line text to store.
        end subroutine parquet_append_line

        !> Shared worker behind every add_metadata specific: appends one
        !> already-stringified key/value/description to `metadata%items`.
        module subroutine parquet_metadata_append_entry(metadata, key, value, description)
            class(parquet_table_metadata), intent(inout) :: metadata !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value !! metadata value, already converted to text.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine parquet_metadata_append_entry

        !> Appends a `- key: / value: / comment:` entry to the `keyarray:` block
        !> inside `lines` (the verbatim source MAML content kept in
        !> metadata%source_maml_lines), so that metadata added at runtime via
        !> add_metadata after parquet_read_maml is reflected in a later
        !> write_maml sidecar. Always appends; does not update an existing entry
        !> that has the same key. Inserted before `extra:` if present, else
        !> before `fields:`; synthesizes the `keyarray:` header itself if the
        !> source MAML did not already have one.
        module subroutine parquet_append_keyarray_line(lines, key, value, desc)
            character(len=:), allocatable, intent(inout) :: lines(:) !! verbatim source MAML lines being amended.
            character(len=*), intent(in) :: key !! metadata key for the new entry.
            character(len=*), intent(in) :: value !! metadata value for the new entry.
            character(len=*), intent(in) :: desc !! metadata description for the new entry (may be "").
        end subroutine parquet_append_keyarray_line

        !> int32 specific of parquet_table_metadata%add_metadata; stores
        !> `value` as plain text via parquet_metadata_append_entry.
        module subroutine add_metadata_int32(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_int32

        !> int64 specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_int64(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_int64

        !> float32 specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float32(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine add_metadata_float32

        !> float64 specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float64(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine add_metadata_float64

        !> logical specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_logical(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_logical

        !> string specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_string(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value !! metadata value.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_string

        !> int32 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_int32_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_int32_array

        !> int64 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_int64_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            integer(int64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_int64_array

        !> float32 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float32_array(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real32), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine add_metadata_float32_array

        !> float64 array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_float64_array(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            real(real64), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
            character(len=*), intent(in), optional :: fmt !! optional Fortran edit descriptor for the stored text.
        end subroutine add_metadata_float64_array

        !> logical array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_logical_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            logical, intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_logical_array

        !> string array specific of parquet_table_metadata%add_metadata; see add_metadata_int32.
        module subroutine add_metadata_string_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this !! table metadata gaining one entry.
            character(len=*), intent(in) :: key !! metadata key.
            character(len=*), intent(in) :: value(:) !! metadata values.
            character(len=*), intent(in), optional :: description !! optional free-text description.
        end subroutine add_metadata_string_array

        !> Grows `columns` by one empty (default-initialized) entry and
        !> increments `n` to match; internal fields: parsing plumbing.
        module subroutine parquet_append_empty_cinfo(columns, n)
            type(parquet_column_type), allocatable, intent(inout) :: columns(:) !! column array being grown.
            integer, intent(inout) :: n !! number of entries in use before this call; incremented by 1.
        end subroutine parquet_append_empty_cinfo

        !> Splits a MAML "key: value" `line` on its first colon, trimming
        !> and unquoting `value` (see parquet_unquote); `key` is trimmed but
        !> never unquoted.
        module subroutine parquet_split_key_value(line, key, value)
            character(len=*), intent(in) :: line !! raw MAML line, "key: value" form.
            character(len=:), allocatable, intent(out) :: key !! trimmed key text.
            character(len=:), allocatable, intent(out) :: value !! trimmed, unquoted value text.
        end subroutine parquet_split_key_value

        !> Strips one matching pair of surrounding single or double quotes
        !> from `s`, if present; returns `s` unchanged otherwise.
        module function parquet_unquote(s) result(out)
            character(len=*), intent(in) :: s !! text to unquote.
            character(len=:), allocatable :: out !! s with surrounding quotes removed, if any.
        end function parquet_unquote

        !> Case-insensitive lowercase of ASCII letters.
        module function parquet_to_lower(s) result(out)
            character(len=*), intent(in) :: s !! input string.
            character(len=:), allocatable :: out !! s with every ASCII A-Z lowercased; other characters unchanged.
        end function parquet_to_lower

        !> The nrows-less form of parquet_open_reader (see the generic
        !> interface above): opens as usual, filling in none of the
        !> post-filter row count. Called directly by the two nrows= forms
        !> below, which then call parquet_get_nrows themselves.
        !>
        !> prefetch (default .false.): when .true., every column in the file is
        !> read and cached right away, after any filter has been applied (see
        !> parquet_open_reader_base's implementation for why that ordering
        !> matters), instead of each column being read lazily on first request.
        !> Equivalent to calling parquet_prefetch_columns for every column in
        !> the file immediately after opening. Materializes the whole file in
        !> memory up front -- see MANUAL.md's "Performance and memory" section.
        module subroutine parquet_open_reader_base(reader, filename, use_threads, filter, schema, qc, qc_soft, prefetch)
            type(parquet_reader), intent(out) :: reader !! reader to open.
            character(len=*), intent(in) :: filename !! .parquet file path.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded reader.
            type(parquet_filter), intent(in), optional :: filter !! row filter to apply.
            type(parquet_schema), intent(in), optional :: schema !! schema/qc-maml to validate columns against.
            logical, intent(in), optional :: qc !! enable qc: min/max/miss enforcement (needs schema).
            logical, intent(in), optional :: qc_soft !! qc violations warn instead of error-stopping.
            logical, intent(in), optional :: prefetch !! read and cache every column immediately.
        end subroutine parquet_open_reader_base

        !> nrows (integer(int64)): filled in with the post-filter row count
        !> via parquet_get_nrows(reader, nrows, check_positive=.true.) -- so,
        !> exactly like that check_positive path, a file (or filter result)
        !> with zero rows fails immediately with error stop instead of
        !> silently returning nrows=0. Omit nrows entirely (dispatches to
        !> parquet_open_reader_base above) if you want to open a reader that
        !> may legitimately have zero matching rows -- call parquet_get_nrows
        !> yourself afterwards, without check_positive, to get 0 back instead
        !> of aborting.
        module subroutine parquet_open_reader_nrows_int64(reader, filename, use_threads, filter, schema, qc, qc_soft, &
                nrows, prefetch)
            type(parquet_reader), intent(out) :: reader !! reader to open.
            character(len=*), intent(in) :: filename !! .parquet file path.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded reader.
            type(parquet_filter), intent(in), optional :: filter !! row filter to apply.
            type(parquet_schema), intent(in), optional :: schema !! schema/qc-maml to validate columns against.
            logical, intent(in), optional :: qc !! enable qc: min/max/miss enforcement (needs schema).
            logical, intent(in), optional :: qc_soft !! qc violations warn instead of error-stopping.
            integer(int64), intent(out) :: nrows !! post-filter row count; error stops if zero.
            logical, intent(in), optional :: prefetch !! read and cache every column immediately.
        end subroutine parquet_open_reader_nrows_int64

        !> Same as parquet_open_reader_nrows_int64, but for a caller-supplied
        !> integer(int32) nrows -- also fails with error stop (via
        !> parquet_get_nrows_int32) if the actual row count overflows int32,
        !> exactly as a direct parquet_get_nrows(reader, nrows) call with an
        !> integer(int32) nrows would.
        module subroutine parquet_open_reader_nrows_int32(reader, filename, use_threads, filter, schema, qc, qc_soft, &
                nrows, prefetch)
            type(parquet_reader), intent(out) :: reader !! reader to open.
            character(len=*), intent(in) :: filename !! .parquet file path.
            logical, intent(in), optional :: use_threads !! use Arrow's multi-threaded reader.
            type(parquet_filter), intent(in), optional :: filter !! row filter to apply.
            type(parquet_schema), intent(in), optional :: schema !! schema/qc-maml to validate columns against.
            logical, intent(in), optional :: qc !! enable qc: min/max/miss enforcement (needs schema).
            logical, intent(in), optional :: qc_soft !! qc violations warn instead of error-stopping.
            integer(int32), intent(out) :: nrows !! post-filter row count; error stops if zero or if it overflows int32.
            logical, intent(in), optional :: prefetch !! read and cache every column immediately.
        end subroutine parquet_open_reader_nrows_int32

        !> Closes `reader`, freeing the underlying C++ handle.
        module subroutine parquet_close_reader(reader, print_stat)
            type(parquet_reader), intent(inout) :: reader !! reader to close.
            logical, intent(in), optional :: print_stat !! print Arrow read-statistics to stdout on close.
        end subroutine parquet_close_reader

        !> Array specific of parquet_prefetch_columns: one name per element,
        !> fixed-length (pad shorter names with blanks).
        module subroutine parquet_prefetch_columns_array(reader, names)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: names(:) !! column names to prefetch.
        end subroutine parquet_prefetch_columns_array

        !> Scalar-string specific of parquet_prefetch_columns: `names` lists
        !> column names separated by commas and/or semicolons.
        module subroutine parquet_prefetch_columns_string(reader, names)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: names !! comma/semicolon-separated column names.
        end subroutine parquet_prefetch_columns_string

        !> FINAL procedure: safety-net close for a reader whose variable goes
        !> out of scope (or is overwritten) still open; see parquet_writer's
        !> doc comment for why this is not a substitute for %close.
        module subroutine reader_finalize(this)
            type(parquet_reader), intent(inout) :: this !! reader being finalized.
        end subroutine reader_finalize

        !> int64 specific of parquet_get_nrows.
        module subroutine parquet_get_nrows_int64(reader, nrows, check_positive)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int64), intent(out) :: nrows !! post-filter row count.
            logical, intent(in), optional :: check_positive !! error stop instead of returning 0 rows.
        end subroutine parquet_get_nrows_int64

        !> int32 specific of parquet_get_nrows; also error stops if the
        !> actual row count overflows int32.
        module subroutine parquet_get_nrows_int32(reader, nrows, check_positive)
            type(parquet_reader), intent(in) :: reader !! open reader.
            integer(int32), intent(out) :: nrows !! post-filter row count.
            logical, intent(in), optional :: check_positive !! error stop instead of returning 0 rows.
        end subroutine parquet_get_nrows_int32

        !> Returns `name`'s declared col_size (vector-column element count;
        !> 1 for a scalar column) in `col_size`.
        module subroutine parquet_get_col_size(reader, name, col_size)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer, intent(out) :: col_size !! that column's declared element count.
        end subroutine parquet_get_col_size

        !> Every parquet_read_column variant calls this with its own `values`
        !> array's row count (size(values) for a scalar column, size(values, 2)
        !> for a vector column) before reading any data. A mismatch against the
        !> file's actual row count fails immediately with error stop, instead of
        !> reaching the underlying C++ read call, whose own "nrows mismatch"
        !> check reports a clean diagnostic but aborts the process rather than
        !> returning control to Fortran (see report_fatal_error in
        !> parquet_wrapper.cpp).
        module subroutine parquet_check_read_row_count(reader, name, given_nrows)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column being read; named only in the error-stop message.
            integer(c_long_long), intent(in) :: given_nrows !! row count of this read call's own `values` array.
        end subroutine parquet_check_read_row_count

        !> int64 specific of parquet_get_column_total_elements.
        module subroutine parquet_get_column_total_elements_int64(reader, name, total_elements)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(out) :: total_elements !! total element count across every row.
        end subroutine parquet_get_column_total_elements_int64

        !> int32 specific of parquet_get_column_total_elements.
        module subroutine parquet_get_column_total_elements_int32(reader, name, total_elements)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(out) :: total_elements !! total element count across every row.
        end subroutine parquet_get_column_total_elements_int32

        !> Returns the longest actual string length among `name`'s values in
        !> `max_string_length`, so a caller can size a fixed-length
        !> character(len=...) buffer before reading a string column.
        module subroutine parquet_get_string_length(reader, name, max_string_length)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! string column name.
            integer, intent(out) :: max_string_length !! longest value length actually present in the column.
        end subroutine parquet_get_string_length

        !> int32 specific of parquet_get_metadata; see the generic interface
        !> above for the full key-missing/conversion-failure behavior.
        module subroutine parquet_get_metadata_int32(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int32), intent(out) :: value !! parsed value.
            integer(int32), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int32

        !> int64 specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_int64(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int64), intent(out) :: value !! parsed value.
            integer(int64), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int64

        !> float32 specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float32(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real32), intent(out) :: value !! parsed value.
            real(real32), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float32

        !> float64 specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float64(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real64), intent(out) :: value !! parsed value.
            real(real64), intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float64

        !> logical specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_logical(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            logical, intent(out) :: value !! parsed value.
            logical, intent(in), optional :: default !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_logical

        !> string specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_string(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            character(len=:), allocatable, intent(out) :: value !! stored value, verbatim.
            character(len=*), intent(in), optional :: default !! fallback if key is missing.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_string

        !> int32 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_int32_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int32), allocatable, intent(out) :: value(:) !! parsed values.
            integer(int32), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int32_array

        !> int64 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_int64_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            integer(int64), allocatable, intent(out) :: value(:) !! parsed values.
            integer(int64), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_int64_array

        !> float32 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float32_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real32), allocatable, intent(out) :: value(:) !! parsed values.
            real(real32), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float32_array

        !> float64 array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_float64_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            real(real64), allocatable, intent(out) :: value(:) !! parsed values.
            real(real64), intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_float64_array

        !> logical array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_logical_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            logical, allocatable, intent(out) :: value(:) !! parsed values.
            logical, intent(in), optional :: default(:) !! fallback if key is missing or unparsable.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_logical_array

        !> string array specific of parquet_get_metadata; see parquet_get_metadata_int32.
        module subroutine parquet_get_metadata_string_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: key !! metadata key to look up.
            character(len=:), allocatable, intent(out) :: value(:) !! stored values, verbatim.
            character(len=*), intent(in), optional :: default(:) !! fallback if key is missing.
            logical, intent(in), optional :: warn !! print a WARNING on the missing-key/default-used path.
        end subroutine parquet_get_metadata_string_array

        !> Scalar int32 specific of parquet_read_column; see the generic
        !> interface above for the shared behavior of this whole family.
        module subroutine parquet_read_int32_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int32), intent(out) :: values(:) !! one value per row.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int32_column_1d

        !> Scalar int64 specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_int64_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            integer(int64), intent(out) :: values(:) !! one value per row.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int64_column_1d

        !> Scalar float32 specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_float32_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            real(real32), intent(out) :: values(:) !! one value per row.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float32_column_1d

        !> Scalar float64 specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_float64_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            real(real64), intent(out) :: values(:) !! one value per row.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float64_column_1d

        !> Scalar logical (boolean) specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_logical_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            logical, intent(out) :: values(:) !! one value per row.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_logical_column_1d

        !> Scalar string specific of parquet_read_column; see parquet_read_column above.
        module subroutine parquet_read_string_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! column name.
            character(len=*), intent(out) :: values(:) !! one value per row.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_string_column_1d

        !> Vector int32 specific of parquet_read_column: reads the whole
        !> 2-D (element, row) array at once; see parquet_read_column above.
        module subroutine parquet_read_int32_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:, :) !! (element, row) values.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int32_array_full

        !> Vector int64 specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_int64_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:, :) !! (element, row) values.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_int64_array_full

        !> Vector float32 specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_float32_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:, :) !! (element, row) values.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float32_array_full

        !> Vector float64 specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_float64_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:, :) !! (element, row) values.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_float64_array_full

        !> Vector logical (boolean) specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_logical_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:, :) !! (element, row) values.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_logical_array_full

        !> Vector string specific of parquet_read_column; see parquet_read_int32_array_full.
        module subroutine parquet_read_string_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:, :) !! (element, row) values.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:, :) !! per-element validity mask.
        end subroutine parquet_read_string_array_full

        !> int32 specific of parquet_read_array_row_mode; see the generic
        !> interface above for the shared "one row of a vector column" behavior.
        module subroutine parquet_read_int32_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:) !! that row's element vector.
            integer, intent(in) :: row_index !! 1-based row to read.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_int32_array_row_mode

        !> int64 specific of parquet_read_array_row_mode; see parquet_read_int32_array_row_mode.
        module subroutine parquet_read_int64_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:) !! that row's element vector.
            integer, intent(in) :: row_index !! 1-based row to read.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_int64_array_row_mode

        !> float32 specific of parquet_read_array_row_mode; see parquet_read_int32_array_row_mode.
        module subroutine parquet_read_float32_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:) !! that row's element vector.
            integer, intent(in) :: row_index !! 1-based row to read.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_float32_array_row_mode

        !> float64 specific of parquet_read_array_row_mode; see parquet_read_int32_array_row_mode.
        module subroutine parquet_read_float64_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:) !! that row's element vector.
            integer, intent(in) :: row_index !! 1-based row to read.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_float64_array_row_mode

        !> logical (boolean) specific of parquet_read_array_row_mode; see parquet_read_int32_array_row_mode.
        module subroutine parquet_read_logical_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:) !! that row's element vector.
            integer, intent(in) :: row_index !! 1-based row to read.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_logical_array_row_mode

        !> string specific of parquet_read_array_row_mode; see parquet_read_int32_array_row_mode.
        module subroutine parquet_read_string_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:) !! that row's element vector.
            integer, intent(in) :: row_index !! 1-based row to read.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-element validity mask.
        end subroutine parquet_read_string_array_row_mode

        !> int32 specific of parquet_read_array_element_mode; see the generic
        !> interface above for the shared "one element across all rows" behavior.
        module subroutine parquet_read_int32_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int32), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            integer(int32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int32_array_element_mode

        !> int64 specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_int64_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            integer(int64), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            integer(int64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_int64_array_element_mode

        !> float32 specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_float32_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real32), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            real(real32), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float32_array_element_mode

        !> float64 specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_float64_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            real(real64), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            real(real64), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_float64_array_element_mode

        !> logical (boolean) specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_logical_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            logical, intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            logical, intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_logical_array_element_mode

        !> string specific of parquet_read_array_element_mode; see parquet_read_int32_array_element_mode.
        module subroutine parquet_read_string_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader !! open reader.
            character(len=*), intent(in) :: name !! vector column name.
            character(len=*), intent(out) :: values(:) !! that element position from every row.
            integer, intent(in) :: elem_index !! 1-based element position to read.
            character(len=*), intent(in), optional :: null_value !! fill value for missing entries.
            logical, intent(out), optional :: is_valid(:) !! per-row validity mask.
        end subroutine parquet_read_string_array_element_mode
    end interface

contains

    !> Returns the library version. Default: the RELEASE_VERSION build macro's
    !> bare release number; prints a WARNING to stdout first if that disagrees
    !> with cversion (a hand-maintained "vX.Y.Z (date)" string), which signals
    !> a build that skipped fpm's macro substitution or a version bump missed
    !> on one side. internal=.true. instead returns cversion verbatim.
    subroutine parquet_get_version(ver_string, internal)
        implicit none
        character(len=:), allocatable, intent(out) :: ver_string !! resulting version string.
        logical, intent(in), optional :: internal !! .true. returns cversion verbatim instead of RELEASE_VERSION.
        integer :: i
        !
! Accept solution from https://stackoverflow.com/questions/31649691/stringify-macro-with-gnu-gfortran
! which provides the easiest way to pass a macro to a string in Fortran complying with both
! gfortran traditional cpp and the standard cpp syntaxes
#ifdef __GFORTRAN__
#  define STRINGIFY_START(X) "&
#  define STRINGIFY_END(X) &X"
#else
#  define STRINGIFY_(X) #X
#  define STRINGIFY_START(X) &
#  define STRINGIFY_END(X) STRINGIFY_(X)
#endif

        ver_string = STRINGIFY_START(RELEASE_VERSION)
        STRINGIFY_END(RELEASE_VERSION)
        !
        i = index(cversion, " ")
        !
        if (cversion(2:i-1) /= ver_string) then ! GCOVR_EXCL_START
            write(*,*) "WARNING: using developmentparquet-fortran library!"
            write(*,*) "         library version: ", trim(cversion)
            write(*,*) "         RELEASE_VERSION: ", trim(ver_string)
        end if ! GCOVR_EXCL_STOP
        !
        if (present(internal)) then
            if (internal) then
                ver_string = trim(cversion)
            else
                ver_string = trim(ver_string)
            end if
        end if
        !
    end subroutine parquet_get_version

    !> Resizes Arrow's global CPU thread pool -- the single pool shared by
    !> every parquet_reader/parquet_writer in this process that has
    !> use_threads enabled (the default). This is NOT a per-reader/per-writer
    !> setting: call it once, e.g. near the start of your program, before
    !> opening readers/writers on other threads -- calling it concurrently
    !> from multiple threads with different values is a race, since it
    !> resizes a pool everyone else is also using at that moment.
    subroutine parquet_set_max_threads(n)
        implicit none
        integer, intent(in) :: n !! new thread-pool capacity; must be >= 1.

        if (n < 1) error stop "parquet_set_max_threads: n must be >= 1"
        call parquet_set_thread_pool_capacity(int(n, kind=c_int))
    end subroutine parquet_set_max_threads

    !> Appends one AND-combined filter clause; see parquet_filter's own doc
    !> comment for the "<column> <op> [value]" rule syntax. Unvalidated here
    !> -- parquet_open_reader validates every rule when it actually applies
    !> the filter.
    subroutine parquet_filter_add(this, rule)
        class(parquet_filter), intent(inout) :: this !! filter gaining one rule.
        character(len=*), intent(in) :: rule !! raw "<column> <op> [value]" rule text (max 512 characters).
        character(len=512), allocatable :: tmp(:)

        if (len(rule) > len(this%rules)) then
            error stop "parquet_filter%add: rule exceeds the maximum supported length (512 characters): " // trim(rule)
        end if

        if (.not. allocated(this%rules)) then
            allocate(this%rules(1))
            this%rules(1) = rule
            this%n = 1
            return
        end if

        allocate(tmp(this%n + 1))
        tmp(1:this%n) = this%rules
        tmp(this%n + 1) = rule
        call move_alloc(tmp, this%rules)
        this%n = this%n + 1
    end subroutine parquet_filter_add

end module
