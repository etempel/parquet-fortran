!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module parquet
    use iso_c_binding
    use iso_fortran_env, only: int8, int32, int64, real32, real64
    use parquet_bindings
    use parquet_maml_base, only: parquet_maml_file, parquet_maml_missing_column, parquet_maml_col_map_entry
    implicit none
    private
    !
    character(len=*),parameter:: cversion = "v0.9.0 (2026-07-10)" !< version info
#ifndef RELEASE_VERSION
#  define RELEASE_VERSION 0.1
#endif

    type parquet_column_type
        logical :: is_set = .false.
        logical :: is_deactivated = .false. ! true for columns merged in from a base MAML that the user's MAML excluded;
                                          ! protects is_set from being changed by set_available/set_unavailable (bulk or by name).
        logical :: is_protected = .false. ! true if this column's name is listed under extra: protected_cols: in
                                           ! whichever MAML built this cinfo; parquet_write_column error stops if an
                                           ! is_valid mask with any .false. entry is passed for such a column.
        logical :: has_qc_min = .false.
        logical :: has_qc_max = .false.
        character(len=2) :: qc_min_op = ">=" ! one of ">", ">="; default when qc: min: has no operator prefix.
        character(len=2) :: qc_max_op = "<=" ! one of "<", "<="; default when qc: max: has no operator prefix.
        character(len=:), allocatable :: qc_min_raw ! qc: min: bound text, operator prefix already stripped/trimmed;
                                                     ! numeric for int32/int64/float32/float64, literal for string.
        character(len=:), allocatable :: qc_max_raw ! qc: max: bound text, same convention as qc_min_raw.
        character(len=:), allocatable :: name      ! The name of the field [required]; always the internal/canonical
                                                    ! name, i.e. what parquet_write_column/set_available/etc. use --
                                                    ! never affected by a col_map: rename (see output_name).
        character(len=:), allocatable :: unit      ! The unit of measurement for the field.
        character(len=:), allocatable :: info      ! A short description of the field.
        character(len=:), allocatable :: ucd       ! Unified Content Descriptor for IVOA (can have many).
        character(len=:), allocatable :: data_type ! The data type of the field [required].
        integer :: array_size = 1 ! Maximum length of character strings.
        integer :: col_size = 1   ! The number of elements in the vector column.
        character(len=:), allocatable :: output_name ! The name actually written to the parquet file/VOTable header.
                                                       ! Equal to `name` unless a col_map: entry in the MAML that
                                                       ! declared this field renamed it (col_map: maps
                                                       ! internal_name -> output_name; `name` is then set to the
                                                       ! internal_name and `output_name` keeps the field's own
                                                       ! declared name from that MAML's fields: section).
    end type parquet_column_type

    type parquet_column_info
        type(parquet_column_type), allocatable :: col(:)
    contains
        procedure :: get_column_index
        procedure :: set_unavailable
        procedure :: set_available
    end type parquet_column_info

    type parquet_metadata_entry
        character(len=:), allocatable :: key
        character(len=:), allocatable :: value
        character(len=:), allocatable :: description
    end type parquet_metadata_entry

    type parquet_table_metadata
        type(parquet_metadata_entry), allocatable :: items(:)
        ! Verbatim source MAML lines, populated by parquet_read_maml; used by
        ! parquet_open_writer(..., write_maml=.true.) to save a sidecar .maml
        ! file next to the .parquet output. add_metadata calls made after
        ! parquet_read_maml append a new keyarray: entry to these lines (see
        ! parquet_append_keyarray_line), so the sidecar reflects them; it is
        ! NOT kept in sync with which columns end up enabled/written, though.
        character(len=:), allocatable :: source_maml_lines(:)
    contains
        procedure :: add_metadata_int32
        procedure :: add_metadata_int64
        procedure :: add_metadata_float32
        procedure :: add_metadata_float64
        procedure :: add_metadata_logical
        procedure :: add_metadata_string
        procedure :: add_metadata_int32_array
        procedure :: add_metadata_int64_array
        procedure :: add_metadata_float32_array
        procedure :: add_metadata_float64_array
        procedure :: add_metadata_logical_array
        procedure :: add_metadata_string_array
        generic :: add_metadata => add_metadata_int32, add_metadata_int64, add_metadata_float32, &
                                    add_metadata_float64, add_metadata_logical, add_metadata_string, &
                                    add_metadata_int32_array, add_metadata_int64_array, add_metadata_float32_array, &
                                    add_metadata_float64_array, add_metadata_logical_array, add_metadata_string_array
    end type parquet_table_metadata

    ! Bundles a parsed MAML source together with the column schema (cinfo)
    ! and table metadata (metadata) that parquet_parse_maml derives from it,
    ! so a single variable carries everything parquet_open_writer needs.
    ! %cinfo and %metadata are public: their fields (col(:), items(:)) and
    ! own type-bound procedures stay directly reachable, and the procedures
    ! below are flat convenience passthroughs (schema%set_column_available("id")
    ! instead of schema%cinfo%set_available("id")).
    !
    ! On the read side a qc-maml is populated into %maml alone -- either by
    ! parquet_load_qc_maml_file or schema%add_col_qc -- and is never run
    ! through parquet_parse_maml (a qc-maml has no data_type and would fail
    ! the full schema validation), so %cinfo/%metadata stay empty;
    ! parquet_open_reader consumes only %maml.
    type parquet_schema
        type(parquet_maml_file)      :: maml
        type(parquet_column_info)    :: cinfo
        type(parquet_table_metadata) :: metadata
    contains
        procedure :: set_column_available
        procedure :: set_column_unavailable
        procedure :: get_column_index => schema_get_column_index
        procedure :: add_col_qc => schema_add_col_qc
        procedure :: get_col_qc => schema_get_col_qc
        procedure :: schema_add_metadata_int32
        procedure :: schema_add_metadata_int64
        procedure :: schema_add_metadata_float32
        procedure :: schema_add_metadata_float64
        procedure :: schema_add_metadata_logical
        procedure :: schema_add_metadata_string
        procedure :: schema_add_metadata_int32_array
        procedure :: schema_add_metadata_int64_array
        procedure :: schema_add_metadata_float32_array
        procedure :: schema_add_metadata_float64_array
        procedure :: schema_add_metadata_logical_array
        procedure :: schema_add_metadata_string_array
        generic :: add_metadata => schema_add_metadata_int32, schema_add_metadata_int64, &
                                    schema_add_metadata_float32, schema_add_metadata_float64, &
                                    schema_add_metadata_logical, schema_add_metadata_string, &
                                    schema_add_metadata_int32_array, schema_add_metadata_int64_array, &
                                    schema_add_metadata_float32_array, schema_add_metadata_float64_array, &
                                    schema_add_metadata_logical_array, schema_add_metadata_string_array
    end type parquet_schema

    ! parquet_writer/parquet_reader own a handle to a C++-side Arrow/Parquet
    ! object with no automatic Fortran cleanup. Always prefer an explicit
    ! parquet_close_writer/parquet_close_reader call; the FINAL procedures
    ! below are only a safety net for a handle that's still open when its
    ! variable goes out of scope or is overwritten (e.g. an early RETURN
    ! between open and close), not a substitute for closing normally.
    !
    ! Do not copy a parquet_writer/parquet_reader (`w2 = w1`, passing one as
    ! a function result, etc.): the handle is a plain c_ptr, so a copy
    ! aliases the same underlying C++ object without any reference counting.
    ! Whichever copy is finalized/closed first frees it out from under the
    ! other, which would then double-free/use-after-free when it is itself
    ! later closed or finalized. Always use a single named writer/reader,
    ! passed by reference (as every procedure in this module already does).
    ! Fully opaque from outside this module: every component is an
    ! implementation detail (the raw C handle, schema bookkeeping, write
    ! tracking) that parquet_write_column/parquet_open_writer/etc. manage
    ! internally. Never referenced directly by any test or consumer -- only
    ! parquet.f90's own submodules (parquet_write, parquet_read,
    ! parquet_metadata) need access, which `private` here still allows.
    type parquet_writer
        private
        type(c_ptr) :: handle = c_null_ptr
        type(parquet_column_type), allocatable :: all_columns(:)
        type(parquet_column_type), allocatable :: enabled_columns(:)
        integer, allocatable :: write_counts(:)
        logical :: is_schema_enforced = .false.
        logical :: qc = .false. ! set from parquet_open_writer(..., qc=); when true, parquet_write_column checks
                                 ! each column's qc: min/max (if declared) against its valid (is_valid) elements
                                 ! and prints a WARNING (never an error) on violation. No-op without a schema.
        integer(c_long_long) :: expected_nrows = -1 ! set by the first parquet_write_column call; every later
                                 ! call must supply this same row count (see parquet_check_row_count), since
                                 ! Arrow/Parquet requires every column in a table to have equal length.
        character(len=256), allocatable :: written_names(:) ! Schema-less writer only (no cinfo, so
                                 ! enabled_columns/write_counts below don't exist): every name
                                 ! parquet_write_column has already written, so a repeat can still be
                                 ! caught -- see parquet_mark_column_written.
        character(len=:), allocatable :: maml_name ! Set from schema%maml%name by parquet_open_writer
                                 ! when a schema is given (unallocated for a schema-less writer, or if
                                 ! the schema's own %maml%name was never set); solely so
                                 ! parquet_write_column's "column not defined"/"type mismatch" errors
                                 ! can name which maml the schema came from -- see writer_maml_suffix.
    contains
        final :: writer_finalize
    end type parquet_writer

    type parquet_reader
        private
        type(c_ptr) :: handle = c_null_ptr
        ! Set by parquet_open_reader from its own `filename` argument, solely
        ! so parquet_get_nrows(..., check_positive=.true.) can name the file
        ! in its error message; not used for anything else.
        character(len=:), allocatable :: filename
        ! Flat key-value table metadata (whatever add_metadata wrote on the
        ! write side), copied once by parquet_open_reader from the underlying
        ! Arrow schema's KeyValueMetadata -- see parquet_reader_get_table_metadata_count
        ! and friends in parquet_bindings.f90. Every parquet_get_metadata call
        ! only scans this in-memory copy; it never re-reads the file. %description
        ! is never populated on this side (Arrow's flat KV store has no
        ! description field), only %key/%value.
        type(parquet_table_metadata) :: metadata
    contains
        final :: reader_finalize
    end type parquet_reader

    ! A row filter for parquet_open_reader: each %add call is one AND-combined
    ! clause, "<column> <op> [value]" (e.g. "ra > 180", "id is_not_null"),
    ! validated (column exists, is a scalar column, value is well-formed for
    ! that column's type) once parquet_open_reader actually applies it --
    ! %add itself just accumulates the raw rule text. See "Row filtering with
    ! parquet_filter" in the README for the full rule syntax.
    type parquet_filter
        character(len=512), allocatable :: rules(:)
        integer :: n = 0
    contains
        procedure :: add => parquet_filter_add
    end type parquet_filter

    ! Internal plumbing only (not part of the public API): one column's
    ! read-time QC declaration, parsed from a qc-maml's fields: entries by
    ! parquet_parse_qc_maml. min_text/max_text carry the qc: min:/max: value
    ! verbatim (operator prefix, if any, already stripped and captured in
    ! min_op/max_op by parquet_set_qc_bound) -- parsed against the actual
    ! Arrow column type only when a check runs, in parquet_wrapper.cpp's
    ! run_qc_range_check, never against a maml-declared data_type (this
    ! maml doesn't even require one). See "Row filtering" for the analogous
    ! convention parquet_filter already uses for its own rule values.
    type :: parquet_qc_rule
        character(len=64) :: name = ""
        ! True only if this field had a qc: sub-block at all (even an empty
        ! one) -- a field with just a name: and no qc: gets no rule at all,
        ! the same as a field never mentioned in the maml (see
        ! parquet_parse_qc_maml); apply_parquet_qc filters these out before
        ! ever reaching parquet_reader_set_qc.
        logical :: has_qc_block = .false.
        logical :: has_min = .false.
        character(len=2) :: min_op = ">="
        character(len=256) :: min_text = ""
        logical :: has_max = .false.
        character(len=2) :: max_op = "<="
        character(len=256) :: max_text = ""
        logical :: null_values_allowed = .false.
    end type parquet_qc_rule

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

    interface parquet_read_array_row_mode
        module procedure parquet_read_int32_array_row_mode
        module procedure parquet_read_int64_array_row_mode
        module procedure parquet_read_float32_array_row_mode
        module procedure parquet_read_float64_array_row_mode
        module procedure parquet_read_logical_array_row_mode
        module procedure parquet_read_string_array_row_mode
    end interface parquet_read_array_row_mode

    interface parquet_read_array_element_mode
        module procedure parquet_read_int32_array_element_mode
        module procedure parquet_read_int64_array_element_mode
        module procedure parquet_read_float32_array_element_mode
        module procedure parquet_read_float64_array_element_mode
        module procedure parquet_read_logical_array_element_mode
        module procedure parquet_read_string_array_element_mode
    end interface parquet_read_array_element_mode

    interface parquet_get_nrows
        module procedure parquet_get_nrows_int64
        module procedure parquet_get_nrows_int32
    end interface parquet_get_nrows

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

    interface parquet_get_column_total_elements
        module procedure parquet_get_column_total_elements_int64
        module procedure parquet_get_column_total_elements_int32
    end interface parquet_get_column_total_elements

    ! parquet_prefetch_columns accepts either an array of column names (each
    ! element sharing one declared length -- pad shorter names with blanks) or
    ! a single scalar string listing the names separated by commas and/or
    ! semicolons ("ra;dec,mag"). The scalar form avoids the fixed-length array
    ! pitfall where a too-short declared length silently truncates a name.
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
        module integer function parquet_get_enabled_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_enabled_column_index

        module integer function parquet_get_defined_column_index(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_defined_column_index

        module logical function parquet_is_type_compatible(actual_type, expected_type)
            character(len=*), intent(in) :: actual_type
            character(len=*), intent(in) :: expected_type
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
            character(len=*), intent(in) :: raw
            character(len=*), intent(in) :: data_type
            real(real64), intent(out) :: value
        end function parquet_qc_numeric_bound

        module subroutine parquet_assert_column_type(writer, name, expected_type)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: expected_type
        end subroutine parquet_assert_column_type

        module subroutine parquet_mark_column_written(writer, name)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
        end subroutine parquet_mark_column_written

        module logical function parquet_is_column_enabled(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_is_column_enabled

        module integer function parquet_get_column_col_size(writer, name)
            type(parquet_writer), intent(in) :: writer
            character(len=*), intent(in) :: name
        end function parquet_get_column_col_size

        !> Every column in a file must have the same number of rows (Arrow/Parquet
        !> requirement). Called by every parquet_write_column variant with that
        !> call's own row count: the first call for a given writer fixes the
        !> expected row count, every later call must match it or error stop.
        module subroutine parquet_check_row_count(writer, name, nrows)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(c_long_long), intent(in) :: nrows
        end subroutine parquet_check_row_count

        module subroutine parquet_open_writer(writer, filename, schema, write_maml, qc, &
                compression, compression_level, chunk_size, use_threads)
            type(parquet_writer), intent(out) :: writer
            character(len=*), intent(in) :: filename
            type(parquet_schema), intent(in), optional :: schema
            logical, intent(in), optional :: write_maml
            logical, intent(in), optional :: qc
            character(len=*), intent(in), optional :: compression
            integer, intent(in), optional :: compression_level
            integer, intent(in), optional :: chunk_size
            logical, intent(in), optional :: use_threads
        end subroutine parquet_open_writer

        module subroutine parquet_add_column_info(writer, name, unit, description, ucd, data_type, array_size, col_size)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: unit
            character(len=*), intent(in) :: description
            character(len=*), intent(in) :: ucd
            character(len=*), intent(in) :: data_type
            integer, intent(in) :: array_size
            integer, intent(in) :: col_size
        end subroutine parquet_add_column_info

        module subroutine parquet_write_int32_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int32), intent(in) :: values(:)
            logical, intent(in), optional :: is_valid(:)
        end subroutine parquet_write_int32_column

        module subroutine parquet_write_int32_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int32), intent(in) :: values(:,:)
            logical, intent(in), optional :: is_valid(:,:)
        end subroutine parquet_write_int32_matrix_column

        module subroutine parquet_write_int64_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: values(:)
            logical, intent(in), optional :: is_valid(:)
        end subroutine parquet_write_int64_column

        module subroutine parquet_write_int64_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: values(:,:)
            logical, intent(in), optional :: is_valid(:,:)
        end subroutine parquet_write_int64_matrix_column

        module subroutine parquet_write_float32_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real32), intent(in) :: values(:)
            logical, intent(in), optional :: is_valid(:)
        end subroutine parquet_write_float32_column

        module subroutine parquet_write_float32_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real32), intent(in) :: values(:,:)
            logical, intent(in), optional :: is_valid(:,:)
        end subroutine parquet_write_float32_matrix_column

        module subroutine parquet_write_float64_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real64), intent(in) :: values(:)
            logical, intent(in), optional :: is_valid(:)
        end subroutine parquet_write_float64_column

        module subroutine parquet_write_float64_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            real(real64), intent(in) :: values(:,:)
            logical, intent(in), optional :: is_valid(:,:)
        end subroutine parquet_write_float64_matrix_column

        module subroutine parquet_write_logical_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            logical, intent(in) :: values(:)
            logical, intent(in), optional :: is_valid(:)
        end subroutine parquet_write_logical_column

        module subroutine parquet_write_logical_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            logical, intent(in) :: values(:,:)
            logical, intent(in), optional :: is_valid(:,:)
        end subroutine parquet_write_logical_matrix_column

        module subroutine parquet_write_string_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: values(:)
            logical, intent(in), optional :: is_valid(:)
        end subroutine parquet_write_string_column

        module subroutine parquet_write_string_matrix_column(writer, name, values, is_valid)
            type(parquet_writer), intent(inout) :: writer
            character(len=*), intent(in) :: name
            character(len=*), intent(in) :: values(:,:)
            logical, intent(in), optional :: is_valid(:,:)
        end subroutine parquet_write_string_matrix_column

        module subroutine parquet_close_writer(writer)
            type(parquet_writer), intent(inout) :: writer
        end subroutine parquet_close_writer

        module subroutine writer_finalize(this)
            type(parquet_writer), intent(inout) :: this
        end subroutine writer_finalize

        module subroutine parquet_parse_maml_from_file(filename, schema)
            character(len=*), intent(in) :: filename
            type(parquet_schema), intent(out) :: schema
        end subroutine parquet_parse_maml_from_file

        module function parquet_load_maml_file(filename) result(maml)
            character(len=*), intent(in) :: filename
            type(parquet_maml_file) :: maml
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
            character(len=*), intent(in) :: filename
            type(parquet_schema) :: schema
        end function parquet_load_qc_maml_file

        !> Validates and parses a qc-maml's fields: entries into `rules`, one
        !> entry per field that has at least a name (data_type/unit/ucd/etc.
        !> are irrelevant here and never required) -- error stops on a
        !> duplicate field name, an unknown top-level section/sub-key, or an
        !> unrecognized qc: miss: value (anything other than Null/NA,
        !> case-insensitive, or empty). Table-level metadata is ignored
        !> entirely. See parquet_qc_rule's own doc comment for min_text/max_text.
        module subroutine parquet_parse_qc_maml(maml, rules)
            type(parquet_maml_file), intent(in) :: maml
            type(parquet_qc_rule), allocatable, intent(out) :: rules(:)
        end subroutine parquet_parse_qc_maml

        module subroutine parquet_validate_user_maml(base_maml, user_maml)
            type(parquet_maml_file), intent(in) :: base_maml
            type(parquet_maml_file), intent(inout) :: user_maml
        end subroutine parquet_validate_user_maml

        module subroutine parquet_validate_maml_internal(maml)
            type(parquet_maml_file), intent(in) :: maml
        end subroutine parquet_validate_maml_internal

        module subroutine parquet_validate_maml_file(maml)
            character(len=*), intent(in) :: maml
        end subroutine parquet_validate_maml_file

        module subroutine parquet_parse_maml_from_object(schema)
            type(parquet_schema), intent(inout) :: schema
        end subroutine parquet_parse_maml_from_object

        ! Flat convenience passthroughs on parquet_schema -- each forwards to
        ! the matching procedure on %cinfo, %metadata or %maml.
        module subroutine set_column_available(this, name)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in), optional :: name
        end subroutine set_column_available

        module subroutine set_column_unavailable(this, name)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in), optional :: name
        end subroutine set_column_unavailable

        module integer function schema_get_column_index(this, name)
            class(parquet_schema), intent(in) :: this
            character(len=*), intent(in) :: name
        end function schema_get_column_index

        module subroutine schema_add_col_qc(this, qc_input, col_name)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: qc_input
            character(len=:), allocatable, intent(out), optional :: col_name
        end subroutine schema_add_col_qc

        module function schema_get_col_qc(this, qc_input) result(col_name)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: qc_input
            character(len=:), allocatable :: col_name
        end function schema_get_col_qc

        module subroutine schema_add_metadata_int32(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_int32

        module subroutine schema_add_metadata_int64(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_int64

        module subroutine schema_add_metadata_float32(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: value
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine schema_add_metadata_float32

        module subroutine schema_add_metadata_float64(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: value
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine schema_add_metadata_float64

        module subroutine schema_add_metadata_logical(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_logical

        module subroutine schema_add_metadata_string(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_string

        module subroutine schema_add_metadata_int32_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_int32_array

        module subroutine schema_add_metadata_int64_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_int64_array

        module subroutine schema_add_metadata_float32_array(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine schema_add_metadata_float32_array

        module subroutine schema_add_metadata_float64_array(this, key, value, description, fmt)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine schema_add_metadata_float64_array

        module subroutine schema_add_metadata_logical_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_logical_array

        module subroutine schema_add_metadata_string_array(this, key, value, description)
            class(parquet_schema), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine schema_add_metadata_string_array

        module subroutine parquet_parse_maml_lines(lines, cinfo, metadata)
            character(len=*), intent(in) :: lines(:)
            type(parquet_column_info), intent(out) :: cinfo
            type(parquet_table_metadata), intent(out) :: metadata
        end subroutine parquet_parse_maml_lines

        module integer function get_column_index(this, name)
            class(parquet_column_info), intent(in) :: this
            character(len=*), intent(in) :: name
        end function get_column_index

        module subroutine set_unavailable(this, name)
            class(parquet_column_info), intent(inout) :: this
            character(len=*), intent(in), optional :: name
        end subroutine set_unavailable

        module subroutine set_available(this, name)
            class(parquet_column_info), intent(inout) :: this
            character(len=*), intent(in), optional :: name
        end subroutine set_available

        module subroutine parquet_append_line(lines, n, line)
            character(len=1024), allocatable, intent(inout) :: lines(:)
            integer, intent(in) :: n
            character(len=*), intent(in) :: line
        end subroutine parquet_append_line

        module subroutine parquet_metadata_append_entry(metadata, key, value, description)
            class(parquet_table_metadata), intent(inout) :: metadata
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine parquet_metadata_append_entry

        module subroutine add_metadata_int32(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_int32

        module subroutine add_metadata_int64(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_int64

        module subroutine add_metadata_float32(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: value
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float32

        module subroutine add_metadata_float64(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: value
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float64

        module subroutine add_metadata_logical(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_logical

        module subroutine add_metadata_string(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_string

        module subroutine add_metadata_int32_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int32), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_int32_array

        module subroutine add_metadata_int64_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            integer(int64), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_int64_array

        module subroutine add_metadata_float32_array(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real32), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float32_array

        module subroutine add_metadata_float64_array(this, key, value, description, fmt)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            real(real64), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
            character(len=*), intent(in), optional :: fmt
        end subroutine add_metadata_float64_array

        module subroutine add_metadata_logical_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            logical, intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_logical_array

        module subroutine add_metadata_string_array(this, key, value, description)
            class(parquet_table_metadata), intent(inout) :: this
            character(len=*), intent(in) :: key
            character(len=*), intent(in) :: value(:)
            character(len=*), intent(in), optional :: description
        end subroutine add_metadata_string_array

        module subroutine parquet_append_empty_cinfo(columns, n)
            type(parquet_column_type), allocatable, intent(inout) :: columns(:)
            integer, intent(inout) :: n
        end subroutine parquet_append_empty_cinfo

        module subroutine parquet_split_key_value(line, key, value)
            character(len=*), intent(in) :: line
            character(len=:), allocatable, intent(out) :: key
            character(len=:), allocatable, intent(out) :: value
        end subroutine parquet_split_key_value

        module function parquet_unquote(s) result(out)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: out
        end function parquet_unquote

        module function parquet_to_lower(s) result(out)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: out
        end function parquet_to_lower

        module subroutine parquet_open_reader(reader, filename, use_threads, filter, schema, qc, qc_soft)
            type(parquet_reader), intent(out) :: reader
            character(len=*), intent(in) :: filename
            logical, intent(in), optional :: use_threads
            type(parquet_filter), intent(in), optional :: filter
            type(parquet_schema), intent(in), optional :: schema
            logical, intent(in), optional :: qc
            logical, intent(in), optional :: qc_soft
        end subroutine parquet_open_reader

        module subroutine parquet_close_reader(reader, print_stat)
            type(parquet_reader), intent(inout) :: reader
            logical, intent(in), optional :: print_stat
        end subroutine parquet_close_reader

        module subroutine parquet_prefetch_columns_array(reader, names)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: names(:)
        end subroutine parquet_prefetch_columns_array

        module subroutine parquet_prefetch_columns_string(reader, names)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: names
        end subroutine parquet_prefetch_columns_string

        module subroutine reader_finalize(this)
            type(parquet_reader), intent(inout) :: this
        end subroutine reader_finalize

        module subroutine parquet_get_nrows_int64(reader, nrows, check_positive)
            type(parquet_reader), intent(in) :: reader
            integer(int64), intent(out) :: nrows
            logical, intent(in), optional :: check_positive
        end subroutine parquet_get_nrows_int64

        module subroutine parquet_get_nrows_int32(reader, nrows, check_positive)
            type(parquet_reader), intent(in) :: reader
            integer(int32), intent(out) :: nrows
            logical, intent(in), optional :: check_positive
        end subroutine parquet_get_nrows_int32

        module subroutine parquet_get_col_size(reader, name, col_size)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer, intent(out) :: col_size
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
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(c_long_long), intent(in) :: given_nrows
        end subroutine parquet_check_read_row_count

        module subroutine parquet_get_column_total_elements_int64(reader, name, total_elements)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: total_elements
        end subroutine parquet_get_column_total_elements_int64

        module subroutine parquet_get_column_total_elements_int32(reader, name, total_elements)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: total_elements
        end subroutine parquet_get_column_total_elements_int32

        module subroutine parquet_get_string_length(reader, name, max_string_length)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer, intent(out) :: max_string_length
        end subroutine parquet_get_string_length

        module subroutine parquet_get_metadata_int32(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            integer(int32), intent(out) :: value
            integer(int32), intent(in), optional :: default
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_int32

        module subroutine parquet_get_metadata_int64(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            integer(int64), intent(out) :: value
            integer(int64), intent(in), optional :: default
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_int64

        module subroutine parquet_get_metadata_float32(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            real(real32), intent(out) :: value
            real(real32), intent(in), optional :: default
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_float32

        module subroutine parquet_get_metadata_float64(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            real(real64), intent(out) :: value
            real(real64), intent(in), optional :: default
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_float64

        module subroutine parquet_get_metadata_logical(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            logical, intent(out) :: value
            logical, intent(in), optional :: default
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_logical

        module subroutine parquet_get_metadata_string(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            character(len=:), allocatable, intent(out) :: value
            character(len=*), intent(in), optional :: default
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_string

        module subroutine parquet_get_metadata_int32_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            integer(int32), allocatable, intent(out) :: value(:)
            integer(int32), intent(in), optional :: default(:)
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_int32_array

        module subroutine parquet_get_metadata_int64_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            integer(int64), allocatable, intent(out) :: value(:)
            integer(int64), intent(in), optional :: default(:)
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_int64_array

        module subroutine parquet_get_metadata_float32_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            real(real32), allocatable, intent(out) :: value(:)
            real(real32), intent(in), optional :: default(:)
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_float32_array

        module subroutine parquet_get_metadata_float64_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            real(real64), allocatable, intent(out) :: value(:)
            real(real64), intent(in), optional :: default(:)
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_float64_array

        module subroutine parquet_get_metadata_logical_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            logical, allocatable, intent(out) :: value(:)
            logical, intent(in), optional :: default(:)
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_logical_array

        module subroutine parquet_get_metadata_string_array(reader, key, value, default, warn)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: key
            character(len=:), allocatable, intent(out) :: value(:)
            character(len=*), intent(in), optional :: default(:)
            logical, intent(in), optional :: warn
        end subroutine parquet_get_metadata_string_array

        module subroutine parquet_read_int32_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:)
            integer(int32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_int32_column_1d

        module subroutine parquet_read_int64_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:)
            integer(int64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_int64_column_1d

        module subroutine parquet_read_float32_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:)
            real(real32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_float32_column_1d

        module subroutine parquet_read_float64_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:)
            real(real64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_float64_column_1d

        module subroutine parquet_read_logical_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:)
            logical, intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_logical_column_1d

        module subroutine parquet_read_string_column_1d(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:)
            character(len=*), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_string_column_1d

        module subroutine parquet_read_int32_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:, :)
            integer(int32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:, :)
        end subroutine parquet_read_int32_array_full

        module subroutine parquet_read_int64_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:, :)
            integer(int64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:, :)
        end subroutine parquet_read_int64_array_full

        module subroutine parquet_read_float32_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:, :)
            real(real32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:, :)
        end subroutine parquet_read_float32_array_full

        module subroutine parquet_read_float64_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:, :)
            real(real64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:, :)
        end subroutine parquet_read_float64_array_full

        module subroutine parquet_read_logical_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:, :)
            logical, intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:, :)
        end subroutine parquet_read_logical_array_full

        module subroutine parquet_read_string_array_full(reader, name, values, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:, :)
            character(len=*), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:, :)
        end subroutine parquet_read_string_array_full

        module subroutine parquet_read_int32_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:)
            integer, intent(in) :: row_index
            integer(int32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_int32_array_row_mode

        module subroutine parquet_read_int64_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:)
            integer, intent(in) :: row_index
            integer(int64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_int64_array_row_mode

        module subroutine parquet_read_float32_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:)
            integer, intent(in) :: row_index
            real(real32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_float32_array_row_mode

        module subroutine parquet_read_float64_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:)
            integer, intent(in) :: row_index
            real(real64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_float64_array_row_mode

        module subroutine parquet_read_logical_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:)
            integer, intent(in) :: row_index
            logical, intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_logical_array_row_mode

        module subroutine parquet_read_string_array_row_mode(reader, name, values, row_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:)
            integer, intent(in) :: row_index
            character(len=*), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_string_array_row_mode

        module subroutine parquet_read_int32_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int32), intent(out) :: values(:)
            integer, intent(in) :: elem_index
            integer(int32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_int32_array_element_mode

        module subroutine parquet_read_int64_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            integer(int64), intent(out) :: values(:)
            integer, intent(in) :: elem_index
            integer(int64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_int64_array_element_mode

        module subroutine parquet_read_float32_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real32), intent(out) :: values(:)
            integer, intent(in) :: elem_index
            real(real32), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_float32_array_element_mode

        module subroutine parquet_read_float64_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            real(real64), intent(out) :: values(:)
            integer, intent(in) :: elem_index
            real(real64), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_float64_array_element_mode

        module subroutine parquet_read_logical_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            logical, intent(out) :: values(:)
            integer, intent(in) :: elem_index
            logical, intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_logical_array_element_mode

        module subroutine parquet_read_string_array_element_mode(reader, name, values, elem_index, null_value, is_valid)
            type(parquet_reader), intent(in) :: reader
            character(len=*), intent(in) :: name
            character(len=*), intent(out) :: values(:)
            integer, intent(in) :: elem_index
            character(len=*), intent(in), optional :: null_value
            logical, intent(out), optional :: is_valid(:)
        end subroutine parquet_read_string_array_element_mode
    end interface

contains

    subroutine parquet_get_version(ver_string, internal)
        implicit none
        character(len=:), allocatable, intent(out) :: ver_string
        logical, intent(in), optional :: internal
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
        if (cversion(2:i-1) /= ver_string) then
            write(*,*) "WARNING: using developmentparquet-fortran library!"
            write(*,*) "         library version: ", trim(cversion)
            write(*,*) "         RELEASE_VERSION: ", trim(ver_string)
        end if
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
        integer, intent(in) :: n

        if (n < 1) error stop "parquet_set_max_threads: n must be >= 1"
        call parquet_set_thread_pool_capacity(int(n, kind=c_int))
    end subroutine parquet_set_max_threads

    subroutine parquet_filter_add(this, rule)
        class(parquet_filter), intent(inout) :: this
        character(len=*), intent(in) :: rule
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
