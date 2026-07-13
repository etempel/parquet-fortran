!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> A thin extern "C" interop shim over src/parquet_wrapper.cpp -- it is not
!> meant to be `use`d directly by consuming projects (use the `parquet`
!> module instead, which wraps every one of these in a proper Fortran API
!> with schema validation, type dispatch, etc.). Every procedure here
!> happens to still need to be public, since `parquet`'s own submodules
!> (parquet_write/parquet_read/parquet_metadata) call them directly -- but
!> that's now an explicit, reviewable list rather than an accidental default.
!> Excluded from FORD generation (see fpm.toml); doc-comments below are for
!> source readers only.
module parquet_bindings
    use iso_c_binding
    implicit none
    private

    public :: create_parquet_writer, create_parquet_reader
    public :: close_parquet_writer, close_parquet_reader
    public :: parquet_reader_print_stat
    public :: parquet_set_writer_options
    public :: parquet_set_thread_pool_capacity
    public :: parquet_maml_lock, parquet_maml_unlock
    public :: parquet_warmup_memory_pool
    public :: parquet_add_column_metadata, parquet_add_table_metadata
    public :: parquet_append_int32_column, parquet_append_int64_column
    public :: parquet_append_float32_column, parquet_append_float64_column
    public :: parquet_append_bool8_column
    public :: parquet_append_string_column, parquet_append_string_array_column
    public :: parquet_reader_get_nrows, parquet_reader_get_total_nrows, parquet_reader_get_column_col_size
    public :: parquet_reader_get_column_total_elements, parquet_reader_get_string_length
    public :: parquet_reader_get_table_metadata_count
    public :: parquet_reader_get_table_metadata_key_length, parquet_reader_get_table_metadata_value_length
    public :: parquet_reader_get_table_metadata_key, parquet_reader_get_table_metadata_value
    public :: parquet_reader_prefetch_columns, parquet_reader_prefetch_all_columns, parquet_reader_has_column
    public :: parquet_reader_set_filter
    public :: parquet_reader_set_qc
    public :: parquet_read_int32_column, parquet_read_int64_column
    public :: parquet_read_float32_column, parquet_read_float64_column
    public :: parquet_read_bool8_column, parquet_read_string_column
    public :: parquet_read_int32_array_column, parquet_read_int64_array_column
    public :: parquet_read_float32_array_column, parquet_read_float64_array_column
    public :: parquet_read_bool8_array_column, parquet_read_string_array_column
    public :: parquet_read_int32_array_row, parquet_read_int64_array_row
    public :: parquet_read_float32_array_row, parquet_read_float64_array_row
    public :: parquet_read_bool8_array_row, parquet_read_string_array_row
    public :: parquet_read_int32_array_element, parquet_read_int64_array_element
    public :: parquet_read_float32_array_element, parquet_read_float64_array_element
    public :: parquet_read_bool8_array_element, parquet_read_string_array_element

    interface
        !> Creates a new parquet writer for `filename` and returns its opaque handle.
        function create_parquet_writer(filename) &
                bind(C, name="create_parquet_writer") result(writer)
            import
            character(kind=c_char) :: filename(*)
            type(c_ptr) :: writer
        end function

        !> Sets compression codec/level, row-group chunk size, and threading on `writer`.
        subroutine parquet_set_writer_options(writer, compression_name, compression_level, chunk_size, use_threads) &
                bind(C, name="parquet_set_writer_options")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: compression_name(*)
            integer(c_int), value :: compression_level
            integer(c_long_long), value :: chunk_size
            integer(c_int), value :: use_threads
        end subroutine

        !> Resizes Arrow's global CPU thread pool to `n` threads.
        subroutine parquet_set_thread_pool_capacity(n) &
                bind(C, name="parquet_set_max_threads")
            import
            integer(c_int), value :: n
        end subroutine

        !> Acquires the process-wide mutex guarding non-reentrant Fortran-side
        !> MAML mutation (see g_maml_mutex in parquet_wrapper.cpp).
        subroutine parquet_maml_lock() bind(C, name="parquet_maml_lock")
        end subroutine

        !> Releases the mutex acquired by parquet_maml_lock.
        subroutine parquet_maml_unlock() bind(C, name="parquet_maml_unlock")
        end subroutine

        !> Forces arrow::default_memory_pool()'s lazy singleton to be
        !> constructed now, single-threaded. See parquet_warmup_memory_pool
        !> in parquet_wrapper.cpp for why: its first call is not safely
        !> reentrant in every Arrow build, and test-drive runs tests within a
        !> suite concurrently via OpenMP.
        subroutine parquet_warmup_memory_pool() bind(C, name="parquet_warmup_memory_pool")
        end subroutine

        !> Creates a new parquet reader for `filename` and returns its opaque handle.
        function create_parquet_reader(filename, use_threads) &
                bind(C, name="create_parquet_reader") result(reader)
            import
            character(kind=c_char) :: filename(*)
            integer(c_int), value :: use_threads
            type(c_ptr) :: reader
        end function

        !> Declares one column's schema metadata on a schema-less `writer`.
        subroutine parquet_add_column_metadata(writer, name, unit, description, ucd, data_type, array_size, col_size) &
                bind(C, name="parquet_add_column_metadata")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: unit(*)
            character(kind=c_char) :: description(*)
            character(kind=c_char) :: ucd(*)
            character(kind=c_char) :: data_type(*)
            integer(c_long_long), value :: array_size
            integer(c_long_long), value :: col_size
        end subroutine

        !> Adds one flat key-value table metadata entry to `writer`.
        subroutine parquet_add_table_metadata(writer, key, value, description) &
                bind(C, name="parquet_add_table_metadata")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: key(*)
            character(kind=c_char) :: value(*)
            character(kind=c_char) :: description(*)
        end subroutine

        !> Appends one int32 column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_int32_column(writer, name, data, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_int32_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends one int64 column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_int64_column(writer, name, data, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_int64_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends one float32 column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_float32_column(writer, name, data, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_float32_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends one float64 column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_float64_column(writer, name, data, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_float64_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends one boolean (bool8) column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_bool8_column(writer, name, data, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_bool8_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends one scalar string column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_string_column(writer, name, data, item_len, nrows, valid_in) &
                bind(C, name="parquet_append_string_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends one vector string column's values (with optional validity mask) to `writer`.
        subroutine parquet_append_string_array_column(writer, name, data, item_len, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_string_array_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Flushes and closes `writer`, freeing the underlying C++ object.
        subroutine close_parquet_writer(writer) &
                bind(C, name="close_parquet_writer")
            import
            type(c_ptr), value :: writer
        end subroutine

        !> Closes `reader`, freeing the underlying C++ object.
        subroutine close_parquet_reader(reader) &
                bind(C, name="close_parquet_reader")
            import
            type(c_ptr), value :: reader
        end subroutine

        !> Prints a debug/diagnostic summary of `reader`'s activity to stdout.
        subroutine parquet_reader_print_stat(reader) &
                bind(C, name="parquet_reader_print_stat")
            import
            type(c_ptr), value :: reader
        end subroutine

        !> Returns `reader`'s post-filter row count.
        function parquet_reader_get_nrows(reader) &
                bind(C, name="parquet_reader_get_nrows") result(nrows)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: nrows
        end function

        !> Returns `reader`'s unfiltered (total) row count.
        function parquet_reader_get_total_nrows(reader) &
                bind(C, name="parquet_reader_get_total_nrows") result(total_nrows)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: total_nrows
        end function

        !> Reads and caches the named columns of `reader` immediately.
        subroutine parquet_reader_prefetch_columns(reader, names_packed, item_len, n) &
                bind(C, name="parquet_reader_prefetch_columns")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: names_packed(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: n
        end subroutine

        !> Reads and caches every column of `reader` immediately.
        subroutine parquet_reader_prefetch_all_columns(reader) &
                bind(C, name="parquet_reader_prefetch_all_columns")
            import
            type(c_ptr), value :: reader
        end subroutine

        !> Returns non-zero if `name` is a column of `reader` (non-throwing existence check).
        function parquet_reader_has_column(reader, name) &
                bind(C, name="parquet_reader_has_column") result(has_column)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: has_column
        end function

        !> Validates and applies a packed row filter to `reader`; returns
        !> non-zero and writes a message to `err_out` on failure.
        function parquet_reader_set_filter(reader, names_packed, name_len, ops_packed, op_len, &
                values_packed, value_len, is_string_flags, n, err_out, err_cap) &
                bind(C, name="parquet_reader_set_filter") result(status)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: names_packed(*)
            integer(c_long_long), value :: name_len
            character(kind=c_char) :: ops_packed(*)
            integer(c_long_long), value :: op_len
            character(kind=c_char) :: values_packed(*)
            integer(c_long_long), value :: value_len
            integer(c_int8_t) :: is_string_flags(*)
            integer(c_long_long), value :: n
            character(kind=c_char) :: err_out(*)
            integer(c_long_long), value :: err_cap
            integer(c_long_long) :: status
        end function

        !> Installs packed per-column qc: min/max/miss rules on `reader`.
        subroutine parquet_reader_set_qc(reader, names_packed, name_len, &
                has_min_flags, min_ops_packed, min_op_len, min_values_packed, min_value_len, &
                has_max_flags, max_ops_packed, max_op_len, max_values_packed, max_value_len, &
                null_allowed_flags, n, qc_soft) &
                bind(C, name="parquet_reader_set_qc")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: names_packed(*)
            integer(c_long_long), value :: name_len
            integer(c_int8_t) :: has_min_flags(*)
            character(kind=c_char) :: min_ops_packed(*)
            integer(c_long_long), value :: min_op_len
            character(kind=c_char) :: min_values_packed(*)
            integer(c_long_long), value :: min_value_len
            integer(c_int8_t) :: has_max_flags(*)
            character(kind=c_char) :: max_ops_packed(*)
            integer(c_long_long), value :: max_op_len
            character(kind=c_char) :: max_values_packed(*)
            integer(c_long_long), value :: max_value_len
            integer(c_int8_t) :: null_allowed_flags(*)
            integer(c_long_long), value :: n
            integer(c_int8_t), value :: qc_soft
        end subroutine

        !> Returns the declared vector-column element count of `name` in `reader`.
        function parquet_reader_get_column_col_size(reader, name) &
                bind(C, name="parquet_reader_get_column_col_size") result(col_size)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: col_size
        end function

        !> Returns the total element count of vector column `name` across every row in `reader`.
        function parquet_reader_get_column_total_elements(reader, name) &
                bind(C, name="parquet_reader_get_column_total_elements") result(nelem)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: nelem
        end function

        !> Returns the longest string value actually present in column `name`.
        function parquet_reader_get_string_length(reader, name) &
                bind(C, name="parquet_reader_get_string_length") result(strlen_max)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: strlen_max
        end function

        !> Returns the number of flat key-value table metadata entries in `reader`.
        function parquet_reader_get_table_metadata_count(reader) &
                bind(C, name="parquet_reader_get_table_metadata_count") result(count)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: count
        end function

        !> Returns the byte length of table metadata entry `index`'s key.
        function parquet_reader_get_table_metadata_key_length(reader, index) &
                bind(C, name="parquet_reader_get_table_metadata_key_length") result(strlen)
            import
            type(c_ptr), value :: reader
            integer(c_long_long), value :: index
            integer(c_long_long) :: strlen
        end function

        !> Returns the byte length of table metadata entry `index`'s value.
        function parquet_reader_get_table_metadata_value_length(reader, index) &
                bind(C, name="parquet_reader_get_table_metadata_value_length") result(strlen)
            import
            type(c_ptr), value :: reader
            integer(c_long_long), value :: index
            integer(c_long_long) :: strlen
        end function

        !> Copies table metadata entry `index`'s key into `buf`.
        subroutine parquet_reader_get_table_metadata_key(reader, index, buf, buf_len) &
                bind(C, name="parquet_reader_get_table_metadata_key")
            import
            type(c_ptr), value :: reader
            integer(c_long_long), value :: index
            character(kind=c_char) :: buf(*)
            integer(c_long_long), value :: buf_len
        end subroutine

        !> Copies table metadata entry `index`'s value into `buf`.
        subroutine parquet_reader_get_table_metadata_value(reader, index, buf, buf_len) &
                bind(C, name="parquet_reader_get_table_metadata_value")
            import
            type(c_ptr), value :: reader
            integer(c_long_long), value :: index
            character(kind=c_char) :: buf(*)
            integer(c_long_long), value :: buf_len
        end subroutine

        !> Reads scalar int32 column `name` from `reader` into `data` (with optional validity mask).
        subroutine parquet_read_int32_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_int32_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads scalar int64 column `name` from `reader` into `data` (with optional validity mask).
        subroutine parquet_read_int64_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_int64_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads scalar float32 column `name` from `reader` into `data` (with optional validity mask).
        subroutine parquet_read_float32_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_float32_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads scalar float64 column `name` from `reader` into `data` (with optional validity mask).
        subroutine parquet_read_float64_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_float64_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads scalar boolean (bool8) column `name` from `reader` into `data` (with optional validity mask).
        subroutine parquet_read_bool8_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_bool8_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads scalar string column `name` from `reader` into `data` (with optional validity mask).
        subroutine parquet_read_string_column(reader, name, data, item_len, nrows, valid_out) &
                bind(C, name="parquet_read_string_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads the full vector int32 column `name` (every row) from `reader` into `data`.
        subroutine parquet_read_int32_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_int32_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads the full vector int64 column `name` (every row) from `reader` into `data`.
        subroutine parquet_read_int64_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_int64_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads the full vector float32 column `name` (every row) from `reader` into `data`.
        subroutine parquet_read_float32_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_float32_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads the full vector float64 column `name` (every row) from `reader` into `data`.
        subroutine parquet_read_float64_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_float64_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads the full vector boolean (bool8) column `name` (every row) from `reader` into `data`.
        subroutine parquet_read_bool8_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_bool8_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads the full vector string column `name` (every row) from `reader` into `data`.
        subroutine parquet_read_string_array_column(reader, name, data, item_len, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_string_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row (`row_index`) of vector int32 column `name` from `reader` into `data`.
        subroutine parquet_read_int32_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_int32_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row (`row_index`) of vector int64 column `name` from `reader` into `data`.
        subroutine parquet_read_int64_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_int64_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row (`row_index`) of vector float32 column `name` from `reader` into `data`.
        subroutine parquet_read_float32_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_float32_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            real(c_float) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row (`row_index`) of vector float64 column `name` from `reader` into `data`.
        subroutine parquet_read_float64_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_float64_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            real(c_double) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row (`row_index`) of vector boolean (bool8) column `name` from `reader` into `data`.
        subroutine parquet_read_bool8_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_bool8_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row (`row_index`) of vector string column `name` from `reader` into `data`.
        subroutine parquet_read_string_array_row(reader, name, row_index, data, item_len, col_size, valid_out) &
                bind(C, name="parquet_read_string_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position (`col_index`) of vector int32 column `name`
        !> across every row from `reader` into `data`.
        subroutine parquet_read_int32_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_int32_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position (`col_index`) of vector int64 column `name`
        !> across every row from `reader` into `data`.
        subroutine parquet_read_int64_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_int64_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position (`col_index`) of vector float32 column `name`
        !> across every row from `reader` into `data`.
        subroutine parquet_read_float32_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_float32_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position (`col_index`) of vector float64 column `name`
        !> across every row from `reader` into `data`.
        subroutine parquet_read_float64_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_float64_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position (`col_index`) of vector boolean (bool8)
        !> column `name` across every row from `reader` into `data`.
        subroutine parquet_read_bool8_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_bool8_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position (`col_index`) of vector string column `name`
        !> across every row from `reader` into `data`.
        subroutine parquet_read_string_array_element(reader, name, col_index, data, item_len, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_string_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine
    end interface

end module parquet_bindings ! GCOVR_EXCL_LINE
