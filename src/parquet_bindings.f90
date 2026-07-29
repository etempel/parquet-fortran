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
    public :: abandon_parquet_writer
    public :: parquet_reader_print_stat
    public :: parquet_set_writer_options
    public :: parquet_set_thread_pool_capacity
    public :: parquet_get_arrow_version, parquet_get_parquet_version
    public :: parquet_maml_lock, parquet_maml_unlock
    public :: parquet_warmup_memory_pool
    public :: parquet_add_column_metadata, parquet_add_table_metadata
    public :: parquet_update_column_metadata_size
    public :: parquet_append_int32_column, parquet_append_int64_column
    public :: parquet_append_float32_column, parquet_append_float64_column
    public :: parquet_append_bool8_column
    public :: parquet_append_string_column, parquet_append_string_array_column
    public :: parquet_append_string_column_buffers
    ! Local (Fortran-side) names deliberately differ from their bind(C, name="...") C++ symbol,
    ! same reason parquet_append_int32_column (this binding) differs from parquet_write_column
    ! (the public generic parquet.f90 exposes for it): parquet_write.f90 is a submodule of
    ! parquet, which use-associates this whole module -- a public Fortran wrapper reusing the
    ! exact same name as the C symbol it wraps would collide with that use association.
    public :: parquet_writer_new_row_group, parquet_writer_finish_row_group, parquet_writer_get_chunk_size
    public :: parquet_append_int32_column_chunk, parquet_append_int64_column_chunk
    public :: parquet_append_float32_column_chunk, parquet_append_float64_column_chunk
    public :: parquet_append_bool8_column_chunk
    public :: parquet_append_string_column_chunk, parquet_append_string_array_column_chunk
    public :: parquet_write_string_column_chunk_buffers
    public :: parquet_reader_get_nrows, parquet_reader_get_total_nrows, parquet_reader_get_column_col_size
    public :: parquet_reader_get_column_total_elements, parquet_reader_get_string_length
    public :: parquet_reader_get_table_metadata_count
    public :: parquet_reader_get_table_metadata_key_length, parquet_reader_get_table_metadata_value_length
    public :: parquet_reader_get_table_metadata_key, parquet_reader_get_table_metadata_value
    public :: parquet_reader_get_column_count, parquet_reader_get_column_name_length
    public :: parquet_reader_get_column_name, parquet_reader_release_column
    public :: parquet_reader_prefetch_columns, parquet_reader_prefetch_all_columns, parquet_reader_has_column
    public :: parquet_reader_get_column_type_name
    public :: parquet_reader_set_filter
    public :: parquet_reader_set_sample
    public :: parquet_reader_set_qc
    public :: parquet_read_int32_column, parquet_read_int64_column
    public :: parquet_read_float32_column, parquet_read_float64_column
    public :: parquet_read_bool8_column, parquet_read_string_column
    public :: parquet_read_int32_array_column, parquet_read_int64_array_column
    public :: parquet_read_float32_array_column, parquet_read_float64_array_column
    public :: parquet_read_bool8_array_column, parquet_read_string_array_column
    public :: parquet_read_string_column_buffers
    public :: parquet_read_int32_array_row, parquet_read_int64_array_row
    public :: parquet_read_float32_array_row, parquet_read_float64_array_row
    public :: parquet_read_bool8_array_row, parquet_read_string_array_row
    public :: parquet_read_int32_array_element, parquet_read_int64_array_element
    public :: parquet_read_float32_array_element, parquet_read_float64_array_element
    public :: parquet_read_bool8_array_element, parquet_read_string_array_element
    public :: parquet_reader_get_num_row_groups, parquet_reader_has_filter, parquet_reader_get_chunk_size_at
    public :: parquet_reader_check_complete
    public :: parquet_read_int32_column_chunk, parquet_read_int64_column_chunk
    public :: parquet_read_float32_column_chunk, parquet_read_float64_column_chunk
    public :: parquet_read_bool8_column_chunk, parquet_read_string_column_chunk
    public :: parquet_read_int32_array_column_chunk, parquet_read_int64_array_column_chunk
    public :: parquet_read_float32_array_column_chunk, parquet_read_float64_array_column_chunk
    public :: parquet_read_bool8_array_column_chunk, parquet_read_string_array_column_chunk
    public :: parquet_read_string_column_chunk_buffers
    ! Temporal (parquet_temporal: date/time/timestamp) bindings.
    public :: parquet_append_date_column, parquet_append_time_column, parquet_append_timestamp_column
    public :: parquet_append_date_column_chunk, parquet_append_time_column_chunk, parquet_append_timestamp_column_chunk
    public :: parquet_read_date_column, parquet_read_time_column, parquet_read_timestamp_column
    public :: parquet_read_date_array_column, parquet_read_time_array_column, parquet_read_timestamp_array_column
    public :: parquet_read_date_column_chunk, parquet_read_time_column_chunk, parquet_read_timestamp_column_chunk
    public :: parquet_read_date_array_column_chunk, parquet_read_time_array_column_chunk
    public :: parquet_read_timestamp_array_column_chunk
    public :: parquet_read_date_array_row, parquet_read_time_array_row, parquet_read_timestamp_array_row
    public :: parquet_read_date_array_element, parquet_read_time_array_element, parquet_read_timestamp_array_element
    public :: parquet_reader_get_column_time_unit
    public :: parquet_reader_get_column_timezone_length, parquet_reader_get_column_timezone

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

        !> Reports the actually-linked Arrow library's runtime version.
        subroutine parquet_get_arrow_version(major, minor, patch) &
                bind(C, name="parquet_get_arrow_version")
            import
            integer(c_int), intent(out) :: major !! major version number.
            integer(c_int), intent(out) :: minor !! minor version number.
            integer(c_int), intent(out) :: patch !! patch version number.
        end subroutine

        !> Reports the compile-time Parquet C++ library version.
        subroutine parquet_get_parquet_version(major, minor, patch) &
                bind(C, name="parquet_get_parquet_version")
            import
            integer(c_int), intent(out) :: major !! major version number.
            integer(c_int), intent(out) :: minor !! minor version number.
            integer(c_int), intent(out) :: patch !! patch version number.
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

        !> Updates an already-declared column's col_size/array_size in place (a no-op if `name`
        !> isn't found); used to resolve a col_size:/array_size: auto placeholder from the first
        !> write call's own data shape.
        subroutine parquet_update_column_metadata_size(writer, name, col_size, array_size) &
                bind(C, name="parquet_update_column_metadata_size")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_size
            integer(c_long_long), value :: array_size
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

        !> Appends one scalar string column straight from a parquet_string_column's own raw
        !> buffers (see parquet_strings.f90's raw_buffers) -- the compact counterpart to
        !> parquet_append_string_column, avoiding the fixed-width padded intermediate entirely.
        !> `offsets` is nrows+1 int64 values (0-based, offsets(1)=0); `data` is nchars bytes;
        !> `validity` is a bit-packed Arrow-style bitmap (LSB-first, 1=valid), or c_null_ptr when
        !> the column has no nulls. Always builds arrow::large_utf8(), matching the source
        !> column's own int64-offset storage.
        subroutine parquet_append_string_column_buffers(writer, name, nrows, nchars, offsets, data, validity) &
                bind(C, name="parquet_append_string_column_buffers")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: nchars
            type(c_ptr), value :: offsets
            type(c_ptr), value :: data
            type(c_ptr), value :: validity
        end subroutine

        !> Starts a new row group of `nrows` rows on `writer` -- every column already known must
        !> then receive exactly one parquet_append_*_column_chunk call (or already be a whole
        !> column written via parquet_append_*_column) before parquet_writer_finish_row_group.
        subroutine parquet_writer_new_row_group(writer, nrows) &
                bind(C, name="parquet_new_row_group")
            import
            type(c_ptr), value :: writer
            integer(c_long_long), value :: nrows
        end subroutine

        !> Writes one int32 column's chunk (scalar or, for col_size > 1, fixed-size-list) for the
        !> currently-open row group -- nrows is implicit (the row group's own size).
        subroutine parquet_append_int32_column_chunk(writer, name, data, col_size, valid_in) &
                bind(C, name="parquet_write_int32_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Same as parquet_append_int32_column_chunk, but for int64.
        subroutine parquet_append_int64_column_chunk(writer, name, data, col_size, valid_in) &
                bind(C, name="parquet_write_int64_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Same as parquet_append_int32_column_chunk, but for float32.
        subroutine parquet_append_float32_column_chunk(writer, name, data, col_size, valid_in) &
                bind(C, name="parquet_write_float32_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            real(c_float) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Same as parquet_append_int32_column_chunk, but for float64.
        subroutine parquet_append_float64_column_chunk(writer, name, data, col_size, valid_in) &
                bind(C, name="parquet_write_float64_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            real(c_double) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Same as parquet_append_int32_column_chunk, but for boolean (bool8).
        subroutine parquet_append_bool8_column_chunk(writer, name, data, col_size, valid_in) &
                bind(C, name="parquet_write_bool8_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Writes one scalar string column's chunk for the currently-open row group -- always
        !> uses arrow::large_utf8() (see parquet_wrapper.cpp's own comment on this function).
        subroutine parquet_append_string_column_chunk(writer, name, data, item_len, valid_in) &
                bind(C, name="parquet_write_string_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            type(c_ptr), value :: valid_in
        end subroutine

        !> Writes one vector string column's chunk for the currently-open row group -- always
        !> uses arrow::large_utf8(), same as parquet_append_string_column_chunk above.
        subroutine parquet_append_string_array_column_chunk(writer, name, data, item_len, col_size, valid_in) &
                bind(C, name="parquet_write_string_array_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Writes one scalar string column's chunk straight from a parquet_string_column's own
        !> raw buffers -- the compact counterpart to parquet_append_string_column_chunk. Same
        !> buffer contract as parquet_append_string_column_buffers above; `nrows` must equal the
        !> currently-open row group's own row count (checked Fortran-side before this is called).
        !> Always uses arrow::large_utf8(), same as parquet_append_string_column_chunk.
        subroutine parquet_write_string_column_chunk_buffers(writer, name, nrows, nchars, offsets, data, validity) &
                bind(C, name="parquet_write_string_column_chunk_buffers")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: nchars
            type(c_ptr), value :: offsets
            type(c_ptr), value :: data
            type(c_ptr), value :: validity
        end subroutine

        !> Ends the currently-open row group, verifying every known column received data for it
        !> and, on the very first call, locking the file's schema and opening it for writing.
        subroutine parquet_writer_finish_row_group(writer) &
                bind(C, name="parquet_finish_row_group")
            import
            type(c_ptr), value :: writer
        end subroutine

        !> Returns `writer`'s resolved/authoritative row-group size (see resolve_chunk_size in
        !> parquet_wrapper.cpp) -- usable at any point after parquet_open_writer, including
        !> before any column has been written.
        function parquet_writer_get_chunk_size(writer) &
                bind(C, name="parquet_writer_get_chunk_size") result(chunk_size)
            import
            type(c_ptr), value :: writer
            integer(c_long_long) :: chunk_size
        end function

        !> Flushes and closes `writer`, freeing the underlying C++ object.
        subroutine close_parquet_writer(writer) &
                bind(C, name="close_parquet_writer")
            import
            type(c_ptr), value :: writer
        end subroutine

        !> Frees `writer` without finalizing/writing its output -- see writer_finalize
        !> (parquet_write.f90) for why the implicit-finalizer safety net uses this instead of
        !> close_parquet_writer.
        subroutine abandon_parquet_writer(writer) &
                bind(C, name="abandon_parquet_writer")
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

        !> Writes `name`'s canonical data-type token into `buf` (space-padded to buf_len) and
        !> returns 1, if its physical type is one of int32/int64/float32/float64/boolean/string/
        !> date/time/timestamp; otherwise writes a raw Arrow type description into `buf` and
        !> returns 0.
        function parquet_reader_get_column_type_name(reader, name, buf, buf_len) &
                bind(C, name="parquet_reader_get_column_type_name") result(recognized)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: buf(*)
            integer(c_long_long), value :: buf_len
            integer(c_long_long) :: recognized
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

        !> Applies Bernoulli(sample_fraction) row sampling to `reader` (see parquet_reader_set_sample
        !> in parquet_wrapper.cpp); returns non-zero and writes a message to `err_out` on failure.
        !> actual_seed_out is always filled with the seed actually used (caller-supplied via seed/
        !> has_seed, or, when has_seed is 0 or seed <= 0, freshly drawn from entropy). filter_will_follow
        !> (non-zero when the caller's own filter= will also be applied right after this): defers
        !> installing the draw as the reader's active mask (stashed in pending_sample_mask instead)
        !> so the filter's own clause evaluation still sees raw, unmasked column data -- see
        !> parquet_reader_set_sample's own comment in parquet_wrapper.cpp.
        function parquet_reader_set_sample(reader, sample_fraction, seed, has_seed, filter_will_follow, &
                actual_seed_out, err_out, err_cap) &
                bind(C, name="parquet_reader_set_sample") result(status)
            import
            type(c_ptr), value :: reader
            real(c_double), value :: sample_fraction
            integer(c_int32_t), value :: seed
            integer(c_int8_t), value :: has_seed
            integer(c_int8_t), value :: filter_will_follow
            integer(c_int32_t) :: actual_seed_out
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

        !> Returns the number of addressable (leaf-path) columns in `reader`'s schema.
        function parquet_reader_get_column_count(reader) &
                bind(C, name="parquet_reader_get_column_count") result(count)
            import
            type(c_ptr), value :: reader
            integer(c_int32_t) :: count
        end function

        !> Returns the byte length of column `index`'s (possibly dotted) name. `index` is 0-based.
        function parquet_reader_get_column_name_length(reader, index) &
                bind(C, name="parquet_reader_get_column_name_length") result(strlen)
            import
            type(c_ptr), value :: reader
            integer(c_int32_t), value :: index
            integer(c_long_long) :: strlen
        end function

        !> Copies column `index`'s (possibly dotted) name into `buf`. `index` is 0-based.
        subroutine parquet_reader_get_column_name(reader, index, buf, buf_len) &
                bind(C, name="parquet_reader_get_column_name")
            import
            type(c_ptr), value :: reader
            integer(c_int32_t), value :: index
            character(kind=c_char) :: buf(*)
            integer(c_long_long), value :: buf_len
        end subroutine

        !> Drops column `name`'s decoded Arrow array from the reader's cache, freeing its buffers.
        subroutine parquet_reader_release_column(reader, name) &
                bind(C, name="parquet_reader_release_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
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

        !> Reads scalar string column `name` from `reader` as its own raw offsets/data/validity
        !> buffers -- the compact counterpart to parquet_read_string_column, for handing straight
        !> to parquet_string_column's append_buffers instead of copying one string at a time into
        !> a fixed-width padded buffer. All six output arguments are plain (non-`value`) dummies,
        !> so they are passed by reference/address, exactly like a C `T *` out-parameter -- the
        !> only such outputs in this module (every other binding's outputs are pre-sized arrays
        !> the caller allocates first). The returned pointers reference memory owned by `reader`
        !> (the file's own decoded/cached column array, or -- for a dotted struct-field path -- a
        !> freshly built array the reader now pins for exactly this purpose) and stay valid only
        !> until the next call into this same reader; the caller (parquet_read.f90) is expected to
        !> consume them (append_buffers) immediately, before making any other call on `reader`.
        !> `offsets_int32` is 1 when `offsets` holds int32 values (a plain STRING column), 0 for
        !> int64 (LARGE_STRING) -- see parquet_strings.f90's append_buffers, which accepts both.
        !> `validity_offset` is the source array's own element offset (usually 0; can be nonzero
        !> for a struct-nested leaf) -- append_buffers needs it to correctly align `validity`'s
        !> bit 0, which (unlike `offsets`/`data`) Arrow never pre-rebases for a sliced array.
        subroutine parquet_read_string_column_buffers(reader, name, nrows, nchars, offsets, data, validity, &
                offsets_int32, validity_offset) bind(C, name="parquet_read_string_column_buffers")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), intent(out) :: nrows
            integer(c_long_long), intent(out) :: nchars
            type(c_ptr), intent(out) :: offsets
            type(c_ptr), intent(out) :: data
            type(c_ptr), intent(out) :: validity
            integer(c_int8_t), intent(out) :: offsets_int32
            integer(c_long_long), intent(out) :: validity_offset
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

        !> Returns `reader`'s row-group count (file-physical, unaffected by any filter).
        function parquet_reader_get_num_row_groups(reader) &
                bind(C, name="parquet_reader_get_num_row_groups") result(num_row_groups)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: num_row_groups
        end function

        !> Returns non-zero if `reader` was opened with an active row filter and/or random
        !> downsample (sample_fraction < 1.0) -- both share the same underlying mask
        !> (see filter_mask in parquet_wrapper.cpp), so this can't distinguish which was used.
        function parquet_reader_has_filter(reader) &
                bind(C, name="parquet_reader_has_filter") result(has_filter)
            import
            type(c_ptr), value :: reader
            integer(c_int) :: has_filter
        end function

        !> Returns the physical row count of `reader`'s row group `row_group` (1-based, already
        !> validated/resolved by the caller).
        function parquet_reader_get_chunk_size_at(reader, row_group) &
                bind(C, name="parquet_reader_get_chunk_size_at") result(chunk_size)
            import
            type(c_ptr), value :: reader
            integer(c_long_long), value :: row_group
            integer(c_long_long) :: chunk_size
        end function

        !> Checks that every column read via parquet_read_*_column_chunk had every row group
        !> read by now; hard/=0 aborts on an incomplete column, else warns once per column.
        subroutine parquet_reader_check_complete(reader, hard) &
                bind(C, name="parquet_reader_check_complete")
            import
            type(c_ptr), value :: reader
            integer(c_int), value :: hard
        end subroutine

        !> Reads row group `row_group` of scalar int32 column `name` from `reader` into `data`.
        subroutine parquet_read_int32_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_int32_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_column_chunk, but for int64.
        subroutine parquet_read_int64_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_int64_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_column_chunk, but for float32.
        subroutine parquet_read_float32_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_float32_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_column_chunk, but for float64.
        subroutine parquet_read_float64_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_float64_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_column_chunk, but for boolean (bool8).
        subroutine parquet_read_bool8_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_bool8_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_column_chunk, but for string (fixed-width, space-padded).
        subroutine parquet_read_string_column_chunk(reader, name, row_group, data, item_len, nrows, valid_out) &
                bind(C, name="parquet_read_string_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads row group `row_group` of vector int32 column `name` from `reader` into `data`.
        subroutine parquet_read_int32_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_int32_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_array_column_chunk, but for int64.
        subroutine parquet_read_int64_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_int64_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_array_column_chunk, but for float32.
        subroutine parquet_read_float32_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_float32_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            real(c_float) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_array_column_chunk, but for float64.
        subroutine parquet_read_float64_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_float64_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            real(c_double) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_array_column_chunk, but for boolean (bool8).
        subroutine parquet_read_bool8_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_bool8_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int8_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Same as parquet_read_int32_array_column_chunk, but for string (fixed-width, space-padded).
        subroutine parquet_read_string_array_column_chunk(reader, name, row_group, data, item_len, nrows, col_size, &
                valid_out) &
                bind(C, name="parquet_read_string_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            character(kind=c_char) :: data(*)
            integer(c_long_long), value :: item_len
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads row group `row_group`'s worth of scalar string column `name` as its own raw
        !> offsets/data/validity buffers -- the compact counterpart to
        !> parquet_read_string_column_chunk. Same by-reference output convention and buffer
        !> lifetime as parquet_read_string_column_buffers above; the returned pointers are only
        !> valid until the next call into this same `reader`. `validity_offset` is the source
        !> array's own element offset -- see parquet_read_string_column_buffers's own doc comment.
        subroutine parquet_read_string_column_chunk_buffers(reader, name, row_group, nrows, nchars, offsets, data, &
                validity, offsets_int32, validity_offset) bind(C, name="parquet_read_string_column_chunk_buffers")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_long_long), intent(out) :: nrows
            integer(c_long_long), intent(out) :: nchars
            type(c_ptr), intent(out) :: offsets
            type(c_ptr), intent(out) :: data
            type(c_ptr), intent(out) :: validity
            integer(c_int8_t), intent(out) :: offsets_int32
            integer(c_long_long), intent(out) :: validity_offset
        end subroutine

        ! ================================================================================
        ! Temporal (parquet_temporal: date/time/timestamp) bindings. See the transport note
        ! in src/parquet_wrapper.cpp (convert_date_values / build_time_array): date crosses as
        ! int32 days, time as int64 canonical nanoseconds-of-day, timestamp as an int64 value
        ! in a per-column `unit` selector (1=s, 2=ms, 3=us, 4=ns, matching parquet_temporal's
        ! parquet_unit_* constants) plus an `is_utc` flag on write / no tz on read (the tz is a
        ! separate parquet_reader_get_column_timezone query).
        ! ================================================================================

        !> Appends a date column (scalar or col_size>1 vector) of int32 days since 1970-01-01.
        subroutine parquet_append_date_column(writer, name, data, nrows, col_size, valid_in) &
                bind(C, name="parquet_append_date_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends a time column of canonical int64 nanoseconds-of-day, stored in file `unit`.
        subroutine parquet_append_time_column(writer, name, data, nrows, col_size, unit, valid_in) &
                bind(C, name="parquet_append_time_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            integer(c_int32_t), value :: unit
            type(c_ptr), value :: valid_in
        end subroutine

        !> Appends a timestamp column of int64 values already in file `unit`; `is_utc`/=0 marks
        !> the column UTC-adjusted (else timezone-naive).
        subroutine parquet_append_timestamp_column(writer, name, data, nrows, col_size, unit, is_utc, valid_in) &
                bind(C, name="parquet_append_timestamp_column")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            integer(c_int32_t), value :: unit
            integer(c_int32_t), value :: is_utc
            type(c_ptr), value :: valid_in
        end subroutine

        !> Streaming (row-group-chunked) date append; col_size>1 is a vector column.
        subroutine parquet_append_date_column_chunk(writer, name, data, col_size, valid_in) &
                bind(C, name="parquet_write_date_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_in
        end subroutine

        !> Streaming (row-group-chunked) time append, canonical ns-of-day stored in file `unit`.
        subroutine parquet_append_time_column_chunk(writer, name, data, col_size, unit, valid_in) &
                bind(C, name="parquet_write_time_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: col_size
            integer(c_int32_t), value :: unit
            type(c_ptr), value :: valid_in
        end subroutine

        !> Streaming (row-group-chunked) timestamp append; `data` already in file `unit`.
        subroutine parquet_append_timestamp_column_chunk(writer, name, data, col_size, unit, is_utc, valid_in) &
                bind(C, name="parquet_write_timestamp_column_chunk")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: col_size
            integer(c_int32_t), value :: unit
            integer(c_int32_t), value :: is_utc
            type(c_ptr), value :: valid_in
        end subroutine

        !> Reads a scalar date column into `data` (int32 days since 1970-01-01).
        subroutine parquet_read_date_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_date_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads a scalar time column into `data` (canonical int64 nanoseconds-of-day).
        subroutine parquet_read_time_column(reader, name, data, nrows, valid_out) &
                bind(C, name="parquet_read_time_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads a scalar timestamp column into `data` (int64 in the column's own unit),
        !> reporting that unit selector via `unit_out`.
        subroutine parquet_read_timestamp_column(reader, name, data, nrows, unit_out, valid_out) &
                bind(C, name="parquet_read_timestamp_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_int32_t), intent(out) :: unit_out
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads a whole vector date column into `data` (flattened, col_size per row).
        subroutine parquet_read_date_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_date_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads a whole vector time column into `data`.
        subroutine parquet_read_time_array_column(reader, name, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_time_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads a whole vector timestamp column into `data`, reporting the unit via `unit_out`.
        subroutine parquet_read_timestamp_array_column(reader, name, data, nrows, col_size, unit_out, valid_out) &
                bind(C, name="parquet_read_timestamp_array_column")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            integer(c_int32_t), intent(out) :: unit_out
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row group of a scalar date column.
        subroutine parquet_read_date_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_date_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row group of a scalar time column.
        subroutine parquet_read_time_column_chunk(reader, name, row_group, data, nrows, valid_out) &
                bind(C, name="parquet_read_time_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row group of a scalar timestamp column, reporting the unit via `unit_out`.
        subroutine parquet_read_timestamp_column_chunk(reader, name, row_group, data, nrows, unit_out, valid_out) &
                bind(C, name="parquet_read_timestamp_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_int32_t), intent(out) :: unit_out
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row group of a vector date column.
        subroutine parquet_read_date_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_date_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row group of a vector time column.
        subroutine parquet_read_time_array_column_chunk(reader, name, row_group, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_time_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row group of a vector timestamp column, reporting the unit via `unit_out`.
        subroutine parquet_read_timestamp_array_column_chunk(reader, name, row_group, data, nrows, col_size, unit_out, valid_out) &
                bind(C, name="parquet_read_timestamp_array_column_chunk")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_group
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            integer(c_int32_t), intent(out) :: unit_out
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row's element vector of a vector date column.
        subroutine parquet_read_date_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_date_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row's element vector of a vector time column.
        subroutine parquet_read_time_array_row(reader, name, row_index, data, col_size, valid_out) &
                bind(C, name="parquet_read_time_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one row's element vector of a vector timestamp column (unit via `unit_out`).
        subroutine parquet_read_timestamp_array_row(reader, name, row_index, data, col_size, unit_out, valid_out) &
                bind(C, name="parquet_read_timestamp_array_row")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: row_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: col_size
            integer(c_int32_t), intent(out) :: unit_out
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position of a vector date column across all rows. `col_size` is
        !> accepted for binding symmetry with the numeric element readers but ignored by C
        !> (element mode resolves it internally).
        subroutine parquet_read_date_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_date_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int32_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position of a vector time column across all rows (col_size ignored).
        subroutine parquet_read_time_array_element(reader, name, col_index, data, nrows, col_size, valid_out) &
                bind(C, name="parquet_read_time_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            type(c_ptr), value :: valid_out
        end subroutine

        !> Reads one element position of a vector timestamp column across all rows (col_size
        !> ignored), reporting the unit via `unit_out`.
        subroutine parquet_read_timestamp_array_element(reader, name, col_index, data, nrows, col_size, unit_out, valid_out) &
                bind(C, name="parquet_read_timestamp_array_element")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: col_index
            integer(c_int64_t) :: data(*)
            integer(c_long_long), value :: nrows
            integer(c_long_long), value :: col_size
            integer(c_int32_t), intent(out) :: unit_out
            type(c_ptr), value :: valid_out
        end subroutine

        !> Returns the parquet_unit_* selector (1..4) of a time/timestamp column `name`.
        function parquet_reader_get_column_time_unit(reader, name) &
                bind(C, name="parquet_reader_get_column_time_unit") result(unit)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int32_t) :: unit
        end function

        !> Byte length of a timestamp column's timezone string (0 for naive / a time column).
        function parquet_reader_get_column_timezone_length(reader, name) &
                bind(C, name="parquet_reader_get_column_timezone_length") result(strlen)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long) :: strlen
        end function

        !> Copies a timestamp column's timezone string into `buf`.
        subroutine parquet_reader_get_column_timezone(reader, name, buf, buf_len) &
                bind(C, name="parquet_reader_get_column_timezone")
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            character(kind=c_char) :: buf(*)
            integer(c_long_long), value :: buf_len
        end subroutine
    end interface

end module parquet_bindings ! GCOVR_EXCL_LINE
