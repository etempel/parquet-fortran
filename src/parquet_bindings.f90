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
    public :: parquet_writer_enter, parquet_writer_leave
    public :: abandon_parquet_writer
    public :: parquet_reader_print_stat
    public :: parquet_set_writer_options
    public :: parquet_set_thread_pool_capacity, parquet_get_thread_pool_capacity
    public :: parquet_push_output_settings
    public :: parquet_push_performance_settings
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
    ! (the public generic parquet_core.f90 exposes for it): parquet_write.f90 is a submodule of
    ! parquet, which use-associates this whole module -- a public Fortran wrapper reusing the
    ! exact same name as the C symbol it wraps would collide with that use association.
    public :: parquet_writer_new_row_group, parquet_writer_finish_row_group, parquet_writer_get_chunk_size
    public :: parquet_append_int32_column_chunk, parquet_append_int64_column_chunk
    public :: parquet_append_float32_column_chunk, parquet_append_float64_column_chunk
    public :: parquet_append_bool8_column_chunk
    public :: parquet_append_string_column_chunk, parquet_append_string_array_column_chunk
    public :: parquet_write_string_column_chunk_buffers
    public :: parquet_reader_get_nrows, parquet_reader_get_total_nrows, parquet_reader_get_column_col_size
    public :: parquet_reader_column_width_is_deferred, parquet_reader_column_has_nulls
    public :: parquet_reader_list_width_candidate, parquet_reader_list_width_verified
    public :: parquet_reader_get_column_total_elements, parquet_reader_get_string_length
    public :: parquet_reader_physical_row_indices
    public :: parquet_reader_get_table_metadata_count
    public :: parquet_reader_get_table_metadata_key_length, parquet_reader_get_table_metadata_value_length
    public :: parquet_reader_get_table_metadata_key, parquet_reader_get_table_metadata_value
    public :: parquet_reader_get_column_count, parquet_reader_get_column_name_length
    public :: parquet_reader_get_column_name, parquet_reader_release_column
    public :: parquet_reader_prefetch_columns, parquet_reader_prefetch_all_columns, parquet_reader_has_column
    public :: parquet_reader_get_column_type_name, parquet_reader_get_column_nullable
    public :: parquet_writer_set_protected_column
    public :: c_reader_set_filter, parquet_reader_has_decoded_columns, parquet_reader_has_filter_clauses
    public :: parquet_reader_has_chunk_reads
    public :: parquet_reader_has_sort
    public :: parquet_reader_sort_key_info, parquet_reader_sort_key_fetch, parquet_reader_sort_install
    public :: parquet_sort_builder_new, parquet_sort_builder_add_key_int64
    public :: parquet_sort_builder_add_key_double, parquet_sort_builder_add_key_string
    public :: parquet_sort_builder_build, parquet_sort_builder_is_sorted, parquet_sort_builder_free
    public :: parquet_sort_builder_build_partial, parquet_sort_builder_nth_element
    public :: parquet_sort_builder_build_runs, parquet_sort_builder_search
    public :: parquet_sort_builder_merge
    public :: parquet_sort_partial_argsort_int64, parquet_sort_partial_argsort_double
    public :: parquet_sort_partial_argsort_string
    public :: parquet_sort_nth_index_int64, parquet_sort_nth_index_double, parquet_sort_nth_index_string
    public :: parquet_sort_argsort_int64, parquet_sort_argsort_double, parquet_sort_argsort_string
    public :: parquet_sort_is_sorted_int64, parquet_sort_is_sorted_double, parquet_sort_is_sorted_string
    public :: parquet_reader_set_sample
    public :: c_reader_adopt_transform
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

        !> Claims `writer`'s concurrency guard for the calling thread until parquet_writer_leave,
        !> aborting if another thread already holds it. Reached only through parquet_core.f90's
        !> writer_lock, never called directly -- see the C++ side for why the Fortran half of a
        !> write has to hold this guard rather than leaving it to the append call at the end.
        subroutine parquet_writer_enter(writer) &
                bind(C, name="parquet_writer_enter")
            import
            type(c_ptr), value :: writer
        end subroutine

        !> Drops one level of the ownership parquet_writer_enter claimed; a no-op on a writer this
        !> thread does not hold, so writer_lock's FINAL can run unconditionally.
        subroutine parquet_writer_leave(writer) &
                bind(C, name="parquet_writer_leave")
            import
            type(c_ptr), value :: writer
        end subroutine

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

        !> Mirrors parquet_settings' verbosity and message-stream choices to the C++ side, which
        !> prints warnings and the reader stats report itself. Both are already-resolved integer
        !> codes: 0/1/2 for normal/silent/errors_only, and 0/1 for stdout/stderr.
        subroutine parquet_push_output_settings(verbosity, message_stream) &
                bind(C, name="parquet_push_output_settings")
            import
            integer(c_int), value :: verbosity
            integer(c_int), value :: message_stream
        end subroutine

        !> Mirrors parquet_settings' five C++-side performance knobs in one call. Every value
        !> arrives already resolved -- the "0 means the built-in default" sentinel is resolved on
        !> the Fortran side, so the C++ half never has to know a default -- and the two flags cross
        !> as 0/1 because this boundary has no logical convention.
        !>
        !> One push rather than five setters so that parquet_reset_settings cannot restore some
        !> knobs and forget others: restoring the Fortran variables and calling this once is the
        !> whole job (feature_risks.md Risk-42).
        subroutine parquet_push_performance_settings(sort_counting_path, &
                sort_counting_bucket_limit, target_row_group_bytes, statistics_prescreen) &
                bind(C, name="parquet_push_performance_settings")
            import
            integer(c_int), value :: sort_counting_path
            integer(c_int64_t), value :: sort_counting_bucket_limit
            integer(c_int64_t), value :: target_row_group_bytes
            integer(c_int), value :: statistics_prescreen
        end subroutine

        !> Reports Arrow's current global CPU thread-pool capacity. Named differently from its
        !> linked symbol for the same reason as the setter above: parquet_settings' own public
        !> procedure is called parquet_get_arrow_threads, and the two cannot share a name in scope.
        function parquet_get_thread_pool_capacity() &
                bind(C, name="parquet_get_max_threads") result(n)
            import
            integer(c_int) :: n !! current thread-pool capacity.
        end function

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

        !> Adds one flat key-value table metadata entry to `writer`. `datatype` is the type
        !> token the typed add_metadata overload recorded ("int32", "float64[]", ...), or ""
        !> for a value that is a string; a non-empty token makes build_file_metadata emit a
        !> companion "<key>.datatype" entry and makes build_votable_xml declare the real
        !> VOTable type instead of char.
        subroutine parquet_add_table_metadata(writer, key, value, description, datatype) &
                bind(C, name="parquet_add_table_metadata")
            import
            type(c_ptr), value :: writer
            character(kind=c_char) :: key(*)
            character(kind=c_char) :: value(*)
            character(kind=c_char) :: description(*)
            character(kind=c_char) :: datatype(*)
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

        !> Declares `name` (a column's OUTPUT name) protected on this writer, so its
        !> Arrow field is built non-nullable on every write path. Pushed once per
        !> protected column by parquet_open_writer, before any write.
        subroutine parquet_writer_set_protected_column(writer, name) &
                bind(C, name="parquet_writer_set_protected_column")
            import
            type(c_ptr), value :: writer !! opaque writer handle.
            character(kind=c_char) :: name(*) !! null-terminated output column name.
        end subroutine

        !> Returns 1 if `name`'s stored Arrow field is declared nullable, 0 if not.
        !> Schema-only (reads no column data). A vector column reports its CHILD
        !> element field's flag, not the outer list field's (which is always
        !> non-nullable); a dotted struct path reports the leaf's own flag.
        function parquet_reader_get_column_nullable(reader, name) &
                bind(C, name="parquet_reader_get_column_nullable") result(nullable)
            import
            type(c_ptr), value :: reader !! opaque reader handle.
            character(kind=c_char) :: name(*) !! null-terminated column name (may be a dotted struct path).
            integer(c_long_long) :: nullable !! 1 if the field is nullable, 0 if not.
        end function

        !> Validates and applies a packed row filter to `reader`; returns
        !> non-zero and writes a message to `err_out` on failure. The clauses
        !> arrive as `n` packed leaves (names/ops/values/is_string_flags) plus
        !> `n_nodes` postfix expression nodes over them (node_kind: 1=leaf,
        !> 2=and, 3=or, 4=not; node_leaf: 1-based leaf index for a leaf node,
        !> 0 otherwise) -- see parquet_parse_filter_expr in
        !> parquet_read_filter.f90, which builds them. `expr_text` is the same
        !> expression re-rendered in canonical form (NUL-terminated), retained
        !> on the handle purely so parquet_reader_print_stat can show what was
        !> applied; the evaluator never parses it.
        !>
        !> Deliberately NOT named parquet_reader_set_filter on the Fortran side,
        !> unlike every other interface here: that name belongs to the public
        !> API procedure in parquet_core.f90 (which calls this one via
        !> parquet_apply_filter), and parquet_core.f90 imports this module
        !> unrestricted, so the two would collide. The linked C symbol is
        !> unchanged.
        function c_reader_set_filter(reader, names_packed, name_len, ops_packed, op_len, &
                values_packed, value_len, is_string_flags, n, node_kind, node_leaf, n_nodes, &
                expr_text, rg_lo, rg_hi, row_lo, row_hi, err_out, err_cap) &
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
            integer(c_int8_t) :: node_kind(*)
            integer(c_int32_t) :: node_leaf(*)
            integer(c_long_long), value :: n_nodes
            character(kind=c_char) :: expr_text(*)
            integer(c_long_long), value :: rg_lo
            integer(c_long_long), value :: rg_hi
            integer(c_long_long), value :: row_lo
            integer(c_long_long), value :: row_hi
            character(kind=c_char) :: err_out(*)
            integer(c_long_long), value :: err_cap
            integer(c_long_long) :: status
        end function

        !> Reports one sort key's reduced family and dimensions, without copying any values.
        !> Paired with `parquet_reader_sort_key_fetch` below: this call sizes the buffers, that one
        !> fills them. Both bind the key, which is deliberate -- see the C++ side for why the
        !> repeated bind is preferred over staging the key on the reader handle.
        function parquet_reader_sort_key_info(reader, name, descending, nulls_first, family, &
                nrows, nbytes, has_nulls, err_out, err_cap) &
                bind(C, name="parquet_reader_sort_key_info") result(status)
            import
            type(c_ptr), value :: reader !! open reader handle.
            character(kind=c_char) :: name(*) !! NUL-terminated column name, possibly a struct path.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_int) :: family !! 0 = integer (incl. boolean/temporal), 1 = real, 2 = string.
            integer(c_long_long) :: nrows !! number of key elements.
            integer(c_long_long) :: nbytes !! total payload bytes; family 2 only, else 0.
            integer(c_int8_t) :: has_nulls !! nonzero when the key carries a validity vector.
            character(kind=c_char) :: err_out(*) !! receives the failure message, if any.
            integer(c_long_long), value :: err_cap !! capacity of err_out, in characters.
            integer(c_long_long) :: status !! 0 on success, 1 on failure.
        end function

        !> Copies one sort key's reduced values into caller-owned buffers sized by
        !> `parquet_reader_sort_key_info`. Exactly one of `ints`/`reals`/(`offsets`,`data`) is
        !> written, per the family reported there; the unused ones may be `C_NULL_PTR`. `valid` is
        !> written only when that call reported `has_nulls`.
        function parquet_reader_sort_key_fetch(reader, name, descending, nulls_first, ints, reals, &
                offsets, data, valid, err_out, err_cap) &
                bind(C, name="parquet_reader_sort_key_fetch") result(status)
            import
            type(c_ptr), value :: reader !! open reader handle.
            character(kind=c_char) :: name(*) !! NUL-terminated column name, possibly a struct path.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            type(c_ptr), value :: ints !! -> nrows int64 values, or C_NULL_PTR.
            type(c_ptr), value :: reals !! -> nrows float64 values, or C_NULL_PTR.
            type(c_ptr), value :: offsets !! -> nrows+1 int64 offsets, offsets(0)=0, or C_NULL_PTR.
            type(c_ptr), value :: data !! -> nbytes payload bytes, or C_NULL_PTR.
            type(c_ptr), value :: valid !! -> nrows int8 flags, 1 = valid, or C_NULL_PTR.
            character(kind=c_char) :: err_out(*) !! receives the failure message, if any.
            integer(c_long_long), value :: err_cap !! capacity of err_out, in characters.
            integer(c_long_long) :: status !! 0 on success, 1 on failure.
        end function

        !> Installs a permutation built by the Fortran engine and settles what happens to every
        !> column already decoded on this reader -- which is only ever the sort's own key columns,
        !> since a sort is refused once anything else has been read. `perm` is 0-based, which is
        !> what Arrow's Take consumes -- `pf_argsort` produces 1-based indices, so the caller
        !> converts on the way out.
        !>
        !> `keep_cache` chooses between the two, and 0 is the ordinary answer: RELEASE the decoded
        !> key columns, so that a later read decodes them again and is permuted on the normal read
        !> path. Sorting by a column does not mean reading it, and reordering data nobody looks at
        !> was measured at up to 49% of a sort-then-read workflow. Pass 1 only when the caller has
        !> asked for everything to be resident anyway -- `parquet_open_reader(..., prefetch=.true.)`
        !> is the sole case -- where releasing would merely force the prefetch that follows to
        !> decode the same columns a second time.
        function parquet_reader_sort_install(reader, perm, n, key_text, keep_cache, err_out, err_cap) &
                bind(C, name="parquet_reader_sort_install") result(status)
            import
            type(c_ptr), value :: reader !! open reader handle.
            integer(c_int64_t) :: perm(*) !! 0-based row permutation of length `n`.
            integer(c_long_long), value :: n !! number of rows the permutation covers.
            character(kind=c_char) :: key_text(*) !! whole key list, for print_stat only.
            integer(c_long_long), value :: keep_cache !! 1 re-Takes the decoded columns, 0 releases them.
            character(kind=c_char) :: err_out(*) !! receives the failure message, if any.
            integer(c_long_long), value :: err_cap !! capacity of err_out, in characters.
            integer(c_long_long) :: status !! 0 on success, 1 on failure.
        end function

        !> Gives `reader` the row transform `source` has already worked out -- its filter/sample
        !> mask, its sort permutation and the row-group bookkeeping derived from them -- rather than
        !> making it derive the same thing from the same file again. A POINTER copy of the two
        !> expensive objects (both immutable Arrow arrays), so the cost is two atomic refcount
        !> increments however large the file. `source` is read WITHOUT the concurrency guard, so
        !> several threads may adopt from one idle source at once; see the C++ side for why that is
        !> safe and what it refuses. Returns 0, or 1 with a NUL-terminated message in `err_out`.
        !> Named c_reader_adopt_transform here because the public API procedure
        !> parquet_reader_adopt_transform (parquet_core.f90) owns that name.
        function c_reader_adopt_transform(reader, source, err_out, err_cap) &
                bind(C, name="parquet_reader_adopt_transform") result(status)
            import
            type(c_ptr), value :: reader !! reader to give the transform to; must have none of its own.
            type(c_ptr), value :: source !! reader whose transform is adopted; must be idle.
            character(kind=c_char) :: err_out(*) !! receives the failure message, if any.
            integer(c_long_long), value :: err_cap !! capacity of err_out, in characters.
            integer(c_long_long) :: status !! 0 on success, 1 on failure.
        end function

        !> Whether a read-time sort is active on `reader` (1) or not (0). The
        !> predicate every "not while sorted" guard keys on -- deliberately
        !> about a sort permutation only, never a filter/sample mask, which
        !> chunked reads and row/element mode all support.
        function parquet_reader_has_sort(reader) &
                bind(C, name="parquet_reader_has_sort") result(has_sort)
            import
            type(c_ptr), value :: reader !! open reader handle.
            integer(c_long_long) :: has_sort !! 1 when a sort is active, 0 otherwise.
        end function

        !> Starts a raw-array sort: allocates a key builder for `nrows` rows and
        !> returns an opaque handle. Feeds the SAME C++ std::sort permutation
        !> engine the read-time `sort_by=` uses, so an in-memory sort and a
        !> read-time sort of the same keys cannot order rows differently.
        !> The caller must free the handle.
        function parquet_sort_builder_new(nrows) &
                bind(C, name="parquet_sort_builder_new") result(builder)
            import
            integer(c_long_long), value :: nrows !! rows to sort.
            type(c_ptr) :: builder !! opaque builder handle.
        end function

        !> Adds an integer sort key. Boolean and every temporal kind come
        !> through here too, since their stored values order exactly as the
        !> values they represent. `valid` may be C_NULL_PTR for a key with no
        !> nulls at all, which is the engine's own fast path.
        subroutine parquet_sort_builder_add_key_int64(builder, values, valid, descending, nulls_first) &
                bind(C, name="parquet_sort_builder_add_key_int64")
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
        end subroutine

        !> Adds a floating-point sort key. NaNs are ordinary values and are
        !> placed by the engine's own tier rule, not by the caller.
        subroutine parquet_sort_builder_add_key_double(builder, values, valid, descending, nulls_first) &
                bind(C, name="parquet_sort_builder_add_key_double")
            import
            type(c_ptr), value :: builder !! builder handle.
            real(c_double) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
        end subroutine

        !> Adds a string sort key from a packed offsets/data pair: row i is
        !> `data(offsets(i) .. offsets(i+1)-1)`, so `offsets` has nrows+1
        !> entries and both are 0-based on the C side. The bytes are copied
        !> into the builder, so the caller may permute its own storage after.
        subroutine parquet_sort_builder_add_key_string(builder, offsets, data, valid, descending, &
                nulls_first) bind(C, name="parquet_sort_builder_add_key_string")
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long) :: offsets(*) !! nrows+1 byte offsets into `data`.
            character(kind=c_char) :: data(*) !! the packed bytes of every row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
        end subroutine

        !> Builds the permutation over every key added so far, writing it
        !> 1-based into `perm` (which the caller sized to nrows) so it can be
        !> handed straight to `parquet_column%reindex`.
        function parquet_sort_builder_build(builder, threads, perm) &
                bind(C, name="parquet_sort_builder_build") result(status)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long), value :: threads !! resolved thread count; <= 1 sorts serially.
            integer(c_long_long) :: perm(*) !! receives the 1-based permutation.
            integer(c_long_long) :: status !! 0 on success, 1 when no key was added.
        end function

        !> Whether every row is already in the stated order under the full key
        !> list. The multi-key counterpart of `parquet_sort_is_sorted_*`, needed
        !> because a `parquet_timestamp` binds as TWO integer keys, so no
        !> single-key entry point can answer the question for one. O(n) with an
        !> early exit, using the same comparator the sort does.
        function parquet_sort_builder_is_sorted(builder) &
                bind(C, name="parquet_sort_builder_is_sorted") result(sorted)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long) :: sorted !! 1 in order, 0 not, -1 when no key was added.
        end function

        !> Writes the first `count` entries of the 1-based permutation over every
        !> key added so far. `perm` is sized `count` by the caller, NOT nrows.
        !> `count` is clamped to nrows on the C side, so asking for more elements
        !> than exist returns all of them rather than failing.
        function parquet_sort_builder_build_partial(builder, count, perm) &
                bind(C, name="parquet_sort_builder_build_partial") result(status)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long), value :: count !! how many leading entries to order.
            integer(c_long_long) :: perm(*) !! receives `count` 1-based indices.
            integer(c_long_long) :: status !! 0 on success, 1 when no key was added.
        end function

        !> The 1-based row index a full stable sort would place at 1-based rank
        !> `nth`, over every key added so far. Returns 0 when no key was added or
        !> `nth` is outside 1..nrows. Only the index is computed -- the caller
        !> reads its own value out with it, which is what keeps the C side free of
        !> any per-type value handling.
        function parquet_sort_builder_nth_element(builder, nth) &
                bind(C, name="parquet_sort_builder_nth_element") result(idx)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long), value :: nth !! 1-based rank wanted.
            integer(c_long_long) :: idx !! 1-based row index at that rank, or 0.
        end function

        !> Sorts, then reports where the runs of EQUAL rows are: `perm` receives
        !> the 1-based permutation and `tie(k)` is 1 when the row at output
        !> position k compares equal to the one before it (`tie(1)` is always 0).
        !> One call rather than a sort plus a separate comparison pass, since
        !> `pf_unique`/`pf_rank` need both and would otherwise sort twice.
        function parquet_sort_builder_build_runs(builder, threads, group_keys, perm, tie) &
                bind(C, name="parquet_sort_builder_build_runs") result(status)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long), value :: threads !! resolved thread count; <= 1 sorts serially.
            !> how many LEADING keys decide a tie. The sort itself always uses every key; only
            !! the tie test is narrowed, which is what yields groups by one key with the rows
            !! inside each group ordered by the rest. Counted in ENGINE keys and already resolved
            !! on the Fortran side -- never a sentinel for the C++ side to interpret.
            integer(c_long_long), value :: group_keys
            integer(c_long_long) :: perm(*) !! receives the 1-based permutation.
            integer(c_int8_t) :: tie(*) !! receives 1 where a row ties the previous one.
            integer(c_long_long) :: status !! 0 on success, 1 when no key was added.
        end function

        !> Binary search over a builder holding `n_search`+1 rows, where the LAST
        !> row is the target value the caller appended. Comparing the target
        !> through the ordinary key layout is what makes drift from the sort
        !> comparator impossible -- there is no compare-a-row-against-a-value arm.
        !> `which` is 0 for lower_bound, 1 for upper_bound.
        function parquet_sort_builder_search(builder, n_search, which) &
                bind(C, name="parquet_sort_builder_search") result(pos)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long), value :: n_search !! rows to search, excluding the target.
            integer(c_int8_t), value :: which !! 0 = lower_bound, 1 = upper_bound.
            integer(c_long_long) :: pos !! 1-based insertion point, or -1 when no key was added.
        end function

        !> Merges two already-ordered ranges of one builder into a 1-based
        !> permutation of all its rows: rows 1..`na` are the first input and the
        !> rest the second, concatenated by the caller. Ties take from the first
        !> input, which is what makes `pf_merge` agree with `pf_sort` of the
        !> concatenation element for element.
        function parquet_sort_builder_merge(builder, na, perm) &
                bind(C, name="parquet_sort_builder_merge") result(status)
            import
            type(c_ptr), value :: builder !! builder handle.
            integer(c_long_long), value :: na !! rows belonging to the first input.
            integer(c_long_long) :: perm(*) !! receives the 1-based permutation.
            integer(c_long_long) :: status !! 0 on success, 1 when no key was added.
        end function

        !> One-shot single-key partial argsort over an integer key: writes the
        !> first `count` entries of the 1-based permutation into `perm`. Borrows
        !> `values` for the duration of the call, exactly as
        !> `parquet_sort_argsort_int64` does.
        subroutine parquet_sort_partial_argsort_int64(n, values, valid, descending, nulls_first, &
                count, perm) bind(C, name="parquet_sort_partial_argsort_int64")
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: count !! how many leading entries to order.
            integer(c_long_long) :: perm(*) !! receives `count` 1-based indices.
        end subroutine

        !> One-shot single-key partial argsort over a floating-point key.
        subroutine parquet_sort_partial_argsort_double(n, values, valid, descending, nulls_first, &
                count, perm) bind(C, name="parquet_sort_partial_argsort_double")
            import
            integer(c_long_long), value :: n !! number of rows.
            real(c_double) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: count !! how many leading entries to order.
            integer(c_long_long) :: perm(*) !! receives `count` 1-based indices.
        end subroutine

        !> One-shot single-key partial argsort over a string key, same packed
        !> offsets/data layout as `parquet_sort_argsort_string`.
        subroutine parquet_sort_partial_argsort_string(n, offsets, data, valid, descending, &
                nulls_first, count, perm) bind(C, name="parquet_sort_partial_argsort_string")
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: offsets(*) !! n+1 byte offsets into `data`.
            character(kind=c_char) :: data(*) !! the packed bytes of every row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: count !! how many leading entries to order.
            integer(c_long_long) :: perm(*) !! receives `count` 1-based indices.
        end subroutine

        !> One-shot single-key nth-element over an integer key: the 1-based row
        !> index a full stable sort would place at 1-based rank `nth`, or 0 when
        !> `nth` is outside 1..n.
        function parquet_sort_nth_index_int64(n, values, valid, descending, nulls_first, nth) &
                bind(C, name="parquet_sort_nth_index_int64") result(idx)
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: nth !! 1-based rank wanted.
            integer(c_long_long) :: idx !! 1-based row index at that rank, or 0.
        end function

        !> One-shot single-key nth-element over a floating-point key.
        function parquet_sort_nth_index_double(n, values, valid, descending, nulls_first, nth) &
                bind(C, name="parquet_sort_nth_index_double") result(idx)
            import
            integer(c_long_long), value :: n !! number of rows.
            real(c_double) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: nth !! 1-based rank wanted.
            integer(c_long_long) :: idx !! 1-based row index at that rank, or 0.
        end function

        !> One-shot single-key nth-element over a string key, same packed
        !> offsets/data layout as `parquet_sort_argsort_string`.
        function parquet_sort_nth_index_string(n, offsets, data, valid, descending, nulls_first, &
                nth) bind(C, name="parquet_sort_nth_index_string") result(idx)
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: offsets(*) !! n+1 byte offsets into `data`.
            character(kind=c_char) :: data(*) !! the packed bytes of every row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: nth !! 1-based rank wanted.
            integer(c_long_long) :: idx !! 1-based row index at that rank, or 0.
        end function

        !> Releases a sort builder and everything it copied.
        subroutine parquet_sort_builder_free(builder) &
                bind(C, name="parquet_sort_builder_free")
            import
            type(c_ptr), value :: builder !! builder handle.
        end subroutine

        !> One-shot single-key argsort over an integer key: writes the 1-based
        !> permutation of `values` into `perm`. Boolean and every temporal kind
        !> come through here too, reduced to their stored integers. `valid` may
        !> be C_NULL_PTR for a key with no nulls, which is the engine's own fast
        !> path.
        !>
        !> Unlike the builder above this BORROWS `values` rather than copying
        !> it, which it can because nothing outlives the call. The actual
        !> argument must therefore be contiguous -- a non-contiguous section
        !> would be passed as a compiler temporary, which is fine here (the
        !> temporary lives for the call) but would not be on the builder.
        subroutine parquet_sort_argsort_int64(n, values, valid, descending, nulls_first, threads, perm) &
                bind(C, name="parquet_sort_argsort_int64")
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: threads !! resolved thread count; <= 1 sorts serially.
            integer(c_long_long) :: perm(*) !! receives the 1-based permutation.
        end subroutine

        !> One-shot single-key argsort over a floating-point key. NaNs are
        !> ordinary values and are placed by the engine's own tier rule.
        subroutine parquet_sort_argsort_double(n, values, valid, descending, nulls_first, threads, perm) &
                bind(C, name="parquet_sort_argsort_double")
            import
            integer(c_long_long), value :: n !! number of rows.
            real(c_double) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: threads !! resolved thread count; <= 1 sorts serially.
            integer(c_long_long) :: perm(*) !! receives the 1-based permutation.
        end subroutine

        !> One-shot single-key argsort over a string key, from a packed
        !> offsets/data pair: row i is `data(offsets(i) .. offsets(i+1)-1)`, so
        !> `offsets` has n+1 entries and both are 0-based on the C side. The
        !> bytes are NOT copied -- they are read in place for the duration of
        !> the call.
        subroutine parquet_sort_argsort_string(n, offsets, data, valid, descending, nulls_first, threads, perm) &
                bind(C, name="parquet_sort_argsort_string")
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: offsets(*) !! n+1 byte offsets into `data`.
            character(kind=c_char) :: data(*) !! the packed bytes of every row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long), value :: threads !! resolved thread count; <= 1 sorts serially.
            integer(c_long_long) :: perm(*) !! receives the 1-based permutation.
        end subroutine

        !> Whether an integer key is already in the stated order (1) or not (0).
        !> Uses the SAME comparator the sort does, minus its row-index
        !> tiebreaker, so the two can never disagree about nulls, NaNs or
        !> direction on one array. O(n) with an early exit, and no copy.
        function parquet_sort_is_sorted_int64(n, values, valid, descending, nulls_first) &
                bind(C, name="parquet_sort_is_sorted_int64") result(sorted)
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long) :: sorted !! 1 when in order, 0 otherwise.
        end function

        !> Whether a floating-point key is already in the stated order.
        function parquet_sort_is_sorted_double(n, values, valid, descending, nulls_first) &
                bind(C, name="parquet_sort_is_sorted_double") result(sorted)
            import
            integer(c_long_long), value :: n !! number of rows.
            real(c_double) :: values(*) !! one value per row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long) :: sorted !! 1 when in order, 0 otherwise.
        end function

        !> Whether a string key is already in the stated order. Same packed
        !> offsets/data layout as `parquet_sort_argsort_string`.
        function parquet_sort_is_sorted_string(n, offsets, data, valid, descending, nulls_first) &
                bind(C, name="parquet_sort_is_sorted_string") result(sorted)
            import
            integer(c_long_long), value :: n !! number of rows.
            integer(c_long_long) :: offsets(*) !! n+1 byte offsets into `data`.
            character(kind=c_char) :: data(*) !! the packed bytes of every row.
            type(c_ptr), value :: valid !! int8 per row (1 = valid), or C_NULL_PTR.
            integer(c_int8_t), value :: descending !! nonzero for descending order.
            integer(c_int8_t), value :: nulls_first !! nonzero to place nulls before values.
            integer(c_long_long) :: sorted !! 1 when in order, 0 otherwise.
        end function

        !> Whether any column of `reader` has already been decoded into its
        !> column cache (1) or not (0) -- the guard parquet_reader_set_filter
        !> (parquet_core.f90) needs, since applying a filter after a read would
        !> misalign what was already returned against everything read after.
        function parquet_reader_has_decoded_columns(reader) &
                bind(C, name="parquet_reader_has_decoded_columns") result(has_any)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: has_any
        end function

        !> Whether any column of `reader` has been read through the CHUNK api
        !> (1) or not (0). Separate from parquet_reader_has_decoded_columns
        !> above because a chunked read caches nothing, so the column cache
        !> stays empty and cannot answer this -- both guards are needed to
        !> refuse a reader that has already handed rows back.
        function parquet_reader_has_chunk_reads(reader) &
                bind(C, name="parquet_reader_has_chunk_reads") result(has_any)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: has_any
        end function

        !> Whether `reader` has filter CLAUSES installed (1) or not (0) --
        !> narrower than parquet_reader_has_filter, which also reports a
        !> sample-only mask. parquet_reader_set_filter (parquet_core.f90) refuses a
        !> reader that is already filtered but accepts a sampled one, and this
        !> is the distinction that lets it tell the two apart.
        function parquet_reader_has_filter_clauses(reader) &
                bind(C, name="parquet_reader_has_filter_clauses") result(has_any)
            import
            type(c_ptr), value :: reader
            integer(c_long_long) :: has_any
        end function

        !> Installs a caller-built Bernoulli row mask on `reader` (see parquet_reader_set_sample in
        !> parquet_wrapper.cpp); returns non-zero and writes a message to `err_out` on failure.
        !>
        !> **No randomness crosses this boundary.** `keep` already holds one byte per PHYSICAL row
        !> of the file, nonzero meaning keep, drawn by parquet_apply_sample (parquet_read.f90) from
        !> this library's own generator -- see parquet_sample_algorithm (parquet_core.f90) for the
        !> frozen mapping. sample_fraction and seed_used are carried only so
        !> parquet_reader_print_stat can report them. keep_len must equal the file's total row
        !> count; the C++ side rejects any other value rather than reading out of bounds.
        !>
        !> filter_will_follow (non-zero when the caller's own filter= will also be applied right
        !> after this): stashes the mask instead of installing it, so the filter's own clause
        !> evaluation still sees raw, unmasked column data. Which rows are selected is identical
        !> either way -- the deferral is about ordering alone.
        function parquet_reader_set_sample(reader, sample_fraction, seed_used, keep, keep_len, &
                filter_will_follow, err_out, err_cap) &
                bind(C, name="parquet_reader_set_sample") result(status)
            import
            type(c_ptr), value :: reader
            real(c_double), value :: sample_fraction
            integer(c_int64_t), value :: seed_used
            integer(c_int8_t) :: keep(*)
            integer(c_int64_t), value :: keep_len
            integer(c_int8_t), value :: filter_will_follow
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

        !> Returns 1 when `name` contains any Null over row groups `rg_lo`..`rg_hi` (1-based
        !> inclusive; `rg_lo` <= 0 means every row group), OR when the file's statistics cannot
        !> answer. Read from the footer; no column data is touched.
        function parquet_reader_column_has_nulls(reader, name, rg_lo, rg_hi) &
                bind(C, name="parquet_reader_column_has_nulls") result(has_nulls)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: rg_lo
            integer(c_long_long), value :: rg_hi
            integer(c_int) :: has_nulls
        end function

        !> Returns 1 when `name`'s width can only be found by reading its data (a plain
        !> LIST/LARGE_LIST), 0 otherwise. Schema-only; lets parquet_table defer such a column's
        !> kind and width to first use instead of reading it at open.
        function parquet_reader_column_width_is_deferred(reader, name) &
                bind(C, name="parquet_reader_column_width_is_deferred") result(deferred)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_int) :: deferred
        end function

        !> Returns a CANDIDATE uniform width for `name` over row groups `rg_lo`..`rg_hi`
        !> (1-based inclusive; `rg_lo` <= 0 means every row group), from the file footer alone with
        !> no column data read. Unproven: 1 means no uniform width above 1 can exist, anything
        !> larger still has to be confirmed by whatever consumes it.
        function parquet_reader_list_width_candidate(reader, name, rg_lo, rg_hi) &
                bind(C, name="parquet_reader_list_width_candidate") result(width)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: rg_lo
            integer(c_long_long), value :: rg_hi
            integer(c_long_long) :: width
        end function

        !> Returns the PROVEN uniform width of `name` over row groups `rg_lo`..`rg_hi` (1-based
        !> inclusive; `rg_lo` <= 0 means every row group), or 1 when it is not a uniform vector
        !> column. Reads at most one row group at a time, so it is safe on a column larger than
        !> memory.
        function parquet_reader_list_width_verified(reader, name, rg_lo, rg_hi) &
                bind(C, name="parquet_reader_list_width_verified") result(width)
            import
            type(c_ptr), value :: reader
            character(kind=c_char) :: name(*)
            integer(c_long_long), value :: rg_lo
            integer(c_long_long), value :: rg_hi
            integer(c_long_long) :: width
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
        !> Fills `indices` with the 1-based physical FILE row index of each row the reader
        !! currently returns, in that order. `n` must be the reader's own row count.
        subroutine parquet_reader_physical_row_indices(reader, indices, n) &
                bind(C, name="parquet_reader_physical_row_indices")
            import :: c_ptr, c_long_long
            type(c_ptr), value :: reader                     !! opaque reader handle.
            integer(c_long_long), intent(out) :: indices(*)  !! receives one index per row.
            integer(c_long_long), value :: n                 !! rows the reader returns.
        end subroutine parquet_reader_physical_row_indices
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
