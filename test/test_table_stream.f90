!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for writing a `parquet_table` out one row group at a time (`feature_pandas_S2.md`):
!> `parquet_derive_schema`, `parquet_open_writer_like`, `parquet_write_table_chunk` and the
!> `parquet_table_writer` sink.
!>
!> Every test writes its own fixtures under `test_run/` -- the tests of one suite run
!> concurrently. Abort paths live in test/error_scenarios.f90, driven from test_errors.f90.
module test_table_stream
    use parquet
    use parquet_tables
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_stream
    !
    !> Row count every fixture in this suite uses.
    integer, parameter :: NROW = 10
    !> Vector width every vector fixture in this suite uses.
    integer, parameter :: NVEC = 3
    !> Row count of the 18-kind fixture (`write_all_kinds_fixture`).
    integer, parameter :: NKROW = 6
    !
contains
    !
    !> Collect all exported unit tests
    subroutine collect_tests_table_stream(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        !
        testsuite = [ &
            new_unittest("parquet_derive_schema builds the schema a schema-less write builds", &
                test_derive_schema_matches_schemaless_write), &
            new_unittest("parquet_derive_schema skips parquet_row_index and unread columns", &
                test_derive_schema_skips_row_index), &
            new_unittest("parquet_derive_schema's table: name is the source stem, 'table', or name=", &
                test_derive_schema_name_rules), &
            new_unittest("set_protected on a derived schema reaches the file", &
                test_derive_schema_set_protected_reaches_the_file), &
            new_unittest("parquet_open_writer_like forwards the writer options", &
                test_open_writer_like_forwards_options), &
            new_unittest("parquet_open_writer_like uses the schema it is given", &
                test_open_writer_like_uses_the_given_schema), &
            new_unittest("parquet_open_writer_like carries the source file's metadata on request", &
                test_open_writer_like_carries_metadata), &
            new_unittest("two chunks equal the one-shot write of their concatenation", &
                test_chunked_write_equals_one_shot), &
            new_unittest("a Null in a later row group survives: the mask is passed on every chunk", &
                test_chunked_write_nullability_survives_a_late_null), &
            new_unittest("a protected column's chunks are written unmasked and non-nullable", &
                test_chunked_write_protected_column_is_unmasked), &
            new_unittest("every scalar and vector kind round-trips through the chunked path", &
                test_chunked_write_every_kind), &
            new_unittest("list, map, struct and compact string columns round-trip through the chunked path", &
                test_chunked_write_containers), &
            new_unittest("row_mask= drops rows per chunk and leaves is_valid pre-mask-indexed", &
                test_chunked_write_row_mask), &
            new_unittest("a field disabled with set_column_unavailable is skipped by a chunk write", &
                test_chunked_write_skips_a_disabled_field), &
            new_unittest("a schema-less writer takes every resident column in slot order, per chunk", &
                test_chunked_write_on_a_schemaless_writer), &
            new_unittest("the sink writes the whole buffer after the append that crosses the threshold", &
                test_sink_uneven_batches), &
            new_unittest("the sink's automatic threshold sees a vector column's width", &
                test_sink_auto_chunk_size), &
            new_unittest("chunk_size= reaches the flush rule and %chunk_size()", &
                test_sink_explicit_chunk_size), &
            new_unittest("%flush writes the pending rows and does nothing with none pending", &
                test_sink_flush_boundary), &
            new_unittest("the close writes the rows below the threshold", &
                test_sink_close_writes_the_last_rows), &
            new_unittest("a sink that accepted no rows closes to a valid zero-row file", &
                test_sink_close_with_no_rows), &
            new_unittest("a missing column is null-filled on both sides of a flush", &
                test_sink_null_fill_across_a_flush), &
            new_unittest("a zero-row template gives the output its columns", &
                test_sink_zero_row_template), &
            new_unittest("the buffer keeps chunk_size rows of capacity across a flush", &
                test_sink_buffer_keeps_capacity), &
            new_unittest("a run of %append(row) calls reallocates nothing", &
                test_sink_row_appends_do_not_reallocate), &
            new_unittest("parquet_derive_schema does not measure nullability from the template", &
                test_derive_schema_does_not_measure_nullability), &
            new_unittest("the sink reads a declared column the appended table has not read, and gives it back", &
                test_sink_reads_an_unread_column), &
            new_unittest("%append(row) reads a declared column once and keeps it", &
                test_sink_row_append_keeps_what_it_read), &
            new_unittest("with schema= the schema selects the columns and an extra one is ignored", &
                test_sink_schema_selects_the_columns), &
            new_unittest("a string-only template resolves to the estimator's ceiling", &
                test_sink_string_template_auto_chunk_size), &
            new_unittest("an appended table's parquet_row_index is dropped, not refused", &
                test_sink_row_index_ignored), &
            new_unittest("a filtered slice serves as a template; its rows are not written", &
                test_sink_template_slice_accepted) &
            ]
    end subroutine collect_tests_table_stream
    !
    !> The four arrays every fixture table here is built from: one scalar integer, one scalar real
    !! with a unit, one string, one vector -- the four descriptor shapes `build_table_schema`
    !! treats differently. The shortest string comes first, per the fixture rule for string arrays.
    subroutine fixture_arrays(ids, masses, names, vec)
        integer(int64), intent(out) :: ids(NROW)
        real(real64), intent(out) :: masses(NROW), vec(NVEC, NROW)
        character(len=8), intent(out) :: names(NROW)
        integer :: i, j
        !
        do i = 1, NROW
            ids(i) = int(100 + i, int64)
            masses(i) = 0.5_real64 * real(i, real64)
            do j = 1, NVEC
                vec(j, i) = real(10 * i + j, real64)
            end do
        end do
        names(1) = "a"
        do i = 2, NROW
            write(names(i), '(a, i0)') "obj_", i
        end do
    end subroutine fixture_arrays
    !
    !> Builds the in-memory fixture table from `fixture_arrays`.
    subroutine fixture_table(t)
        type(parquet_table), intent(out) :: t
        integer(int64) :: ids(NROW)
        real(real64) :: masses(NROW), vec(NVEC, NROW)
        character(len=8) :: names(NROW)
        !
        call fixture_arrays(ids, masses, names, vec)
        call parquet_new_table(t)
        call t%add_column("id", ids)
        call t%add_column("mass", masses, unit="Msun")
        call t%add_column("name", names)
        call t%add_column("vec", vec)
    end subroutine fixture_table
    !
    !> Reads the `table:` value out of a sidecar .maml; "" when the file is missing or has none.
    !! The file operations sit in one critical: NAG's I/O runtime keeps a global unit table, and
    !! the tests of this suite run concurrently.
    subroutine sidecar_table_name(maml_file, tname)
        character(len=*), intent(in) :: maml_file
        character(len=:), allocatable, intent(out) :: tname
        character(len=256) :: line
        integer :: u, ios
        !
        tname = ""
        !$omp critical (test_table_stream_sidecar)
        open(newunit=u, file=maml_file, status="old", action="read", iostat=ios)
        if (ios == 0) then
            do
                read(u, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (line(1:6) == "table:") then
                    tname = trim(adjustl(line(7:)))
                    exit
                end if
            end do
            close(u)
        end if
        !$omp end critical (test_table_stream_sidecar)
    end subroutine sidecar_table_name
    !
    !> The derived schema, written through a hand-opened writer, produces the same file a
    !! schema-less `parquet_write_table` produces: the same columns in the same order, the same
    !! types, the same units through the sidecar. Asserted on the reopened files, never on the
    !! MAML text -- and on the schema's own fields for what a file cannot show (the `auto` sizes).
    subroutine test_derive_schema_matches_schemaless_write(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, ta, tb
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        type(parquet_reader) :: ra, rb
        integer(int64) :: ids(NROW)
        real(real64) :: masses(NROW), vec(NVEC, NROW)
        character(len=8) :: names(NROW)
        real(real64), allocatable :: back(:)
        character(len=:), allocatable :: na(:), nb(:), type_a, type_b, ua, ub, fname, dtype
        integer :: i, csize, asize
        integer(int64) :: nra, nrb
        character(len=*), parameter :: fa = "test_run/table_stream_derive_a.parquet"
        character(len=*), parameter :: ma = "test_run/table_stream_derive_a.maml"
        character(len=*), parameter :: fb = "test_run/table_stream_derive_b.parquet"
        character(len=*), parameter :: mb = "test_run/table_stream_derive_b.maml"
        !
        call fixture_arrays(ids, masses, names, vec)
        call fixture_table(t)
        !
        ! The schema-less write, and the derived schema through a hand-opened writer.
        call parquet_write_table(t, fa, write_maml=.true.)
        call parquet_derive_schema(t, s)
        call check(error, s%is_parsed(), "the derived schema must come back parsed")
        if (allocated(error)) return
        call check(error, s%get_num_fields() == 4, "one field per resident column")
        if (allocated(error)) return
        call s%get_field(2, fname, unit=ua)
        call check(error, fname == "mass", "fields come in slot order")
        if (allocated(error)) return
        call check(error, allocated(ua), "a column with a unit declares one")
        if (allocated(error)) return
        call check(error, ua == "Msun", "the unit comes from the column descriptor, got '" // ua // "'")
        if (allocated(error)) return
        call s%get_field(1, fname, unit=ua)
        if (allocated(ua)) then
            call check(error, len_trim(ua) == 0, "a column with no unit declares none")
            if (allocated(error)) return
        end if
        call s%get_field(3, fname, data_type=dtype, array_size=asize)
        call check(error, fname == "name", "the third field is the string column")
        if (allocated(error)) return
        call check(error, dtype == "string", "a character column derives a string field")
        if (allocated(error)) return
        call check(error, asize == parquet_size_auto, "array_size: is auto, never measured")
        if (allocated(error)) return
        call s%get_field(4, fname, col_size=csize)
        call check(error, fname == "vec", "the fourth field is the vector column")
        if (allocated(error)) return
        call check(error, csize == parquet_size_auto, "col_size: is auto, never measured")
        if (allocated(error)) return
        !
        call parquet_open_writer(w, fb, s, write_maml=.true.)
        call parquet_write_column(w, "id", ids)
        call parquet_write_column(w, "mass", masses)
        call parquet_write_column(w, "name", names)
        call parquet_write_column(w, "vec", vec)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(ra, fa)
        call parquet_open_reader(rb, fb)
        call parquet_get_column_names(ra, na)
        call parquet_get_column_names(rb, nb)
        call check(error, size(na) == 4, "the schema-less file holds every resident column")
        if (allocated(error)) return
        call check(error, size(nb) == size(na), "the derived file holds the same column count")
        if (allocated(error)) return
        do i = 1, size(na)
            call check(error, trim(na(i)) == trim(nb(i)), "the same column names in the same order")
            if (allocated(error)) return
            call parquet_get_column_type(ra, trim(na(i)), type_a)
            call parquet_get_column_type(rb, trim(nb(i)), type_b)
            call check(error, type_a == type_b, "the same stored type for column " // trim(na(i)))
            if (allocated(error)) return
        end do
        call parquet_get_nrows(ra, nra)
        call parquet_get_nrows(rb, nrb)
        call check(error, nra == nrb, "the same row count")
        if (allocated(error)) return
        call parquet_close_reader(ra)
        call parquet_close_reader(rb)
        !
        ! Units travel through the sidecar; both sidecars must carry the same ones.
        call parquet_open_table(ta, fa, maml=ma)
        call parquet_open_table(tb, fb, maml=mb)
        call ta%unit("mass", ua)
        call tb%unit("mass", ub)
        call check(error, ua == "Msun", "the schema-less sidecar carries the unit")
        if (allocated(error)) return
        call check(error, ub == "Msun", "the derived sidecar carries the unit")
        if (allocated(error)) return
        call ta%unit("id", ua)
        call tb%unit("id", ub)
        call check(error, len_trim(ua) == 0, "a column without a unit has none in the schema-less file")
        if (allocated(error)) return
        call check(error, len_trim(ub) == 0, "...and none in the derived file")
        if (allocated(error)) return
        call tb%get("mass", back)
        call check(error, all(back == masses), "the values written through the derived schema read back")
    end subroutine test_derive_schema_matches_schemaless_write
    !
    !> A materialized `parquet_row_index` is never in the derived schema, and neither is a column
    !! the table has but has not read: the schema is built from what is resident, exactly as the
    !! schema-less write's column set is.
    subroutine test_derive_schema_skips_row_index(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, lazy
        type(parquet_schema) :: s
        integer(int32) :: a(NROW)
        real(real64) :: b(NROW)
        character(len=:), allocatable :: resident(:), fname
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_rowidx.parquet"
        !
        do i = 1, NROW
            a(i) = i
            b(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call parquet_write_table(t, f)
        !
        call parquet_open_table(lazy, f)
        call lazy%prefetch("a")
        call lazy%prefetch(PARQUET_ROW_INDEX)
        call lazy%column_names(resident, resident_only=.true.)
        call check(error, size(resident) == 2, "precondition: 'a' and the row index are resident, 'b' is not")
        if (allocated(error)) return
        call parquet_derive_schema(lazy, s)
        call check(error, s%get_num_fields() == 1, "one field: the row index and the unread column are skipped")
        if (allocated(error)) return
        call s%get_field(1, fname)
        call check(error, fname == "a", "the one field is the resident data column")
    end subroutine test_derive_schema_skips_row_index
    !
    !> The `table:` name: "table" for a table built in memory, the source file's stem for a
    !! file-backed one, `name=` when given. Read back from the sidecar a writer emits from the
    !! schema, which is the only place the name reaches a file.
    subroutine test_derive_schema_name_rules(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, src
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        integer(int32) :: a(NROW)
        character(len=:), allocatable :: tname
        integer :: i
        character(len=*), parameter :: fsrc = "test_run/table_stream_name_source.parquet"
        character(len=*), parameter :: fmem = "test_run/table_stream_name_mem.parquet"
        character(len=*), parameter :: mmem = "test_run/table_stream_name_mem.maml"
        character(len=*), parameter :: ffile = "test_run/table_stream_name_file.parquet"
        character(len=*), parameter :: mfile = "test_run/table_stream_name_file.maml"
        character(len=*), parameter :: fexp = "test_run/table_stream_name_explicit.parquet"
        character(len=*), parameter :: mexp = "test_run/table_stream_name_explicit.maml"
        !
        do i = 1, NROW
            a(i) = i
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        !
        ! In memory: "table".
        call parquet_derive_schema(t, s)
        call parquet_open_writer(w, fmem, s, write_maml=.true.)
        call parquet_write_column(w, "a", a)
        call parquet_close_writer(w)
        call sidecar_table_name(mmem, tname)
        call check(error, tname == "table", "an in-memory table's schema is named 'table', got '" // tname // "'")
        if (allocated(error)) return
        !
        ! File-backed: the source file's stem, not the output's.
        call parquet_write_table(t, fsrc)
        call parquet_open_table(src, fsrc)
        call src%materialize_all()
        call parquet_derive_schema(src, s)
        call parquet_open_writer(w, ffile, s, write_maml=.true.)
        call parquet_write_column(w, "a", a)
        call parquet_close_writer(w)
        call sidecar_table_name(mfile, tname)
        call check(error, tname == "table_stream_name_source", &
            "a file-backed table's schema is named after its source file, got '" // tname // "'")
        if (allocated(error)) return
        !
        ! Explicit: name= wins on both.
        call parquet_derive_schema(src, s, name="my_catalogue")
        call parquet_open_writer(w, fexp, s, write_maml=.true.)
        call parquet_write_column(w, "a", a)
        call parquet_close_writer(w)
        call sidecar_table_name(mexp, tname)
        call check(error, tname == "my_catalogue", "name= is the table: name, got '" // tname // "'")
    end subroutine test_derive_schema_name_rules
    !
    !> `schema%set_protected` on a derived schema is what recovers the unmasked write path: a
    !! protected column written through the streaming API with an all-valid mask is stored
    !! non-nullable, while its unprotected neighbour, written the same way, is nullable (the
    !! streamed write fixes nullability from mask PRESENCE on the first row group).
    subroutine test_derive_schema_set_protected_reaches_the_file(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        real(real64) :: x(NROW), y(NROW)
        logical :: valid(NROW)
        logical :: nullable
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_protected.parquet"
        !
        do i = 1, NROW
            x(i) = real(i, real64)
            y(i) = real(-i, real64)
        end do
        valid = .true.
        call parquet_new_table(t)
        call t%add_column("x", x)
        call t%add_column("y", y)
        call parquet_derive_schema(t, s)
        call s%set_protected("x")
        !
        call parquet_open_writer(w, f, s)
        call parquet_new_row_group(w, NROW)
        call parquet_write_column_chunk(w, "x", x, is_valid=valid)
        call parquet_write_column_chunk(w, "y", y, is_valid=valid)
        call parquet_finish_row_group(w)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_nullable(r, "x", nullable)
        call check(error, .not. nullable, "the protected column's field is non-nullable")
        if (allocated(error)) return
        call parquet_get_column_nullable(r, "y", nullable)
        call check(error, nullable, "negative control: the unprotected column, masked, is nullable")
        if (allocated(error)) return
        call parquet_close_reader(r)
    end subroutine test_derive_schema_set_protected_reaches_the_file
    !
    !> Every writer option is forwarded: `chunk_size=` reaches the writer (queried right after the
    !! open, and visible in the file's row-group count), `write_maml=` emits the sidecar at close
    !! under the OUTPUT file's stem -- the name a schema-less `parquet_write_table` gives.
    subroutine test_open_writer_like_forwards_options(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        integer(int32) :: a(NROW), chunk, nrg
        real(real64) :: b(NROW)
        character(len=:), allocatable :: names(:), tname
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_like_opts.parquet"
        character(len=*), parameter :: m = "test_run/table_stream_like_opts.maml"
        !
        do i = 1, NROW
            a(i) = i
            b(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        !
        call parquet_open_writer_like(w, f, t, chunk_size=4, write_maml=.true., compression="snappy")
        call parquet_get_chunk_size(w, chunk)
        call check(error, chunk == 4, "chunk_size= reaches the writer as given")
        if (allocated(error)) return
        call parquet_get_column_names(w, names)
        call check(error, size(names) == 2, "the derived schema declares both resident columns")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "a" .and. trim(names(2)) == "b", "...in slot order")
        if (allocated(error)) return
        call parquet_write_column(w, "a", a)
        call parquet_write_column(w, "b", b)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        call parquet_get_num_row_groups(r, nrg)
        call check(error, nrg == 3, "10 rows at chunk_size=4 make three row groups")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call sidecar_table_name(m, tname)
        call check(error, tname == "table_stream_like_opts", &
            "the sidecar is written and named after the output file, got '" // tname // "'")
    end subroutine test_open_writer_like_forwards_options
    !
    !> With `schema=`, nothing is derived: the writer declares the schema's fields in the schema's
    !! order, whatever the table holds, and the caller's schema is left as it was.
    subroutine test_open_writer_like_uses_the_given_schema(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        integer(int32) :: a(NROW), b(NROW), c(NROW)
        character(len=:), allocatable :: names(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_like_schema.parquet"
        !
        do i = 1, NROW
            a(i) = i
            b(i) = 10 * i
            c(i) = 100 * i
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%add_column("c", c)
        call s%init("picked")
        call s%add_field("c", "int32")
        call s%add_field("a", "int32")
        !
        call parquet_open_writer_like(w, f, t, schema=s)
        call parquet_get_column_names(w, names)
        call check(error, size(names) == 2, "the writer declares the schema's fields, not the table's columns")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "c" .and. trim(names(2)) == "a", "...in the schema's order")
        if (allocated(error)) return
        call check(error, parquet_is_column_enabled(w, "c"), "a declared field is enabled")
        if (allocated(error)) return
        call check(error, .not. parquet_is_column_enabled(w, "b"), "a column the schema does not name is not")
        if (allocated(error)) return
        call parquet_write_column(w, "c", c)
        call parquet_write_column(w, "a", a)
        call parquet_close_writer(w)
        call check(error, s%get_num_fields() == 2, "the caller's schema is unchanged")
        if (allocated(error)) return
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_names(r, names)
        call check(error, size(names) == 2, "the file holds the schema's two columns")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "c" .and. trim(names(2)) == "a", "...in the schema's order")
        if (allocated(error)) return
        call parquet_close_reader(r)
    end subroutine test_open_writer_like_uses_the_given_schema
    !
    !> `copy_metadata=`/`metadata_keys=` carry the table's source-file metadata into the output
    !! under `parquet_write_table`'s rules, onto a private copy: a later open without the request
    !! carries nothing.
    subroutine test_open_writer_like_carries_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, plain
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        real(real64) :: v(NROW)
        character(len=:), allocatable :: val
        logical :: ok
        integer :: i
        character(len=*), parameter :: fsrc = "test_run/table_stream_meta_source.parquet"
        character(len=*), parameter :: fall = "test_run/table_stream_meta_all.parquet"
        character(len=*), parameter :: fkey = "test_run/table_stream_meta_keys.parquet"
        character(len=*), parameter :: fnone = "test_run/table_stream_meta_none.parquet"
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call s%init("source")
        call s%add_field("v", "float64")
        call s%add_metadata("origin", "survey_A")
        call s%add_metadata("instrument", "spectro")
        call parquet_open_writer(w, fsrc, s)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, fsrc)
        call t%materialize_all()
        !
        call parquet_open_writer_like(w, fall, t, copy_metadata=.true.)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        call parquet_open_table(plain, fall)
        call plain%get_file_metadata("origin", val, found=ok)
        call check(error, ok, "copy_metadata=.true. carries the source file's metadata")
        if (allocated(error)) return
        call check(error, val == "survey_A", "...with its value")
        if (allocated(error)) return
        call plain%get_file_metadata("instrument", val, found=ok)
        call check(error, ok, "...every key of it")
        if (allocated(error)) return
        !
        call parquet_open_writer_like(w, fkey, t, metadata_keys=["origin"])
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        call parquet_open_table(plain, fkey)
        call plain%get_file_metadata("origin", val, found=ok)
        call check(error, ok, "metadata_keys= carries the key it names")
        if (allocated(error)) return
        call plain%get_file_metadata("instrument", val, found=ok)
        call check(error, .not. ok, "metadata_keys= carries no key it does not name")
        if (allocated(error)) return
        !
        call parquet_open_writer_like(w, fnone, t)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        call parquet_open_table(plain, fnone)
        call plain%get_file_metadata("origin", val, found=ok)
        call check(error, .not. ok, "an open without the request carries nothing")
    end subroutine test_open_writer_like_carries_metadata
    !
    !> The 18-kind fixture the chunked-path test reads back: nine scalar kinds and their nine
    !! vector forms, written whole. The shortest string comes first, per the fixture rule.
    subroutine write_all_kinds_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer :: i, e
        integer(int32) :: a_i32(NKROW), v_i32(NVEC, NKROW)
        integer(int64) :: a_i64(NKROW), v_i64(NVEC, NKROW)
        real(real32) :: a_f32(NKROW), v_f32(NVEC, NKROW)
        real(real64) :: a_f64(NKROW), v_f64(NVEC, NKROW)
        logical :: a_bool(NKROW), v_bool(NVEC, NKROW)
        character(len=8) :: a_str(NKROW), v_str(NVEC, NKROW)
        type(parquet_date) :: a_date(NKROW), v_date(NVEC, NKROW)
        type(parquet_time) :: a_time(NKROW), v_time(NVEC, NKROW)
        type(parquet_timestamp) :: a_ts(NKROW), v_ts(NVEC, NKROW)
        !
        do i = 1, NKROW
            a_i32(i) = i
            a_i64(i) = int(i, int64) * 1000000000_int64
            a_f32(i) = real(i, real32) * 0.25_real32
            a_f64(i) = real(i, real64) * 1.75_real64
            a_bool(i) = mod(i, 2) == 1
            a_date(i) = parquet_date(2026, 3, i)
            a_time(i) = parquet_time(10, 20, i)
            a_ts(i) = parquet_timestamp(2026, 3, i, 1, 2, 3)
            write(a_str(i), '(a, i0)') "row", i
            do e = 1, NVEC
                v_i32(e, i) = i * 10 + e
                v_i64(e, i) = int(i * 10 + e, int64) * 1000000000_int64
                v_f32(e, i) = real(i * 10 + e, real32) * 0.5_real32
                v_f64(e, i) = real(i * 10 + e, real64) * 1.5_real64
                v_bool(e, i) = mod(i + e, 2) == 0
                v_date(e, i) = parquet_date(2026, 4, e)
                v_time(e, i) = parquet_time(5, 6, e)
                v_ts(e, i) = parquet_timestamp(2026, 4, e, 7, 8, 9)
                write(v_str(e, i), '(a, i0, i0)') "v", i, e
            end do
        end do
        a_str(1) = "a"
        v_str(1, 1) = "b"
        !
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "s_i32", a_i32)
        call parquet_write_column(w, "s_i64", a_i64)
        call parquet_write_column(w, "s_f32", a_f32)
        call parquet_write_column(w, "s_f64", a_f64)
        call parquet_write_column(w, "s_bool", a_bool)
        call parquet_write_column(w, "s_str", a_str)
        call parquet_write_column(w, "s_date", a_date)
        call parquet_write_column(w, "s_time", a_time)
        call parquet_write_column(w, "s_ts", a_ts)
        call parquet_write_column(w, "v_i32", v_i32)
        call parquet_write_column(w, "v_i64", v_i64)
        call parquet_write_column(w, "v_f32", v_f32)
        call parquet_write_column(w, "v_f64", v_f64)
        call parquet_write_column(w, "v_bool", v_bool)
        call parquet_write_column(w, "v_str", v_str)
        call parquet_write_column(w, "v_date", v_date)
        call parquet_write_column(w, "v_time", v_time)
        call parquet_write_column(w, "v_ts", v_ts)
        call parquet_close_writer(w)
    end subroutine write_all_kinds_fixture
    !
    !> Compares column `name` of two tables value by value, over every kind the table layer
    !! holds; `why` comes back empty when they agree and names the first difference otherwise.
    !! Temporal values are compared one at a time (their operators are not elemental), strings
    !! trimmed. The fixtures this serves hold no Null, so the temporal operators cannot abort.
    subroutine same_column(ta, tb, name, why)
        type(parquet_table), intent(inout) :: ta          !! the reference table.
        type(parquet_table), intent(inout) :: tb          !! the table under test.
        character(len=*), intent(in) :: name              !! the column to compare.
        character(len=:), allocatable, intent(out) :: why !! empty, or the first difference.
        integer(int32), allocatable :: i32a(:), i32b(:), i32va(:,:), i32vb(:,:)
        integer(int64), allocatable :: i64a(:), i64b(:), i64va(:,:), i64vb(:,:)
        real(real32), allocatable :: f32a(:), f32b(:), f32va(:,:), f32vb(:,:)
        real(real64), allocatable :: f64a(:), f64b(:), f64va(:,:), f64vb(:,:)
        logical, allocatable :: la(:), lb(:), lva(:,:), lvb(:,:)
        character(len=:), allocatable :: ca(:), cb(:), cva(:,:), cvb(:,:)
        type(parquet_date), allocatable :: da(:), db(:), dva(:,:), dvb(:,:)
        type(parquet_time), allocatable :: tma(:), tmb(:), tmva(:,:), tmvb(:,:)
        type(parquet_timestamp), allocatable :: tsa(:), tsb(:), tsva(:,:), tsvb(:,:)
        integer :: i, e
        !
        why = ""
        if (ta%kind(name) /= tb%kind(name)) then
            why = "kind differs"
            return
        end if
        if (ta%width(name) /= tb%width(name)) then
            why = "width differs"
            return
        end if
        if (ta%nrows() /= tb%nrows()) then
            why = "row count differs"
            return
        end if
        select case (ta%kind(name))
        case (PK_INT32)
            call ta%get(name, i32a)
            call tb%get(name, i32b)
            if (any(i32a /= i32b)) why = "int32 values differ"
        case (PK_INT64)
            call ta%get(name, i64a)
            call tb%get(name, i64b)
            if (any(i64a /= i64b)) why = "int64 values differ"
        case (PK_FLOAT32)
            call ta%get(name, f32a)
            call tb%get(name, f32b)
            if (any(f32a /= f32b)) why = "float32 values differ"
        case (PK_FLOAT64)
            call ta%get(name, f64a)
            call tb%get(name, f64b)
            if (any(f64a /= f64b)) why = "float64 values differ"
        case (PK_LOGICAL)
            call ta%get(name, la)
            call tb%get(name, lb)
            if (any(la .neqv. lb)) why = "logical values differ"
        case (PK_STRING)
            call ta%get(name, ca)
            call tb%get(name, cb)
            do i = 1, size(ca)
                if (trim(ca(i)) /= trim(cb(i))) why = "string values differ"
            end do
        case (PK_DATE)
            call ta%get(name, da)
            call tb%get(name, db)
            do i = 1, size(da)
                if (da(i) /= db(i)) why = "date values differ"
            end do
        case (PK_TIME)
            call ta%get(name, tma)
            call tb%get(name, tmb)
            do i = 1, size(tma)
                if (tma(i) /= tmb(i)) why = "time values differ"
            end do
        case (PK_TIMESTAMP)
            call ta%get(name, tsa)
            call tb%get(name, tsb)
            do i = 1, size(tsa)
                if (tsa(i) /= tsb(i)) why = "timestamp values differ"
            end do
        case (PK_INT32_VEC)
            call ta%get(name, i32va)
            call tb%get(name, i32vb)
            if (any(i32va /= i32vb)) why = "int32 vector values differ"
        case (PK_INT64_VEC)
            call ta%get(name, i64va)
            call tb%get(name, i64vb)
            if (any(i64va /= i64vb)) why = "int64 vector values differ"
        case (PK_FLOAT32_VEC)
            call ta%get(name, f32va)
            call tb%get(name, f32vb)
            if (any(f32va /= f32vb)) why = "float32 vector values differ"
        case (PK_FLOAT64_VEC)
            call ta%get(name, f64va)
            call tb%get(name, f64vb)
            if (any(f64va /= f64vb)) why = "float64 vector values differ"
        case (PK_LOGICAL_VEC)
            call ta%get(name, lva)
            call tb%get(name, lvb)
            if (any(lva .neqv. lvb)) why = "logical vector values differ"
        case (PK_STRING_VEC)
            call ta%get(name, cva)
            call tb%get(name, cvb)
            do i = 1, size(cva, 2)
                do e = 1, size(cva, 1)
                    if (trim(cva(e, i)) /= trim(cvb(e, i))) why = "string vector values differ"
                end do
            end do
        case (PK_DATE_VEC)
            call ta%get(name, dva)
            call tb%get(name, dvb)
            do i = 1, size(dva, 2)
                do e = 1, size(dva, 1)
                    if (dva(e, i) /= dvb(e, i)) why = "date vector values differ"
                end do
            end do
        case (PK_TIME_VEC)
            call ta%get(name, tmva)
            call tb%get(name, tmvb)
            do i = 1, size(tmva, 2)
                do e = 1, size(tmva, 1)
                    if (tmva(e, i) /= tmvb(e, i)) why = "time vector values differ"
                end do
            end do
        case (PK_TIMESTAMP_VEC)
            call ta%get(name, tsva)
            call tb%get(name, tsvb)
            do i = 1, size(tsva, 2)
                do e = 1, size(tsva, 1)
                    if (tsva(e, i) /= tsvb(e, i)) why = "timestamp vector values differ"
                end do
            end do
        case default
            why = "a kind this helper does not compare"
        end select
    end subroutine same_column
    !
    !> Fills a list, a map and a struct column with rows `lo..hi` of one deterministic fixture
    !! (ragged list rows, two-key maps, an int32 and a string field), with one null row of each
    !! kind at rows 4, 5 and 6 when the range covers them -- so a chunk built for a sub-range
    !! holds exactly the rows the whole fixture holds there.
    subroutine container_fixture(lo, hi, lc, mc, sc)
        integer, intent(in) :: lo                         !! first fixture row.
        integer, intent(in) :: hi                         !! last fixture row.
        type(parquet_list_column), intent(out) :: lc      !! list<int32>.
        type(parquet_map_column), intent(out) :: mc       !! map<string, int32>.
        type(parquet_struct_column), intent(out) :: sc    !! struct<id: int32, nm: string>.
        character(len=8) :: nm
        integer :: i
        integer(int64) :: k
        !
        call lc%init(PK_INT32)
        call mc%init(PK_INT32)
        call sc%init(["id ", "nm "], [PK_INT32, PK_STRING])
        do i = lo, hi
            k = int(i - lo + 1, int64)
            select case (mod(i, 3))
            case (0)
                call lc%append_row([int(10 * i + 1, int32)])
            case (1)
                call lc%append_row([int(10 * i + 1, int32), int(10 * i + 2, int32)])
            case default
                call lc%append_row([int(10 * i + 1, int32), int(10 * i + 2, int32), int(10 * i + 3, int32)])
            end select
            call mc%append_row(["k1", "k2"], [int(i, int32), int(-i, int32)])
            call sc%append_row()
            call sc%set_field(k, "id", int(7 * i, int32))
            write(nm, '(a, i0)') "obj_", i
            call sc%set_field(k, "nm", trim(nm))
            if (i == 4) call lc%set_null(k)
            if (i == 5) call mc%set_null(k)
            if (i == 6) call sc%set_null(k)
        end do
    end subroutine container_fixture
    !
    !> Two tables written as two row groups equal the one-shot write of their concatenation:
    !! the same rows in the same order, in two row groups whose bounds are the chunks' sizes,
    !! and the sidecar the derived schema writes carries the unit.
    subroutine test_chunked_write_equals_one_shot(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, a, b, ta, tb
        type(parquet_writer) :: w
        integer(int64) :: ids(NROW)
        real(real64) :: masses(NROW), vec(NVEC, NROW)
        character(len=8) :: names(NROW)
        integer(int64), allocatable :: ia(:), ib(:), bounds(:,:)
        real(real64), allocatable :: ma(:), mb(:), va(:,:), vb(:,:)
        character(len=:), allocatable :: na(:), nb(:), ub
        integer :: i
        integer, parameter :: SPLIT = 4
        character(len=*), parameter :: fone = "test_run/table_stream_chunk_one.parquet"
        character(len=*), parameter :: ftwo = "test_run/table_stream_chunk_two.parquet"
        character(len=*), parameter :: mtwo = "test_run/table_stream_chunk_two.maml"
        !
        call fixture_arrays(ids, masses, names, vec)
        call fixture_table(t)
        call parquet_write_table(t, fone)
        !
        call parquet_new_table(a)
        call a%add_column("id", ids(1:SPLIT))
        call a%add_column("mass", masses(1:SPLIT), unit="Msun")
        call a%add_column("name", names(1:SPLIT))
        call a%add_column("vec", vec(:, 1:SPLIT))
        call parquet_new_table(b)
        call b%add_column("id", ids(SPLIT + 1:))
        call b%add_column("mass", masses(SPLIT + 1:), unit="Msun")
        call b%add_column("name", names(SPLIT + 1:))
        call b%add_column("vec", vec(:, SPLIT + 1:))
        call parquet_open_writer_like(w, ftwo, a, write_maml=.true.)
        call parquet_write_table_chunk(w, a)
        call parquet_write_table_chunk(w, b)
        call parquet_close_writer(w)
        !
        call parquet_table_row_group_bounds(ftwo, bounds)
        call check(error, size(bounds, 2) == 2, "two chunks make two row groups")
        if (allocated(error)) return
        call check(error, bounds(2, 1) == int(SPLIT, int64), "the first row group holds exactly the first chunk's rows")
        if (allocated(error)) return
        call check(error, bounds(1, 2) == int(SPLIT + 1, int64), "the second row group starts where the first ended")
        if (allocated(error)) return
        !
        call parquet_open_table(ta, fone)
        call parquet_open_table(tb, ftwo, maml=mtwo)
        call check(error, tb%nrows() == ta%nrows(), "the chunked file holds every row")
        if (allocated(error)) return
        call ta%get("id", ia)
        call tb%get("id", ib)
        call check(error, all(ia == ib), "the id column matches the one-shot write, row for row")
        if (allocated(error)) return
        call ta%get("mass", ma)
        call tb%get("mass", mb)
        call check(error, all(ma == mb), "the mass column matches")
        if (allocated(error)) return
        call ta%get("name", na)
        call tb%get("name", nb)
        do i = 1, NROW
            call check(error, trim(na(i)) == trim(nb(i)), "the string column matches")
            if (allocated(error)) return
        end do
        call ta%get("vec", va)
        call tb%get("vec", vb)
        call check(error, all(va == vb), "the vector column matches, element for element")
        if (allocated(error)) return
        call tb%unit("mass", ub)
        call check(error, ub == "Msun", "the sidecar the derived schema wrote carries the unit")
    end subroutine test_chunked_write_equals_one_shot
    !
    !> Chunk 1 is Null-free, chunk 2 holds a Null: the Null reads back, and every column that
    !! takes a mask is stored nullable -- the column that never held a Null included, which is
    !! the visible sign that the mask was passed on the first row group. Without the always-mask
    !! rule the first row group fixes the field non-nullable and the second row group's mask is
    !! a hard abort inside the writer (feature_risks.md Risk-226).
    subroutine test_chunked_write_nullability_survives_a_late_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        real(real64) :: x(NROW), vec(NVEC, NROW), got(NVEC, NROW)
        integer(int32) :: n(NROW)
        integer(int32), allocatable :: nback(:)
        logical :: elem_valid(NVEC, NROW)
        logical :: nullable
        integer :: i, j
        integer, parameter :: SPLIT = 4
        character(len=*), parameter :: f = "test_run/table_stream_chunk_late_null.parquet"
        !
        do i = 1, NROW
            x(i) = real(i, real64)
            n(i) = 10 * i
            do j = 1, NVEC
                vec(j, i) = real(100 * i + j, real64)
            end do
        end do
        call parquet_new_table(a)
        call a%add_column("x", x(1:SPLIT))
        call a%add_column("n", n(1:SPLIT))
        call a%add_column("vec", vec(:, 1:SPLIT))
        call parquet_new_table(b)
        call b%add_column("x", x(SPLIT + 1:))
        call b%add_column("n", n(SPLIT + 1:))
        call b%add_column("vec", vec(:, SPLIT + 1:))
        call b%set_null("x", 2_int64)               ! row SPLIT + 2 of the file
        call b%set_null("vec", 3_int64, 2_int64)    ! element 2 of row SPLIT + 3
        !
        call parquet_open_writer_like(w, f, a)
        call parquet_write_table_chunk(w, a)
        call parquet_write_table_chunk(w, b)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_nullable(r, "x", nullable)
        call check(error, nullable, "a column that received a Null in its second row group is nullable")
        if (allocated(error)) return
        call parquet_get_column_nullable(r, "n", nullable)
        call check(error, nullable, "a column that never held a Null is nullable too: the mask is passed regardless")
        if (allocated(error)) return
        call parquet_read_column(r, "vec", got, is_valid=elem_valid)
        call check(error, .not. elem_valid(2, SPLIT + 3), "the null element reads back null")
        if (allocated(error)) return
        call check(error, count(.not. elem_valid) == 1, "...and it is the only null element")
        if (allocated(error)) return
        call check(error, got(1, SPLIT + 3) == vec(1, SPLIT + 3), "its sibling elements keep their values")
        if (allocated(error)) return
        call parquet_close_reader(r)
        !
        call parquet_open_table(back, f)
        call check(error, back%is_null("x", int(SPLIT + 2, int64)), "the null row reads back null")
        if (allocated(error)) return
        call check(error, .not. back%is_null("x", int(SPLIT + 1, int64)), "its neighbour does not")
        if (allocated(error)) return
        call back%get("n", nback)
        call check(error, all(nback == n), "the never-null column round-trips")
    end subroutine test_chunked_write_nullability_survives_a_late_null
    !
    !> `set_protected` on the derived schema is the opt-out from the always-present mask: the
    !! protected column's field is stored non-nullable while its unprotected neighbour, written
    !! by the same chunks, is nullable -- and both round-trip. A Null in the protected column is
    !! the `write_table_chunk_protected_null` scenario.
    subroutine test_chunked_write_protected_column_is_unmasked(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, back
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        real(real64) :: x(NROW)
        integer(int32) :: n(NROW)
        integer(int32), allocatable :: nback(:)
        logical :: nullable
        integer :: i
        integer, parameter :: SPLIT = 4
        character(len=*), parameter :: f = "test_run/table_stream_chunk_protected.parquet"
        !
        do i = 1, NROW
            x(i) = real(i, real64)
            n(i) = 10 * i
        end do
        call parquet_new_table(a)
        call a%add_column("x", x(1:SPLIT))
        call a%add_column("n", n(1:SPLIT))
        call parquet_new_table(b)
        call b%add_column("x", x(SPLIT + 1:))
        call b%add_column("n", n(SPLIT + 1:))
        call b%set_null("x", 2_int64)
        call parquet_derive_schema(a, s)
        call s%set_protected("n")
        !
        call parquet_open_writer_like(w, f, a, schema=s)
        call parquet_write_table_chunk(w, a)
        call parquet_write_table_chunk(w, b)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_nullable(r, "n", nullable)
        call check(error, .not. nullable, "the protected column's field is non-nullable: its mask was erased")
        if (allocated(error)) return
        call parquet_get_column_nullable(r, "x", nullable)
        call check(error, nullable, "negative control: the unprotected neighbour, written by the same chunks, is nullable")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call parquet_open_table(back, f)
        call back%get("n", nback)
        call check(error, all(nback == n), "the protected column's values round-trip")
        if (allocated(error)) return
        call check(error, back%is_null("x", int(SPLIT + 2, int64)), "the unprotected column's Null reads back")
    end subroutine test_chunked_write_protected_column_is_unmasked
    !
    !> Every one of the 18 scalar and vector kinds round-trips through the chunked path: the
    !! fixture is written whole by the writer, read back as two slices, written as two row
    !! groups, and compared column by column against the one-shot table write of the same rows.
    !! A `case` wired to the wrong specific shows up as a value or kind difference.
    subroutine test_chunked_write_every_kind(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: full, s1, s2, ta, tb
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: cn(:), why
        integer(int32) :: nrg
        integer :: i
        integer, parameter :: SPLIT = 3
        character(len=*), parameter :: fsrc = "test_run/table_stream_kinds_src.parquet"
        character(len=*), parameter :: fone = "test_run/table_stream_kinds_one.parquet"
        character(len=*), parameter :: ftwo = "test_run/table_stream_kinds_two.parquet"
        !
        call write_all_kinds_fixture(fsrc)
        call parquet_open_table(full, fsrc)
        call full%materialize_all()
        call parquet_write_table(full, fone)
        !
        call parquet_open_table(s1, fsrc, 1, SPLIT)
        call s1%materialize_all()
        call parquet_open_table(s2, fsrc, SPLIT + 1, NKROW)
        call s2%materialize_all()
        call parquet_open_writer_like(w, ftwo, s1)
        call parquet_write_table_chunk(w, s1)
        call parquet_write_table_chunk(w, s2)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, ftwo)
        call parquet_get_num_row_groups(r, nrg)
        call check(error, nrg == 2, "two chunks make two row groups")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call parquet_open_table(ta, fone)
        call parquet_open_table(tb, ftwo)
        call ta%column_names(cn)
        call check(error, size(cn) == 18, "precondition: the fixture holds every scalar and vector kind")
        if (allocated(error)) return
        do i = 1, size(cn)
            call check(error, tb%has_column(trim(cn(i))), "the chunked file holds column " // trim(cn(i)))
            if (allocated(error)) return
            call same_column(ta, tb, trim(cn(i)), why)
            call check(error, len(why) == 0, "column " // trim(cn(i)) // ": " // why)
            if (allocated(error)) return
        end do
    end subroutine test_chunked_write_every_kind
    !
    !> A list, a map, a struct and a scalar string column -- the kinds whose nulls live inside
    !! the column rather than in a mask, the string one on the compact `parquet_string_column`
    !! path -- through the chunked path, against the one-shot write of the same rows. Null rows
    !! of each container kind sit in the second chunk, so the always-mask rule is seen to leave
    !! these kinds alone.
    subroutine test_chunked_write_containers(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_list_column), target :: lc, la, lb
        type(parquet_map_column), target :: mc, ma, mb
        type(parquet_struct_column), target :: sc, sa, sb
        type(parquet_list_row) :: lh
        type(parquet_map_row) :: mh
        type(parquet_struct_row) :: sh
        type(parquet_table) :: t, a, b, ta, tb
        type(parquet_writer) :: w
        type(parquet_reader) :: ra, rb
        character(len=8) :: names(NROW)
        character(len=:), allocatable :: na(:), nb(:), fa, fb
        integer(int32), allocatable :: ea(:), eb(:)
        integer(int32) :: va, vb, ia, ib
        integer(int64) :: k
        integer :: i
        integer, parameter :: SPLIT = 4
        character(len=*), parameter :: fone = "test_run/table_stream_cont_one.parquet"
        character(len=*), parameter :: ftwo = "test_run/table_stream_cont_two.parquet"
        !
        names(1) = "a"
        do i = 2, NROW
            write(names(i), '(a, i0)') "obj_", i
        end do
        call container_fixture(1, NROW, lc, mc, sc)
        call parquet_new_table(t)
        call t%add_column("lst", lc)
        call t%add_column("mp", mc)
        call t%add_column("st", sc)
        call t%add_column("s", names)
        call parquet_write_table(t, fone)
        !
        call container_fixture(1, SPLIT, lc, mc, sc)
        call parquet_new_table(a)
        call a%add_column("lst", lc)
        call a%add_column("mp", mc)
        call a%add_column("st", sc)
        call a%add_column("s", names(1:SPLIT))
        call container_fixture(SPLIT + 1, NROW, lc, mc, sc)
        call parquet_new_table(b)
        call b%add_column("lst", lc)
        call b%add_column("mp", mc)
        call b%add_column("st", sc)
        call b%add_column("s", names(SPLIT + 1:))
        call parquet_open_writer_like(w, ftwo, a)
        call parquet_write_table_chunk(w, a)
        call parquet_write_table_chunk(w, b)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(ra, fone)
        call parquet_open_reader(rb, ftwo)
        call parquet_read_column(ra, "lst", la)
        call parquet_read_column(rb, "lst", lb)
        call check(error, lb%size() == la%size(), "the list column holds every row")
        if (allocated(error)) return
        call check(error, lb%total_elements() == la%total_elements(), "...and every element")
        if (allocated(error)) return
        call parquet_read_column(ra, "mp", ma)
        call parquet_read_column(rb, "mp", mb)
        call check(error, mb%size() == ma%size(), "the map column holds every row")
        if (allocated(error)) return
        call check(error, mb%total_entries() == ma%total_entries(), "...and every entry")
        if (allocated(error)) return
        call parquet_read_column(ra, "st", sa)
        call parquet_read_column(rb, "st", sb)
        call check(error, sb%size() == sa%size(), "the struct column holds every row")
        if (allocated(error)) return
        ! The string column through the table layer, whose %get sizes the receiver itself.
        call parquet_open_table(ta, fone)
        call parquet_open_table(tb, ftwo)
        call ta%get("s", na)
        call tb%get("s", nb)
        call check(error, size(nb) == size(na), "the string column holds every row")
        if (allocated(error)) return
        do i = 1, NROW
            k = int(i, int64)
            call check(error, lb%is_null(k) .eqv. la%is_null(k), "list null rows agree")
            if (allocated(error)) return
            if (.not. la%is_null(k)) then
                lh = la%view(k)
                call lh%get(ea)
                lh = lb%view(k)
                call lh%get(eb)
                call check(error, size(eb) == size(ea), "list row lengths agree")
                if (allocated(error)) return
                call check(error, all(ea == eb), "list elements agree")
                if (allocated(error)) return
            end if
            call check(error, mb%is_null(k) .eqv. ma%is_null(k), "map null rows agree")
            if (allocated(error)) return
            if (.not. ma%is_null(k)) then
                mh = ma%view(k)
                call mh%get("k2", va)
                mh = mb%view(k)
                call mh%get("k2", vb)
                call check(error, va == vb, "map values agree")
                if (allocated(error)) return
            end if
            call check(error, sb%is_null(k) .eqv. sa%is_null(k), "struct null rows agree")
            if (allocated(error)) return
            if (.not. sa%is_null(k)) then
                sh = sa%view(k)
                call sh%get_field("id", ia)
                call sh%get_field("nm", fa)
                sh = sb%view(k)
                call sh%get_field("id", ib)
                call sh%get_field("nm", fb)
                call check(error, ia == ib, "struct int32 fields agree")
                if (allocated(error)) return
                call check(error, fa == fb, "struct string fields agree")
                if (allocated(error)) return
            end if
            call check(error, trim(nb(i)) == trim(na(i)), "string values agree")
            if (allocated(error)) return
        end do
        call parquet_close_reader(ra)
        call parquet_close_reader(rb)
    end subroutine test_chunked_write_containers
    !
    !> `row_mask=` drops rows from the output entirely, chunk by chunk, while a Null stays
    !! indexed against the table's own rows (Risk-24): a Null at row 2 of a chunk whose row 3 is
    !! dropped is still the output's row 2, and the dropped row leaves no trace.
    subroutine test_chunked_write_row_mask(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, back
        type(parquet_writer) :: w
        real(real64) :: x(NROW)
        real(real64), allocatable :: got(:)
        integer(int64), allocatable :: bounds(:,:)
        logical :: keep_a(4), keep_b(NROW - 4)
        integer :: i
        integer, parameter :: SPLIT = 4
        character(len=*), parameter :: f = "test_run/table_stream_chunk_row_mask.parquet"
        !
        do i = 1, NROW
            x(i) = real(i, real64)
        end do
        call parquet_new_table(a)
        call a%add_column("x", x(1:SPLIT))
        call a%set_null("x", 2_int64)
        call parquet_new_table(b)
        call b%add_column("x", x(SPLIT + 1:))
        keep_a = [.true., .true., .false., .true.]
        keep_b = .true.
        keep_b(1) = .false.
        keep_b(NROW - SPLIT) = .false.
        !
        call parquet_open_writer_like(w, f, a)
        call parquet_write_table_chunk(w, a, row_mask=keep_a)
        call parquet_write_table_chunk(w, b, row_mask=keep_b)
        call parquet_close_writer(w)
        !
        call parquet_table_row_group_bounds(f, bounds)
        call check(error, size(bounds, 2) == 2, "two masked chunks make two row groups")
        if (allocated(error)) return
        call check(error, bounds(2, 1) == 3_int64, "the first row group holds the three kept rows")
        if (allocated(error)) return
        call check(error, bounds(2, 2) == 7_int64, "the second holds the four kept rows")
        if (allocated(error)) return
        call parquet_open_table(back, f)
        call check(error, back%nrows() == 7_int64, "every dropped row is absent")
        if (allocated(error)) return
        call check(error, back%is_null("x", 2_int64), "the Null keeps its pre-mask row: output row 2")
        if (allocated(error)) return
        call back%get("x", got)
        call check(error, got(1) == x(1), "row 1 kept")
        if (allocated(error)) return
        call check(error, got(3) == x(4), "row 3 of the output is the chunk's row 4: its row 3 was dropped")
        if (allocated(error)) return
        call check(error, got(4) == x(SPLIT + 2), "the second chunk's first kept row follows")
        if (allocated(error)) return
        call check(error, got(7) == x(NROW - 1), "...through its last kept row")
        if (allocated(error)) return
        do i = 3, 7
            call check(error, .not. back%is_null("x", int(i, int64)), "no other row is null")
            if (allocated(error)) return
        end do
    end subroutine test_chunked_write_row_mask
    !
    !> A field disabled with `set_column_unavailable` is skipped: the file has no such column,
    !! and the chunk table need not carry it.
    subroutine test_chunked_write_skips_a_disabled_field(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, a
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        integer(int64) :: ids(NROW)
        real(real64) :: masses(NROW), vec(NVEC, NROW)
        character(len=8) :: names(NROW)
        character(len=:), allocatable :: cn(:)
        character(len=*), parameter :: f = "test_run/table_stream_chunk_disabled.parquet"
        !
        call fixture_arrays(ids, masses, names, vec)
        call fixture_table(t)
        call parquet_derive_schema(t, s)
        call s%set_column_unavailable("name")
        call parquet_new_table(a)
        call a%add_column("id", ids)
        call a%add_column("mass", masses, unit="Msun")
        call a%add_column("vec", vec)
        !
        call parquet_open_writer(w, f, s)
        call check(error, .not. parquet_is_column_enabled(w, "name"), "precondition: the field is disabled")
        if (allocated(error)) return
        call parquet_write_table_chunk(w, a)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_names(r, cn)
        call check(error, size(cn) == 3, "the disabled field is absent from the file")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(r, "name"), "...by name too")
        if (allocated(error)) return
        call check(error, trim(cn(1)) == "id" .and. trim(cn(2)) == "mass" .and. trim(cn(3)) == "vec", &
            "the enabled fields keep the schema's order")
        call parquet_close_reader(r)
    end subroutine test_chunked_write_skips_a_disabled_field
    !
    !> A schema-less writer takes the schema-less table write's rule: every resident column in
    !! slot order, the unread column and `parquet_row_index` never, the file's columns fixed by
    !! the first row group. Residency is arranged in reverse order to show that slot order, not
    !! read order, decides.
    subroutine test_chunked_write_on_a_schemaless_writer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, s1, s2, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        integer(int32) :: a(NROW), c(NROW)
        real(real64) :: b(NROW)
        integer(int32), allocatable :: aback(:), cback(:)
        integer(int32) :: nrg
        character(len=:), allocatable :: cn(:)
        integer :: i
        integer, parameter :: SPLIT = 5
        character(len=*), parameter :: fsrc = "test_run/table_stream_schemaless_src.parquet"
        character(len=*), parameter :: f = "test_run/table_stream_schemaless_out.parquet"
        !
        do i = 1, NROW
            a(i) = i
            b(i) = real(i, real64)
            c(i) = 100 * i
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%add_column("c", c)
        call parquet_write_table(t, fsrc)
        !
        call parquet_open_table(s1, fsrc, 1, SPLIT)
        call s1%prefetch("c")
        call s1%prefetch("a")
        call s1%prefetch(PARQUET_ROW_INDEX)
        call parquet_open_table(s2, fsrc, SPLIT + 1, NROW)
        call s2%prefetch("c")
        call s2%prefetch("a")
        call s2%prefetch(PARQUET_ROW_INDEX)
        call parquet_open_writer(w, f, chunk_size=SPLIT)
        call parquet_write_table_chunk(w, s1)
        call parquet_write_table_chunk(w, s2)
        call parquet_close_writer(w)
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_names(r, cn)
        call check(error, size(cn) == 2, "the two resident columns are written, the unread one and the row index are not")
        if (allocated(error)) return
        call check(error, trim(cn(1)) == "a" .and. trim(cn(2)) == "c", "...in slot order, whatever the read order")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(r, PARQUET_ROW_INDEX), "parquet_row_index is never written unasked")
        if (allocated(error)) return
        call parquet_get_num_row_groups(r, nrg)
        call check(error, nrg == 2, "two chunks make two row groups")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call parquet_open_table(back, f)
        call back%get("a", aback)
        call back%get("c", cback)
        call check(error, all(aback == a), "the first column round-trips across both row groups")
        if (allocated(error)) return
        call check(error, all(cback == c), "so does the second")
    end subroutine test_chunked_write_on_a_schemaless_writer
    !
    !> A two-column in-memory table over rows `lo..hi` of one deterministic fixture (`id` is the
    !! row number, `x` ten times it), so the batches of a sink test are pieces of one sequence.
    subroutine batch_table(lo, hi, t, without_x)
        integer, intent(in) :: lo                     !! first fixture row.
        integer, intent(in) :: hi                     !! last fixture row.
        type(parquet_table), intent(out) :: t         !! receives the batch.
        logical, intent(in), optional :: without_x    !! .true.: only the id column.
        integer(int64), allocatable :: ids(:)
        real(real64), allocatable :: x(:)
        integer :: i, n
        logical :: with_x
        !
        with_x = .true.
        if (present(without_x)) with_x = .not. without_x
        n = max(hi - lo + 1, 0)
        allocate(ids(n), x(n))
        do i = 1, n
            ids(i) = int(lo + i - 1, int64)
            x(i) = 10.0_real64 * real(lo + i - 1, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("id", ids)
        if (with_x) call t%add_column("x", x)
    end subroutine batch_table
    !
    !> Seven appends of uneven size against `chunk_size=64`: every row present and in order, and
    !! the row groups are exactly 103, 251 and 10 -- the whole buffer written after the append
    !! that crossed the threshold, the last by the close (contract 3). The counters are asserted
    !! after every append, so a flush before the threshold, or on an append that did not cross
    !! it, is caught at the step it happens.
    subroutine test_sink_uneven_batches(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b, back
        integer(int64), allocatable :: ids(:), bounds(:,:)
        integer, parameter :: sizes(7) = [3, 100, 1, 50, 200, 1, 9]
        integer(int64), parameter :: pending(7) = [3_int64, 0_int64, 1_int64, 51_int64, 0_int64, 1_int64, 10_int64]
        integer(int64), parameter :: groups(7) = [0_int64, 1_int64, 1_int64, 1_int64, 2_int64, 2_int64, 2_int64]
        integer :: k, lo, hi, i
        character(len=*), parameter :: f = "test_run/table_stream_sink_uneven.parquet"
        !
        call batch_table(1, 3, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=64)
        call check(error, out%is_open(), "the sink is open")
        if (allocated(error)) return
        call check(error, out%chunk_size() == 64, "chunk_size= is the threshold")
        if (allocated(error)) return
        call check(error, out%nrows() == 0_int64, "the template's rows are not written")
        if (allocated(error)) return
        lo = 1
        do k = 1, size(sizes)
            hi = lo + sizes(k) - 1
            call batch_table(lo, hi, b)
            call out%append(b)
            call check(error, out%rows_pending() == pending(k), "rows pending after an append")
            if (allocated(error)) return
            call check(error, out%row_groups() == groups(k), "row groups written after an append")
            if (allocated(error)) return
            call check(error, out%nrows() == int(hi, int64), "rows accepted so far")
            if (allocated(error)) return
            lo = hi + 1
        end do
        call parquet_close_table_writer(out)
        call check(error, .not. out%is_open(), "the sink is closed")
        if (allocated(error)) return
        !
        call parquet_table_row_group_bounds(f, bounds)
        call check(error, size(bounds, 2) == 3, "three row groups: two flushes and the close")
        if (allocated(error)) return
        call check(error, bounds(2, 1) == 103_int64, "the first row group is the 103 rows that crossed the threshold")
        if (allocated(error)) return
        call check(error, bounds(2, 2) == 354_int64, "the second is the next 251")
        if (allocated(error)) return
        call check(error, bounds(2, 3) == 364_int64, "the last is the 10 rows the close wrote")
        if (allocated(error)) return
        call parquet_open_table(back, f)
        call back%get("id", ids)
        call check(error, size(ids) == 364, "every row is in the file")
        if (allocated(error)) return
        do i = 1, 364
            call check(error, ids(i) == int(i, int64), "...in order")
            if (allocated(error)) return
        end do
    end subroutine test_sink_uneven_batches
    !
    !> Without `chunk_size=` the threshold is the library's estimate from the schema, with every
    !! vector column's width resolved from the template first: a wide vector column gives a
    !! threshold far below the one a width-1 reading of the same schema gives (feature_risks.md
    !! Risk-228).
    subroutine test_sink_auto_chunk_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed
        type(parquet_schema) :: s
        type(parquet_writer) :: w
        integer, parameter :: WIDE = 100
        real(real64) :: v(WIDE, 2)
        integer(int64) :: ids(2)
        integer :: c_sink, c_width1, cs
        character(len=:), allocatable :: fname
        character(len=*), parameter :: f = "test_run/table_stream_sink_auto.parquet"
        character(len=*), parameter :: f1 = "test_run/table_stream_sink_auto_width1.parquet"
        !
        v = 1.0_real64
        ids = [1_int64, 2_int64]
        call parquet_new_table(seed)
        call seed%add_column("id", ids)
        call seed%add_column("v", v)
        call parquet_open_table_writer(out, f, seed)
        c_sink = out%chunk_size()
        call parquet_close_table_writer(out)
        ! The width-1 reading: the derived schema as it stands, col_size still auto.
        call parquet_derive_schema(seed, s)
        call s%get_field(2, fname, col_size=cs)
        call check(error, cs == parquet_size_auto, "precondition: the derived schema leaves col_size auto")
        if (allocated(error)) return
        call parquet_open_writer(w, f1, s)
        call parquet_get_chunk_size(w, c_width1)
        call parquet_write_column(w, "id", ids)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        call check(error, c_sink >= 1000, "the estimate is clamped from below")
        if (allocated(error)) return
        call check(error, c_sink * 10 < c_width1, &
            "the sink's threshold sees the vector width, so it is far below the width-1 estimate")
    end subroutine test_sink_auto_chunk_size
    !
    !> `chunk_size=` reaches both the flush rule and `%chunk_size()`.
    subroutine test_sink_explicit_chunk_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b
        character(len=*), parameter :: f = "test_run/table_stream_sink_explicit.parquet"
        !
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=5)
        call check(error, out%chunk_size() == 5, "%chunk_size() reports the explicit value")
        if (allocated(error)) return
        call batch_table(1, 4, b)
        call out%append(b)
        call check(error, out%row_groups() == 0_int64, "four rows against five: nothing written yet")
        if (allocated(error)) return
        call batch_table(5, 5, b)
        call out%append(b)
        call check(error, out%row_groups() == 1_int64, "the fifth row crosses the threshold and flushes")
        if (allocated(error)) return
        call check(error, out%rows_pending() == 0_int64, "...leaving nothing pending")
        if (allocated(error)) return
        call parquet_close_table_writer(out)
    end subroutine test_sink_explicit_chunk_size
    !
    !> `%flush()` makes a row group of exactly the pending rows, and a second `%flush()` with
    !! nothing pending writes nothing (an empty row group would be the writer's abort).
    subroutine test_sink_flush_boundary(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b
        integer(int64), allocatable :: bounds(:,:)
        character(len=*), parameter :: f = "test_run/table_stream_sink_flush.parquet"
        !
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=100)
        call batch_table(1, 3, b)
        call out%append(b)
        call out%flush()
        call check(error, out%row_groups() == 1_int64, "a flush writes the pending rows as a row group")
        if (allocated(error)) return
        call check(error, out%rows_pending() == 0_int64, "...and empties the buffer")
        if (allocated(error)) return
        call out%flush()
        call check(error, out%row_groups() == 1_int64, "a flush with nothing pending writes nothing")
        if (allocated(error)) return
        call batch_table(4, 5, b)
        call out%append(b)
        call parquet_close_table_writer(out)
        call parquet_table_row_group_bounds(f, bounds)
        call check(error, size(bounds, 2) == 2, "the flush and the close made one row group each")
        if (allocated(error)) return
        call check(error, bounds(2, 1) == 3_int64, "the flushed row group holds exactly the pending rows")
        if (allocated(error)) return
        call check(error, bounds(2, 2) == 5_int64, "the close wrote the rest")
    end subroutine test_sink_flush_boundary
    !
    !> Rows below the threshold at close are in the file, as the final row group.
    subroutine test_sink_close_writes_the_last_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b, back
        type(parquet_reader) :: r
        integer(int32) :: nrg
        character(len=*), parameter :: f = "test_run/table_stream_sink_last.parquet"
        !
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=100)
        call batch_table(1, 7, b)
        call out%append(b)
        call check(error, out%row_groups() == 0_int64, "precondition: nothing written before the close")
        if (allocated(error)) return
        call parquet_close_table_writer(out)
        call parquet_open_reader(r, f)
        call parquet_get_num_row_groups(r, nrg)
        call check(error, nrg == 1, "the close wrote one row group")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call parquet_open_table(back, f)
        call check(error, back%nrows() == 7_int64, "...holding every pending row")
    end subroutine test_sink_close_writes_the_last_rows
    !
    !> A sink that accepted no rows closes to a valid file carrying every column and zero rows
    !! (the writer warns; the file is right).
    subroutine test_sink_close_with_no_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, back
        character(len=:), allocatable :: cn(:)
        character(len=*), parameter :: f = "test_run/table_stream_sink_norows.parquet"
        !
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed)
        call parquet_close_table_writer(out)
        call parquet_open_table(back, f)
        call check(error, back%nrows() == 0_int64, "no rows")
        if (allocated(error)) return
        call back%column_names(cn)
        call check(error, size(cn) == 2, "...but every column the template declared")
        if (allocated(error)) return
        call check(error, trim(cn(1)) == "id" .and. trim(cn(2)) == "x", "...by name and in order")
    end subroutine test_sink_close_with_no_rows
    !
    !> A column the appended table lacks is null in exactly those rows, on both sides of a flush
    !! boundary: the first row group is Null-free, the second is the null-filled one, and the
    !! always-present mask is what lets the second carry Nulls at all.
    subroutine test_sink_null_fill_across_a_flush(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b, back
        type(parquet_reader) :: r
        real(real64), allocatable :: x(:)
        logical :: nullable
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_sink_nullfill.parquet"
        !
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=4)
        call batch_table(1, 4, b)
        call out%append(b)
        call check(error, out%row_groups() == 1_int64, "precondition: the Null-free batch was flushed on its own")
        if (allocated(error)) return
        call batch_table(5, 7, b, without_x=.true.)
        call out%append(b)
        call parquet_close_table_writer(out)
        !
        call parquet_open_reader(r, f)
        call parquet_get_column_nullable(r, "x", nullable)
        call check(error, nullable, "the null-filled column is nullable although its first row group held no Null")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call parquet_open_table(back, f)
        call check(error, back%nrows() == 7_int64, "every row is in the file")
        if (allocated(error)) return
        do i = 1, 4
            call check(error, .not. back%is_null("x", int(i, int64)), "the rows the batch supplied are not null")
            if (allocated(error)) return
        end do
        do i = 5, 7
            call check(error, back%is_null("x", int(i, int64)), "the rows the batch lacked are null")
            if (allocated(error)) return
        end do
        call back%get("x", x)
        call check(error, x(4) == 40.0_real64, "a supplied value survives the flush")
    end subroutine test_sink_null_fill_across_a_flush
    !
    !> A template built with zero-length arrays gives the output the right columns: only its
    !! descriptors are read.
    subroutine test_sink_zero_row_template(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b, back
        integer(int64) :: no_ids(0)
        real(real64) :: no_x(0)
        character(len=:), allocatable :: cn(:)
        character(len=*), parameter :: f = "test_run/table_stream_sink_zero_template.parquet"
        !
        call parquet_new_table(seed)
        call seed%add_column("id", no_ids)
        call seed%add_column("x", no_x)
        call check(error, seed%nrows() == 0_int64, "precondition: a zero-row template")
        if (allocated(error)) return
        call parquet_open_table_writer(out, f, seed)
        call batch_table(1, 5, b)
        call out%append(b)
        call parquet_close_table_writer(out)
        call parquet_open_table(back, f)
        call back%column_names(cn)
        call check(error, size(cn) == 2, "the zero-row template's two columns")
        if (allocated(error)) return
        call check(error, back%kind("id") == PK_INT64 .and. back%kind("x") == PK_FLOAT64, "...with their kinds")
        if (allocated(error)) return
        call check(error, back%nrows() == 5_int64, "...and the appended rows")
    end subroutine test_sink_zero_row_template
    !
    !> The buffer keeps at least `chunk_size` rows of capacity across a flush, through the debug
    !! observer -- a reset without the reserve leaves it at zero, and every later append then
    !! grows it geometrically at full correctness (feature_risks.md Risk-227).
    subroutine test_sink_buffer_keeps_capacity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b
        integer(int64) :: cap0, cap1, cap2
        character(len=*), parameter :: f = "test_run/table_stream_sink_capacity.parquet"
        !
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=8)
        call parquet_debug_table_writer_capacity(out, cap0)
        call check(error, cap0 >= 8_int64, "reserved to chunk_size at open")
        if (allocated(error)) return
        call batch_table(1, 8, b)
        call out%append(b)
        call check(error, out%row_groups() == 1_int64, "precondition: the append flushed")
        if (allocated(error)) return
        call parquet_debug_table_writer_capacity(out, cap1)
        call check(error, cap1 >= 8_int64, "reserved to chunk_size again after the flush")
        if (allocated(error)) return
        call batch_table(9, 11, b)
        call out%append(b)
        call parquet_debug_table_writer_capacity(out, cap2)
        call check(error, cap2 == cap1, "an append below the threshold reallocates nothing")
        if (allocated(error)) return
        call parquet_close_table_writer(out)
    end subroutine test_sink_buffer_keeps_capacity
    !
    !> Capacity is unchanged across `chunk_size` consecutive `%append(row)` calls, before and after
    !! a flush (contract 7): no row append reallocates.
    subroutine test_sink_row_appends_do_not_reallocate(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, src
        integer(int64) :: cap0, cap, cap1
        integer :: i
        integer, parameter :: CHUNK = 6
        character(len=*), parameter :: f = "test_run/table_stream_sink_rowcap.parquet"
        !
        call batch_table(1, 2, seed)
        call batch_table(1, 2 * CHUNK, src)
        call parquet_open_table_writer(out, f, seed, chunk_size=CHUNK)
        call parquet_debug_table_writer_capacity(out, cap0)
        do i = 1, CHUNK - 1
            call out%append(src%row(i))
            call parquet_debug_table_writer_capacity(out, cap)
            call check(error, cap == cap0, "a row append below the threshold reallocates nothing")
            if (allocated(error)) return
        end do
        call out%append(src%row(CHUNK))
        call check(error, out%row_groups() == 1_int64, "the chunk_size-th row flushes")
        if (allocated(error)) return
        call parquet_debug_table_writer_capacity(out, cap1)
        call check(error, cap1 >= int(CHUNK, int64), "the buffer is reserved again after the flush")
        if (allocated(error)) return
        do i = CHUNK + 1, 2 * CHUNK - 1
            call out%append(src%row(i))
            call parquet_debug_table_writer_capacity(out, cap)
            call check(error, cap == cap1, "...and the next row group's appends reallocate nothing either")
            if (allocated(error)) return
        end do
        call check(error, out%nrows() == int(2 * CHUNK - 1, int64), "every row was accepted")
        if (allocated(error)) return
        call parquet_close_table_writer(out)
    end subroutine test_sink_row_appends_do_not_reallocate
    !
    !> A Null-free template, then batches holding Nulls: the Nulls read back and the field is
    !! nullable. The derived schema reads descriptors only; a derivation that measured the
    !! template's null state would have declared the column non-nullable and the second batch
    !! would abort inside the writer (feature_risks.md Risk-225, contract 8).
    subroutine test_derive_schema_does_not_measure_nullability(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed, b, back
        type(parquet_reader) :: r
        logical :: nullable
        character(len=*), parameter :: f = "test_run/table_stream_sink_nomeasure.parquet"
        !
        call batch_table(1, 3, seed)
        call parquet_open_table_writer(out, f, seed, chunk_size=3)
        call out%append(seed)
        call check(error, out%row_groups() == 1_int64, "precondition: the Null-free batch is its own row group")
        if (allocated(error)) return
        call batch_table(4, 5, b)
        call b%set_null("x", 1_int64)
        call out%append(b)
        call parquet_close_table_writer(out)
        call parquet_open_reader(r, f)
        call parquet_get_column_nullable(r, "x", nullable)
        call check(error, nullable, "a column Null-free in the template is still nullable in the file")
        if (allocated(error)) return
        call parquet_close_reader(r)
        call parquet_open_table(back, f)
        call check(error, back%is_null("x", 4_int64), "the later Null reads back")
        if (allocated(error)) return
        call check(error, .not. back%is_null("x", 3_int64), "...and only it")
    end subroutine test_derive_schema_does_not_measure_nullability
    !
    !> A file-backed slice with a never-read column is appended: the output names the column, so
    !! the append reads it, the file holds its values, and the slice's residency is what it was
    !! afterwards (read and given back).
    subroutine test_sink_reads_an_unread_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: src, tmpl, sl, back
        real(real64), allocatable :: x(:)
        integer :: i
        character(len=*), parameter :: fsrc = "test_run/table_stream_sink_unread_src.parquet"
        character(len=*), parameter :: f = "test_run/table_stream_sink_unread_out.parquet"
        !
        call batch_table(1, NROW, src)
        call parquet_write_table(src, fsrc)
        call parquet_open_table(tmpl, fsrc)
        call tmpl%materialize_all()
        call parquet_open_table_writer(out, f, tmpl)
        call parquet_open_table(sl, fsrc, 1, 5)
        call sl%prefetch("id")
        call check(error, sl%residency("x") == RES_EMPTY, "precondition: x is not read")
        if (allocated(error)) return
        call out%append(sl)
        call check(error, sl%residency("x") == RES_EMPTY, "what the append read, it gave back")
        if (allocated(error)) return
        call check(error, sl%residency("id") == RES_FULL, "what the caller had read stays")
        if (allocated(error)) return
        call parquet_close_table_writer(out)
        call parquet_open_table(back, f)
        call back%get("x", x)
        call check(error, size(x) == 5, "the slice's rows are in the file")
        if (allocated(error)) return
        do i = 1, 5
            call check(error, .not. back%is_null("x", int(i, int64)), "the unread column was read, not null-filled")
            if (allocated(error)) return
            call check(error, x(i) == 10.0_real64 * real(i, real64), "...with its values")
            if (allocated(error)) return
        end do
    end subroutine test_sink_reads_an_unread_column
    !
    !> `%append(row)` over an unread column reads it once and leaves it resident: the values are
    !! in the file, and the source table holds the column after the first row.
    subroutine test_sink_row_append_keeps_what_it_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: src, tmpl, lazy, back
        real(real64), allocatable :: x(:)
        character(len=*), parameter :: fsrc = "test_run/table_stream_sink_rowread_src.parquet"
        character(len=*), parameter :: f = "test_run/table_stream_sink_rowread_out.parquet"
        !
        call batch_table(1, NROW, src)
        call parquet_write_table(src, fsrc)
        call parquet_open_table(tmpl, fsrc)
        call tmpl%materialize_all()
        call parquet_open_table_writer(out, f, tmpl)
        call parquet_open_table(lazy, fsrc)
        call lazy%prefetch("id")
        call out%append(lazy%row(1))
        call check(error, lazy%residency("x") == RES_FULL, "the row append read the column and kept it")
        if (allocated(error)) return
        call out%append(lazy%row(2))
        call parquet_close_table_writer(out)
        call parquet_open_table(back, f)
        call back%get("x", x)
        call check(error, size(x) == 2, "both rows are in the file")
        if (allocated(error)) return
        call check(error, x(1) == 10.0_real64 .and. x(2) == 20.0_real64, "...with the read column's values")
    end subroutine test_sink_row_append_keeps_what_it_read
    !
    !> With `schema=` naming two of a template's three columns, an appended table's third column
    !! is ignored and absent from the file, in the schema's order; without `schema=` the same
    !! append is refused (the `sink_extra_column_refused` scenario).
    subroutine test_sink_schema_selects_the_columns(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: t, back
        type(parquet_schema) :: s
        integer(int32) :: a(NROW), b(NROW), c(NROW)
        integer(int32), allocatable :: got(:)
        character(len=:), allocatable :: cn(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_sink_schema_select.parquet"
        !
        do i = 1, NROW
            a(i) = i
            b(i) = 10 * i
            c(i) = 100 * i
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%add_column("c", c)
        call s%init("picked")
        call s%add_field("c", "int32")
        call s%add_field("a", "int32")
        call parquet_open_table_writer(out, f, t, schema=s)
        call out%append(t)
        call parquet_close_table_writer(out)
        call parquet_open_table(back, f)
        call back%column_names(cn)
        call check(error, size(cn) == 2, "the schema's two fields, the third column ignored")
        if (allocated(error)) return
        call check(error, trim(cn(1)) == "c" .and. trim(cn(2)) == "a", "...in the schema's order")
        if (allocated(error)) return
        call back%get("c", got)
        call check(error, all(got == c), "the selected columns carry their values")
    end subroutine test_sink_schema_selects_the_columns
    !
    !> A string-only template with no `chunk_size=` resolves to the estimator's ceiling, because
    !! the estimate counts a string column as one byte per row (question 16). The day the
    !! estimator sizes strings, this assertion becomes an upper bound rather than an equality.
    subroutine test_sink_string_template_auto_chunk_size(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: seed
        character(len=8) :: names(NROW)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_stream_sink_string_auto.parquet"
        !
        names(1) = "a"
        do i = 2, NROW
            write(names(i), '(a, i0)') "obj_", i
        end do
        call parquet_new_table(seed)
        call seed%add_column("name", names)
        call parquet_open_table_writer(out, f, seed)
        call check(error, out%chunk_size() == 10000000, &
            "a string-only template resolves to the estimator's 10,000,000-row ceiling")
        if (allocated(error)) return
        call parquet_close_table_writer(out)
    end subroutine test_sink_string_template_auto_chunk_size
    !
    !> A template without the row index and an appended table with it resident: accepted, and the
    !! file has no such column.
    subroutine test_sink_row_index_ignored(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: src, seed, lazy
        type(parquet_reader) :: r
        character(len=:), allocatable :: cn(:)
        character(len=*), parameter :: fsrc = "test_run/table_stream_sink_rowidx_src.parquet"
        character(len=*), parameter :: f = "test_run/table_stream_sink_rowidx_out.parquet"
        !
        call batch_table(1, NROW, src)
        call parquet_write_table(src, fsrc)
        call batch_table(1, 2, seed)
        call parquet_open_table_writer(out, f, seed)
        call parquet_open_table(lazy, fsrc)
        call lazy%prefetch(PARQUET_ROW_INDEX)
        call lazy%materialize_all()
        call check(error, lazy%residency(PARQUET_ROW_INDEX) == RES_FULL, "precondition: the row index is resident")
        if (allocated(error)) return
        call out%append(lazy)
        call parquet_close_table_writer(out)
        call parquet_open_reader(r, f)
        call parquet_get_column_names(r, cn)
        call check(error, size(cn) == 2, "the two data columns and nothing else")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(r, PARQUET_ROW_INDEX), "the row index was dropped, not refused")
        call parquet_close_reader(r)
    end subroutine test_sink_row_index_ignored
    !
    !> A slice with an active row filter serves as a template: only its descriptors are read,
    !! its rows are not written, and appending it afterwards writes exactly its surviving rows.
    subroutine test_sink_template_slice_accepted(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table_writer) :: out
        type(parquet_table) :: src, sl, back
        logical, allocatable :: keep(:)
        integer(int64), allocatable :: ids(:)
        character(len=*), parameter :: fsrc = "test_run/table_stream_sink_slice_src.parquet"
        character(len=*), parameter :: f = "test_run/table_stream_sink_slice_out.parquet"
        !
        call batch_table(1, NROW, src)
        call parquet_write_table(src, fsrc)
        call parquet_open_table(sl, fsrc, 3, 8)
        call sl%materialize_all()
        allocate(keep(sl%nrows()))
        keep = .true.
        keep(2) = .false.
        call sl%filter_rows(keep)
        call check(error, sl%nrows() == 5_int64, "precondition: a filtered slice of five rows")
        if (allocated(error)) return
        call parquet_open_table_writer(out, f, sl)
        call check(error, out%nrows() == 0_int64, "the template's rows are not written")
        if (allocated(error)) return
        call out%append(sl)
        call parquet_close_table_writer(out)
        call parquet_open_table(back, f)
        call back%get("id", ids)
        call check(error, size(ids) == 5, "the surviving rows, and only those")
        if (allocated(error)) return
        call check(error, ids(1) == 3_int64 .and. ids(2) == 5_int64 .and. ids(5) == 8_int64, "...in order")
    end subroutine test_sink_template_slice_accepted
    !
end module test_table_stream
