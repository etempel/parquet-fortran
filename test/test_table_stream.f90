!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for writing a `parquet_table` out one row group at a time (`feature_pandas_S2.md`):
!> `parquet_derive_schema` and `parquet_open_writer_like` here, `parquet_write_table_chunk` and
!> the `parquet_table_writer` sink in the stages that follow.
!>
!> Every test writes its own fixtures under `test_run/` -- the tests of one suite run
!> concurrently. Abort paths live in test/error_scenarios.f90, driven from test_errors.f90.
module test_table_stream
    use parquet
    use parquet_tables
    use iso_fortran_env, only : int32, int64, real64
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
                test_open_writer_like_carries_metadata) &
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
end module test_table_stream
