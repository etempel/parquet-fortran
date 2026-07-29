!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the parquet_tables module (`parquet_table`, the eager read-only table layer).
!>
!> The suite is organised around what this layer actually has to get right:
!>
!> * **the round-trip matrix** -- write a fixture, open it as a table, read it back both ways
!>   (`%get` and `%col`), write it out again, reopen: values must survive unchanged. Repeated
!>   per kind rather than spot-checked, since each kind has its own materializer;
!> * **nulls per validity dispatch class** (column bitmap / embedded string column / inside the
!>   element), the same three-way split that makes parquet_column's own validity fragile;
!> * **widening**, which only the copy path does (the pointer path is exact-kind by design);
!> * **columns this library cannot read**, which must not stop a file from opening.
!>
!> Abort paths (`error stop`) cannot be exercised here because they kill the process -- they
!> live in test/error_scenarios.f90 as `table_*` scenarios, driven from test_errors.f90.
!>
!> Fixtures follow CLAUDE.md's rule for string arrays: the FIRST element is deliberately the
!> shortest, so a "sized from the first element" bug is actively provoked rather than avoided.
module test_table
    use parquet
    use parquet_tables
    use parquet_columns, only : PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, &
        PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP, PK_FLOAT64_VEC, PK_INT32_VEC, PK_STRING_VEC, &
        PK_INT64_VEC, PK_FLOAT32_VEC, PK_LOGICAL_VEC, PK_DATE_VEC, PK_TIME_VEC, PK_TIMESTAMP_VEC, &
        PK_NONE
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_table
    !
    !> Row count every fixture in this suite uses.
    integer, parameter :: NROW = 6
    !> Vector width every vector fixture in this suite uses.
    integer, parameter :: NVEC = 3
    !
contains
    !
    subroutine collect_tests_parquet_table(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("open reports rows, columns and names in file order", test_open_basics), &
            new_unittest("scalar numeric kinds round-trip through get/col/set/write", &
                test_roundtrip_scalar_numeric), &
            new_unittest("string columns round-trip in both the compact and character forms", &
                test_roundtrip_string), &
            new_unittest("temporal kinds round-trip and keep their own null state", &
                test_roundtrip_temporal), &
            new_unittest("vector kinds round-trip with (element, row) orientation", &
                test_roundtrip_vector), &
            new_unittest("get widens int32 to int64 and float32 to float64", test_widening), &
            new_unittest("col hands back a live pointer: writes through it are visible", &
                test_pointer_is_live), &
            new_unittest("nulls survive a round trip for all three validity dispatch classes", &
                test_nulls_roundtrip), &
            new_unittest("a column this library cannot read does not stop the file opening", &
                test_unsupported_column), &
            new_unittest("nested struct leaves become dotted columns", test_struct_leaves), &
            new_unittest("parquet_new_table plus add_column builds a table and writes it", &
                test_from_scratch), &
            new_unittest("add_column(force=) replaces a column of the same name", &
                test_add_column_force), &
            new_unittest("set replaces values without changing the row set", test_set_values), &
            new_unittest("row_mask writes a row subset and leaves the table untouched", &
                test_write_row_mask), &
            new_unittest("reopening the same table variable frees the old table first", &
                test_reopen_same_variable), &
            new_unittest("found= reports a missing column instead of aborting", test_soft_fail), &
            new_unittest("unit is carried by add_column and reported by %unit", test_unit), &
            new_unittest("all 18 kinds: read back, pointer-alias and write out", test_kind_matrix), &
            new_unittest("all 18 kinds: %col aliases and %set replaces", test_kind_matrix_col_set), &
            new_unittest("all 18 kinds: add_column builds a table from scratch", test_kind_matrix_add), &
            new_unittest("filename and file metadata are reported back from the source file", &
                test_filename_and_metadata) &
            ]
    end subroutine collect_tests_parquet_table
    !
    !> Writes the shared numeric/string fixture used by most tests below.
    subroutine write_basic_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer(int32) :: i32(NROW)
        integer(int64) :: i64(NROW)
        real(real32) :: f32(NROW)
        real(real64) :: f64(NROW)
        logical :: b(NROW)
        character(len=8) :: s(NROW)
        integer :: i
        !
        do i = 1, NROW
            i32(i) = i
            i64(i) = int(i, int64) * 1000000000_int64
            f32(i) = real(i, real32) * 0.5_real32
            f64(i) = real(i, real64) * 2.25_real64
            b(i) = mod(i, 2) == 0
        end do
        ! First element deliberately the shortest (CLAUDE.md).
        s = ["a       ", "bcd     ", "ef      ", "ghijklm ", "no      ", "p       "]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "i32", i32)
        call parquet_write_column(w, "i64", i64)
        call parquet_write_column(w, "f32", f32)
        call parquet_write_column(w, "f64", f64)
        call parquet_write_column(w, "b", b)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
    end subroutine write_basic_fixture
    !
    subroutine test_open_basics(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        character(len=*), parameter :: f = "test_run/table_basic.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call check(error, t%nrows() == NROW, "table should report the fixture's row count")
        if (allocated(error)) return
        call check(error, t%ncols() == 6, "table should report 6 columns")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, size(names) == 6, "column_names should return one entry per column")
        if (allocated(error)) return
        call check(error, trim(names(1)) == "i32", "column_names should preserve file order")
        if (allocated(error)) return
        call check(error, trim(names(6)) == "s", "column_names(6) should be the last written column")
        if (allocated(error)) return
        call check(error, t%has_column("f64"), "has_column should find an existing column")
        if (allocated(error)) return
        call check(error, .not. t%has_column("nope"), "has_column should not invent a column")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "a freshly opened table is not detached")
        if (allocated(error)) return
        call check(error, t%residency("f64") == RES_FULL, &
            "an eagerly materialized column should be RES_FULL")
    end subroutine test_open_basics
    !
    subroutine test_roundtrip_scalar_numeric(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        integer(int32), allocatable :: g32(:)
        integer(int64), allocatable :: g64(:)
        real(real32), allocatable :: r32(:)
        real(real64), allocatable :: r64(:)
        logical, allocatable :: gb(:)
        character(len=*), parameter :: f = "test_run/table_num.parquet"
        character(len=*), parameter :: fo = "test_run/table_num_out.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call check(error, t%kind("i32") == PK_INT32, "i32 column should resolve to PK_INT32")
        if (allocated(error)) return
        call check(error, t%kind("i64") == PK_INT64, "i64 column should resolve to PK_INT64")
        if (allocated(error)) return
        call check(error, t%kind("f32") == PK_FLOAT32, "f32 column should resolve to PK_FLOAT32")
        if (allocated(error)) return
        call check(error, t%kind("f64") == PK_FLOAT64, "f64 column should resolve to PK_FLOAT64")
        if (allocated(error)) return
        call check(error, t%kind("b") == PK_LOGICAL, "b column should resolve to PK_LOGICAL")
        if (allocated(error)) return
        call check(error, t%width("i32") == 1, "a scalar column's width should be 1")
        if (allocated(error)) return
        !
        call t%get("i32", g32)
        call check(error, all(g32 == [1, 2, 3, 4, 5, 6]), "i32 values should survive the read")
        if (allocated(error)) return
        call t%get("i64", g64)
        call check(error, g64(6) == 6000000000_int64, "i64 values beyond int32 range should survive")
        if (allocated(error)) return
        call t%get("f32", r32)
        call check(error, abs(r32(4) - 2.0_real32) < 1.0e-6_real32, "f32 values should survive")
        if (allocated(error)) return
        call t%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, "f64 values should survive")
        if (allocated(error)) return
        call t%get("b", gb)
        call check(error, all(gb .eqv. [.false., .true., .false., .true., .false., .true.]), &
            "logical values should survive the read")
        if (allocated(error)) return
        !
        ! Write back out through a schema, then reopen and compare.
        call s%init("num")
        call s%add_field("i32", "int32")
        call s%add_field("i64", "int64")
        call s%add_field("f32", "float32")
        call s%add_field("f64", "float64")
        call s%add_field("b", "boolean")
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s)
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == NROW, "the written table should have the same row count")
        if (allocated(error)) return
        call check(error, t2%ncols() == 5, "the written table should have the schema's columns only")
        if (allocated(error)) return
        call t2%get("f64", r64)
        call check(error, abs(r64(4) - 9.0_real64) < 1.0e-12_real64, &
            "f64 values should survive the write/reopen round trip")
        if (allocated(error)) return
        call t2%get("i64", g64)
        call check(error, g64(6) == 6000000000_int64, &
            "i64 values should survive the write/reopen round trip")
    end subroutine test_roundtrip_scalar_numeric
    !
    subroutine test_roundtrip_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_string_column) :: sc
        character(len=:), allocatable :: chr(:), one
        character(len=*), parameter :: f = "test_run/table_str.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call check(error, t%kind("s") == PK_STRING, "s column should resolve to PK_STRING")
        if (allocated(error)) return
        !
        ! Compact form: the parquet_string_column itself.
        call t%get("s", sc)
        call check(error, sc%size() == NROW, "the compact string form should hold every row")
        if (allocated(error)) return
        call sc%get(1_int64, one)
        call check(error, one == "a", "compact form element 1 should be 'a'")
        if (allocated(error)) return
        call sc%get(4_int64, one)
        call check(error, one == "ghijklm", "compact form element 4 should be the longest value")
        if (allocated(error)) return
        !
        ! Character-array form: width comes from the LONGEST element, not the first one -- this
        ! is the "sized from the first element" bug class, and the fixture's first element is
        ! deliberately the shortest so a regression shows up here.
        call t%get("s", chr)
        call check(error, len(chr) == 7, &
            "the character form should be as wide as the longest value (7), not the first (1)")
        if (allocated(error)) return
        call check(error, trim(chr(1)) == "a", "character form element 1 should be 'a'")
        if (allocated(error)) return
        call check(error, trim(chr(4)) == "ghijklm", &
            "character form element 4 should not be truncated")
    end subroutine test_roundtrip_string
    !
    subroutine test_roundtrip_temporal(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        type(parquet_date) :: d(NROW)
        type(parquet_date), allocatable :: gd(:)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_temporal.parquet"
        !
        do i = 1, NROW
            d(i) = parquet_date(2026, 7, i)
        end do
        ! A null element in the middle: temporal kinds carry validity inside the element, so this
        ! must survive without any is_valid mask anywhere on the path.
        d(3) = parquet_date()
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "d", d)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call check(error, t%kind("d") == PK_DATE, "d column should resolve to PK_DATE")
        if (allocated(error)) return
        call t%get("d", gd)
        call check(error, size(gd) == NROW, "every date row should be read")
        if (allocated(error)) return
        call check(error, gd(1)%year() == 2026 .and. gd(1)%day() == 1, &
            "date values should survive the read")
        if (allocated(error)) return
        call check(error, gd(3)%is_null(), "a null date should still be null after the read")
        if (allocated(error)) return
        call check(error, t%is_null("d", 3_int64), &
            "%is_null should agree with the element's own null state")
        if (allocated(error)) return
        call check(error, .not. t%is_null("d", 1_int64), &
            "a non-null date row should not report null")
    end subroutine test_roundtrip_temporal
    !
    subroutine test_roundtrip_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        real(real64) :: v(NVEC, NROW)
        real(real64), allocatable :: gv(:,:)
        real(real64), pointer :: pv(:,:)
        integer :: i, e
        character(len=*), parameter :: f = "test_run/table_vec.parquet"
        !
        do i = 1, NROW
            do e = 1, NVEC
                v(e, i) = real(i * 10 + e, real64)
            end do
        end do
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call check(error, t%kind("v") == PK_FLOAT64_VEC, "v column should resolve to PK_FLOAT64_VEC")
        if (allocated(error)) return
        call check(error, t%width("v") == NVEC, "a vector column's width should be its element count")
        if (allocated(error)) return
        call t%get("v", gv)
        call check(error, size(gv, 1) == NVEC .and. size(gv, 2) == NROW, &
            "a vector column must come back shaped (element, row)")
        if (allocated(error)) return
        call check(error, abs(gv(2, 3) - 32.0_real64) < 1.0e-12_real64, &
            "vector element (2,3) should be row 3's second element")
        if (allocated(error)) return
        call t%col("v", pv)
        call check(error, abs(pv(3, 6) - 63.0_real64) < 1.0e-12_real64, &
            "the pointer form should see the same (element, row) layout")
    end subroutine test_roundtrip_vector
    !
    subroutine test_widening(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: g64(:)
        real(real64), allocatable :: r64(:)
        character(len=*), parameter :: f = "test_run/table_widen.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        ! The stored kinds are int32 and float32; the caller asks for int64/float64 arrays.
        call check(error, t%kind("i32") == PK_INT32, "precondition: i32 is stored as int32")
        if (allocated(error)) return
        call t%get("i32", g64)
        call check(error, all(g64 == [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]), &
            "get should widen an int32 column into an int64 array")
        if (allocated(error)) return
        call check(error, t%kind("f32") == PK_FLOAT32, "precondition: f32 is stored as float32")
        if (allocated(error)) return
        call t%get("f32", r64)
        call check(error, abs(r64(4) - 2.0_real64) < 1.0e-6_real64, &
            "get should widen a float32 column into a float64 array")
    end subroutine test_widening
    !
    subroutine test_pointer_is_live(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), pointer :: p(:)
        real(real64), allocatable :: g(:)
        character(len=*), parameter :: f = "test_run/table_ptr.parquet"
        !
        call write_basic_fixture(f)
        ! Note the table is NOT declared `target` here: %col points into the cache's heap, not
        ! into this dummy/local, so the pointer stays valid. That is a deliberate property of
        ! the design, and this test is what would catch it being lost.
        call parquet_open_table(t, f)
        call t%col("f64", p)
        call check(error, associated(p), "col should return an associated pointer")
        if (allocated(error)) return
        call check(error, size(p) == NROW, "the pointer should span the whole column")
        if (allocated(error)) return
        p(2) = -7.5_real64
        call t%get("f64", g)
        call check(error, abs(g(2) + 7.5_real64) < 1.0e-12_real64, &
            "a write through the pointer must be visible to a later copy-out")
        if (allocated(error)) return
        ! And the pointer still refers to the same storage after an unrelated read.
        call check(error, abs(p(2) + 7.5_real64) < 1.0e-12_real64, &
            "the pointer should remain valid across an unrelated read")
    end subroutine test_pointer_is_live
    !
    subroutine test_nulls_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        real(real64) :: f64(NROW)
        character(len=8) :: s(NROW)
        type(parquet_date) :: d(NROW)
        logical :: valid_f(NROW), valid_s(NROW)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_nulls.parquet"
        !
        do i = 1, NROW
            f64(i) = real(i, real64)
            d(i) = parquet_date(2026, 1, i)
        end do
        s = ["a       ", "bcd     ", "ef      ", "ghij    ", "kl      ", "m       "]
        valid_f = .true.
        valid_s = .true.
        valid_f(2) = .false.   ! bitmap class
        valid_s(5) = .false.   ! embedded string column class
        d(4) = parquet_date()  ! inside-the-element class
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "f", f64, is_valid=valid_f)
        call parquet_write_column(w, "s", s, is_valid=valid_s)
        call parquet_write_column(w, "d", d)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        ! Class 1: column bitmap.
        call check(error, t%is_null("f", 2_int64), "a null numeric row should read back null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("f", 1_int64), "a valid numeric row should not be null")
        if (allocated(error)) return
        ! Class 2: embedded parquet_string_column.
        call check(error, t%is_null("s", 5_int64), "a null string row should read back null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("s", 1_int64), "a valid string row should not be null")
        if (allocated(error)) return
        ! Class 3: inside the element.
        call check(error, t%is_null("d", 4_int64), "a null date row should read back null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("d", 1_int64), "a valid date row should not be null")
    end subroutine test_nulls_roundtrip
    !
    subroutine test_unsupported_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        integer :: i
        logical :: saw_uint32, ok
        character(len=*), parameter :: f = "test/fixtures/extended_types.parquet"
        !
        ! extended_types.parquet carries a uint32 column, whose physical type falls outside the
        ! nine types this library reads. Opening the file must still work: the column gets a
        ! slot, appears in the listing, and is simply marked unreadable.
        call parquet_open_table(t, f)
        call check(error, t%nrows() > 0, "a file with a foreign column should still open")
        if (allocated(error)) return
        call t%column_names(names)
        saw_uint32 = .false.
        do i = 1, size(names)
            if (trim(names(i)) == "v_uint32") saw_uint32 = .true.
        end do
        call check(error, saw_uint32, "an unsupported column should still be listed")
        if (allocated(error)) return
        call check(error, .not. t%is_supported("v_uint32"), &
            "a uint32 column should report as unsupported")
        if (allocated(error)) return
        call check(error, t%kind("v_uint32") == PK_NONE, &
            "an unsupported column should have no PK_* kind")
        if (allocated(error)) return
        call check(error, t%residency("v_uint32") == RES_EMPTY, &
            "an unsupported column should hold no values")
        if (allocated(error)) return
        ! A supported column in the same file still works normally.
        call check(error, t%is_supported("id"), &
            "a supported column in the same file should still be readable")
        if (allocated(error)) return
        call check(error, t%residency("id") == RES_FULL, &
            "a supported column should still be materialized despite an unsupported sibling")
        if (allocated(error)) return
        ! And a soft-failing read of the unsupported column reports rather than aborts.
        call t%get("v_uint32", names, found=ok)
        call check(error, .not. ok, "a soft-failing read of an unsupported column should report .false.")
    end subroutine test_unsupported_column
    !
    subroutine test_struct_leaves(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: f = "test/fixtures/nested_struct.parquet"
        !
        call parquet_open_table(t, f)
        call check(error, t%has_column("main.id"), "a struct leaf should be addressable by its dotted path")
        if (allocated(error)) return
        call check(error, t%has_column("main.inner.deep.value"), &
            "a doubly-nested struct leaf should be addressable too")
        if (allocated(error)) return
        call check(error, .not. t%has_column("main"), &
            "a bare struct name should not be a column -- it is not readable")
        if (allocated(error)) return
        call check(error, t%kind("main.id") == PK_INT32, "main.id should resolve to PK_INT32")
        if (allocated(error)) return
        call t%get("main.id", ids)
        call check(error, size(ids) == int(t%nrows()), "a struct leaf should read every row")
    end subroutine test_struct_leaves
    !
    subroutine test_from_scratch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        integer(int32) :: ids(NROW)
        real(real64) :: mass(NROW)
        character(len=8) :: nm(NROW)
        real(real64), allocatable :: g(:)
        character(len=:), allocatable :: chr(:)
        integer :: i
        character(len=*), parameter :: fo = "test_run/table_scratch.parquet"
        !
        do i = 1, NROW
            ids(i) = i * 11
            mass(i) = real(i, real64) * 1.5_real64
        end do
        nm = ["a       ", "bcd     ", "ef      ", "ghij    ", "kl      ", "m       "]
        !
        call parquet_new_table(t)
        call check(error, t%nrows() == 0, "a new table starts with no rows")
        if (allocated(error)) return
        call check(error, t%ncols() == 0, "a new table starts with no columns")
        if (allocated(error)) return
        call t%add_column("id", ids)
        call check(error, t%nrows() == NROW, "the first add_column should fix the row count")
        if (allocated(error)) return
        call t%add_column("mass", mass, unit="Msun")
        call t%add_column("name", nm)
        call check(error, t%ncols() == 3, "add_column should append a column each time")
        if (allocated(error)) return
        call check(error, t%kind("mass") == PK_FLOAT64, "an added float64 column should be PK_FLOAT64")
        if (allocated(error)) return
        !
        call s%init("scratch")
        call s%add_field("id", "int32")
        call s%add_field("mass", "float64", unit="Msun")
        call s%add_field("name", "string", array_size=16)
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == NROW, "the written table should keep its row count")
        if (allocated(error)) return
        call t2%get("mass", g)
        call check(error, abs(g(3) - 4.5_real64) < 1.0e-12_real64, &
            "an in-memory column should survive the write/reopen round trip")
        if (allocated(error)) return
        call t2%get("name", chr)
        call check(error, trim(chr(4)) == "ghij", "string values should survive the round trip")
    end subroutine test_from_scratch
    !
    subroutine test_add_column_force(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: a(NROW), b(NROW)
        real(real64), allocatable :: g(:)
        integer :: i
        !
        do i = 1, NROW
            a(i) = real(i, real64)
            b(i) = real(i, real64) * 100.0_real64
        end do
        call parquet_new_table(t)
        call t%add_column("x", a)
        call t%add_column("x", b, force=.true.)
        call check(error, t%ncols() == 1, "a forced replace should not add a second column")
        if (allocated(error)) return
        call t%get("x", g)
        call check(error, abs(g(2) - 200.0_real64) < 1.0e-12_real64, &
            "a forced replace should leave the new values in place")
    end subroutine test_add_column_force
    !
    subroutine test_set_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64) :: repl(NROW)
        integer :: i
        character(len=*), parameter :: f = "test_run/table_set.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        do i = 1, NROW
            repl(i) = real(i, real64) * -3.0_real64
        end do
        call t%set("f64", repl)
        call t%get("f64", g)
        call check(error, all(abs(g - repl) < 1.0e-12_real64), &
            "set should replace every value of the column")
        if (allocated(error)) return
        call check(error, t%nrows() == NROW, "set must not change the table's row count")
    end subroutine test_set_values
    !
    subroutine test_write_row_mask(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        logical :: mask(NROW)
        integer(int32), allocatable :: g(:)
        character(len=*), parameter :: f = "test_run/table_mask_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_mask_out.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        mask = .false.
        mask(2) = .true.
        mask(5) = .true.
        call s%init("masked")
        call s%add_field("i32", "int32")
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s, row_mask=mask)
        !
        call check(error, t%nrows() == NROW, "a row_mask write must not change the source table")
        if (allocated(error)) return
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == 2, "only the masked-in rows should be written")
        if (allocated(error)) return
        call t2%get("i32", g)
        call check(error, all(g == [2, 5]), "the written rows should be the masked-in ones")
    end subroutine test_write_row_mask
    !
    subroutine test_reopen_same_variable(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        character(len=*), parameter :: f1 = "test_run/table_reopen1.parquet"
        character(len=*), parameter :: f2 = "test_run/table_reopen2.parquet"
        type(parquet_writer) :: w
        real(real64) :: v(3)
        !
        call write_basic_fixture(f1)
        v = [10.0_real64, 20.0_real64, 30.0_real64]
        call parquet_open_writer(w, f2)
        call parquet_write_column(w, "only", v)
        call parquet_close_writer(w)
        !
        ! intent(out) on a finalizable type finalizes the previous contents on entry, which is
        ! what stops a reopen from leaking the whole previous table. If that ever regressed the
        ! symptom would be a leak rather than a wrong answer, so this checks the visible half:
        ! the second open must fully replace the first.
        call parquet_open_table(t, f1)
        call check(error, t%ncols() == 6, "precondition: the first file has 6 columns")
        if (allocated(error)) return
        call parquet_open_table(t, f2)
        call check(error, t%ncols() == 1, "a reopen should replace the previous table entirely")
        if (allocated(error)) return
        call check(error, t%nrows() == 3, "a reopen should adopt the new file's row count")
        if (allocated(error)) return
        call check(error, .not. t%has_column("i32"), &
            "no column of the previous file should survive a reopen")
        if (allocated(error)) return
        call t%get("only", g)
        call check(error, abs(g(3) - 30.0_real64) < 1.0e-12_real64, &
            "the reopened table should hold the new file's values")
    end subroutine test_reopen_same_variable
    !
    subroutine test_soft_fail(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64), pointer :: p(:)
        logical :: ok
        integer :: k
        character(len=*), parameter :: f = "test_run/table_soft.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        ! Every lookup takes an optional found=: present, a miss is reported rather than fatal.
        call t%get("no_such_column", g, found=ok)
        call check(error, .not. ok, "get should report a missing column through found=")
        if (allocated(error)) return
        call t%col("no_such_column", p, found=ok)
        call check(error, .not. ok, "col should report a missing column through found=")
        if (allocated(error)) return
        call check(error, .not. associated(p), "col should leave the pointer unassociated on a miss")
        if (allocated(error)) return
        k = t%kind("no_such_column", found=ok)
        call check(error, .not. ok, "kind should report a missing column through found=")
        if (allocated(error)) return
        ! And a hit still reports .true.
        call t%get("f64", g, found=ok)
        call check(error, ok, "found= should be .true. for a column that exists")
    end subroutine test_soft_fail
    !
    subroutine test_unit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64) :: v(NROW)
        character(len=:), allocatable :: u
        integer :: i
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("speed", v, unit="km/s")
        call t%unit("speed", u)
        call check(error, u == "km/s", "add_column(unit=) should be reported back by %unit")
        if (allocated(error)) return
        call t%add_column("plain", v)
        call t%unit("plain", u)
        call check(error, u == "", "a column with no unit should report an empty unit")
    end subroutine test_unit
    !
    !> Every one of the 18 column kinds, end to end: write a fixture holding all of them, open it
    !! as a table, check each column's kind and width, copy it out with %get, alias it with %col
    !! where a pointer path exists, then write every column back through a schema and reopen.
    !!
    !! Each kind has its own materializer, its own %get/%col/%set specifics and its own write
    !! branch, so a kind that is merely "similar to one that is tested" is not tested at all --
    !! which is why this sweeps rather than spot-checks.
    !> %filename reports where a table came from, and %get_file_metadata reads the source file's
    !! own key/value metadata -- including the soft-fail path for an in-memory table, which has
    !! no file to ask.
    subroutine test_filename_and_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t, mem
        type(parquet_schema) :: s
        real(real64) :: v(NROW)
        character(len=:), allocatable :: fname, val
        logical :: ok
        integer :: i
        character(len=*), parameter :: f = "test_run/table_meta.parquet"
        !
        do i = 1, NROW
            v(i) = real(i, real64)
        end do
        call s%init("metatable", survey="TESTSURVEY")
        call s%add_field("v", "float64")
        call parquet_parse_maml(s)
        call s%add_metadata("mykey", "myvalue")
        call parquet_open_writer(w, f, s)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%filename(fname)
        call check(error, fname == f, "%filename should report the file the table was opened from")
        if (allocated(error)) return
        call t%get_file_metadata("mykey", val, found=ok)
        call check(error, ok, "a metadata key written into the file should be found")
        if (allocated(error)) return
        call check(error, val == "myvalue", "the metadata value should round-trip")
        if (allocated(error)) return
        call t%get_file_metadata("no_such_key", val, found=ok)
        call check(error, .not. ok, "a missing metadata key should report through found=")
        if (allocated(error)) return
        !
        ! An in-memory table has no file to ask.
        call parquet_new_table(mem)
        call mem%filename(fname)
        call check(error, fname == "", "an in-memory table should report an empty filename")
        if (allocated(error)) return
        call mem%get_file_metadata("anything", val, found=ok)
        call check(error, .not. ok, &
            "metadata on an in-memory table should report .false. rather than aborting")
    end subroutine test_filename_and_metadata
    !
    !> Writes the all-18-kinds fixture the three matrix tests share.
    subroutine write_matrix_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        integer :: i, e
        ! scalar fixtures
        integer(int32) :: a_i32(NROW)
        integer(int64) :: a_i64(NROW)
        real(real32) :: a_f32(NROW)
        real(real64) :: a_f64(NROW)
        logical :: a_bool(NROW)
        character(len=8) :: a_str(NROW)
        type(parquet_date) :: a_date(NROW)
        type(parquet_time) :: a_time(NROW)
        type(parquet_timestamp) :: a_ts(NROW)
        ! vector fixtures, (element, row)
        integer(int32) :: v_i32(NVEC, NROW)
        integer(int64) :: v_i64(NVEC, NROW)
        real(real32) :: v_f32(NVEC, NROW)
        real(real64) :: v_f64(NVEC, NROW)
        logical :: v_bool(NVEC, NROW)
        character(len=8) :: v_str(NVEC, NROW)
        type(parquet_date) :: v_date(NVEC, NROW)
        type(parquet_time) :: v_time(NVEC, NROW)
        type(parquet_timestamp) :: v_ts(NVEC, NROW)
        !
        do i = 1, NROW
            a_i32(i) = i
            a_i64(i) = int(i, int64) * 1000000000_int64
            a_f32(i) = real(i, real32) * 0.25_real32
            a_f64(i) = real(i, real64) * 1.75_real64
            a_bool(i) = mod(i, 2) == 1
            a_date(i) = parquet_date(2026, 3, i)
            a_time(i) = parquet_time(10, 20, i)
            a_ts(i) = parquet_timestamp(2026, 3, i, 1, 2, 3)
            do e = 1, NVEC
                v_i32(e, i) = i * 10 + e
                v_i64(e, i) = int(i * 10 + e, int64) * 1000000000_int64
                v_f32(e, i) = real(i * 10 + e, real32) * 0.5_real32
                v_f64(e, i) = real(i * 10 + e, real64) * 1.5_real64
                v_bool(e, i) = mod(i + e, 2) == 0
                v_date(e, i) = parquet_date(2026, 4, e)
                v_time(e, i) = parquet_time(5, 6, e)
                v_ts(e, i) = parquet_timestamp(2026, 4, e, 7, 8, 9)
            end do
        end do
        ! First element deliberately the shortest, in both the scalar and vector fixtures.
        a_str = ["a       ", "bcd     ", "ef      ", "ghijklm ", "no      ", "p       "]
        v_str(1, :) = "a"
        v_str(2, :) = "bcde"
        v_str(3, :) = "fg"
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
    end subroutine write_matrix_fixture
    !
    !> Builds the parsed schema that writes all 18 kinds back out.
    subroutine build_matrix_schema(sc)
        type(parquet_schema), intent(out) :: sc !! schema to build.
        !
        call sc%init("matrix")
        call sc%add_field("s_i32", "int32")
        call sc%add_field("s_i64", "int64")
        call sc%add_field("s_f32", "float32")
        call sc%add_field("s_f64", "float64")
        call sc%add_field("s_bool", "boolean")
        call sc%add_field("s_str", "string", array_size=16)
        call sc%add_field("s_date", "date")
        call sc%add_field("s_time", "time")
        call sc%add_field("s_ts", "timestamp")
        call sc%add_field("v_i32", "int32", col_size=NVEC)
        call sc%add_field("v_i64", "int64", col_size=NVEC)
        call sc%add_field("v_f32", "float32", col_size=NVEC)
        call sc%add_field("v_f64", "float64", col_size=NVEC)
        call sc%add_field("v_bool", "boolean", col_size=NVEC)
        call sc%add_field("v_str", "string", array_size=16, col_size=NVEC)
        call sc%add_field("v_date", "date", col_size=NVEC)
        call sc%add_field("v_time", "time", col_size=NVEC)
        call sc%add_field("v_ts", "timestamp", col_size=NVEC)
        call parquet_parse_maml(sc)
    end subroutine build_matrix_schema
    !
    subroutine test_kind_matrix(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, t2
        type(parquet_schema) :: sc
        character(len=*), parameter :: f = "test_run/table_matrix.parquet"
        character(len=*), parameter :: fo = "test_run/table_matrix_out.parquet"
        ! read-back targets
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_str(:), gv_str(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        integer(int32), pointer :: p_i32(:)
        real(real64), pointer :: p_f64v(:,:)
        ! A timestamp's civil fields are reached through its date part rather than directly.
        type(parquet_date) :: dpart
        !
        call write_matrix_fixture(f)
        !
        call parquet_open_table(t, f)
        call check(error, t%ncols() == 18, "the matrix fixture should have all 18 kinds")
        if (allocated(error)) return
        !
        ! --- kinds and widths ---
        call check(error, t%kind("s_i32") == PK_INT32, "s_i32 should be PK_INT32")
        if (allocated(error)) return
        call check(error, t%kind("s_i64") == PK_INT64, "s_i64 should be PK_INT64")
        if (allocated(error)) return
        call check(error, t%kind("s_f32") == PK_FLOAT32, "s_f32 should be PK_FLOAT32")
        if (allocated(error)) return
        call check(error, t%kind("s_f64") == PK_FLOAT64, "s_f64 should be PK_FLOAT64")
        if (allocated(error)) return
        call check(error, t%kind("s_bool") == PK_LOGICAL, "s_bool should be PK_LOGICAL")
        if (allocated(error)) return
        call check(error, t%kind("s_str") == PK_STRING, "s_str should be PK_STRING")
        if (allocated(error)) return
        call check(error, t%kind("s_date") == PK_DATE, "s_date should be PK_DATE")
        if (allocated(error)) return
        call check(error, t%kind("s_time") == PK_TIME, "s_time should be PK_TIME")
        if (allocated(error)) return
        call check(error, t%kind("s_ts") == PK_TIMESTAMP, "s_ts should be PK_TIMESTAMP")
        if (allocated(error)) return
        call check(error, t%kind("v_i32") == PK_INT32_VEC, "v_i32 should be PK_INT32_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_i64") == PK_INT64_VEC, "v_i64 should be PK_INT64_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_f32") == PK_FLOAT32_VEC, "v_f32 should be PK_FLOAT32_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_f64") == PK_FLOAT64_VEC, "v_f64 should be PK_FLOAT64_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_bool") == PK_LOGICAL_VEC, "v_bool should be PK_LOGICAL_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_str") == PK_STRING_VEC, "v_str should be PK_STRING_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_date") == PK_DATE_VEC, "v_date should be PK_DATE_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_time") == PK_TIME_VEC, "v_time should be PK_TIME_VEC")
        if (allocated(error)) return
        call check(error, t%kind("v_ts") == PK_TIMESTAMP_VEC, "v_ts should be PK_TIMESTAMP_VEC")
        if (allocated(error)) return
        call check(error, t%width("s_f64") == 1 .and. t%width("v_f64") == NVEC, &
            "a scalar column's width should be 1 and a vector column's its element count")
        if (allocated(error)) return
        !
        ! --- copy out every kind ---
        call t%get("s_i32", g_i32)
        call check(error, g_i32(3) == 3 .and. size(g_i32) == NROW, "s_i32 values should survive")
        if (allocated(error)) return
        call t%get("s_i64", g_i64)
        call check(error, g_i64(3) == 3000000000_int64, "s_i64 values should survive")
        if (allocated(error)) return
        call t%get("s_f32", g_f32)
        call check(error, abs(g_f32(4) - 1.0_real32) < 1.0e-6_real32, "s_f32 values should survive")
        if (allocated(error)) return
        call t%get("s_f64", g_f64)
        call check(error, abs(g_f64(4) - 7.0_real64) < 1.0e-12_real64, "s_f64 values should survive")
        if (allocated(error)) return
        call t%get("s_bool", g_bool)
        call check(error, g_bool(1) .and. .not. g_bool(2), "s_bool values should survive")
        if (allocated(error)) return
        call t%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", "s_str values should survive untruncated")
        if (allocated(error)) return
        call t%get("s_date", g_date)
        call check(error, g_date(2)%day() == 2, "s_date values should survive")
        if (allocated(error)) return
        call t%get("s_time", g_time)
        call check(error, g_time(2)%second() == 2, "s_time values should survive")
        if (allocated(error)) return
        call t%get("s_ts", g_ts)
        dpart = g_ts(2)%get_date()
        call check(error, dpart%day() == 2, "s_ts values should survive")
        if (allocated(error)) return
        call t%get("v_i32", gv_i32)
        call check(error, gv_i32(2, 3) == 32, "v_i32 values should survive")
        if (allocated(error)) return
        call t%get("v_i64", gv_i64)
        call check(error, gv_i64(2, 3) == 32000000000_int64, "v_i64 values should survive")
        if (allocated(error)) return
        call t%get("v_f32", gv_f32)
        call check(error, abs(gv_f32(2, 3) - 16.0_real32) < 1.0e-5_real32, "v_f32 values should survive")
        if (allocated(error)) return
        call t%get("v_f64", gv_f64)
        call check(error, abs(gv_f64(2, 3) - 48.0_real64) < 1.0e-12_real64, "v_f64 values should survive")
        if (allocated(error)) return
        call t%get("v_bool", gv_bool)
        call check(error, gv_bool(1, 1) .and. .not. gv_bool(2, 1), "v_bool values should survive")
        if (allocated(error)) return
        call t%get("v_str", gv_str)
        call check(error, trim(gv_str(2, 1)) == "bcde", "v_str values should survive untruncated")
        if (allocated(error)) return
        call t%get("v_date", gv_date)
        call check(error, gv_date(2, 1)%day() == 2, "v_date values should survive")
        if (allocated(error)) return
        call t%get("v_time", gv_time)
        call check(error, gv_time(2, 1)%second() == 2, "v_time values should survive")
        if (allocated(error)) return
        call t%get("v_ts", gv_ts)
        dpart = gv_ts(2, 1)%get_date()
        call check(error, dpart%day() == 2, "v_ts values should survive")
        if (allocated(error)) return
        !
        ! --- pointer path, one scalar and one vector kind ---
        call t%col("s_i32", p_i32)
        call check(error, p_i32(3) == 3, "the pointer path should see the same scalar values")
        if (allocated(error)) return
        call t%col("v_f64", p_f64v)
        call check(error, abs(p_f64v(2, 3) - 48.0_real64) < 1.0e-12_real64, &
            "the pointer path should see the same vector values")
        if (allocated(error)) return
        !
        ! --- copy back, then write every kind out and reopen ---
        call t%set("s_f64", g_f64)
        call t%set("v_f64", gv_f64)
        call build_matrix_schema(sc)
        call parquet_write_table(t, fo, sc)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%ncols() == 18, "every kind should survive the write")
        if (allocated(error)) return
        call check(error, t2%nrows() == NROW, "the written table should keep its row count")
        if (allocated(error)) return
        call t2%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", "s_str should survive the write round trip")
        if (allocated(error)) return
        call t2%get("v_f64", gv_f64)
        call check(error, abs(gv_f64(2, 3) - 48.0_real64) < 1.0e-12_real64, &
            "v_f64 should survive the write round trip")
        if (allocated(error)) return
        call t2%get("v_ts", gv_ts)
        dpart = gv_ts(2, 1)%get_date()
        call check(error, dpart%day() == 2, "v_ts should survive the write round trip")
        if (allocated(error)) return
        call t2%get("s_date", g_date)
        call check(error, g_date(2)%day() == 2, "s_date should survive the write round trip")
    end subroutine test_kind_matrix
    !
    !> %col for every kind that has a pointer path, and %set for every kind, swept rather than
    !! spot-checked: each has its own generated specific, so an untested kind is untested code.
    subroutine test_kind_matrix_col_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        ! Its own fixture path: test-drive runs tests concurrently, so sharing one file with
        ! another test lets one truncate it while the other is reading.
        character(len=*), parameter :: f = "test_run/table_matrix_cs.parquet"
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        logical, pointer :: p_bool(:), p_boolv(:,:)
        type(parquet_date), pointer :: p_date(:), p_datev(:,:)
        type(parquet_time), pointer :: p_time(:), p_timev(:,:)
        type(parquet_timestamp), pointer :: p_ts(:), p_tsv(:,:)
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_str(:), gv_str(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        !
        ! test_kind_matrix writes this fixture; regenerate it here so the two tests are
        ! independent of each other's execution order (test-drive does not promise one).
        call write_matrix_fixture(f)
        call parquet_open_table(t, f)
        !
        ! --- every pointer path (all kinds except the two string ones) ---
        call t%col("s_i32", p_i32)
        call check(error, associated(p_i32) .and. size(p_i32) == NROW, "col s_i32")
        if (allocated(error)) return
        call t%col("s_i64", p_i64)
        call check(error, associated(p_i64) .and. size(p_i64) == NROW, "col s_i64")
        if (allocated(error)) return
        call t%col("s_f32", p_f32)
        call check(error, associated(p_f32) .and. size(p_f32) == NROW, "col s_f32")
        if (allocated(error)) return
        call t%col("s_f64", p_f64)
        call check(error, associated(p_f64) .and. size(p_f64) == NROW, "col s_f64")
        if (allocated(error)) return
        call t%col("s_bool", p_bool)
        call check(error, associated(p_bool) .and. size(p_bool) == NROW, "col s_bool")
        if (allocated(error)) return
        call t%col("s_date", p_date)
        call check(error, associated(p_date) .and. size(p_date) == NROW, "col s_date")
        if (allocated(error)) return
        call t%col("s_time", p_time)
        call check(error, associated(p_time) .and. size(p_time) == NROW, "col s_time")
        if (allocated(error)) return
        call t%col("s_ts", p_ts)
        call check(error, associated(p_ts) .and. size(p_ts) == NROW, "col s_ts")
        if (allocated(error)) return
        call t%col("v_i32", p_i32v)
        call check(error, associated(p_i32v) .and. size(p_i32v, 1) == NVEC, "col v_i32")
        if (allocated(error)) return
        call t%col("v_i64", p_i64v)
        call check(error, associated(p_i64v) .and. size(p_i64v, 1) == NVEC, "col v_i64")
        if (allocated(error)) return
        call t%col("v_f32", p_f32v)
        call check(error, associated(p_f32v) .and. size(p_f32v, 1) == NVEC, "col v_f32")
        if (allocated(error)) return
        call t%col("v_f64", p_f64v)
        call check(error, associated(p_f64v) .and. size(p_f64v, 1) == NVEC, "col v_f64")
        if (allocated(error)) return
        call t%col("v_bool", p_boolv)
        call check(error, associated(p_boolv) .and. size(p_boolv, 1) == NVEC, "col v_bool")
        if (allocated(error)) return
        call t%col("v_date", p_datev)
        call check(error, associated(p_datev) .and. size(p_datev, 1) == NVEC, "col v_date")
        if (allocated(error)) return
        call t%col("v_time", p_timev)
        call check(error, associated(p_timev) .and. size(p_timev, 1) == NVEC, "col v_time")
        if (allocated(error)) return
        call t%col("v_ts", p_tsv)
        call check(error, associated(p_tsv) .and. size(p_tsv, 1) == NVEC, "col v_ts")
        if (allocated(error)) return
        !
        ! --- %set every kind: copy out, then write the same values straight back ---
        call t%get("s_i32", g_i32);   call t%set("s_i32", g_i32)
        call t%get("s_i64", g_i64);   call t%set("s_i64", g_i64)
        call t%get("s_f32", g_f32);   call t%set("s_f32", g_f32)
        call t%get("s_f64", g_f64);   call t%set("s_f64", g_f64)
        call t%get("s_bool", g_bool); call t%set("s_bool", g_bool)
        call t%get("s_str", g_str);   call t%set("s_str", g_str)
        call t%get("s_date", g_date); call t%set("s_date", g_date)
        call t%get("s_time", g_time); call t%set("s_time", g_time)
        call t%get("s_ts", g_ts);     call t%set("s_ts", g_ts)
        call t%get("v_i32", gv_i32);   call t%set("v_i32", gv_i32)
        call t%get("v_i64", gv_i64);   call t%set("v_i64", gv_i64)
        call t%get("v_f32", gv_f32);   call t%set("v_f32", gv_f32)
        call t%get("v_f64", gv_f64);   call t%set("v_f64", gv_f64)
        call t%get("v_bool", gv_bool); call t%set("v_bool", gv_bool)
        call t%get("v_str", gv_str);   call t%set("v_str", gv_str)
        call t%get("v_date", gv_date); call t%set("v_date", gv_date)
        call t%get("v_time", gv_time); call t%set("v_time", gv_time)
        call t%get("v_ts", gv_ts);     call t%set("v_ts", gv_ts)
        !
        ! A set-then-get round trip must be the identity.
        call t%get("s_f64", g_f64)
        call check(error, size(g_f64) == NROW, "a set/get round trip should preserve the row count")
        if (allocated(error)) return
        call t%get("v_str", gv_str)
        call check(error, trim(gv_str(2, 1)) == "bcde", &
            "a set/get round trip should preserve vector string values")
        if (allocated(error)) return
        call t%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", &
            "a set/get round trip should preserve scalar string values")
    end subroutine test_kind_matrix_col_set
    !
    !> add_column for every kind: build a whole table from scratch in memory, then write it and
    !! read it back. Each kind has its own add_column specific, so this sweeps too.
    subroutine test_kind_matrix_add(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: src, t, t2
        type(parquet_schema) :: sc
        character(len=*), parameter :: f = "test_run/table_matrix_add_in.parquet"
        character(len=*), parameter :: fo = "test_run/table_matrix_add.parquet"
        integer(int32), allocatable :: g_i32(:), gv_i32(:,:)
        integer(int64), allocatable :: g_i64(:), gv_i64(:,:)
        real(real32), allocatable :: g_f32(:), gv_f32(:,:)
        real(real64), allocatable :: g_f64(:), gv_f64(:,:)
        logical, allocatable :: g_bool(:), gv_bool(:,:)
        character(len=:), allocatable :: g_str(:), gv_str(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        !
        ! Take the values from a file-backed table so the fixture is written in one place only.
        call write_matrix_fixture(f)
        call parquet_open_table(src, f)
        call src%get("s_i32", g_i32);   call src%get("s_i64", g_i64)
        call src%get("s_f32", g_f32);   call src%get("s_f64", g_f64)
        call src%get("s_bool", g_bool); call src%get("s_str", g_str)
        call src%get("s_date", g_date); call src%get("s_time", g_time)
        call src%get("s_ts", g_ts)
        call src%get("v_i32", gv_i32);   call src%get("v_i64", gv_i64)
        call src%get("v_f32", gv_f32);   call src%get("v_f64", gv_f64)
        call src%get("v_bool", gv_bool); call src%get("v_str", gv_str)
        call src%get("v_date", gv_date); call src%get("v_time", gv_time)
        call src%get("v_ts", gv_ts)
        !
        call parquet_new_table(t)
        call t%add_column("s_i32", g_i32)
        call t%add_column("s_i64", g_i64)
        call t%add_column("s_f32", g_f32)
        call t%add_column("s_f64", g_f64, unit="kg")
        call t%add_column("s_bool", g_bool)
        call t%add_column("s_str", g_str)
        call t%add_column("s_date", g_date)
        call t%add_column("s_time", g_time)
        call t%add_column("s_ts", g_ts)
        call t%add_column("v_i32", gv_i32)
        call t%add_column("v_i64", gv_i64)
        call t%add_column("v_f32", gv_f32)
        call t%add_column("v_f64", gv_f64)
        call t%add_column("v_bool", gv_bool)
        call t%add_column("v_str", gv_str)
        call t%add_column("v_date", gv_date)
        call t%add_column("v_time", gv_time)
        call t%add_column("v_ts", gv_ts)
        call check(error, t%ncols() == 18, "add_column should build all 18 kinds")
        if (allocated(error)) return
        call check(error, t%nrows() == NROW, "the from-scratch table should adopt the row count")
        if (allocated(error)) return
        call check(error, t%width("v_f64") == NVEC, "a vector column's width should come from its values")
        if (allocated(error)) return
        !
        call build_matrix_schema(sc)
        call parquet_write_table(t, fo, sc)
        call parquet_open_table(t2, fo)
        call check(error, t2%ncols() == 18, "every added kind should survive the write")
        if (allocated(error)) return
        call t2%get("s_str", g_str)
        call check(error, trim(g_str(4)) == "ghijklm", "added string values should survive")
        if (allocated(error)) return
        call t2%get("v_i64", gv_i64)
        call check(error, gv_i64(2, 1) == 12000000000_int64, "added vector int64 values should survive")
    end subroutine test_kind_matrix_add
    !
end module test_table
