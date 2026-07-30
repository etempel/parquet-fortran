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
            new_unittest("a plain LIST column's width is deferred, then resolved and proven", &
                test_deferred_list_width), &
            new_unittest("a slice measures a plain LIST column over its own row groups only", &
                test_deferred_list_width_slice), &
            new_unittest("the no-nulls materialize fast path never loses a null", &
                test_materialize_null_fast_path), &
            new_unittest("a file without column statistics still reads its nulls", &
                test_materialize_without_statistics), &
            new_unittest("nulls survive being written back out by parquet_write_table", &
                test_write_table_nulls), &
            new_unittest("a null in a vector string column is widened to the whole row", &
                test_string_vector_nulls), &
            new_unittest("opening reads nothing; each accessor touches only its own column", &
                test_lazy_first_touch), &
            new_unittest("prefetch and materialize_all read columns ahead of first touch", &
                test_prefetch), &
            new_unittest("reload restores a column's file values after set", test_reload), &
            new_unittest("row group bounds partition the file's rows exactly", &
                test_row_group_bounds), &
            new_unittest("a slice straddling a row-group boundary reads all 18 kinds correctly", &
                test_slice_kind_matrix), &
            new_unittest("slice bounds inside, on and across row groups all agree with the full table", &
                test_slice_shapes), &
            new_unittest("a row handle reads every kind, widens, and triggers its own first touch", &
                test_row_view), &
            new_unittest("get_slice copies range, strided, descending and list selections", &
                test_get_slice), &
            new_unittest("a thread's own slice table may be read lazily inside a parallel region", &
                test_parallel_private_slices), &
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
            new_unittest("all 18 kinds: found= reports a missing column instead of aborting", &
                test_kind_matrix_found), &
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
        ! Everything above was answered without reading a single column: opening classifies
        ! from the schema and stops there.
        call check(error, t%residency("f64") == RES_EMPTY, &
            "no column should be resident before it is touched")
        if (allocated(error)) return
        call check(error, t%kind("f64") == PK_FLOAT64, &
            "kind must answer from the schema, before the column is read")
        if (allocated(error)) return
        call check(error, t%width("f64") == 1, &
            "width must answer from the schema, before the column is read")
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
    !> A plain Parquet `LIST` column carries no width in its schema, so `parquet_table` defers its
    !! kind and width to first use rather than decoding it at open. This checks both halves: that
    !! opening leaves such a column unclassified and unread, and that `%kind`/`%width` then resolve
    !! it to a PROVEN answer.
    !!
    !! `test/fixtures/list_widths.parquet` (see tools/generate_fixtures.cpp) holds one column per
    !! outcome of the two-tier resolution. The one that matters most is `avg_ok`: its rows alternate
    !! between lengths 3 and 1, so every row group averages exactly 2 elements per row and the
    !! footer screen CANNOT reject it. Only the row-group scan can, which is precisely why
    !! `%kind`/`%width` prove rather than trust the screen -- a candidate is not a proof.
    subroutine test_deferred_list_width(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int32), allocatable :: v(:,:)
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"
        !
        call parquet_open_table(t, f)
        call check(error, t%ncols() == 9, "the fixture should present all 9 columns")
        if (allocated(error)) return
        ! Opening must not have read anything, deferred columns included.
        call check(error, t%residency("uniform") == RES_EMPTY, &
            "opening must not materialize a deferred-width column")
        if (allocated(error)) return
        !
        ! Uniform: screen yields candidate 3, the scan confirms it -> a real vector column.
        call check(error, t%kind("uniform") == PK_INT32_VEC, &
            "a uniform-width LIST column should resolve to the vector kind")
        if (allocated(error)) return
        call check(error, t%width("uniform") == 3, "its width should be 3")
        if (allocated(error)) return
        ! Resolving the width is not the same as reading the column.
        call check(error, t%residency("uniform") == RES_EMPTY, &
            "%kind/%width must resolve the width without materializing the column")
        if (allocated(error)) return
        !
        ! avg_ok: lengths 3,1,3,1 average to exactly 2, so the footer screen passes it as a
        ! candidate of 2 and only the scan rejects it. Getting 1 here is the whole point.
        call check(error, t%width("avg_ok") == 1, &
            "a LIST column whose mean row length is a whole number but whose rows differ must " // &
            "not be reported as a vector column")
        if (allocated(error)) return
        call check(error, t%kind("avg_ok") == PK_INT32, &
            "avg_ok should resolve to the scalar kind, not the vector kind")
        if (allocated(error)) return
        !
        ! ragged: non-integral mean, rejected by the screen alone.
        call check(error, t%width("ragged") == 1, "a ragged LIST column should report width 1")
        if (allocated(error)) return
        ! late: uniform except in the final row group, so only a row-group-vs-row-group comparison
        ! catches it -- divisibility alone would not.
        call check(error, t%width("late") == 1, &
            "a LIST column that changes width only in its last row group should report width 1")
        if (allocated(error)) return
        ! A null or empty row has length 0, which no width >= 1 covers.
        call check(error, t%width("with_null") == 1, &
            "a LIST column containing a null row should report width 1")
        if (allocated(error)) return
        call check(error, t%width("with_empty") == 1, &
            "a LIST column containing an empty row should report width 1")
        if (allocated(error)) return
        ! null_avg's mean IS a whole number (a null row occupies one leaf slot), so the screen
        ! passes it and only the scan can reject it -- the null counterpart of avg_ok above.
        call check(error, t%width("null_avg") == 1, &
            "a LIST column whose null row keeps the mean integral must still report width 1")
        if (allocated(error)) return
        !
        ! A plain LIST leaf underneath a STRUCT is a different matter: struct_path_exists
        ! (parquet_wrapper.cpp) deliberately refuses to address a LIST/LARGE_LIST/MAP leaf through a
        ! dotted path, so such a column is visible but not readable and deferral never applies to
        ! it. Pinned here so the boundary is explicit rather than discovered -- a FIXED_SIZE_LIST
        ! leaf under a struct IS addressable (see test/fixtures/nested_struct.parquet); only the
        ! variable-length form is not.
        call check(error, t%has_column("nested.vals"), &
            "a struct-nested LIST leaf should still be listed as a column")
        if (allocated(error)) return
        call check(error, .not. t%is_supported("nested.vals"), &
            "but it should be reported unsupported, not silently classified")
        if (allocated(error)) return
        !
        ! The control: a scalar column is classified from the schema at open and never deferred.
        call check(error, t%kind("scalar") == PK_INT32, "a scalar column should stay scalar")
        if (allocated(error)) return
        call check(error, t%width("scalar") == 1, "a scalar column's width should be 1")
        if (allocated(error)) return
        !
        ! And the resolved column still reads correctly afterwards.
        call t%get("uniform", v)
        call check(error, size(v, 1) == 3 .and. size(v, 2) == 16, &
            "the resolved vector column should read back as (3, 16)")
        if (allocated(error)) return
        call check(error, all(v(:, 2) == [100, 101, 102]), &
            "row 2 of the resolved vector column should hold its own values")
    end subroutine test_deferred_list_width
    !
    !> A slice measures a deferred column over the row groups IT covers, not the whole file.
    !!
    !! The `late` column is uniformly width 3 for its first three row groups and width 2 in the
    !! fourth, so it has no file-wide width at all -- yet each of those two ranges is internally
    !! uniform. A slice over rows 1..12 must therefore see a width-3 vector column, a slice over
    !! rows 13..16 a width-2 one, and the whole file neither. Two tables over the same file
    !! legitimately disagreeing is the documented consequence of measuring slice-locally, and this
    !! test is what pins it down.
    subroutine test_deferred_list_width_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test/fixtures/list_widths.parquet"
        !
        call parquet_open_table(t, f, 1_int64, 12_int64)
        call check(error, t%width("late") == 3, &
            "a slice covering only the uniform row groups should see width 3")
        if (allocated(error)) return
        call check(error, t%kind("late") == PK_INT32_VEC, &
            "and should resolve to the vector kind")
        if (allocated(error)) return
        !
        call parquet_open_table(t, f, 13_int64, 16_int64)
        call check(error, t%width("late") == 2, &
            "a slice covering only the final row group should see that row group's own width")
        if (allocated(error)) return
        !
        call parquet_open_table(t, f)
        call check(error, t%width("late") == 1, &
            "the whole file has no single width, so the full table should report 1")
    end subroutine test_deferred_list_width_slice
    !
    !> Materializing must carry every null across, whichever internal path it takes.
    !!
    !! `mat_*` now asks the file's footer statistics whether a column has any Nulls, and skips the
    !! whole validity pipeline when the answer is no -- no mask is allocated, requested, converted or
    !! replayed. That is a silent failure mode if it ever gets the question wrong: the column simply
    !! comes back with every row valid. So this drives both branches over the same table and checks
    !! the nulls that must survive AND the non-nulls that must not appear.
    !!
    !! Both validity dispatch classes that use the mask path are covered (`bitmap` via a numeric
    !! column, `embedded string column` via a string one), plus a vector column, whose mask is
    !! row-granular and read per element. The temporal class carries its nulls inside the element
    !! and never used a mask, so it is covered by test_nulls_roundtrip instead.
    subroutine test_materialize_null_fast_path(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NB = 40 !! spans more than one 64-bit validity block boundary at 40.
        type(error_type), allocatable :: e2
        type(parquet_writer) :: w
        type(parquet_table) :: t
        real(real64) :: dirty(NB), clean(NB), dirtyv(3, NB)
        real(real64), allocatable :: back(:)
        character(len=6) :: s6(NB)
        logical :: vd(NB), vs(NB), vv(3, NB)
        integer :: i
        integer, parameter :: nulls(4) = [1, 17, 33, 40]
        character(len=*), parameter :: f = "test_run/table_mat_nullfast.parquet"
        !
        do i = 1, NB
            dirty(i) = real(i, real64)
            clean(i) = real(100 + i, real64)
            dirtyv(:, i) = real(i, real64)
            write(s6(i), '(a,i0)') "v", i
        end do
        vd = .true.
        vs = .true.
        vv = .true.
        do i = 1, size(nulls)
            vd(nulls(i)) = .false.
            vs(nulls(i)) = .false.
            vv(:, nulls(i)) = .false.
        end do
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "dirty", dirty, is_valid=vd)
        call parquet_write_column(w, "clean", clean)
        call parquet_write_column(w, "s", s6, is_valid=vs)
        call parquet_write_column(w, "dv", dirtyv, is_valid=vv)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%materialize_all()
        ! The slow path: every null must have survived, and no extra null invented.
        do i = 1, NB
            call check(e2, t%is_null("dirty", int(i, int64)) .eqv. any(nulls == i), &
                "a numeric column's nulls must survive materialize exactly")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
            call check(e2, t%is_null("s", int(i, int64)) .eqv. any(nulls == i), &
                "a string column's nulls must survive materialize exactly")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
            call check(e2, t%is_null("dv", int(i, int64)) .eqv. any(nulls == i), &
                "a vector column's nulls must survive materialize exactly")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
            ! And the fast path: a clean column must gain no nulls at all.
            call check(e2, .not. t%is_null("clean", int(i, int64)), &
                "the no-nulls fast path must not invent a null")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
        end do
        ! Values must survive both paths -- adopt hands the array over rather than copying it, so a
        ! mistake there shows up as wrong or missing data rather than as wrong validity.
        call t%get("clean", back)
        call check(error, all(back == clean), "the fast path must keep its values")
        if (allocated(error)) return
        call t%get("dirty", back)
        call check(error, back(2) == 2.0_real64 .and. back(NB - 1) == real(NB - 1, real64), &
            "the mask path must keep the values of its non-null rows")
    end subroutine test_materialize_null_fast_path
    !
    !> The fallback when the footer cannot answer: a file written with statistics DISABLED.
    !!
    !! `parquet_column_has_nulls` reads a column's null count from Parquet statistics, which are
    !! optional in the format. When they are missing it must answer "might have Nulls" so the
    !! validity mask is still requested. Getting that backwards aborts the read outright
    !! ("column contains Null value(s), which is not supported"), so this is not a silent failure --
    !! but it is unreachable with any other fixture, since every other one carries statistics.
    !!
    !! `test/fixtures/no_stats.parquet` (see tools/generate_fixtures.cpp) has no statistics at all:
    !! column `v` holds Nulls at rows 2 and 5, column `c` holds none.
    subroutine test_materialize_without_statistics(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: r
        type(parquet_table) :: t
        real(real64), allocatable :: back(:)
        character(len=*), parameter :: f = "test/fixtures/no_stats.parquet"
        !
        ! Without statistics the answer must be the conservative one, for the clean column too.
        call parquet_open_reader(r, f)
        call check(error, parquet_column_has_nulls(r, "v", 0, 0), &
            "a column with no statistics must be reported as possibly holding nulls")
        if (allocated(error)) return
        call check(error, parquet_column_has_nulls(r, "c", 0, 0), &
            "even a genuinely clean column must be reported that way when statistics are absent")
        if (allocated(error)) return
        call parquet_close_reader(r)
        !
        call parquet_open_table(t, f)
        call t%materialize_all()
        call check(error, t%is_null("v", 2_int64) .and. t%is_null("v", 5_int64), &
            "nulls must survive materialize when the footer could not report them")
        if (allocated(error)) return
        call check(error, .not. (t%is_null("v", 1_int64) .or. t%is_null("v", 6_int64)), &
            "and non-null rows must not become null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("c", 3_int64), &
            "the clean column must still come back with no nulls")
        if (allocated(error)) return
        call t%get("c", back)
        call check(error, abs(back(3) - 300.0_real64) < 1.0e-9_real64, &
            "and with its values intact")
    end subroutine test_materialize_without_statistics
    !
    !> Nulls must survive being written back out by `parquet_write_table`, not merely read.
    !!
    !! This guards the two shortcuts the write path takes when building its `is_valid=` mask, both
    !! of which fail silently if wrong -- the file simply comes back with every row valid:
    !!
    !!  * a column with NO nulls is written with no mask at all (`row_validity` returns an
    !!    unallocated array, which makes the `optional` dummy absent), so `clean` below must come
    !!    back with no nulls AND its values intact;
    !!  * a column WITH nulls has its mask built by walking the validity bitmap 64 bits at a time,
    !!    so the null rows are placed deliberately at and around block boundaries (64/65, 128/129)
    !!    and at the very first and last row, which is where an off-by-one in that walk shows up.
    !!
    !! The vector column is not redundant with the scalar one: a vector row's validity is its first
    !! element's bit, so consecutive rows sit `width` bits apart and the walk has to derive which
    !! rows a block covers rather than reading them off directly.
    subroutine test_write_table_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NBIG = 200 !! spans four 64-bit validity blocks.
        integer, parameter :: NW = 3     !! vector width, so rows sit 3 bits apart.
        type(error_type), allocatable :: e2
        type(parquet_writer) :: w
        type(parquet_table) :: t, t2
        type(parquet_schema) :: s
        real(real64) :: f64(NBIG), clean(NBIG), fv(NW, NBIG)
        real(real64), allocatable :: back(:)
        logical :: valid_f(NBIG), valid_v(NW, NBIG)
        integer :: i
        integer, parameter :: fnull(6) = [1, 64, 65, 128, 129, 200]
        integer, parameter :: vnull(4) = [2, 64, 65, 130]
        character(len=*), parameter :: f = "test_run/table_write_nulls.parquet"
        character(len=*), parameter :: fo = "test_run/table_write_nulls_out.parquet"
        !
        do i = 1, NBIG
            f64(i) = real(i, real64)
            clean(i) = real(1000 + i, real64)
            fv(:, i) = real(i, real64)
        end do
        valid_f = .true.
        valid_v = .true.
        do i = 1, size(fnull)
            valid_f(fnull(i)) = .false.
        end do
        do i = 1, size(vnull)
            valid_v(:, vnull(i)) = .false.
        end do
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "f", f64, is_valid=valid_f)
        call parquet_write_column(w, "fv", fv, is_valid=valid_v)
        call parquet_write_column(w, "clean", clean)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call s%init("wn")
        call s%add_field("f", "float64")
        call s%add_field("fv", "float64", col_size=NW)
        call s%add_field("clean", "float64")
        call parquet_parse_maml(s)
        call parquet_write_table(t, fo, s)
        !
        call parquet_open_table(t2, fo)
        call check(error, t2%nrows() == int(NBIG, int64), "the rewritten table should keep its row count")
        if (allocated(error)) return
        call check_null_positions(t2, "f", fnull, NBIG, error)
        if (allocated(error)) return
        call check_null_positions(t2, "fv", vnull, NBIG, error)
        if (allocated(error)) return
        ! The no-mask fast path must not invent nulls, and must not disturb the values either.
        do i = 1, NBIG
            call check(e2, .not. t2%is_null("clean", int(i, int64)), &
                "a column written with no validity mask should come back with no nulls")
            if (allocated(e2)) then
                call move_alloc(e2, error)
                return
            end if
        end do
        call t2%get("clean", back)
        call check(error, all(back == clean), &
            "a column written with no validity mask should keep its values")
    end subroutine test_write_table_nulls
    !
    !> Asserts that exactly the rows listed in `nulls` are null in `name`, and no others.
    subroutine check_null_positions(t, name, nulls, nrow, error)
        type(parquet_table), intent(inout) :: t              !! the reopened table.
        character(len=*), intent(in) :: name                 !! column to check.
        integer, intent(in) :: nulls(:)                      !! the row numbers expected to be null.
        integer, intent(in) :: nrow                          !! total rows.
        type(error_type), allocatable, intent(out) :: error  !! set on the first disagreement.
        logical :: want
        integer :: i
        character(len=64) :: msg
        !
        do i = 1, nrow
            want = any(nulls == i)
            if (t%is_null(name, int(i, int64)) .eqv. want) cycle
            if (want) then
                write(msg, '(a,i0,a)') "row ", i, " should be null after the table write"
            else
                write(msg, '(a,i0,a)') "row ", i, " should NOT be null after the table write"
            end if
            call check(error, .false., trim(msg) // " (column " // name // ")")
            return
        end do
    end subroutine check_null_positions
    !
    !> A vector string column whose LAST row carries a null -- the case that caught the
    !! materializer indexing validity by flat element position. parquet_column's validity is
    !! row-granular even for a vector kind, so a per-element null has to be widened to the whole
    !! row; a flat (row-1)*width+element index runs past nrows and aborts the read outright.
    subroutine test_string_vector_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: w
        type(parquet_table) :: t
        character(len=6) :: sv(NVEC, NROW)
        logical :: valid(NVEC, NROW)
        integer :: i, e
        character(len=*), parameter :: f = "test_run/table_strvec_nulls.parquet"
        !
        do i = 1, NROW
            do e = 1, NVEC
                write(sv(e, i), '(a,i0,i0)') "s", i, e
            end do
        end do
        ! First element deliberately the shortest (CLAUDE.md).
        sv(1, 1) = "a"
        valid = .true.
        valid(2, NROW) = .false.   ! a late row: a flat index here exceeds nrows
        valid(1, 4) = .false.
        !
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "sv", sv, is_valid=valid)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call check(error, t%kind("sv") == PK_STRING_VEC, "sv should resolve to PK_STRING_VEC")
        if (allocated(error)) return
        call check(error, t%is_null("sv", int(NROW, int64)), &
            "a null element in the last row should make that row null")
        if (allocated(error)) return
        call check(error, t%is_null("sv", 4_int64), &
            "a null element in row 4 should make row 4 null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("sv", 1_int64), &
            "a row with no null element should not report null")
        if (allocated(error)) return
        call check(error, .not. t%is_null("sv", 2_int64), &
            "a row with no null element should not report null")
    end subroutine test_string_vector_nulls
    !
    !> Opening classifies but reads nothing, and each accessor pulls in exactly the column it
    !! was asked about -- the whole point of the lazy layer, and the easiest property to lose
    !! silently (a stray whole-table read at open still passes every value assertion).
    subroutine test_lazy_first_touch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64), pointer :: p(:)
        integer :: i
        character(len=:), allocatable :: names(:)
        character(len=*), parameter :: f = "test_run/table_lazy.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%column_names(names)
        do i = 1, size(names)
            call check(error, t%residency(trim(names(i))) == RES_EMPTY, &
                "no column may be resident before it is touched: "//trim(names(i)))
            if (allocated(error)) return
        end do
        !
        ! %get touches one column and only that one.
        call t%get("f64", g)
        call check(error, t%residency("f64") == RES_FULL, "%get should make its column resident")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_EMPTY, &
            "%get on one column must not read any other")
        if (allocated(error)) return
        call check(error, abs(g(2) - 4.5_real64) < 1.0e-12_real64, &
            "a lazily read column must hold the file's values")
        if (allocated(error)) return
        ! %col triggers a first touch of its own, on a column nothing has read yet.
        call parquet_open_table(t, f)
        call check(error, t%residency("f64") == RES_EMPTY, "precondition: the reopened table is empty")
        if (allocated(error)) return
        call t%col("f64", p)
        call check(error, t%residency("f64") == RES_FULL, "%col should make its column resident")
        if (allocated(error)) return
        call check(error, abs(p(2) - 4.5_real64) < 1.0e-12_real64, &
            "the pointer must alias the lazily read values")
        if (allocated(error)) return
        ! %is_null is a value question, so it touches as well.
        call check(error, .not. t%is_null("b", 1_int64), "b row 1 should not be null")
        if (allocated(error)) return
        call check(error, t%residency("b") == RES_FULL, "%is_null should make its column resident")
    end subroutine test_lazy_first_touch
    !
    subroutine test_prefetch(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        logical :: got
        character(len=*), parameter :: f = "test_run/table_prefetch.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%prefetch("f64")
        call check(error, t%residency("f64") == RES_FULL, "prefetch should read its column")
        if (allocated(error)) return
        call check(error, t%residency("i32") == RES_EMPTY, "prefetch should read nothing else")
        if (allocated(error)) return
        ! Prefetching an already-resident column is a no-op, not an error.
        call t%prefetch("f64")
        call check(error, t%residency("f64") == RES_FULL, "prefetching twice should be harmless")
        if (allocated(error)) return
        !
        call t%prefetch(["i32", "f32"])
        call check(error, t%residency("i32") == RES_FULL .and. t%residency("f32") == RES_FULL, &
            "the array form should read every name it is given")
        if (allocated(error)) return
        call check(error, t%residency("s") == RES_EMPTY, "the array form should read nothing else")
        if (allocated(error)) return
        ! found= turns a missing name into a report rather than an abort, and the names that DO
        ! exist are still read.
        call t%prefetch(["i64 ", "nope"], found=got)
        call check(error, .not. got, "prefetch should report a missing name through found=")
        if (allocated(error)) return
        call check(error, t%residency("i64") == RES_FULL, &
            "a missing name must not stop the other names being read")
        if (allocated(error)) return
        !
        call t%materialize_all()
        call check(error, t%residency("s") == RES_FULL, "materialize_all should read the rest")
        if (allocated(error)) return
        call t%materialize_all()
        call check(error, t%residency("s") == RES_FULL, "materialize_all should be idempotent")
    end subroutine test_prefetch
    !
    subroutine test_reload(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        real(real64), allocatable :: g(:)
        real(real64) :: edited(NROW)
        character(len=*), parameter :: f = "test_run/table_reload.parquet"
        !
        call write_basic_fixture(f)
        call parquet_open_table(t, f)
        call t%get("f64", g)
        edited = 0.0_real64
        call t%set("f64", edited)
        call t%get("f64", g)
        call check(error, all(abs(g) < 1.0e-12_real64), "precondition: set replaced the values")
        if (allocated(error)) return
        !
        call t%reload("f64")
        call t%get("f64", g)
        call check(error, abs(g(2) - 4.5_real64) < 1.0e-12_real64, &
            "reload should bring back the file's own values")
        if (allocated(error)) return
        call check(error, t%residency("f64") == RES_FULL, "a reloaded column is resident")
        if (allocated(error)) return
        ! Reloading a column that was never touched is just a first touch.
        call t%reload("i32")
        call check(error, t%residency("i32") == RES_FULL, &
            "reloading an untouched column should simply read it")
    end subroutine test_reload
    !
    subroutine test_unsupported_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        integer(int64), allocatable :: ids(:)
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
        call check(error, t%residency("id") == RES_EMPTY, &
            "a supported column starts empty like any other")
        if (allocated(error)) return
        call t%get("id", ids)
        call check(error, t%residency("id") == RES_FULL, &
            "a supported column should still be readable despite an unsupported sibling")
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
    !> All 18 kinds again, but long enough to span several row groups -- which is what the slice
    !! regime is about, and what a 6-row single-row-group fixture cannot exercise at all.
    !! `chunk` forces the row-group size, so the boundaries are known to the test rather than
    !! left to the writer's own auto-sizing.
    subroutine write_slice_fixture(fname, n, chunk)
        character(len=*), intent(in) :: fname !! file to write.
        integer, intent(in) :: n              !! rows to write.
        integer, intent(in) :: chunk          !! rows per row group.
        type(parquet_writer) :: w
        integer :: i, e
        integer(int32), allocatable :: a_i32(:), v_i32(:,:)
        integer(int64), allocatable :: a_i64(:), v_i64(:,:)
        real(real32), allocatable :: a_f32(:), v_f32(:,:)
        real(real64), allocatable :: a_f64(:), v_f64(:,:)
        logical, allocatable :: a_bool(:), v_bool(:,:), valid(:)
        character(len=8), allocatable :: a_str(:), v_str(:,:)
        type(parquet_date), allocatable :: a_date(:), v_date(:,:)
        type(parquet_time), allocatable :: a_time(:), v_time(:,:)
        type(parquet_timestamp), allocatable :: a_ts(:), v_ts(:,:)
        !
        allocate(a_i32(n), a_i64(n), a_f32(n), a_f64(n), a_bool(n), a_str(n), valid(n))
        allocate(a_date(n), a_time(n), a_ts(n))
        allocate(v_i32(NVEC, n), v_i64(NVEC, n), v_f32(NVEC, n), v_f64(NVEC, n))
        allocate(v_bool(NVEC, n), v_str(NVEC, n), v_date(NVEC, n), v_time(NVEC, n), v_ts(NVEC, n))
        do i = 1, n
            a_i32(i) = i
            a_i64(i) = int(i, int64) * 1000_int64
            a_f32(i) = real(i, real32) * 0.25_real32
            a_f64(i) = real(i, real64) * 1.75_real64
            a_bool(i) = mod(i, 2) == 1
            a_date(i) = parquet_date(2026, 1, 1 + mod(i, 28))
            a_time(i) = parquet_time(1, 2, mod(i, 60))
            a_ts(i) = parquet_timestamp(2026, 1, 1 + mod(i, 28), 3, 4, mod(i, 60))
            ! Deliberately the shortest first, so a "sized from the first element" bug shows up.
            write(a_str(i), '(a,i0)') "r", i
            do e = 1, NVEC
                v_i32(e, i) = i * 10 + e
                v_i64(e, i) = int(i * 10 + e, int64) * 1000_int64
                v_f32(e, i) = real(i * 10 + e, real32) * 0.5_real32
                v_f64(e, i) = real(i * 10 + e, real64) * 1.5_real64
                v_bool(e, i) = mod(i + e, 2) == 0
                v_date(e, i) = parquet_date(2026, 2, e)
                v_time(e, i) = parquet_time(5, 6, e)
                v_ts(e, i) = parquet_timestamp(2026, 2, e, 7, 8, 9)
                write(v_str(e, i), '(a,i0,i0)') "v", i, e
            end do
        end do
        a_str(1) = "a"
        v_str(1, 1) = "b"
        ! One null per validity dispatch class, placed in the middle so a slice can straddle it.
        valid = .true.
        valid(n / 2) = .false.
        a_date(n / 2 + 1) = parquet_date()
        !
        call parquet_open_writer(w, fname, chunk_size=chunk)
        call parquet_write_column(w, "s_i32", a_i32, is_valid=valid)
        call parquet_write_column(w, "s_i64", a_i64)
        call parquet_write_column(w, "s_f32", a_f32)
        call parquet_write_column(w, "s_f64", a_f64)
        call parquet_write_column(w, "s_bool", a_bool)
        call parquet_write_column(w, "s_str", a_str, is_valid=valid)
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
    end subroutine write_slice_fixture
    !
    subroutine test_row_group_bounds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_reader) :: r
        integer(int64), allocatable :: bounds(:,:), tbounds(:,:)
        integer(int64) :: nrg, rg
        character(len=*), parameter :: f = "test_run/table_rgbounds.parquet"
        !
        call write_slice_fixture(f, 20, 7)
        !
        ! The standalone planning form: no table needed, and none open.
        call parquet_table_row_group_bounds(f, bounds)
        call parquet_open_reader(r, f)
        call parquet_get_num_row_groups(r, nrg)
        call parquet_close_reader(r)
        call check(error, size(bounds, 2, kind=int64) == nrg, &
            "there should be one bounds entry per row group")
        if (allocated(error)) return
        call check(error, bounds(1, 1) == 1_int64, "the first row group must start at row 1")
        if (allocated(error)) return
        call check(error, bounds(2, nrg) == 20_int64, "the last row group must end at the last row")
        if (allocated(error)) return
        do rg = 2_int64, nrg
            call check(error, bounds(1, rg) == bounds(2, rg - 1) + 1_int64, &
                "row groups must partition the rows with no gap and no overlap")
            if (allocated(error)) return
        end do
        !
        ! The table-level form answers the same thing, in FILE row numbering, even for a slice.
        call parquet_open_table(t, f, 9, 12)
        call t%row_group_bounds(tbounds)
        call check(error, all(tbounds == bounds), &
            "a slice table should report the file's own row groups, not the slice's")
        if (allocated(error)) return
        !
        ! A full-regime table never precomputes rg_bounds at open time (only the slice regime
        ! does, to plan which row groups it needs) -- so this must compute them on demand.
        call parquet_open_table(t, f)
        call t%row_group_bounds(tbounds)
        call check(error, all(tbounds == bounds), &
            "a full-regime table should compute row groups on demand too")
    end subroutine test_row_group_bounds
    !
    !> The slice regime over every kind, with the slice deliberately straddling two row-group
    !! boundaries so that both the head trim and the tail trim are exercised, and the middle row
    !! group is taken whole.
    subroutine test_slice_kind_matrix(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: full, sl
        integer, parameter :: N = 20, CH = 7, LO = 6, HI = 16
        integer(int32), allocatable :: f_i32(:), s_i32(:), fv_i32(:,:), sv_i32(:,:)
        integer(int64), allocatable :: f_i64(:), s_i64(:), fv_i64(:,:), sv_i64(:,:)
        real(real32), allocatable :: f_f32(:), s_f32(:), fv_f32(:,:), sv_f32(:,:)
        real(real64), allocatable :: f_f64(:), s_f64(:), fv_f64(:,:), sv_f64(:,:)
        logical, allocatable :: f_bool(:), s_bool(:), fv_bool(:,:), sv_bool(:,:)
        character(len=:), allocatable :: f_str(:), s_str(:), fv_str(:,:), sv_str(:,:)
        type(parquet_date), allocatable :: f_date(:), s_date(:), fv_date(:,:), sv_date(:,:)
        type(parquet_time), allocatable :: f_time(:), s_time(:), fv_time(:,:), sv_time(:,:)
        type(parquet_timestamp), allocatable :: f_ts(:), s_ts(:), fv_ts(:,:), sv_ts(:,:)
        ! A timestamp's civil fields are reached through its date part rather than directly, and
        ! a chained x%get_date()%raw() is not valid Fortran, so the part needs a name.
        type(parquet_date) :: dp_a, dp_b
        integer :: i
        character(len=*), parameter :: f = "test_run/table_slice_matrix.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(full, f)
        call parquet_open_table(sl, f, LO, HI)
        !
        call check(error, sl%nrows() == int(HI - LO + 1, int64), &
            "a slice table's row count is the slice's own length")
        if (allocated(error)) return
        call check(error, full%nrows() == int(N, int64), "the full table still covers every row")
        if (allocated(error)) return
        !
        call full%get("s_i32", f_i32);  call sl%get("s_i32", s_i32)
        call check(error, all(s_i32 == f_i32(LO:HI)), "s_i32 slice should equal the full column's rows")
        if (allocated(error)) return
        ! Values alone do not prove the assembly is right: a slice is built row group by row
        ! group, and each piece's validity has to land on the same rows its values did. The
        ! fixture's null sits at row N/2, inside this slice, so a null that is dropped, kept
        ! after it should have been replaced, or shifted by a row shows up here -- checked for
        ! both validity dispatch classes that have a bitmap or a store behind them.
        do i = 1, HI - LO + 1
            call check(error, sl%is_null("s_i32", int(i, int64)) .eqv. &
                full%is_null("s_i32", int(LO + i - 1, int64)), &
                "s_i32 slice validity should match the same row of the full column")
            if (allocated(error)) return
            call check(error, sl%is_null("s_str", int(i, int64)) .eqv. &
                full%is_null("s_str", int(LO + i - 1, int64)), &
                "s_str slice validity should match the same row of the full column")
            if (allocated(error)) return
        end do
        call full%get("s_i64", f_i64);  call sl%get("s_i64", s_i64)
        call check(error, all(s_i64 == f_i64(LO:HI)), "s_i64 slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_f32", f_f32);  call sl%get("s_f32", s_f32)
        call check(error, all(abs(s_f32 - f_f32(LO:HI)) < 1.0e-6_real32), &
            "s_f32 slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_f64", f_f64);  call sl%get("s_f64", s_f64)
        call check(error, all(abs(s_f64 - f_f64(LO:HI)) < 1.0e-12_real64), &
            "s_f64 slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_bool", f_bool); call sl%get("s_bool", s_bool)
        call check(error, all(s_bool .eqv. f_bool(LO:HI)), &
            "s_bool slice should equal the full column's rows")
        if (allocated(error)) return
        call full%get("s_str", f_str);  call sl%get("s_str", s_str)
        do i = 1, HI - LO + 1
            call check(error, trim(s_str(i)) == trim(f_str(LO + i - 1)), &
                "s_str slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        ! %raw is the comparator of choice for the temporal kinds: unlike the civil accessors
        ! it never aborts on a null element, and the fixture has one.
        call full%get("s_date", f_date); call sl%get("s_date", s_date)
        do i = 1, HI - LO + 1
            call check(error, s_date(i)%raw() == f_date(LO + i - 1)%raw() .and. &
                (s_date(i)%is_null() .eqv. f_date(LO + i - 1)%is_null()), &
                "s_date slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        call full%get("s_time", f_time); call sl%get("s_time", s_time)
        do i = 1, HI - LO + 1
            call check(error, s_time(i)%raw() == f_time(LO + i - 1)%raw(), &
                "s_time slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        call full%get("s_ts", f_ts);    call sl%get("s_ts", s_ts)
        do i = 1, HI - LO + 1
            dp_a = s_ts(i)%get_date()
            dp_b = f_ts(LO + i - 1)%get_date()
            call check(error, dp_a%raw() == dp_b%raw(), &
                "s_ts slice should equal the full column's rows")
            if (allocated(error)) return
        end do
        !
        call full%get("v_i32", fv_i32); call sl%get("v_i32", sv_i32)
        call check(error, all(sv_i32 == fv_i32(:, LO:HI)), "v_i32 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_i64", fv_i64); call sl%get("v_i64", sv_i64)
        call check(error, all(sv_i64 == fv_i64(:, LO:HI)), "v_i64 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_f32", fv_f32); call sl%get("v_f32", sv_f32)
        call check(error, all(abs(sv_f32 - fv_f32(:, LO:HI)) < 1.0e-6_real32), &
            "v_f32 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_f64", fv_f64); call sl%get("v_f64", sv_f64)
        call check(error, all(abs(sv_f64 - fv_f64(:, LO:HI)) < 1.0e-12_real64), &
            "v_f64 slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_bool", fv_bool); call sl%get("v_bool", sv_bool)
        call check(error, all(sv_bool .eqv. fv_bool(:, LO:HI)), "v_bool slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_str", fv_str); call sl%get("v_str", sv_str)
        call check(error, trim(sv_str(2, 1)) == trim(fv_str(2, LO)), &
            "v_str slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_date", fv_date); call sl%get("v_date", sv_date)
        call check(error, sv_date(2, 1)%raw() == fv_date(2, LO)%raw(), &
            "v_date slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_time", fv_time); call sl%get("v_time", sv_time)
        call check(error, sv_time(2, 1)%raw() == fv_time(2, LO)%raw(), &
            "v_time slice should equal the full rows")
        if (allocated(error)) return
        call full%get("v_ts", fv_ts);   call sl%get("v_ts", sv_ts)
        dp_a = sv_ts(2, 1)%get_date()
        dp_b = fv_ts(2, LO)%get_date()
        call check(error, dp_a%raw() == dp_b%raw(), "v_ts slice should equal the full rows")
        if (allocated(error)) return
        !
        ! Nulls have to survive the trim-and-concatenate too: the fixture's null row is inside
        ! this slice.
        do i = 1, HI - LO + 1
            call check(error, sl%is_null("s_i32", int(i, int64)) .eqv. &
                full%is_null("s_i32", int(LO + i - 1, int64)), &
                "a null must land on the same row after slicing")
            if (allocated(error)) return
        end do
    end subroutine test_slice_kind_matrix
    !
    !> The four slice shapes that differ in how much trimming they need: wholly inside one row
    !! group, exactly on row-group boundaries, the whole file, and a single row at each end.
    subroutine test_slice_shapes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: full, sl
        integer, parameter :: N = 20, CH = 7
        integer(int32), allocatable :: fg(:), sg(:)
        character(len=*), parameter :: f = "test_run/table_slice_shapes.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(full, f)
        call full%get("s_i32", fg)
        !
        ! Wholly inside row group 2 (rows 8..14).
        call parquet_open_table(sl, f, 9, 12)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 4 .and. all(sg == fg(9:12)), &
            "a slice inside one row group should read just those rows")
        if (allocated(error)) return
        !
        ! Exactly one whole row group: no trimming at either end.
        call parquet_open_table(sl, f, 8, 14)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 7 .and. all(sg == fg(8:14)), &
            "a row-group-aligned slice should need no trimming")
        if (allocated(error)) return
        !
        ! The whole file, expressed as a slice: must agree with the full regime exactly.
        call parquet_open_table(sl, f, 1, N)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == N .and. all(sg == fg), &
            "a whole-file slice should equal the full-regime table")
        if (allocated(error)) return
        !
        ! Single rows at both ends -- the extreme trims.
        call parquet_open_table(sl, f, 1, 1)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(1), &
            "a one-row slice at the start should read only row 1")
        if (allocated(error)) return
        call parquet_open_table(sl, f, N, N)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 1 .and. sg(1) == fg(N), &
            "a one-row slice at the end should read only the last row")
        if (allocated(error)) return
        !
        ! A slice crossing every boundary, read through the pointer path rather than the copy.
        call parquet_open_table(sl, f, 2, 19)
        call sl%get("s_i32", sg)
        call check(error, size(sg) == 18 .and. all(sg == fg(2:19)), &
            "a slice spanning all row groups should still line up")
    end subroutine test_slice_shapes
    !
    !> The row handle over every kind, plus the two properties that are easy to lose: it reads
    !! through a table declared WITHOUT `target` (it must point at the store, never at the
    !! table), and it triggers a lazy first touch of its own.
    subroutine test_row_view(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t          ! deliberately NOT `target` -- see the module doc
        type(parquet_table_row) :: r
        integer, parameter :: N = 20, CH = 7
        integer(int32) :: x_i32
        integer(int64) :: x_i64
        real(real32) :: x_f32
        real(real64) :: x_f64
        logical :: x_bool
        character(len=:), allocatable :: x_str, xv_str(:)
        type(parquet_date) :: x_date
        type(parquet_time) :: x_time
        type(parquet_timestamp) :: x_ts
        integer(int32), allocatable :: xv_i32(:)
        real(real64), allocatable :: xv_f64(:)
        integer(int64), allocatable :: w_i64(:)
        real(real64), allocatable :: wv_f64(:)
        integer(int64) :: w_scalar
        real(real64) :: w_f64
        character(len=*), parameter :: f = "test_run/table_rowview.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(t, f)
        !
        ! A handle on a table where nothing has been read yet.
        r = t%row(5)
        call check(error, r%index() == 5_int64, "a handle should report the row it was made for")
        if (allocated(error)) return
        call check(error, t%residency("s_i32") == RES_EMPTY, "precondition: nothing read yet")
        if (allocated(error)) return
        call r%get("s_i32", x_i32)
        call check(error, x_i32 == 5, "row 5 of s_i32 should be 5")
        if (allocated(error)) return
        call check(error, t%residency("s_i32") == RES_FULL, &
            "a row handle's get must trigger the same first touch the table's get does")
        if (allocated(error)) return
        !
        call r%get("s_i64", x_i64)
        call check(error, x_i64 == 5000_int64, "row 5 of s_i64 should be 5000")
        if (allocated(error)) return
        call r%get("s_f32", x_f32)
        call check(error, abs(x_f32 - 1.25_real32) < 1.0e-6_real32, "row 5 of s_f32 should be 1.25")
        if (allocated(error)) return
        call r%get("s_f64", x_f64)
        call check(error, abs(x_f64 - 8.75_real64) < 1.0e-12_real64, "row 5 of s_f64 should be 8.75")
        if (allocated(error)) return
        call r%get("s_bool", x_bool)
        call check(error, x_bool, "row 5 of s_bool should be true")
        if (allocated(error)) return
        call r%get("s_str", x_str)
        call check(error, x_str == "r5", "row 5 of s_str should be r5")
        if (allocated(error)) return
        call r%get("s_date", x_date)
        call check(error, x_date%day() == 1 + mod(5, 28), "row 5 of s_date should keep its day")
        if (allocated(error)) return
        call r%get("s_time", x_time)
        call check(error, x_time%second() == mod(5, 60), "row 5 of s_time should keep its second")
        if (allocated(error)) return
        call r%get("s_ts", x_ts)
        call check(error, .not. x_ts%is_null(), "row 5 of s_ts should not be null")
        if (allocated(error)) return
        !
        ! Vector kinds come back as one array per row.
        call r%get("v_i32", xv_i32)
        call check(error, size(xv_i32) == NVEC .and. xv_i32(2) == 52, &
            "a vector row should come back width-long, in element order")
        if (allocated(error)) return
        call r%get("v_f64", xv_f64)
        call check(error, abs(xv_f64(3) - 79.5_real64) < 1.0e-12_real64, &
            "a float64 vector row should hold its own values")
        if (allocated(error)) return
        call r%get("v_str", xv_str)
        call check(error, size(xv_str) == NVEC .and. trim(xv_str(1)) == "v51", &
            "a string vector row should come back width-long")
        if (allocated(error)) return
        !
        ! Widening works exactly as it does on the table's own %get.
        call r%get("s_i32", w_scalar)
        call check(error, w_scalar == 5_int64, "a row get should widen int32 into int64")
        if (allocated(error)) return
        call r%get("s_f32", w_f64)
        call check(error, abs(w_f64 - 1.25_real64) < 1.0e-6_real64, &
            "a row get should widen float32 into float64")
        if (allocated(error)) return
        call r%get("v_i32", w_i64)
        call check(error, w_i64(2) == 52_int64, "a row get should widen an int32 vector too")
        if (allocated(error)) return
        call r%get("v_f32", wv_f64)
        call check(error, abs(wv_f64(2) - 26.0_real64) < 1.0e-5_real64, &
            "a row get should widen a float32 vector too")
        if (allocated(error)) return
        !
        ! The remaining kinds, so every generated row_get_* specific is exercised rather than
        ! sampled -- each is its own procedure, and an untested one is untested code.
        block
            integer(int64), allocatable :: yv_i64(:)
            real(real32), allocatable :: yv_f32(:)
            logical, allocatable :: yv_bool(:)
            type(parquet_date), allocatable :: yv_date(:)
            type(parquet_time), allocatable :: yv_time(:)
            type(parquet_timestamp), allocatable :: yv_ts(:)
            r = t%row(5)
            call r%get("v_i64", yv_i64)
            call check(error, yv_i64(2) == 52000_int64, "row 5 of v_i64, element 2")
            if (allocated(error)) return
            call r%get("v_f32", yv_f32)
            call check(error, abs(yv_f32(2) - 26.0_real32) < 1.0e-5_real32, "row 5 of v_f32, element 2")
            if (allocated(error)) return
            call r%get("v_bool", yv_bool)
            call check(error, size(yv_bool) == NVEC, "row 5 of v_bool should be width-long")
            if (allocated(error)) return
            call r%get("v_date", yv_date)
            call check(error, yv_date(2)%day() == 2, "row 5 of v_date, element 2 keeps its day")
            if (allocated(error)) return
            call r%get("v_time", yv_time)
            call check(error, yv_time(2)%second() == 2, "row 5 of v_time, element 2 keeps its second")
            if (allocated(error)) return
            call r%get("v_ts", yv_ts)
            call check(error, .not. yv_ts(2)%is_null(), "row 5 of v_ts, element 2 should not be null")
            if (allocated(error)) return
        end block
        !
        ! Nulls, and a handle on a slice table, whose row 1 is the slice's own first row.
        r = t%row(int(N / 2, int64))
        call check(error, r%is_null("s_i32"), "the fixture's null row should report null")
        if (allocated(error)) return
        call parquet_open_table(t, f, 6, 16)
        r = t%row(1)
        call r%get("s_i32", x_i32)
        call check(error, x_i32 == 6, "row 1 of a [6,16] slice is the file's row 6")
    end subroutine test_row_view
    !
    !> Every slice form against the same column, each checked against the equivalent Fortran
    !! array section so the expected answer is not restated by hand.
    subroutine test_get_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_slice) :: s
        integer, parameter :: N = 20, CH = 7
        integer(int32), allocatable :: full(:), part(:)
        integer(int64), allocatable :: wide(:)
        real(real64), allocatable :: fv(:,:), pv(:,:)
        character(len=:), allocatable :: cs(:)
        type(parquet_string_column) :: sc_out, sc_null
        real(real64), allocatable :: wf(:), wvf(:,:)
        character(len=*), parameter :: f = "test_run/table_getslice.parquet"
        !
        call write_slice_fixture(f, N, CH)
        call parquet_open_table(t, f)
        call t%get("s_i32", full)
        !
        ! start:stop
        s = parquet_slice_range(3, 8)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 6 .and. all(part == full(3:8)), &
            "a start:stop slice should equal the same array section")
        if (allocated(error)) return
        ! start:stop:step
        s = parquet_slice_range(1, 10, 2)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 5 .and. all(part == full(1:10:2)), &
            "a strided slice should equal the same array section")
        if (allocated(error)) return
        ! open-ended start: -- resolved against the table, at use
        s = parquet_slice_range(17)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 4 .and. all(part == full(17:N)), &
            "an open-ended slice should run to the table's last row")
        if (allocated(error)) return
        ! descending
        s = parquet_slice_range(10, 6, -2)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 3 .and. all(part == full(10:6:-2)), &
            "a descending slice should equal the same array section")
        if (allocated(error)) return
        ! explicit list, deliberately out of order and with a repeat
        s = parquet_slice_list([9, 2, 2, 15])
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 4 .and. &
            all(part == [full(9), full(2), full(2), full(15)]), &
            "a list slice should gather in the order given, repeats included")
        if (allocated(error)) return
        ! the int64 constructors, and a descending slice with an open end (which runs to row 1)
        s = parquet_slice_list([9_int64, 2_int64])
        call t%get_slice("s_i32", s, part)
        call check(error, all(part == [full(9), full(2)]), &
            "the int64 list constructor should gather the same rows")
        if (allocated(error)) return
        s = parquet_slice_range(3_int64, 8_int64, 2_int64)
        call t%get_slice("s_i32", s, part)
        call check(error, all(part == full(3:8:2)), &
            "the int64 range constructor should select the same rows")
        if (allocated(error)) return
        s = parquet_slice_range(4, step=-1)
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 4 .and. all(part == full(4:1:-1)), &
            "a descending slice with no stop should run down to row 1")
        if (allocated(error)) return
        ! an empty gather is legal and yields nothing
        s = parquet_slice_list([integer(int32) ::])
        call t%get_slice("s_i32", s, part)
        call check(error, size(part) == 0, "an empty list slice should select no rows")
        if (allocated(error)) return
        ! widening, exactly as %get does
        s = parquet_slice_range(3, 5)
        call t%get_slice("s_i32", s, wide)
        call check(error, all(wide == int(full(3:5), int64)), &
            "get_slice should widen int32 into int64")
        if (allocated(error)) return
        ! a vector column keeps its (element, row) shape
        call t%get("v_f64", fv)
        call t%get_slice("v_f64", s, pv)
        call check(error, size(pv, 1) == NVEC .and. size(pv, 2) == 3 .and. &
            all(abs(pv - fv(:, 3:5)) < 1.0e-12_real64), &
            "a sliced vector column should keep its (element, row) shape")
        if (allocated(error)) return
        ! strings, in both the character and the compact forms
        call t%get_slice("s_str", s, cs)
        call check(error, size(cs) == 3 .and. trim(cs(1)) == "r3", &
            "a sliced string column should come back in row order")
        if (allocated(error)) return
        call t%get_slice("s_str", s, sc_out)
        call check(error, sc_out%size() == 3_int64, &
            "the compact form should hold one element per selected row")
        if (allocated(error)) return
        ! float32 -> float64 widening, scalar and vector (get_slice's own widen path, distinct
        ! from %get's -- each generated get_slice_* specific has its own widen branch).
        call t%get_slice("s_f32", s, wf)
        block
            real(real32), allocatable :: r_f32full(:), r_f32vfull(:,:)
            call t%get("s_f32", r_f32full)
            call check(error, all(abs(wf - real(r_f32full(3:5), real64)) < 1.0e-6_real64), &
                "get_slice should widen float32 into float64")
            if (allocated(error)) return
            call t%get_slice("v_f32", s, wvf)
            call t%get("v_f32", r_f32vfull)
            call check(error, all(abs(wvf - real(r_f32vfull(:, 3:5), real64)) < 1.0e-6_real64), &
                "get_slice should widen a float32 vector column into a float64 array")
            if (allocated(error)) return
        end block
        !
        ! The remaining kinds, so every generated get_slice_* specific is exercised. Each is
        ! checked against the equivalent array section of the same column read whole.
        block
            integer(int64), allocatable :: q_i64(:), qv_i64(:,:)
            real(real32), allocatable :: q_f32(:), qv_f32(:,:)
            real(real64), allocatable :: q_f64(:)
            logical, allocatable :: q_bool(:), qv_bool(:,:)
            integer(int32), allocatable :: qv_i32(:,:)
            type(parquet_date), allocatable :: q_date(:), qv_date(:,:)
            type(parquet_time), allocatable :: q_time(:), qv_time(:,:)
            type(parquet_timestamp), allocatable :: q_ts(:), qv_ts(:,:)
            character(len=:), allocatable :: qv_str(:,:)
            integer(int64), allocatable :: r_i64(:)
            real(real32), allocatable :: r_f32(:)
            real(real64), allocatable :: r_f64(:)
            logical, allocatable :: r_bool(:)
            !
            call t%get_slice("s_i64", s, q_i64); call t%get("s_i64", r_i64)
            call check(error, all(q_i64 == r_i64(3:5)), "s_i64 slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_f32", s, q_f32); call t%get("s_f32", r_f32)
            call check(error, all(abs(q_f32 - r_f32(3:5)) < 1.0e-6_real32), &
                "s_f32 slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_f64", s, q_f64); call t%get("s_f64", r_f64)
            call check(error, all(abs(q_f64 - r_f64(3:5)) < 1.0e-12_real64), &
                "s_f64 slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_bool", s, q_bool); call t%get("s_bool", r_bool)
            call check(error, all(q_bool .eqv. r_bool(3:5)), "s_bool slice equals its array section")
            if (allocated(error)) return
            call t%get_slice("s_date", s, q_date)
            call check(error, size(q_date) == 3, "s_date slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("s_time", s, q_time)
            call check(error, size(q_time) == 3, "s_time slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("s_ts", s, q_ts)
            call check(error, size(q_ts) == 3, "s_ts slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_i32", s, qv_i32)
            call check(error, size(qv_i32, 1) == NVEC .and. size(qv_i32, 2) == 3, &
                "v_i32 slice keeps its (element, row) shape")
            if (allocated(error)) return
            call t%get_slice("v_i64", s, qv_i64)
            call check(error, size(qv_i64, 2) == 3, "v_i64 slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_f32", s, qv_f32)
            call check(error, size(qv_f32, 2) == 3, "v_f32 slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_bool", s, qv_bool)
            call check(error, size(qv_bool, 2) == 3, "v_bool slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_date", s, qv_date)
            call check(error, size(qv_date, 2) == 3, "v_date slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_time", s, qv_time)
            call check(error, size(qv_time, 2) == 3, "v_time slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_ts", s, qv_ts)
            call check(error, size(qv_ts, 2) == 3, "v_ts slice should hold the selected rows")
            if (allocated(error)) return
            call t%get_slice("v_str", s, qv_str)
            call check(error, size(qv_str, 1) == NVEC .and. size(qv_str, 2) == 3, &
                "v_str slice keeps its (element, row) shape")
            if (allocated(error)) return
            ! Widening on the vector path too.
            call t%get_slice("v_i32", s, qv_i64)
            call check(error, size(qv_i64, 2) == 3, "get_slice should widen an int32 vector column")
            if (allocated(error)) return
        end block
        !
        ! A slice covering a null string row must widen the null itself, not just the shape.
        s = parquet_slice_range(9, 11)
        call t%get_slice("s_str", s, sc_null)
        call check(error, sc_null%is_null(2_int64), &
            "get_slice should widen a null string row into a null element rather than an empty one")
        if (allocated(error)) return
        !
        ! A slice is relative to the TABLE, so on a slice-regime table row 1 is its own first row.
        call parquet_open_table(t, f, 6, 16)
        s = parquet_slice_range(1, 3)
        call t%get_slice("s_i32", s, part)
        call check(error, all(part == full(6:8)), &
            "a slice of a slice-regime table counts from that table's own first row")
    end subroutine test_get_slice
    !
    !> The parallel-per-row-group shape: each iteration opens its OWN slice table and reads it
    !! lazily. That first touch happens inside a parallel region, and it must be allowed --
    !! a table a thread opened itself cannot be shared with another thread, which is exactly the
    !! distinction the first-touch guard draws. A blanket "no first touch in a parallel region"
    !! rule would make the slice regime unusable where it matters most.
    subroutine test_parallel_private_slices(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer(int64), allocatable :: bounds(:,:)
        integer(int32), allocatable :: full(:)
        integer(int64) :: total
        integer :: rg
        character(len=*), parameter :: f = "test_run/table_parallel_slices.parquet"
        !
        call write_slice_fixture(f, 20, 7)
        call parquet_open_table(t, f)
        call t%get("s_i32", full)
        call parquet_table_row_group_bounds(f, bounds)
        !
        total = 0_int64
        ! The per-thread table is declared in a BLOCK inside the loop, not with private(t).
        ! An OpenMP private copy of a finalizable derived type is not reliably default-
        ! initialized by gfortran, so `parquet_open_table`'s intent(out) finalizer runs over an
        ! undefined `cache` pointer and the program dies in the allocator -- confirmed, and
        ! reproducible even with OMP_NUM_THREADS=1. A block-local is properly initialized on
        ! entry and finalized at exit, which is what a per-thread table wants anyway.
        !$omp parallel do default(shared) private(rg) reduction(+:total)
        do rg = 1, size(bounds, 2)
            block
                type(parquet_table) :: mine
                integer(int32), allocatable :: part(:)
                call parquet_open_table(mine, f, bounds(1, rg), bounds(2, rg))
                call mine%get("s_i32", part)   ! lazy first touch, inside the region
                total = total + sum(int(part, int64))
            end block
        end do
        !$omp end parallel do
        !
        call check(error, total == sum(int(full, int64)), &
            "reading every row group's own slice should cover the file exactly once")
    end subroutine test_parallel_private_slices
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
    !> found= on every %get/%col specific, swept the same way as test_kind_matrix_col_set: each
    !! kind's %get/%col has its own generated "column not found" early-return branch, so it is
    !! untested code unless a bogus name is actually looked up through that exact specific.
    subroutine test_kind_matrix_found(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        character(len=*), parameter :: f = "test_run/table_matrix_found.parquet"
        character(len=*), parameter :: miss = "no_such_column"
        logical :: ok
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
        character(len=:), allocatable :: g_chr(:), gv_chr(:,:)
        type(parquet_date), allocatable :: g_date(:), gv_date(:,:)
        type(parquet_time), allocatable :: g_time(:), gv_time(:,:)
        type(parquet_timestamp), allocatable :: g_ts(:), gv_ts(:,:)
        type(parquet_string_column) :: g_str
        !
        call write_matrix_fixture(f)
        call parquet_open_table(t, f)
        !
        ! --- %col: every kind with a pointer path ---
        call t%col(miss, p_i32, found=ok);   call check(error, .not. ok .and. .not. associated(p_i32), "col i32 miss")
        if (allocated(error)) return
        call t%col(miss, p_i64, found=ok);   call check(error, .not. ok .and. .not. associated(p_i64), "col i64 miss")
        if (allocated(error)) return
        call t%col(miss, p_f32, found=ok);   call check(error, .not. ok .and. .not. associated(p_f32), "col f32 miss")
        if (allocated(error)) return
        call t%col(miss, p_f64, found=ok);   call check(error, .not. ok .and. .not. associated(p_f64), "col f64 miss")
        if (allocated(error)) return
        call t%col(miss, p_bool, found=ok);  call check(error, .not. ok .and. .not. associated(p_bool), "col bool miss")
        if (allocated(error)) return
        call t%col(miss, p_date, found=ok);  call check(error, .not. ok .and. .not. associated(p_date), "col date miss")
        if (allocated(error)) return
        call t%col(miss, p_time, found=ok);  call check(error, .not. ok .and. .not. associated(p_time), "col time miss")
        if (allocated(error)) return
        call t%col(miss, p_ts, found=ok);    call check(error, .not. ok .and. .not. associated(p_ts), "col ts miss")
        if (allocated(error)) return
        call t%col(miss, p_i32v, found=ok);  call check(error, .not. ok .and. .not. associated(p_i32v), "col i32v miss")
        if (allocated(error)) return
        call t%col(miss, p_i64v, found=ok);  call check(error, .not. ok .and. .not. associated(p_i64v), "col i64v miss")
        if (allocated(error)) return
        call t%col(miss, p_f32v, found=ok);  call check(error, .not. ok .and. .not. associated(p_f32v), "col f32v miss")
        if (allocated(error)) return
        call t%col(miss, p_f64v, found=ok);  call check(error, .not. ok .and. .not. associated(p_f64v), "col f64v miss")
        if (allocated(error)) return
        call t%col(miss, p_boolv, found=ok); call check(error, .not. ok .and. .not. associated(p_boolv), "col boolv miss")
        if (allocated(error)) return
        call t%col(miss, p_datev, found=ok); call check(error, .not. ok .and. .not. associated(p_datev), "col datev miss")
        if (allocated(error)) return
        call t%col(miss, p_timev, found=ok); call check(error, .not. ok .and. .not. associated(p_timev), "col timev miss")
        if (allocated(error)) return
        call t%col(miss, p_tsv, found=ok);   call check(error, .not. ok .and. .not. associated(p_tsv), "col tsv miss")
        if (allocated(error)) return
        !
        ! --- %get: every kind, scalar and vector, plus both string forms ---
        call t%get(miss, g_i32, found=ok);   call check(error, .not. ok .and. size(g_i32) == 0, "get i32 miss")
        if (allocated(error)) return
        call t%get(miss, g_i64, found=ok);   call check(error, .not. ok .and. size(g_i64) == 0, "get i64 miss")
        if (allocated(error)) return
        call t%get(miss, g_f32, found=ok);   call check(error, .not. ok .and. size(g_f32) == 0, "get f32 miss")
        if (allocated(error)) return
        call t%get(miss, g_f64, found=ok);   call check(error, .not. ok .and. size(g_f64) == 0, "get f64 miss")
        if (allocated(error)) return
        call t%get(miss, g_bool, found=ok);  call check(error, .not. ok .and. size(g_bool) == 0, "get bool miss")
        if (allocated(error)) return
        call t%get(miss, g_date, found=ok);  call check(error, .not. ok .and. size(g_date) == 0, "get date miss")
        if (allocated(error)) return
        call t%get(miss, g_time, found=ok);  call check(error, .not. ok .and. size(g_time) == 0, "get time miss")
        if (allocated(error)) return
        call t%get(miss, g_ts, found=ok);    call check(error, .not. ok .and. size(g_ts) == 0, "get ts miss")
        if (allocated(error)) return
        call t%get(miss, gv_i32, found=ok);  call check(error, .not. ok .and. size(gv_i32) == 0, "get i32v miss")
        if (allocated(error)) return
        call t%get(miss, gv_i64, found=ok);  call check(error, .not. ok .and. size(gv_i64) == 0, "get i64v miss")
        if (allocated(error)) return
        call t%get(miss, gv_f32, found=ok);  call check(error, .not. ok .and. size(gv_f32) == 0, "get f32v miss")
        if (allocated(error)) return
        call t%get(miss, gv_f64, found=ok);  call check(error, .not. ok .and. size(gv_f64) == 0, "get f64v miss")
        if (allocated(error)) return
        call t%get(miss, gv_bool, found=ok); call check(error, .not. ok .and. size(gv_bool) == 0, "get boolv miss")
        if (allocated(error)) return
        call t%get(miss, gv_date, found=ok); call check(error, .not. ok .and. size(gv_date) == 0, "get datev miss")
        if (allocated(error)) return
        call t%get(miss, gv_time, found=ok); call check(error, .not. ok .and. size(gv_time) == 0, "get timev miss")
        if (allocated(error)) return
        call t%get(miss, gv_ts, found=ok);   call check(error, .not. ok .and. size(gv_ts) == 0, "get tsv miss")
        if (allocated(error)) return
        call t%get(miss, g_str, found=ok);   call check(error, .not. ok, "get str (compact) miss")
        if (allocated(error)) return
        call t%get(miss, g_chr, found=ok);   call check(error, .not. ok .and. size(g_chr) == 0, "get str (chr) miss")
        if (allocated(error)) return
        call t%get(miss, gv_chr, found=ok)
        call check(error, .not. ok .and. size(gv_chr) == 0, "get str vector (chrv) miss")
    end subroutine test_kind_matrix_found
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
