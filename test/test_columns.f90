!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the independent parquet_columns module (`parquet_column`, the type-erased
!> column-value foundation both parquet_table and the future container column types build on).
!> Runs in isolation from the rest of the library -- no reader, writer or parquet file involved.
!>
!> The suite is organised around the two things that are easy to get wrong in this type:
!>
!> * **the kind matrix** -- every kind must support every primitive, so the value round-trips
!>   are repeated per kind rather than spot-checked on one;
!> * **the three validity dispatch classes** (column bitmap / embedded string column / inside
!>   the element), which is where a "fixed the bitmap, forgot the temporal cache" bug lands, and
!>   which is therefore tested per class rather than per kind.
!>
!> Abort paths (kind mismatch, bad index, bad permutation, ...) cannot be exercised here because
!> `error stop` kills the process -- they live in test/error_scenarios.f90 as `columns_*`
!> scenarios, driven from test_errors.f90.
module test_columns
    use parquet_columns
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp, parquet_unit_millis
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_columns
    !
contains
    !
    subroutine collect_tests_parquet_columns(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("init sets kind, geometry and unit", test_init_geometry), &
            new_unittest("clear resets to an empty PK_NONE column", test_clear), &
            new_unittest("int32/int64 value round-trips", test_roundtrip_integer), &
            new_unittest("float32/float64 value round-trips", test_roundtrip_real), &
            new_unittest("logical value round-trips", test_roundtrip_logical), &
            new_unittest("string value round-trips (shortest element first)", test_roundtrip_string), &
            new_unittest("date/time/timestamp value round-trips", test_roundtrip_temporal), &
            new_unittest("vector kinds round-trip as (width, nrows)", test_roundtrip_vector), &
            new_unittest("string vector uses one flat store, stride width", test_roundtrip_string_vector), &
            new_unittest("data_ptr aliases live storage (writes show through)", test_data_ptr_aliases), &
            new_unittest("a null-free column allocates no validity bitmap", test_sparse_validity_unallocated), &
            new_unittest("set_null allocates the bitmap lazily", test_set_null_allocates_bitmap), &
            new_unittest("whole-column set_all drops the bitmap in O(1)", test_set_all_drops_bitmap), &
            new_unittest("set_all/set_at honour modify_nulls=.false.", test_modify_nulls_false), &
            new_unittest("set_at clears only its own null bit", test_set_at_clears_one_bit), &
            new_unittest("compact_validity drops the bitmap only when null-free", test_compact_validity), &
            new_unittest("vector kinds carry width*nrows validity bits", test_vector_validity_bits), &
            new_unittest("string validity delegates to the embedded column", test_string_validity_delegates), &
            new_unittest("temporal validity lives in the element and is cached", test_temporal_validity_cache), &
            new_unittest("append_values grows every kind", test_append_values), &
            new_unittest("append concatenates two columns and their nulls", test_append_column), &
            new_unittest("append_nulls adds all-null rows", test_append_nulls), &
            new_unittest("reindex permutes values and validity together", test_reindex), &
            new_unittest("delete_by_mask keeps order and recompacts", test_delete_by_mask), &
            new_unittest("deep_copy is independent of its source", test_deep_copy), &
            new_unittest("zero-row and no-op edge cases", test_edge_cases), &
            new_unittest("unit string is stored, copied and cleared", test_unit_string), &
            new_unittest("kindof/parquet_kind_name report the active kind", test_kind_reporting), &
            new_unittest("every scalar kind supports data_ptr and append_values", test_matrix_scalar_kinds), &
            new_unittest("every vector kind round-trips through all primitives", test_matrix_vector_kinds), &
            new_unittest("set_null/is_null work on every kind", test_matrix_set_null_all_kinds), &
            new_unittest("string vector set_at/get_at and modify_nulls", test_string_vector_set_at), &
            new_unittest("clear_null marks a row valid again", test_clear_null), &
            new_unittest("every PK_* constant has a name", test_kind_names_complete), &
            new_unittest("modify_nulls= is honoured by every kind", test_matrix_modify_nulls_all_kinds) &
            ]
    end subroutine collect_tests_parquet_columns
    !
    ! ==================================================================================
    ! Lifecycle and geometry
    ! ==================================================================================
    !
    subroutine test_init_geometry(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: u
        !
        call c%init(PK_FLOAT64, 7_int64, unit="m/s")
        call check(error, c%kindof() == PK_FLOAT64, "init should set kindof() to PK_FLOAT64")
        if (allocated(error)) return
        call check(error, c%length() == 7_int64, "init should set length() to the requested row count")
        if (allocated(error)) return
        call check(error, c%colwidth() == 1_int32, "a scalar kind should have width 1")
        if (allocated(error)) return
        call c%unit_string(u)
        call check(error, u == "m/s", "init should store the unit string")
        if (allocated(error)) return
        !
        call c%init(PK_INT32_VEC, 3_int64, width=4_int32)
        call check(error, c%colwidth() == 4_int32, "a vector kind should keep the requested width")
        if (allocated(error)) return
        call check(error, c%length() == 3_int64, "a vector column's length() counts ROWS, not elements")
        if (allocated(error)) return
        call c%unit_string(u)
        call check(error, u == "", "re-init should drop the previous unit")
    end subroutine test_init_geometry
    !
    subroutine test_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT64, 4_int64, unit="s")
        call c%set_null(1_int64)
        call c%clear()
        call check(error, c%kindof() == PK_NONE, "clear should reset the kind to PK_NONE")
        if (allocated(error)) return
        call check(error, c%length() == 0_int64, "clear should reset the row count to 0")
        if (allocated(error)) return
        call check(error, c%colwidth() == 1_int32, "clear should reset the width to 1")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, "clear should release the validity bitmap")
    end subroutine test_clear
    !
    ! ==================================================================================
    ! Value round-trips, per kind
    ! ==================================================================================
    !
    subroutine test_roundtrip_integer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v32
        integer(int64) :: v64
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_all([10_int32, 20_int32, 30_int32])
        call c%get_at(2_int64, v32)
        call check(error, v32 == 20_int32, "get_at should return the int32 value written by set_all")
        if (allocated(error)) return
        call c%set_at(3_int64, -7_int32)
        call c%get_at(3_int64, v32)
        call check(error, v32 == -7_int32, "set_at should overwrite a single int32 element")
        if (allocated(error)) return
        !
        call c%init(PK_INT64, 3_int64)
        call c%set_all([10_int64, 20_int64, 30_int64])
        call c%get_at(1_int64, v64)
        call check(error, v64 == 10_int64, "get_at should return the int64 value written by set_all")
        if (allocated(error)) return
        call c%set_at(1_int64, huge(1_int64))
        call c%get_at(1_int64, v64)
        call check(error, v64 == huge(1_int64), "set_at should store a full-range int64 value")
    end subroutine test_roundtrip_integer
    !
    subroutine test_roundtrip_real(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        real(real32) :: r32
        real(real64) :: r64
        !
        call c%init(PK_FLOAT32, 3_int64)
        call c%set_all([1.5_real32, 2.5_real32, 3.5_real32])
        call c%get_at(3_int64, r32)
        call check(error, r32 == 3.5_real32, "get_at should return the float32 value written by set_all")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64, 3_int64)
        call c%set_all([1.5_real64, 2.5_real64, 3.5_real64])
        call c%get_at(2_int64, r64)
        call check(error, r64 == 2.5_real64, "get_at should return the float64 value written by set_all")
        if (allocated(error)) return
        call c%set_at(2_int64, -0.25_real64)
        call c%get_at(2_int64, r64)
        call check(error, r64 == -0.25_real64, "set_at should overwrite a single float64 element")
    end subroutine test_roundtrip_real
    !
    subroutine test_roundtrip_logical(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        logical :: b
        !
        call c%init(PK_LOGICAL, 3_int64)
        call c%set_all([.true., .false., .true.])
        call c%get_at(2_int64, b)
        call check(error, .not. b, "get_at should return .false. for the element set_all wrote as .false.")
        if (allocated(error)) return
        call c%set_at(2_int64, .true.)
        call c%get_at(2_int64, b)
        call check(error, b, "set_at should overwrite a single logical element")
    end subroutine test_roundtrip_logical
    !
    !> String round-trip with the FIRST element deliberately the shortest (CLAUDE.md's
    !> "sized/typed from the first element" fixture rule) -- a fixture whose first element is
    !> longest would pass even if per-element length were derived from element 1.
    subroutine test_roundtrip_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: sp
        !
        call c%init(PK_STRING, 4_int64)
        call c%set_all(["a    ", "bcdef", "gh   ", "ijk  "])
        call c%get_at(1_int64, s)
        call check(error, s == "a    ", "get_at should return the first (shortest) string verbatim")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "bcdef", "a later, LONGER element must not be truncated to the first one's length")
        if (allocated(error)) return
        call c%set_at(3_int64, "replacement")
        call c%get_at(3_int64, s)
        call check(error, s == "replacement", "set_at should replace a string element with a longer value")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%size() == 4_int64, "string_column should alias a store holding one element per row")
    end subroutine test_roundtrip_string
    !
    subroutine test_roundtrip_temporal(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(3), d1
        type(parquet_time) :: t(2), t1
        type(parquet_timestamp) :: ts(2), ts1
        integer :: i, yy, mm, dd, hh, mi, ss
        !
        do i = 1, 3
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 3_int64)
        call c%set_all(d)
        call c%get_at(3_int64, d1)
        call check(error, d1%year() == 2026 .and. d1%month() == 7 .and. d1%day() == 23, &
            "get_at should return the date written by set_all")
        if (allocated(error)) return
        !
        call t(1)%set(1, 2, 3)
        call t(2)%set(23, 59, 58)
        call c%init(PK_TIME, 2_int64)
        call c%set_all(t)
        call c%get_at(2_int64, t1)
        call check(error, t1%hour() == 23 .and. t1%minute() == 59 .and. t1%second() == 58, &
            "get_at should return the time written by set_all")
        if (allocated(error)) return
        !
        call ts(1)%set(2026, 1, 2, 3, 4, 5)
        call ts(2)%set(2026, 12, 31, 23, 59, 59)
        call c%init(PK_TIMESTAMP, 2_int64)
        call c%set_all(ts)
        call c%get_at(1_int64, ts1)
        call ts1%get(yy, mm, dd, hh, mi, ss)
        call check(error, yy == 2026 .and. dd == 2 .and. hh == 3, &
            "get_at should return the timestamp written by set_all")
    end subroutine test_roundtrip_temporal
    !
    subroutine test_roundtrip_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        real(real64) :: mat(3, 4), row(3)
        integer(int32) :: imat(2, 3), irow(2)
        integer :: i
        !
        mat = reshape([(real(i, real64), i = 1, 12)], [3, 4])
        call c%init(PK_FLOAT64_VEC, 4_int64, width=3_int32)
        call c%set_all(mat)
        call c%get_at(3_int64, row)
        call check(error, all(row == [7.0_real64, 8.0_real64, 9.0_real64]), &
            "get_at on a vector column should return that ROW's whole vector, (element, row) order")
        if (allocated(error)) return
        call c%set_at(1_int64, [-1.0_real64, -2.0_real64, -3.0_real64])
        call c%get_at(1_int64, row)
        call check(error, all(row == [-1.0_real64, -2.0_real64, -3.0_real64]), &
            "set_at on a vector column should replace that row's whole vector")
        if (allocated(error)) return
        !
        imat = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        call c%init(PK_INT32_VEC, 3_int64, width=2_int32)
        call c%set_all(imat)
        call c%get_at(2_int64, irow)
        call check(error, all(irow == [3_int32, 4_int32]), "int32 vector rows should round-trip")
    end subroutine test_roundtrip_vector
    !
    !> PK_STRING_VEC keeps ONE string store for the whole column, flat-indexed (i-1)*width + e
    !> (RF6/s1), so a row's vector is contiguous. This checks both the row view and the flat
    !> layout underneath it.
    subroutine test_roundtrip_string_vector(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=4) :: row(3)
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: sp
        !
        call c%init(PK_STRING_VEC, 2_int64, width=3_int32)
        call c%set_all(reshape(["p ", "qq", "r ", "s ", "tt", "u "], [3, 2]))
        call c%get_at(2_int64, row)
        call check(error, trim(row(1)) == "s" .and. trim(row(2)) == "tt" .and. trim(row(3)) == "u", &
            "get_at on a string vector column should return row 2's three elements in order")
        if (allocated(error)) return
        call check(error, c%length() == 2_int64, "a string vector column's length() counts rows, not strings")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%size() == 6_int64, "the flat string store should hold nrows*width elements")
        if (allocated(error)) return
        call sp%get(4_int64, s)
        call check(error, trim(s) == "s", "row 2 element 1 should sit at flat index (2-1)*3 + 1 = 4")
    end subroutine test_roundtrip_string_vector
    !
    subroutine test_data_ptr_aliases(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        real(real64), pointer :: p(:)
        real(real64), pointer :: pm(:,:)
        real(real64) :: v
        !
        call c%init(PK_FLOAT64, 3_int64)
        call c%set_all([1.0_real64, 2.0_real64, 3.0_real64])
        call c%data_ptr(p)
        call check(error, size(p) == 3, "data_ptr should expose exactly the column's rows")
        if (allocated(error)) return
        call check(error, sum(p) == 6.0_real64, "data_ptr should alias the values already stored")
        if (allocated(error)) return
        p(2) = 20.0_real64
        call c%get_at(2_int64, v)
        call check(error, v == 20.0_real64, "a write through the data_ptr alias should show in the column")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%data_ptr(pm)
        call check(error, size(pm, 1) == 2 .and. size(pm, 2) == 2, &
            "a vector column's data_ptr should be shaped (width, nrows)")
    end subroutine test_data_ptr_aliases
    !
    ! ==================================================================================
    ! Validity -- one test per dispatch class, plus the sparse-allocation contract
    ! ==================================================================================
    !
    !> The central memory requirement of R2: a column that holds no nulls must not allocate a
    !> validity array at all. This is the easiest property to regress silently, since everything
    !> still *works* if a bitmap is allocated eagerly.
    subroutine test_sparse_validity_unallocated(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: big(1000)
        !
        call c%init(PK_INT32, 1000_int64)
        call check(error, c%validity_bytes() == 0_int64, &
            "a freshly initialized column must not allocate a validity bitmap")
        if (allocated(error)) return
        big = 1_int32
        call c%set_all(big)
        call check(error, c%validity_bytes() == 0_int64, &
            "writing values must not allocate a validity bitmap")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "a column with no nulls should report any_null() = .false.")
    end subroutine test_sparse_validity_unallocated
    !
    subroutine test_set_null_allocates_bitmap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 100_int64)
        call c%set_null(50_int64)
        call check(error, c%validity_bytes() > 0_int64, "the first set_null should allocate the bitmap")
        if (allocated(error)) return
        call check(error, c%any_null(), "any_null() should be .true. after set_null")
        if (allocated(error)) return
        call check(error, c%is_null(50_int64), "the row passed to set_null should read back as null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(49_int64), "set_null must not affect its neighbours")
        if (allocated(error)) return
        call check(error, .not. c%is_null(51_int64), "set_null must not affect its neighbours")
    end subroutine test_set_null_allocates_bitmap
    !
    !> RF9: writing every value clears every null bit, so a default set_all can release the
    !> bitmap outright instead of scanning for survivors.
    subroutine test_set_all_drops_bitmap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 4_int64)
        call c%set_null(2_int64)
        call check(error, c%validity_bytes() > 0_int64, "precondition: the bitmap exists after set_null")
        if (allocated(error)) return
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32])
        call check(error, c%validity_bytes() == 0_int64, &
            "a whole-column set_all should release the bitmap")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "a whole-column set_all should leave no nulls behind")
    end subroutine test_set_all_drops_bitmap
    !
    subroutine test_modify_nulls_false(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_all([1_int32, 2_int32, 3_int32])
        call c%set_null(2_int64)
        call c%set_all([10_int32, 20_int32, 30_int32], modify_nulls=.false.)
        call check(error, c%is_null(2_int64), "set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, &
            "set_all(modify_nulls=.false.) must not release the bitmap")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 10_int32, "set_all(modify_nulls=.false.) should still write non-null rows")
        if (allocated(error)) return
        call c%set_at(2_int64, 99_int32, modify_nulls=.false.)
        call check(error, c%is_null(2_int64), "set_at(modify_nulls=.false.) must leave a null row null")
    end subroutine test_modify_nulls_false
    !
    subroutine test_set_at_clears_one_bit(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_null(1_int64)
        call c%set_null(3_int64)
        call c%set_at(1_int64, 42_int32)
        call check(error, .not. c%is_null(1_int64), "set_at should clear the null bit of the row it writes")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 42_int32, "set_at should store the value it was given")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "set_at must not clear any OTHER row's null bit")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, &
            "a single-cell set_at must not release the bitmap (R2 iii: no scan on single-cell edits)")
    end subroutine test_set_at_clears_one_bit
    !
    subroutine test_compact_validity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_null(1_int64)
        call c%set_null(2_int64)
        call c%set_at(1_int64, 1_int32)
        call c%compact_validity()
        call check(error, c%validity_bytes() > 0_int64, &
            "compact_validity must keep the bitmap while a null remains")
        if (allocated(error)) return
        call c%set_at(2_int64, 2_int32)
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, &
            "compact_validity should release the bitmap once the last null is gone")
        if (allocated(error)) return
        call check(error, .not. c%any_null(), "the column should report no nulls after compaction")
        if (allocated(error)) return
        ! the bitmap must GROW when rows are appended to an already-null-bearing column
        call c%init(PK_INT32, 4_int64)
        call c%set_null(1_int64)
        call c%append_nulls(500_int64)
        call check(error, c%is_null(504_int64), "an appended row past the old bitmap size should be null")
        if (allocated(error)) return
        call check(error, c%is_null(1_int64), "growing the bitmap must preserve the existing null bits")
        if (allocated(error)) return
        ! compact_validity is defined for every kind, including those with no column bitmap
        call c%init(PK_DATE, 2_int64)
        call c%compact_validity()
        call check(error, c%any_null(), "compact_validity on a temporal column should refresh, not clear, its nulls")
        if (allocated(error)) return
        call c%init(PK_STRING, 2_int64)
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, "compact_validity on a string column is a no-op")
    end subroutine test_compact_validity
    !
    !> A vector column needs one bit per ELEMENT (nrows*width), not per row -- sizing the bitmap
    !> by nrows would silently corrupt validity for every row past the first few.
    subroutine test_vector_validity_bits(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int64) :: need_blocks
        !
        call c%init(PK_FLOAT64_VEC, 100_int64, width=8_int32)
        call c%set_null(100_int64)
        need_blocks = (100_int64*8_int64 + 63_int64)/64_int64
        call check(error, c%validity_bytes() >= need_blocks*8_int64, &
            "a vector column's bitmap must cover nrows*width bits, not nrows bits")
        if (allocated(error)) return
        call check(error, c%is_null(100_int64), "the last row of a wide vector column should read back null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(99_int64), "its neighbour must stay valid")
    end subroutine test_vector_validity_bits
    !
    subroutine test_string_validity_delegates(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_string_column), pointer :: sp
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["a  ", "bcd", "ef "])
        call check(error, .not. c%any_null(), "a string column with values written should hold no nulls")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call check(error, c%any_null(), "any_null() should see a null set in the embedded string store")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64), "is_null should report the string element's own null state")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, &
            "a string column must NOT allocate a column-level bitmap -- validity is delegated (DD1)")
        if (allocated(error)) return
        call c%string_column(sp)
        call check(error, sp%null_count() == 1_int64, "the embedded string column should own the null")
    end subroutine test_string_validity_delegates
    !
    !> Temporal kinds carry null state inside each element, and `any_null` caches the answer
    !> (RF8) -- so the interesting case is that the cache is invalidated by a mutation, in both
    !> directions.
    subroutine test_temporal_validity_cache(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(3), one
        integer :: i
        !
        do i = 1, 3
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 3_int64)
        call check(error, c%any_null(), "a freshly initialized date column holds null elements")
        if (allocated(error)) return
        call c%set_all(d)
        call check(error, .not. c%any_null(), &
            "the cached null flag must be refreshed after set_all writes real values")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call check(error, c%any_null(), "the cached null flag must be refreshed after set_null")
        if (allocated(error)) return
        call check(error, c%is_null(2_int64), "is_null should read the element's own null state")
        if (allocated(error)) return
        call check(error, c%validity_bytes() == 0_int64, &
            "a temporal column must NOT allocate a column-level bitmap -- the element carries it")
        if (allocated(error)) return
        call one%set(2000, 1, 1)
        call c%set_at(2_int64, one)
        call check(error, .not. c%any_null(), &
            "overwriting the only null element should clear the cached null flag")
    end subroutine test_temporal_validity_cache
    !
    ! ==================================================================================
    ! Structural mutation
    ! ==================================================================================
    !
    subroutine test_append_values(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        real(real64) :: row(2)
        type(parquet_date) :: d(2), d1
        integer :: i
        !
        call c%init(PK_INT32, 2_int64)
        call c%set_all([1_int32, 2_int32])
        call c%append_values([3_int32, 4_int32])
        call check(error, c%length() == 4_int64, "append_values should grow the column by the value count")
        if (allocated(error)) return
        call c%get_at(4_int64, v)
        call check(error, v == 4_int32, "append_values should store the appended values")
        if (allocated(error)) return
        call c%get_at(1_int64, v)
        call check(error, v == 1_int32, "append_values must preserve the existing values")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 1_int64)
        call c%set_all(["a"])
        call c%append_values(["bcd", "ef "])
        call check(error, c%length() == 3_int64, "append_values should grow a string column")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "bcd", "append_values should store appended strings")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 1_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64], [2, 1]))
        call c%append_values(reshape([3.0_real64, 4.0_real64], [2, 1]))
        call check(error, c%length() == 2_int64, "append_values should grow a vector column by rows")
        if (allocated(error)) return
        call c%get_at(2_int64, row)
        call check(error, all(row == [3.0_real64, 4.0_real64]), "the appended vector row should round-trip")
        if (allocated(error)) return
        !
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
        end do
        call c%init(PK_DATE, 0_int64)
        call c%append_values(d)
        call check(error, c%length() == 2_int64, "append_values should grow a temporal column from empty")
        if (allocated(error)) return
        call c%get_at(2_int64, d1)
        call check(error, d1%day() == 22, "the appended date should round-trip")
    end subroutine test_append_values
    !
    subroutine test_append_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: a, b
        integer(int32) :: v
        character(len=:), allocatable :: s
        !
        call a%init(PK_INT32, 2_int64)
        call a%set_all([1_int32, 2_int32])
        call b%init(PK_INT32, 2_int64)
        call b%set_all([3_int32, 4_int32])
        call b%set_null(1_int64)
        call a%append(b)
        call check(error, a%length() == 4_int64, "append should extend the destination by the source's rows")
        if (allocated(error)) return
        call a%get_at(4_int64, v)
        call check(error, v == 4_int32, "append should copy the source's values")
        if (allocated(error)) return
        call check(error, a%is_null(3_int64), "append should carry the source's nulls across")
        if (allocated(error)) return
        call check(error, .not. a%is_null(1_int64), "append must not disturb the destination's own rows")
        if (allocated(error)) return
        call check(error, b%length() == 2_int64, "append must leave the source unchanged")
        if (allocated(error)) return
        !
        call a%init(PK_STRING, 1_int64)
        call a%set_all(["a"])
        call b%init(PK_STRING, 2_int64)
        call b%set_all(["bcd", "ef "])
        call a%append(b)
        call check(error, a%length() == 3_int64, "append should extend a string column")
        if (allocated(error)) return
        call a%get_at(3_int64, s)
        call check(error, s == "ef ", "append should copy the source's strings")
    end subroutine test_append_column
    !
    subroutine test_append_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_FLOAT64, 2_int64)
        call c%set_all([1.0_real64, 2.0_real64])
        call check(error, c%validity_bytes() == 0_int64, "precondition: no bitmap before any null exists")
        if (allocated(error)) return
        call c%append_nulls(3_int64)
        call check(error, c%length() == 5_int64, "append_nulls should add the requested number of rows")
        if (allocated(error)) return
        call check(error, c%is_null(5_int64), "the appended rows should be null")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "the existing rows must stay non-null")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, "append_nulls should materialize the bitmap")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 1_int64)
        call c%set_all(["a"])
        call c%append_nulls(2_int64)
        call check(error, c%length() == 3_int64, "append_nulls should grow a string column")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "the appended string rows should be null")
    end subroutine test_append_nulls
    !
    subroutine test_reindex(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        real(real64) :: row(2)
        !
        call c%init(PK_INT32, 4_int64)
        call c%set_all([10_int32, 20_int32, 30_int32, 40_int32])
        call c%set_null(2_int64)
        call c%reindex([4_int64, 3_int64, 2_int64, 1_int64])
        call c%get_at(1_int64, v)
        call check(error, v == 40_int32, "reindex should move the row named by perm(1) to position 1")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "reindex should move a null along with its row")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "reindex must not leave stale null bits behind")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 3_int64)
        call c%set_all(["a  ", "bcd", "ef "])
        call c%reindex([3_int64, 1_int64, 2_int64])
        call c%get_at(1_int64, s)
        call check(error, s == "ef ", "reindex should permute a string column's payload")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "a  ", "reindex should keep every string element intact")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%reindex([2_int64, 1_int64])
        call c%get_at(1_int64, row)
        call check(error, all(row == [3.0_real64, 4.0_real64]), "reindex should permute whole vector rows")
    end subroutine test_reindex
    !
    subroutine test_delete_by_mask(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        integer(int32) :: v
        character(len=:), allocatable :: s
        !
        call c%init(PK_INT32, 5_int64)
        call c%set_all([1_int32, 2_int32, 3_int32, 4_int32, 5_int32])
        call c%set_null(4_int64)
        call c%delete_by_mask([.true., .false., .true., .true., .false.])
        call check(error, c%length() == 3_int64, "delete_by_mask should keep exactly the .true. rows")
        if (allocated(error)) return
        call c%get_at(2_int64, v)
        call check(error, v == 3_int32, "delete_by_mask should preserve the surviving rows' order")
        if (allocated(error)) return
        call check(error, c%is_null(3_int64), "a surviving null row should still be null after compaction")
        if (allocated(error)) return
        call check(error, .not. c%is_null(1_int64), "a surviving non-null row must stay non-null")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 4_int64)
        call c%set_all(["a    ", "bcdef", "gh   ", "ijk  "])
        call c%delete_by_mask([.false., .true., .false., .true.])
        call check(error, c%length() == 2_int64, "delete_by_mask should shrink a string column")
        if (allocated(error)) return
        call c%get_at(1_int64, s)
        call check(error, s == "bcdef", "the surviving string payload must be intact, not truncated")
        if (allocated(error)) return
        call c%get_at(2_int64, s)
        call check(error, s == "ijk  ", "the second surviving string should follow in order")
    end subroutine test_delete_by_mask
    !
    subroutine test_deep_copy(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: a, b
        integer(int32) :: v
        character(len=:), allocatable :: s, u
        !
        call a%init(PK_INT32, 3_int64, unit="count")
        call a%set_all([1_int32, 2_int32, 3_int32])
        call a%set_null(2_int64)
        call a%deep_copy(b)
        call check(error, b%length() == 3_int64, "deep_copy should reproduce the row count")
        if (allocated(error)) return
        call check(error, b%kindof() == PK_INT32, "deep_copy should reproduce the kind")
        if (allocated(error)) return
        call check(error, b%is_null(2_int64), "deep_copy should reproduce the validity state")
        if (allocated(error)) return
        call b%unit_string(u)
        call check(error, u == "count", "deep_copy should reproduce the unit string")
        if (allocated(error)) return
        call b%set_at(1_int64, 99_int32)
        call a%get_at(1_int64, v)
        call check(error, v == 1_int32, "mutating the copy must not change the source")
        if (allocated(error)) return
        call a%set_at(3_int64, 77_int32)
        call b%get_at(3_int64, v)
        call check(error, v == 3_int32, "mutating the source must not change the copy")
        if (allocated(error)) return
        !
        call a%init(PK_STRING, 2_int64)
        call a%set_all(["a  ", "bcd"])
        call a%deep_copy(b)
        call b%set_at(1_int64, "changed")
        call a%get_at(1_int64, s)
        call check(error, s == "a  ", "a string column's deep copy must own its payload independently")
    end subroutine test_deep_copy
    !
    subroutine test_edge_cases(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: a, b
        integer(int32) :: v
        logical :: none_keep(3)
        !
        call a%init(PK_INT32, 0_int64)
        call check(error, a%length() == 0_int64, "a zero-row column should be legal")
        if (allocated(error)) return
        call check(error, .not. a%any_null(), "a zero-row column holds no nulls")
        if (allocated(error)) return
        call a%deep_copy(b)
        call check(error, b%length() == 0_int64, "deep_copy of an empty column should give an empty column")
        if (allocated(error)) return
        !
        call a%init(PK_INT32, 3_int64)
        call a%set_all([1_int32, 2_int32, 3_int32])
        call a%reindex([1_int64, 2_int64, 3_int64])
        call a%get_at(2_int64, v)
        call check(error, v == 2_int32, "the identity permutation should leave the column unchanged")
        if (allocated(error)) return
        call a%delete_by_mask([.true., .true., .true.])
        call check(error, a%length() == 3_int64, "a keep-everything mask should change nothing")
        if (allocated(error)) return
        none_keep = .false.
        call a%delete_by_mask(none_keep)
        call check(error, a%length() == 0_int64, "a keep-nothing mask should empty the column")
        if (allocated(error)) return
        call a%append_nulls(0_int64)
        call check(error, a%length() == 0_int64, "append_nulls(0) should be a no-op")
        if (allocated(error)) return
        call a%append_values([5_int32])
        call check(error, a%length() == 1_int64, "an emptied column should still accept appends")
    end subroutine test_edge_cases
    !
    subroutine test_unit_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: u
        !
        call c%init(PK_FLOAT64, 1_int64)
        call c%unit_string(u)
        call check(error, u == "", "a column with no unit should report an empty unit string")
        if (allocated(error)) return
        call c%set_unit("km/h")
        call c%unit_string(u)
        call check(error, u == "km/h", "set_unit should store the unit string")
        if (allocated(error)) return
        call c%set_unit("")
        call c%unit_string(u)
        call check(error, u == "", "set_unit with an empty string should clear the unit")
    end subroutine test_unit_string
    !
    subroutine test_kind_reporting(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=:), allocatable :: nm
        !
        call parquet_kind_name(PK_NONE, nm)
        call check(error, nm == "PK_NONE", "parquet_kind_name should name PK_NONE")
        if (allocated(error)) return
        call c%init(PK_TIMESTAMP_VEC, 1_int64, width=2_int32)
        call parquet_kind_name(c%kindof(), nm)
        call check(error, nm == "PK_TIMESTAMP_VEC", "parquet_kind_name should name the column's active kind")
        if (allocated(error)) return
        call parquet_kind_name(PK_LIST, nm)
        call check(error, nm == "PK_LIST", "the reserved container kinds should still have names")
    end subroutine test_kind_reporting
    !
    ! ==================================================================================
    ! Kind matrix -- every kind must support every primitive, so the remaining kinds are
    ! swept here rather than relying on the representative kinds used above. A gap in this
    ! matrix is exactly what a generator template bug looks like.
    ! ==================================================================================
    !
    subroutine test_matrix_scalar_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: pr32(:)
        real(real64), pointer :: pr64(:)
        logical, pointer :: pb(:)
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        type(parquet_timestamp), pointer :: pts(:)
        type(parquet_date) :: d(2)
        type(parquet_time) :: t(2)
        type(parquet_timestamp) :: ts(2)
        integer :: i
        !
        call c%init(PK_INT32, 2_int64)
        call c%set_all([1_int32, 2_int32])
        call c%data_ptr(p32)
        call check(error, sum(p32) == 3_int32, "int32 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3_int32])
        call check(error, c%length() == 3_int64, "int32 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT32")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_INT64, 2_int64)
        call c%set_all([1_int64, 2_int64])
        call c%data_ptr(p64)
        call check(error, sum(p64) == 3_int64, "int64 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3_int64])
        call check(error, c%length() == 3_int64, "int64 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT64")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32, 2_int64)
        call c%set_all([1.0_real32, 2.0_real32])
        call c%data_ptr(pr32)
        call check(error, sum(pr32) == 3.0_real32, "float32 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3.0_real32])
        call check(error, c%length() == 3_int64, "float32 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT32")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64, 2_int64)
        call c%set_all([1.0_real64, 2.0_real64])
        call c%data_ptr(pr64)
        call check(error, sum(pr64) == 3.0_real64, "float64 data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([3.0_real64])
        call check(error, c%length() == 3_int64, "float64 append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT64")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL, 2_int64)
        call c%set_all([.true., .false.])
        call c%data_ptr(pb)
        call check(error, count(pb) == 1, "logical data_ptr should alias the stored values")
        if (allocated(error)) return
        call c%append_values([.true.])
        call check(error, c%length() == 3_int64, "logical append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_LOGICAL")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
            call t(i)%set(i, 30, 0)
            call ts(i)%set(2026, 7, 20 + i, 12, 0, 0)
        end do
        call c%init(PK_DATE, 2_int64)
        call c%set_all(d)
        call c%data_ptr(pd)
        call check(error, pd(2)%day() == 22, "date data_ptr should alias the stored elements")
        if (allocated(error)) return
        call c%append_values(d)
        call check(error, c%length() == 4_int64, "date append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_DATE")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIME, 2_int64)
        call c%set_all(t)
        call c%data_ptr(pt)
        call check(error, pt(2)%hour() == 2, "time data_ptr should alias the stored elements")
        if (allocated(error)) return
        call c%append_values(t)
        call check(error, c%length() == 4_int64, "time append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIME")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP, 2_int64)
        call c%set_all(ts)
        call c%data_ptr(pts)
        call check(error, .not. pts(1)%is_null(), "timestamp data_ptr should alias the stored elements")
        if (allocated(error)) return
        call c%append_values(ts)
        call check(error, c%length() == 4_int64, "timestamp append_values should grow the column")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIMESTAMP")
        if (allocated(error)) return
    end subroutine test_matrix_scalar_kinds
    !
    subroutine test_matrix_vector_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column), target :: c
        integer(int32), pointer :: p32(:,:)
        integer(int64), pointer :: p64(:,:)
        real(real32), pointer :: pr32(:,:)
        real(real64), pointer :: pr64(:,:)
        logical, pointer :: pb(:,:)
        type(parquet_date), pointer :: pd(:,:)
        type(parquet_time), pointer :: pt(:,:)
        type(parquet_timestamp), pointer :: pts(:,:)
        integer(int32) :: r32(2)
        integer(int64) :: r64(2)
        real(real32) :: rr32(2)
        real(real64) :: rr64(2)
        logical :: rb(2)
        type(parquet_date) :: dm(2, 2), rd(2)
        type(parquet_time) :: tm(2, 2), rt(2)
        type(parquet_timestamp) :: tsm(2, 2), rts(2)
        integer :: i, j
        !
        call c%init(PK_INT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]))
        call c%get_at(2_int64, r32)
        call check(error, all(r32 == [3_int32, 4_int32]), "int32 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int32, 8_int32])
        call c%data_ptr(p32)
        call check(error, p32(1, 1) == 9_int32, "int32 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5_int32, 6_int32], [2, 1]))
        call check(error, c%length() == 3_int64, "int32 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT32_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_INT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2]))
        call c%get_at(2_int64, r64)
        call check(error, all(r64 == [3_int64, 4_int64]), "int64 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int64, 8_int64])
        call c%data_ptr(p64)
        call check(error, p64(1, 1) == 9_int64, "int64 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5_int64, 6_int64], [2, 1]))
        call check(error, c%length() == 3_int64, "int64 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_INT64_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2, 2]))
        call c%get_at(2_int64, rr32)
        call check(error, all(rr32 == [3.0_real32, 4.0_real32]), "float32 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real32, 8.0_real32])
        call c%data_ptr(pr32)
        call check(error, pr32(1, 1) == 9.0_real32, "float32 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5.0_real32, 6.0_real32], [2, 1]))
        call check(error, c%length() == 3_int64, "float32 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT32_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%get_at(2_int64, rr64)
        call check(error, all(rr64 == [3.0_real64, 4.0_real64]), "float64 vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real64, 8.0_real64])
        call c%data_ptr(pr64)
        call check(error, pr64(1, 1) == 9.0_real64, "float64 vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([5.0_real64, 6.0_real64], [2, 1]))
        call check(error, c%length() == 3_int64, "float64 vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_FLOAT64_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([.true., .false., .true., .true.], [2, 2]))
        call c%get_at(2_int64, rb)
        call check(error, all(rb), "logical vector get_at should return that row")
        if (allocated(error)) return
        call c%set_at(1_int64, [.false., .false.])
        call c%data_ptr(pb)
        call check(error, .not. pb(1, 1), "logical vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(reshape([.true., .false.], [2, 1]))
        call check(error, c%length() == 3_int64, "logical vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_LOGICAL_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        do j = 1, 2
            do i = 1, 2
                call dm(i, j)%set(2026, 7, 10*j + i)
                call tm(i, j)%set(j, i, 0)
                call tsm(i, j)%set(2026, 7, 10*j + i, 6, 0, 0)
            end do
        end do
        call c%init(PK_DATE_VEC, 2_int64, width=2_int32)
        call c%set_all(dm)
        call c%get_at(2_int64, rd)
        call check(error, rd(1)%day() == 21, "date vector get_at should return that row's elements")
        if (allocated(error)) return
        call c%set_at(1_int64, rd)
        call c%data_ptr(pd)
        call check(error, pd(1, 1)%day() == 21, "date vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(dm(:, 1:1))
        call check(error, c%length() == 3_int64, "date vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_DATE_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIME_VEC, 2_int64, width=2_int32)
        call c%set_all(tm)
        call c%get_at(2_int64, rt)
        call check(error, rt(1)%hour() == 2, "time vector get_at should return that row's elements")
        if (allocated(error)) return
        call c%set_at(1_int64, rt)
        call c%data_ptr(pt)
        call check(error, pt(1, 1)%hour() == 2, "time vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(tm(:, 1:1))
        call check(error, c%length() == 3_int64, "time vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIME_VEC")
        if (allocated(error)) return
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP_VEC, 2_int64, width=2_int32)
        call c%set_all(tsm)
        call c%get_at(2_int64, rts)
        call check(error, .not. rts(1)%is_null(), "timestamp vector get_at should return that row's elements")
        if (allocated(error)) return
        call c%set_at(1_int64, rts)
        call c%data_ptr(pts)
        call check(error, .not. pts(1, 1)%is_null(), "timestamp vector set_at + data_ptr should agree")
        if (allocated(error)) return
        call c%append_values(tsm(:, 1:1))
        call check(error, c%length() == 3_int64, "timestamp vector append_values should add rows")
        if (allocated(error)) return
        call exercise_structural(c, error, "PK_TIMESTAMP_VEC")
        if (allocated(error)) return
    end subroutine test_matrix_vector_kinds
    !
    !> Kind-agnostic structural sweep: reindex, deep_copy, delete_by_mask and append_nulls all
    !> dispatch internally on the column's kind, so calling this once per kind is what actually
    !> covers every arm of that dispatch. Expects a column with at least two rows.
    subroutine exercise_structural(c, error, label)
        class(parquet_column), intent(inout) :: c
        type(error_type), allocatable, intent(inout) :: error
        character(len=*), intent(in) :: label
        type(parquet_column) :: copy
        integer(int64) :: n, i
        integer(int64), allocatable :: perm(:)
        logical, allocatable :: keep(:)
        !
        n = c%length()
        allocate(perm(n), keep(n))
        do i = 1_int64, n
            perm(i) = n - i + 1_int64
            keep(i) = (mod(i, 2_int64) == 1_int64)
        end do
        call c%reindex(perm)
        call check(error, c%length() == n, label//": reindex must preserve the row count")
        if (allocated(error)) return
        call c%deep_copy(copy)
        call check(error, copy%length() == n .and. copy%kindof() == c%kindof(), &
            label//": deep_copy must reproduce the row count and kind")
        if (allocated(error)) return
        call c%append(copy)
        call check(error, c%length() == 2_int64*n, label//": append must add the source's rows")
        if (allocated(error)) return
        call c%delete_by_mask([(i <= n, i = 1_int64, 2_int64*n)])
        call check(error, c%length() == n, label//": the appended half should be droppable again")
        if (allocated(error)) return
        call c%delete_by_mask(keep)
        call check(error, c%length() == count(keep, kind=int64), &
            label//": delete_by_mask must keep exactly the .true. rows")
        if (allocated(error)) return
        call c%append_nulls(1_int64)
        call check(error, c%is_null(c%length()), label//": append_nulls must append a null row")
    end subroutine exercise_structural
    !
    !> set_null/is_null/any_null dispatch on the kind three different ways (column bitmap,
    !> embedded string column, inside the element), so every kind gets its own pass here --
    !> this is the sweep that would catch "fixed the bitmap, forgot the temporal cache".
    subroutine test_matrix_set_null_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call null_sweep(error, PK_INT32, 1_int32, "PK_INT32")
        if (allocated(error)) return
        call null_sweep(error, PK_INT64, 1_int32, "PK_INT64")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT32, 1_int32, "PK_FLOAT32")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT64, 1_int32, "PK_FLOAT64")
        if (allocated(error)) return
        call null_sweep(error, PK_LOGICAL, 1_int32, "PK_LOGICAL")
        if (allocated(error)) return
        call null_sweep(error, PK_STRING, 1_int32, "PK_STRING")
        if (allocated(error)) return
        call null_sweep(error, PK_DATE, 1_int32, "PK_DATE")
        if (allocated(error)) return
        call null_sweep(error, PK_TIME, 1_int32, "PK_TIME")
        if (allocated(error)) return
        call null_sweep(error, PK_TIMESTAMP, 1_int32, "PK_TIMESTAMP")
        if (allocated(error)) return
        call null_sweep(error, PK_INT32_VEC, 2_int32, "PK_INT32_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_INT64_VEC, 2_int32, "PK_INT64_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT32_VEC, 2_int32, "PK_FLOAT32_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_FLOAT64_VEC, 2_int32, "PK_FLOAT64_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_LOGICAL_VEC, 2_int32, "PK_LOGICAL_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_STRING_VEC, 2_int32, "PK_STRING_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_DATE_VEC, 2_int32, "PK_DATE_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_TIME_VEC, 2_int32, "PK_TIME_VEC")
        if (allocated(error)) return
        call null_sweep(error, PK_TIMESTAMP_VEC, 2_int32, "PK_TIMESTAMP_VEC")
    end subroutine test_matrix_set_null_all_kinds
    !
    !> One kind's null round-trip: mark row 2 null, and check the column agrees -- while row 1,
    !> which was never touched, does not.
    subroutine null_sweep(error, kind, width, label)
        type(error_type), allocatable, intent(inout) :: error
        integer, intent(in) :: kind
        integer(int32), intent(in) :: width
        character(len=*), intent(in) :: label
        type(parquet_column) :: c
        !
        call c%init(kind, 3_int64, width=width)
        call c%set_null(2_int64)
        call check(error, c%is_null(2_int64), label//": the row passed to set_null should read back null")
        if (allocated(error)) return
        call check(error, c%any_null(), label//": any_null should be .true. after set_null")
        if (allocated(error)) return
        call check(error, c%length() == 3_int64, label//": set_null must not change the row count")
    end subroutine null_sweep
    !
    subroutine test_string_vector_set_at(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        character(len=6) :: row(2)
        !
        call c%init(PK_STRING_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape(["a  ", "bcd", "ef ", "gh "], [2, 2]))
        call c%set_at(1_int64, ["xy   ", "zzzzz"])
        call c%get_at(1_int64, row)
        call check(error, trim(row(1)) == "xy" .and. trim(row(2)) == "zzzzz", &
            "set_at on a string vector row should replace both of its elements")
        if (allocated(error)) return
        call c%set_null(2_int64)
        call c%set_at(2_int64, ["p", "q"], modify_nulls=.false.)
        call check(error, c%is_null(2_int64), &
            "set_at(modify_nulls=.false.) must leave a null string vector row null")
        if (allocated(error)) return
        call c%set_all(reshape(["m", "n", "o", "p"], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(2_int64), &
            "set_all(modify_nulls=.false.) must leave a null string vector row null")
        if (allocated(error)) return
        call c%append_values(reshape(["r ", "st"], [2, 1]))
        call check(error, c%length() == 3_int64, "append_values should add a string vector row")
        if (allocated(error)) return
        call c%get_at(3_int64, row)
        call check(error, trim(row(2)) == "st", "the appended string vector row should round-trip")
    end subroutine test_string_vector_set_at
    !
    subroutine test_clear_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        !
        call c%init(PK_INT32, 3_int64)
        call c%set_null(2_int64)
        call c%clear_null(2_int64)
        call check(error, .not. c%is_null(2_int64), "clear_null should mark a bitmap-backed row valid again")
        if (allocated(error)) return
        call check(error, c%validity_bytes() > 0_int64, &
            "clear_null must not release the bitmap on its own (that is compact_validity's job)")
        if (allocated(error)) return
        call c%compact_validity()
        call check(error, c%validity_bytes() == 0_int64, "compact_validity should then release it")
        if (allocated(error)) return
        !
        call c%init(PK_INT32, 2_int64)
        call c%clear_null(1_int64)
        call check(error, .not. c%is_null(1_int64), "clear_null on a bitmap-free column should be a no-op")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 2_int64)
        call c%set_all(["a  ", "bcd"])
        call c%set_null(1_int64)
        call c%clear_null(1_int64)
        call check(error, .not. c%is_null(1_int64), "clear_null should clear a string element's null state")
        if (allocated(error)) return
        !
        call c%init(PK_STRING_VEC, 2_int64, width=2_int32)
        call c%set_null(1_int64)
        call c%clear_null(1_int64)
        call check(error, .not. c%is_null(1_int64), "clear_null should clear a string vector row's null state")
    end subroutine test_clear_null
    !
    !> Every discriminator must map to a name -- a missing arm would silently produce
    !> "PK_UNKNOWN" inside an error message, exactly when the message matters most.
    subroutine test_kind_names_complete(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: kinds(22), k
        character(len=:), allocatable :: nm
        !
        kinds = [PK_NONE, PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, PK_STRING, &
                 PK_DATE, PK_TIME, PK_TIMESTAMP, PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, &
                 PK_FLOAT64_VEC, PK_LOGICAL_VEC, PK_STRING_VEC, PK_DATE_VEC, PK_TIME_VEC, &
                 PK_TIMESTAMP_VEC, PK_LIST, PK_MAP, PK_STRUCT]
        do k = 1, size(kinds)
            call parquet_kind_name(kinds(k), nm)
            call check(error, len(nm) > 0 .and. nm /= "PK_UNKNOWN", &
                "every PK_* constant should have its own name in parquet_kind_name")
            if (allocated(error)) return
        end do
        call parquet_kind_name(-1, nm)
        call check(error, nm == "PK_UNKNOWN", "an unrecognized discriminator should report PK_UNKNOWN")
    end subroutine test_kind_names_complete
    !
    !> R3/M8's `modify_nulls=` guard, swept over every kind: with `.false.` a null row keeps both
    !> its null state and its stored value, and with the default a write clears the row's null
    !> bit (RF9). Both branches are generated per kind, so this is the sweep that proves the
    !> template is right for all of them rather than for the two spot-checked above.
    subroutine test_matrix_modify_nulls_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_column) :: c
        type(parquet_date) :: d(2)
        type(parquet_time) :: t(2)
        type(parquet_timestamp) :: ts(2)
        integer :: i
        !
        call c%init(PK_INT32, 2_int64)
        call c%set_all([1_int32, 2_int32])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9_int32, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1_int32, 2_int32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9_int32)
        call check(error, .not. c%is_null(1_int64), "PK_INT32: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_INT64, 2_int64)
        call c%set_all([1_int64, 2_int64])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9_int64, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1_int64, 2_int64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9_int64)
        call check(error, .not. c%is_null(1_int64), "PK_INT64: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32, 2_int64)
        call c%set_all([1.0_real32, 2.0_real32])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9.0_real32, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1.0_real32, 2.0_real32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9.0_real32)
        call check(error, .not. c%is_null(1_int64), "PK_FLOAT32: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64, 2_int64)
        call c%set_all([1.0_real64, 2.0_real64])
        call c%set_null(1_int64)
        call c%set_at(1_int64, 9.0_real64, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([1.0_real64, 2.0_real64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, 9.0_real64)
        call check(error, .not. c%is_null(1_int64), "PK_FLOAT64: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL, 2_int64)
        call c%set_all([.true., .false.])
        call c%set_null(1_int64)
        call c%set_at(1_int64, .true., modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all([.true., .false.], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, .true.)
        call check(error, .not. c%is_null(1_int64), "PK_LOGICAL: a default set_at should clear the row's null bit")
        if (allocated(error)) return
        !
        call c%init(PK_INT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9_int32, 9_int32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1_int32, 2_int32, 3_int32, 4_int32], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT32_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int32, 9_int32])
        call check(error, .not. c%is_null(1_int64), &
            "PK_INT32_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_INT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9_int64, 9_int64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1_int64, 2_int64, 3_int64, 4_int64], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_INT64_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9_int64, 9_int64])
        call check(error, .not. c%is_null(1_int64), &
            "PK_INT64_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT32_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9.0_real32, 9.0_real32], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1.0_real32, 2.0_real32, 3.0_real32, 4.0_real32], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT32_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real32, 9.0_real32])
        call check(error, .not. c%is_null(1_int64), &
            "PK_FLOAT32_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_FLOAT64_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [9.0_real64, 9.0_real64], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_FLOAT64_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [9.0_real64, 9.0_real64])
        call check(error, .not. c%is_null(1_int64), &
            "PK_FLOAT64_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        call c%init(PK_LOGICAL_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([.true., .false., .true., .true.], [2, 2]))
        call c%set_null(1_int64)
        call c%set_at(1_int64, [.true., .true.], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(reshape([.true., .false., .true., .true.], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_LOGICAL_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [.true., .true.])
        call check(error, .not. c%is_null(1_int64), &
            "PK_LOGICAL_VEC: a default set_at should clear every one of the row's null bits")
        if (allocated(error)) return
        !
        do i = 1, 2
            call d(i)%set(2026, 7, 20 + i)
            call t(i)%set(i, 0, 0)
            call ts(i)%set(2026, 7, 20 + i, 6, 0, 0)
        end do
        call c%init(PK_DATE, 2_int64)
        call c%set_all(d)
        call c%set_null(1_int64)
        call c%set_at(1_int64, d(2), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_DATE: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_all(d, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_DATE: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_TIME, 2_int64)
        call c%set_all(t)
        call c%set_null(1_int64)
        call c%set_all(t, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_TIME: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, t(2), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_TIME: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, t(2))
        call check(error, .not. c%is_null(1_int64), "PK_TIME: a default set_at should overwrite a null element")
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP, 2_int64)
        call c%set_all(ts)
        call c%set_null(1_int64)
        call c%set_all(ts, modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, ts(2), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, ts(2))
        call check(error, .not. c%is_null(1_int64), &
            "PK_TIMESTAMP: a default set_at should overwrite a null element")
        if (allocated(error)) return
        !
        call c%init(PK_DATE_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([d(1), d(2), d(1), d(2)], [2, 2]))
        call c%set_null(1_int64)
        call c%set_all(reshape([d(1), d(2), d(1), d(2)], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_DATE_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [d(1), d(2)], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_DATE_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_TIME_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([t(1), t(2), t(1), t(2)], [2, 2]))
        call c%set_null(1_int64)
        call c%set_all(reshape([t(1), t(2), t(1), t(2)], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIME_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [t(1), t(2)], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIME_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_TIMESTAMP_VEC, 2_int64, width=2_int32)
        call c%set_all(reshape([ts(1), ts(2), ts(1), ts(2)], [2, 2]))
        call c%set_null(1_int64)
        call c%set_all(reshape([ts(1), ts(2), ts(1), ts(2)], [2, 2]), modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP_VEC: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, [ts(1), ts(2)], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), &
            "PK_TIMESTAMP_VEC: set_at(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        !
        call c%init(PK_STRING, 2_int64)
        call c%set_all(["a  ", "bcd"])
        call c%set_null(1_int64)
        call c%set_all(["xy ", "zzz"], modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_STRING: set_all(modify_nulls=.false.) must leave a null row null")
        if (allocated(error)) return
        call c%set_at(1_int64, "nope", modify_nulls=.false.)
        call check(error, c%is_null(1_int64), "PK_STRING: set_at(modify_nulls=.false.) must leave a null row null")
    end subroutine test_matrix_modify_nulls_all_kinds
    !
end module test_columns
