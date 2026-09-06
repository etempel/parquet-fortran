!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the table's column-shape verbs: `%get_matrix`, `%set_matrix`, `%drop_columns`
!! and `%keep_columns`.
!!
!! **The matrix pair's real subject is the ORIENTATION.** `arr(k, i)` is column `k` at row `i`,
!! and a transposed implementation still returns every value the caller asked for -- so a test
!! that only checks the multiset of values, or that uses a square fixture, passes against the
!! transpose. Every matrix test here therefore uses a NON-SQUARE fixture (three columns, six
!! rows) and asserts `shape(arr)` before anything else, and `test_get_matrix_row_cut` asserts the
!! `count(..., dim=1)` reduction the orientation exists for, against an oracle built by a loop of
!! `%get`. That oracle is independent because it never touches the matrix code.
!!
!! **The widening rule is asserted in both directions, because half of it is an absence.**
!! `%get_matrix` accepts exactly what `%get` widens -- `int32` into an `int64` matrix, `float32`
!! into a `real64` one -- and `%set_matrix` accepts neither, because `%set` narrows nothing.
!! `test_get_matrix_widens` is the positive half; the negative half is
!! `set_matrix_kind_mismatch` in test/error_scenarios.f90, without which "no widening on the way
!! back" is a sentence in a doc-comment that nothing checks.
!!
!! **The drop pair's negative controls are the ones that stop the tests passing vacuously.** Both
!! verbs return early when they would drop nothing, so `test_drop_columns_nothing_dropped` and
!! `test_keep_columns_everything` assert that `%generation()` is UNCHANGED -- the observable that
!! distinguishes "decided there was nothing to do" from "did the work and happened to end up in
!! the same place". `test_drop_columns_keeps_reading` is the other control: neither verb detaches,
!! so a column that was never read must still be readable after its neighbours are dropped, which
!! is the whole reason projecting a wide lazy table is cheap.
!!
!! **`force=` is tested against a generated table type**, since a predefined column is exactly
!! what `%bind_predefined` creates and nothing else in this suite has one. `%keep_columns` reaches
!! the R8 rule by NOT naming a column, which is the direction `%drop_column` cannot be used to
!! test at all.
!!
!! Abort paths live in test/error_scenarios.f90 as `get_matrix_*`/`set_matrix_*`/`drop_columns_*`/
!! `keep_columns_*` scenarios. Every test that writes a file uses its own path -- the suite runs
!! its tests concurrently, so a shared one would be truncated out from under its neighbour.
module test_table_matrix
    use parquet
    use parquet_table_example, only : parquet_table_test
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_matrix

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_matrix(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("get_matrix is (column, row) and holds every value", test_get_matrix_shape), &
            new_unittest("get_matrix answers a per-row count over the group", test_get_matrix_row_cut), &
            new_unittest("get_matrix widens exactly what get widens", test_get_matrix_widens), &
            new_unittest("get_matrix reports validity in the matching shape", test_get_matrix_mask), &
            new_unittest("get_matrix over a name string equals the array form", test_get_matrix_name_string), &
            new_unittest("get_matrix naming nothing gives a (0, nrows) matrix", test_get_matrix_no_names), &
            new_unittest("get_matrix carries every element kind", test_get_matrix_every_kind), &
            new_unittest("get_matrix reads a column nothing has touched", test_get_matrix_touches), &
            new_unittest("set_matrix is get_matrix's inverse", test_set_matrix_round_trip), &
            new_unittest("set_matrix marks the nulls its mask names", test_set_matrix_mask), &
            new_unittest("set_matrix leaves existing nulls when told to", test_set_matrix_modify_nulls), &
            new_unittest("set_matrix over a name string writes the same columns", test_set_matrix_name_string), &
            new_unittest("set_matrix neither detaches nor moves storage", test_set_matrix_keeps_pointers), &
            new_unittest("drop_columns removes several and keeps the order", test_drop_columns_order), &
            new_unittest("drop_columns ignore_missing skips what is absent", test_drop_columns_ignore_missing), &
            new_unittest("drop_columns that drops nothing changes nothing", test_drop_columns_nothing_dropped), &
            new_unittest("drop_columns does not stop the table reading", test_drop_columns_keeps_reading), &
            new_unittest("drop_columns takes a repeated name once", test_drop_columns_repeat), &
            new_unittest("keep_columns keeps the table's order, not the list's", test_keep_columns_order), &
            new_unittest("keep_columns naming every column changes nothing", test_keep_columns_everything), &
            new_unittest("keep_columns naming nothing empties the table", test_keep_columns_none), &
            new_unittest("keep_columns reads nothing it keeps", test_keep_columns_reads_nothing), &
            new_unittest("force lets both verbs drop a predefined column", test_force_drops_predefined), &
            new_unittest("a column added after a drop inherits nothing", test_recycled_slot_is_blank) &
            ]
    end subroutine collect_tests_table_matrix

    ! ---- fixtures -----------------------------------------------------------------------------

    !> A three-column, six-row real64 table. Deliberately NON-SQUARE, so a transposed
    !! `%get_matrix` cannot pass, and deliberately holding a sentinel (-999) in a different
    !! pattern per column, so a per-row count over the group is not the same for every row.
    subroutine build_bands(t)
        type(parquet_table), intent(out) :: t !! the table to build.
        call parquet_new_table(t)
        call t%add_column("b1", [1.0_real64, 2.0_real64, -999.0_real64, 4.0_real64, 5.0_real64, -999.0_real64])
        call t%add_column("b2", [-999.0_real64, 2.5_real64, 3.5_real64, -999.0_real64, -999.0_real64, 6.5_real64])
        call t%add_column("b3", [1.25_real64, 2.25_real64, 3.25_real64, 4.25_real64, -999.0_real64, 6.25_real64])
    end subroutine build_bands

    !> The same three columns plus two the matrix verbs never name, for the drop/keep tests.
    subroutine build_five(t)
        type(parquet_table), intent(out) :: t !! the table to build.
        call build_bands(t)
        call t%add_column("id", [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64])
        call t%add_column("ok", [.true., .false., .true., .true., .false., .true.])
    end subroutine build_five

    !> A file with two real64 columns and one nobody reads, for the laziness tests.
    subroutine write_lazy_fixture(file)
        character(len=*), intent(in) :: file !! path to write.
        type(parquet_table) :: t
        call parquet_new_table(t)
        call t%add_column("a", [1.0_real64, 2.0_real64, 3.0_real64])
        call t%add_column("b", [10.0_real64, 20.0_real64, 30.0_real64])
        call t%add_column("untouched", [7_int32, 8_int32, 9_int32])
        call parquet_write_table(t, file, overwrite=.true.)
    end subroutine write_lazy_fixture

    ! ---- %get_matrix --------------------------------------------------------------------------

    subroutine test_get_matrix_shape(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:), col(:)
        integer :: k
        character(len=2), parameter :: names(3) = ["b1", "b2", "b3"]

        call build_bands(t)
        call t%get_matrix(names, m)
        ! Asserted before the values: a transposed implementation returns every value the caller
        ! asked for, and 3 x 6 is what tells the two apart.
        call check(error, size(m, 1) == 3 .and. size(m, 2) == 6, &
            "%get_matrix must be shaped (size(names), nrows), i.e. 3 x 6 here")
        if (allocated(error)) return
        do k = 1, 3
            call t%get(names(k), col)
            call check(error, all(m(k, :) == col), "row " // achar(48 + k) // " of the matrix " // &
                "must be that column, in row order")
            if (allocated(error)) return
        end do
    end subroutine test_get_matrix_shape

    subroutine test_get_matrix_row_cut(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:), c1(:), c2(:), c3(:)
        integer, allocatable :: measured(:), oracle(:)
        integer(int64) :: i

        call build_bands(t)
        call t%get_matrix("b1, b2, b3", m)
        ! The reduction the column-major orientation exists for: one row's bands are contiguous,
        ! so dim=1 counts across them.
        measured = count(m > -900.0_real64, dim=1)
        ! An independent oracle, built without touching the matrix code at all.
        call t%get("b1", c1)
        call t%get("b2", c2)
        call t%get("b3", c3)
        allocate(oracle(6))
        do i = 1_int64, 6_int64
            oracle(i) = 0
            if (c1(i) > -900.0_real64) oracle(i) = oracle(i) + 1
            if (c2(i) > -900.0_real64) oracle(i) = oracle(i) + 1
            if (c3(i) > -900.0_real64) oracle(i) = oracle(i) + 1
        end do
        call check(error, size(measured) == 6, "count(m, dim=1) must give one entry per row")
        if (allocated(error)) return
        call check(error, all(measured == oracle), "the per-row band count must equal a loop of %get")
        if (allocated(error)) return
        ! And the fixture must actually discriminate, or the assertion above holds for any
        ! orientation: every row has exactly two good bands only if the sentinels were placed
        ! carelessly.
        call check(error, any(measured /= measured(1)), "the fixture must not give every row the " // &
            "same count, or a transposed matrix would satisfy this test too")
    end subroutine test_get_matrix_row_cut

    subroutine test_get_matrix_widens(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: mr(:,:)
        integer(int64), allocatable :: mi(:,:)

        call parquet_new_table(t)
        call t%add_column("wide", [1.5_real64, 2.5_real64])
        call t%add_column("narrow", [3.5_real32, 4.5_real32])
        call t%add_column("big", [10_int64, 20_int64])
        call t%add_column("small", [30_int32, 40_int32])
        call t%get_matrix("wide, narrow", mr)
        call check(error, all(mr(1, :) == [1.5_real64, 2.5_real64]), "the real64 column is unchanged")
        if (allocated(error)) return
        call check(error, all(mr(2, :) == [3.5_real64, 4.5_real64]), &
            "a float32 column must widen into a real64 matrix, exactly as %get widens it")
        if (allocated(error)) return
        call t%get_matrix("big, small", mi)
        call check(error, all(mi(1, :) == [10_int64, 20_int64]), "the int64 column is unchanged")
        if (allocated(error)) return
        call check(error, all(mi(2, :) == [30_int64, 40_int64]), &
            "an int32 column must widen into an int64 matrix, exactly as %get widens it")
    end subroutine test_get_matrix_widens

    subroutine test_get_matrix_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)
        logical, allocatable :: mask(:,:), vk(:)
        real(real64), allocatable :: col(:)
        integer :: k
        character(len=2), parameter :: names(3) = ["b1", "b2", "b3"]

        call build_bands(t)
        ! Nulls in two columns, in different rows, so a mask that reported one column's validity
        ! for the whole matrix would fail.
        call t%set_null("b1", 2_int64)
        call t%set_null("b3", 5_int64)
        call t%get_matrix(names, m, is_valid=mask)
        call check(error, size(mask, 1) == 3 .and. size(mask, 2) == 6, &
            "the mask must be shaped like the matrix")
        if (allocated(error)) return
        do k = 1, 3
            call t%get(names(k), col, is_valid=vk)
            call check(error, all(mask(k, :) .eqv. vk), "the mask's row " // achar(48 + k) // &
                " must equal that column's own %get validity")
            if (allocated(error)) return
        end do
        call check(error, .not. mask(1, 2) .and. .not. mask(3, 5), &
            "the two nulls placed in the fixture must both be reported")
        if (allocated(error)) return
        call check(error, mask(2, 2) .and. mask(1, 5), "and no other entry may be marked null")
    end subroutine test_get_matrix_mask

    subroutine test_get_matrix_name_string(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: ma(:,:), ms(:,:)
        character(len=2), parameter :: names(3) = ["b1", "b2", "b3"]

        call build_bands(t)
        call t%get_matrix(names, ma)
        call t%get_matrix("b1; b2, b3", ms)
        call check(error, all(shape(ma) == shape(ms)), "both name forms must give the same shape")
        if (allocated(error)) return
        call check(error, all(ma == ms), "both name forms must give the same values")
    end subroutine test_get_matrix_name_string

    subroutine test_get_matrix_no_names(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)
        integer, allocatable :: per_row(:)

        call build_bands(t)
        call t%get_matrix("", m)
        call check(error, size(m, 1) == 0 .and. size(m, 2) == 6, &
            "naming nothing must give a (0, nrows) matrix rather than an error or a (0, 0) one")
        if (allocated(error)) return
        ! The reason (0, nrows) rather than (0, 0): the degenerate cut still answers per row.
        per_row = count(m > 0.0_real64, dim=1)
        call check(error, size(per_row) == 6 .and. all(per_row == 0), &
            "a zero-column matrix must still reduce to one count per row")
    end subroutine test_get_matrix_no_names

    subroutine test_get_matrix_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        integer(int32), allocatable :: m32(:,:)
        integer(int64), allocatable :: m64(:,:)
        real(real32), allocatable :: r32(:,:)
        real(real64), allocatable :: r64(:,:)
        logical, allocatable :: mb(:,:)

        call parquet_new_table(t)
        call t%add_column("i32a", [1_int32, 2_int32])
        call t%add_column("i32b", [3_int32, 4_int32])
        call t%add_column("i64a", [5_int64, 6_int64])
        call t%add_column("f32a", [1.5_real32, 2.5_real32])
        call t%add_column("f64a", [3.5_real64, 4.5_real64])
        call t%add_column("ba", [.true., .false.])
        call t%add_column("bb", [.false., .true.])
        call t%get_matrix("i32a, i32b", m32)
        call check(error, all(m32(1, :) == [1_int32, 2_int32]) .and. all(m32(2, :) == [3_int32, 4_int32]), &
            "an int32 matrix must carry both int32 columns")
        if (allocated(error)) return
        call t%get_matrix("i64a", m64)
        call check(error, all(m64(1, :) == [5_int64, 6_int64]), "an int64 matrix must carry its column")
        if (allocated(error)) return
        call t%get_matrix("f32a", r32)
        call check(error, all(r32(1, :) == [1.5_real32, 2.5_real32]), "a real32 matrix must carry its column")
        if (allocated(error)) return
        call t%get_matrix("f64a", r64)
        call check(error, all(r64(1, :) == [3.5_real64, 4.5_real64]), "a real64 matrix must carry its column")
        if (allocated(error)) return
        call t%get_matrix("ba, bb", mb)
        call check(error, all(mb(1, :) .eqv. [.true., .false.]) .and. all(mb(2, :) .eqv. [.false., .true.]), &
            "a logical matrix must carry both logical columns")
    end subroutine test_get_matrix_every_kind

    subroutine test_get_matrix_touches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_matrix_touch.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)

        call write_lazy_fixture(file)
        call parquet_open_table(t, file)
        call check(error, t%residency("a") == RES_EMPTY, "the fixture must open with nothing read")
        if (allocated(error)) return
        call t%get_matrix("a, b", m)
        call check(error, all(m(1, :) == [1.0_real64, 2.0_real64, 3.0_real64]), &
            "%get_matrix must read a column that is not resident yet, as %get does")
        if (allocated(error)) return
        call check(error, t%residency("a") == RES_FULL .and. t%residency("b") == RES_FULL, &
            "both named columns must now be resident")
        if (allocated(error)) return
        call check(error, t%residency("untouched") == RES_EMPTY, &
            "and a column nobody named must still be unread")
    end subroutine test_get_matrix_touches

    ! ---- %set_matrix --------------------------------------------------------------------------

    subroutine test_set_matrix_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:), back(:,:), col(:)
        character(len=2), parameter :: names(3) = ["b1", "b2", "b3"]

        call build_bands(t)
        call t%get_matrix(names, m)
        call t%set_matrix(names, m)
        call t%get_matrix(names, back)
        call check(error, all(back == m), "%get_matrix then %set_matrix must be an identity")
        if (allocated(error)) return
        ! Written back into the RIGHT columns, which an identity over the matrix alone cannot
        ! show: a %set_matrix that wrote every column the same values would satisfy it.
        m(2, :) = m(2, :) + 100.0_real64
        call t%set_matrix(names, m)
        call t%get("b2", col)
        call check(error, all(col == m(2, :)), "a changed matrix row must land in that column")
        if (allocated(error)) return
        call t%get("b1", col)
        call check(error, all(col == m(1, :)), "and must not disturb its neighbours")
    end subroutine test_set_matrix_round_trip

    subroutine test_set_matrix_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)
        logical, allocatable :: mask(:,:), got(:)
        real(real64), allocatable :: col(:)
        character(len=2), parameter :: names(3) = ["b1", "b2", "b3"]

        call build_bands(t)
        call t%get_matrix(names, m)
        allocate(mask(3, 6))
        mask = .true.
        mask(1, 3) = .false.
        mask(3, 4) = .false.
        call t%set_matrix(names, m, is_valid=mask)
        call t%get("b1", col, is_valid=got)
        call check(error, .not. got(3) .and. count(.not. got) == 1, &
            "the mask's .false. entry must null exactly that row of that column")
        if (allocated(error)) return
        call t%get("b3", col, is_valid=got)
        call check(error, .not. got(4) .and. count(.not. got) == 1, &
            "and the same for the second column named in the mask")
        if (allocated(error)) return
        call t%get("b2", col, is_valid=got)
        call check(error, all(got), "a column whose mask row is all .true. must keep no nulls")
    end subroutine test_set_matrix_mask

    subroutine test_set_matrix_modify_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)
        logical, allocatable :: got(:)
        real(real64), allocatable :: col(:)
        character(len=2), parameter :: names(2) = ["b1", "b2"]

        call build_bands(t)
        call t%set_null("b1", 2_int64)
        call t%get_matrix(names, m)
        ! The default drops the bitmap, exactly as %set does.
        call t%set_matrix(names, m)
        call check(error, .not. t%has_nulls("b1"), &
            "the default %set_matrix must clear the nulls, as a whole-column %set does")
        if (allocated(error)) return
        call t%set_null("b1", 2_int64)
        call t%set_matrix(names, m, modify_nulls=.false.)
        call t%get("b1", col, is_valid=got)
        call check(error, .not. got(2) .and. count(.not. got) == 1, &
            "modify_nulls=.false. must leave the column's existing nulls exactly as they were")
    end subroutine test_set_matrix_modify_nulls

    subroutine test_set_matrix_name_string(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:), back(:,:)

        call build_bands(t)
        call t%get_matrix("b1, b2, b3", m)
        m = m * 2.0_real64
        call t%set_matrix("b1, b2, b3", m)
        call t%get_matrix("b1, b2, b3", back)
        call check(error, all(back == m), "the name-string form must write the same columns")
    end subroutine test_set_matrix_name_string

    subroutine test_set_matrix_keeps_pointers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_matrix_setptr.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: m(:,:)
        real(real64), pointer :: p(:)
        integer(int64) :: gen

        call write_lazy_fixture(file)
        call parquet_open_table(t, file)
        call t%get_matrix("a, b", m)
        call t%col("a", p)
        gen = t%generation()
        m = m + 1.0_real64
        call t%set_matrix("a, b", m)
        call check(error, .not. t%is_detached(), "%set_matrix must not detach the table")
        if (allocated(error)) return
        call check(error, t%generation() == gen, &
            "%set_matrix writes in place, so it must not advance the generation")
        if (allocated(error)) return
        call check(error, all(p == [2.0_real64, 3.0_real64, 4.0_real64]), &
            "an outstanding %col pointer must stay valid and see the written values")
        if (allocated(error)) return
        ! And the table can still read what it never read, which is what "does not detach" buys.
        call check(error, t%residency("untouched") == RES_EMPTY, "the third column is still unread")
    end subroutine test_set_matrix_keeps_pointers

    ! ---- %drop_columns ------------------------------------------------------------------------

    subroutine test_drop_columns_order(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)

        call build_five(t)
        call t%drop_columns("b2, id")
        call check(error, t%ncols() == 3, "five columns less two must leave three")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, trim(names(1)) == "b1" .and. trim(names(2)) == "b3" .and. &
            trim(names(3)) == "ok", "the survivors must keep their original order")
        if (allocated(error)) return
        call check(error, t%nrows() == 6, "dropping a column must not change the row count")
    end subroutine test_drop_columns_order

    subroutine test_drop_columns_ignore_missing(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t

        call build_five(t)
        call t%drop_columns("b1, nosuch, ok", ignore_missing=.true.)
        call check(error, t%ncols() == 3, "the two names that exist must be dropped")
        if (allocated(error)) return
        call check(error, .not. t%has_column("b1") .and. .not. t%has_column("ok"), &
            "and they must be the right two")
        if (allocated(error)) return
        call check(error, t%has_column("b2") .and. t%has_column("b3") .and. t%has_column("id"), &
            "the unnamed columns must be untouched")
    end subroutine test_drop_columns_ignore_missing

    subroutine test_drop_columns_nothing_dropped(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        integer(int64) :: gen

        call build_five(t)
        gen = t%generation()
        ! Nothing here exists, so with ignore_missing the whole call is a no-op. The generation is
        ! the observable that separates "decided there was nothing to do" from "did the work".
        call t%drop_columns("nosuch, alsonot", ignore_missing=.true.)
        call check(error, t%ncols() == 5, "a call that drops nothing must leave every column")
        if (allocated(error)) return
        call check(error, t%generation() == gen, &
            "and must not advance the generation, which would force every caller to re-fetch")
        if (allocated(error)) return
        ! The positive control: a real drop DOES advance it, or the assertion above is vacuous.
        call t%drop_columns("b1")
        call check(error, t%generation() > gen, "a drop that removes a column must advance it")
    end subroutine test_drop_columns_nothing_dropped

    subroutine test_drop_columns_keeps_reading(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_matrix_dropread.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)

        call write_lazy_fixture(file)
        call parquet_open_table(t, file)
        call t%drop_columns("a, b")
        call check(error, .not. t%is_detached(), "%drop_columns must not detach the table")
        if (allocated(error)) return
        ! The point of not detaching: a column that was never read is still readable afterwards.
        call t%get("untouched", got)
        call check(error, all(got == [7_int32, 8_int32, 9_int32]), &
            "a column nothing had read must still read from the file after its neighbours went")
    end subroutine test_drop_columns_keeps_reading

    subroutine test_drop_columns_repeat(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=2), parameter :: names(3) = ["b1", "b1", "b2"]

        call build_five(t)
        call t%drop_columns(names)
        call check(error, t%ncols() == 3, "a name repeated in the list must drop that column once")
        if (allocated(error)) return
        call check(error, t%has_column("b3") .and. t%has_column("id") .and. t%has_column("ok"), &
            "and must not take a neighbour with it")
    end subroutine test_drop_columns_repeat

    ! ---- %keep_columns ------------------------------------------------------------------------

    subroutine test_keep_columns_order(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        real(real64), allocatable :: col(:)

        call build_five(t)
        ! Named in an order the table does not use, which is what makes the assertion below say
        ! something: the result keeps the TABLE's order.
        call t%keep_columns("ok, b1")
        call check(error, t%ncols() == 2, "keeping two of five must leave two")
        if (allocated(error)) return
        call t%column_names(names)
        call check(error, trim(names(1)) == "b1" .and. trim(names(2)) == "ok", &
            "the kept columns must be in the TABLE's order, not the order they were named in")
        if (allocated(error)) return
        call t%get("b1", col)
        call check(error, all(col == [1.0_real64, 2.0_real64, -999.0_real64, 4.0_real64, &
            5.0_real64, -999.0_real64]), "a kept column's values must survive the compaction")
    end subroutine test_keep_columns_order

    subroutine test_keep_columns_everything(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        integer(int64) :: gen

        call build_five(t)
        gen = t%generation()
        call t%keep_columns("b1, b2, b3, id, ok")
        call check(error, t%ncols() == 5, "naming every column must keep every column")
        if (allocated(error)) return
        call check(error, t%generation() == gen, &
            "and must not advance the generation, since nothing moved")
        if (allocated(error)) return
        ! The positive control for that assertion.
        call t%keep_columns("b1, b2, b3, id")
        call check(error, t%generation() > gen, "dropping one column must advance it")
    end subroutine test_keep_columns_everything

    subroutine test_keep_columns_none(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t

        call build_five(t)
        call t%keep_columns("")
        call check(error, t%ncols() == 0, "naming nothing must drop every column")
        if (allocated(error)) return
        call check(error, t%nrows() == 6, &
            "and must leave the row count alone -- this is column-structural, not row-structural")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "an emptied table is still not detached")
    end subroutine test_keep_columns_none

    subroutine test_keep_columns_reads_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_matrix_keepread.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)

        call write_lazy_fixture(file)
        call parquet_open_table(t, file)
        call t%keep_columns("untouched")
        call check(error, t%ncols() == 1, "the projection must leave one column")
        if (allocated(error)) return
        call check(error, t%residency("untouched") == RES_EMPTY, &
            "projecting must read NOTHING -- that is what makes it cheap on a wide lazy table")
        if (allocated(error)) return
        call t%get("untouched", got)
        call check(error, all(got == [7_int32, 8_int32, 9_int32]), &
            "and the kept column must still read from the file afterwards")
    end subroutine test_keep_columns_reads_nothing

    subroutine test_force_drops_predefined(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table_test) :: t
        integer :: before

        call t%init_empty(4_int32)
        before = t%ncols()
        call check(error, before > 2, "the generated fixture must have several predefined columns")
        if (allocated(error)) return
        ! The negative control for the R8 guard, from both sides: with force= it goes through, so
        ! the abort scenarios are testing a guard that CAN be satisfied rather than one that
        ! refuses unconditionally.
        call t%drop_columns("ra, dec", force=.true.)
        call check(error, t%ncols() == before - 2, "force=.true. must let %drop_columns drop them")
        if (allocated(error)) return
        call t%keep_columns("uberid, flux", force=.true.)
        call check(error, t%ncols() == 2, "force=.true. must let %keep_columns drop the rest")
        if (allocated(error)) return
        call check(error, t%has_column("uberid") .and. t%has_column("flux"), &
            "and must keep the two that were named")
    end subroutine test_force_drops_predefined

    !> A slot vacated by a drop and then reused by `%add_column` must carry nothing over.
    !!
    !! This lives here rather than beside `%drop_column`'s own tests because P4 gave the library
    !! two more procedures that vacate a slot, and the three used to hold three hand-written
    !! copies of the same field list -- one of which was missing `unit`. The three arms below run
    !! the same assertion through `%drop_column`, `%drop_columns` and `%keep_columns`, so a fourth
    !! vacating path that forgets `reset_column_slot` fails here rather than in whichever verb
    !! happened to be exercised.
    !!
    !! `unit` is the field that escaped, and it is worse than a lingering value: `slot_unit` reads
    !! the SLOT's unit in preference to the parquet_column's own, so a stale one shadows the unit
    !! `%add_column(..., unit=)` was given and is then written into the file's MAML. The fixture
    !! is a generated table type because its predefined columns carry declared units (`flux` is
    !! "Jy"), which is what makes a leak visible at all. See feature_risks.md Risk-204.
    subroutine test_recycled_slot_is_blank(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table_test) :: t
        character(len=:), allocatable :: u
        real(real64), parameter :: v(2) = [5.0_real64, 6.0_real64]
        integer :: arm

        do arm = 1, 3
            call t%init_empty(2_int32)
            ! The fixture must actually carry a unit, or every assertion below holds vacuously.
            call t%unit("flux", u)
            call check(error, u == "Jy", "the generated fixture must declare a unit somewhere, " // &
                "or nothing here can detect one leaking")
            if (allocated(error)) return
            select case (arm)
            case (1)
                call t%drop_column("idx", force=.true.)
            case (2)
                call t%drop_columns("idx, flag", force=.true.)
            case default
                call t%keep_columns("uberid, flux", force=.true.)
            end select
            call t%add_column("fresh", v)
            call t%unit("fresh", u)
            call check(error, len(u) == 0, "arm " // achar(48 + arm) // ": a column added into a " // &
                "recycled slot inherited the unit '" // u // "'")
            if (allocated(error)) return
            ! `predefined` has no query of its own, so it is asserted by exercising the rule it
            ! governs: dropping a predefined column without force= aborts. If the flag ever leaks
            ! into a recycled slot this line kills the run with R8's message naming 'fresh', which
            ! is a louder failure than a [FAILED] line but is the only one available.
            call t%drop_column("fresh")
            call t%add_column("fresh", v)
            ! The positive control: the unit the caller DOES give must survive the same path.
            call t%add_column("withunit", v, unit="mJy")
            call t%unit("withunit", u)
            call check(error, u == "mJy", "arm " // achar(48 + arm) // ": a unit passed to " // &
                "%add_column must still be reported")
            if (allocated(error)) return
        end do
    end subroutine test_recycled_slot_is_blank

end module test_table_matrix
