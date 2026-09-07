!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the table's missing-data family: `%fillna`, `%ffill`, `%bfill` and `%dropna`.
!!
!! **The subject of most of these tests is the NULL FLAG, not the value.** A fill that writes the
!! sentinel and leaves the flag standing reads back perfectly through `%get` -- the value is
!! there, and nothing about it looks wrong -- while `%has_nulls` still says `.true.` and the row
!! is written to a Parquet file as Null. That is the failure `%fillna` exists to prevent and it is
!! invisible to any assertion about values alone, so every fill test asserts the flag as well
!! (`%has_nulls`, `%get(..., is_valid=)`), and one asserts it the whole way through a file:
!! `test_fillna_writes_non_nullable` fills, writes, reopens and reads the stored Arrow field's own
!! nullability back with `parquet_get_column_nullable`. Its negative control is the same table
!! written WITHOUT the fill, which must come back nullable -- without that half the test passes
!! against a writer that declares every column non-nullable.
!!
!! **A null lives in three different places and each has its own test.** A numeric or logical
!! column keeps it in a packed bitmap; a string column inside the `parquet_string_column`; a
!! temporal column inside the element itself, where `%clear_null` refuses to act at all. A rule
!! written for the first class alone passes every bitmap test and is wrong for the other two, so
!! `test_fillna_string`, `test_fillna_temporal` and `test_ffill_temporal` are not variants of the
!! numeric tests but the point of the exercise.
!!
!! **The negative controls, which are what stop these tests passing vacuously.** A fill on a
!! null-free column must change NO value and must NOT claim the column as the caller's own
!! (`test_fillna_no_nulls_changes_nothing` -- `%generation()` cannot see this either way, since
!! `%fillna` never advances it, so `%is_user_populated` is the only observable that rule has, and
!! `test_fillna_clears_the_flag` is its positive half); `%dropna` that drops no row must not
!! detach (`test_dropna_no_op_does_not_detach`); `%ffill` must leave a leading run of nulls alone
!! and `%bfill` a trailing one, since a scan that filled those would satisfy every "the nulls are
!! gone" assertion.
!!
!! **Two fixtures are shaped by mutations that survived the obvious ones.** `test_dropna_any_is_a_union`
!! names two columns whose nulls do NOT coincide, because with one named column -- or two that are
!! null in the same rows -- `how="any"` and `how="all"` are the same rule and nothing distinguishes
!! the default from its opposite. And `test_fillna_string` fills with `"n/a"` rather than `""`,
!! because a null string already reads back as `""`, so filling with `""` would pass against a fill
!! that routed the string column through the bitmap class's null-clearing pass -- which writes `""`
!! over what it just wrote. See feature_risks.md Risk-201 and Risk-203.
!!
!! Abort paths live in test/error_scenarios.f90 as `fillna_*`/`ffill_*`/`dropna_*` scenarios.
!! Every test writes its own fixture path -- the suite runs its tests concurrently, so a shared
!! one would be truncated out from under its neighbour.
module test_table_fill
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_fill

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_fill(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("fillna writes the value and clears the null", test_fillna_clears_the_flag), &
            new_unittest("fillna leaves the non-null rows alone", test_fillna_leaves_values_alone), &
            new_unittest("fillna widens an integer into a real column", test_fillna_widens), &
            new_unittest("fillna fills a string column", test_fillna_string), &
            new_unittest("fillna fills a temporal column", test_fillna_temporal), &
            new_unittest("fillna fills every null element of a vector column", test_fillna_vector), &
            new_unittest("fillna on a null-free column changes nothing", test_fillna_no_nulls_changes_nothing), &
            new_unittest("fillna neither detaches nor moves storage", test_fillna_keeps_pointers), &
            new_unittest("fillna over a name string fills every column", test_fillna_name_string), &
            new_unittest("a filled column is written non-nullable", test_fillna_writes_non_nullable), &
            new_unittest("fillna reads a column nothing has touched", test_fillna_touches), &
            new_unittest("ffill carries a value forward, leading nulls stay", test_ffill_basic), &
            new_unittest("bfill carries a value back, trailing nulls stay", test_bfill_basic), &
            new_unittest("ffill limit caps the run it fills", test_ffill_limit), &
            new_unittest("bfill limit caps the run it fills", test_bfill_limit), &
            new_unittest("ffill fills a string column", test_ffill_string), &
            new_unittest("ffill fills a temporal column", test_ffill_temporal), &
            new_unittest("ffill treats each vector element as its own series", test_ffill_vector), &
            new_unittest("ffill on an all-null column fills nothing", test_ffill_all_null), &
            new_unittest("fillna fills every null element of a string vector column", &
                test_fillna_string_vector), &
            new_unittest("ffill with limit on a string column whose values differ in length", &
                test_ffill_string_limit_lengths), &
            new_unittest("bfill with limit on a string column whose values differ in length", &
                test_bfill_string_limit_lengths), &
            new_unittest("ffill treats each string vector element as its own series", &
                test_ffill_string_vector), &
            new_unittest("bfill treats each string vector element as its own series", &
                test_bfill_string_vector), &
            new_unittest("dropna drops a row null in any named column", test_dropna_any), &
            new_unittest("dropna any is the union across columns, not the intersection", &
                test_dropna_any_is_a_union), &
            new_unittest("dropna how=all drops only all-null rows", test_dropna_all), &
            new_unittest("dropna min_valid keeps a row with enough values", test_dropna_min_valid), &
            new_unittest("dropna with no names uses the resident columns", test_dropna_resident_only), &
            new_unittest("dropna that drops nothing does not detach", test_dropna_no_op_does_not_detach), &
            new_unittest("dropna detaches when it drops a row", test_dropna_detaches), &
            new_unittest("dropna over a name string", test_dropna_name_string), &
            new_unittest("dropna counts a vector row null if any element is", test_dropna_vector), &
            new_unittest("fillna and dropna on a slice see the slice only", test_fill_on_a_slice), &
            new_unittest("fillna resolves every value kind, in both name forms", &
                test_fillna_every_value_kind) &
            ]
    end subroutine collect_tests_table_fill

    ! ---- fixtures -----------------------------------------------------------------------------

    !> Six rows: `v` (int32) and `x` (real64) null in rows 3 and 5, `u` (int32) null-free.
    subroutine write_gap_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: v(6) = [10, 20, 0, 40, 0, 60]
        integer(int32) :: u(6) = [1, 2, 3, 4, 5, 6]
        real(real64) :: x(6) = [1.5_real64, 2.5_real64, 0.0_real64, 4.5_real64, 0.0_real64, 6.5_real64]
        logical :: valid(6) = [.true., .true., .false., .true., .false., .true.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_write_column(writer, "x", x, is_valid=valid)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
    end subroutine write_gap_fixture

    !> Eight rows whose nulls form runs at both ends and in the middle, so that a leading run, a
    !> trailing run and a capped run can all be asserted from one column.
    !>
    !> Valid at rows 3, 4 and 7 only: rows 1-2 lead, rows 5-6 are a run of two between values, and
    !> row 8 trails. That shape is what makes `limit=1` distinguishable from no limit at all.
    subroutine write_run_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: v(8) = [0, 0, 30, 40, 0, 0, 70, 0]
        integer(int32) :: u(8) = [1, 2, 3, 4, 5, 6, 7, 8]
        logical :: valid(8) = [.false., .false., .true., .true., .false., .false., .true., .false.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
    end subroutine write_run_fixture

    ! ---- %fillna ------------------------------------------------------------------------------

    !> The whole point: the value lands AND the null flag goes with it.
    subroutine test_fillna_clears_the_flag(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_flag.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)
        logical, allocatable :: valid(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call check(error, t%has_nulls("v"), "the fixture's v column should start with nulls")
        if (allocated(error)) return
        call t%fillna("v", -999_int32)
        call check(error, .not. t%has_nulls("v"), "%fillna must clear the null flag, not only " // &
            "write the value -- a row that is both -999 and Null is the bug this verb replaces")
        if (allocated(error)) return
        call t%get("v", got, is_valid=valid)
        call check(error, all(got == [10, 20, -999, 40, -999, 60]), "filled values are wrong")
        if (allocated(error)) return
        call check(error, all(valid), "%get's own validity mask must report every row valid")
        if (allocated(error)) return
        ! The positive control for test_fillna_no_nulls_changes_nothing's last assertion: a fill
        ! that DID write claims the column, so that assertion is not merely reading a flag nothing
        ! ever sets.
        call check(error, t%is_user_populated("v"), "a fill that wrote must claim the column's values")
    end subroutine test_fillna_clears_the_flag

    !> A fill must touch the null rows and nothing else.
    subroutine test_fillna_leaves_values_alone(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_untouched.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: before(:), after(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%get("x", before)
        call t%fillna("x", -1.0_real64)
        call t%get("x", after)
        call check(error, all(after([1, 2, 4, 6]) == before([1, 2, 4, 6])), &
            "%fillna must not change a row that was not null")
        if (allocated(error)) return
        call check(error, all(after([3, 5]) == -1.0_real64), "the null rows must hold the fill value")
    end subroutine test_fillna_leaves_values_alone

    !> An integer value fills a real column: the widening half of the compatibility rule.
    subroutine test_fillna_widens(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_widen.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: got(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%fillna("x", -9_int32)
        call t%get("x", got)
        call check(error, all(got([3, 5]) == -9.0_real64), "an int32 value must widen into a real column")
        if (allocated(error)) return
        call check(error, .not. t%has_nulls("x"), "the nulls must be cleared on the widening path too")
    end subroutine test_fillna_widens

    !> Storage class 2: the null lives in the parquet_string_column, and its own %set clears it.
    subroutine test_fillna_string(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_string.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        character(len=8) :: s(4) = ["alpha   ", "        ", "gamma   ", "        "]
        logical :: valid(4) = [.true., .false., .true., .false.]
        character(len=:), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "s", s, is_valid=valid)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call check(error, t%has_nulls("s"), "the fixture's s column should start with nulls")
        if (allocated(error)) return
        ! The sentinel is deliberately NOT "" even though the empty string is the WAVES spelling
        ! and is equally accepted (scenario_fill_control uses it). A null string already reads
        ! back as "", so filling with "" makes the value assertion below true whatever the code
        ! did -- including for a fill that routed a string column through the bitmap class's
        ! null-clearing pass, which writes "" over whatever it just wrote.
        call t%fillna("s", "n/a")
        call check(error, .not. t%has_nulls("s"), "%fillna must clear a string column's nulls")
        if (allocated(error)) return
        call t%get("s", got, is_valid=vmask)
        call check(error, all(vmask), "every row must report valid after the fill")
        if (allocated(error)) return
        call check(error, trim(got(2)) == "n/a" .and. trim(got(4)) == "n/a", &
            "the null rows must hold the sentinel, not an empty string")
        if (allocated(error)) return
        call check(error, trim(got(3)) == "gamma", "a row that had a value must keep it")
    end subroutine test_fillna_string

    !> Storage class 3: the null is INSIDE the element, so %clear_null refuses it and writing the
    !> value is the only way to clear it. A rule written for the bitmap kinds aborts here.
    subroutine test_fillna_temporal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_temporal.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        type(parquet_date) :: d(4), fill, got(4)
        logical, allocatable :: vmask(:)
        type(parquet_date), allocatable :: back(:)

        call d(1)%set(2020, 1, 1)
        call d(3)%set(2020, 3, 3)
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "d", d)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call check(error, t%has_nulls("d"), "rows 2 and 4 were never set, so they are null")
        if (allocated(error)) return
        ! ARMS the trap the next assertion springs. A temporal column caches "does this hold a
        ! null" and only `rescan_temporal_nulls` ever marks that cache clean, so without this call
        ! the cache stays dirty, every query rescans, and a fill that forgot to refresh it would
        ! still answer correctly. With the cache clean and saying .true., a fill that writes the
        ! values and leaves it alone reports a column full of nulls that has none.
        call t%compact_validity("d")
        call fill%set(1970, 1, 1)
        call t%fillna("d", fill)
        call check(error, .not. t%has_nulls("d"), "%fillna must clear a temporal column's nulls")
        if (allocated(error)) return
        call t%get("d", back, is_valid=vmask)
        call check(error, all(vmask), "every temporal row must report valid after the fill")
        if (allocated(error)) return
        got = back
        call check(error, .not. got(2)%is_null() .and. got(2)%year() == 1970, &
            "the filled element must carry the fill value and stop being null")
    end subroutine test_fillna_temporal

    !> On a vector column every null ELEMENT takes the value, not the row as a whole.
    subroutine test_fillna_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_vector.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: v(3, 4)
        logical :: valid(3, 4)
        integer(int32), allocatable :: got(:,:)
        logical, allocatable :: vmask(:,:)
        integer :: i, j

        do j = 1, 4
            do i = 1, 3
                v(i, j) = i*10 + j
                valid(i, j) = .not. (i == 2 .and. j == 3)
            end do
        end do
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "m", v, is_valid=valid)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%fillna("m", -7_int32)
        call t%get("m", got, is_valid=vmask)
        call check(error, got(2, 3) == -7_int32, "the null ELEMENT must take the fill value")
        if (allocated(error)) return
        call check(error, got(1, 3) == 13 .and. got(3, 3) == 33, &
            "the other elements of that row must be untouched -- the fill is per element, not per row")
        if (allocated(error)) return
        call check(error, all(vmask), "every element must report valid after the fill")
    end subroutine test_fillna_vector

    !> The negative control for every test above: with no nulls there is nothing to write, and a
    !> fill that wrote anyway would overwrite real data with the sentinel.
    subroutine test_fillna_no_nulls_changes_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_nonull.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: before(:), after(:)
        integer(int64) :: gen

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%get("u", before)
        gen = t%generation()
        call t%fillna("u", -999_int32)
        call t%get("u", after)
        call check(error, all(after == before), "a fill on a null-free column must write nothing")
        if (allocated(error)) return
        call check(error, .not. t%has_nulls("u"), "the column must still report no nulls")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "%fillna reallocates nothing, so the generation " // &
            "counter must not move")
        if (allocated(error)) return
        ! %generation() cannot see this either way -- %fillna never advances it -- so the observable
        ! that a no-op fill really is one is the claim it did NOT make on the column's values. A
        ! fill that marked every named column would make %evict_column and %reload start refusing
        ! this one, which is a change to the table by a call documented as making none.
        call check(error, .not. t%is_user_populated("u"), "a fill that wrote nothing must not " // &
            "claim the column's values as the caller's own")
    end subroutine test_fillna_no_nulls_changes_nothing

    !> Values are written in place, so an outstanding %col pointer stays valid AND sees the fill.
    subroutine test_fillna_keeps_pointers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_pointer.parquet"
        type(parquet_table), target :: t
        integer(int32), pointer :: p(:)
        integer(int64) :: gen

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%col("v", p)
        gen = t%generation()
        call t%fillna("v", -5_int32)
        call check(error, t%generation() == gen, "%fillna must not advance the generation counter")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "%fillna must not detach the table")
        if (allocated(error)) return
        call check(error, p(3) == -5_int32 .and. p(5) == -5_int32, &
            "the pointer taken before the fill must see the filled values")
    end subroutine test_fillna_keeps_pointers

    !> The separated-string name form fills every column it names.
    subroutine test_fillna_name_string(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_namestring.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: x(:)
        integer(int32), allocatable :: v(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        ! `v` is int32 and `x` is real64, so one integer value has to widen for the second column
        ! and not for the first -- which is what makes this a test of the per-column rule rather
        ! than of the name splitting alone.
        call t%fillna("v, x", -3_int32)
        call t%get("v", v)
        call t%get("x", x)
        call check(error, .not. t%has_nulls("v") .and. .not. t%has_nulls("x"), &
            "both named columns must be filled")
        if (allocated(error)) return
        call check(error, v(3) == -3_int32 .and. x(3) == -3.0_real64, "filled values are wrong")
    end subroutine test_fillna_name_string

    !> **The nullability contract, end to end.** A filled column has no nulls, so the writer
    !> declares its Arrow field non-nullable -- and the negative control is the same table written
    !> without the fill, which must come back nullable. Without that half this test would pass
    !> against a writer that declared everything non-nullable.
    subroutine test_fillna_writes_non_nullable(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: src = "test_run/test_fill_rt_src.parquet"
        character(len=*), parameter :: filled = "test_run/test_fill_rt_filled.parquet"
        character(len=*), parameter :: plain = "test_run/test_fill_rt_plain.parquet"
        type(parquet_table) :: t
        type(parquet_reader) :: reader
        logical :: nullable
        integer(int32), allocatable :: got(:)

        call write_gap_fixture(src)
        ! The control first: unfilled, the column still carries nulls and the field is nullable.
        call parquet_open_table(t, src)
        call t%materialize_all()
        call parquet_write_table(t, plain, overwrite=.true.)
        call parquet_open_reader(reader, plain)
        call parquet_get_column_nullable(reader, "v", nullable)
        call parquet_close_reader(reader)
        call check(error, nullable, "the control write must declare a null-carrying column nullable")
        if (allocated(error)) return
        ! And now the same table, filled.
        call t%fillna("v", -999_int32)
        call parquet_write_table(t, filled, overwrite=.true.)
        call parquet_open_reader(reader, filled)
        call parquet_get_column_nullable(reader, "v", nullable)
        call parquet_close_reader(reader)
        call check(error, .not. nullable, "a filled column has no nulls left, so its Arrow field " // &
            "must be written non-nullable")
        if (allocated(error)) return
        call parquet_open_table(t, filled)
        call t%get("v", got)
        call check(error, all(got == [10, 20, -999, 40, -999, 60]), &
            "the filled values must survive the round trip")
        if (allocated(error)) return
        call check(error, .not. t%has_nulls("v"), "and the column must read back with no nulls")
    end subroutine test_fillna_writes_non_nullable

    !> Naming a column reads it: %fillna resolves through the ordinary lazy touch.
    subroutine test_fillna_touches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_touch.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call check(error, t%residency("v") == RES_EMPTY, "nothing should be resident on a freshly opened table")
        if (allocated(error)) return
        call t%fillna("v", -1_int32)
        call check(error, t%residency("v") == RES_FULL, "%fillna must read the column it names")
        if (allocated(error)) return
        call t%get("v", got)
        call check(error, got(3) == -1_int32, "and must fill it")
    end subroutine test_fillna_touches

    ! ---- %ffill / %bfill ----------------------------------------------------------------------

    !> Forward: rows 1-2 lead and stay null; every later null takes the value above it.
    subroutine test_ffill_basic(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_ffill.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call write_run_fixture(file)
        call parquet_open_table(t, file)
        call t%ffill("v")
        call t%get("v", got, is_valid=vmask)
        call check(error, .not. vmask(1) .and. .not. vmask(2), &
            "a LEADING run of nulls has no value before it and must stay null")
        if (allocated(error)) return
        call check(error, all(vmask(3:8)), "every null after the first value must be filled")
        if (allocated(error)) return
        call check(error, all(got(3:8) == [30, 40, 40, 40, 70, 70]), "forward-filled values are wrong")
        if (allocated(error)) return
        call check(error, t%has_nulls("v"), "two nulls remain, so the column still reports nulls")
    end subroutine test_ffill_basic

    !> Backward: row 8 trails and stays null; every earlier null takes the value below it.
    subroutine test_bfill_basic(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_bfill.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call write_run_fixture(file)
        call parquet_open_table(t, file)
        call t%bfill("v")
        call t%get("v", got, is_valid=vmask)
        call check(error, .not. vmask(8), &
            "a TRAILING run of nulls has no value after it and must stay null")
        if (allocated(error)) return
        call check(error, all(vmask(1:7)), "every null before the last value must be filled")
        if (allocated(error)) return
        call check(error, all(got(1:7) == [30, 30, 30, 40, 70, 70, 70]), "back-filled values are wrong")
    end subroutine test_bfill_basic

    !> `limit` caps the RUN, so in the two-null run at rows 5-6 only row 5 is filled.
    subroutine test_ffill_limit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_ffill_limit.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call write_run_fixture(file)
        call parquet_open_table(t, file)
        call t%ffill("v", 1)
        call t%get("v", got, is_valid=vmask)
        call check(error, vmask(5) .and. .not. vmask(6), &
            "with limit=1 the first null of a run is filled and the second is not")
        if (allocated(error)) return
        call check(error, got(5) == 40, "the filled row takes the value above the run")
        if (allocated(error)) return
        call check(error, vmask(8), "row 8 is a run of one, so limit=1 still fills it")
        if (allocated(error)) return
        call check(error, got(8) == 70, "and it takes the value above it")
    end subroutine test_ffill_limit

    !> The same cap the other way round, and with the int64 limit specific.
    subroutine test_bfill_limit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_bfill_limit.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call write_run_fixture(file)
        call parquet_open_table(t, file)
        call t%bfill("v", 1_int64)
        call t%get("v", got, is_valid=vmask)
        call check(error, vmask(2) .and. .not. vmask(1), &
            "backwards, the run at rows 1-2 is filled from row 3, so row 2 fills and row 1 does not")
        if (allocated(error)) return
        call check(error, got(2) == 30, "row 2 takes the value below it")
        if (allocated(error)) return
        call check(error, vmask(6) .and. .not. vmask(5), "and the run at rows 5-6 fills only row 6")
    end subroutine test_bfill_limit

    !> A string column carries its own null, so the fill is a get-then-set rather than a bit clear.
    subroutine test_ffill_string(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_ffill_str.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        character(len=8) :: s(4) = ["alpha   ", "        ", "        ", "delta   "]
        logical :: valid(4) = [.true., .false., .false., .true.]
        character(len=:), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "s", s, is_valid=valid)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%ffill("s")
        call t%get("s", got, is_valid=vmask)
        call check(error, all(vmask), "every row follows a value, so all four must be filled")
        if (allocated(error)) return
        call check(error, trim(got(2)) == "alpha" .and. trim(got(3)) == "alpha", &
            "the string value must be carried forward, not replaced by an empty string")
        if (allocated(error)) return
        call check(error, trim(got(4)) == "delta", "a row that had a value keeps it")
    end subroutine test_ffill_string

    !> A temporal element's null is inside it, so the copy both fills and clears.
    subroutine test_ffill_temporal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_ffill_dt.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        type(parquet_date) :: d(4)
        type(parquet_date), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call d(1)%set(2021, 6, 15)
        call d(4)%set(2022, 1, 1)
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "d", d)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%compact_validity("d")   ! arm the stale-cache trap; see test_fillna_temporal
        call t%ffill("d")
        call t%get("d", got, is_valid=vmask)
        call check(error, all(vmask), "every temporal row must be filled and report valid")
        if (allocated(error)) return
        call check(error, got(2)%year() == 2021 .and. got(2)%month() == 6 .and. got(2)%day() == 15, &
            "the carried date must be the previous non-null one")
        if (allocated(error)) return
        call check(error, .not. t%has_nulls("d"), "the column's cached null answer must be refreshed")
    end subroutine test_ffill_temporal

    !> Element 1 and element 2 of a vector column are separate series, so a null in one is filled
    !> from its own column of elements and not from its row neighbour.
    subroutine test_ffill_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_ffill_vec.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: v(2, 4)
        logical :: valid(2, 4)
        integer(int32), allocatable :: got(:,:)
        logical, allocatable :: vmask(:,:)

        v = reshape([11, 21, 12, 22, 13, 23, 14, 24], [2, 4])
        valid = .true.
        valid(1, 2) = .false.   ! element 1 falls out at row 2 ...
        valid(1, 3) = .false.   ! ... and at row 3, so both take row 1's element 1
        valid(2, 1) = .false.   ! element 2 leads with a null, which must stay
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "m", v, is_valid=valid)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%ffill("m")
        call t%get("m", got, is_valid=vmask)
        call check(error, got(1, 2) == 11 .and. got(1, 3) == 11, &
            "element 1 must be carried from element 1 of the row above, not from element 2")
        if (allocated(error)) return
        call check(error, .not. vmask(2, 1), &
            "element 2 leads with a null and has nothing before it, so it must stay null")
        if (allocated(error)) return
        call check(error, got(2, 2) == 22, "an element that had a value keeps it")
    end subroutine test_ffill_vector

    !> Storage class 2 on a vector column: every null ELEMENT takes the value through one rebuild
    !> of the store, and the elements around it -- of different lengths, shortest first -- keep
    !> their own bytes. Built in memory; nothing here needs a file.
    subroutine test_fillna_string_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=6) :: m(3, 4)
        logical :: valid(3, 4)
        character(len=:), allocatable :: got(:,:)
        logical, allocatable :: vmask(:,:)

        m(:, 1) = ["a     ", "bb    ", "ccc   "]
        m(:, 2) = ["dddd  ", "eeeee ", "ffffff"]
        m(:, 3) = ["g     ", "hh    ", "iii   "]
        m(:, 4) = ["jjjj  ", "k     ", "ll    "]
        valid = .true.
        valid(2, 1) = .false.
        valid(3, 3) = .false.
        valid(1, 4) = .false.
        call parquet_new_table(t)
        call t%add_column("m", m)
        call t%set("m", m, is_valid=valid)
        call check(error, t%has_nulls("m"), "the fixture must start with nulls")
        if (allocated(error)) return
        call t%fillna("m", "n/a")
        call check(error, .not. t%has_nulls("m"), "%fillna must clear a string vector's nulls")
        if (allocated(error)) return
        call t%get("m", got, is_valid=vmask)
        call check(error, all(vmask), "every element must report valid after the fill")
        if (allocated(error)) return
        call check(error, trim(got(2, 1)) == "n/a" .and. trim(got(3, 3)) == "n/a" .and. trim(got(1, 4)) == "n/a", &
            "each null element takes the value")
        if (allocated(error)) return
        call check(error, trim(got(1, 1)) == "a" .and. trim(got(3, 2)) == "ffffff" .and. trim(got(2, 4)) == "k", &
            "the elements around them keep their own bytes")
    end subroutine test_fillna_string_vector

    !> `%ffill` on a string column is ONE gather of the store over its own elements; a wrong
    !> source index would carry the wrong bytes, so the carried values differ in length (shortest
    !> first), and `limit=` leaves a run's tail null, which the gather must carry as null too.
    subroutine test_ffill_string_limit_lengths(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=4) :: s(8) = ["a   ", "    ", "    ", "bbbb", "    ", "    ", "    ", "cc  "]
        logical :: valid(8) = [.true., .false., .false., .true., .false., .false., .false., .true.]
        character(len=:), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%set("s", s, is_valid=valid)
        call t%ffill("s", 2)
        call t%get("s", got, is_valid=vmask)
        call check(error, all(vmask(1:6)) .and. .not. vmask(7) .and. vmask(8), &
            "limit=2 fills two nulls after each value and leaves the third of a run null")
        if (allocated(error)) return
        call check(error, trim(got(2)) == "a" .and. trim(got(3)) == "a", "rows 2 and 3 carry the one-byte value")
        if (allocated(error)) return
        call check(error, trim(got(5)) == "bbbb" .and. trim(got(6)) == "bbbb", &
            "rows 5 and 6 carry the four-byte value")
        if (allocated(error)) return
        call check(error, trim(got(8)) == "cc" .and. trim(got(4)) == "bbbb" .and. trim(got(1)) == "a", &
            "a row that had a value keeps it")
        if (allocated(error)) return
        call check(error, t%has_nulls("s"), "row 7 stays null, so the column still holds a null")
    end subroutine test_ffill_string_limit_lengths

    !> The same fixture backwards, with the int64 limit specific: one row of each run fills from
    !> the value BELOW it and the rest of the run stays null.
    subroutine test_bfill_string_limit_lengths(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=4) :: s(8) = ["a   ", "    ", "    ", "bbbb", "    ", "    ", "    ", "cc  "]
        logical :: valid(8) = [.true., .false., .false., .true., .false., .false., .false., .true.]
        character(len=:), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%set("s", s, is_valid=valid)
        call t%bfill("s", 1_int64)
        call t%get("s", got, is_valid=vmask)
        call check(error, vmask(3) .and. .not. vmask(2) .and. vmask(7) .and. .not. vmask(6) .and. .not. vmask(5), &
            "limit=1 fills the last null of each run and leaves the rest of the run null")
        if (allocated(error)) return
        call check(error, trim(got(3)) == "bbbb" .and. trim(got(7)) == "cc", "each filled row carries the value below it")
        if (allocated(error)) return
        call check(error, trim(got(1)) == "a" .and. trim(got(4)) == "bbbb" .and. trim(got(8)) == "cc", &
            "a row that had a value keeps it")
    end subroutine test_bfill_string_limit_lengths

    !> Element 1 and element 2 of a string vector are separate series: a null in one fills from
    !> its own column of elements, never from its row neighbour. The flat index the gather is
    !> built on is what a scalar-only test cannot see.
    subroutine test_ffill_string_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=4) :: m(2, 4)
        logical :: valid(2, 4)
        character(len=:), allocatable :: got(:,:)
        logical, allocatable :: vmask(:,:)

        m(:, 1) = ["aaa ", "    "]
        m(:, 2) = ["    ", "cc  "]
        m(:, 3) = ["    ", "    "]
        m(:, 4) = ["b   ", "dddd"]
        valid = .true.
        valid(2, 1) = .false.   ! element 2 leads with a null, which must stay
        valid(1, 2) = .false.   ! element 1 falls out at rows 2 and 3 ...
        valid(1, 3) = .false.   ! ... and both take row 1's element 1
        valid(2, 3) = .false.   ! element 2 falls out at row 3 and takes row 2's element 2
        call parquet_new_table(t)
        call t%add_column("m", m)
        call t%set("m", m, is_valid=valid)
        call t%ffill("m")
        call t%get("m", got, is_valid=vmask)
        call check(error, trim(got(1, 2)) == "aaa" .and. trim(got(1, 3)) == "aaa", &
            "element 1 must be carried from element 1 of the row above, not from element 2")
        if (allocated(error)) return
        call check(error, trim(got(2, 3)) == "cc", "element 2 must be carried from element 2 of the row above")
        if (allocated(error)) return
        call check(error, .not. vmask(2, 1), "element 2 leads with a null and has nothing before it, so it stays null")
        if (allocated(error)) return
        call check(error, trim(got(1, 4)) == "b" .and. trim(got(2, 4)) == "dddd", "elements that had values keep them")
    end subroutine test_ffill_string_vector

    !> The same fixture backwards: element 1 fills from row 4's one-byte value, element 2 from row
    !> 2's and row 4's, and nothing crosses between the two series.
    subroutine test_bfill_string_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        character(len=4) :: m(2, 4)
        logical :: valid(2, 4)
        character(len=:), allocatable :: got(:,:)
        logical, allocatable :: vmask(:,:)

        m(:, 1) = ["aaa ", "    "]
        m(:, 2) = ["    ", "cc  "]
        m(:, 3) = ["    ", "    "]
        m(:, 4) = ["b   ", "dddd"]
        valid = .true.
        valid(2, 1) = .false.
        valid(1, 2) = .false.
        valid(1, 3) = .false.
        valid(2, 3) = .false.
        call parquet_new_table(t)
        call t%add_column("m", m)
        call t%set("m", m, is_valid=valid)
        call t%bfill("m")
        call t%get("m", got, is_valid=vmask)
        call check(error, all(vmask), "every element follows a value backwards, so all must be filled")
        if (allocated(error)) return
        call check(error, trim(got(1, 2)) == "b" .and. trim(got(1, 3)) == "b", &
            "element 1 must be carried back from element 1 of row 4")
        if (allocated(error)) return
        call check(error, trim(got(2, 1)) == "cc" .and. trim(got(2, 3)) == "dddd", &
            "element 2 must be carried back from element 2 of the next row that has one")
        if (allocated(error)) return
        call check(error, trim(got(1, 1)) == "aaa" .and. trim(got(2, 2)) == "cc", "elements that had values keep them")
    end subroutine test_bfill_string_vector

    !> With no value anywhere there is nothing to carry, and the column must come back unchanged
    !> rather than filled with whatever the storage happened to hold.
    subroutine test_ffill_all_null(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_ffill_allnull.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: v(4) = [0, 0, 0, 0]
        logical :: valid(4) = [.false., .false., .false., .false.]
        integer(int32), allocatable :: got(:)
        logical, allocatable :: vmask(:)

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%ffill("v")
        call t%get("v", got, is_valid=vmask)
        call check(error, .not. any(vmask), "an all-null column has nothing to carry and stays all null")
    end subroutine test_ffill_all_null

    ! ---- %dropna ------------------------------------------------------------------------------

    !> The default: a row null in ANY named column goes.
    subroutine test_dropna_any(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropany.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%dropna(["v"])
        call check(error, t%nrows() == 4, "rows 3 and 5 are null in v and must go")
        if (allocated(error)) return
        call t%get("u", got)
        call check(error, all(got == [1, 2, 4, 6]), "the surviving rows must keep their order")
    end subroutine test_dropna_any

    !> With ONE named column "any" and "all" are the same rule, so a suite that only ever names
    !> one cannot tell them apart. Here `a` and `b` are null in DIFFERENT rows, which is the
    !> smallest fixture on which the default has to be the union.
    subroutine test_dropna_any_is_a_union(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropunion.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: a(4) = [1, 0, 3, 4], b(4) = [1, 2, 0, 4], u(4) = [1, 2, 3, 4]
        logical :: va(4) = [.true., .false., .true., .true.]
        logical :: vb(4) = [.true., .true., .false., .true.]
        integer(int32), allocatable :: got(:)

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "a", a, is_valid=va)
        call parquet_write_column(writer, "b", b, is_valid=vb)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%dropna(["a", "b"])
        call check(error, t%nrows() == 2, "row 2 is null in a and row 3 in b, so the default " // &
            "drops BOTH -- keeping either would be how=all's answer")
        if (allocated(error)) return
        call t%get("u", got)
        call check(error, all(got == [1, 4]), "the wrong rows survived the default policy")
    end subroutine test_dropna_any_is_a_union

    !> `how="all"` keeps a row unless EVERY named column is null there.
    subroutine test_dropna_all(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropall.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: a(4) = [1, 0, 0, 4], b(4) = [1, 2, 0, 4], u(4) = [1, 2, 3, 4]
        logical :: va(4) = [.true., .false., .false., .true.]
        logical :: vb(4) = [.true., .true., .false., .true.]
        integer(int32), allocatable :: got(:)

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "a", a, is_valid=va)
        call parquet_write_column(writer, "b", b, is_valid=vb)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        ! Read every column first: a drop detaches the table, and a column that was
        ! never read can never be read afterwards.
        call t%materialize_all()
        call t%dropna(["a", "b"], how="all")
        call check(error, t%nrows() == 3, "only row 3 is null in BOTH columns, so only it goes")
        if (allocated(error)) return
        call t%get("u", got)
        call check(error, all(got == [1, 2, 4]), "row 2, null in a but not in b, must survive how=all")
    end subroutine test_dropna_all

    !> `min_valid` counts how many named columns a row must have values in.
    subroutine test_dropna_min_valid(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropmin.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: a(4) = [1, 0, 0, 4], b(4) = [1, 2, 0, 4], c(4) = [1, 2, 3, 0]
        integer(int32) :: u(4) = [1, 2, 3, 4]
        logical :: va(4) = [.true., .false., .false., .true.]
        logical :: vb(4) = [.true., .true., .false., .true.]
        logical :: vc(4) = [.true., .true., .true., .false.]
        integer(int32), allocatable :: got(:)

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "a", a, is_valid=va)
        call parquet_write_column(writer, "b", b, is_valid=vb)
        call parquet_write_column(writer, "c", c, is_valid=vc)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        ! Valid counts per row: 3, 2, 1, 2. min_valid=2 keeps rows 1, 2 and 4.
        ! Read every column first: a drop detaches the table, and a column that was
        ! never read can never be read afterwards.
        call t%materialize_all()
        call t%dropna(["a", "b", "c"], min_valid=2)
        call check(error, t%nrows() == 3, "only row 3, with one value of three, falls short")
        if (allocated(error)) return
        call t%get("u", got)
        call check(error, all(got == [1, 2, 4]), "the wrong rows survived min_valid=2")
    end subroutine test_dropna_min_valid

    !> With no names, %dropna looks at the RESIDENT columns -- so on a table nothing has read it
    !> drops nothing, and after reading one column it drops that column's null rows.
    subroutine test_dropna_resident_only(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropres.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%dropna()
        call check(error, t%nrows() == 6, "nothing is resident, so there is nothing to test a row " // &
            "against and no row may be dropped")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "and dropping nothing must not detach the table")
        if (allocated(error)) return
        call t%prefetch("v")
        call t%dropna()
        call check(error, t%nrows() == 4, "with v resident, its two null rows go")
        if (allocated(error)) return
        call t%get("v", got)
        call check(error, all(got == [10, 20, 40, 60]), "the surviving values are wrong")
    end subroutine test_dropna_resident_only

    !> The negative control for the detach rule: a drop that drops nothing changes nothing.
    subroutine test_dropna_no_op_does_not_detach(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropnoop.parquet"
        type(parquet_table) :: t
        integer(int64) :: gen

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        gen = t%generation()
        call t%dropna(["u"])
        call check(error, t%nrows() == 6, "u has no nulls, so no row may be dropped")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "a drop that drops nothing must not detach")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "and must not advance the generation counter")
        if (allocated(error)) return
        ! Still attached, so a column nothing has read can still be read.
        call check(error, t%has_nulls("x"), "an unread column must still be readable afterwards")
    end subroutine test_dropna_no_op_does_not_detach

    !> ... and the positive half: dropping a row is row-structural, so the table detaches.
    subroutine test_dropna_detaches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropdetach.parquet"
        type(parquet_table) :: t
        integer(int64) :: gen

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        gen = t%generation()
        call t%dropna(["v"])
        call check(error, t%is_detached(), "dropping a row detaches the table from its file")
        if (allocated(error)) return
        call check(error, t%generation() > gen, "and advances the generation counter, since every " // &
            "column's storage was rebuilt")
    end subroutine test_dropna_detaches

    !> The separated-string name form.
    subroutine test_dropna_name_string(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropstr.parquet"
        type(parquet_table) :: t

        call write_gap_fixture(file)
        call parquet_open_table(t, file)
        call t%dropna("v, x")
        call check(error, t%nrows() == 4, "both named columns are null in rows 3 and 5")
    end subroutine test_dropna_name_string

    !> On a vector column a row counts as null when ANY of its elements is.
    subroutine test_dropna_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_dropvec.parquet"
        type(parquet_writer) :: writer
        type(parquet_table) :: t
        integer(int32) :: v(2, 4), u(4) = [1, 2, 3, 4]
        logical :: valid(2, 4)
        integer(int32), allocatable :: got(:)

        v = reshape([11, 21, 12, 22, 13, 23, 14, 24], [2, 4])
        valid = .true.
        valid(2, 3) = .false.
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "m", v, is_valid=valid)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
        call parquet_open_table(t, file)
        ! Read every column first: a drop detaches the table, and a column that was
        ! never read can never be read afterwards.
        call t%materialize_all()
        call t%dropna(["m"])
        call check(error, t%nrows() == 3, "row 3 has one null element, so the whole row goes")
        if (allocated(error)) return
        call t%get("u", got)
        call check(error, all(got == [1, 2, 4]), "the wrong rows survived")
    end subroutine test_dropna_vector

    !> A slice's resident columns hold the slice's rows and no others, so both verbs must agree
    !> with `%nrows()` rather than with the file's row count. A mismatch there is a shape error in
    !> the mask, which a plain build runs straight past.
    subroutine test_fill_on_a_slice(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        character(len=*), parameter :: file = "test_run/test_fill_slice.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)

        call write_gap_fixture(file)
        ! Rows 2..5 of the fixture, so exactly one of the two null rows (3 and 5) sits inside the
        ! slice at each end -- a slice that happened to contain neither would assert nothing.
        call parquet_open_table(t, file, 2_int64, 5_int64)
        call check(error, t%nrows() == 4, "the slice should hold four rows")
        if (allocated(error)) return
        call t%fillna("v", -2_int32)
        call t%get("v", got)
        call check(error, all(got == [20, -2, 40, -2]), "the fill must act on the slice's own rows")
        if (allocated(error)) return
        call t%materialize_all()
        call t%dropna(["x"])
        call check(error, t%nrows() == 2, "x is null in two of the slice's four rows")
        if (allocated(error)) return
        call t%get("u", got)
        call check(error, all(got == [2, 4]), "the surviving slice rows are wrong")
    end subroutine test_fill_on_a_slice

    !> `%fillna` is eighteen generated specifics -- nine value kinds times two name forms -- and a
    !> suite that calls six of them proves the generic RESOLVES for six. The compiler already
    !> proves the eighteen are distinguishable; what this adds is that each picks the specific it
    !> should, which a wrongly wired body or a mis-tagged generator row would break silently for
    !> the kinds nothing calls.
    !>
    !> Built in memory rather than from a file so that every kind can be an all-null column
    !> without needing a fixture that carries one.
    subroutine test_fillna_every_value_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_table) :: t
        type(parquet_date) :: dv(3), dfill
        type(parquet_time) :: tv(3), tfill
        type(parquet_timestamp) :: sv(3), sfill
        logical :: nul(3) = [.false., .false., .false.]
        integer(int64), allocatable :: g64(:)
        real(real32), allocatable :: g32(:)
        logical, allocatable :: gb(:)
        character(len=:), allocatable :: gs(:)
        type(parquet_time), allocatable :: gt(:)

        call parquet_new_table(t)
        call t%add_column("i64", [0_int64, 0_int64, 0_int64])
        call t%add_column("f32", [0.0_real32, 0.0_real32, 0.0_real32])
        call t%add_column("bl", [.false., .false., .false.])
        call t%add_column("st", ["   ", "   ", "   "])
        call t%add_column("dt", dv)
        call t%add_column("tm", tv)
        call t%add_column("ts", sv)
        ! The four bitmap-backed columns need their nulls set; the temporal ones are already null,
        ! since a default-initialized element is.
        call t%set_null("i64", nul)
        call t%set_null("f32", nul)
        call t%set_null("bl", nul)
        call t%set_null("st", nul)
        !
        ! The ARRAY name form for four kinds ...
        call t%fillna(["i64"], -7_int64)
        call t%fillna(["f32"], 1.5_real32)
        call t%fillna(["bl "], .true.)
        call t%fillna(["st "], "x")
        ! ... and the separated-string form for the three temporal ones.
        call dfill%set(2001, 2, 3)
        call tfill%set(4, 5, 6)
        call sfill%set(2001, 2, 3, 4, 5, 6)
        call t%fillna("dt", dfill)
        call t%fillna("tm", tfill)
        call t%fillna("ts", sfill)
        !
        call check(error, .not. (t%has_nulls("i64") .or. t%has_nulls("f32") .or. t%has_nulls("bl") &
            .or. t%has_nulls("st") .or. t%has_nulls("dt") .or. t%has_nulls("tm") &
            .or. t%has_nulls("ts")), "every value kind must clear its column's nulls")
        if (allocated(error)) return
        call t%get("i64", g64)
        call t%get("f32", g32)
        call t%get("bl", gb)
        call t%get("st", gs)
        call t%get("tm", gt)
        call check(error, all(g64 == -7_int64), "the int64 value reached the int64 column")
        if (allocated(error)) return
        call check(error, all(g32 == 1.5_real32), "the real32 value reached the float32 column")
        if (allocated(error)) return
        call check(error, all(gb), "the logical value reached the logical column")
        if (allocated(error)) return
        call check(error, trim(gs(1)) == "x" .and. trim(gs(3)) == "x", &
            "the character value reached the string column")
        if (allocated(error)) return
        call check(error, gt(2)%hour() == 4 .and. gt(2)%minute() == 5, &
            "the parquet_time value reached the time column")
    end subroutine test_fillna_every_value_kind
end module test_table_fill
