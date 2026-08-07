!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the internally-parallel per-column rewrite behind `parquet_table`'s
!> row-structural mutations -- `%sort_by`, `%filter_rows` and `%top_n`.
!>
!> The parallel and serial paths must produce an **identical table**. That is what makes the feature
!> safe, and it is also what makes it hard to test: every assertion about values, order and nulls
!> passes just as happily against an implementation that never threads at all. So every test here
!> does two things, and neither is sufficient alone:
!>
!> * **An A/B equality**, column by column, between a table mutated on several threads and an
!>   independent clone of it mutated with `parquet_set_table_threads(1)`. The forced-serial run is
!>   the oracle, and it is reached through the public knob rather than a debug hook -- so the same
!>   mechanism serves the equality test and the knob's own observed-effect test in
!>   `test/test_settings.f90`, and neither can pass while the other is broken.
!> * **An assertion that the parallel run really was parallel**, read back through the
!>   `parquet_debug_get_table_threads_used` hook. Without it, a gate that silently declined would
!>   pass every equality test ever written for this path -- and a parallel loop that skipped a
!>   column would leave that column unpermuted while every other column moved, which is a wrong
!>   answer with no abort, in the row-correspondence class this project guards hardest.
!>
!> **This suite runs its tests SEQUENTIALLY** (`test/run_tester.f90`'s
!> `suite_is_safe_to_parallelize` excludes it), and it cannot work any other way. test-drive runs a
!> suite's tests inside its own `!$omp parallel do`, and the mutation's thread count resolves to 1
!> inside an existing parallel region -- deliberately, since a nested region is the caller's
!> business. Run concurrently, every test here would compare the serial path against itself and pass
!> while testing nothing. The thread counter is process-global too, which is the second, independent
!> reason.
!>
!> **The fixtures must clear the work floor**, or the gate declines on size and the same vacuous
!> pass follows. The floor is on the largest column's element count (`rows * width`), so a width-8
!> vector column reaches it at 20000 rows and the whole fixture stays small and fast.
module test_table_parallel
    use parquet
    use iso_fortran_env, only : int32, int64, real64
    use iso_c_binding, only : c_int64_t
    use testdrive, only : new_unittest, unittest_type, error_type, check
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads
#endif
    !
    implicit none
    private
    public :: collect_tests_table_parallel
    !
    integer, parameter :: NROW = 20000 !! rows in every fixture; with VW below, clears the work floor.
    integer, parameter :: VW = 8       !! width of the vector column: 8*20000 = 160000 elements.
    !
    !> The test-only observation hook (`src/parquet_wrapper.cpp`). Declared locally here rather than
    !> in `src/parquet_bindings.f90` -- the same convention every other `parquet_debug_*` hook
    !> follows, so no debug entry point becomes part of the library's own interface.
    interface
        !> Threads the last row-structural table mutation resolved to; 1 when it ran serially.
        function parquet_debug_get_table_threads_used() result(res) &
            bind(C, name="parquet_debug_get_table_threads_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! resolved thread count of the last mutation.
        end function parquet_debug_get_table_threads_used
        !> Clears the counter, so a test observes its own mutation rather than an earlier one.
        subroutine parquet_debug_set_table_threads_used(n) &
            bind(C, name="parquet_debug_set_table_threads_used")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new counter value; tests use 0.
        end subroutine parquet_debug_set_table_threads_used
        !> Overrides the parallel gate's work floor, in elements. 0 or less restores the real one.
        subroutine parquet_debug_set_colwork_min_elements(n) &
            bind(C, name="parquet_debug_set_colwork_min_elements")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new floor, or 0 to restore.
        end subroutine parquet_debug_set_colwork_min_elements
        !> Overrides the parallel gate's minimum mutable-column count. 0 or less restores the real one.
        subroutine parquet_debug_set_colwork_min_columns(n) &
            bind(C, name="parquet_debug_set_colwork_min_columns")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new minimum, or 0 to restore.
        end subroutine parquet_debug_set_colwork_min_columns
    end interface
    !
contains
    !
    !> Collects the tests of this suite.
    subroutine collect_tests_table_parallel(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        !
        testsuite = [ &
            new_unittest("sort_by gives the same table on many threads as on one", &
                test_sort_by_parallel_equals_serial), &
            new_unittest("filter_rows gives the same table on many threads as on one", &
                test_filter_rows_parallel_equals_serial), &
            new_unittest("top_n gives the same table on many threads as on one", &
                test_top_n_parallel_equals_serial), &
            new_unittest("a mutation inside the caller's own parallel region stays serial", &
                test_mutation_in_caller_region_is_serial), &
            new_unittest("each gate limit can be overridden, and each one alone closes the gate", &
                test_gate_limit_overrides) &
            ]
    end subroutine collect_tests_table_parallel
    !
    ! ==================================================================================
    ! The three A/B equality tests
    ! ==================================================================================
    !
    !> `%sort_by`: the operation with the most to lose, because it is the one whose per-column loop
    !> has a validating first column hoisted out of the parallel path (`feature_risks.md` Risk-46).
    !> A hoist that lost the validation, or a loop that skipped the hoisted column, both show up
    !> here as a column out of step with its neighbours.
    subroutine test_sort_by_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: par, ser
        integer :: used
        !
        call build_fixture(par)
        call par%clone(ser)
        !
        call parquet_set_table_threads(1)
        call ser%sort_by(["k"])
        call parquet_reset_settings()
        !
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call par%sort_by(["k"])
        used = int(parquet_debug_get_table_threads_used())
        !
        call check_really_parallel(error, used, "sort_by")
        if (allocated(error)) return
        call check_tables_identical(error, par, ser, "sort_by")
        if (allocated(error)) return
        ! The independent oracle -- see check_rows_consistent for why the A/B above is not
        ! enough on its own.
        call check_rows_consistent(error, par, "sort_by")
    end subroutine test_sort_by_parallel_equals_serial
    !
    !> `%filter_rows`, which is also what `%delete_rows` and `%truncate` go through
    !> (`table_apply_keep`). Every column shrinks, so a skipped column leaves a table whose columns
    !> disagree about how many rows it has.
    subroutine test_filter_rows_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: par, ser
        logical :: keep(NROW)
        integer :: i, used
        !
        ! Irregular rather than "every other row", so a mask applied with an off-by-one or an
        ! inverted sense cannot coincide with the right answer.
        do i = 1, NROW
            keep(i) = mod(i, 3) /= 0 .and. i /= 7
        end do
        call build_fixture(par)
        call par%clone(ser)
        !
        call parquet_set_table_threads(1)
        call ser%filter_rows(keep)
        call parquet_reset_settings()
        !
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call par%filter_rows(keep)
        used = int(parquet_debug_get_table_threads_used())
        !
        call check_really_parallel(error, used, "filter_rows")
        if (allocated(error)) return
        call check_tables_identical(error, par, ser, "filter_rows")
        if (allocated(error)) return
        ! The independent oracle -- see check_rows_consistent for why the A/B above is not
        ! enough on its own.
        call check_rows_consistent(error, par, "filter_rows")
    end subroutine test_filter_rows_parallel_equals_serial
    !
    !> `%top_n`, whose per-column work is a `%gather` of `n` rows rather than a rewrite of all of
    !> them -- so it reaches the same worker by a different route and with a much smaller payload.
    !> `n` is well below the row count, or `%top_n` delegates to `%sort_by` and this would be a
    !> second copy of the test above.
    subroutine test_top_n_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: par, ser
        integer :: used
        !
        call build_fixture(par)
        call par%clone(ser)
        !
        call parquet_set_table_threads(1)
        call ser%top_n(["k"], 500)
        call parquet_reset_settings()
        !
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call par%top_n(["k"], 500)
        used = int(parquet_debug_get_table_threads_used())
        !
        call check_really_parallel(error, used, "top_n")
        if (allocated(error)) return
        call check_tables_identical(error, par, ser, "top_n")
        if (allocated(error)) return
        ! The independent oracle -- see check_rows_consistent for why the A/B above is not
        ! enough on its own.
        call check_rows_consistent(error, par, "top_n")
    end subroutine test_top_n_parallel_equals_serial
    !
    !> The auto-threading rule, from the caller's side: a table mutated inside the caller's OWN
    !> parallel region rewrites its columns serially, because a nested region is the caller's
    !> business and T threads each asking for T more is slower than not threading at all.
    !>
    !> **This is a NEGATIVE control for the gate as well as a rule in its own right.** The three
    !> tests above establish that the parallel path engages; this one establishes that it declines
    !> where it must, so a gate stuck open would fail here and a gate stuck shut would fail there.
    !>
    !> The table is declared inside a `block` in the loop body, never `private()`d -- CLAUDE.md's
    !> rule for a finalizable type, and `parquet_table` is the one such type this library exposes
    !> that is safe in a block, because it deliberately has no allocatable components.
    subroutine test_mutation_in_caller_region_is_serial(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: r, used, avail
        integer(int32) :: k(NROW)
        real(real64) :: v(VW, NROW)
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call fill_key(k)
        call fill_vector(v)
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        !$omp parallel do default(shared) private(r) num_threads(min(avail, 2))
        do r = 1, 2
            block
                ! A table THIS thread creates inside the region is thread-private, so mutating it
                ! is permitted -- the guards key on ownership, not on being in a region at all.
                type(parquet_table) :: mine
                !
                call parquet_new_table(mine)
                call mine%add_column("k", k)
                call mine%add_column("fv", v)
                call mine%sort_by(["k"])
            end block
        end do
        !$omp end parallel do
        used = int(parquet_debug_get_table_threads_used())
        !
        ! Every thread writes the same 1, so the shared counter is unambiguous however the writes
        ! interleave. A 0 here would mean the mutation never reached the loop at all.
        call check(error, used == 1, &
            "a mutation inside the caller's own parallel region must rewrite its columns serially")
    end subroutine test_mutation_in_caller_region_is_serial
    !
    !> The two test-only overrides of the gate's tuning constants (`colwork_gate_limits`,
    !> `src/parquet_tables_parallel.f90`).
    !>
    !> **A round trip would prove nothing here — the overrides have no getter to round-trip against,
    !> and their only observable is whether the gate changes its mind.** So each limit is exercised
    !> in three steps: confirm the gate is open at the real constants, close it with that one
    !> override alone, and confirm it opens again when the override is cleared. The middle step is
    !> what the override is for; the first and third are what stop a hook that does nothing (or one
    !> that never restores) from passing.
    !>
    !> **Each limit is raised on its own, never both at once.** `colwork_threads` returns 1 if *any*
    !> gate closes, so overriding both together would pass identically against an implementation that
    !> read only one of them — which is exactly the defect two separate overrides could introduce.
    !>
    !> The answer is checked after every mutation, not only the thread count: an override that
    !> accidentally changed the *result* rather than only the schedule would otherwise go unnoticed.
    subroutine test_gate_limit_overrides(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        integer :: used, avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        ! With one thread available the gate's answer is 1 whatever the limits say, so there is no
        ! observable to assert on and the test would be vacuous rather than failing.
        if (avail <= 1) return
        !
        ! (1) The gate is open at the real constants. Without this the two "closed" assertions below
        !     would pass against a gate that was never open in the first place.
        call run_sort(t, used)
        call check(error, used > 1, "the gate must be open at the real constants")
        if (allocated(error)) return
        call check_rows_consistent(error, t, "sort_by at the real constants")
        if (allocated(error)) return
        !
        ! (2) The column minimum alone closes it. The fixture has 5 columns, so 6 excludes it.
        call parquet_debug_set_colwork_min_columns(6_c_int64_t)
        call run_sort(t, used)
        call parquet_debug_set_colwork_min_columns(0_c_int64_t)
        call check(error, used == 1, "raising the minimum column count above the fixture's must close the gate")
        if (allocated(error)) return
        call check_rows_consistent(error, t, "sort_by under a raised column minimum")
        if (allocated(error)) return
        !
        ! (3) The work floor alone closes it. The largest column is VW*NROW elements, so one more
        !     than that excludes it -- and only just, so a floor compared with the wrong operator
        !     or against the wrong column shows up here.
        call parquet_debug_set_colwork_min_elements(int(VW, c_int64_t) * int(NROW, c_int64_t) + 1_c_int64_t)
        call run_sort(t, used)
        call parquet_debug_set_colwork_min_elements(0_c_int64_t)
        call check(error, used == 1, "raising the work floor above the largest column must close the gate")
        if (allocated(error)) return
        call check_rows_consistent(error, t, "sort_by under a raised work floor")
        if (allocated(error)) return
        !
        ! (4) Both overrides cleared, the gate is open again. This is what proves the restore path:
        !     without it a hook that latched would leave every later test in this process serial,
        !     and they would all still pass.
        call run_sort(t, used)
        call check(error, used > 1, "clearing both overrides must reopen the gate")
    end subroutine test_gate_limit_overrides
    !
    !> Builds a fresh fixture, sorts it, and reports the thread count that mutation resolved to.
    !>
    !> Fresh each time because `%sort_by` returns early without touching a column when the rows are
    !> already in order, so re-sorting an already-sorted table would report a thread count for a
    !> mutation that never happened.
    subroutine run_sort(t, used)
        type(parquet_table), intent(out) :: t !! receives the sorted fixture.
        integer, intent(out) :: used          !! threads the mutation resolved to.
        !
        call build_fixture(t)
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call t%sort_by(["k"])
        used = int(parquet_debug_get_table_threads_used())
    end subroutine run_sort
    !
    ! ==================================================================================
    ! Fixture and comparison helpers
    ! ==================================================================================
    !
    !> Builds the fixture every equality test uses: five columns of three dispatch classes, with
    !> nulls, sized to clear the parallel path's work floor.
    !>
    !> Each part earns its place:
    !>
    !> * **`fv`, a width-8 `float64` vector column**, is what clears the work floor at only 20000
    !>   rows (the floor counts `rows * width`), and it carries per-ELEMENT nulls, which is a
    !>   different validity dispatch class from a scalar column's.
    !> * **`s`, a string column**, is the expensive and structurally different one: its rewrite
    !>   rebuilds a whole packed payload rather than gathering fixed-width slots, which is exactly
    !>   the imbalance `schedule(dynamic)` exists for. It is filled through `%append_string` rather
    !>   than `%add_column` over a `character` array, because the latter sets one element at a time
    !>   and every set rewrites the offsets of all later elements.
    !> * **`a` carries scalar nulls**, so a rewrite that moved values without moving the validity
    !>   bitmap with them shows up.
    !> * **`k` is scattered**, so the sort permutation actually moves rows -- `%sort_by` returns
    !>   early without touching a column when the rows are already in order.
    subroutine build_fixture(t)
        type(parquet_table), intent(out) :: t !! receives the fixture.
        type(parquet_string_column) :: sc
        integer(int32) :: k(NROW), n(NROW)
        real(real64) :: a(NROW), v(VW, NROW)
        logical :: avalid(NROW), vvalid(VW, NROW)
        character(len=12) :: buf
        integer :: i, e
        !
        call fill_key(k)
        call fill_vector(v)
        do i = 1, NROW
            n(i) = i
            a(i) = real(i, real64) * 0.5_real64
            avalid(i) = mod(i, 11) /= 0
            do e = 1, VW
                vvalid(e, i) = .not. (mod(i, 13) == 0 .and. e == 3)
            end do
        end do
        !
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("n", n)
        call t%add_column("a", a)
        call t%add_column("fv", v)
        ! Nulls are applied after the values: %add_column takes no is_valid=, and %set_null(mask)
        ! reads a VALIDITY mask -- a .false. entry becomes a null.
        call t%set_null("a", avalid)
        call t%set_null("fv", vvalid)
        !
        call sc%reserve(int(NROW, int64), int(NROW, int64) * 12_int64)
        do i = 1, NROW
            write(buf, '(i12.12)') NROW - i
            call sc%append_string(buf)
        end do
        call t%add_column("s", sc)
        call sc%clear()
    end subroutine build_fixture
    !
    !> A scattered integer key: every row distinct, no row in its own place.
    subroutine fill_key(k)
        integer(int32), intent(out) :: k(:) !! receives the key.
        integer :: i
        !
        do i = 1, size(k)
            k(i) = key_for(i, size(k))
        end do
    end subroutine fill_key
    !
    !> Fills the vector column's values, distinct per (element, row).
    subroutine fill_vector(v)
        real(real64), intent(out) :: v(:,:) !! receives the values.
        integer :: i, e
        !
        do i = 1, size(v, 2)
            do e = 1, size(v, 1)
                v(e, i) = real(100*i + e, real64)
            end do
        end do
    end subroutine fill_vector
    !
    !> Asserts the run under test really used more than one thread.
    !>
    !> Skipped when OpenMP offers only one thread, which is the one case where the serial answer is
    !> also the correct one. Everywhere else this is what stops the equality assertion that follows
    !> from being vacuous.
    subroutine check_really_parallel(error, used, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer, intent(in) :: used                         !! threads the mutation reported.
        character(len=*), intent(in) :: what                !! operation name, for the message.
        integer :: avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        if (avail <= 1) return
        call check(error, used > 1, &
            "%" // what // " must rewrite its columns in parallel here, or the equality below " // &
            "compares the serial path against itself")
    end subroutine check_really_parallel
    !
    !> Asserts every column is still in step with every other, using the fixture's own construction
    !> as an independent oracle.
    !>
    !> **This is the check the A/B equality cannot make, and the reason both exist.** `check_tables_identical`
    !> compares the parallel path against the serial one — so it catches anything the parallel path
    !> does differently, and **nothing** the two share. A defect in the code above the split (the
    !> hoisted first column rewritten twice, the slot list built wrongly) breaks both paths
    !> identically and the comparison still holds. That is not hypothetical: handing the whole slot
    !> list to the worker instead of `slots(2:)`, so the sort key is permuted twice while every other
    !> column is permuted once, passed every A/B assertion here before this check existed.
    !>
    !> Every column of the fixture is a pure function of `n`, the original row number, values and
    !> nulls alike. So whatever the mutation did to the row set, each surviving row must still
    !> satisfy those functions — and a column that moved differently from its neighbours cannot.
    subroutine check_rows_consistent(error, t, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table), intent(inout) :: t             !! the mutated table.
        character(len=*), intent(in) :: what                !! operation name, for the messages.
        integer(int32), allocatable :: n(:), k(:)
        real(real64), allocatable :: a(:), v(:,:)
        logical, allocatable :: am(:), vm(:,:)
        character(len=:), allocatable :: s(:)
        character(len=12) :: buf
        integer :: i, e, row
        !
        call t%get("n", n)
        call t%get("k", k)
        call t%get("a", a, is_valid=am)
        call t%get("fv", v, is_valid=vm)
        call t%get("s", s)
        do i = 1, size(n)
            row = int(n(i))
            call check(error, k(i) == key_for(row, NROW), &
                "%" // what // ": the key column is out of step with the row identity")
            if (allocated(error)) return
            call check(error, am(i) .eqv. (mod(row, 11) /= 0), &
                "%" // what // ": the scalar column's nulls are out of step with the row identity")
            if (allocated(error)) return
            ! Only where valid: a null row's stored value is unspecified by parquet_column's own
            ! contract, so asserting on it would be testing an implementation detail.
            if (am(i)) then
                call check(error, a(i) == real(row, real64) * 0.5_real64, &
                    "%" // what // ": the scalar column is out of step with the row identity")
                if (allocated(error)) return
            end if
            do e = 1, VW
                call check(error, vm(e, i) .eqv. .not. (mod(row, 13) == 0 .and. e == 3), &
                    "%" // what // ": the vector column's element nulls are out of step with the row identity")
                if (allocated(error)) return
                if (vm(e, i)) then
                    call check(error, v(e, i) == real(100*row + e, real64), &
                        "%" // what // ": the vector column is out of step with the row identity")
                    if (allocated(error)) return
                end if
            end do
            write(buf, '(i12.12)') NROW - row
            call check(error, s(i) == buf, &
                "%" // what // ": the string column is out of step with the row identity")
            if (allocated(error)) return
        end do
    end subroutine check_rows_consistent
    !
    !> The fixture's key as a function of the row number -- the one place that formula lives, so the
    !> fixture and the oracle cannot drift apart.
    pure integer(int32) function key_for(row, nrow) result(k)
        integer, intent(in) :: row  !! 1-based original row number.
        integer, intent(in) :: nrow !! rows in the fixture.
        !
        k = int(mod(int(row, int64) * 7919_int64, int(nrow, int64)), int32)
    end function key_for
    !
    !> Asserts two tables hold exactly the same rows, column by column, values and nulls alike.
    !>
    !> Deliberately compares **every column**, not a sample: the failure this exists to catch is one
    !> column left out of step with the others, so a comparison that checked only the sort key would
    !> miss precisely the defect. The vector column is compared per element and the string column
    !> per row, since those are their true validity granularities.
    subroutine check_tables_identical(error, got, want, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table), intent(inout) :: got           !! the table mutated in parallel.
        type(parquet_table), intent(inout) :: want          !! the reference, mutated serially.
        character(len=*), intent(in) :: what                !! operation name, for the messages.
        integer(int32), allocatable :: gk(:), wk(:), gn(:), wn(:)
        real(real64), allocatable :: ga(:), wa(:), gv(:,:), wv(:,:)
        logical, allocatable :: gm(:), wm(:), gme(:,:), wme(:,:)
        character(len=:), allocatable :: gs(:), ws(:)
        !
        call check(error, got%nrows() == want%nrows(), &
            "%" // what // " must leave the same row count on both paths")
        if (allocated(error)) return
        !
        call got%get("k", gk)
        call want%get("k", wk)
        call check(error, all(gk == wk), "%" // what // ": the key column must match the serial result")
        if (allocated(error)) return
        !
        ! The column that says whether the ROWS travelled together: `n` is the original row number,
        ! so comparing it proves the two paths kept the same rows in the same order.
        call got%get("n", gn)
        call want%get("n", wn)
        call check(error, all(gn == wn), &
            "%" // what // ": the row-identity column must match the serial result")
        if (allocated(error)) return
        !
        call got%get("a", ga, is_valid=gm)
        call want%get("a", wa, is_valid=wm)
        call check(error, all(ga == wa), "%" // what // ": the scalar column must match the serial result")
        if (allocated(error)) return
        call check(error, all(gm .eqv. wm), &
            "%" // what // ": the scalar column's nulls must travel with its values")
        if (allocated(error)) return
        !
        call got%get("fv", gv, is_valid=gme)
        call want%get("fv", wv, is_valid=wme)
        call check(error, all(gv == wv), "%" // what // ": the vector column must match the serial result")
        if (allocated(error)) return
        call check(error, all(gme .eqv. wme), &
            "%" // what // ": the vector column's per-element nulls must travel with its values")
        if (allocated(error)) return
        !
        call got%get("s", gs)
        call want%get("s", ws)
        call check(error, size(gs) == size(ws), &
            "%" // what // ": the string column must have the same length on both paths")
        if (allocated(error)) return
        call check(error, all(gs == ws), "%" // what // ": the string column must match the serial result")
    end subroutine check_tables_identical
    !
end module test_table_parallel
