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
    ! parquet_validity_block_bits is no longer re-exported by the facade (row 30 hid it: it is
    ! published for parquet_tables' benefit, not for a user's). This file needs it because its
    ! whole subject is the read-modify-write on one bitmap block, so it takes it from the module
    ! that owns it -- which is the more accurate import in any case.
    use parquet_columns, only : parquet_validity_block_bits
    use iso_fortran_env, only : int32, int64, real64
    use iso_c_binding, only : c_int64_t
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_threads, omp_get_thread_num, omp_get_wtime, &
        omp_get_num_procs
#endif
    !
    implicit none
    private
    public :: collect_tests_table_parallel
    !
    integer, parameter :: NROW = 20000 !! rows in every fixture; with VW below, clears the work floor.
    integer, parameter :: VW = 8       !! width of the vector column: 8*20000 = 160000 elements.
    !> Rows in the on-disk fixture. Scalar float64 columns, so the row count alone has to clear the
    !! work floor (131072 elements) -- a lazy clone has no vector column to lean on.
    integer, parameter :: FROWS = 200000
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
        !> The level the last mutation spent its team at: 0 serial, 1 across columns, 2 within one.
        function parquet_debug_get_table_level_used() result(res) &
            bind(C, name="parquet_debug_get_table_level_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! the level of the last mutation.
        end function parquet_debug_get_table_level_used
        !> Clears the level record, so a test observes its own mutation rather than an earlier one.
        subroutine parquet_debug_set_table_level_used(n) &
            bind(C, name="parquet_debug_set_table_level_used")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new record value; tests use a value no mutation writes.
        end subroutine parquet_debug_set_table_level_used
        !> Forces the level a gather spends its team at: 0 automatic, 1 across columns, 2 within.
        subroutine parquet_debug_set_colwork_level(mode) &
            bind(C, name="parquet_debug_set_colwork_level")
            import :: c_int64_t
            integer(c_int64_t), value :: mode !! 0, 1 or 2.
        end subroutine parquet_debug_set_colwork_level
        !> The team the sort engine's passes over the runs ran on for the last join; 0 after a
        !! hash-engine join, which has no such passes.
        function parquet_debug_get_join_group_threads_used() result(res) &
            bind(C, name="parquet_debug_get_join_group_threads_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! the team; 1 means serial, 0 the hash engine.
        end function parquet_debug_get_join_group_threads_used
        !> The team the last join's two side-index passes ran on, whatever its engine.
        function parquet_debug_get_join_side_threads_used() result(res) &
            bind(C, name="parquet_debug_get_join_side_threads_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! the team; 1 means serial.
        end function parquet_debug_get_join_side_threads_used
        !> The team the last per-group callback loop of a `parquet_grouping` (`%apply`) ran on.
        function parquet_debug_get_group_threads_used() result(res) &
            bind(C, name="parquet_debug_get_group_threads_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! the team; 1 means serial.
        end function parquet_debug_get_group_threads_used
        !> Overwrites that record, so a test can tell a loop that wrote it from one that did not.
        subroutine parquet_debug_set_group_threads_used(n) &
            bind(C, name="parquet_debug_set_group_threads_used")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! the value to plant.
        end subroutine parquet_debug_set_group_threads_used
        !> Forces the join's pair-list engine: 0 automatic, 1 the sort engine, 2 the hash engine.
        subroutine parquet_debug_set_join_engine(mode) &
            bind(C, name="parquet_debug_set_join_engine")
            import :: c_int64_t
            integer(c_int64_t), value :: mode !! 0, 1 or 2.
        end subroutine parquet_debug_set_join_engine
        !> Threads the last internally-parallel PREFETCH was given; 0 when it ran serially.
        function parquet_debug_get_prefetch_threads_used() result(res) &
            bind(C, name="parquet_debug_get_prefetch_threads_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! resolved thread count of the last prefetch.
        end function parquet_debug_get_prefetch_threads_used
        !> Clears the prefetch counter, so a test observes its own prefetch rather than an earlier one.
        subroutine parquet_debug_set_prefetch_threads_used(n) &
            bind(C, name="parquet_debug_set_prefetch_threads_used")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new counter value; tests use 0.
        end subroutine parquet_debug_set_prefetch_threads_used
        !> Threads the last SINGLE-COLUMN row-group-split read was given; 0 when it stayed whole.
        function parquet_debug_get_colread_threads_used() result(res) &
            bind(C, name="parquet_debug_get_colread_threads_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! resolved thread count of the last single-column read.
        end function parquet_debug_get_colread_threads_used
        !> Clears that counter, so a test observes its own read rather than an earlier one.
        subroutine parquet_debug_set_colread_threads_used(n) &
            bind(C, name="parquet_debug_set_colread_threads_used")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new counter value; tests use 0.
        end subroutine parquet_debug_set_colread_threads_used
        !> Overrides that read's work floor, in elements. 0 or less restores the real one.
        subroutine parquet_debug_set_colread_min_elements(n) &
            bind(C, name="parquet_debug_set_colread_min_elements")
            import :: c_int64_t
            integer(c_int64_t), value :: n !! new floor, or 0 to restore.
        end subroutine parquet_debug_set_colread_min_elements
    end interface
    !
    !> Context for `%apply`'s and `%add_apply`'s procedure-form callbacks in
    !! `test_apply_group_team` and `test_add_apply_group_team`: a module
    !! variable, because a callback must be a module procedure (an internal one crashes under
    !! one supported compiler; .claude/rules/fortran-gotchas.md, flang). Read on every thread of
    !! the team, which is what an explicit threads= declares safe. This suite runs its tests
    !! SEQUENTIALLY (see the header), so the two tests cannot be in it at once.
    real(real64), pointer :: par_apply_p(:) => null() !! the fixture's payload column.
    !
    !> The object form's context, in a component: `out(1)` is the payload's sum over the group,
    !! `out(2)`, when there is room, the group's size.
    type, extends(parquet_group_reducer) :: par_sum_reducer
        real(real64), pointer :: p(:) => null() !! the payload column.
    contains
        procedure :: reduce => par_sum_reduce !! See the type.
    end type par_sum_reducer
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
                test_gate_limit_overrides), &
            new_unittest("clone gives the same table on many threads as on one", &
                test_clone_parallel_equals_serial), &
            new_unittest("clone leaves an unread column unread, on either path", &
                test_clone_keeps_lazy_columns_unread), &
            new_unittest("a filtered table prefetches in parallel and agrees with the serial read", &
                test_prefetch_filter_parallel_equals_serial), &
            new_unittest("an UNSEEDED sample prefetches in parallel with every column on one sample", &
                test_prefetch_sample_unseeded_columns_agree), &
            new_unittest("sample_seed=0_int64 is treated as unseeded and is equally safe in parallel", &
                test_prefetch_sample_seed_zero_columns_agree), &
            new_unittest("a masked slice prefetches in parallel with every column on one row set", &
                test_prefetch_masked_slice_columns_agree), &
            new_unittest("a qc schema prefetches in parallel and agrees with the serial read", &
                test_prefetch_qc_parallel_equals_serial), &
            new_unittest("a soft qc violation warns once per column, on either path", &
                test_prefetch_qc_soft_warns_per_column), &
            new_unittest("a sorted table prefetches in parallel, sharing one permutation", &
                test_prefetch_sort_parallel_equals_serial), &
            new_unittest("one column's read splits across row groups and matches the whole read", &
                test_colread_split_equals_whole), &
            new_unittest("nulls land in the right rows when one column's read is split", &
                test_colread_nulls_survive_the_split), &
            new_unittest("a row group too short to hold a whole validity block is pasted serially", &
                test_colread_short_row_groups), &
            new_unittest("a vector column's read splits with every element in place", &
                test_colread_vector_column_splits), &
            new_unittest("a string column's read is not split, and is still correct", &
                test_colread_string_stays_whole), &
            new_unittest("the single-column work floor opens and closes the split", &
                test_colread_floor_override), &
            new_unittest("a sampled single-column read splits onto one sample", &
                test_colread_sampled_agrees), &
            new_unittest("no two row groups' pastes share a validity block", &
                test_colread_block_alignment), &
            new_unittest("concurrent set_null keeps every null when rows share a validity block", &
                test_concurrent_set_null_shares_block), &
            new_unittest("concurrent clear_null and value writes keep every clear when rows share a block", &
                test_concurrent_clear_null_shares_block), &
            new_unittest("has_nulls still reports a null a concurrent writer set on a date column", &
                test_has_nulls_survives_concurrent_null), &
            new_unittest("a join's threads= reaches the sort and not the column rewrite", &
                test_join_thread_split), &
            new_unittest("a join's group passes and its side index run on the join's team", &
                test_join_group_passes_threads), &
            new_unittest("a grouping's apply is serial unless asked, opens the team asked for, and agrees", &
                test_apply_group_team), &
            new_unittest("a grouping's agg threads automatically, honours the cap and the request, and agrees", &
                test_agg_group_team), &
            new_unittest("a grouping's add_apply forwards threads= to the same loop and agrees with serial", &
                test_add_apply_group_team), &
            new_unittest("a grouping's broadcast threads automatically, honours the cap and the request, and agrees", &
                test_broadcast_group_team), &
            new_unittest("a gather's team goes inside the column when the columns are fewer than the threads", &
                test_colwork_level_rule), &
            new_unittest("bounded= reads correctly through both parallel paths", &
                test_bounded_parallel_paths) &
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
    !> `%clone`, which reaches the same machinery by a different route: it is the only caller that
    !> copies BETWEEN two column stores, so it has its own worker (`table_colwork_clone`) rather than
    !> a `PCW_*` op code.
    !>
    !> A clone's failure mode is the mirror of a mutation's. A mutation that skipped a column leaves
    !> that column out of step with its neighbours; a clone that skipped one leaves the destination
    !> holding a column the source does not have — or, worse, leaves the two stores' slots crossed,
    !> which `check_rows_consistent` is what catches.
    subroutine test_clone_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: src, par, ser
        integer :: used
        !
        call build_fixture(src)
        !
        call parquet_set_table_threads(1)
        call src%clone(ser)
        call parquet_reset_settings()
        !
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call src%clone(par)
        used = int(parquet_debug_get_table_threads_used())
        !
        call check_really_parallel(error, used, "clone")
        if (allocated(error)) return
        call check_tables_identical(error, par, ser, "clone")
        if (allocated(error)) return
        ! The independent oracle -- see check_rows_consistent for why the A/B above is not
        ! enough on its own.
        call check_rows_consistent(error, par, "clone")
        if (allocated(error)) return
        ! The source must be untouched: the worker reads it and writes only the destination, which
        ! is what makes cloning a shared table safe to thread at all (colwork_avail's own note).
        call check_rows_consistent(error, src, "clone (source)")
    end subroutine test_clone_parallel_equals_serial
    !
    !> A clone copies what the source actually HOLDS, not what its file contains — so a column that
    !> was never read must stay unread in the clone, and the parallel path must not quietly read it.
    !>
    !> **This is the clone-specific failure the equality test cannot see, and the reason the
    !> prefetched columns are the ODD-NUMBERED ones.** With every column resident the slot list is
    !> `1..n`, so `dst%cols(j)` and `dst%cols(slots(j))` are the same thing and a worker that
    !> indexed the destination by loop counter instead of by slot would be indistinguishable from a
    !> correct one — in the equality test *and* here. Reading columns `a`, `c`, `e` makes the slot
    !> list `[1, 3, 5]`, so the two indexings disagree from the second iteration onward, and a
    !> crossed pairing lands column `e`'s values in column `c`.
    subroutine test_clone_keeps_lazy_columns_unread(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_clone_lazy.parquet"
        type(parquet_table) :: t, c
        real(real64), allocatable :: got(:)
        integer :: used
        !
        call write_wide_fixture(f)
        call parquet_open_table(t, f)
        ! Slots 1, 3, 5 of 5: enough resident columns to clear the gate's minimum, two left unread
        ! for the residency assertions, and a non-contiguous slot list per the note above.
        call t%prefetch("a")
        call t%prefetch("c")
        call t%prefetch("e")
        !
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call t%clone(c)
        used = int(parquet_debug_get_table_threads_used())
        call check_really_parallel(error, used, "clone of a lazy table")
        if (allocated(error)) return
        !
        call check(error, c%residency("a") == RES_FULL .and. c%residency("c") == RES_FULL .and. &
            c%residency("e") == RES_FULL, "a clone must hold every column the source held")
        if (allocated(error)) return
        call check(error, c%residency("b") == RES_EMPTY .and. c%residency("d") == RES_EMPTY, &
            "a clone must leave unread what the source had not read")
        if (allocated(error)) return
        ! Values, not just presence: column c is written as 3*i, so a crossed slot pairing shows up
        ! here as column e's 5*i.
        call c%get("c", got)
        call check(error, size(got) == FROWS, "the clone's copied column must keep its row count")
        if (allocated(error)) return
        call check(error, abs(got(1) - 3.0_real64) < 1.0e-12_real64 .and. &
            abs(got(FROWS) - 3.0_real64 * real(FROWS, real64)) < 1.0e-12_real64, &
            "the clone's copied column must hold ITS OWN values, not another slot's")
        if (allocated(error)) return
        call c%get("e", got)
        call check(error, abs(got(FROWS) - 5.0_real64 * real(FROWS, real64)) < 1.0e-12_real64, &
            "the clone's last copied column must hold its own values")
        if (allocated(error)) return
        ! An unread column must still be readable afterwards, through the clone's own reader.
        call c%get("d", got)
        call check(error, size(got) == FROWS .and. abs(got(FROWS) - 4.0_real64 * real(FROWS, real64)) &
            < 1.0e-12_real64, "a clone must still be able to read the columns it left unread")
    end subroutine test_clone_keeps_lazy_columns_unread
    !
    ! ==================================================================================
    ! P4 -- the widened prefetch gate
    ! ==================================================================================
    !
    !> A `filter=` table now prefetches on several threads, and this asserts the answer is unchanged.
    !>
    !> **It used to assert the refusal, and the change of subject is the milestone.** The clause was
    !> closed on measured cost -- every per-thread reader re-decoded the filter's key columns and
    !> rebuilt the row mask, at 0.65x-1.15x depending on how many columns the filter named, against
    !> 4.5x for an unfiltered read. Additional readers now ADOPT the table's own mask
    !> (`parquet_reader_adopt_transform`), which is a refcount increment on an immutable Arrow array,
    !> so there is nothing left to rebuild and nothing left to trade off.
    !>
    !> Correctness was never the question -- a filter is a pure function of the file, so every reader
    !> computed the same mask even when each built its own. What sharing changes is that they can no
    !> longer differ *at all*, which is a stronger statement than this test is able to make.
    subroutine test_prefetch_filter_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_filter.parquet"
        type(parquet_table) :: par, ser
        type(parquet_filter) :: filt
        real(real64), allocatable :: pa(:), sa(:), pe(:), se(:)
        integer :: used
        !
        call write_wide_fixture(f)
        call filt%add("a > 100000")
        !
        call parquet_reset_settings()
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        call parquet_open_table(par, f, filter=filt)
        call par%materialize_all()
        used = int(parquet_debug_get_prefetch_threads_used())
        call check_prefetch_really_parallel(error, used, "a filtered table")
        if (allocated(error)) return
        !
        call parquet_set_prefetch_threads(1)
        call parquet_open_table(ser, f, filter=filt)
        call ser%materialize_all()
        call parquet_reset_settings()
        !
        call check(error, par%nrows() == ser%nrows() .and. par%nrows() == int(FROWS / 2, int64), &
            "the parallel and serial reads of a filtered table disagreed on the row count")
        if (allocated(error)) return
        call par%get("a", pa)
        call ser%get("a", sa)
        call par%get("e", pe)
        call ser%get("e", se)
        call check(error, all(abs(pa - sa) < 1.0e-9_real64), &
            "a filtered table's first column differed between the parallel and serial prefetch")
        if (allocated(error)) return
        ! The last column too: the first is likeliest to be read by thread 0 either way, so a reader
        ! that had somehow lost the mask would show up in a later column, not this one.
        call check(error, all(abs(pe - se) < 1.0e-9_real64), &
            "a filtered table's last column differed between the parallel and serial prefetch")
        if (allocated(error)) return
        ! And the columns must be in step with EACH OTHER, which the A/B comparison above cannot
        ! see: two readers agreeing on a wrong mask would satisfy it (feature_risks.md Risk-52).
        call check(error, all(abs(pe - 5.0_real64 * pa) < 1.0e-9_real64), &
            "two columns of a filtered table came from different row sets")
    end subroutine test_prefetch_filter_parallel_equals_serial
    !
    !> **The load-bearing test of P4.**
    !>
    !> A table opened with `sample_fraction=` and NO `sample_seed=` now prefetches in parallel. That
    !> is only safe because `parquet_open_table` settles a seed before any reader exists
    !> (`feature_risks.md` Risk-55), so every per-thread reader draws the identical rows. A
    !> per-thread reader opened without that seed -- a bare `parquet_open_reader`, as this path used
    !> to do -- gives each column its own random subset, and every column still looks perfectly
    !> ordinary on its own.
    !>
    !> **The assertion is that the columns are in step with EACH OTHER**, not a row count and not an
    !> A/B equality. The fixture writes column *c* as `c*i`, so `e == 5*a` holds for any set of rows
    !> and fails the moment two columns come from two different draws.
    !>
    !> Honest about what usually catches it: a redraw normally makes two columns differ in LENGTH,
    !> and `parquet_check_read_row_count` aborts on that first -- confirmed by mutation, where five
    !> per-thread readers reported 100191/99806/100031/99689/99728 rows. The identity above is the
    !> defence for the remaining case, two draws that coincide in count while holding different
    !> rows, which is the one that would otherwise pass in silence. Its own teeth were confirmed
    !> separately, by perturbing the fixture so `e /= 5*a` and watching this test fail.
    subroutine test_prefetch_sample_unseeded_columns_agree(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_sample.parquet"
        !
        call check_sampled_prefetch_agrees(error, f, use_zero_seed=.false.)
    end subroutine test_prefetch_sample_unseeded_columns_agree
    !
    !> `sample_seed=0_int64` is `parquet_open_reader`'s own spelling of "draw a fresh seed", so it is the
    !> unseeded case wearing an explicit argument -- and it is the one a gate written to test
    !> `allocated(read_sample_seed)` alone would have admitted while every reader drew separately.
    !> N4 resolves it to a real positive seed at open, so it is exactly as safe as the test above;
    !> this asserts that rather than assuming the two spellings travel the same path.
    subroutine test_prefetch_sample_seed_zero_columns_agree(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_seed0.parquet"
        !
        call check_sampled_prefetch_agrees(error, f, use_zero_seed=.true.)
    end subroutine test_prefetch_sample_seed_zero_columns_agree
    !
    !> Shared body of the two sampled-prefetch tests; `use_zero_seed` picks which spelling of
    !> "unseeded" the table is opened with. Its own fixture file per caller, since the suite's
    !> siblings may run concurrently with other suites.
    subroutine check_sampled_prefetch_agrees(error, f, use_zero_seed)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), intent(in) :: f                   !! fixture path, one per caller.
        logical, intent(in) :: use_zero_seed                !! .true. passes sample_seed=0_int64 explicitly.
        type(parquet_table) :: t
        real(real64), allocatable :: a(:), c(:), e(:)
        integer :: used
        !
        call write_wide_fixture(f)
        call parquet_reset_settings()
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        if (use_zero_seed) then
            call parquet_open_table(t, f, sample_fraction=0.5_real64, sample_seed=0_int64)
        else
            call parquet_open_table(t, f, sample_fraction=0.5_real64)
        end if
        call t%materialize_all()
        used = int(parquet_debug_get_prefetch_threads_used())
        call check_prefetch_really_parallel(error, used, "a sampled table")
        if (allocated(error)) return
        !
        call check(error, t%nrows() > 0_int64 .and. t%nrows() < int(FROWS, int64), &
            "sample_fraction=0.5 should keep some but not all of the rows")
        if (allocated(error)) return
        call t%get("a", a)
        call t%get("c", c)
        call t%get("e", e)
        call check(error, size(a) == size(c) .and. size(a) == size(e), &
            "three columns of one sampled table came back with different lengths")
        if (allocated(error)) return
        ! The whole point: column c is 3*i and column e is 5*i over the SAME original rows, so
        ! these two identities hold for any sample and for no pair of different samples. Columns
        ! a, c and e are far enough apart in the slot order to land on different threads.
        call check(error, all(abs(c - 3.0_real64 * a) < 1.0e-9_real64), &
            "two columns of a sampled table came from different draws; every per-thread reader " // &
            "must sample with the seed the table settled at open")
        if (allocated(error)) return
        call check(error, all(abs(e - 5.0_real64 * a) < 1.0e-9_real64), &
            "the last column of a sampled table came from a different draw than the first")
    end subroutine check_sampled_prefetch_agrees
    !
    !> A **masked slice** -- a slice whose transform narrows it, so the reader carries the slice's
    !> own row range as part of its mask -- prefetches in parallel too.
    !>
    !> This is the one branch a per-thread reader takes *differently* from the table's own, and the
    !> only place P4 needed a rule rather than a refactor. `table_open_reader_with_transform`'s
    !> masked path ends by attaching the slice's row range with `parquet_reader_set_filter` and then
    !> rebuilding `cache%rg_bounds` from the resulting reader. The attach must happen on every
    !> reader; the rebuild must happen on exactly one, because `rg_bounds` is shared cache state and
    !> several threads writing it at once is a data race on a component every read path consults.
    !>
    !> A per-thread reader that skipped the attach would return the covering row groups' survivors
    !> in full, so a column read on a worker thread would be longer than one read on the main
    !> thread; the cross-column identities below are what catch that, and the row-range assertion is
    !> what catches a reader that was scoped to the wrong rows entirely.
    subroutine test_prefetch_masked_slice_columns_agree(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_maskslice.parquet"
        integer(int64), parameter :: LO = 50000_int64, HI = 150000_int64
        type(parquet_table) :: t
        real(real64), allocatable :: a(:), c(:), e(:)
        integer :: used
        !
        call write_wide_fixture(f)
        call parquet_reset_settings()
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        ! Slice PLUS a narrowing transform is what makes the slice masked; a slice on its own is
        ! trimmed in memory and never reaches this path.
        call parquet_open_table(t, f, LO, HI, sample_fraction=0.5_real64)
        call t%materialize_all()
        used = int(parquet_debug_get_prefetch_threads_used())
        call check_prefetch_really_parallel(error, used, "a masked slice")
        if (allocated(error)) return
        !
        call check(error, t%nrows() > 0_int64 .and. t%nrows() < HI - LO + 1_int64, &
            "a sampled slice should keep some but not all of its rows")
        if (allocated(error)) return
        call t%get("a", a)
        call t%get("c", c)
        call t%get("e", e)
        call check(error, size(a) == size(c) .and. size(a) == size(e), &
            "three columns of one masked slice came back with different lengths")
        if (allocated(error)) return
        call check(error, all(abs(c - 3.0_real64 * a) < 1.0e-9_real64) .and. &
            all(abs(e - 5.0_real64 * a) < 1.0e-9_real64), &
            "two columns of a masked slice came from different row sets")
        if (allocated(error)) return
        ! Column a is written as 1*i, so its value IS the original file row number -- which makes
        ! the slice's own bounds directly assertable rather than inferred from a count.
        call check(error, all(a >= real(LO, real64)) .and. all(a <= real(HI, real64)), &
            "a masked slice returned rows from outside its own row range")
    end subroutine test_prefetch_masked_slice_columns_agree
    !
    !> A `qc=` table now prefetches in parallel, each thread's reader carrying the same rules. The
    !> rules are installed per reader but the checks run per column read, and each column is read by
    !> exactly one thread -- so nothing is checked twice and the values are untouched either way.
    subroutine test_prefetch_qc_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_qc.parquet"
        type(parquet_table) :: par, ser
        type(parquet_read_qc) :: qc
        real(real64), allocatable :: pe(:), se(:)
        integer :: used
        !
        call write_wide_fixture(f)
        ! Satisfied by the fixture (every value is positive), so this exercises the qc machinery
        ! without provoking a violation -- the violation case is the soft test below.
        call qc%add("a, >=1")
        call qc%add("e, >=1")
        !
        call parquet_reset_settings()
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        call parquet_open_table(par, f, qc=qc)
        call par%materialize_all()
        used = int(parquet_debug_get_prefetch_threads_used())
        call check_prefetch_really_parallel(error, used, "a qc-checked table")
        if (allocated(error)) return
        !
        call parquet_set_prefetch_threads(1)
        call parquet_open_table(ser, f, qc=qc)
        call ser%materialize_all()
        call parquet_reset_settings()
        !
        call par%get("e", pe)
        call ser%get("e", se)
        call check(error, size(pe) == size(se) .and. size(pe) == FROWS, &
            "a qc-checked table's row count differed between the parallel and serial prefetch")
        if (allocated(error)) return
        call check(error, all(abs(pe - se) < 1.0e-9_real64), &
            "a qc-checked table's values differed between the parallel and serial prefetch")
    end subroutine test_prefetch_qc_parallel_equals_serial
    !
    !> A qc bound the data VIOLATES, with `qc_soft=.true.`: the read must complete on both paths and
    !> hand back the same values, with the violation reported rather than enforced.
    !>
    !> **What this does not assert, and why.** The number of warnings printed is not observable from
    !> inside the process -- they go to the library's output channel, not to a counter -- so the
    !> assertion here is that a soft violation changes *nothing* about the answer on the parallel
    !> path. Which warnings duplicate was established separately, by running this shape and reading
    !> the output; the finding is recorded in `doc/pages/schema/quality-control.md` and
    !> `doc/pages/operating/thread-safety.md`. A HARD violation is deliberately not tested here: it aborts,
    !> which needs an out-of-process error scenario rather than an in-process test.
    subroutine test_prefetch_qc_soft_warns_per_column(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_qcsoft.parquet"
        type(parquet_table) :: par, ser
        type(parquet_read_qc) :: qc
        real(real64), allocatable :: pa(:), sa(:), pe(:), se(:)
        integer :: used
        !
        call write_wide_fixture(f)
        ! Violated by construction: column a runs 1..FROWS and column e runs 5..5*FROWS, so both
        ! upper bounds are exceeded and both columns have something to warn about. The lower bound
        ! is satisfied and is there only because a rule's first bound is its `min:`, which accepts
        ! `>=`/`>` alone -- an upper bound cannot be given on its own.
        call qc%add("a, >=1, <=10")
        call qc%add("e, >=1, <=10")
        !
        call parquet_reset_settings()
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        call parquet_open_table(par, f, qc=qc, qc_soft=.true.)
        call par%materialize_all()
        used = int(parquet_debug_get_prefetch_threads_used())
        call check_prefetch_really_parallel(error, used, "a soft-qc table")
        if (allocated(error)) return
        !
        call parquet_set_prefetch_threads(1)
        call parquet_open_table(ser, f, qc=qc, qc_soft=.true.)
        call ser%materialize_all()
        call parquet_reset_settings()
        !
        call par%get("a", pa)
        call ser%get("a", sa)
        call par%get("e", pe)
        call ser%get("e", se)
        call check(error, size(pa) == FROWS .and. size(pe) == FROWS, &
            "a soft qc violation must not change how many rows are read")
        if (allocated(error)) return
        call check(error, all(abs(pa - sa) < 1.0e-9_real64) .and. all(abs(pe - se) < 1.0e-9_real64), &
            "a soft qc violation gave different values on the parallel and serial prefetch")
    end subroutine test_prefetch_qc_soft_warns_per_column
    !
    !> A `sort=` table now prefetches on several threads, sharing one permutation.
    !>
    !> **This is the case sharing was invented for.** A per-thread reader that rebuilt the sort would
    !> do it serially -- `pf_sort_threads` stands down inside a parallel region, deliberately -- and
    !> the rebuild was measured at 5.88 s on a 20.8 M-row file, per thread, against a prefetch saving
    !> of a couple of seconds. Adopting the permutation instead is one atomic refcount increment on
    !> an immutable `arrow::Array`, so the cost that justified the refusal is simply gone.
    !>
    !> **The assertion that matters is the cross-column one.** A permutation destroys row-group
    !> locality: sorted row 5 may come from row group 47. So two readers holding *different*
    !> permutations of the same rows would each return a perfectly plausible sorted column of exactly
    !> the right length, and only their disagreement with each other would show it. That is what
    !> `e == 5*a` catches and what a row count or an A/B length check never could.
    subroutine test_prefetch_sort_parallel_equals_serial(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_prefetch_sort.parquet"
        type(parquet_table) :: par, ser
        type(parquet_sortkey) :: srt
        real(real64), allocatable :: pa(:), sa(:), pe(:), se(:)
        integer :: used
        !
        call write_wide_fixture(f)
        call srt%add("-a")
        !
        call parquet_reset_settings()
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        call parquet_open_table(par, f, sort=srt)
        call par%materialize_all()
        used = int(parquet_debug_get_prefetch_threads_used())
        call check_prefetch_really_parallel(error, used, "a sorted table")
        if (allocated(error)) return
        !
        call parquet_set_prefetch_threads(1)
        call parquet_open_table(ser, f, sort=srt)
        call ser%materialize_all()
        call parquet_reset_settings()
        !
        call par%get("a", pa)
        call ser%get("a", sa)
        call par%get("e", pe)
        call ser%get("e", se)
        call check(error, size(pa) == FROWS .and. size(sa) == FROWS, &
            "a sorted table's row count changed between the parallel and serial prefetch")
        if (allocated(error)) return
        ! Descending on a, so the largest row comes first -- checked before the A/B comparison,
        ! because two readers that had both lost the sort would agree with each other perfectly.
        call check(error, abs(pa(1) - real(FROWS, real64)) < 1.0e-9_real64, &
            "the parallel read of a sorted table lost its sort order")
        if (allocated(error)) return
        call check(error, all(abs(pa - sa) < 1.0e-9_real64) .and. all(abs(pe - se) < 1.0e-9_real64), &
            "a sorted table's values differed between the parallel and serial prefetch")
        if (allocated(error)) return
        call check(error, all(abs(pe - 5.0_real64 * pa) < 1.0e-9_real64), &
            "two columns of a sorted table came back in different orders; the per-thread readers " // &
            "did not share one permutation")
    end subroutine test_prefetch_sort_parallel_equals_serial
    !
    !> The prefetch counterpart of `check_really_parallel`: asserts the internally-parallel prefetch
    !> actually engaged, and says nothing on a single-threaded OpenMP build where declining is
    !> correct.
    !>
    !> Without this, every equality assertion in the tests above passes against a gate that quietly
    !> kept refusing -- which is exactly what those clauses did before P4, so the vacuous version of
    !> each test is the one that would have passed on the previous commit.
    subroutine check_prefetch_really_parallel(error, used, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer, intent(in) :: used                         !! threads the prefetch reported.
        character(len=*), intent(in) :: what                !! what was opened, for the message.
        integer :: avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        if (avail <= 1) return
        call check(error, used > 1, &
            what // " must prefetch in parallel here, or the comparison below tests nothing")
    end subroutine check_prefetch_really_parallel
    !
    ! ==================================================================================
    ! P5 -- one column's read, split across its row groups
    ! ==================================================================================
    !
    !> The base case: a single column, several row groups, read once on many threads and once with
    !> `parquet_set_prefetch_threads(1)`, compared value for value.
    !>
    !> **This is the shape the over-columns prefetch could never help with.** That gate needs two
    !> top-level names and declines at one, so a `%get` or `%prefetch` of a single name has always
    !> been serial however large the column was. Splitting by row group is the complement, and the
    !> two are mutually exclusive by construction -- the row-group split is only ever considered
    !> where no reader was passed in, which is exactly "not already inside the over-columns region".
    !>
    !> The fixture's values are a pure function of the row number, so a chunk pasted at the wrong
    !> offset -- the characteristic failure of a row-group split, and one that leaves a
    !> perfectly-sized column full of plausible numbers -- shows up as a value in the wrong place
    !> rather than as a length or a crash.
    subroutine test_colread_split_equals_whole(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_split.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: got(:)
        integer :: used, i
        !
        call write_rowgroup_fixture(f)
        call parquet_reset_settings()
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("v", got)
        used = int(parquet_debug_get_colread_threads_used())
        call check_colread_really_parallel(error, used, "a multi-row-group single column")
        if (allocated(error)) return
        !
        call check(error, size(got) == FROWS, "the split read returned the wrong number of rows")
        if (allocated(error)) return
        ! Every row, not a sample of them: a wrong paste offset moves a whole row group, so
        ! checking only the ends would miss a swap of two interior groups entirely.
        call check(error, all(abs(got - [(1.5_real64 * real(i, real64), i = 1, FROWS)]) &
            < 1.0e-9_real64), "the split read put at least one row group's values in the wrong rows")
    end subroutine test_colread_split_equals_whole
    !
    !> Nulls, spread across every row group.
    !>
    !> **Validity is allocated lazily, so this is the one place the split could race rather than
    !> merely be wrong.** Two threads pasting row groups that each contain a null would both find the
    !> bitmap unallocated and both allocate it. The read asks the file footer whether the column has
    !> any nulls and allocates the bitmap up front when it does — so this test needs nulls in more
    !> than one row group to exercise it at all, which is why they are strewn rather than clustered.
    !>
    !> A race is not reliably reproducible, so what this asserts is the observable consequence: every
    !> null in its own row and no others. `%paste` REPLACES a range's validity rather than merging
    !> it, which is what makes disjoint row-group pastes correct here.
    subroutine test_colread_nulls_survive_the_split(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_nulls.parquet"
        type(parquet_table) :: t
        type(parquet_writer) :: w
        real(real64) :: v(FROWS)
        logical :: valid(FROWS)
        logical, allocatable :: mask(:)
        integer :: used, i, nbad
        !
        do i = 1, FROWS
            v(i) = 1.5_real64 * real(i, real64)
            valid(i) = mod(i, 7) /= 0        ! every 7th row null, so every row group has some
        end do
        call parquet_open_writer(w, f, chunk_size=FROWS / 8)
        call parquet_write_column(w, "v", v, is_valid=valid)
        call parquet_close_writer(w)
        !
        call parquet_reset_settings()
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get_valid_mask("v", mask)
        used = int(parquet_debug_get_colread_threads_used())
        call check_colread_really_parallel(error, used, "a null-carrying single column")
        if (allocated(error)) return
        !
        call check(error, size(mask) == FROWS, "the split read lost rows from a null-carrying column")
        if (allocated(error)) return
        nbad = 0
        do i = 1, FROWS
            if (mask(i) .neqv. valid(i)) nbad = nbad + 1
        end do
        call check(error, nbad == 0, "the split read placed nulls in the wrong rows")
    end subroutine test_colread_nulls_survive_the_split
    !
    !> Row groups SHORTER than one validity block, where the split has no whole block to paste
    !> freely and every row group goes through the critical section entire.
    !>
    !> `paste_row_group_safely` divides a row group's range into a middle occupying whole bitmap
    !> blocks — pasted with no lock — and the ragged element at each end, which is shared with the
    !> neighbouring row group and so has to be serialised. A row group short enough to contain no
    !> whole block at all has no middle, and takes a third arm that serialises the lot.
    !>
    !> **The test above cannot reach it, and that is a property of the row-group SIZE rather than
    !> of anything about nulls.** Its fixture has 25,000-row groups, so a whole block always exists;
    !> here the row groups are 40 rows against a 64-bit block, which is the only way the arm is
    !> reachable at all. The two are each other's control: same column shape, same null pattern,
    !> different arm, and both must produce exactly the same answer — which is the point, since a
    !> lock that was skipped and a lock that was taken must be indistinguishable in the result.
    !>
    !> The work floor is moved rather than the fixture grown for the usual reason (Risk-49): a
    !> fixture large enough to clear the real floor with 40-row row groups would be tens of
    !> thousands of row groups.
    subroutine test_colread_short_row_groups(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_shortrg.parquet"
        ! 40 rows to a row group against a 64-bit validity block: no row group can contain a whole
        ! block, whatever offset it starts at, so every one of the ten takes the serialised arm.
        integer, parameter :: N = 400, CH = 40
        type(parquet_table) :: t
        type(parquet_writer) :: w
        real(real64) :: v(N)
        logical :: valid(N)
        logical, allocatable :: mask(:)
        real(real64), allocatable :: got(:)
        integer :: used, i, nbad
        !
        do i = 1, N
            v(i) = 1.5_real64 * real(i, real64)
            valid(i) = mod(i, 3) /= 0
        end do
        call parquet_open_writer(w, f, chunk_size=CH)
        call parquet_write_column(w, "v", v, is_valid=valid)
        call parquet_close_writer(w)
        !
        call parquet_reset_settings()
        call parquet_debug_set_colread_min_elements(1_c_int64_t)
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("v", got, mask)
        used = int(parquet_debug_get_colread_threads_used())
        call parquet_debug_set_colread_min_elements(0_c_int64_t)
        call check_colread_really_parallel(error, used, "a column read in row groups shorter than a block")
        if (allocated(error)) return
        !
        call check(error, size(got) == N .and. size(mask) == N, &
            "the split read of short row groups returned the wrong number of rows")
        if (allocated(error)) return
        nbad = 0
        do i = 1, N
            if (mask(i) .neqv. valid(i)) nbad = nbad + 1
            ! A null row's stored value is unspecified, so only the live rows are compared.
            if (valid(i) .and. abs(got(i) - v(i)) > 1.0e-9_real64) nbad = nbad + 1
        end do
        call check(error, nbad == 0, &
            "serialising a whole short row group must place the same values and nulls as the " // &
            "whole-block path does")
    end subroutine test_colread_short_row_groups
    !
    !> A vector column: the work floor is in ELEMENTS (`rows * width`), and `%paste` addresses rows,
    !> so a width greater than one is where a confusion between the two units would show.
    subroutine test_colread_vector_column_splits(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_vec.parquet"
        ! WID, not W: Fortran is case-insensitive, so a `W` parameter and the `w` writer below
        ! would be one symbol.
        integer, parameter :: N = 40000, WID = 4
        type(parquet_table) :: t
        type(parquet_writer) :: w
        real(real64) :: v(WID, N)
        real(real64), allocatable :: got(:,:)
        integer :: used, i, e, nbad
        !
        do i = 1, N
            do e = 1, WID
                v(e, i) = real(i, real64) + 0.25_real64 * real(e, real64)
            end do
        end do
        call parquet_open_writer(w, f, chunk_size=N / 8)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        call parquet_reset_settings()
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("v", got)
        used = int(parquet_debug_get_colread_threads_used())
        call check_colread_really_parallel(error, used, "a vector single column")
        if (allocated(error)) return
        !
        call check(error, size(got, 1) == WID .and. size(got, 2) == N, &
            "the split read changed a vector column's shape")
        if (allocated(error)) return
        nbad = 0
        do i = 1, N
            do e = 1, WID
                if (abs(got(e, i) - v(e, i)) > 1.0e-9_real64) nbad = nbad + 1
            end do
        end do
        call check(error, nbad == 0, "the split read misplaced elements of a vector column")
    end subroutine test_colread_vector_column_splits
    !
    !> A string column must NOT be split, and must still read correctly.
    !>
    !> `%paste` is what puts each row group in its place without reallocating, and a
    !> `parquet_string_column` is a packed variable-length store with no fixed row slots, so it
    !> cannot be overwritten in place. The refusal is therefore structural, not a tuning choice --
    !> and it costs nothing, because those buffers grow geometrically rather than exact-fit.
    !>
    !> Both halves matter: a thread count of 0 alone would pass against a read that returned
    !> nonsense, and correct values alone would pass against a split that had somehow been allowed.
    subroutine test_colread_string_stays_whole(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_string.parquet"
        type(parquet_table) :: t
        type(parquet_writer) :: w
        character(len=12) :: s(FROWS)
        character(len=:), allocatable :: got(:)
        integer :: used, i
        !
        do i = 1, FROWS
            write(s(i), '(a,i0)') "r", i
        end do
        s(1) = "a"                      ! shortest first, per CLAUDE.md's string-fixture rule
        call parquet_open_writer(w, f, chunk_size=FROWS / 8)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
        !
        call parquet_reset_settings()
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("s", got)
        used = int(parquet_debug_get_colread_threads_used())
        call check(error, used == 0, &
            "a string column must not be split across row groups: its packed store has no fixed " // &
            "row slots, so %paste cannot address them")
        if (allocated(error)) return
        call check(error, size(got) == FROWS, "the unsplit string read returned the wrong row count")
        if (allocated(error)) return
        call check(error, trim(got(1)) == "a" .and. trim(got(FROWS)) == "r200000", &
            "the unsplit string read returned the wrong values")
    end subroutine test_colread_string_stays_whole
    !
    !> The work floor, in both directions.
    !>
    !> **A round trip proves nothing here** -- the override has no getter, and its only observable is
    !> whether the gate changes its mind. So the same three-step shape the mutation gate's own limits
    !> use: confirm the split happens at the real constant, close it with a floor above the fixture,
    !> and confirm it opens again when the override is cleared. Without the first and third steps, a
    !> hook that did nothing (or one that never restored) would pass.
    !>
    !> The floor exists at all because each thread opens its own reader, which parses the footer;
    !> below roughly a megabyte of column that dominates and the split loses. No fixture a test can
    !> afford sits near the real break-even, which is why the override exists rather than a sweep
    !> over fixture sizes (`feature_risks.md` Risk-49).
    subroutine test_colread_floor_override(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_floor.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: got(:)
        integer :: used_before, used_closed, used_after
        !
        call write_rowgroup_fixture(f)
        call parquet_reset_settings()
        !
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("v", got)
        used_before = int(parquet_debug_get_colread_threads_used())
        !
        ! One element above what this fixture offers, so the gate closes on size alone.
        call parquet_debug_set_colread_min_elements(int(FROWS, c_int64_t) + 1_c_int64_t)
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("v", got)
        used_closed = int(parquet_debug_get_colread_threads_used())
        !
        call parquet_debug_set_colread_min_elements(0_c_int64_t)
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f)
        call t%get("v", got)
        used_after = int(parquet_debug_get_colread_threads_used())
        !
        call check(error, used_closed == 0, &
            "raising the single-column work floor above the fixture must close the split")
        if (allocated(error)) return
        call check_colread_really_parallel(error, used_before, "the fixture at the real floor")
        if (allocated(error)) return
        call check_colread_really_parallel(error, used_after, "the fixture after clearing the override")
        if (allocated(error)) return
        ! The answer must be right on BOTH sides of the gate, or the floor would be hiding a defect
        ! rather than choosing a path.
        call check(error, size(got) == FROWS .and. abs(got(FROWS) - 1.5_real64 * real(FROWS, real64)) &
            < 1.0e-9_real64, "the read was wrong after the floor override was cleared")
    end subroutine test_colread_floor_override
    !
    !> A sampled table's single-column split: each row group is read by a different thread through a
    !> different reader, and every one of them must have drawn the SAME sample.
    !>
    !> This is the row-group counterpart of the unseeded-sample test above, and it is sharper in one
    !> way: there, two columns disagreeing showed up as two different row sets; here the disagreement
    !> is *within one column*, between row groups, and the column still comes back exactly as long as
    !> the table says it should be. So the assertion has to be on the values themselves.
    !>
    !> The fixture's value is `1.5 * row`, so every surviving value must be a multiple of 1.5 whose
    !> row number is in range, and the sequence must be strictly increasing — a row group drawn from
    !> a different sample breaks the ordering even when it does not break the length.
    subroutine test_colread_sampled_agrees(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/tblpar_colread_sample.parquet"
        type(parquet_table) :: t
        real(real64), allocatable :: got(:)
        integer :: used, i, nbad
        !
        call write_rowgroup_fixture(f)
        call parquet_reset_settings()
        ! The floor is lowered because the SAMPLE puts this fixture under it: the gate measures the
        ! rows it will actually materialize, and half of FROWS is below the real constant. That is
        ! correct behaviour, and it is not what this test is about -- so the floor is moved out of
        ! the way rather than the fixture grown, which would make the whole suite slower to protect
        ! an assertion about sampling.
        call parquet_debug_set_colread_min_elements(1000_c_int64_t)
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(t, f, sample_fraction=0.5_real64)
        call t%get("v", got)
        used = int(parquet_debug_get_colread_threads_used())
        call parquet_debug_set_colread_min_elements(0_c_int64_t)
        call check_colread_really_parallel(error, used, "a sampled single column")
        if (allocated(error)) return
        !
        call check(error, size(got) == int(t%nrows()), &
            "a sampled split read disagreed with the table's own row count")
        if (allocated(error)) return
        call check(error, size(got) > 0 .and. size(got) < FROWS, &
            "sample_fraction=0.5 should keep some but not all of the rows")
        if (allocated(error)) return
        nbad = 0
        do i = 2, size(got)
            if (got(i) <= got(i - 1)) nbad = nbad + 1
        end do
        call check(error, nbad == 0, &
            "a sampled split read returned rows out of order; the row groups did not all draw " // &
            "the same sample")
    end subroutine test_colread_sampled_agrees
    !
    !> The single-column counterpart of `check_prefetch_really_parallel`.
    !
    !> `bounded=` through BOTH parallel read paths, against the same filter on the default engine.
    !!
    !! Both paths are already chunked, so `bounded=` does not change what they do -- what this
    !! pins is that routing the serial arms through the chunked assembler did not break either of
    !! them, and that the answers still agree with the default engine's when several threads and
    !! several readers are involved.
    !!
    !!   * `materialize_column_parallel` splits ONE column across its row groups, one reader per
    !!     thread. Its work floor is judged on the rows the read will actually materialize --
    !!     SURVIVORS times width -- so an aggressive filter puts a test-sized fixture under it and
    !!     the split silently declines. The floor is therefore lowered here rather than the fixture
    !!     grown, exactly as the sampled test above does, and
    !!     `parquet_debug_get_colread_threads_used` is what proves the split really happened: an
    !!     A/B whose parallel arm ran serially would agree for the wrong reason.
    !!   * `materialize_marked_parallel` splits the COLUMNS across threads, each with its own
    !!     reader adopting the table's mask. `parquet_debug_get_prefetch_threads_used` is its
    !!     counterpart observation.
    subroutine test_bounded_parallel_paths(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f1 = "test_run/tblpar_bounded_colread.parquet"
        character(len=*), parameter :: f2 = "test_run/tblpar_bounded_wide.parquet"
        type(parquet_table) :: plain, bnd
        type(parquet_filter) :: filt, filt_wide
        real(real64), allocatable :: a(:), b(:)
        integer :: used_col, used_pref, avail
        character(len=1), parameter :: names(5) = ["a", "b", "c", "d", "e"]
        integer :: c
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it neither parallel read path exists, so " // &
            "both arms of the A/B below would run the same serial code and agree for the wrong " // &
            "reason, and the thread-count assertions would have nothing to observe")
        return
#endif
        !
        ! Read the ONE way that compiles without OpenMP too: the skip above is a runtime
        ! decision, so every line below it still has to build in a serial configuration.
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        !
        ! ---- one column, split across its row groups ----
        call write_rowgroup_fixture(f1)
        call parquet_reset_settings()
        ! The fixture's value is 1.5*row, so this keeps the second half of the file: four of the
        ! eight row groups survive whole and four are pruned by the statistics screen, which is
        ! the case a parallel chunked read has to step over. The 100000 survivors are below the
        ! real single-column work floor -- the gate measures what it will MATERIALIZE, not what
        ! the file holds -- so the floor is lowered rather than the fixture grown.
        call filt%add("v > 150000")
        call parquet_debug_set_colread_min_elements(1000_c_int64_t)
        call parquet_debug_set_colread_threads_used(0_c_int64_t)
        call parquet_open_table(bnd, f1, filter=filt, bounded=.true.)
        call bnd%get("v", b)
        used_col = int(parquet_debug_get_colread_threads_used())
        call parquet_debug_set_colread_min_elements(0_c_int64_t)
        call check_colread_really_parallel(error, used_col, "a bounded single-column read")
        if (allocated(error)) return
        call parquet_open_table(plain, f1, filter=filt)
        call plain%get("v", a)
        call check(error, size(a) == size(b), &
            "a bounded split read should return as many rows as the default engine")
        if (allocated(error)) return
        call check(error, all(abs(a - b) < 1.0e-9_real64), &
            "a bounded split read should return the same values as the default engine")
        if (allocated(error)) return
        !
        ! ---- several columns, split across threads ----
        ! Its own filter: the wide fixture's columns are a..e, so the one above names nothing
        ! here. Column `a` holds the row number, so this keeps the same second half.
        call write_wide_fixture(f2)
        call filt_wide%add("a > 100000")
        call parquet_debug_set_prefetch_threads_used(0_c_int64_t)
        call parquet_open_table(bnd, f2, filter=filt_wide, bounded=.true.)
        call bnd%materialize_all()
        used_pref = int(parquet_debug_get_prefetch_threads_used())
        call check(error, used_pref > 1 .or. avail <= 1, &
            "a bounded materialize_all must split its columns across threads here, or the " // &
            "comparison below tests the serial path against itself")
        if (allocated(error)) return
        call parquet_open_table(plain, f2, filter=filt_wide)
        call plain%materialize_all()
        call check(error, bnd%nrows() == plain%nrows(), &
            "a bounded parallel materialize should hold the same rows as the default engine")
        if (allocated(error)) return
        do c = 1, 5
            call plain%get(names(c), a)
            call bnd%get(names(c), b)
            call check(error, size(a) == size(b), &
                "a bounded parallel materialize should return as many rows per column")
            if (allocated(error)) return
            call check(error, all(abs(a - b) < 1.0e-9_real64), &
                "a bounded parallel materialize should return the same values per column")
            if (allocated(error)) return
        end do
    end subroutine test_bounded_parallel_paths
    subroutine check_colread_really_parallel(error, used, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer, intent(in) :: used                         !! threads the read reported.
        character(len=*), intent(in) :: what                !! what was read, for the message.
        integer :: avail
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        if (avail <= 1) return
        call check(error, used > 1, &
            what // " must have its read split across row groups here, or the assertions below " // &
            "test the serial path against itself")
    end subroutine check_colread_really_parallel
    !
    !> One float64 column over eight row groups, its value a pure function of the row number.
    subroutine write_rowgroup_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write (one per test).
        type(parquet_writer) :: w
        real(real64) :: v(FROWS)
        integer :: i
        !
        do i = 1, FROWS
            v(i) = 1.5_real64 * real(i, real64)
        end do
        ! Eight row groups, so the split has something to split even on a modest machine, and so
        ! that an off-by-one in the paste offset moves a whole group rather than a single row.
        call parquet_open_writer(w, fname, chunk_size=FROWS / 8)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
    end subroutine write_rowgroup_fixture
    !
    !> Five float64 columns, wide enough that three of them clear the parallel gate's work floor.
    subroutine write_wide_fixture(fname)
        character(len=*), intent(in) :: fname !! file to write.
        type(parquet_writer) :: w
        real(real64) :: v(FROWS)
        integer :: c, i
        character(len=1), parameter :: names(5) = ["a", "b", "c", "d", "e"]
        !
        call parquet_open_writer(w, fname)
        do c = 1, 5
            do i = 1, FROWS
                v(i) = real(c, real64) * real(i, real64)
            end do
            call parquet_write_column(w, names(c), v)
        end do
        call parquet_close_writer(w)
    end subroutine write_wide_fixture
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
    !> **Each limit is raised on its own, never both at once.** `colwork_plan` answers serial if *any*
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
        ! **The six messages are built ONCE, above the loop, and that is required rather than tidy.**
        ! The loop makes about `NROW * (3 + 2*VW)` `check` calls -- 19 per row here -- and each used
        ! to take a freshly concatenated `"%" // what // ": ..."`. flang 22.1.8 gives every such
        ! temporary its own stack slot and does not reclaim them until the procedure returns, so the
        ! frame grew by about 90 bytes per call and the suite died of stack exhaustion at row 4500 -- a
        ! SIGSEGV in test-drive's own `check_logical`, naming nothing in this file. Hoisting removes
        ! the temporary rather than enlarging the stack, so it needs no flag and no ulimit, and it is
        ! faster on every compiler. Keep any new message in this loop hoisted too.
        character(len=:), allocatable :: m_key, m_anull, m_a, m_vnull, m_v, m_s
        !
        call t%get("n", n)
        call t%get("k", k)
        call t%get("a", a, is_valid=am)
        call t%get("fv", v, is_valid=vm)
        call t%get("s", s)
        m_key   = "%" // what // ": the key column is out of step with the row identity"
        m_anull = "%" // what // ": the scalar column's nulls are out of step with the row identity"
        m_a     = "%" // what // ": the scalar column is out of step with the row identity"
        m_vnull = "%" // what // ": the vector column's element nulls are out of step with the row identity"
        m_v     = "%" // what // ": the vector column is out of step with the row identity"
        m_s     = "%" // what // ": the string column is out of step with the row identity"
        do i = 1, size(n)
            row = int(n(i))
            call check(error, k(i) == key_for(row, NROW), m_key)
            if (allocated(error)) return
            call check(error, am(i) .eqv. (mod(row, 11) /= 0), m_anull)
            if (allocated(error)) return
            ! Only where valid: a null row's stored value is unspecified by parquet_column's own
            ! contract, so asserting on it would be testing an implementation detail.
            if (am(i)) then
                call check(error, a(i) == real(row, real64) * 0.5_real64, m_a)
                if (allocated(error)) return
            end if
            do e = 1, VW
                call check(error, vm(e, i) .eqv. .not. (mod(row, 13) == 0 .and. e == 3), m_vnull)
                if (allocated(error)) return
                if (vm(e, i)) then
                    call check(error, v(e, i) == real(100*row + e, real64), m_v)
                    if (allocated(error)) return
                end if
            end do
            write(buf, '(i12.12)') NROW - row
            call check(error, s(i) == buf, m_s)
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
    !> **The validity-block alignment rule the parallel column read depends on, asserted directly.**
    !!
    !! `parquet_column` packs validity as a bit map, so two threads pasting *adjacent row groups*
    !! read-modify-write the same block unless each trims its range to whole blocks first. That race
    !! is a few instructions wide: it was seen twice in a few dozen full test runs before being
    !! diagnosed, and `nulls land in the right rows when one column's read is split` — the test that
    !! caught it — cannot be relied on to catch a regression, because a lost update simply may not
    !! happen on any given run. So the rule itself is what is asserted here, deterministically, the
    !! way `thread row ranges cover every row and never share a validity byte` does for
    !! `parquet_string_column` (`feature_risks.md` Risk-61).
    !!
    !! What it checks, for a sweep of widths and row-group layouts including ones whose boundaries
    !! deliberately do not divide the alignment: the reported middle is inside the range; no two
    !! adjacent row groups' middles share a block; and the middles cover everything except ragged
    !! ends short enough to be worth serialising.
    subroutine test_colread_block_alignment(error)
        type(error_type), allocatable, intent(out) :: error
        ! The library's own published block width. Importing it rather than restating it is right
        ! here, because there is exactly one definition of it -- the thing this test guards is the
        ! ALIGNMENT ARITHMETIC, not the constant.
        integer(int64), parameter :: ALIGN = parquet_validity_block_bits
        integer(int64) :: w, rg, lo, hi, mid_lo, mid_hi, prev_end_blk, blk_first, blk_last, chunk
        integer :: bad_inside, bad_shared, bad_ragged
        !
        bad_inside = 0
        bad_shared = 0
        bad_ragged = 0
        do w = 1_int64, 5_int64
            ! 25000 is the size the failing fixture used and divides none of the alignments; 4096
            ! divides all of them, so it is the control where no trimming should be needed at all.
            do chunk = 1_int64, 2_int64
                prev_end_blk = -1_int64
                do rg = 0_int64, 7_int64
                    if (chunk == 1_int64) then
                        lo = rg*25000_int64 + 1_int64
                        hi = lo + 25000_int64 - 1_int64
                    else
                        lo = rg*4096_int64 + 1_int64
                        hi = lo + 4096_int64 - 1_int64
                    end if
                    call parquet_debug_colread_block_rows(lo, hi, w, mid_lo, mid_hi)
                    if (mid_lo > mid_hi) cycle          ! no whole block: the caller serialises it all
                    ! The middle must lie inside the range it was derived from.
                    if (mid_lo < lo .or. mid_hi > hi) bad_inside = bad_inside + 1
                    ! Element indices are 1-based over (row-1)*w + 1 .. row*w.
                    blk_first = ((mid_lo - 1_int64)*w)/ALIGN
                    blk_last = (mid_hi*w - 1_int64)/ALIGN
                    ! The middle must start on a block boundary and end on one.
                    if (modulo((mid_lo - 1_int64)*w, ALIGN) /= 0_int64) bad_shared = bad_shared + 1
                    if (modulo(mid_hi*w, ALIGN) /= 0_int64) bad_shared = bad_shared + 1
                    ! ...and it must not begin in a block a previous row group's middle ended in.
                    if (prev_end_blk >= 0_int64 .and. blk_first <= prev_end_blk) bad_shared = bad_shared + 1
                    prev_end_blk = blk_last
                    ! The ragged ends left over must be short: they go through a critical section,
                    ! so a rule that trimmed away most of the range would be correct but useless.
                    ! The bound is in ROWS and is the alignment period -- for a width coprime with
                    ! the block that is the whole block, and for a width dividing it, fewer.
                    if (mid_lo - lo >= ALIGN) bad_ragged = bad_ragged + 1
                    if (hi - mid_hi >= ALIGN) bad_ragged = bad_ragged + 1
                end do
            end do
        end do
        call check(error, bad_inside == 0, "the whole-block sub-range must lie inside the row group it came from")
        if (allocated(error)) return
        call check(error, bad_shared == 0, &
            "two row groups' pasted ranges must never share a validity block")
        if (allocated(error)) return
        call check(error, bad_ragged == 0, &
            "the serialised ragged ends must stay under one block, or the split gives back its parallelism")
        if (allocated(error)) return
        ! A range shorter than one block has no whole block in it and must say so rather than
        ! returning something that looks usable -- the caller keys on mid_lo > mid_hi.
        call parquet_debug_colread_block_rows(5_int64, 9_int64, 1_int64, mid_lo, mid_hi)
        call check(error, mid_lo > mid_hi, "a range too short to hold a whole block must report none")
        if (allocated(error)) return
        ! A width that is itself a multiple of the alignment makes every row boundary aligned, so
        ! nothing should be trimmed at all.
        call parquet_debug_colread_block_rows(7_int64, 19_int64, ALIGN, mid_lo, mid_hi)
        call check(error, mid_lo == 7_int64 .and. mid_hi == 19_int64, &
            "when every row fills whole blocks the range must be used untrimmed")
    end subroutine test_colread_block_alignment
    !
    !> Concurrent `%set_null` on ONE column must keep every null, including when the rows two
    !> threads write share a validity block.
    !>
    !> **The two arms are the whole point, and neither is sufficient alone.** Validity is packed
    !> `parquet_validity_block_bits` elements to one `integer(int64)`, so "different rows" is not
    !> "different memory": without the `!$omp atomic` in `bit_set`
    !> (`src/parquet_columns_util.f90`) two threads writing rows in one block both read, modify and
    !> write it, and one update is lost silently -- the column still validates and the row count is
    !> still right. See feature_risks.md Risk-135.
    !>
    !>   * The **sharing** arm uses `schedule(dynamic)`, which hands out single iterations, so
    !>     threads interleave across every block. Measured against the unfixed code on machine A:
    !>     333666 of 819200 nulls lost, about 41%.
    !>   * The **aligned** arm is the negative control. Each thread owns a contiguous, block-aligned
    !>     span, so no block is ever touched by two threads and the arm passes with or without the
    !>     atomic. It is what localises a future regression: if the atomic is removed, the first arm
    !>     fails and this one still passes, which says the defect is block sharing rather than
    !>     anything else about concurrent nulling.
    !>
    !> `NR` is a multiple of the block size so the aligned arm can be exact, and the loop asserts it
    !> really ran on more than one thread -- a one-thread run would pass both arms while testing
    !> nothing.
    subroutine test_concurrent_set_null_shares_block(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), parameter :: NR = 4096_int64
        type(parquet_table) :: t
        real(real64) :: v(NR)
        integer(int64) :: i, blk, lo, hi
        integer :: lost, avail, seen_threads, nblocks
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: both arms below would run serially, so the equality " // &
            "they assert would hold because nothing was concurrent rather than because the " // &
            "bitmap update is atomic")
        return
#endif
        if (avail < 2) then
            call skip_test(error, "needs at least 2 threads: one thread cannot share a validity " // &
                "block with itself, so neither arm would exercise the atomic")
            return
        end if
        v = 1.0_real64
        !
        ! ---- Arm 1: interleaved, so threads share blocks. Fails without the atomic. ----
        call parquet_new_table(t)
        call t%add_column("x", v)
        call t%ensure_validity("x")
        seen_threads = 1
        !$omp parallel default(shared) private(i)
        !$omp single
#ifdef _OPENMP
        seen_threads = omp_get_num_threads()
#endif
        !$omp end single
        !$omp do schedule(dynamic)
        do i = 1_int64, NR
            call t%set_null("x", i)
        end do
        !$omp end do
        !$omp end parallel
        call check(error, seen_threads > 1, &
            "the interleaved arm must actually run on more than one thread, or it tests nothing")
        if (allocated(error)) return
        lost = 0
        do i = 1_int64, NR
            if (.not. t%is_null("x", i)) lost = lost + 1
        end do
        call check(error, lost == 0, &
            "every concurrently written null must survive when threads share validity blocks")
        if (allocated(error)) return
        !
        ! ---- Arm 2 (negative control): block-aligned spans, so no block is shared. ----
        nblocks = int(NR/parquet_validity_block_bits)
        call parquet_new_table(t)
        call t%add_column("x", v)
        call t%ensure_validity("x")
        !$omp parallel do default(shared) private(blk, lo, hi, i) schedule(static)
        do blk = 1_int64, int(nblocks, int64)
            lo = (blk - 1_int64)*parquet_validity_block_bits + 1_int64
            hi = blk*parquet_validity_block_bits
            do i = lo, hi
                call t%set_null("x", i)
            end do
        end do
        !$omp end parallel do
        lost = 0
        do i = 1_int64, NR
            if (.not. t%is_null("x", i)) lost = lost + 1
        end do
        call check(error, lost == 0, &
            "the block-aligned control must keep every null too -- if this fails, the problem is " // &
            "not block sharing")
    end subroutine test_concurrent_set_null_shares_block
    !
    !> **The clearing half of the same bitmap update, which `%set_null` alone cannot reach.**
    !!
    !! `bit_set` and `bit_clear` (`src/parquet_columns_util.f90`) are separate procedures with
    !! separate `!$omp atomic update` directives, and the test above exercises only the first: it
    !! nulls rows and never clears one. Removing the atomic from `bit_clear` alone therefore left
    !! the whole suite green while losing roughly two thirds of the clears -- measured at 119659
    !! to 128777 rows of 200003 at 8 and 16 threads, in both arms below.
    !!
    !! Both public paths that clear a bit are covered, because they are different entry points
    !! reaching one primitive: `%clear_null` says so outright, while a VALUE write clears the
    !! row's null as a side effect -- which is the shape a user is far more likely to reach, since
    !! filling a null-carrying column from several threads is the documented pattern.
    subroutine test_concurrent_clear_null_shares_block(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), parameter :: NR = 4096_int64
        type(parquet_table) :: t
        real(real64) :: v(NR)
        real(real64) :: cell
        integer(int64) :: i, blk, lo, hi
        integer :: left, avail, seen_threads, nblocks
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: every arm below would run serially, so no two " // &
            "threads could share a validity block and the equality would hold for the wrong reason")
        return
#endif
        if (avail < 2) then
            call skip_test(error, "needs at least 2 threads: one thread cannot share a validity " // &
                "block with itself, so no arm would exercise the atomic")
            return
        end if
        v = 1.0_real64
        !
        ! ---- Arm 1: interleaved %clear_null on an all-null column. Fails without the atomic. ----
        call fill_all_null(t, v, NR)
        seen_threads = 1
        !$omp parallel default(shared) private(i)
        !$omp single
#ifdef _OPENMP
        seen_threads = omp_get_num_threads()
#endif
        !$omp end single
        !$omp do schedule(dynamic)
        do i = 1_int64, NR
            call t%clear_null("x", i)
        end do
        !$omp end do
        !$omp end parallel
        call check(error, seen_threads > 1, &
            "the interleaved clear arm must actually run on more than one thread, or it tests nothing")
        if (allocated(error)) return
        left = 0
        do i = 1_int64, NR
            if (t%is_null("x", i)) left = left + 1
        end do
        call check(error, left == 0, &
            "every concurrent clear_null must survive when threads share validity blocks")
        if (allocated(error)) return
        !
        ! ---- Arm 2: interleaved VALUE writes, which clear each row's null in passing. ----
        call fill_all_null(t, v, NR)
        !$omp parallel do default(shared) private(i) schedule(dynamic)
        do i = 1_int64, NR
            call t%set_element("x", i, real(i, real64))
        end do
        !$omp end parallel do
        left = 0
        do i = 1_int64, NR
            if (t%is_null("x", i)) then
                left = left + 1
            else
                call t%get_element("x", i, cell)
                ! Row-distinct values, so a misplaced write is caught as well as a lost clear --
                ! a constant would make this arm pass against either.
                if (cell /= real(i, real64)) left = left + 1
            end if
        end do
        call check(error, left == 0, &
            "a concurrent value write must clear its own row's null and store its own value")
        if (allocated(error)) return
        !
        ! ---- Arm 3 (negative control): block-aligned spans, so no block is shared. ----
        nblocks = int(NR/parquet_validity_block_bits)
        call fill_all_null(t, v, NR)
        !$omp parallel do default(shared) private(blk, lo, hi, i) schedule(static)
        do blk = 1_int64, int(nblocks, int64)
            lo = (blk - 1_int64)*parquet_validity_block_bits + 1_int64
            hi = blk*parquet_validity_block_bits
            do i = lo, hi
                call t%clear_null("x", i)
            end do
        end do
        !$omp end parallel do
        left = 0
        do i = 1_int64, NR
            if (t%is_null("x", i)) left = left + 1
        end do
        call check(error, left == 0, &
            "the block-aligned control must clear every null too -- if this fails, the problem " // &
            "is not block sharing")
    end subroutine test_concurrent_clear_null_shares_block
    !
    !> A fresh single-column table of `n` rows, resident, with validity allocated and EVERY row
    !> null -- the starting state all three arms above clear from.
    subroutine fill_all_null(t, v, n)
        type(parquet_table), intent(out) :: t   !! the table to build.
        real(real64), intent(in) :: v(:)        !! values to seed the column with.
        integer(int64), intent(in) :: n         !! rows.
        integer(int64) :: i
        call parquet_new_table(t)
        call t%add_column("x", v)
        ! Before the region, deliberately: allocating validity on first use is itself a race, and
        ! is refused rather than raced -- which is a different guard from the one under test.
        call t%ensure_validity("x")
        do i = 1_int64, n
            call t%set_null("x", i)
        end do
    end subroutine fill_all_null
    !
    !> **A read accessor must not WRITE to the column it is asked about.**
    !!
    !! A temporal column caches "does this hold a null?", because answering means an O(n) element
    !! scan (`nulls_cached`/`nulls_dirty`, `src/parquet_columns.f90`). No other kind caches
    !! anything. `%has_nulls` used to answer through the type-bound `%any_null()`, which is
    !! `intent(inout)` and REFRESHES that cache -- so a documented read became a writer, reaching
    !! the column through the table's `cache` POINTER, which is what let it compile while the
    !! table itself stayed `intent(in)`.
    !!
    !! The refresh ends by clearing the dirty flag. A `%set_null` raised on another thread during
    !! the scan sets that flag; the clearing discards it, and the column then reports "no nulls"
    !! for good -- single-threaded, afterwards, with nothing to notice. Observed on 194-200 of 200
    !! rounds at 4, 8 and 16 threads before the fix.
    !!
    !! **Both assertions are deterministic once `%has_nulls` stops writing**, which is what makes
    !! this a regression test rather than a flaky one: the cache stays dirty, so the final call
    !! always rescans and always finds the null. The `%is_null` assertion is the discriminator --
    !! without it a failure could equally mean the `%set_null` never landed, which is a different
    !! defect with a different fix.
    subroutine test_has_nulls_survives_concurrent_null(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NDATE = 200000 !! long enough that one scan outlasts the writer's stagger.
        !> Rounds. ONE round caught the regression in only 8 of 10 runs -- the interleaving has to
        !! happen, and sometimes does not. Repeating is what makes the test reliable, and it is
        !! machine-independent in a way that tuning the stagger is not: a faster or slower machine
        !! moves the per-round odds but not the conclusion. Post-fix every round is deterministic,
        !! so this can never fail spuriously, only fail to catch.
        integer, parameter :: ROUNDS = 8
        type(parquet_table) :: t
        type(parquet_date), allocatable :: d(:)
        integer(int64) :: i
        integer :: avail, seen_threads, tid, q, r, latched, unset
        logical :: res
        real(real64) :: t0
        !
        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: with one thread the null is set before any scan " // &
            "begins, so the final assertion would hold whether or not %has_nulls writes the cache")
        return
#endif
        if (avail < 2) then
            call skip_test(error, "needs at least 2 threads: one thread cannot be scanning while " // &
                "another raises the dirty flag, which is the whole mechanism under test")
            return
        end if
        allocate(d(NDATE))
        do i = 1_int64, int(NDATE, int64)
            d(i) = parquet_date(2026, 7, 1)
        end do
        seen_threads = 1
        latched = 0
        unset = 0
        do r = 1, ROUNDS
            ! A FRESH table each round, so the null cache starts dirty and the readers really
            ! rescan. A cache already settled is the state in which no reader scans and the race
            ! cannot occur -- which is why the precondition below is asserted through %is_null and
            ! not through %has_nulls.
            call parquet_new_table(t)
            call t%add_column("d", d)
            if (r == 1) then
                call check(error, .not. t%is_null("d", 1_int64), &
                    "row 1 must start non-null, or the assertions below say nothing about the cache")
                if (allocated(error)) return
            end if
            !$omp parallel default(shared) private(tid, q, res, t0)
            !$omp single
#ifdef _OPENMP
            seen_threads = omp_get_num_threads()
#endif
            !$omp end single
            tid = 0
#ifdef _OPENMP
            tid = omp_get_thread_num()
#endif
            !$omp barrier
            if (tid == 0) then
                ! A short stagger so the readers are already INSIDE a scan when the flag is
                ! raised. One scan of NDATE rows takes several times this, so the writer lands
                ! mid-scan rather than before it, which is the ordering the lost update needs.
                ! It is a stagger, not a synchronisation: the assertions hold whatever order
                ! results, and a round in which the ordering does not occur simply proves nothing.
#ifdef _OPENMP
                t0 = omp_get_wtime()
                do while (omp_get_wtime() - t0 < 2.0e-4_real64)
                end do
#endif
                call t%set_null("d", 1_int64)
            else
                do q = 1, 4
                    res = t%has_nulls("d")
                end do
            end if
            !$omp end parallel
            if (.not. t%is_null("d", 1_int64)) unset = unset + 1
            if (.not. t%has_nulls("d")) latched = latched + 1
        end do
        call check(error, seen_threads > 1, &
            "this test must actually run on more than one thread, or it tests nothing")
        if (allocated(error)) return
        ! The discriminator: without it a failure below could equally mean the %set_null never
        ! landed, which is a different defect with a different fix.
        call check(error, unset == 0, &
            "the concurrent set_null must have landed in every round -- if this fails the null " // &
            "cache is not the problem")
        if (allocated(error)) return
        call check(error, latched == 0, &
            "%has_nulls must still report the null a concurrent writer set: a read accessor that " // &
            "refreshes the temporal null cache discards that writer's dirty flag permanently")
    end subroutine test_has_nulls_survives_concurrent_null
    !
    !> A join's column rewrite answers to `parquet_set_table_threads`, and `threads=` does not
    !! reach it.
    !!
    !! `%join` has two thread controls and they are not interchangeable: the column work -- this
    !! table's own columns gathered, `other`'s copied in beside them -- is `table_colwork`'s, capped
    !! by `parquet_set_table_threads` like every other row-structural mutation, while `threads=`
    !! belongs to the sort that builds the pair list. Nothing asserted either half: a join's ANSWER
    !! does not depend on either team size, so every other test in the join suite passes whatever
    !! the two knobs do.
    !!
    !! **Three arms, and the third is what makes this a split rather than one fact.** The cap must
    !! bite (arm 2 against arm 1), and `threads=` must NOT bite on the same observable (arm 3) --
    !! without arm 3 an implementation in which `threads=` drove the column work too would pass.
    !!
    !! **Arms 4 and 5 are the match half, one per engine, and they close the split in both
    !! directions.** `join_choose_engine` (`src/parquet_tables_join.f90`) takes the hash engine for
    !! this fixture's integer key under the default `order="left"`, and the sort engine under
    !! `order="key"`; a pair list is identical at every team size, so each engine's own thread
    !! record is the only observation that `threads=` reached it. Arm 4 reads the hash engine's
    !! two records -- `parquet_debug_index_threads_used` for the multimap's build over the right
    !! keys, `parquet_debug_index_get_many_threads_used` for its probe with the left ones -- and
    !! arm 5 the sort engine's `parquet_debug_sort_threads_used`. Each asserts the team really is
    !! what `threads=` asked for, AND that `parquet_set_table_threads` does not move it -- the
    !! mirror of arm 3, which asserts `threads=` does not move the column team. Together the arms
    !! pin each knob to its own half and to nothing else.
    !!
    !! Arm 5 is the older of the two and carries a history: on the `group_offsets=` path
    !! `engine_build_runs` (`src/parquet_argsort_kernel.f90`) used to resolve the thread count and
    !! then call the SERIAL builder, so `threads=` changed nothing at all -- measured here at 1 for
    !! `threads=4` with the engine floor lowered, while the ungrouped path's own test (`a
    !! selection's ordering route really opens a team`, `test/test_sorting.f90`) opened 4 on the
    !! same machine. The builder now receives that count (`feature_risks.md` Risk-189).
    !!
    !! **Neither engine's record can be reset, so every request is preceded by an explicit
    !! `threads=1` join that pins the record to 1.** An `== nt` read against a record an earlier
    !! automatic join had already left at nt would hold for the wrong reason; the priming join
    !! makes each later read discriminating. The suite runs serialised
    !! (`suite_is_safe_to_parallelize`), so nothing else writes the records in between.
    !!
    !! The join is made to DETACH so the column work is a real gather over all five columns rather
    !! than a no-op, which is what clears `colwork_avail`'s floor (the Risk-49 trap).
    subroutine test_join_thread_split(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c, d, e
        integer(int32) :: k(NROW), rk(NROW + 1)
        integer :: nt, tab_free, tab_capped, tab_threads1
        !> The HASH engine's teams arm 4 observes: build and probe, primed, asked, and under the cap.
        integer :: hash_build1, hash_probe1, hash_build, hash_probe, hash_build_cap, hash_probe_cap
        integer(int64) :: sort_one, sort_asked, sort_under_cap !! the SORT teams arm 5 observes.
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: table_colwork opens its team inside #ifdef _OPENMP, " // &
            "so every arm below would report one thread and the split would hold for the wrong reason")
        return
#else
        nt = min(4, omp_get_num_procs())
        if (nt < 2) then
            call skip_test(error, "needs at least two processors: colwork_avail clamps to " // &
                "omp_get_num_procs(), so every arm would resolve to 1")
            return
        end if
        ! **The processor count is not enough on its own.** The column team resolves from
        ! `omp_get_max_threads()`, which `OMP_NUM_THREADS=1` sets to 1 on a machine with any number
        ! of processors -- and `fpm test --profile nagdeb` is run exactly that way, because a
        ! multi-threaded abort under a checking build reports from several threads at once. Arm 1's
        ! precondition then fails on a correctly-working library. The sort half is unaffected
        ! (`resolve_thread_count` honours an explicit `threads=` and clamps it to the PROCESSOR
        ! count, not to the ICV), but the arms are one test and skip together.
        if (omp_get_max_threads() < 2) then
            call skip_test(error, "needs OMP_NUM_THREADS >= 2: the column team resolves from " // &
                "omp_get_max_threads(), so arms 1-3 would compare 1 against 1 and hold for the " // &
                "wrong reason")
            return
        end if
        !
        ! A right key holding every left key ONCE, plus one duplicate -- so every left row survives
        ! but one of them twice. The join therefore detaches, and this table's five columns are
        ! really gathered rather than left in place.
        call fill_key(k)
        rk(1:NROW) = k
        rk(NROW + 1) = k(1)
        call build_key_table(b, rk)
        !
        ! ---- Arm 1: the default. The column rewrite must really thread, or arm 2 cannot tell a
        ! cap from a floor that had already declined.
        call build_fixture(a)
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call a%join(b, "k", how="left")
        tab_free = int(parquet_debug_get_table_threads_used())
        !
        ! ---- Arm 2: the cap bites.
        call build_fixture(c)
        call parquet_set_table_threads(1)
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call c%join(b, "k", how="left")
        tab_capped = int(parquet_debug_get_table_threads_used())
        call parquet_reset_settings()
        !
        ! ---- Arm 3, the control: threads= is the SORT's argument and must not touch this team.
        call build_fixture(d)
        call parquet_debug_set_table_threads_used(0_c_int64_t)
        call d%join(b, "k", how="left", threads=1)
        tab_threads1 = int(parquet_debug_get_table_threads_used())
        !
        call check(error, tab_free > 1, &
            "precondition: a detaching join must rewrite its columns in parallel here, or the " // &
            "cap below cannot be told from a floor that already declined")
        if (allocated(error)) return
        call check(error, tab_capped == 1, &
            "parquet_set_table_threads(1) must cap the join's column rewrite")
        if (allocated(error)) return
        call check(error, tab_threads1 > 1, &
            "threads=1 must NOT cap the column rewrite -- it sizes the engine that builds the " // &
            "match, and the two knobs are separate")
        if (allocated(error)) return
        !
        ! ---- Arm 4: the match half on the DEFAULT engine, the mirror of arm 3. This key under
        ! order="left" takes the hash engine: its multimap is built over the right keys through
        ! `pf_index_map%get_or_add_many`, which records the team it resolved in
        ! `parquet_debug_index_threads_used`, and probed with the left keys through `%probe_many`,
        ! which records its own in `parquet_debug_index_get_many_threads_used`. The threads=1 join
        ! first pins both records to 1 (see the doc-comment above). `nt` rather than a flat 4: both
        ! engines clamp an explicit request to `omp_get_num_procs()`, so the expectation carries the
        ! clamp rather than the test demanding a machine wide enough to avoid it.
        call build_fixture(e)
        call e%join(b, "k", how="left", threads=1)
        hash_build1 = parquet_debug_index_threads_used()
        hash_probe1 = parquet_debug_index_get_many_threads_used()
        call build_fixture(e)
        call e%join(b, "k", how="left", threads=nt)
        hash_build = parquet_debug_index_threads_used()
        hash_probe = parquet_debug_index_get_many_threads_used()
        call build_fixture(c)
        call parquet_set_table_threads(1)
        call c%join(b, "k", how="left", threads=nt)
        hash_build_cap = parquet_debug_index_threads_used()
        hash_probe_cap = parquet_debug_index_get_many_threads_used()
        call parquet_reset_settings()
        !
        call check(error, hash_build1 == 1 .and. hash_probe1 == 1, &
            "threads=1 must run the hash engine's build and probe serially, pinning both records " // &
            "to 1 -- or the assertions that follow could read a team an earlier join left behind")
        if (allocated(error)) return
        call check(error, hash_build == nt, &
            "threads= must reach the hash engine's build: the join hands the multimap over the " // &
            "right keys the resolved count, and its record has to show that count")
        if (allocated(error)) return
        call check(error, hash_probe == nt, &
            "threads= must reach the hash engine's probe: the multimap's probe_many over the " // &
            "left keys is handed the same count as the build")
        if (allocated(error)) return
        call check(error, hash_build_cap == nt .and. hash_probe_cap == nt, &
            "parquet_set_table_threads must NOT move the hash engine's build or probe team -- " // &
            "it caps the column work, and this is the mirror of arm 3")
        if (allocated(error)) return
        !
        ! ---- Arm 5: the same three joins on the SORT engine, which order="key" selects. The
        ! engine floor is lowered so a fixture of NROW rows reaches the engine at all, and restored
        ! before the assertions, because every `check` can return early and a leaked floor would
        ! rethread every later sort in this suite. The record is written in the engine's one
        ! shared body (`sort_build_permutation_impl`), so the threads=1 join pins it to 1.
        call parquet_debug_set_sort_engine_min_rows(1_int64)
        call build_fixture(e)
        call e%join(b, "k", how="left", order="key", threads=1)
        sort_one = parquet_debug_sort_threads_used()
        call build_fixture(e)
        call e%join(b, "k", how="left", order="key", threads=nt)
        sort_asked = parquet_debug_sort_threads_used()
        call build_fixture(c)
        call parquet_set_table_threads(1)
        call c%join(b, "k", how="left", order="key", threads=nt)
        sort_under_cap = parquet_debug_sort_threads_used()
        call parquet_reset_settings()
        call parquet_debug_set_sort_engine_min_rows(-1_int64)
        !
        call check(error, sort_one == 1_int64, &
            "threads=1 must run the pair-list sort serially, pinning its record to 1 -- or the " // &
            "assertions that follow could read a team an earlier sort left behind")
        if (allocated(error)) return
        call check(error, sort_asked == int(nt, int64), &
            "threads= must reach the pair-list sort: the join asks pf_argsort for group_offsets=, " // &
            "and that path has to be handed the resolved thread count, not merely resolve one")
        if (allocated(error)) return
        call check(error, sort_under_cap == int(nt, int64), &
            "parquet_set_table_threads must NOT move the sort team -- it caps the column work, " // &
            "and this is the mirror of arm 3")
#endif
    end subroutine test_join_thread_split
    !
    !> **The sort engine's passes over the runs, and the side-index passes of the apply, run on
    !! the join's team -- and give the serial answer.** Since feature_join.md stage 5 the sort
    !! engine resolves ONE team by the sort's own rule and runs the classification, the
    !! cardinality check, the counting, `matched=` and the left-order emission on it, and
    !! `join_apply` turns the pair list into gather indices on the same team. Nothing in a result
    !! can see any of that -- the pair list is identical at every team size -- so the two records
    !! `parquet_debug_get_join_group_threads_used` and `parquet_debug_get_join_side_threads_used`
    !! are the only observables, and the A/B against `threads=1` is what proves the threaded
    !! passes right (feature_risks.md Risk-189, Risk-220).
    !!
    !! **Five arms.** `threads=1` on the sort engine pins both records to 1; `threads=nt` records
    !! nt in both; `parquet_set_table_threads(1)` moves neither -- the mirror of the thread-split
    !! test's arm 3; the hash engine records 0 for the group passes it does not have and nt for
    !! the side index; and every `how` under both orderings, joined with `threads=nt`, equals the
    !! same join with `threads=1` -- rows, values, nulls, the carried column, `matched=` and both
    !! pair lists. The fixture has duplicate keys on both sides, a null key on each, and rows that
    !! match nothing on either, so every branch of the classification, the counting and the
    !! emission runs. The sort engine is forced through the hook for the `order="left"` arms (the
    !! automatic choice is the hash engine) and the tail floor is lowered so the passes over an
    !! anti join's few output rows still open the team; both are restored, and the A/B's verdict
    !! is carried in a string, so that no `check` can return with either still in force.
    subroutine test_join_group_passes_threads(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: NL = 3000, NR = 2000 !! rows a side; every branch below is reached.
        type(parquet_table) :: a, b, c
        integer(int32) :: lk(NL), ln(NL), rk(NR), rp(NR)
        logical :: lvalid(NL), rvalid(NR)
        integer :: nt, i, h, o
        integer(int64) :: g_one, s_one, g_nt, s_nt, g_cap, s_cap, g_hash, s_hash
        logical, allocatable :: gm(:), wm(:)
        integer(int64), allocatable :: gp(:), wp(:), gq(:), wq(:)
        character(len=8) :: hows(6), ords(2)
        character(len=:), allocatable :: fail
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: no pass here can open a team, so every record would " // &
            "read 1 and the A/B would compare serial against serial")
        return
#else
        nt = min(4, omp_get_num_procs())
        if (nt < 2) then
            call skip_test(error, "needs at least two processors: both engines clamp an explicit " // &
                "threads= to omp_get_num_procs(), so every arm would resolve to 1")
            return
        end if
#endif
        hows = [character(len=8) :: "inner", "left", "right", "outer", "semi", "anti"]
        ords = [character(len=8) :: "left", "key"]
        ! Left keys 0..1399, about twice each; right keys 300..1699, about once or twice each:
        ! so 0..299 match nothing on the left, 1400..1699 nothing on the right, the overlap has
        ! duplicates on both sides, and every 97th left and 89th right key is null.
        do i = 1, NL
            lk(i) = int(mod(i * 7919, 1400), int32)
            ln(i) = int(i, int32)
            lvalid(i) = mod(i, 97) /= 0
        end do
        do i = 1, NR
            rk(i) = int(300 + mod(i * 104729, 1400), int32)
            rp(i) = int(i, int32)
            rvalid(i) = mod(i, 89) /= 0
        end do
        call build_keyed_table(b, rk, "p", rp, rvalid)
        !
        ! Lowered and forced around every join, and restored BEFORE the first assertion.
        call parquet_debug_set_sort_tail_min_rows(1_int64)
        call parquet_debug_set_join_engine(1_c_int64_t)
        ! ---- Arm 1: threads=1 pins both records to 1, so every later read is discriminating.
        call build_keyed_table(a, lk, "n", ln, lvalid)
        call a%join(b, "k", how="left", threads=1)
        g_one = parquet_debug_get_join_group_threads_used()
        s_one = parquet_debug_get_join_side_threads_used()
        ! ---- Arm 2: threads=nt reaches both.
        call build_keyed_table(a, lk, "n", ln, lvalid)
        call a%join(b, "k", how="left", threads=nt)
        g_nt = parquet_debug_get_join_group_threads_used()
        s_nt = parquet_debug_get_join_side_threads_used()
        ! ---- Arm 3: the column cap moves neither.
        call build_keyed_table(a, lk, "n", ln, lvalid)
        call parquet_set_table_threads(1)
        call a%join(b, "k", how="left", threads=nt)
        g_cap = parquet_debug_get_join_group_threads_used()
        s_cap = parquet_debug_get_join_side_threads_used()
        call parquet_reset_settings()
        ! ---- Arm 4: the hash engine.
        call parquet_debug_set_join_engine(2_c_int64_t)
        call build_keyed_table(a, lk, "n", ln, lvalid)
        call a%join(b, "k", how="left", threads=nt)
        g_hash = parquet_debug_get_join_group_threads_used()
        s_hash = parquet_debug_get_join_side_threads_used()
        ! ---- Arm 5: the A/B, every how under both orderings, on the sort engine.
        call parquet_debug_set_join_engine(1_c_int64_t)
        fail = ""
        outer: do h = 1, 6
            do o = 1, 2
                call build_keyed_table(a, lk, "n", ln, lvalid)
                call a%join(b, "k", how=trim(hows(h)), order=trim(ords(o)), threads=nt, &
                    matched=gm, pairs=gp, other_pairs=gq)
                call build_keyed_table(c, lk, "n", ln, lvalid)
                call c%join(b, "k", how=trim(hows(h)), order=trim(ords(o)), threads=1, &
                    matched=wm, pairs=wp, other_pairs=wq)
                call compare_joined(a, c, gm, wm, gp, wp, gq, wq, &
                    "how=" // trim(hows(h)) // " order=" // trim(ords(o)), fail)
                if (len(fail) > 0) exit outer
            end do
        end do outer
        call parquet_debug_set_join_engine(0_c_int64_t)
        call parquet_debug_set_sort_tail_min_rows(-1_int64)
        !
        call check(error, g_one == 1_int64 .and. s_one == 1_int64, &
            "threads=1 must run the sort engine's group passes and the side-index passes " // &
            "serially, pinning both records to 1 -- or the reads that follow could see a team " // &
            "an earlier join left behind")
        if (allocated(error)) return
        call check(error, g_nt == int(nt, int64), &
            "threads= must reach the sort engine's group passes: the engine resolves the team " // &
            "the sort's own rule gives and runs the classification, the counting, the " // &
            "cardinality check and the emission on it")
        if (allocated(error)) return
        call check(error, s_nt == int(nt, int64), &
            "threads= must reach the side-index passes: join_apply is handed the engine's team")
        if (allocated(error)) return
        call check(error, g_cap == int(nt, int64) .and. s_cap == int(nt, int64), &
            "parquet_set_table_threads must NOT move the group passes' team or the side index's " // &
            "-- it caps the column work, and this is the mirror of the thread-split test's arm 3")
        if (allocated(error)) return
        call check(error, g_hash == 0_int64, &
            "a hash-engine join must record 0 for the group passes it does not have -- a team " // &
            "left by an earlier sort-engine join would otherwise read as this join's")
        if (allocated(error)) return
        call check(error, s_hash == int(nt, int64), &
            "the side-index passes must run on the hash engine's team too: join_apply is handed " // &
            "that engine's team as it is handed the sort engine's")
        if (allocated(error)) return
        call check(error, len(fail) == 0, &
            "the threaded sort-engine join must equal the serial one: " // fail)
    end subroutine test_join_group_passes_threads
    !
    !> A two-column int32 table -- the key `k`, nulled where `valid` is .false., and one more
    !! column -- for the group-pass test's two sides.
    subroutine build_keyed_table(t, keys, oname, other, valid)
        type(parquet_table), intent(out) :: t         !! receives the table.
        integer(int32), intent(in) :: keys(:)         !! the `k` column's values.
        character(len=*), intent(in) :: oname         !! the other column's name.
        integer(int32), intent(in) :: other(:)        !! its values.
        logical, intent(in) :: valid(:)               !! validity of `k`, per row.
        !
        call parquet_new_table(t)
        call t%add_column("k", keys)
        call t%add_column(oname, other)
        call t%set_null("k", valid)
    end subroutine build_keyed_table
    !
    !> Compares two joined tables of `build_keyed_table` rows and everything a join reports
    !! beside them: row and column counts, every column of the three that is present (values
    !! where valid, and the validity itself), `matched=`, `pairs=` and `other_pairs=`. `fail`
    !! is empty when they agree and names the first difference otherwise -- a string rather
    !! than a `check`, so the caller can restore its hooks before asserting.
    subroutine compare_joined(got, want, gm, wm, gp, wp, gq, wq, what, fail)
        type(parquet_table), intent(inout) :: got     !! the table joined on a team.
        type(parquet_table), intent(inout) :: want    !! the table joined serially.
        logical, intent(in) :: gm(:), wm(:)           !! the two `matched=` answers.
        integer(int64), intent(in) :: gp(:), wp(:)    !! the two `pairs=` answers.
        integer(int64), intent(in) :: gq(:), wq(:)    !! the two `other_pairs=` answers.
        character(len=*), intent(in) :: what          !! the arm, for the message.
        character(len=:), allocatable, intent(out) :: fail !! empty, or the first difference.
        character(len=1), parameter :: cols(3) = ["k", "n", "p"]
        integer(int32), allocatable :: gv(:), wv(:)
        logical, allocatable :: gvalid(:), wvalid(:)
        integer :: j
        !
        fail = ""
        if (got%nrows() /= want%nrows()) then
            fail = what // ": the row counts differ"
            return
        end if
        if (got%ncols() /= want%ncols()) then
            fail = what // ": the column counts differ"
            return
        end if
        do j = 1, size(cols)
            if (.not. want%has_column(cols(j))) cycle
            if (.not. got%has_column(cols(j))) then
                fail = what // ": column " // cols(j) // " is missing on the team"
                return
            end if
            call got%get(cols(j), gv, is_valid=gvalid)
            call want%get(cols(j), wv, is_valid=wvalid)
            if (any(gvalid .neqv. wvalid)) then
                fail = what // ": the nulls of column " // cols(j) // " differ"
                return
            end if
            if (any(gv /= wv .and. gvalid)) then
                fail = what // ": the values of column " // cols(j) // " differ"
                return
            end if
        end do
        if (size(gm) /= size(wm)) then
            fail = what // ": matched= has a different length"
            return
        end if
        if (any(gm .neqv. wm)) then
            fail = what // ": matched= differs"
            return
        end if
        if (size(gp) /= size(wp) .or. size(gq) /= size(wq)) then
            fail = what // ": the pair lists have different lengths"
            return
        end if
        if (any(gp /= wp) .or. any(gq /= wq)) then
            fail = what // ": the pair lists differ"
            return
        end if
    end subroutine compare_joined
    !
    !> **Where a gather's team goes is a rule, and this pins it in both directions.** `colwork_plan`
    !! (`src/parquet_tables_parallel.f90`) spends a gather's team ACROSS the columns when there are
    !! at least as many columns as threads, and otherwise INSIDE each column in turn, its rows
    !! across the whole team. The second level is what lets a join carrying one column use the
    !! machine at all; the first is the shape every other mutation has always had. Nothing in a
    !! result can tell the two apart -- the rows come out the same at every team size -- so the
    !! only observables are the two records the plan writes on every mutation: the team's size and
    !! the level it went to. A plan that always chose one level, or none, passes every equality
    !! test in this suite and fails here.
    !!
    !! The team is fixed at two through `parquet_set_table_threads(2)` so the arms hold on any
    !! machine with two processors: five columns are at least two (across), one column is not
    !! (within). The work floor is lowered for the one-column tables, whose 20001 rows sit under
    !! it. The forcing hook (`parquet_debug_set_colwork_level`) is then shown to pick only a level
    !! the operation has -- across on one column has no second column to give a thread to and
    !! stays serial; within on a sort's replay does not exist and stays across -- and the serial
    !! cap is shown to write the level record too, so no arm can read a record an earlier one
    !! left. The last arm is the CARRY side: a non-detaching join carrying one column takes the
    !! same rule through `table_colwork_join`. Every threaded table is compared with a serially
    !! capped one, since a level that gathered the wrong rows would otherwise be the fast wrong
    !! answer this whole file exists to exclude.
    subroutine test_colwork_level_rule(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c, r, s, ser
        integer(int32) :: k(NROW), rk(NROW + 1), n(NROW)
        integer :: used, level, i
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: colwork_plan resolves inside #ifdef _OPENMP, so every arm " // &
            "below would read a serial plan and the rule would hold for the wrong reason")
        return
#else
        if (omp_get_num_procs() < 2 .or. omp_get_max_threads() < 2) then
            call skip_test(error, "needs two processors and OMP_NUM_THREADS >= 2: the team is capped at " // &
                "two, and a team of one has no level to choose")
            return
        end if
        call fill_key(k)
        rk(1:NROW) = k
        rk(NROW + 1) = k(1)
        call build_key_table(b, rk)
        do i = 1, NROW
            n(i) = i
        end do
        !
        ! ---- Arm 1: five columns on a team of two -- the columns can occupy the team: ACROSS.
        call build_fixture(a)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call a%join(b, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_set_table_threads(1)
        call build_fixture(ser)
        call ser%join(b, "k", how="left")
        call parquet_reset_settings()
        call check(error, level == 1 .and. used == 2, &
            "five columns on a team of two must be rewritten ACROSS the columns, two at a time")
        if (allocated(error)) return
        call check_tables_identical(error, a, ser, "join rewritten across columns")
        if (allocated(error)) return
        !
        ! ---- Arm 2: one column on a team of two -- it cannot occupy the team: WITHIN.
        call parquet_debug_set_colwork_min_elements(1000_c_int64_t)
        call build_key_table(c, k)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call c%join(b, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_set_table_threads(1)
        call build_key_table(ser, k)
        call ser%join(b, "k", how="left")
        call parquet_reset_settings()
        call check(error, level == 2 .and. used == 2, &
            "one column on a team of two must be gathered WITHIN the column, its rows across both threads")
        if (allocated(error)) return
        call check_key_tables_identical(error, c, ser, "join rewritten within the column", with_p=.false.)
        if (allocated(error)) return
        !
        ! ---- Arm 3: the hook forces across on the one-column table. There is no second column to
        ! give a thread to, so the plan is serial: the hook chooses a level, it never opens a team
        ! the gate declined.
        call build_key_table(c, k)
        call parquet_debug_set_colwork_level(1_c_int64_t)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call c%join(b, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_debug_set_colwork_level(0_c_int64_t)
        call parquet_reset_settings()
        call check(error, level == 0 .and. used == 1, &
            "forcing the across level on a one-column table must leave the rewrite serial")
        if (allocated(error)) return
        !
        ! ---- Arm 4: the hook forces within on the five-column table: the team goes inside each
        ! column in turn, and the rows still come out right.
        call build_fixture(a)
        call parquet_debug_set_colwork_level(2_c_int64_t)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call a%join(b, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_debug_set_colwork_level(0_c_int64_t)
        call parquet_set_table_threads(1)
        call build_fixture(ser)
        call ser%join(b, "k", how="left")
        call parquet_reset_settings()
        call check(error, level == 2 .and. used == 2, &
            "forcing the within level on a five-column table must spend the team inside each column")
        if (allocated(error)) return
        call check_tables_identical(error, a, ser, "join forced within the column")
        if (allocated(error)) return
        !
        ! ---- Arm 5: a sort's replay has no within-column level, so forced within it stays across.
        call build_fixture(s)
        call parquet_debug_set_colwork_level(2_c_int64_t)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call s%sort_by(["k"])
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_debug_set_colwork_level(0_c_int64_t)
        call parquet_reset_settings()
        call check(error, level == 1 .and. used == 2, &
            "a sort's replay has no within-column level, so the hook must leave it across columns")
        if (allocated(error)) return
        !
        ! ---- Arm 6: the serial cap writes the level record too, or an earlier arm's record could
        ! stand in for a later one.
        call build_fixture(a)
        call parquet_set_table_threads(1)
        call reset_level_records()
        call a%join(b, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_reset_settings()
        call check(error, level == 0 .and. used == 1, "a serial rewrite must record level 0 and a team of 1")
        if (allocated(error)) return
        !
        ! ---- Arm 7: the CARRY side. `r` holds every key of `k` once, in order, so the join keeps
        ! this table's rows in place and the only mutation is the carry of `r`'s one payload
        ! column through table_colwork_join -- one column on a team of two: WITHIN.
        call parquet_new_table(r)
        call r%add_column("k", k)
        call r%add_column("p", n)
        call build_key_table(c, k)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call c%join(r, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_set_table_threads(1)
        call build_key_table(ser, k)
        call ser%join(r, "k", how="left")
        call parquet_reset_settings()
        call parquet_debug_set_colwork_min_elements(0_c_int64_t)
        call check(error, level == 2 .and. used == 2, &
            "a join carrying one column on a team of two must gather it within the column")
        if (allocated(error)) return
        call check_key_tables_identical(error, c, ser, "join carrying one column within the column", with_p=.true.)
        if (allocated(error)) return
        !
        ! ---- Arm 8: the work floor sees the OUTPUT rows. The right table has 100 rows, far under a
        ! floor of 5000 elements, and the join produces NROW rows, far over it -- a floor measured
        ! on the source alone would keep this rewrite serial, which is exactly what kept a
        ! lookup join's 10M-row carry serial before stage 4 of feature_join.md.
        call parquet_new_table(r)
        call r%add_column("k", k(1:100))
        call r%add_column("p", n(1:100))
        call build_key_table(c, k)
        call parquet_debug_set_colwork_min_elements(5000_c_int64_t)
        call parquet_set_table_threads(2)
        call reset_level_records()
        call c%join(r, "k", how="left")
        used = int(parquet_debug_get_table_threads_used())
        level = int(parquet_debug_get_table_level_used())
        call parquet_set_table_threads(1)
        call build_key_table(ser, k)
        call ser%join(r, "k", how="left")
        call parquet_reset_settings()
        call parquet_debug_set_colwork_min_elements(0_c_int64_t)
        call check(error, level == 2 .and. used == 2, &
            "the work floor must be measured on the rows the carry PRODUCES, not on the 100-row source")
        if (allocated(error)) return
        call check_key_tables_identical(error, c, ser, "join carrying one column from a small source", with_p=.true.)
#endif
    end subroutine test_colwork_level_rule
    !
    !> `check_tables_identical` for the one-column tables of `test_colwork_level_rule`: the key
    !! column, and with `with_p` the carried `p` column too, values and nulls.
    subroutine check_key_tables_identical(error, got, want, what, with_p)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table), intent(inout) :: got           !! the table rewritten on a team.
        type(parquet_table), intent(inout) :: want          !! the reference, rewritten serially.
        character(len=*), intent(in) :: what                !! operation name, for the messages.
        logical, intent(in) :: with_p                       !! whether both tables carry a `p` column.
        integer(int32), allocatable :: gk(:), wk(:), gp(:), wp(:)
        logical, allocatable :: gm(:), wm(:)
        !
        call check(error, got%nrows() == want%nrows(), &
            "%" // what // " must leave the same row count on both paths")
        if (allocated(error)) return
        call got%get("k", gk)
        call want%get("k", wk)
        call check(error, all(gk == wk), "%" // what // ": the key column must match the serial result")
        if (allocated(error)) return
        if (.not. with_p) return
        call got%get("p", gp, is_valid=gm)
        call want%get("p", wp, is_valid=wm)
        call check(error, all(gp == wp) .and. all(gm .eqv. wm), &
            "%" // what // ": the carried column must match the serial result, nulls included")
    end subroutine check_key_tables_identical
    !
    !> Clears both records to values no mutation writes (-1), so an arm that never reached the
    !! plan reads as neither serial nor threaded rather than as whichever the previous arm left.
    subroutine reset_level_records()
        call parquet_debug_set_table_threads_used(-1_c_int64_t)
        call parquet_debug_set_table_level_used(-1_c_int64_t)
    end subroutine reset_level_records
    !
    !> A one-column int32 table, for the join thread test's right-hand side.
    subroutine build_key_table(t, keys)
        type(parquet_table), intent(out) :: t !! receives the table.
        integer(int32), intent(in) :: keys(:) !! the `k` column's values.
        !
        call parquet_new_table(t)
        call t%add_column("k", keys)
    end subroutine build_key_table
    !
    ! ==================================================================================
    ! %apply's team
    ! ==================================================================================
    !
    !> `%apply`'s team, and the A/B on it. Absent `threads=` is a DECISION for serial and records
    !! 1; `threads=1` records 1; `threads=nt` records nt on the procedure and the object form
    !! alike; and the table tier's cap (`parquet_set_table_threads(1)`) does not move an
    !! explicit request, which is the caller's re-entrancy declaration rather than a ceiling to
    !! negotiate. Then every form under `threads=nt` equals its serial run bit for bit: the
    !! fixture's groups are unequal in size and its payload row-distinct, so a result stored at
    !! the wrong group under the dynamic schedule is a wrong VALUE -- a defect of that kind
    !! shows intermittently, so this test's power against it is statistical (run it several
    !! times when hunting one). The callbacks read the table through `%col` pointers taken
    !! before the calls, on every thread. This suite runs its tests with no enclosing region,
    !! so the team opened here is a real one.
    subroutine test_apply_group_team(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: N = 20000 !! rows; about five hundred groups of unequal size.
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        type(par_sum_reducer) :: red
        integer(int32) :: k(N)
        real(real64) :: v(N)
        real(real64), allocatable :: s_abs(:), s_one(:), s_team(:), s_cap(:), o_one(:), o_team(:)
        real(real64), allocatable :: m_one(:, :), m_team(:, :), om_one(:, :), om_team(:, :)
        integer(int64) :: r_abs, r_one, r_team, r_cap, r_obj
        integer :: nt, i
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: no explicit threads= can open a team, so every record " // &
            "would read 1 and the A/B would compare serial against serial")
        return
#else
        nt = min(4, omp_get_num_procs())
        if (nt < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is clamped " // &
                "to omp_get_num_procs(), so every arm would resolve to 1")
            return
        end if
#endif
        ! Keys 0..499 whose group sizes grow with the key (the square root of a spread residue),
        ! so the dynamic schedule has something to balance; a row-distinct payload.
        do i = 1, N
            k(i) = int(sqrt(real(mod(i * 7919, 250000), real64)), int32)
            v(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("v", v)
        call t%col("v", par_apply_p)
        call t%col("v", red%p)
        call t%group_by("k", grp)
        ! ---- the records: cleared to a value no loop writes, then one arm each.
        call parquet_debug_set_group_threads_used(-1_c_int64_t)
        call grp%apply(par_cb_sum, s_abs)
        r_abs = parquet_debug_get_group_threads_used()
        call grp%apply(par_cb_sum, s_one, threads=1)
        r_one = parquet_debug_get_group_threads_used()
        call grp%apply(par_cb_sum, s_team, threads=nt)
        r_team = parquet_debug_get_group_threads_used()
        call parquet_set_table_threads(1)
        call grp%apply(par_cb_sum, s_cap, threads=nt)
        r_cap = parquet_debug_get_group_threads_used()
        call parquet_reset_settings()
        call grp%apply(red, o_team, threads=nt)
        r_obj = parquet_debug_get_group_threads_used()
        ! ---- the A/B: the other three forms, serial and on the team.
        call grp%apply(red, o_one)
        call grp%apply(par_cb_two, 2, m_one)
        call grp%apply(par_cb_two, 2, m_team, threads=nt)
        call grp%apply(red, 2, om_one)
        call grp%apply(red, 2, om_team, threads=nt)
        call check(error, grp%ngroups() > 100_int64, "the fixture has a few hundred groups")
        if (allocated(error)) return
        call check(error, r_abs == 1_int64, "absent threads= runs serially and says so")
        if (allocated(error)) return
        call check(error, r_one == 1_int64, "threads=1 runs serially and says so")
        if (allocated(error)) return
        call check(error, r_team == int(nt, int64), "threads=nt opens a team of nt on the procedure form")
        if (allocated(error)) return
        call check(error, r_cap == int(nt, int64), "the table-tier cap does not move an explicit threads=")
        if (allocated(error)) return
        call check(error, r_obj == int(nt, int64), "threads=nt opens a team of nt on the object form")
        if (allocated(error)) return
        call check(error, all(s_team == s_abs) .and. all(s_one == s_abs) .and. all(s_cap == s_abs), &
            "the one-value procedure form: the team's answer equals the serial one, bit for bit")
        if (allocated(error)) return
        call check(error, all(o_team == o_one) .and. all(o_one == s_abs), &
            "the one-value object form: the same, and equal to the procedure form")
        if (allocated(error)) return
        call check(error, all(m_team == m_one) .and. all(m_one(1, :) == s_abs), &
            "the matrix procedure form: the same, its first row the one-value answer")
        if (allocated(error)) return
        call check(error, all(om_team == om_one) .and. all(om_one == m_one), &
            "the matrix object form: the same, and equal to the procedure form")
    end subroutine test_apply_group_team
    !
    !> `%agg`'s team, and the A/B on it. Absent `threads=` is AUTOMATIC on the token form --
    !! the record reads at least 1, and reads 1 under `parquet_set_table_threads(1)`, the cap the
    !! automatic rule takes -- while `threads=1` records 1 and `threads=nt` records nt; on the
    !! procedure form absent is serial and `threads=nt` is nt. Then six tokens under
    !! `threads=nt` equal their `threads=1` run bit for bit (a NaN against a NaN counts as equal)
    !! over about five hundred unequal groups with nulls and NaNs scattered through them, and so
    !! does the procedure form: each group's statistic is computed by one thread on one buffer,
    !! and the pairing of groups with threads cannot change a bit. Serial in this suite, so the
    !! team is real.
    subroutine test_agg_group_team(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: N = 20000 !! rows; about five hundred groups of unequal size.
        character(len=8), parameter :: tokens(6) = [character(len=8) :: "mean", "median", "std", "quantile", &
            "sum", "count"]
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int32) :: k(N)
        real(real64) :: v(N)
        real(real64), allocatable :: one(:), team(:), f_one(:), f_team(:)
        integer(int64) :: r_abs, r_cap, r_one, r_team, r_fabs, r_fteam
        integer :: nt, i, tk
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: no team can open, so every record would read 1 and the A/B " // &
            "would compare serial against serial")
        return
#else
        nt = min(4, omp_get_num_procs())
        if (nt < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is clamped " // &
                "to omp_get_num_procs(), so every arm would resolve to 1")
            return
        end if
#endif
        do i = 1, N
            k(i) = int(sqrt(real(mod(i * 7919, 250000), real64)), int32)
            v(i) = real(mod(i * 31, 997), real64) + real(i, real64) * 1.0e-6_real64
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("v", v)
        do i = 1, N, 37
            call t%set_null("v", int(i, int64))
        end do
        do i = 5, N, 53
            call t%set_element("v", int(i, int64), ieee_value(0.0_real64, ieee_quiet_nan))
        end do
        call t%group_by("k", grp)
        ! ---- the records, one arm each.
        call parquet_debug_set_group_threads_used(-1_c_int64_t)
        call grp%agg("v", "mean", one)
        r_abs = parquet_debug_get_group_threads_used()
        call parquet_set_table_threads(1)
        call grp%agg("v", "mean", one)
        r_cap = parquet_debug_get_group_threads_used()
        call parquet_reset_settings()
        call grp%agg("v", "mean", one, threads=1)
        r_one = parquet_debug_get_group_threads_used()
        call grp%agg("v", "mean", team, threads=nt)
        r_team = parquet_debug_get_group_threads_used()
        call grp%agg("v", par_col_mean, f_one)
        r_fabs = parquet_debug_get_group_threads_used()
        call grp%agg("v", par_col_mean, f_team, threads=nt)
        r_fteam = parquet_debug_get_group_threads_used()
        call check(error, grp%ngroups() > 100_int64, "the fixture has a few hundred groups")
        if (allocated(error)) return
        call check(error, r_abs >= 1_int64, "absent threads= resolves automatically on the token form")
        if (allocated(error)) return
        call check(error, r_cap == 1_int64, "parquet_set_table_threads(1) caps the automatic answer to 1")
        if (allocated(error)) return
        call check(error, r_one == 1_int64 .and. r_team == int(nt, int64), "threads=1 records 1, threads=nt records nt")
        if (allocated(error)) return
        call check(error, r_fabs == 1_int64 .and. r_fteam == int(nt, int64), &
            "the procedure form: absent is serial, threads=nt is nt")
        if (allocated(error)) return
        call check(error, all(f_team == f_one .or. (ieee_is_nan(f_team) .and. ieee_is_nan(f_one))), &
            "the procedure form on a team equals its serial run")
        if (allocated(error)) return
        do tk = 1, size(tokens)
            if (tokens(tk) == "quantile") then
                call grp%agg("v", trim(tokens(tk)), one, q=0.25_real64, threads=1)
                call grp%agg("v", trim(tokens(tk)), team, q=0.25_real64, threads=nt)
            else
                call grp%agg("v", trim(tokens(tk)), one, threads=1)
                call grp%agg("v", trim(tokens(tk)), team, threads=nt)
            end if
            if (.not. all(team == one .or. (ieee_is_nan(team) .and. ieee_is_nan(one)))) then
                call check(error, .false., trim(tokens(tk)) // " on a team differs from its serial run")
                return
            end if
        end do
        call check(error, .true., "every token agreed at both thread counts")
    end subroutine test_agg_group_team
    !
    !> `%add_apply`'s team, and the A/B on it. The binding forwards `threads=` to the same
    !! `%apply` loop and nothing else, so the contract is `%apply`'s: absent is a DECISION for
    !! serial and records 1, `threads=1` records 1, and `threads=nt` records nt on the procedure
    !! and the object form alike -- while `%add_agg`'s token form, which forwards to `%agg`'s
    !! loop, resolves automatically when absent. Then every column written on the team equals the
    !! one written serially, bit for bit: the fixture's groups are unequal and its payload
    !! row-distinct, so a result stored at the wrong group is a wrong VALUE (a defect of that
    !! kind shows intermittently, so this test's power against it is statistical). The target is
    !! the grouping's own key table, reserved wide enough that no add relocates a slot.
    subroutine test_add_apply_group_team(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: N = 20000 !! rows; about five hundred groups of unequal size.
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        type(par_sum_reducer) :: red
        integer(int32) :: k(N)
        real(real64) :: v(N)
        real(real64), allocatable :: a1(:), a2(:), b1(:), b2(:)
        integer(int64) :: r_abs, r_one, r_team, r_obj, r_agg
        integer :: nt, i
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: no explicit threads= can open a team, so every record " // &
            "would read 1 and the A/B would compare serial against serial")
        return
#else
        nt = min(4, omp_get_num_procs())
        if (nt < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is clamped " // &
                "to omp_get_num_procs(), so every arm would resolve to 1")
            return
        end if
#endif
        do i = 1, N
            k(i) = int(sqrt(real(mod(i * 7919, 250000), real64)), int32)
            v(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("v", v)
        call t%col("v", par_apply_p)
        call t%col("v", red%p)
        call t%group_by("k", grp)
        call grp%key_table(kt, reserve=16)
        ! ---- the records: cleared to a value no loop writes, then one arm each.
        call parquet_debug_set_group_threads_used(-1_c_int64_t)
        call grp%add_apply(par_cb_two, kt, "s1,s2")
        r_abs = parquet_debug_get_group_threads_used()
        call grp%add_apply(par_cb_two, kt, "o1,o2", threads=1)
        r_one = parquet_debug_get_group_threads_used()
        call grp%add_apply(par_cb_two, kt, "t1,t2", threads=nt)
        r_team = parquet_debug_get_group_threads_used()
        call grp%add_apply(red, kt, "r1,r2")
        call grp%add_apply(red, kt, "p1,p2", threads=nt)
        r_obj = parquet_debug_get_group_threads_used()
        call grp%add_agg("v", "mean", kt, "am")
        r_agg = parquet_debug_get_group_threads_used()
        call check(error, grp%ngroups() > 100_int64, "the fixture has a few hundred groups")
        if (allocated(error)) return
        call check(error, r_abs == 1_int64, "absent threads= runs serially and says so")
        if (allocated(error)) return
        call check(error, r_one == 1_int64, "threads=1 runs serially and says so")
        if (allocated(error)) return
        call check(error, r_team == int(nt, int64), "threads=nt opens a team of nt on the procedure form")
        if (allocated(error)) return
        call check(error, r_obj == int(nt, int64), "threads=nt opens a team of nt on the object form")
        if (allocated(error)) return
        call check(error, r_agg >= 1_int64, "%add_agg's token form resolves automatically when threads= is absent")
        if (allocated(error)) return
        ! ---- the A/B, read back from the target: every column the team wrote equals the serial one.
        call kt%get("s1", a1)
        call kt%get("s2", a2)
        call kt%get("t1", b1)
        call kt%get("t2", b2)
        call check(error, all(b1 == a1) .and. all(b2 == a2), &
            "the procedure form on a team writes the serial columns, bit for bit")
        if (allocated(error)) return
        call kt%get("o1", b1)
        call kt%get("o2", b2)
        call check(error, all(b1 == a1) .and. all(b2 == a2), "and so does threads=1")
        if (allocated(error)) return
        call kt%get("r1", b1)
        call kt%get("r2", b2)
        call check(error, all(b1 == a1) .and. all(b2 == a2), "the object form serially: the same columns again")
        if (allocated(error)) return
        call kt%get("p1", b1)
        call kt%get("p2", b2)
        call check(error, all(b1 == a1) .and. all(b2 == a2), "...and on the team")
        if (allocated(error)) return
        call check(error, kt%nrows() == grp%ngroups(), "every column landed on the key table's one row per group")
    end subroutine test_add_apply_group_team

    !> `%broadcast`'s team, and the A/B on it: absent `threads=` is automatic (the record reads
    !! at least 1, and 1 under `parquet_set_table_threads(1)`), `threads=1` records 1 and
    !! `threads=nt` records nt; then the int64 broadcast of `%size` and the real64 broadcast of
    !! an `%agg` mean under `threads=nt` equal their serial runs bit for bit over about five
    !! hundred unequal groups with dropped rows -- every grouped row is written once, by its
    !! own group, and the fill before the team opens. Serial in this suite, so the team is real.
    subroutine test_broadcast_group_team(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: N = 20000 !! rows; about five hundred groups of unequal size.
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int32) :: k(N)
        real(real64) :: v(N)
        integer(int64), allocatable :: counts(:), one(:), team(:), codes(:)
        real(real64), allocatable :: means(:), one_r(:), team_r(:)
        integer(int64) :: r_abs, r_cap, r_one, r_team
        integer :: nt, i
        !
#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: no team can open, so every record would read 1 and the A/B " // &
            "would compare serial against serial")
        return
#else
        nt = min(4, omp_get_num_procs())
        if (nt < 2) then
            call skip_test(error, "needs at least two processors: an explicit threads= is clamped " // &
                "to omp_get_num_procs(), so every arm would resolve to 1")
            return
        end if
#endif
        do i = 1, N
            k(i) = int(sqrt(real(mod(i * 7919, 250000), real64)), int32)
            v(i) = real(mod(i * 31, 997), real64) + real(i, real64) * 1.0e-6_real64
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("v", v)
        do i = 1, N, 41
            call t%set_null("k", int(i, int64))
        end do
        call t%group_by("k", grp)
        call grp%size(counts)
        call grp%group_ids(codes)
        ! ---- the records, one arm each.
        call parquet_debug_set_group_threads_used(-1_c_int64_t)
        call grp%broadcast(counts, one)
        r_abs = parquet_debug_get_group_threads_used()
        call parquet_set_table_threads(1)
        call grp%broadcast(counts, one)
        r_cap = parquet_debug_get_group_threads_used()
        call parquet_reset_settings()
        call grp%broadcast(counts, one, threads=1)
        r_one = parquet_debug_get_group_threads_used()
        call grp%broadcast(counts, team, threads=nt)
        r_team = parquet_debug_get_group_threads_used()
        call check(error, grp%ngroups() > 100_int64 .and. count(codes == 0_int64) > 0, &
            "the fixture has a few hundred groups and some dropped rows")
        if (allocated(error)) return
        call check(error, r_abs >= 1_int64, "absent threads= resolves automatically")
        if (allocated(error)) return
        call check(error, r_cap == 1_int64, "parquet_set_table_threads(1) caps the automatic answer to 1")
        if (allocated(error)) return
        call check(error, r_one == 1_int64 .and. r_team == int(nt, int64), "threads=1 records 1, threads=nt records nt")
        if (allocated(error)) return
        call check(error, all(team == one), "the int64 broadcast on a team equals its serial run")
        if (allocated(error)) return
        call grp%agg("v", "mean", means)
        call grp%broadcast(means, one_r, fill=-1.0_real64, threads=1)
        call grp%broadcast(means, team_r, fill=-1.0_real64, threads=nt)
        call check(error, all(team_r == one_r) .and. count(one_r == -1.0_real64) == count(codes == 0_int64), &
            "the real64 broadcast on a team equals its serial run, the fill on exactly the dropped rows")
    end subroutine test_broadcast_group_team
    !
    !> The mean of a group's valid values, for the procedure form's team arm.
    function par_col_mean(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! the mean of the valid values.
        !
        call pf_mean(values, r, is_valid=is_valid, weights=weights)
    end function par_col_mean
    !
    !> The payload's sum over the group, through `par_apply_p`.
    function par_cb_sum(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! the sum.
        !
        r = sum(par_apply_p(rows))
        if (g < 1_int64) r = -huge(r)   ! a group number below 1 is a library defect: poison the answer
    end function par_cb_sum
    !
    !> The payload's sum over the group and the group's size.
    subroutine par_cb_two(g, rows, out)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64), intent(out) :: out(:)   !! receives the two values.
        !
        out(1) = sum(par_apply_p(rows))
        out(2) = real(size(rows), real64)
        if (g < 1_int64) out(1) = -huge(out(1))
    end subroutine par_cb_two
    !
    !> `par_sum_reducer%reduce`; see the type.
    subroutine par_sum_reduce(self, g, rows, out)
        class(par_sum_reducer), intent(in) :: self !! the reducer.
        integer(int64), intent(in) :: g            !! the group number.
        integer(int64), intent(in) :: rows(:)      !! the group's rows.
        real(real64), intent(out) :: out(:)        !! receives the results.
        !
        out(1) = sum(self%p(rows))
        if (size(out) > 1) out(2) = real(size(rows), real64)
        if (g < 1_int64) out(1) = -huge(out(1))
    end subroutine par_sum_reduce
    !
end module test_table_parallel
