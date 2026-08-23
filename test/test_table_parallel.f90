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
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads, omp_get_num_threads
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
                test_concurrent_set_null_shares_block) &
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
        ! is what makes cloning a shared table safe to thread at all (colwork_threads' own note).
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
        seen_threads = omp_get_num_threads()
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
end module test_table_parallel
