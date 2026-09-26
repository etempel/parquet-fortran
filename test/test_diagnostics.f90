!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the maintainer-only PHASE TIMERS in `src/parquet_wrapper.cpp` -- the
!> `parquet_debug_*_nanos` family that says which part of an operation the time went into.
!>
!> Nothing in the library reads these, and no user API exposes them: they exist so that
!> "X is slow" can be turned into "X is slow HERE" by one benchmark run rather than a fresh
!> investigation (CLAUDE.md, "Instrument phases before optimising a multi-phase operation").
!> That is exactly why they need tests. **A phase timer that silently records nothing is not a
!> visible failure** -- it looks like a phase that costs nothing, which is the most misleading
!> answer a diagnostic can give, and the next person to reach for it would spend the
!> investigation the timer was added to prevent. The library builds and every other test passes
!> whether or not a single `charge_phase` call is still wired up.
!>
!> So each test here asserts that a timer MOVES across the operation it claims to measure. What
!> it cannot assert is a duration: a timer is not a promise about how long anything takes, and an
!> upper bound would be a flaky test on a loaded machine. Movement is the whole contract.
!>
!> **This suite runs its tests SEQUENTIALLY** (`test/run_tester.f90`'s
!> `suite_is_safe_to_parallelize` excludes it) and it cannot work any other way. Every counter
!> here is a process-global `static` that ACCUMULATES, so a sibling test doing a filtered read or
!> a sort in the same window would add to the very counter under assertion -- which does not fail,
!> it passes, against a timer that recorded nothing itself. That is this project's worst failure
!> mode, and the reason the reset-then-observe shape below is only sound in a serial suite.
!>
!> Every test writes its own fixture under test_run/, with its own filename, per CLAUDE.md.
module test_diagnostics
    use parquet
    ! The C++ sort engine is TEST-ONLY and is not re-exported by the `parquet` facade: reaching it
    ! needs this import, which is what keeps it out of every other program's dependency graph.
    ! Importing it is also what BINDS it -- `parquet_debug_use_fortran_sort_engine` lives here and
    ! registers the engine's entry points as a side effect of being called.
    use parquet_sorting_oracle, only : parquet_debug_use_fortran_sort_engine
    use iso_fortran_env, only : int32, int64, real64
    use iso_c_binding, only : c_int, c_long_long, c_int64_t
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_diagnostics
    !
    !> The phase timers themselves. Declared locally here rather than in `src/parquet_bindings.f90`
    !> -- the same convention every other `parquet_debug_*` hook follows, so that no debug entry
    !> point ever becomes part of the library's own interface.
    interface
        !> Zeroes the three `parquet_reader_set_filter` phase timers.
        subroutine parquet_debug_reset_filter_phase_nanos() &
            bind(C, name="parquet_debug_reset_filter_phase_nanos")
        end subroutine parquet_debug_reset_filter_phase_nanos
        !> Nanoseconds the last filter installs spent reading their key columns.
        function parquet_debug_get_filter_decode_nanos() result(res) &
            bind(C, name="parquet_debug_get_filter_decode_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated decode nanoseconds.
        end function parquet_debug_get_filter_decode_nanos
        !> Nanoseconds spent evaluating clauses into Kleene values.
        function parquet_debug_get_filter_eval_nanos() result(res) &
            bind(C, name="parquet_debug_get_filter_eval_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated evaluation nanoseconds.
        end function parquet_debug_get_filter_eval_nanos
        !> Nanoseconds spent collapsing those values into the final row mask.
        function parquet_debug_get_filter_mask_nanos() result(res) &
            bind(C, name="parquet_debug_get_filter_mask_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated mask-build nanoseconds.
        end function parquet_debug_get_filter_mask_nanos
        !> Zeroes the two space-padded string-read phase timers.
        subroutine parquet_debug_reset_string_read_phase_nanos() &
            bind(C, name="parquet_debug_reset_string_read_phase_nanos")
        end subroutine parquet_debug_reset_string_read_phase_nanos
        !> Nanoseconds the padded string read spent in Arrow's own decode.
        function parquet_debug_get_string_read_decode_nanos() result(res) &
            bind(C, name="parquet_debug_get_string_read_decode_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated decode nanoseconds.
        end function parquet_debug_get_string_read_decode_nanos
        !> Nanoseconds the padded string read spent in its own per-row copy loop.
        function parquet_debug_get_string_read_copy_nanos() result(res) &
            bind(C, name="parquet_debug_get_string_read_copy_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated copy-loop nanoseconds.
        end function parquet_debug_get_string_read_copy_nanos
        !> Zeroes the read-time sort's five phase timers and its two column counts.
        subroutine parquet_debug_reset_sort_phase_nanos() &
            bind(C, name="parquet_debug_reset_sort_phase_nanos")
        end subroutine parquet_debug_reset_sort_phase_nanos
        !> Nanoseconds the read-time sort spent reporting its key's family and size to Fortran.
        function parquet_debug_get_sort_info_nanos() result(res) &
            bind(C, name="parquet_debug_get_sort_info_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated key-info nanoseconds.
        end function parquet_debug_get_sort_info_nanos
        !> Nanoseconds spent materialising the key again inside the fetch call.
        function parquet_debug_get_sort_bind_nanos() result(res) &
            bind(C, name="parquet_debug_get_sort_bind_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated key-bind nanoseconds.
        end function parquet_debug_get_sort_bind_nanos
        !> Nanoseconds spent copying that key into Fortran-owned buffers.
        function parquet_debug_get_sort_copy_nanos() result(res) &
            bind(C, name="parquet_debug_get_sort_copy_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated key-copy nanoseconds.
        end function parquet_debug_get_sort_copy_nanos
        !> Nanoseconds spent building the Arrow permutation array from Fortran's answer.
        function parquet_debug_get_sort_perm_nanos() result(res) &
            bind(C, name="parquet_debug_get_sort_perm_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated permutation-build nanoseconds.
        end function parquet_debug_get_sort_perm_nanos
        !> Nanoseconds the permutation install spent on the column cache.
        function parquet_debug_get_sort_take_nanos() result(res) &
            bind(C, name="parquet_debug_get_sort_take_nanos")
            import :: c_long_long
            integer(c_long_long) :: res !! accumulated install nanoseconds.
        end function parquet_debug_get_sort_take_nanos
        !> Columns the permutation install re-Took in place since the last reset.
        function parquet_debug_get_sort_take_columns() result(res) &
            bind(C, name="parquet_debug_get_sort_take_columns")
            import :: c_long_long
            integer(c_long_long) :: res !! columns Taken.
        end function parquet_debug_get_sort_take_columns
        !> Columns the permutation install dropped from the cache since the last reset.
        function parquet_debug_get_sort_released_columns() result(res) &
            bind(C, name="parquet_debug_get_sort_released_columns")
            import :: c_long_long
            integer(c_long_long) :: res !! columns released.
        end function parquet_debug_get_sort_released_columns
        !> Nanoseconds the last threaded sort spent in `phase` (0, 1 or 2); 0 for anything else.
        function parquet_debug_get_sort_phase_ns(phase) result(res) &
            bind(C, name="parquet_debug_get_sort_phase_ns")
            import :: c_int, c_long_long
            integer(c_int), value :: phase !! 0 chunk sorts, 1 early merge rounds, 2 last round.
            integer(c_long_long) :: res    !! nanoseconds charged to that phase.
        end function parquet_debug_get_sort_phase_ns
        !> How many threads the last threaded sort's chunk phase actually used.
        function parquet_debug_get_sort_threads_used() result(res) &
            bind(C, name="parquet_debug_get_sort_threads_used")
            import :: c_long_long
            integer(c_long_long) :: res !! thread count of the last sort.
        end function parquet_debug_get_sort_threads_used
    end interface
    !
contains

    !> Lowers the C++ engine's threading floor so a small fixture reaches its parallel path.
    !!
    !! The published `sort_parallel_min_rows` setting used to do this. It was retired once the
    !! Fortran engine stopped reading it, leaving the C++ floor an internal constant -- so a test
    !! needing the threaded path at a few thousand rows now goes through this bind(C) override,
    !! declared locally per this project's debug-hook convention rather than in parquet_bindings.
    subroutine set_cpp_sort_min(n)
        integer(c_int64_t), intent(in) :: n !! rows; <= 0 restores the built-in floor.
        interface
            subroutine set_min(k) bind(C, name="parquet_debug_set_sort_parallel_min_rows")
                use iso_c_binding, only : c_int64_t
                integer(c_int64_t), value :: k !! rows; <= 0 restores the built-in floor.
            end subroutine set_min
        end interface
        !
        call set_min(n)
    end subroutine set_cpp_sort_min
    !
    subroutine collect_tests_diagnostics(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        !
        testsuite = [ &
            new_unittest("the filter's three phase timers each record their own phase", &
                test_filter_phase_timers), &
            new_unittest("the padded string read's two phase timers record", &
                test_string_read_phase_timers), &
            new_unittest("the threaded sort's phase timers record, and reject a bad phase", &
                cpp_test_sort_phase_timers), &
            new_unittest("the read-time sort's five phase timers each record their own phase", &
                test_read_sort_phase_timers) &
            ]
    end subroutine collect_tests_diagnostics
    !
    !> All three `parquet_reader_set_filter` phases must move across one filtered open.
    !!
    !! The reset is the negative control and has to come first: without it the assertion would
    !! pass against timers left nonzero by an earlier test in this suite, which is precisely what
    !! a dead `charge_phase` would look like. Asserting all three separately rather than their sum
    !! is what stops one live timer covering for two dead ones.
    subroutine test_filter_phase_timers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: dec, ev, msk, nrows
        character(len=*), parameter :: f = "test_run/diag_filter_phases.parquet"
        !
        call write_numeric_fixture(f, 20000)
        !
        call parquet_debug_reset_filter_phase_nanos()
        call check(error, parquet_debug_get_filter_decode_nanos() == 0_c_long_long .and. &
            parquet_debug_get_filter_eval_nanos() == 0_c_long_long .and. &
            parquet_debug_get_filter_mask_nanos() == 0_c_long_long, &
            "reset must zero all three filter phase timers")
        if (allocated(error)) return
        !
        call filt%add("v > 5000")
        call parquet_open_reader(reader, f, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows > 0_int64 .and. nrows < 20000_int64, &
            "the filter must actually have removed some rows, or there was nothing to time")
        if (allocated(error)) return
        !
        dec = parquet_debug_get_filter_decode_nanos()
        ev = parquet_debug_get_filter_eval_nanos()
        msk = parquet_debug_get_filter_mask_nanos()
        call check(error, dec > 0_int64, "the filter decode phase timer must record its phase")
        if (allocated(error)) return
        call check(error, ev > 0_int64, "the filter evaluation phase timer must record its phase")
        if (allocated(error)) return
        call check(error, msk > 0_int64, "the filter mask-build phase timer must record its phase")
    end subroutine test_filter_phase_timers
    !
    !> Both phases of the fixed-width space-padded string read must move across one column read.
    !!
    !! This is the path a `character(len=*)` array argument takes -- not `parquet_string_column`,
    !! which has its own compact buffer handoff and does not charge these timers at all.
    subroutine test_string_read_phase_timers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_writer) :: w
        type(parquet_reader) :: reader
        integer, parameter :: N = 20000
        character(len=16) :: s(N), back(N)
        integer(int64) :: dec, cpy
        integer :: i
        character(len=*), parameter :: f = "test_run/diag_string_phases.parquet"
        !
        ! First element deliberately the shortest (CLAUDE.md's sized-from-the-first-element rule).
        s(1) = "a"
        do i = 2, N
            write(s(i), "('row', I0)") i
        end do
        call parquet_open_writer(w, f)
        call parquet_write_column(w, "s", s)
        call parquet_close_writer(w)
        !
        call parquet_debug_reset_string_read_phase_nanos()
        call check(error, parquet_debug_get_string_read_decode_nanos() == 0_c_long_long .and. &
            parquet_debug_get_string_read_copy_nanos() == 0_c_long_long, &
            "reset must zero both string-read phase timers")
        if (allocated(error)) return
        !
        call parquet_open_reader(reader, f)
        call parquet_read_column(reader, "s", back)
        ! The read has to be a real one, or the timers would be measuring nothing: check the
        ! payload came back before believing anything they say about it.
        call check(error, trim(back(1)) == "a" .and. trim(back(N)) == trim(s(N)), &
            "the padded string column must round-trip, or the timers timed nothing")
        if (allocated(error)) return
        !
        dec = parquet_debug_get_string_read_decode_nanos()
        cpy = parquet_debug_get_string_read_copy_nanos()
        call check(error, dec > 0_int64, "the string-read decode phase timer must record its phase")
        if (allocated(error)) return
        call check(error, cpy > 0_int64, "the string-read copy phase timer must record its phase")
    end subroutine test_string_read_phase_timers
    !
    !> The threaded sort's phase timers, plus the out-of-range `phase` answer.
    !!
    !! Two process-global settings have to be forced for the threaded path to run at all, and both
    !! are restored afterwards: the counting fast path is O(n) and returns before phase 0 is ever
    !! timed, and the parallel path declines below a row threshold far above any test fixture. Both
    !! are the "a threshold no test-sized fixture can reach is a threshold no test exercises" case
    !! CLAUDE.md describes.
    subroutine test_sort_phase_timers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        integer, parameter :: N = 40000
        real(real64) :: v(N)
        integer(int64), allocatable :: perm(:)
        type(pf_sort_keys) :: keys
        integer(int64) :: p0, p1, p2
        logical :: counting
        integer :: i
        !
        ! An out-of-range phase answers 0 rather than reading past the array. Asserted BEFORE any
        ! sort runs, so a live phase 0 cannot make the guard look like it works.
        call check(error, parquet_debug_get_sort_phase_ns(-1_c_int) == 0_c_long_long, &
            "a negative phase must answer 0")
        if (allocated(error)) return
        call check(error, parquet_debug_get_sort_phase_ns(3_c_int) == 0_c_long_long, &
            "a phase past the last must answer 0")
        if (allocated(error)) return
        !
        counting = parquet_get_sort_counting_path()
        call parquet_set_sort_counting_path(.false.)
        call set_cpp_sort_min(1000_c_int64_t)
        !
        ! Values chosen so no two are equal and the order is nowhere near sorted, so the chunk
        ! sorts and the merge rounds both have real work to do.
        do i = 1, N
            v(i) = real(mod(i * 7919, N), real64) + real(i, real64) / real(N, real64)
        end do
        call keys%add(v)
        call pf_argsort(keys, perm, threads=4)
        !
        call parquet_set_sort_counting_path(counting)
        call set_cpp_sort_min(-1_c_int64_t)   ! restores the built-in floor
        !
        call check(error, size(perm) == N, "the sort must have produced a full permutation")
        if (allocated(error)) return
        call check(error, all(v(perm(2:)) > v(perm(:size(perm) - 1))), &
            "the permutation must actually be sorted, or the timers timed the wrong thing")
        if (allocated(error)) return
        ! Without more than one thread there are no chunk sorts and no merge rounds to charge, so
        ! the timers below would legitimately be 0 -- this is what makes them assertable.
        call check(error, parquet_debug_get_sort_threads_used() > 1_c_long_long, &
            "the sort must have taken the threaded path, or its phase timers cannot record")
        if (allocated(error)) return
        !
        p0 = parquet_debug_get_sort_phase_ns(0_c_int)
        p1 = parquet_debug_get_sort_phase_ns(1_c_int)
        p2 = parquet_debug_get_sort_phase_ns(2_c_int)
        call check(error, p0 > 0_int64, "the chunk-sort phase timer must record its phase")
        if (allocated(error)) return
        ! Phases 1 and 2 split the merge rounds between them: every round but the last, and the
        ! last alone. With four chunks there are two rounds, so both must have been charged.
        call check(error, p1 > 0_int64, "the early-merge-round phase timer must record its phase")
        if (allocated(error)) return
        call check(error, p2 > 0_int64, "the last-merge-round phase timer must record its phase")
    end subroutine test_sort_phase_timers
    !
    !> All five READ-TIME sort phases must move across one sorted open, and the install has to be
    !! shown to have run at all.
    !!
    !! These are the `parquet_open_reader(..., sort_by=)` phases -- key info, key bind, the copy into
    !! Fortran-owned buffers, building the Arrow permutation, and what the install then does with the
    !! column cache -- and they are a different family from `cpp_test_sort_phase_timers` above, which
    !! times the C++ engine's own chunk sorts and merge rounds. Nothing in the library reads either
    !! set, so a `charge_phase` call that stopped being wired up would look like a phase that costs
    !! nothing: the most misleading answer a diagnostic can give, and invisible to every other test.
    !!
    !! All five are asserted separately, for the same reason the filter's three are: one live timer
    !! must not be able to cover for four dead ones. The install's own phase is paired with the two
    !! COLUMN COUNTS, because a Take count of 0 cannot tell "the install ran and released the key"
    !! from "the install never ran" -- only the pair distinguishes them.
    subroutine test_read_sort_phase_timers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer, parameter :: N = 20000
        integer(int32), allocatable :: back(:)
        integer(int64) :: info, bind_, copy, perm, take, taken, released
        character(len=*), parameter :: f = "test_run/diag_read_sort_phases.parquet"
        !
        call write_numeric_fixture(f, N)
        !
        call parquet_debug_reset_sort_phase_nanos()
        call check(error, parquet_debug_get_sort_info_nanos() == 0_c_long_long .and. &
            parquet_debug_get_sort_bind_nanos() == 0_c_long_long .and. &
            parquet_debug_get_sort_copy_nanos() == 0_c_long_long .and. &
            parquet_debug_get_sort_perm_nanos() == 0_c_long_long .and. &
            parquet_debug_get_sort_take_nanos() == 0_c_long_long .and. &
            parquet_debug_get_sort_take_columns() == 0_c_long_long .and. &
            parquet_debug_get_sort_released_columns() == 0_c_long_long, &
            "reset must zero all five read-sort phase timers and both column counts")
        if (allocated(error)) return
        !
        allocate(back(N))
        call srt%add("v desc")
        call parquet_open_reader(reader, f, sort_by=srt)
        call parquet_read_column(reader, "v", back)
        call parquet_close_reader(reader)
        ! The sort has to be a real one, or the timers would be measuring nothing: the fixture is
        ! written ascending, so a descending read-time sort must reverse it.
        call check(error, back(1) == int(N, int32) .and. back(N) == 1_int32, &
            "the read-time sort must have reordered the rows, or the timers timed nothing")
        if (allocated(error)) return
        !
        info = parquet_debug_get_sort_info_nanos()
        bind_ = parquet_debug_get_sort_bind_nanos()
        copy = parquet_debug_get_sort_copy_nanos()
        perm = parquet_debug_get_sort_perm_nanos()
        take = parquet_debug_get_sort_take_nanos()
        taken = parquet_debug_get_sort_take_columns()
        released = parquet_debug_get_sort_released_columns()
        call check(error, info > 0_int64, "the key-info phase timer must record its phase")
        if (allocated(error)) return
        call check(error, bind_ > 0_int64, "the key-bind phase timer must record its phase")
        if (allocated(error)) return
        call check(error, copy > 0_int64, "the key-copy phase timer must record its phase")
        if (allocated(error)) return
        call check(error, perm > 0_int64, "the permutation-build phase timer must record its phase")
        if (allocated(error)) return
        call check(error, take > 0_int64, "the install phase timer must record its phase")
        if (allocated(error)) return
        call check(error, taken + released > 0_int64, &
            "the install must have either Taken or released a column, or it never ran")
    end subroutine test_read_sort_phase_timers
    !
    !> Writes an `n`-row single-column int32 file for the filter tests to filter over.
    subroutine write_numeric_fixture(fname, n)
        character(len=*), intent(in) :: fname !! file to write.
        integer, intent(in) :: n              !! rows to write.
        type(parquet_writer) :: w
        integer(int32), allocatable :: v(:)
        integer :: i
        !
        allocate(v(n))
        do i = 1, n
            v(i) = int(i, int32)
        end do
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
    end subroutine write_numeric_fixture
    !

    ! ---- C++-engine pins ------------------------------------------------------------------
    !
    ! Each test wrapped below observes a C++-SIDE counter, which the Fortran engine (the default
    ! since Stage 6) does not populate. Pinning keeps them testing the engine they were written
    ! for -- one that still ships, since `parquet_open_reader(..., sort_by=)` and
    ! `parquet_reader_set_sort` reach it directly with no selector in the path.
    !
    ! The wrapper shape rather than a pin inside each body is deliberate: these tests have early
    ! `return`s, and a selector leaked on one would not fail the test that leaked it -- it would
    ! silently change which engine a LATER test measures.
    !> Pins the C++ engine for `test_sort_phase_timers` -- see the note above.
    subroutine cpp_test_sort_phase_timers(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_sort_phase_timers(error)
        call parquet_debug_use_fortran_sort_engine(.true.)   ! the shipped default
    end subroutine cpp_test_sort_phase_timers

end module test_diagnostics
