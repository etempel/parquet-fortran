!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Everything that makes a `parquet_table` safe to use from more than one thread: the table's own
!! lock, the counters behind the append/read contract, and the shared refusal every structural
!! mutation routes through.
!!
!! **This file exists so the `#ifdef _OPENMP` plumbing lives in exactly one place.** Every other
!! file in the table layer calls the plain procedures below and never sees a conditional or an
!! `omp_lib` import. The two exceptions are `unsafe_first_touch` and `record_open_thread`, which
!! stayed in `parquet_tables_read.f90` next to the materialization path they guard; they implement
!! the same ownership test `unsafe_shared_mutation` below uses, and the three must agree.
!!
!! **The concurrency model, in one paragraph.** Reading an already-resident column is free: no
!! lock, no atomic, unlimited threads -- that is the property everything here exists to protect,
!! and any change that puts a lock on the read path is a design change rather than an
!! optimisation. A *first touch* is refused on a shared table (`unsafe_first_touch`), because it
!! publishes an allocation with no ordering guarantee behind it. A *structural* change is refused
!! on a shared table (`unsafe_shared_mutation`). `%append` is the one mutation that is allowed
!! concurrently, and it is allowed because this file serialises it -- so the caller never writes
!! `!$omp critical` by hand and cannot wrap the wrong statement.
!!
!! **What the guards can and cannot see.** They key on OpenMP thread identity, so a caller
!! threading some other way (pthreads through C interop, coarrays) gets no enforcement at all, and
!! a read through a `%col` pointer the caller already holds is invisible to the library the way
!! every pointer dereference is. Both are documented limits, not gaps to close later; the
!! `%generation()` counter is what a caller checks a pointer against.
submodule (parquet_tables) parquet_tables_parallel
    implicit none
    !
contains
    !
    module procedure unsafe_shared_mutation
#ifdef _OPENMP
        use omp_lib, only : omp_in_parallel, omp_get_thread_num
        unsafe = .false.
        if (.not. omp_in_parallel()) return
        ! Identical to unsafe_first_touch's test, deliberately: a table this very thread opened
        ! inside the region cannot be shared with another thread, so what it does with it is its
        ! own business -- that is the per-thread-slice shape of use case B, where each thread
        ! filters and sorts its own private slice. Anything else may be shared.
        unsafe = .not. (cache%opened_in_parallel .and. cache%owner_thread == omp_get_thread_num())
#else
        unsafe = .false.
#endif
    end procedure unsafe_shared_mutation
    !
    module procedure table_init_lock
#ifdef _OPENMP
        use omp_lib, only : omp_init_lock
        ! Guarded rather than unconditional so that a second call cannot leak the first lock. The
        ! cache is freshly allocated at every open, so this is belt and braces -- but the failure
        ! it prevents (initialising an already-held lock) has no diagnostic at all.
        if (cache%lock_ready) return
        call omp_init_lock(cache%lock)
#endif
        cache%lock_ready = .true.
    end procedure table_init_lock
    !
    module procedure table_destroy_lock
#ifdef _OPENMP
        use omp_lib, only : omp_destroy_lock
        if (.not. cache%lock_ready) return
        call omp_destroy_lock(cache%lock)
#endif
        cache%lock_ready = .false.
    end procedure table_destroy_lock
    !
    module procedure table_lock
#ifdef _OPENMP
        use omp_lib, only : omp_set_lock
        ! A cache that somehow reached here without a lock is not worth aborting over: the
        ! serial path is correct, just unsynchronised, and a table with no lock is a table no
        ! other thread can have seen either.
        if (.not. cache%lock_ready) return
        call omp_set_lock(cache%lock)
#endif
    end procedure table_lock
    !
    module procedure table_unlock
#ifdef _OPENMP
        use omp_lib, only : omp_unset_lock
        if (.not. cache%lock_ready) return
        call omp_unset_lock(cache%lock)
#endif
    end procedure table_unlock
    !
    module procedure parquet_debug_table_set_inflight
        ! Written through the cache POINTER, which is why `table` is intent(in): the counters are
        ! not part of the table's own value, so this needs neither intent(inout) nor any of the
        ! finalization care a finalizable dummy would otherwise call for.
        if (.not. associated(table%cache)) return
        if (present(appending)) then
            table%cache%append_active = merge(1, 0, appending)
        end if
        if (present(reading)) then
            table%cache%readers_active = merge(1, 0, reading)
        end if
    end procedure parquet_debug_table_set_inflight
    !
    module procedure table_check_no_append
        integer :: active
        !
        active = 0
        !$omp atomic read
        active = cache%append_active
        if (active /= 0) then
            error stop EP // trim(proc) // ": another thread is appending to this table right " // &
                "now. A parallel append region is append-only -- no thread may read the table " // &
                "while any thread is appending to it, because the append reallocates every " // &
                "column's storage. Read it before the region or after it."
        end if
    end procedure table_check_no_append
    !
    module procedure table_read_enter
        call table_check_no_append(cache, proc)
        !$omp atomic update
        cache%readers_active = cache%readers_active + 1
    end procedure table_read_enter
    !
    module procedure table_read_exit
        !$omp atomic update
        cache%readers_active = cache%readers_active - 1
    end procedure table_read_exit
    !
    module procedure table_check_shared_write
        character(len=:), allocatable :: sfx
        !
        ! The overwhelmingly common case, and the only one on the serial path: nothing shared,
        ! nothing to check. omp_in_parallel() plus two integer comparisons.
        if (.not. unsafe_shared_mutation(self%cache)) return
        associate (col => self%cache%cols(idx))
            if (col%values%kindof() == PK_STRING .or. col%values%kindof() == PK_STRING_VEC) then
                call table_context_suffix(self%cache, col%name, sfx)
                error stop EP // trim(proc) // ": this is a string column, whose rows share one " // &
                    "packed store -- writing any element can move the whole payload, so its rows " // &
                    "cannot be divided between threads the way a fixed-width column's can. Write " // &
                    "it before the parallel region or after it" // sfx
            end if
            if (nulling .and. .not. col%values%has_validity_storage()) then
                call table_context_suffix(self%cache, col%name, sfx)
                error stop EP // trim(proc) // ": this column has no validity storage yet, so " // &
                    "the first null would allocate it and two threads doing that race with no " // &
                    "diagnostic. Call %ensure_validity('" // trim(col%name) // "') before the " // &
                    "parallel region" // sfx
            end if
        end associate
    end procedure table_check_shared_write
    !
    module procedure table_ensure_validity
        integer :: idx, i
        !
        call table_check_open(self, "ensure_validity")
        ! Guarded like any other structural change, and for the exact reason this procedure
        ! exists: it ALLOCATES the validity storage, so two threads calling it on one shared
        ! table would race on the very allocation it is meant to get out of the way. It belongs
        ! before the parallel region, which is where its own documentation puts it.
        call table_check_not_shared(self, "ensure_validity")
        if (present(name)) then
            call table_resolve(self, name, "ensure_validity", idx, found)
            if (idx == 0) return
            call self%cache%cols(idx)%values%ensure_validity()
            return
        end if
        if (present(found)) found = .true.
        ! Every RESIDENT column, and deliberately not the others: making a deferred column's
        ! validity exist would mean reading it, turning a preparation call into a whole-file read.
        ! A column that is not resident cannot be nulled concurrently either, because the first
        ! touch that would make it resident is itself already refused on a shared table.
        do i = 1, self%cache%ncols
            if (self%cache%cols(i)%residency /= RES_FULL) cycle
            call self%cache%cols(i)%values%ensure_validity()
        end do
    end procedure table_ensure_validity
    !
    module procedure table_check_not_shared
        character(len=:), allocatable :: sfx
        !
        if (.not. associated(self%cache)) return
        if (.not. unsafe_shared_mutation(self%cache)) return
        call table_context_suffix(self%cache, "", sfx)
        error stop EP // trim(proc) // ": this table was not opened by this thread inside the " // &
            "parallel region, so it may be shared, and changing its structure would pull " // &
            "storage out from under another thread. Do it before the region or after it; " // &
            "%append is the only change a shared table permits" // sfx
    end procedure table_check_not_shared
    !
end submodule parquet_tables_parallel ! GCOVR_EXCL_LINE
