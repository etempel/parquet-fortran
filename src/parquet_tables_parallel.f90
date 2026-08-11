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
    !> Below this much work in the largest column a mutation would rewrite, the loop runs serially.
    !!
    !! Measured in ELEMENTS (`rows * width`) rather than bytes, because `parquet_column` exposes no
    !! byte size and deriving one here would mean a second copy of the kind-to-size table that lives
    !! in the generator. 131072 elements is 1 MiB of `float64`, the widest ordinary element, so the
    !! floor is at most 1 MiB and less for narrower kinds.
    !!
    !! **The value is deliberately a wide margin rather than a tuned constant.** Spawning an OpenMP
    !! team costs single-digit microseconds; gathering 1 MiB reads and writes 2 MiB, a few hundred
    !! microseconds at ordinary memory bandwidth. The real break-even is one to two orders of
    !! magnitude below this, so the floor errs toward serial and cannot plausibly be too small. It
    !! is also low enough for an ordinary unit test to exceed it (131k `float64` rows is ~1 MB), so
    !! the tests are not confined to the serial path by it.
    !!
    !! **Not a public setting** -- it is an input to a decision the library makes for the caller, and
    !! `parquet_set_table_threads` is already the user-facing control over this loop. It IS
    !! overridable for tuning through a test-only hook; see `colwork_gate_limits`.
    integer(int64), parameter :: colwork_min_elements = 131072_int64
    !
    !> Fewer mutable columns than this and the loop runs serially.
    !!
    !! Two is the smallest number that can use a second thread at all, so this is a statement that
    !! the loop threads whenever threading is possible, not a tuned choice. Named rather than written
    !! as a literal in `colwork_threads` so that it can carry this note and be swept alongside
    !! `colwork_min_elements`.
    integer, parameter :: colwork_min_columns = 2
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
        call cache_check_shared_write(self%cache, idx, proc, nulling)
    end procedure table_check_shared_write
    !
    module procedure cache_check_shared_write
        character(len=:), allocatable :: sfx
        !
        ! The overwhelmingly common case, and the only one on the serial path: nothing shared,
        ! nothing to check. omp_in_parallel() plus two integer comparisons.
        if (.not. unsafe_shared_mutation(cache)) return
        associate (col => cache%cols(idx))
            if (col%values%kindof() == PK_STRING .or. col%values%kindof() == PK_STRING_VEC) then
                call table_context_suffix(cache, col%name, sfx)
                error stop EP // trim(proc) // ": this is a string column, whose rows share one " // &
                    "packed store -- writing any element can move the whole payload, so its rows " // &
                    "cannot be divided between threads the way a fixed-width column's can. Write " // &
                    "it before the parallel region or after it" // sfx
            end if
            if (nulling .and. .not. col%values%has_validity_storage()) then
                call table_context_suffix(cache, col%name, sfx)
                error stop EP // trim(proc) // ": this column has no validity storage yet, so " // &
                    "the first null would allocate it and two threads doing that race with no " // &
                    "diagnostic. Call %ensure_validity('" // trim(col%name) // "') before the " // &
                    "parallel region" // sfx
            end if
        end associate
    end procedure cache_check_shared_write
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
    module procedure table_mutable_slots
        integer :: i, n
        !
        allocate(slots(self%cache%ncols))
        n = 0
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            n = n + 1
            slots(n) = i
        end do
        ! Trimmed to what was actually collected, so every caller can loop over the whole array
        ! and `size(slots)` is the count -- an unusable column left in it would be rewritten.
        slots = slots(1:n)
    end procedure table_mutable_slots
    !
    module procedure table_colwork
        integer :: j, nt
        !
        nt = colwork_threads(cache, slots)
        ! Noted on BOTH paths, serial included, so a test can tell "ran on one thread" from "the
        ! gate declined and the counter was never written". A gate that always declines otherwise
        ! passes every correctness test written for this feature.
        call parquet_debug_note_table_threads(int(nt, int64))
        if (nt <= 1) then
            do j = 1, size(slots)
                call colwork_one(cache, op, slots(j), rows, keep)
            end do
            return
        end if
        ! Each iteration rewrites `cache%cols(slots(j))%values` and nothing else: the slots are
        ! distinct allocations, each written by exactly one thread, and the shared `rows`/`keep`
        ! arrays are only read. Nothing here is written that another iteration reads.
        !
        ! `schedule(dynamic)` rather than static, because the work per column is wildly uneven: a
        ! PK_STRING column's reindex rebuilds its whole packed payload (offsets, data and validity)
        ! where a float64 column's is a gather, so a static split leaves threads idle behind the
        ! string columns.
        !
        ! **The imbalance is real but the choice is not settled -- measured, and the measurement did
        ! not confirm this.** On 5.2M rows a PK_STRING (len 16) reindex costs 7.9x a float64 one, so
        ! the premise above holds. But over 24 columns on 8 threads, which schedule wins depends on
        ! where the string column SITS: dynamic is ~12% faster when it comes first and ~14% slower
        ! when it comes last (it is then picked up near the end and 7 threads idle behind it), while
        ! static is within 5% of itself either way. Neither dominates, because the loop is
        ! bandwidth-saturated -- 24 columns summing to 0.068 s serially take 0.031 s on 8 threads,
        ! a 2.2x speedup, so the machine binds before the schedule does. **Do not "fix" this either
        ! way on reasoning alone; the ordering fix (expensive columns first) beats both and is what
        ! a change here should actually do.** See `feature_table.md` for the numbers and the option.
        !
        ! Nothing finalizable is declared inside this construct -- the body only references slots
        ! that already exist. That is what keeps feature_risks.md Risk-45 (gfortran and ifx forbid
        ! opposite shapes for a finalizable type in a parallel region) out of this region entirely;
        ! a finalizable local inside a procedure CALLED from here is fine and is what
        ! parquet_column's own procedures already do.
        !$omp parallel do default(shared) private(j) schedule(dynamic) num_threads(nt)
        do j = 1, size(slots)
            call colwork_one(cache, op, slots(j), rows, keep)
        end do
        !$omp end parallel do
    end procedure table_colwork
    !
    !> Applies one `PCW_*` operation to one column. The whole body of both loops above, so the two
    !! paths cannot drift.
    subroutine colwork_one(cache, op, idx, rows, keep)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        integer, intent(in) :: op                         !! which operation; a PCW_* constant.
        integer, intent(in) :: idx                        !! slot to rewrite.
        integer(int64), intent(in), optional :: rows(:)   !! permutation or selection, per `op`.
        logical, intent(in), optional :: keep(:)          !! per-row keep mask, per `op`.
        !
        select case (op)
        case (PCW_REINDEX_TRUSTED)
            ! Trusted, never `%reindex`: the permutation is validated ONCE per sort, by the caller,
            ! on a column it rewrites serially before handing the rest here (feature_risks.md
            ! Risk-46). Collapsing this to all-trusted with no validating column anywhere, or
            ! making this one validate too, both undo that.
            call cache%cols(idx)%values%reindex_trusted(rows)
        case (PCW_DELETE_MASK)
            call cache%cols(idx)%values%delete_by_mask(keep)
        case (PCW_GATHER)
            call cache%cols(idx)%values%gather(rows)
        case default
            ! Not reachable: `op` comes from a PCW_* constant at each of the three call sites, all
            ! in parquet_tables_rowmutate.f90, never from user input. Kept because a new op added
            ! without a branch here would otherwise leave every column silently unrewritten --
            ! which is the row-correspondence failure this whole path is guarded against.
            !
            ! gcov credits this line with the WHOLE select's dispatch count -- 223 in one full run,
            ! which is exactly 83 + 115 + 25, the three real arms above. An `error stop` that had
            ! run even once would have ended the process, so the count is attribution, not
            ! execution. Tagged so the stale-exclusion report stops offering it.
            error stop EP // "colwork: unknown per-column operation" ! GCOVR_EXCL_LINE -- gcov attribution artifact
        end select
    end subroutine colwork_one
    !
    module procedure table_colwork_clone
        integer :: j, nt
        !
        ! Gated by the same rule as a mutation, measured on the SOURCE: the destination slots do not
        ! exist yet, and the work is one full copy of each source column.
        nt = colwork_threads(src, slots)
        ! The same counter table_colwork writes, on both paths, for the same reason -- a clone is a
        ! per-column table operation under the same gate, so a test observes it the same way.
        call parquet_debug_note_table_threads(int(nt, int64))
        if (nt <= 1) then
            do j = 1, size(slots)
                call src%cols(slots(j))%values%deep_copy(dst%cols(slots(j))%values)
            end do
            return
        end if
        ! Each iteration reads one source column and writes one destination column, both indexed by
        ! the same distinct slot, so no two iterations touch the same allocation in either store.
        ! The source is only read -- `deep_copy` takes `self` intent(in) -- which is what makes it
        ! safe for this to run while the source table is shared with another thread. See
        ! colwork_threads' own note on why %clone's safety argument differs from a mutation's.
        !
        ! `schedule(dynamic)` for the reason table_colwork uses it: a PK_STRING column's copy is a
        ! whole packed payload where a float64 column's is a memcpy, so a static split would leave
        ! threads idle behind the string columns.
        !$omp parallel do default(shared) private(j) schedule(dynamic) num_threads(nt)
        do j = 1, size(slots)
            call src%cols(slots(j))%values%deep_copy(dst%cols(slots(j))%values)
        end do
        !$omp end parallel do
    end procedure table_colwork_clone
    !
    !> How many threads this mutation may use: 1 (serial) or more.
    !!
    !! **Deliberately conservative, and it picks a DEFAULT rather than refusing anything** -- the
    !! same shape as `parallel_prefetch_ok` (src/parquet_tables_read.f90) and `pf_sort_threads`
    !! (src/parquet_sorting_keys.f90), for the same reasons. Four things make it answer 1:
    !!
    !!   * **Fewer mutable columns than `colwork_min_columns`.** The parallelism is bounded by the
    !!     column count, so a one-column table can use nothing and a thread team would be pure
    !!     overhead. This is the opposite bound from `%prefetch`'s, which is bounded by I/O.
    !!   * **Already inside a parallel region.** Nested regions are the caller's business; without
    !!     this, T threads would each ask for T more, and T*T oversubscription is slower than not
    !!     threading at all. `omp_get_max_threads()` reads an ICV, not the current team size, so
    !!     inside an 8-thread region it still answers 8.
    !!   * **Too little work.** See `colwork_min_elements`.
    !!   * **`parquet_set_table_threads(1)`**, which is how a caller -- and every equality test in
    !!     the suite -- forces the serial path through the public API rather than a debug hook.
    !!
    !! The cap only ever reduces: the answer is never more than OpenMP offers and never more than
    !! there are columns, so `T` transient column copies are at most one extra copy of the table.
    !!
    !! **No ownership guard is needed here, but the reason differs between the two callers and both
    !! halves have to hold.** Every *mutation* caller has already run `table_check_not_shared`, so
    !! the table is provably not shared and the threads spawned are its own, exactly as
    !! `%prefetch`'s are. **`table_colwork_clone` does NOT run that guard** -- `%clone` checks only
    !! that the table is open -- and is safe for a different reason: it only ever READS the source
    !! (`deep_copy` takes `self` intent(in)) and writes a destination the caller has just created
    !! and nobody else can reach. Do not "unify" these into one sentence: a future caller that
    !! mutated a possibly-shared source would satisfy the second argument while breaking the first.
    integer function colwork_threads(cache, slots) result(n)
        use parquet_settings, only : parquet_get_table_threads
#ifdef _OPENMP
        use omp_lib, only : omp_get_max_threads, omp_in_parallel
#endif
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer, intent(in) :: slots(:)                !! slots the mutation will rewrite.
        integer :: cap, min_columns
        integer(int64) :: min_elements
        !
        n = 1
#ifdef _OPENMP
        ! Cheapest exit first: this one costs nothing and needs neither limit.
        if (omp_in_parallel()) return
        call colwork_gate_limits(min_elements, min_columns)
        if (size(slots) < min_columns) return
        if (largest_column_elements(cache, slots) < min_elements) return
        n = omp_get_max_threads()
        cap = parquet_get_table_threads()
        if (cap > 0 .and. cap < n) n = cap
        if (n > size(slots)) n = size(slots)
        if (n < 1) n = 1
#endif
    end function colwork_threads
    !
    !> The two gate limits actually in force: the test-only override where one is set, otherwise the
    !> real constant.
    !!
    !! **Neither constant can be tuned by a benchmark, which is why the override exists.** Every
    !! table size a benchmark can afford sits orders of magnitude above `colwork_min_elements`, so no
    !! sweep over rows or columns ever visits its break-even — the only way to find it is to move the
    !! constant rather than the input. `parquet_debug_set_sort_merge_min_segment` exists for the
    !! mirror-image problem (a constant no test-sized input can reach) and takes the same shape.
    !!
    !! The overrides live in `src/parquet_wrapper.cpp` rather than in a public Fortran procedure for
    !! the reason CLAUDE.md gives: a Fortran-side hook would have to be public. Two `bind(C)` reads
    !! per mutation is the same ratio `parquet_debug_note_table_threads` justifies — a coarse
    !! operation about to rewrite every column of a table, never a per-row path.
    subroutine colwork_gate_limits(min_elements, min_columns)
        use iso_c_binding, only : c_int64_t
        integer(int64), intent(out) :: min_elements !! work floor in elements, override applied.
        integer, intent(out) :: min_columns         !! minimum mutable columns, override applied.
        interface
            function get_elems() result(res) bind(C, name="parquet_debug_get_colwork_min_elements")
                import :: c_int64_t
                integer(c_int64_t) :: res
            end function get_elems
            function get_cols() result(res) bind(C, name="parquet_debug_get_colwork_min_columns")
                import :: c_int64_t
                integer(c_int64_t) :: res
            end function get_cols
        end interface
        integer(int64) :: v
        !
        min_elements = colwork_min_elements
        min_columns = colwork_min_columns
        v = int(get_elems(), int64)
        if (v > 0_int64) min_elements = v
        v = int(get_cols(), int64)
        if (v > 0_int64) min_columns = int(v)
    end subroutine colwork_gate_limits
    !
    !> Elements (`rows * width`) in the largest column the mutation will rewrite.
    !!
    !! The largest rather than the total, because the floor asks "is one column's worth of work
    !! big enough to be worth a thread", and every column of a table has the same row count -- so
    !! this reduces to the row count times the widest column.
    integer(int64) function largest_column_elements(cache, slots) result(biggest)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer, intent(in) :: slots(:)                !! slots the mutation will rewrite.
        integer :: j
        integer(int64) :: here
        !
        biggest = 0_int64
        do j = 1, size(slots)
            here = cache%cols(slots(j))%values%length() * &
                int(max(cache%cols(slots(j))%values%colwidth(), 1), int64)
            if (here > biggest) biggest = here
        end do
    end function largest_column_elements
    !
    !> Records, for the test suite only, how many threads the last row-structural mutation used.
    !!
    !! Pushed to a C++ global for the reason CLAUDE.md gives ("A Fortran-side debug hook has to be
    !! PUBLIC, so prefer a C++ one"): the number is a local of `table_colwork`, and a Fortran hook
    !! for it would have to be a public procedure in this module, visible to every `use parquet`.
    !! The same shape as `parquet_debug_note_prefetch_threads` (src/parquet_tables_read.f90).
    !!
    !! Called once per mutation, on a path about to rewrite every column of a table, so the cost is
    !! unmeasurable. **That ratio is the rule**: a debug hook may sit on a coarse operation like
    !! this one, never on a per-row or per-element path.
    subroutine parquet_debug_note_table_threads(n)
        use iso_c_binding, only : c_int64_t
        integer(int64), intent(in) :: n !! threads the mutation resolved to; 1 when it ran serially.
        interface
            subroutine set_used(k) bind(C, name="parquet_debug_set_table_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine set_used
        end interface
        !
        call set_used(int(n, c_int64_t))
    end subroutine parquet_debug_note_table_threads
    !
    module procedure table_check_not_shared
        character(len=:), allocatable :: sfx
        !
        ! Defensive: every caller runs table_check_open first, which aborts on a table with no
        ! cache, so this cannot be reached through any of them. Kept so a future caller that guards
        ! differently cannot dereference a null cache here.
        if (.not. associated(self%cache)) return ! GCOVR_EXCL_LINE
        if (.not. unsafe_shared_mutation(self%cache)) return
        call table_context_suffix(self%cache, "", sfx)
        error stop EP // trim(proc) // ": this table was not opened by this thread inside the " // &
            "parallel region, so it may be shared, and changing its structure would pull " // &
            "storage out from under another thread. Do it before the region or after it; " // &
            "%append is the only change a shared table permits" // sfx
    end procedure table_check_not_shared
    !
end submodule parquet_tables_parallel ! GCOVR_EXCL_LINE
