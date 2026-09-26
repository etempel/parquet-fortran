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
!! lock, no bookkeeping, unlimited threads. It is not literally atomic-free -- every value
!! accessor makes the one `atomic read` of `append_active` that `table_check_no_append` below
!! performs -- but that single relaxed read is the whole cost, and it is the property everything
!! here exists to protect. Any change that puts a lock, or a second atomic, on the resident read
!! path is a design change rather than an optimisation. A *first touch* is refused on a shared
!! table (`unsafe_first_touch`), because it publishes an allocation with no ordering guarantee
!! behind it. A *structural* change is refused on a shared table (`unsafe_shared_mutation`).
!! `%append` is the one mutation that is allowed concurrently, and it is allowed because this
!! file serialises it -- so the caller never writes `!$omp critical` by hand and cannot wrap the
!! wrong statement.
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
    !> Fewer mutable columns than this and the loop over columns runs serially.
    !!
    !! Two is the smallest number that can use a second thread at all, so this is a statement that
    !! the loop threads whenever threading is possible, not a tuned choice. Named rather than written
    !! as a literal in `colwork_plan` so that it can carry this note and be swept alongside
    !! `colwork_min_elements`. **It gates the ACROSS-columns level only**: a gather with fewer
    !! columns than this still spends the team inside each column (`colwork_plan`'s second level),
    !! which is the whole reason that level exists.
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
        integer :: j, nt, inner
        !
        ! A gather may spend the team inside one column; the other two operations rewrite a column
        ! serially and have only the level across columns. Its destination row count is the
        ! selection's length, which is what the work floor has to see (a %top_n keeps a handful of
        ! rows out of millions; the join's own side may emit many more rows than it had).
        if (op == PCW_GATHER .and. present(rows)) then
            call colwork_plan(cache, .true., slots, nt, inner, nout=size(rows, kind=int64))
        else
            call colwork_plan(cache, .false., slots, nt, inner)
        end if
        ! Noted on BOTH paths, serial included, so a test can tell "ran on one thread" from "the
        ! gate declined and the counter was never written". A gate that always declines otherwise
        ! passes every correctness test written for this feature.
        call parquet_debug_note_table_threads(nt, inner)
        if (nt <= 1) then
            do j = 1, size(slots)
                call colwork_one(cache, op, slots(j), rows, keep, valid, inner)
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
        ! a change here should actually do.**
        !
        ! Nothing finalizable is declared inside this construct -- the body only references slots
        ! that already exist. That is what keeps feature_risks.md Risk-45 (gfortran and ifx forbid
        ! opposite shapes for a finalizable type in a parallel region) out of this region entirely;
        ! a finalizable local inside a procedure CALLED from here is fine and is what
        ! parquet_column's own procedures already do.
        !$omp parallel do default(shared) private(j) schedule(dynamic) num_threads(nt)
        do j = 1, size(slots)
            call colwork_one(cache, op, slots(j), rows, keep, valid, 1)
        end do
        !$omp end parallel do
    end procedure table_colwork
    !
    module procedure table_stat_threads
        integer :: inner
        !
        ! A column is scanned by one thread from end to end -- the pass has no within-column level
        ! -- so the plan is the reindex's: across columns only, gated on the same work floor and
        ! column count, capped by the same setting.
        call colwork_plan(cache, .false., slots, nt, inner)
        ! Noted on both paths, serial included, for the reason table_colwork notes it: so a test
        ! can tell "scanned on one thread" from "the gate declined and nothing was recorded".
        call parquet_debug_note_table_threads(nt, inner)
    end procedure table_stat_threads
    !
    !> Applies one `PCW_*` operation to one column. The whole body of both loops above, so the two
    !! paths cannot drift.
    subroutine colwork_one(cache, op, idx, rows, keep, valid, threads)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        integer, intent(in) :: op                         !! which operation; a PCW_* constant.
        integer, intent(in) :: idx                        !! slot to rewrite.
        integer(int64), intent(in), optional :: rows(:)   !! permutation or selection, per `op`.
        logical, intent(in), optional :: keep(:)          !! per-row keep mask, per `op`.
        logical, intent(in), optional :: valid(:)         !! per destination row of a gather; .false. nulls it.
        integer, intent(in) :: threads                    !! threads a gather may split its rows across; 1 serial.
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
            ! The mask and the team ride with the gather: `valid` nulls the destination rows the
            ! caller marks (the join's own side under how="right"/"outer") on top of whatever the
            ! gathered row carried, and `threads` above 1 is colwork_plan's within-column level --
            ! always 1 when this loop is itself running across columns.
            call cache%cols(idx)%values%gather(rows, valid=valid, threads=threads)
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
        call parquet_debug_note_table_threads(nt, 1)
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
    module procedure table_colwork_join
        integer :: j, nt, inner
        !
        ! Gated on the SOURCE's columns and the OUTPUT's rows: the destination slots have just been
        ! created and hold nothing, while the work per column is one gather of size(idx) rows out
        ! of a source column -- and for a lookup join the output is orders of magnitude larger
        ! than the table it carries from, so measuring the source alone kept a 10M-row rewrite
        ! under the floor and serial.
        call colwork_plan(src, .true., sslots, nt, inner, nout=size(idx, kind=int64))
        ! Noted on both paths, serial included -- see table_colwork's own note.
        call parquet_debug_note_table_threads(nt, inner)
        if (nt <= 1) then
            do j = 1, size(sslots)
                call join_one_column(src, dst, sslots(j), dslots(j), idx, valid, inner)
            end do
            return
        end if
        ! Each iteration reads one source column and writes one destination column, both named by
        ! their own entry of two arrays of DISTINCT slots, so no two iterations touch the same
        ! allocation in either store. `idx` and `valid` are only read. The source table is a
        ! different table from the destination -- a self-join is refused before anything reaches
        ! here -- so the two stores are distinct objects and not merely distinct slots.
        !
        ! `schedule(dynamic)` for the reason both siblings use it: a PK_STRING column's gather
        ! rebuilds a whole packed payload where a float64 column's is one scattered copy.
        !$omp parallel do default(shared) private(j) schedule(dynamic) num_threads(nt)
        do j = 1, size(sslots)
            call join_one_column(src, dst, sslots(j), dslots(j), idx, valid, 1)
        end do
        !$omp end parallel do
    end procedure table_colwork_join
    !
    !> Carries ONE column across: the whole body of both loops above, so the two cannot drift.
    !!
    !! **One call.** `%gather_from` builds the destination from the source's rows `idx` names in
    !! a single pass -- kind, width, unit and values, the source's own nulls carried with their
    !! rows, and every row `valid` marks `.false.` null on top of them. That mask only ever ADDS
    !! nulls, which is what lets it describe the unmatched rows alone and leaves the nulls the
    !! source column already had exactly where the gather put them (`feature_risks.md` Risk-182).
    !! Until stage 4 this was three calls -- `deep_copy`, `%gather`, `%set_validity` -- which
    !! copied the whole source before rebuilding the copy, held two transient copies where this
    !! holds one, and walked the rows three times.
    subroutine join_one_column(src, dst, sslot, dslot, idx, valid, threads)
        type(parquet_table_cache), intent(in) :: src      !! the source column store.
        type(parquet_table_cache), intent(inout) :: dst   !! the destination column store.
        integer, intent(in) :: sslot                      !! source slot.
        integer, intent(in) :: dslot                      !! destination slot.
        integer(int64), intent(in) :: idx(:)              !! per output row: source row, never 0.
        logical, intent(in), optional :: valid(:)         !! per output row: .false. to null it.
        integer, intent(in) :: threads                    !! threads the gather may split its rows across; 1 serial.
        !
        ! A source column with no rows has no row 1 for `idx` to have named, so it cannot be
        ! gathered at all -- every output row is unmatched by construction. Appending null rows to
        ! an empty copy gives the same result and keeps the kind, width and unit `deep_copy`
        ! carries across. Reachable whenever a zero-row right table is joined with how="left".
        if (src%cols(sslot)%values%length() < 1_int64) then
            call src%cols(sslot)%values%deep_copy(dst%cols(dslot)%values)
            call dst%cols(dslot)%values%append_nulls(size(idx, kind=int64))
            return
        end if
        call dst%cols(dslot)%values%gather_from(src%cols(sslot)%values, idx, valid=valid, threads=threads)
    end subroutine join_one_column
    !
    !> The plan for one mutation: how many columns it rewrites at once (`outer`) and how many
    !! threads each of those gives its column (`inner`). At most one of the two is above 1.
    !!
    !! **Two levels, one team.** A reindex and a delete rewrite a column serially, so their only
    !! level is across columns: `outer` is `colwork_avail`'s team bounded by the column count and
    !! gated by `colwork_min_columns`, exactly the rule this decision always applied. A GATHER can
    !! divide one column's rows across a team (`parquet_column%gather`/`%gather_from`), and for a
    !! gather the level is chosen per table from the same resolved team: **across the columns when
    !! there are at least as many columns as threads, else inside each column with the whole team,
    !! the columns one after another.** The second level is what makes a join that carries one or
    !! two columns use the machine at all -- bounding the team by the column count left a lookup
    !! join's single carried column rewritten serially whatever the thread count.
    !!
    !! **The two levels are never combined.** This library enables no nested parallelism, so a
    !! team opened inside a team collapses to one thread, silently, and a "hybrid" would measure
    !! exactly as the outer level alone. `bench/benchmark_join.sh --mode=payload` times the two
    !! levels side by side per column count; stage 4's measurement is what fixed the rule.
    !!
    !! The test-only hook `parquet_debug_set_colwork_level` (`src/parquet_wrapper.cpp`) forces one
    !! level -- 1 across, 2 within -- for that benchmark and for the negative controls in
    !! `test/test_table_parallel.f90`. It can only pick a level the operation has, so it never
    !! threads a reindex within a column, and it never opens a team the gate declined.
    subroutine colwork_plan(cache, splits, slots, outer, inner, nout)
        type(parquet_table_cache), intent(in) :: cache !! the column store whose columns are measured.
        logical, intent(in) :: splits                  !! the operation can divide one column's rows across a team.
        integer, intent(in) :: slots(:)                !! slots the mutation will rewrite.
        integer, intent(out) :: outer                  !! columns rewritten at once; 1 for one at a time.
        integer, intent(out) :: inner                  !! threads inside each column; 1 for serial.
        integer(int64), intent(in), optional :: nout   !! destination rows, where they differ from the columns' own.
        integer :: avail, mode, min_columns
        integer(int64) :: min_elements
        !
        outer = 1
        inner = 1
        avail = colwork_avail(cache, slots, nout)
        if (avail <= 1) return
        call colwork_gate_limits(min_elements, min_columns)
        mode = 1
        if (splits) then
            mode = colwork_level_mode()
            if (mode == 0) then
                mode = 2
                if (size(slots) >= avail .and. size(slots) >= min_columns) mode = 1
            end if
        end if
        if (mode == 1) then
            if (size(slots) < min_columns) return
            outer = min(avail, size(slots))
        else
            inner = avail
        end if
    end subroutine colwork_plan
    !
    !> How many columns a mutation that rewrites each column serially may rewrite at once: the
    !! plan for an operation with no within-column level. `table_colwork_clone` uses it.
    integer function colwork_threads(cache, slots) result(n)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer, intent(in) :: slots(:)                !! slots the mutation will rewrite.
        integer :: inner
        !
        call colwork_plan(cache, .false., slots, n, inner)
    end function colwork_threads
    !
    !> The team this mutation may open at all, before `colwork_plan` decides where: 1 (serial) or
    !! more.
    !!
    !! **Deliberately conservative, and it picks a DEFAULT rather than refusing anything** -- the
    !! same shape as `parallel_prefetch_ok` (src/parquet_tables_read.f90) and `pf_sort_threads`
    !! (src/parquet_sorting_keys.f90), for the same reasons. Three things make it answer 1:
    !!
    !!   * **Already inside a parallel region.** Nested regions are the caller's business; without
    !!     this, T threads would each ask for T more, and T*T oversubscription is slower than not
    !!     threading at all. `omp_get_max_threads()` reads an ICV, not the current team size, so
    !!     inside an 8-thread region it still answers 8.
    !!   * **Too little work.** See `colwork_min_elements`; measured on the larger of the columns'
    !!     own rows and `nout`, the rows the mutation will produce.
    !!   * **`parquet_set_table_threads(1)`**, which is how a caller -- and every equality test in
    !!     the suite -- forces the serial path through the public API rather than a debug hook.
    !!
    !! (The fourth reason this decision used to have -- fewer columns than `colwork_min_columns` --
    !! now belongs to the across-columns level alone, in `colwork_plan`.)
    !!
    !! The cap only ever reduces: the answer is never more than OpenMP offers, and the plan never
    !! rewrites more columns at once than there are, so `T` transient column copies are at most one
    !! extra copy of the table -- and one extra copy of one column when the team goes inside it.
    !!
    !! **No ownership guard is needed here, but the reason differs between the two callers and both
    !! halves have to hold.** Every *mutation* caller has already run `table_check_not_shared`, so
    !! the table is provably not shared and the threads spawned are its own, exactly as
    !! `%prefetch`'s are. **`table_colwork_clone` does NOT run that guard** -- `%clone` checks only
    !! that the table is open -- and is safe for a different reason: it only ever READS the source
    !! (`deep_copy` takes `self` intent(in)) and writes a destination the caller has just created
    !! and nobody else can reach. The join's carry is the clone's case, and so is
    !! `table_stat_threads`: `%print_stat`'s scan reads every column it visits and writes nothing
    !! but its own text cells. Do not "unify" these into one sentence: a future caller that
    !! mutated a possibly-shared source would satisfy the second argument while breaking the
    !! first.
    integer function colwork_avail(cache, slots, nout) result(n)
        use parquet_settings, only : parquet_get_table_threads, parquet_clamp_to_affinity
#ifdef _OPENMP
        use omp_lib, only : omp_get_max_threads, omp_in_parallel
#endif
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer, intent(in) :: slots(:)                !! slots the mutation will rewrite.
        integer(int64), intent(in), optional :: nout   !! destination rows, where they differ from the columns' own.
        integer :: cap, min_columns
        integer(int64) :: min_elements
        !
        n = 1
#ifdef _OPENMP
        ! Cheapest exit first: this one costs nothing and needs neither limit.
        if (omp_in_parallel()) return
        call colwork_gate_limits(min_elements, min_columns)
        if (largest_column_elements(cache, slots, nout) < min_elements) return
        n = omp_get_max_threads()
        cap = parquet_get_table_threads()
        if (cap > 0 .and. cap < n) n = cap
        if (n < 1) n = 1
        ! **Clamped to the affinity mask, like every other thread count this library resolves.**
        ! `omp_get_max_threads()` answers what the environment asked for; a process bound by
        ! `OMP_PROC_BIND` with `OMP_PLACES=cores` may hold far fewer processors than that, and a
        ! team opened at the ICV then time-shares them -- measurably worse than not threading.
        !
        ! **This was the LAST resolver to get it, and the gap was worse here than anywhere else.**
        ! The clamp is also where the once-per-process warning lives, so until this line existed a
        ! table rewrite was the one subsystem that could be silently oversubscribed *without even
        ! the warning firing* -- the other four resolvers would each have reported it. The rule and
        ! the warning live in `parquet_clamp_to_affinity` (src/parquet_settings_base.f90); this must
        ! not grow a second copy of either.
        !
        ! **After the cap, deliberately**, so the helper receives the PRE-clamp count and the
        ! message names what this rewrite actually asked for rather than the environment's ICV.
        n = parquet_clamp_to_affinity(n, "table rewriting")
#endif
    end function colwork_avail
    !
    !> The level the test-only hook asks for: 0 automatic, 1 across columns, 2 within each column.
    !! Read once per mutation through a local `bind(C)` interface, as `join_engine_mode`
    !! (`src/parquet_tables_join.f90`) reads its selector.
    integer function colwork_level_mode() result(mode)
        use iso_c_binding, only : c_int64_t
        interface
            !> The selector's value: 0 automatic, 1 across columns, 2 within a column.
            function get_level() result(res) bind(C, name="parquet_debug_get_colwork_level")
                import :: c_int64_t
                integer(c_int64_t) :: res
            end function get_level
        end interface
        !
        mode = int(get_level())
        if (mode < 0 .or. mode > 2) mode = 0
    end function colwork_level_mode
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
    integer(int64) function largest_column_elements(cache, slots, nout) result(biggest)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer, intent(in) :: slots(:)                !! slots the mutation will rewrite.
        integer(int64), intent(in), optional :: nout   !! destination rows, where they differ from the columns' own.
        integer :: j
        integer(int64) :: here, rows
        !
        biggest = 0_int64
        do j = 1, size(slots)
            ! The larger of what the column holds and what the mutation will produce: a gather
            ! reads the one and writes the other, and either can be the big one.
            rows = cache%cols(slots(j))%values%length()
            if (present(nout)) rows = max(rows, nout)
            here = rows*int(max(cache%cols(slots(j))%values%colwidth(), 1), int64)
            if (here > biggest) biggest = here
        end do
    end function largest_column_elements
    !
    !> Records, for the test suite only, the team the last row-structural mutation used and the
    !! level it spent it at: the count is `max(outer, inner)` -- the size of the one team that
    !! opened -- and the level is 0 serial, 1 across columns, 2 within a column.
    !!
    !! Pushed to two C++ globals for the reason CLAUDE.md gives ("A Fortran-side debug hook has to
    !! be PUBLIC, so prefer a C++ one"): the numbers are locals of `table_colwork`, and a Fortran
    !! hook for them would have to be a public procedure in this module, visible to every
    !! `use parquet`. The same shape as `parquet_debug_note_prefetch_threads`
    !! (src/parquet_tables_read.f90).
    !!
    !! Called once per mutation, on a path about to rewrite every column of a table, so the cost is
    !! unmeasurable. **That ratio is the rule**: a debug hook may sit on a coarse operation like
    !! this one, never on a per-row or per-element path.
    subroutine parquet_debug_note_table_threads(outer, inner)
        use iso_c_binding, only : c_int64_t
        integer, intent(in) :: outer !! columns rewritten at once; 1 for one at a time.
        integer, intent(in) :: inner !! threads inside each column; 1 for serial.
        interface
            !> Records the team's size.
            subroutine set_used(k) bind(C, name="parquet_debug_set_table_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine set_used
            !> Records the level: 0 serial, 1 across columns, 2 within a column.
            subroutine set_level(k) bind(C, name="parquet_debug_set_table_level_used")
                import :: c_int64_t
                integer(c_int64_t), value :: k
            end subroutine set_level
        end interface
        !
        call set_used(int(max(outer, inner), c_int64_t))
        if (outer > 1) then
            call set_level(1_c_int64_t)
        else if (inner > 1) then
            call set_level(2_c_int64_t)
        else
            call set_level(0_c_int64_t)
        end if
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
