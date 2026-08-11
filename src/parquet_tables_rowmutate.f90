!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Mutation that changes a `parquet_table`'s ROW SET: `%filter_rows`, `%sort_by`, `%top_n`,
!! `%delete_rows`, `%truncate`, `%append` and `%append_null_rows`.
!!
!! **Everything in this file detaches the table when it actually changes the row set** (F-mut-7),
!! and that is why it is a separate file from `parquet_tables_mutate.f90`. Once the row set has
!! changed, a column still sitting in the file can never be lined up with the columns already in
!! memory again -- so rather than hand back silently misaligned data later, the table records that
!! it has left its file behind and every later read from that file is a named error.
!!
!! **A call that changes nothing changes nothing at all** -- it does not touch a column, does not
!! invalidate a `%col` pointer, and does not detach. Detaching costs the caller their file, so it
!! is only ever paid for when something actually required it. Five calls reach this file with
!! nothing to do, all decided by the caller's own arguments (`%truncate(n)` with `n >= %nrows()`,
!! `%filter_rows` with an all-`.true.` mask, `%delete_rows` with no indices, `%append` of a
!! zero-row table, `%append_null_rows(0)`), plus one decided by the data (`%sort_by` whose
!! permutation moves no row, which includes every table of fewer than two rows). Each returns
!! early, AFTER its own validation -- a no-op still rejects a bad argument. `%top_n` with
!! `n >= %nrows()` reaches the same place by delegating to `%sort_by`, which is where its own
!! data-decided no-op comes from.
!!
!! **Two rules hold this together, and a new operation added here must follow both:**
!!
!! 1. **Validate everything, THEN mutate everything.** A mutation that aborts halfway leaves some
!!    columns changed and some not -- a state with no diagnostic and no way back. So every check
!!    (mask length, index bounds, key names and kinds, the appended table's column set) runs
!!    before the first column is touched.
!! 2. **Detach LAST.** If a mutation does fail, the table should still be attached and
!!    diagnosable rather than detached and half-changed.
!!
!! **A column that has not been read is skipped, not an obstacle** (`table_mutable_column`). That
!! is the approved policy: requiring every column to be resident first would force a lazy table to
!! read everything it has before it could drop a single row, which is exactly the memory cost
!! laziness and the slice regime exist to avoid. The skipped column is then unreadable for good --
!! detaching sees to that -- so a caller who wants it must `%prefetch` it BEFORE mutating, and the
!! detach guard is what says so if they did not.
!!
!! **`%append` is the one mutation a SHARED table permits**, and the split between its two public
!! entry points and the private `append_table_worker` is what makes that safe: each entry point
!! takes the table's own lock exactly once and then calls the worker, and neither calls the other.
!! An OpenMP simple lock is not recursive, so a second acquisition on one thread deadlocks rather
!! than failing to build -- any future internal caller must come to the worker, never to
!! `table_append_table`. Everything else in this file is refused outright on a shared table
!! (`table_check_not_shared`), because it would pull storage out from under another thread.
submodule (parquet_tables) parquet_tables_rowmutate
    implicit none
    !
contains
    !
    ! ---- the shared machinery -------------------------------------------------------------
    !
    module procedure table_detach
        ! "Detached" means "had a file and can no longer read from it". A table built in memory
        ! never had one, so growing or reordering it detaches nothing and %is_detached must keep
        ! answering .false. -- otherwise every from-scratch table would report itself detached the
        ! moment it was filled. The flag is only ever SET here, never cleared, so a table that has
        ! already detached stays detached through any later mutation.
        if (self%cache%file_backed) then
            ! The reader can never be read from again, so let it go now rather than at
            ! finalization: it holds an open file and whatever Arrow state it cached, and
            ! releasing it here also turns any missed read-after-detach into a clean guard failure
            ! instead of a read that quietly succeeds through a still-live reader.
            if (allocated(self%cache%reader)) then
                call parquet_close_reader(self%cache%reader)
                deallocate(self%cache%reader)
            end if
            self%cache%file_backed = .false.
            self%detached = .true.
        end if
        ! The surviving rows are no longer a contiguous range of FILE rows, so the slice scope
        ! stops meaning anything. A detached table is simply an in-memory table whose rows are its
        ! own, which is what every path that tests file_backed already assumes.
        self%regime = REGIME_FULL
        self%row_lo = 1_int64
        self%row_hi = self%row_count
        if (allocated(self%cache%rg_bounds)) deallocate(self%cache%rg_bounds)
    end procedure table_detach
    !
    module procedure table_mutable_column
        ok = self%cache%cols(idx)%supported .and. self%cache%cols(idx)%residency == RES_FULL
    end procedure table_mutable_column
    !
    module procedure table_apply_keep
        integer, allocatable :: slots(:)
        !
        ! Keeping every row removes none, so there is nothing to rewrite and nothing to detach
        ! for: `delete_by_mask` would reallocate every column's storage to the same contents,
        ! invalidating every %col pointer, and the table would lose its file for a call that did
        ! not change a single row. This covers %filter_rows with an all-.true. mask,
        ! %delete_rows() with no indices, and a filter of a zero-row table.
        if (all(keep)) return
        call table_mutable_slots(self, slots)
        call table_colwork(self%cache, PCW_DELETE_MASK, slots, keep=keep)
        self%row_count = count(keep, kind=int64)
        self%cache%generation = self%cache%generation + 1_int64
        call table_detach(self)
    end procedure table_apply_keep
    !
    ! ---- filter / delete / truncate ---------------------------------------------------------
    !
    module procedure table_filter_rows
        character(len=32) :: got, want
        !
        call table_check_not_shared(self, "filter_rows")
        call table_check_open(self, "filter_rows")
        if (size(keep, kind=int64) /= self%row_count) then
            write(got, "(I0)") size(keep, kind=int64)
            write(want, "(I0)") self%row_count
            error stop EP // "filter_rows: the mask has " // trim(got) // " entries but the " // &
                "table has " // trim(want) // " rows"
        end if
        call table_apply_keep(self, keep, "filter_rows")
    end procedure table_filter_rows
    !
    module procedure table_delete_rows_i32
        call self%delete_rows(int(indices, int64))
    end procedure table_delete_rows_i32
    !
    module procedure table_delete_rows_i64
        logical, allocatable :: keep(:)
        integer(int64) :: k
        character(len=32) :: got, want
        !
        call table_check_not_shared(self, "delete_rows")
        call table_check_open(self, "delete_rows")
        ! Every index is checked before any is used, so a bad one aborts with the table intact.
        do k = 1_int64, size(indices, kind=int64)
            if (indices(k) < 1_int64 .or. indices(k) > self%row_count) then
                write(got, "(I0)") indices(k)
                write(want, "(I0)") self%row_count
                error stop EP // "delete_rows: row index " // trim(got) // " is outside this " // &
                    "table's 1.." // trim(want) // " rows"
            end if
        end do
        allocate(keep(max(self%row_count, 1_int64)))
        keep = .true.
        ! A repeat is harmless by construction: clearing an already-cleared entry changes nothing,
        ! so naming a row twice removes it once.
        do k = 1_int64, size(indices, kind=int64)
            keep(indices(k)) = .false.
        end do
        call table_apply_keep(self, keep(1:self%row_count), "delete_rows")
    end procedure table_delete_rows_i64
    !
    module procedure table_truncate_i32
        call self%truncate(int(n, int64))
    end procedure table_truncate_i32
    !
    module procedure table_truncate_i64
        logical, allocatable :: keep(:)
        character(len=32) :: got
        !
        call table_check_not_shared(self, "truncate")
        call table_check_open(self, "truncate")
        if (n < 0_int64) then
            write(got, "(I0)") n
            error stop EP // "truncate: cannot keep " // trim(got) // " rows"
        end if
        ! Asking to keep more rows than there are is a no-op rather than an error: "give me the
        ! first 1000" is a reasonable thing to say about a table that may have fewer.
        if (n >= self%row_count) return
        allocate(keep(max(self%row_count, 1_int64)))
        keep = .false.
        if (n > 0_int64) keep(1_int64:n) = .true.
        call table_apply_keep(self, keep(1:self%row_count), "truncate")
    end procedure table_truncate_i64
    !
    ! ---- sort -------------------------------------------------------------------------------
    !
    module procedure table_sort_by
        integer(int64), allocatable :: perm(:)
        integer, allocatable :: slots(:)
        !
        call table_check_not_shared(self, "sort_by")
        ! The open check and the key/flag-count validation live in sort_collect_keys
        ! (parquet_tables_sort.f90), so %sort_by, %argsort_by, %is_sorted_by and %argsort_partial
        ! all raise the same messages from one place rather than four copies drifting apart.
        !
        ! Builds (and so validates every key) before a single column is touched.
        call table_build_sort_permutation(self, keys, descending, nulls_first, perm)
        ! A permutation that moves no row leaves the table exactly as it was, so it costs neither
        ! a reindex (which reallocates every column) nor the file. Unlike the other early returns
        ! in this file this one depends on the DATA, not on the arguments: sorting an
        ! already-ordered column keeps the table attached, sorting the same column after an edit
        ! may not. Fewer than two rows always lands here.
        if (permutation_moves_nothing(perm(1:self%row_count))) return
        ! The permutation is validated ONCE, by the first column that takes it, and trusted by every
        ! column after that. `%reindex` validates unconditionally -- correctly, since it is a public
        ! entry point -- so replaying it per column re-checked one permutation the sort engine had
        ! just produced: measured at a THIRD of %sort_by's total time on a 24-column, 20M-row table.
        !
        ! Doing it this way rather than with a validator here is deliberate: the check stays in the
        ! code that owns it, with its existing messages and no fourth copy of the loop (see
        ! check_row_permutation in parquet_columns_structural.f90). The one validation is what stands
        ! between a defective sort engine and silently duplicated rows -- an invalid permutation is
        ! not reachable from user input here, only from a library bug, but that is exactly the class
        ! of failure this project refuses to leave undetected (feature_risks.md Risk-46).
        !
        ! **The validating column is HOISTED OUT of the parallel path, not selected inside it.** A
        ! `validated` flag read and written by every thread would be a race, and a needless one --
        ! taking the first slot serially preserves the invariant by construction, with no shared
        ! state at all, and it keeps the one reachable abort in this loop (an invalid permutation)
        ! on a single thread with its ordinary behaviour. It costs one column's serial time, which
        ! a dynamic schedule would have had to absorb anyway.
        call table_mutable_slots(self, slots)
        if (size(slots) > 0) then
            call self%cache%cols(slots(1))%values%reindex(perm(1:self%row_count))
            if (size(slots) > 1) then
                call table_colwork(self%cache, PCW_REINDEX_TRUSTED, slots(2:), &
                    rows=perm(1:self%row_count))
            end if
        end if
        self%cache%generation = self%cache%generation + 1_int64
        call table_detach(self)
    end procedure table_sort_by
    !
    module procedure table_top_n
        integer(int64), allocatable :: sel(:)
        integer, allocatable :: slots(:)
        !
        call table_check_not_shared(self, "top_n")
        ! Called here rather than left to sort_collect_keys, because the delegation below reads
        ! row_count first -- and calling it here is also what makes an unopened table say "top_n"
        ! rather than "sort_by".
        call table_check_open(self, "top_n")
        ! Keeping every row in key order IS a sort, so it delegates rather than clamping into the
        ! gather path. That is not an optimisation: %sort_by declines to touch a table whose rows are
        ! already in the order asked for, and going through the gather would detach it instead.
        if (int(n, int64) >= self%row_count) then
            call table_sort_by(self, keys, descending, nulls_first)
            return
        end if
        ! Selects (and so validates every key) before a single column is touched. Also rejects a
        ! negative n.
        call table_build_top_n_permutation(self, keys, n, descending, nulls_first, sel)
        call check_selection(sel, self%row_count)
        ! No hoist here, unlike %sort_by: `%gather` does its own range check on every column and
        ! there is no trusted variant to skip it with, so every column is on the same path and the
        ! whole list goes to the worker. `check_selection` above is what stands in for the sort's
        ! one validating column -- it is the duplicate scan `%gather` deliberately does not do.
        call table_mutable_slots(self, slots)
        call table_colwork(self%cache, PCW_GATHER, slots, rows=sel)
        self%row_count = int(n, int64)
        self%cache%generation = self%cache%generation + 1_int64
        call table_detach(self)
    end procedure table_top_n
    !
    !> Checks that a selection really is a set of distinct rows of this table, before any column
    !! takes it.
    !!
    !! `%gather` deliberately does not do this itself: it permits repeats, and a per-column duplicate
    !! scan would need a seen-set sized by the ROW COUNT once per column, which is the cost the
    !! gather path exists to avoid. Checking once here costs one such seen-set for the whole
    !! operation, and it is the only thing standing between a defective selection engine and a table
    !! whose rows are silently duplicated -- the same reasoning that keeps exactly one column of
    !! `%sort_by` on the validating path.
    !!
    !! This is a fourth copy of the bit-packed seen-set shape (`check_row_permutation` in
    !! `parquet_columns_structural.f90`, `parquet_string_column%reindex`, `check_permutation` in
    !! `parquet_sorting_keys.f90`), and deliberately so: **all three of those check a permutation of
    !! length nrows**, where this checks a SUBSET of length n against a range of nrows -- sized by
    !! `nrows` bits but scanned over `n` entries. It cannot be merged with them and is not a
    !! duplicate to delete.
    subroutine check_selection(sel, nrows)
        integer(int64), intent(in) :: sel(:)  !! the selected 1-based row indices.
        integer(int64), intent(in) :: nrows   !! the table's row count.
        integer(int64), allocatable :: seen(:)
        integer(int64) :: k, p, word
        character(len=32) :: got, want
        !
        if (size(sel, kind=int64) == 0_int64) return
        ! 64-bit words, matching parquet_column's own validity bitmap rather than the int8 the two
        ! permutation checks use -- int8 is not in this module's iso_fortran_env import list, and the
        ! word width makes no difference to a scan this short.
        allocate(seen((nrows + 63_int64)/64_int64))
        seen = 0_int64
        ! Both arms below are DEFENSIVE and have no error scenario: the selection comes from
        ! pf_partial_argsort, not from the caller, so reaching either means the sort engine returned
        ! something that is not a set of distinct rows of this table. That is not constructible from
        ! user input -- only from a library bug -- which is exactly the class this check exists to
        ! catch rather than let through as silently duplicated rows.
        !
        ! Both `if` lines below therefore carry the artifact tag: the CONDITION is evaluated once
        ! per selected row whether or not the guard fires, so gcov counts each of those two lines
        ! as hit (1017 apiece in one full run) while every line of both guarded bodies stays at 0.
        ! That is the guard-clause shape CLAUDE.md's "Fortran gcov attribution artifacts" note
        ! describes, and the zero-count bodies are what distinguishes it from a live exclusion.
        do k = 1_int64, size(sel, kind=int64)
            p = sel(k)
            if (p < 1_int64 .or. p > nrows) then ! GCOVR_EXCL_START -- gcov attribution artifact
                write(got, "(I0)") p
                write(want, "(I0)") nrows
                error stop EP // "top_n: the selection names row " // trim(got) // ", outside " // &
                    "this table's 1.." // trim(want) // " rows"
            end if ! GCOVR_EXCL_STOP
            word = (p - 1_int64)/64_int64 + 1_int64
            ! gcov attribution artifact -- see the note above the first guard in this loop.
            if (btest(seen(word), int(mod(p - 1_int64, 64_int64)))) then ! GCOVR_EXCL_START
                write(got, "(I0)") p
                error stop EP // "top_n: the selection names row " // trim(got) // " twice"
            end if ! GCOVR_EXCL_STOP
            seen(word) = ibset(seen(word), int(mod(p - 1_int64, 64_int64)))
        end do
    end subroutine check_selection
    !
    !> .true. when a sort permutation sends every row to its own position, i.e. the sort is a
    !! no-op. A zero- or one-row table always answers .true.
    logical function permutation_moves_nothing(perm) result(still)
        integer(int64), intent(in) :: perm(:) !! the permutation, one destination row per source row.
        integer(int64) :: k
        !
        still = .true.
        do k = 1_int64, size(perm, kind=int64)
            if (perm(k) /= k) then
                still = .false.
                return
            end if
        end do
    end function permutation_moves_nothing
    !
    ! ---- append -----------------------------------------------------------------------------
    !
    module procedure table_append_table
        call table_check_open(self, "append")
        call table_lock(self%cache)
        call append_begin(self)
        call append_table_worker(self, other)
        call append_end(self)
        call table_unlock(self%cache)
    end procedure table_append_table
    !
    !> Marks an append as in progress, and refuses to start one while a read is in flight.
    !!
    !! Called with the table's lock already held, by each of the two public %append entry points.
    !! Reading the table while any thread appends to it is forbidden, so an append starting while
    !! a read is in flight is the same violation seen from the other side -- caught here, under the
    !! lock, where the count cannot change for the duration of the check.
    !!
    !! Best-effort in the same way `table_check_no_append` is: a read that begins between this
    !! check and the append's first reallocation is not seen. That is why the append-only rule
    !! stays a documented contract with these two checks as its safety net rather than its
    !! mechanism. Note also that only the COARSE read entry points maintain `readers_active` --
    !! a per-element accessor cannot afford two atomics per cell, so an element-by-element read
    !! racing an append is caught by the reader's own `table_check_no_append`, not by this.
    subroutine append_begin(self)
        class(parquet_table), intent(in) :: self !! the table about to be appended to.
        integer :: active
        !
        active = 0
        !$omp atomic read
        active = self%cache%readers_active
        if (active /= 0) then
            error stop EP // "append: another thread is reading this table right now. A parallel " // &
                "append region is append-only -- no thread may read the table while any thread " // &
                "is appending to it, because the append reallocates every column's storage."
        end if
        !$omp atomic update
        self%cache%append_active = self%cache%append_active + 1
    end subroutine append_begin
    !
    !> Ends an append started by `append_begin`. Must run on every exit path from it.
    subroutine append_end(self)
        class(parquet_table), intent(in) :: self !! the table that was appended to.
        !
        !$omp atomic update
        self%cache%append_active = self%cache%append_active - 1
    end subroutine append_end
    !
    !> The unlocked body of `%append`.
    !!
    !! **Both public %append entry points take the table's lock themselves and then call this;
    !! neither calls the other.** That is exactly one acquisition per public call, which is what an
    !! OpenMP simple lock requires -- it is not recursive, so a second acquisition on one thread
    !! deadlocks rather than failing to build. Any future internal caller must come here, never to
    !! `table_append_table`, and must already hold the lock if it can be reached concurrently.
    !!
    !! The lock covers the validation passes as well as the mutation, so a half-appended table is
    !! never visible to another thread, and `table_detach`/the `generation` bump happen under it
    !! too. `other` is deliberately NOT locked: it is the appending thread's own private table by
    !! construction, and locking two tables in one operation would introduce a lock-ordering
    !! problem (A appends B while B appends A) for no gain.
    !> Whether slot `idx` of a column store is one an append can read values out of.
    !!
    !! The cache-level form of `table_mutable_column`, needed because a row handle carries a
    !! `parquet_table_cache` pointer and not a table -- so the row path has no table to ask.
    logical function cache_column_usable(cache, idx) result(ok)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer, intent(in) :: idx                     !! slot index.
        ok = cache%cols(idx)%supported .and. cache%cols(idx)%residency == RES_FULL
    end function cache_column_usable
    !
    !> The validation both append workers run before either of them mutates anything: every column
    !! the source has must exist in the destination and be compatible with it.
    !!
    !! **One helper rather than a copy in each worker**, because these are the rules most damaging
    !! to get subtly different between the two: a table append and a row append that disagreed
    !! about which column mismatches are refused would be a difference nothing reports. Takes the
    !! source's CACHE rather than a table so the row path, which only has a cache pointer, can use
    !! it unchanged.
    !!
    !! Runs in full before a single value is written, which is rule 1 of this file's own header --
    !! a mutation that aborts halfway leaves some columns longer than others, with no diagnostic
    !! and no way back.
    subroutine append_validate_source(self, src)
        class(parquet_table), intent(in) :: self     !! the destination table.
        type(parquet_table_cache), intent(in) :: src !! the source table's column store.
        integer :: i, j
        !
        do j = 1, src%ncols
            if (.not. cache_column_usable(src, j)) cycle
            i = table_find(self, src%cols(j)%name)
            if (i == 0) call append_unknown_column(self, src%cols(j)%name)
            if (.not. table_mutable_column(self, i)) cycle
            call append_check_compatible(self, i, src, j)
        end do
    end subroutine append_validate_source
    !
    subroutine append_table_worker(self, other)
        class(parquet_table), intent(inout) :: self !! the table to grow.
        class(parquet_table), intent(in) :: other   !! the table whose rows are appended.
        integer :: i, j
        integer(int64) :: added
        !
        call table_check_open(self, "append")
        call table_check_open(other, "append")
        call append_validate_source(self, other%cache)
        added = other%row_count
        ! Appending no rows adds nothing, so the table keeps its columns' storage and its file --
        ! but only after the compatibility checks above have run, so an incompatible zero-row
        ! table is still refused rather than quietly accepted.
        if (added == 0_int64) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            j = cache_find(other%cache, self%cache%cols(i)%name)
            if (j == 0) then
                ! M1's default fill: a column this table has and `other` does not gets nulls for
                ! the appended rows, rather than the append being refused.
                call self%cache%cols(i)%values%append_nulls(added)
            else
                call self%cache%cols(i)%values%append(other%cache%cols(j)%values)
            end if
        end do
        self%row_count = self%row_count + added
        self%cache%generation = self%cache%generation + 1_int64
        call table_detach(self)
    end subroutine append_table_worker
    !
    !> The unlocked body of `%append(row)`, and the counterpart of `append_table_worker`.
    !!
    !! **Appends the row DIRECTLY, column by column.** It used to deep-copy every column of the
    !! row's whole source table, delete all but one row of each, assemble a one-row
    !! `parquet_table` from the pieces and append that -- so appending N rows out of a source of M
    !! cost O(N*M) element copies plus a table's worth of allocation per row. `%append_row_of` on
    !! `parquet_column` copies exactly the one row, so nothing here scales with the source's size.
    !!
    !! Every rule `append_table_worker` follows is followed here too, deliberately: the shared
    !! validation above, the null fill for a column the row's table does not have, the refusal of a
    !! column it has and this table does not, the skip of a column that is not resident, and the
    !! row-count / generation / detach bookkeeping at the end.
    subroutine append_row_worker(self, r)
        class(parquet_table), intent(inout) :: self !! the table to grow.
        type(parquet_table_row), intent(in) :: r    !! the row to append.
        integer :: i, j, matched
        !
        call table_check_open(self, "append")
        ! With nothing in common there is no row to append -- every column would be null-filled,
        ! which is %append_null_rows(1) said in a confusing way, and far more likely a mistake.
        ! Counted over THIS table's columns, so it asks what the append would actually write.
        matched = 0
        do i = 1, self%cache%ncols
            if (cache_find(r%cache, self%cache%cols(i)%name) > 0) matched = matched + 1
        end do
        if (matched == 0) error stop EP // "append: the row's table has no column in common " // &
            "with this table, so there is nothing to append"
        call append_validate_source(self, r%cache)
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            j = cache_find(r%cache, self%cache%cols(i)%name)
            ! A source column that is not resident is treated as ABSENT, exactly as the validation
            ! above already treats it -- so this table's column is null-filled rather than being
            ! handed a row index into storage that was never read. The two have to agree: a
            ! validation that skips a column and an append loop that does not would write a row
            ! from an empty column.
            if (j > 0) then
                if (.not. cache_column_usable(r%cache, j)) j = 0
            end if
            if (j == 0) then
                call self%cache%cols(i)%values%append_nulls(1_int64)
            else
                call self%cache%cols(i)%values%append_row_of(r%cache%cols(j)%values, r%irow)
            end if
        end do
        self%row_count = self%row_count + 1_int64
        self%cache%generation = self%cache%generation + 1_int64
        call table_detach(self)
    end subroutine append_row_worker
    !
    module procedure table_append_row
        call table_check_open(self, "append")
        ! Validated BEFORE the lock and before anything is written, so a rejected append leaves
        ! the destination exactly as it was.
        !
        ! Two cases, two messages, and the split is the point. A handle on ANOTHER table that has
        ! since changed is the ordinary staleness case, and "re-fetch it" is good advice -- it is
        ! also the one staleness case that corrupts a DIFFERENT table's data, since the row index
        ! would be read against a source whose rows have moved. A handle on the DESTINATION is a
        ! different mistake: appending bumps this table's own generation, so such a handle is
        ! self-invalidating and re-fetching it inside the loop would fail again on the next
        ! iteration. Telling that caller to re-fetch would send them round the same loop.
        if (.not. r%is_valid()) then
            if (associated(self%cache, r%cache)) then
                error stop EP // "append: this row handle names the table being appended to, and " // &
                    "the append invalidated it; a table cannot be grown from a handle on itself " // &
                    "-- take a %clone first and append that, or use %append_null_rows and fill"
            end if
            call row_check_current(r, "append")
        end if
        ! This is a public entry point, not an internal caller, so it takes the lock itself and
        ! goes to the worker -- never to table_append_table, which would acquire a second time and
        ! deadlock (an OpenMP simple lock is not recursive). Everything the worker does happens
        ! inside the lock: it reads `self`'s column list, which a concurrent append is in the
        ! middle of changing.
        call table_lock(self%cache)
        call append_begin(self)
        call append_row_worker(self, r)
        call append_end(self)
        call table_unlock(self%cache)
    end procedure table_append_row
    !
    module procedure table_append_null_rows_i32
        call self%append_null_rows(int(n, int64))
    end procedure table_append_null_rows_i32
    !
    module procedure table_append_null_rows_i64
        integer :: i
        character(len=32) :: got
        !
        call table_check_not_shared(self, "append_null_rows")
        call table_check_open(self, "append_null_rows")
        if (n < 0_int64) then
            write(got, "(I0)") n
            error stop EP // "append_null_rows: cannot append " // trim(got) // " rows"
        end if
        ! Appending no rows leaves the row set exactly as it was, so it does not detach: the
        ! table keeps its file, and a later touch can still read a column it has not read yet.
        if (n == 0_int64) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            call self%cache%cols(i)%values%append_nulls(n)
        end do
        self%row_count = self%row_count + n
        self%cache%generation = self%cache%generation + 1_int64
        call table_detach(self)
    end procedure table_append_null_rows_i64
    !
    !> Reports a column `other` has and this table does not. Never silently dropped: losing a
    !! column's values because the destination happened not to have that column is the kind of
    !! quiet data loss an append should refuse to perform.
    subroutine append_unknown_column(self, name)
        class(parquet_table), intent(in) :: self !! the destination table.
        character(len=*), intent(in) :: name     !! the offending column name.
        character(len=:), allocatable :: sfx
        !
        call table_context_suffix(self%cache, name, sfx)
        error stop EP // "append: the appended table has a column this table does not; add it " // &
            "first (%add_column) or drop it there, but it will not be silently dropped" // sfx
    end subroutine append_unknown_column
    !
    !> error stops unless two columns can be concatenated: same kind, same width, same unit.
    !!
    !! A kind mismatch is a hard error rather than a silent widen. The widening rule elsewhere in
    !! this type is a READ rule -- it chooses the type a caller receives -- whereas an append
    !! would have to change the destination column's stored kind behind the caller's back.
    !! `%cast` is the explicit way round it.
    !!
    !! A unit mismatch is an error for a sharper reason: there is no unit conversion in this
    !! library, so concatenating "m/s" rows with "km/h" rows would produce a column whose rows
    !! mean different things with nothing recording it. A column with NO declared unit on either
    !! side is accepted, keeping the destination's -- an in-memory table built without `unit=` is
    !! ordinary and should not be unappendable.
    subroutine append_check_compatible(self, i, src, j)
        class(parquet_table), intent(in) :: self       !! the destination table.
        integer, intent(in) :: i                       !! destination slot.
        type(parquet_table_cache), intent(in) :: src   !! the source table's column store.
        integer, intent(in) :: j                       !! source slot.
        character(len=:), allocatable :: sfx, mine, theirs
        character(len=32) :: got, want
        !
        if (self%cache%cols(i)%values%kindof() /= src%cols(j)%values%kindof()) then
            call parquet_kind_name(self%cache%cols(i)%values%kindof(), mine)
            call parquet_kind_name(src%cols(j)%values%kindof(), theirs)
            call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
            error stop EP // "append: this column is " // mine // " here but " // theirs // &
                " in the appended table; convert it first (%cast)" // sfx
        end if
        if (self%cache%cols(i)%values%colwidth() /= src%cols(j)%values%colwidth()) then
            write(want, "(I0)") self%cache%cols(i)%values%colwidth()
            write(got, "(I0)") src%cols(j)%values%colwidth()
            call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
            error stop EP // "append: this column holds " // trim(want) // " values per row " // &
                "here but " // trim(got) // " in the appended table" // sfx
        end if
        call self%cache%cols(i)%values%unit_string(mine)
        call src%cols(j)%values%unit_string(theirs)
        if (len_trim(mine) > 0 .and. len_trim(theirs) > 0 .and. trim(mine) /= trim(theirs)) then
            call table_context_suffix(self%cache, self%cache%cols(i)%name, sfx)
            error stop EP // "append: this column is in '" // trim(mine) // "' here but '" // &
                trim(theirs) // "' in the appended table, and this library does not convert " // &
                "units" // sfx
        end if
    end subroutine append_check_compatible
    !
end submodule parquet_tables_rowmutate
