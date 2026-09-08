!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The join's HASH engine: the pair list built on a `pf_index_multimap` over the RIGHT keys and
!! probed once per left row, in place of one `pf_argsort` over both key columns concatenated.
!!
!! **NOT a generated file** -- `tools/generate_parquet_tables.py` emits the module's spec, and
!! the parent submodule (`parquet_tables_join.f90`) declares this engine's interface; a signature
!! change is an edit there, a body change is not.
!!
!! **Same answer, different arithmetic.** `join_pairs_sort` (the parent) and this engine must
!! agree to the row on every call the dispatcher can hand here, and `test/test_table_join.f90`
!! runs its whole suite under each of them to hold that. What the sort engine reads off the run
!! structure of one permutation, this one has to provide itself, clause by clause:
!!
!!   * **Key codes.** Every key column goes through `index_extract_keys`, the table layer's ONE
!!     key conversion (feature_risks.md Risk-211): an integer as it is, a real by
!!     `parquet_index_real_key` -- every NaN one key, `-0.0` equal to `+0.0`, which is the sort
!!     comparator's equality and the guide page's rule -- a date or time by its raw storage, a
!!     timestamp as its unit-free `(seconds, nanoseconds)` pair. One key is a scalar code; more
!!     than one, or a timestamp, is an `(n, ncomp)` tuple. A row's validity is the CONJUNCTION
!!     over its keys of each column's, so a null in any key column makes the row match nothing.
!!     A single string key is handed to the multimap's own string forms in place, by its exact
!!     bytes, with no conversion at all.
!!   * **Build over the RIGHT keys, probe with the LEFT.** Always that way round: the probe order
!!     is the left order, and `%probe_many` answers one range per probe, in probe order,
!!     ascending by right row within a range -- which is exactly `order="left"`'s pair list, with
!!     no further pass. A null-keyed right row is in no group (the build skipped it); a null-keyed
!!     left row has an empty range without being looked up.
!!   * **`require=`.** `"m:1"` (and the right half of `"1:1"`): a probe whose range holds two
!!     right rows names them and the range length through `join_refuse_require`, the message the
!!     sort engine gives. `%max_multiplicity()` is NOT the test: it counts a repeated key that
!!     matched nothing, which the guide says is not counted. `"1:m"` (and the left half): a right
!!     row appearing in two probes' ranges has two left rows on its key; a mask over the right
!!     rows finds it in one pass over the matches, and the two probes are named on the abort path
!!     alone.
!!   * **Counting.** `n_pairs` is the match count (the multimap refuses a pair count above
!!     `huge(int64)` itself); the unmatched left rows are the probes with an empty range,
!!     null-keyed ones included; the unmatched right rows are every row of a group no probe
!!     reached (`group_hit`, read against `%csr`) plus every null-keyed right row, computed only
!!     for the two `how` values that emit them. `biggest`, which only the `max_rows=` message
!!     uses, is computed on the refusal path alone.
!!   * **Emission**, `order="left"` only: one exclusive scan over the probes gives each left row
!!     its output position, so the fill writes disjoint ranges of `il`/`ir` on a team; `semi` and
!!     `anti` are the same walk with one pair or none per row; then, for `right`/`outer`, the
!!     unmatched right rows in right-table order.
!!
!! What is deliberately NOT here: `order="key"` (the hash engine has no key order to offer), a
!! `PK_LOGICAL` key (two groups, and the sort's counting path is already O(n) there), and a
!! string key beside another key (the multimap's tuple is integer-only) -- `join_hash_eligible`
!! sends all three to the sort engine, and forcing this engine through the hook does not override
!! it. The rewrite that follows (`join_apply`) is shared and untouched.
submodule (parquet_tables:parquet_tables_join) parquet_tables_join_hash
    use parquet_index, only : pf_index_multimap
    implicit none
    !
contains
    !
    module procedure join_pairs_hash
        type(pf_index_multimap) :: mm
        type(parquet_string_column), pointer :: lsc, rsc
        integer(int64), allocatable :: offsets(:), matches(:)
        integer(int64), allocatable :: lkeys(:), rkeys(:), ltup(:, :), rtup(:, :)
        logical, allocatable :: lvalid(:), rvalid(:), hit(:), unm(:)
        integer(int64) :: nl, nr, n_pairs, n_matched, n_lunm, n_runm, biggest, i
        !
        ! Recorded FIRST, before anything can abort -- see join_pairs_sort for why the engine
        ! body writes it and not the dispatcher.
        call join_note_engine(ENGINE_HASH)
        nullify(lsc, rsc)
        nl = self%nrows()
        nr = other%nrows()
        ! This engine's team for its own pass (the emission): the index tier's rule, as the
        ! multimap's build and probe resolve theirs. Handed back for the caller's passes over
        ! the pair list. The sort engine's group-pass record is written as 0 here: this engine
        ! has no such passes, and a value left by an earlier sort-engine join would otherwise
        ! read as this join's.
        team = index_team(nl, threads)
        call join_note_group_threads(0)
        call hash_key_codes(self, lslots, nl, threads, lkeys, ltup, lvalid, lsc)
        call hash_key_codes(other, rslots, nr, threads, rkeys, rtup, rvalid, rsc)
        call hash_build_and_probe(mm, nl, nr, threads, lkeys, rkeys, ltup, rtup, lvalid, rvalid, &
            lsc, rsc, offsets, matches, n_matched, hit)
        call hash_check_require(req_id, nl, nr, offsets, matches)
        n_pairs = size(matches, kind=int64)
        n_lunm = nl - n_matched
        n_runm = 0_int64
        if (how_id == HOW_RIGHT .or. how_id == HOW_OUTER) then
            call hash_unmatched_right(mm, nr, hit, unm, n_runm)
        end if
        ! The same sums, through the same guarded addition, as join_count.
        select case (how_id)
        case (HOW_LEFT)
            n_out = join_add_checked(n_pairs, n_lunm)
        case (HOW_RIGHT)
            n_out = join_add_checked(n_pairs, n_runm)
        case (HOW_OUTER)
            n_out = join_add_checked(join_add_checked(n_pairs, n_lunm), n_runm)
        case (HOW_SEMI)
            n_out = nl - n_lunm
        case (HOW_ANTI)
            n_out = n_lunm
        case default
            n_out = n_pairs
        end select
        if (present(max_rows)) then
            if (n_out > max_rows) then
                call hash_biggest(mm, nl, threads, lkeys, ltup, lvalid, lsc, biggest)
                call join_refuse_size(n_out, max_rows, nl, nr, biggest)
            end if
        end if
        if (present(matched)) then
            allocate(matched(nl))
            do i = 1_int64, nl
                matched(i) = offsets(i + 1_int64) > offsets(i)
            end do
        end if
        allocate(il(n_out), ir(n_out))
        if (n_out < 1_int64) return
        call hash_emit(how_id, nl, nr, offsets, matches, unm, team, il, ir)
    end procedure join_pairs_hash
    !
    !> Clause 2: one side's key codes, in whichever of the three shapes the multimap takes.
    !!
    !! A single non-timestamp key is a scalar `keys(n)`; anything else is a `tup(n, ncomp)`
    !! tuple, one column per key and two for a timestamp, in `slots` order; a single string key
    !! is the column's own string store, `sc`, and no array at all. Exactly one of the three is
    !! produced, and `allocated()`/`associated()` on the result is how the callers tell which.
    !! `valid` is the conjunction over the keys of each column's own mask, and stays unallocated
    !! -- which the engine reads as "every row valid" -- when no key column holds a null.
    subroutine hash_key_codes(t, slots, n, threads, keys, tup, valid, sc)
        class(parquet_table), intent(in) :: t     !! the table this side is.
        integer, intent(in) :: slots(:)           !! its key slots, primary first.
        integer(int64), intent(in) :: n           !! its rows.
        integer, intent(in), optional :: threads  !! team for the validity passes.
        integer(int64), allocatable, intent(out) :: keys(:)    !! scalar codes, one key.
        integer(int64), allocatable, intent(out) :: tup(:, :)  !! `(n, ncomp)` tuples, otherwise.
        logical, allocatable, intent(out) :: valid(:)          !! per row; unallocated = all valid.
        type(parquet_string_column), pointer, intent(out) :: sc !! the store, for a string key.
        integer(int64), allocatable :: k1(:), p2(:, :)
        logical, allocatable :: v1(:)
        integer :: j, kd, ncomp, c
        !
        nullify(sc)
        if (t%cache%cols(slots(1))%values%kindof() == PK_STRING) then
            ! The store in place, and its own null mask through the same routine every other
            ! kind's comes from, so the two sides' masks are built by one rule.
            call parquet_column_string_column(t%cache%cols(slots(1))%values, sc)
            call index_extract_keys(t%cache%cols(slots(1))%values, PK_STRING, n, k1, p2, valid, &
                threads)
            return
        end if
        ncomp = 0
        do j = 1, size(slots)
            if (t%cache%cols(slots(j))%values%kindof() == PK_TIMESTAMP) then
                ncomp = ncomp + 2
            else
                ncomp = ncomp + 1
            end if
        end do
        if (ncomp > 1) allocate(tup(n, ncomp))
        c = 0
        do j = 1, size(slots)
            kd = t%cache%cols(slots(j))%values%kindof()
            call index_extract_keys(t%cache%cols(slots(j))%values, kd, n, k1, p2, v1, threads)
            if (ncomp == 1) then
                call move_alloc(k1, keys)
            else if (kd == PK_TIMESTAMP) then
                tup(:, c + 1) = p2(:, 1)
                tup(:, c + 2) = p2(:, 2)
                c = c + 2
            else
                tup(:, c + 1) = k1
                c = c + 1
            end if
            if (allocated(v1)) then
                if (allocated(valid)) then
                    valid = valid .and. v1
                else
                    call move_alloc(v1, valid)
                end if
            end if
        end do
    end subroutine hash_key_codes
    !
    !> Clauses 3 and 4: the multimap over the right keys, probed with the left ones, in whichever
    !! shape `hash_key_codes` produced. Nothing is built for an empty right table and nothing
    !! probed for an empty left one; both then answer as the sort engine does, every row on either
    !! side unmatched.
    subroutine hash_build_and_probe(mm, nl, nr, threads, lkeys, rkeys, ltup, rtup, lvalid, &
            rvalid, lsc, rsc, offsets, matches, n_matched, hit)
        type(pf_index_multimap), intent(inout) :: mm !! receives the build.
        integer(int64), intent(in) :: nl             !! left rows.
        integer(int64), intent(in) :: nr             !! right rows.
        integer, intent(in), optional :: threads     !! team for the build and the probe.
        integer(int64), allocatable, intent(in) :: lkeys(:)   !! left scalar codes, if that shape.
        integer(int64), allocatable, intent(in) :: rkeys(:)   !! right scalar codes, likewise.
        integer(int64), allocatable, intent(in) :: ltup(:, :) !! left tuples, if that shape.
        integer(int64), allocatable, intent(in) :: rtup(:, :) !! right tuples, likewise.
        logical, allocatable, intent(in) :: lvalid(:) !! left validity; unallocated = all valid.
        logical, allocatable, intent(in) :: rvalid(:) !! right validity, likewise.
        type(parquet_string_column), pointer, intent(in) :: lsc !! the left store, if a string key.
        type(parquet_string_column), pointer, intent(in) :: rsc !! the right store, likewise.
        !> probe `i`'s right rows are `matches(offsets(i) : offsets(i+1) - 1)`; length `nl + 1`.
        integer(int64), allocatable, intent(out) :: offsets(:)
        integer(int64), allocatable, intent(out) :: matches(:) !! the right rows, per probe, ascending.
        integer(int64), intent(out) :: n_matched     !! probes with a non-empty range.
        logical, allocatable, intent(out) :: hit(:)  !! per group: some probe reached it.
        !
        ! An unallocated `rvalid`/`lvalid` handed to the optional `valid=` makes it ABSENT
        ! (F2018 15.5.2.12), which is the "every row valid" the engine wants; the same idiom
        ! %build_index uses.
        if (nr > 0_int64) then
            if (associated(rsc)) then
                call mm%build(rsc, valid=rvalid, threads=threads)
            else if (allocated(rtup)) then
                call mm%build(rtup, valid=rvalid, threads=threads)
            else
                call mm%build(rkeys, valid=rvalid, threads=threads)
            end if
        end if
        if (nl > 0_int64 .and. nr > 0_int64) then
            if (associated(lsc)) then
                call mm%probe_many(lsc, offsets, matches, valid=lvalid, threads=threads, &
                    n_matched=n_matched, group_hit=hit)
            else if (allocated(ltup)) then
                call mm%probe_many(ltup, offsets, matches, valid=lvalid, threads=threads, &
                    n_matched=n_matched, group_hit=hit)
            else
                call mm%probe_many(lkeys, offsets, matches, valid=lvalid, threads=threads, &
                    n_matched=n_matched, group_hit=hit)
            end if
        else
            allocate(offsets(nl + 1_int64))
            offsets = 1_int64
            allocate(matches(0))
            n_matched = 0_int64
            allocate(hit(mm%ngroups()))
            hit = .false.
        end if
    end subroutine hash_build_and_probe
    !
    !> Clause 5: the cardinality assertion, from the CSR alone, with the sort engine's exclusions
    !! by construction: a null-keyed row is in no range on either side, and a repeated key that
    !! matched nothing has no range to be seen in -- so, as `join_check_require` has it,
    !! `require=` asserts something about the rows that TOOK PART in the match.
    subroutine hash_check_require(req_id, nl, nr, offsets, matches)
        integer, intent(in) :: req_id            !! REQ_* token.
        integer(int64), intent(in) :: nl         !! left rows, i.e. probes.
        integer(int64), intent(in) :: nr         !! right rows.
        integer(int64), intent(in) :: offsets(:) !! the probe ranges.
        integer(int64), intent(in) :: matches(:) !! the right rows per probe, ascending.
        logical, allocatable :: seen(:)
        integer(int64) :: i, t, r
        !
        if (req_id == REQ_MM) return
        if (req_id == REQ_11 .or. req_id == REQ_M1) then
            ! A probe with two right rows in its range is a left row that matched twice: the
            ! right key repeats among the rows that took part. The two rows named are the first
            ! two of the range, ascending, which is what the sort engine's group listing names.
            do i = 1_int64, nl
                if (offsets(i + 1_int64) - offsets(i) > 1_int64) then
                    call join_refuse_require("m:1", "right", "other_on=", matches(offsets(i)), &
                        matches(offsets(i) + 1_int64), offsets(i + 1_int64) - offsets(i))
                end if
            end do
        end if
        if (req_id == REQ_11 .or. req_id == REQ_1M) then
            ! A right row two probes both reached has two left rows on its key. The probes are
            ! walked in left order, so the FIRST repeat found names the two lowest left rows
            ! sharing that key -- again the two the sort engine's ascending listing names.
            allocate(seen(max(nr, 1_int64)))
            seen = .false.
            do i = 1_int64, nl
                do t = offsets(i), offsets(i + 1_int64) - 1_int64
                    r = matches(t)
                    if (seen(r)) call hash_refuse_1m(nl, offsets, matches, r, i)
                    seen(r) = .true.
                end do
            end do
        end if
    end subroutine hash_check_require
    !
    !> The `"1:m"` abort path: names the first two left rows sharing right row `r`'s key and how
    !! many share it in all -- one walk over every probe's range, affordable because it ends in
    !! `join_refuse_require`'s `error stop`.
    subroutine hash_refuse_1m(nl, offsets, matches, r, second)
        integer(int64), intent(in) :: nl         !! left rows, i.e. probes.
        integer(int64), intent(in) :: offsets(:) !! the probe ranges.
        integer(int64), intent(in) :: matches(:) !! the right rows per probe.
        integer(int64), intent(in) :: r          !! the right row found in two ranges.
        integer(int64), intent(in) :: second     !! the probe that found it the second time.
        integer(int64) :: i, t, first, n_dup
        !
        first = 0_int64
        n_dup = 0_int64
        do i = 1_int64, nl
            do t = offsets(i), offsets(i + 1_int64) - 1_int64
                if (matches(t) /= r) cycle
                n_dup = n_dup + 1_int64
                if (first == 0_int64) first = i
                exit
            end do
        end do
        call join_refuse_require("1:m", "left", "on=", first, second, n_dup)
    end subroutine hash_refuse_1m
    !
    !> Clause 6's right half: every right row no probe reached -- the rows of every group
    !! `group_hit` does not mark, plus every null-keyed row, which is in no group at all -- as the
    !! mask `hash_emit` walks in right-table order, and its count.
    subroutine hash_unmatched_right(mm, nr, hit, unm, n_runm)
        type(pf_index_multimap), intent(in) :: mm    !! the build over the right keys.
        integer(int64), intent(in) :: nr             !! right rows.
        logical, intent(in) :: hit(:)                !! per group: some probe reached it.
        logical, allocatable, intent(out) :: unm(:)  !! per right row: no counterpart.
        integer(int64), intent(out) :: n_runm        !! how many of them.
        integer(int64), allocatable :: goff(:), grows(:)
        integer(int64) :: g, t
        !
        allocate(unm(max(nr, 1_int64)))
        unm = .true.
        ! Cleared group by group off the CSR rather than row by row off `matches`: a hit group's
        ! rows appear in `matches` once per probe that reached it, so that pass would be the
        ! pair count long where this one is the right row count long.
        call mm%csr(goff, grows)
        do g = 1_int64, size(hit, kind=int64)
            if (.not. hit(g)) cycle
            do t = goff(g), goff(g + 1_int64) - 1_int64
                unm(grows(t)) = .false.
            end do
        end do
        n_runm = count(unm(1:nr), kind=int64)
    end subroutine hash_unmatched_right
    !
    !> The largest single key group's pair count, for the `max_rows=` message alone: every left
    !! key's group id (`%get_many`) histogrammed against the right group sizes (`%csr`). Two
    !! O(nl) passes that only a refused join pays for.
    subroutine hash_biggest(mm, nl, threads, lkeys, ltup, lvalid, lsc, biggest)
        type(pf_index_multimap), intent(in) :: mm    !! the build over the right keys.
        integer(int64), intent(in) :: nl             !! left rows.
        integer, intent(in), optional :: threads     !! team for the lookup.
        integer(int64), allocatable, intent(in) :: lkeys(:)   !! left scalar codes, if that shape.
        integer(int64), allocatable, intent(in) :: ltup(:, :) !! left tuples, if that shape.
        logical, allocatable, intent(in) :: lvalid(:) !! left validity; unallocated = all valid.
        type(parquet_string_column), pointer, intent(in) :: lsc !! the left store, if a string key.
        integer(int64), intent(out) :: biggest       !! the largest group's pair count.
        integer(int64), allocatable :: groups(:), goff(:), grows(:), lcount(:)
        integer(int64) :: i, g, ng
        !
        biggest = 0_int64
        ng = mm%ngroups()
        if (nl < 1_int64 .or. ng < 1_int64) return
        allocate(groups(nl))
        if (associated(lsc)) then
            call mm%get_many(lsc, groups, valid=lvalid, threads=threads)
        else if (allocated(ltup)) then
            call mm%get_many(ltup, groups, valid=lvalid, threads=threads)
        else
            call mm%get_many(lkeys, groups, valid=lvalid, threads=threads)
        end if
        call mm%csr(goff, grows)
        allocate(lcount(ng))
        lcount = 0_int64
        do i = 1_int64, nl
            g = groups(i)
            if (g > 0_int64) lcount(g) = lcount(g) + 1_int64
        end do
        do g = 1_int64, ng
            biggest = max(biggest, lcount(g) * (goff(g + 1_int64) - goff(g)))
        end do
    end subroutine hash_biggest
    !
    !> Clause 8, `order="left"`: the left rows in their own order with each one's matches in
    !! right-row order, then, under `right`/`outer`, the unmatched right rows in right-table
    !! order -- `join_emit_left_order`'s sequence, from the CSR instead of the groups.
    !!
    !! One exclusive scan gives every left row its output position, so the fill writes disjoint
    !! ranges of `il`/`ir` and runs on a team; the scan itself is one pass over the probes.
    subroutine hash_emit(how_id, nl, nr, offsets, matches, unm, nt, il, ir)
        integer, intent(in) :: how_id            !! HOW_* token.
        integer(int64), intent(in) :: nl         !! left rows.
        integer(int64), intent(in) :: nr         !! right rows.
        integer(int64), intent(in) :: offsets(:) !! the probe ranges.
        integer(int64), intent(in) :: matches(:) !! the right rows per probe, ascending.
        logical, allocatable, intent(in) :: unm(:) !! per right row: no counterpart (right/outer only).
        integer, intent(in) :: nt                !! team for the fill; 1 means serial.
        integer(int64), intent(out) :: il(:)     !! per output row: left row, or 0.
        integer(int64), intent(out) :: ir(:)     !! per output row: right row, or 0.
        integer(int64), allocatable :: pos(:)
        integer(int64) :: i, o, t, len, r
        !
        allocate(pos(nl + 1_int64))
        pos(1) = 1_int64
        do i = 1_int64, nl
            len = offsets(i + 1_int64) - offsets(i)
            select case (how_id)
            case (HOW_LEFT, HOW_OUTER)
                pos(i + 1_int64) = pos(i) + max(len, 1_int64)
            case (HOW_SEMI)
                pos(i + 1_int64) = pos(i) + min(len, 1_int64)
            case (HOW_ANTI)
                pos(i + 1_int64) = pos(i) + 1_int64 - min(len, 1_int64)
            case default
                pos(i + 1_int64) = pos(i) + len
            end select
        end do
        !$omp parallel do num_threads(nt) if (nt > 1) schedule(static) private(i, o, t, len)
        do i = 1_int64, nl
            o = pos(i)
            if (pos(i + 1_int64) == o) cycle
            len = offsets(i + 1_int64) - offsets(i)
            if (len > 0_int64) then
                if (how_id == HOW_SEMI) then
                    il(o) = i
                    ir(o) = 0_int64
                else
                    do t = offsets(i), offsets(i + 1_int64) - 1_int64
                        il(o) = i
                        ir(o) = matches(t)
                        o = o + 1_int64
                    end do
                end if
            else
                ! LEFT, OUTER and ANTI: the unmatched left row itself, with no counterpart.
                il(o) = i
                ir(o) = 0_int64
            end if
        end do
        !$omp end parallel do
        if (how_id /= HOW_RIGHT .and. how_id /= HOW_OUTER) return
        o = pos(nl + 1_int64)
        do r = 1_int64, nr
            if (.not. unm(r)) cycle
            il(o) = 0_int64
            ir(o) = r
            o = o + 1_int64
        end do
    end subroutine hash_emit
    !
end submodule parquet_tables_join_hash ! GCOVR_EXCL_LINE
