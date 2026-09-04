!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The join's PAIR LIST: which left row meets which right row, over nothing but row indices.
!!
!! **NOT a generated file** -- `tools/generate_parquet_tables.py` emits this module's spec, so a
!! signature change here is a generator edit and a body change is not.
!!
!! **One engine, over the two key columns CONCATENATED.** For each join key this file builds one
!! `parquet_column` holding the left table's key rows followed by the right table's, hands all of
!! them to `pf_argsort` as a `pf_sort_keys`, and reads the matches off `group_offsets` -- the run
!! boundaries the sort reports in the same pass. A run holding rows from both halves is a match: a
!! member at position `p <= nl` is left row `p`, one at `p > nl` is right row `p - nl`.
!!
!! That is astropy's join algorithm, and it needs no new sorting code at all. Multi-column keys,
!! all nine orderable kinds, per-key null handling, the threading and the stable tie rule are
!! already what `pf_argsort` does -- so "equal" here means exactly what it means in `%sort_by`, in
!! a read-time `sort_by=`, and in `pf_match`. A join with a comparator of its own would be a
!! second null policy and a second chance to disagree about what equality is.
!!
!! **Everything that can produce a silent WRONG ANSWER is decided here**, before a single value
!! column is touched: which groups are null-bearing and so match nothing, how many rows the output
!! will have, whether the caller's cardinality assertion holds, and what order the rows come out
!! in. All of it is arithmetic on row indices, which is why it can be asserted directly rather
!! than inferred from a joined table.
!!
!! **Correctness does not rest on the sort being stable.** Each run is split into its left and
!! right members by a linear scan against `nl`, never by binary-searching for a split point that
!! only a stable sort guarantees exists. Stability decides only the ORDER within a group, which is
!! a documented output property with its own test.
submodule (parquet_tables) parquet_tables_join
    use parquet_sorting, only : pf_sort_keys, pf_argsort
    implicit none
    !
    !> `how=` tokens, in the order the error message lists them.
    integer, parameter :: HOW_INNER = 1, HOW_LEFT = 2, HOW_RIGHT = 3, HOW_OUTER = 4, &
        HOW_SEMI = 5, HOW_ANTI = 6
    !> `require=` tokens. REQ_MM asserts nothing and is the default.
    integer, parameter :: REQ_MM = 0, REQ_11 = 1, REQ_1M = 2, REQ_M1 = 3
    !> `order=` tokens.
    integer, parameter :: ORD_LEFT = 1, ORD_KEY = 2
    !
contains
    !
    module procedure parquet_debug_table_join_pairs
        call table_join_pairs(left, right, on, other_on=other_on, how=how, require=require, &
            order=order, max_rows=max_rows, il=il, ir=ir, n_out=n_out, matched=matched, &
            threads=threads)
    end procedure parquet_debug_table_join_pairs
    !
    module procedure table_join_pairs
        character(len=*), parameter :: PROC = "join"
        type(pf_sort_keys) :: skeys
        type(parquet_column), allocatable :: kc(:)
        integer(int64), allocatable :: perm(:), go(:)
        integer(int64), allocatable :: nleft(:), nright(:)
        logical, allocatable :: lmatched(:), rmatched(:)
        integer(int64), allocatable :: group_of_left(:)
        integer(int64), allocatable :: lg_off(:), lg_idx(:), rg_off(:), rg_idx(:)
        integer(int64) :: nl, nr, ngroups
        integer(int64) :: n_pairs, n_lunm, n_runm, biggest
        integer :: how_id, req_id, ord_id
        !
        call table_check_open(self, PROC)
        call table_check_open(other, PROC)
        ! Refused BEFORE anything else. A self-join argument-associates one table with an
        ! intent(inout) and an intent(in) dummy one level up, which F2018 15.5.2.13 forbids as
        ! soon as either is defined and which no compiler in this project's fleet diagnoses.
        if (associated(self%cache, other%cache)) then
            error stop EP // PROC // ": a table cannot be joined to itself -- take a %clone " // &
                "first and join that, so the two sides are genuinely separate tables"
        end if
        call join_resolve_tokens(how, require, order, how_id, req_id, ord_id)
        call join_build_keys(self, other, on, other_on, skeys, kc)
        nl = self%nrows()
        nr = other%nrows()
        call pf_argsort(skeys, perm, group_offsets=go, threads=threads)
        ngroups = size(go, kind=int64) - 1_int64
        call join_classify(kc, perm, go, nl, nr, ngroups, nleft, nright, lmatched, rmatched, &
            group_of_left, lg_off, lg_idx, rg_off, rg_idx)
        call join_check_require(req_id, nleft, nright, lmatched, rmatched, lg_off, lg_idx, &
            rg_off, rg_idx, ngroups)
        call join_count(how_id, nleft, nright, lmatched, rmatched, ngroups, nl, &
            n_pairs, n_lunm, n_runm, biggest, n_out)
        if (present(max_rows)) then
            if (n_out > max_rows) call join_refuse_size(n_out, max_rows, nl, nr, biggest)
        end if
        if (present(matched)) then
            allocate(matched(nl))
            if (nl > 0_int64) call join_fill_matched(group_of_left, lmatched, nl, matched)
        end if
        allocate(il(n_out), ir(n_out))
        if (n_out < 1_int64) return
        if (ord_id == ORD_KEY) then
            call join_emit_key_order(how_id, ngroups, lmatched, rmatched, lg_off, lg_idx, &
                rg_off, rg_idx, il, ir)
        else
            call join_emit_left_order(how_id, nl, group_of_left, lmatched, rmatched, &
                rg_off, rg_idx, ngroups, il, ir)
        end if
    end procedure table_join_pairs
    !
    !> Phase A: one concatenated `parquet_column` per join key, added to `skeys` in order.
    !!
    !! `kc` is returned rather than kept local because the group-nullness test in phase C reads
    !! it -- a `pf_sort_keys` does not hand its keys back, and re-deriving nullness from the two
    !! source columns would mean mapping every group member back across the concatenation.
    !!
    !! **Kinds are checked here rather than left to `%append`.** `%append` refuses a mismatch too,
    !! but its message names two columns where this one can name two columns, two kinds and two
    !! files -- and a future permissive `%append` would let the concatenation succeed with an
    !! int32 key silently compared against an int64 one. A 64-bit catalogue identifier above
    !! 2**53 is exactly what that loses, which is why this library refuses rather than promotes.
    subroutine join_build_keys(self, other, on, other_on, skeys, kc)
        class(parquet_table), intent(in) :: self  !! the left table.
        class(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: on(:)     !! left key columns, primary first.
        character(len=*), intent(in), optional :: other_on(:) !! right key columns.
        type(pf_sort_keys), intent(out) :: skeys  !! the assembled key list.
        type(parquet_column), allocatable, intent(out) :: kc(:) !! one concatenated key per entry.
        integer :: j, li, ri
        character(len=32) :: got, want
        character(len=:), allocatable :: rname
        !
        if (size(on) < 1) error stop EP // "join: no join key was given"
        if (present(other_on)) then
            if (size(other_on) /= size(on)) then
                write(got, "(I0)") size(other_on)
                write(want, "(I0)") size(on)
                error stop EP // "join: other_on= has " // trim(got) // " entries but on= has " // &
                    trim(want) // "; it takes one right-hand key name per left-hand one"
            end if
        end if
        allocate(kc(size(on, kind=int64)))
        do j = 1, size(on)
            if (present(other_on)) then
                rname = trim(other_on(j))
            else
                rname = trim(on(j))
            end if
            call table_lookup_sort_key(self, on(j), "join", li)
            call table_lookup_sort_key(other, rname, "join", ri)
            call join_check_key_kinds(self, other, on(j), rname, li, ri)
            call self%cache%cols(li)%values%deep_copy(kc(j))
            call kc(j)%append(other%cache%cols(ri)%values)
            call skeys%add(kc(j))
        end do
    end subroutine join_build_keys
    !
    !> Aborts unless two key columns hold the same kind and the same width.
    subroutine join_check_key_kinds(self, other, lname, rname, li, ri)
        class(parquet_table), intent(in) :: self  !! the left table.
        class(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: lname     !! left key column name.
        character(len=*), intent(in) :: rname     !! right key column name.
        integer, intent(in) :: li                 !! left key slot.
        integer, intent(in) :: ri                 !! right key slot.
        character(len=:), allocatable :: lk, rk, lsfx, rsfx
        character(len=32) :: lw, rw
        !
        if (self%cache%cols(li)%values%kindof() /= other%cache%cols(ri)%values%kindof()) then
            call parquet_kind_name(self%cache%cols(li)%values%kindof(), lk)
            call parquet_kind_name(other%cache%cols(ri)%values%kindof(), rk)
            call table_context_suffix(self%cache, trim(lname), lsfx)
            call table_context_suffix(other%cache, trim(rname), rsfx)
            error stop EP // "join: key '" // trim(lname) // "' is " // lk // " but '" // &
                trim(rname) // "' is " // rk // "; a join compares like with like and will not " // &
                "promote one to the other -- an identifier above 2**53 does not survive that -- " // &
                "so cast one side with %cast first" // lsfx // rsfx
        end if
        if (self%cache%cols(li)%values%colwidth() /= other%cache%cols(ri)%values%colwidth()) then
            write(lw, "(I0)") self%cache%cols(li)%values%colwidth()
            write(rw, "(I0)") other%cache%cols(ri)%values%colwidth()
            call table_context_suffix(self%cache, trim(lname), lsfx)
            error stop EP // "join: key '" // trim(lname) // "' is " // trim(lw) // " wide but '" // &
                trim(rname) // "' is " // trim(rw) // "; two key columns must have the same " // &
                "width" // lsfx
        end if
    end subroutine join_check_key_kinds
    !
    !> Phase C: per group, how many rows each side contributed and whether it can match at all.
    !!
    !! Also builds three index maps the emission needs: which group each left row belongs to, and
    !! per group the left rows and the right rows it holds. All three are CSR-shaped rather than
    !! re-scanned per output row, so the emission is O(n_out) rather than O(n_out * group size).
    !!
    !! **A group is null-bearing when ANY of its keys is null for it**, tested on the group's
    !! FIRST member alone. That is sufficient rather than a shortcut: a null compares equal only
    !! to another null, so within one group a given key is null for every member or for none.
    subroutine join_classify(kc, perm, go, nl, nr, ngroups, nleft, nright, lmatched, rmatched, &
            group_of_left, lg_off, lg_idx, rg_off, rg_idx)
        type(parquet_column), intent(in) :: kc(:)   !! the concatenated key columns.
        integer(int64), intent(in) :: perm(:)       !! the concatenation's permutation.
        integer(int64), intent(in) :: go(:)         !! group offsets into `perm`, length ngroups+1.
        integer(int64), intent(in) :: nl            !! left rows.
        integer(int64), intent(in) :: nr            !! right rows.
        integer(int64), intent(in) :: ngroups       !! number of groups.
        integer(int64), allocatable, intent(out) :: nleft(:)  !! left rows per group.
        integer(int64), allocatable, intent(out) :: nright(:) !! right rows per group.
        logical, allocatable, intent(out) :: lmatched(:) !! per group: its left rows found a match.
        logical, allocatable, intent(out) :: rmatched(:) !! per group: its right rows found a match.
        integer(int64), allocatable, intent(out) :: group_of_left(:) !! per left row: its group.
        integer(int64), allocatable, intent(out) :: lg_off(:) !! group -> slice of lg_idx.
        integer(int64), allocatable, intent(out) :: lg_idx(:) !! left rows, grouped, ascending.
        integer(int64), allocatable, intent(out) :: rg_off(:) !! group -> slice of rg_idx.
        integer(int64), allocatable, intent(out) :: rg_idx(:) !! right rows, grouped, ascending.
        integer(int64) :: g, t, p, cl, cr, lc, rc
        integer :: j
        logical :: isnull
        !
        allocate(nleft(max(ngroups, 1_int64)), nright(max(ngroups, 1_int64)))
        allocate(lmatched(max(ngroups, 1_int64)), rmatched(max(ngroups, 1_int64)))
        allocate(group_of_left(max(nl, 0_int64)))
        allocate(lg_off(ngroups + 1_int64), rg_off(ngroups + 1_int64))
        allocate(lg_idx(max(nl, 0_int64)), rg_idx(max(nr, 0_int64)))
        lg_off(1) = 1_int64
        rg_off(1) = 1_int64
        do g = 1_int64, ngroups
            cl = 0_int64
            cr = 0_int64
            do t = go(g), go(g + 1_int64) - 1_int64
                p = perm(t)
                if (p <= nl) then
                    cl = cl + 1_int64
                    group_of_left(p) = g
                else
                    cr = cr + 1_int64
                end if
            end do
            nleft(g) = cl
            nright(g) = cr
            isnull = .false.
            do j = 1, size(kc)
                if (parquet_column_is_null(kc(j), perm(go(g)))) then
                    isnull = .true.
                    exit
                end if
            end do
            ! A null-bearing group matches nothing in either direction: a null is UNKNOWN, and
            ! `unknown = unknown` is not true. Its left rows are unmatched left rows and its
            ! right rows are unmatched right rows.
            lmatched(g) = (.not. isnull) .and. cr > 0_int64
            rmatched(g) = (.not. isnull) .and. cl > 0_int64
            lg_off(g + 1_int64) = lg_off(g) + cl
            rg_off(g + 1_int64) = rg_off(g) + cr
        end do
        do g = 1_int64, ngroups
            lc = lg_off(g) - 1_int64
            rc = rg_off(g) - 1_int64
            do t = go(g), go(g + 1_int64) - 1_int64
                p = perm(t)
                if (p <= nl) then
                    lc = lc + 1_int64
                    lg_idx(lc) = p
                else
                    rc = rc + 1_int64
                    rg_idx(rc) = p - nl
                end if
            end do
        end do
    end subroutine join_classify
    !
    !> Phase C's cardinality assertion, and the reason a lookup-table join can be trusted.
    !!
    !! **NULL-BEARING GROUPS ARE EXCLUDED.** A null key matches nothing, so several null-keyed
    !! rows on one side are not duplicates of one key -- they are several rows whose key is
    !! unknown, and none of them can contribute an output row. Counting them would make
    !! `require="m:1"` abort on a lookup table with two blank identifiers, which is a false alarm
    !! on a join that would have produced exactly the right answer.
    !!
    !! The message names the two offending ROW indices rather than the key value: the engine holds
    !! indices, a key may be several columns, and a row index is what a caller can look up.
    subroutine join_check_require(req_id, nleft, nright, lmatched, rmatched, lg_off, lg_idx, &
            rg_off, rg_idx, ngroups)
        integer, intent(in) :: req_id            !! REQ_* token.
        integer(int64), intent(in) :: nleft(:)   !! left rows per group.
        integer(int64), intent(in) :: nright(:)  !! right rows per group.
        logical, intent(in) :: lmatched(:)       !! per group: its left rows found a match.
        logical, intent(in) :: rmatched(:)       !! per group: its right rows found a match.
        integer(int64), intent(in) :: lg_off(:)  !! group -> slice of lg_idx.
        integer(int64), intent(in) :: lg_idx(:)  !! left rows, grouped.
        integer(int64), intent(in) :: rg_off(:)  !! group -> slice of rg_idx.
        integer(int64), intent(in) :: rg_idx(:)  !! right rows, grouped.
        integer(int64), intent(in) :: ngroups    !! number of groups.
        integer(int64) :: g
        !
        if (req_id == REQ_MM) return
        do g = 1_int64, ngroups
            if (req_id == REQ_11 .or. req_id == REQ_1M) then
                ! A null-bearing group has lmatched .false. AND cannot violate anything, so the
                ! same test excludes both it and a group whose right side is empty. The latter
                ! genuinely has duplicate left keys, but with nothing to match they cannot
                ! multiply the output either -- and refusing them would make the assertion
                ! depend on the OTHER table's contents, which is not what it claims to check.
                if (lmatched(g) .and. nleft(g) > 1_int64) then
                    call join_refuse_require("1:m", "left", "on=", lg_idx(lg_off(g)), &
                        lg_idx(lg_off(g) + 1_int64), nleft(g))
                end if
            end if
            if (req_id == REQ_11 .or. req_id == REQ_M1) then
                if (rmatched(g) .and. nright(g) > 1_int64) then
                    call join_refuse_require("m:1", "right", "other_on=", rg_idx(rg_off(g)), &
                        rg_idx(rg_off(g) + 1_int64), nright(g))
                end if
            end if
        end do
    end subroutine join_check_require
    !
    !> The `require=` abort, factored out so the two sides produce the same shape of message.
    subroutine join_refuse_require(token, side, argname, row_a, row_b, n_dup)
        character(len=*), intent(in) :: token   !! the assertion that failed, as the caller wrote it.
        character(len=*), intent(in) :: side    !! "left" or "right".
        character(len=*), intent(in) :: argname !! the argument naming that side's keys.
        integer(int64), intent(in) :: row_a     !! first row sharing the key.
        integer(int64), intent(in) :: row_b     !! second row sharing it.
        integer(int64), intent(in) :: n_dup     !! how many rows share it in total.
        character(len=32) :: ta, tb, tn
        !
        write(ta, "(I0)") row_a
        write(tb, "(I0)") row_b
        write(tn, "(I0)") n_dup
        error stop EP // "join: require='" // token // "' asserts the " // side // " key is " // &
            "unique, but " // trim(tn) // " " // side // " rows share one key value -- rows " // &
            trim(ta) // " and " // trim(tb) // " among them. Read " // argname // "'s columns " // &
            "at those rows to see which value, or pass require='m:m' to allow it."
    end subroutine join_refuse_require
    !
    !> Phase C's counting pass: how many rows the output will have, before anything is allocated.
    !!
    !! `biggest` is the largest single group's pair count, which is what makes a refused join
    !! actionable: "the join wanted 4 100 000 000 rows" is not, without "one key value has 64 000
    !! matches on each side".
    subroutine join_count(how_id, nleft, nright, lmatched, rmatched, ngroups, nl, &
            n_pairs, n_lunm, n_runm, biggest, n_out)
        integer, intent(in) :: how_id            !! HOW_* token.
        integer(int64), intent(in) :: nleft(:)   !! left rows per group.
        integer(int64), intent(in) :: nright(:)  !! right rows per group.
        logical, intent(in) :: lmatched(:)       !! per group: its left rows found a match.
        logical, intent(in) :: rmatched(:)       !! per group: its right rows found a match.
        integer(int64), intent(in) :: ngroups    !! number of groups.
        integer(int64), intent(in) :: nl         !! left rows.
        integer(int64), intent(out) :: n_pairs   !! matched left-right pairs.
        integer(int64), intent(out) :: n_lunm    !! left rows with no counterpart.
        integer(int64), intent(out) :: n_runm    !! right rows with no counterpart.
        integer(int64), intent(out) :: biggest   !! the largest group's pair count.
        integer(int64), intent(out) :: n_out     !! rows this `how` will emit.
        integer(int64) :: g, prod
        character(len=32) :: tl, tr
        !
        n_pairs = 0_int64
        n_lunm = 0_int64
        n_runm = 0_int64
        biggest = 0_int64
        do g = 1_int64, ngroups
            if (lmatched(g)) then
                ! Guarded rather than trusted: a wrapped product would make n_out negative, the
                ! allocation fail somewhere unrelated, and the cause invisible. The division is
                ! one per group and only on the groups that actually contribute pairs.
                if (nleft(g) > huge(0_int64) / nright(g)) then
                    write(tl, "(I0)") nleft(g)
                    write(tr, "(I0)") nright(g)
                    error stop EP // "join: one key value has " // trim(tl) // " rows on the " // &
                        "left and " // trim(tr) // " on the right; their product overflows a " // &
                        "64-bit row count. Deduplicate a side, or add require='m:1'."
                end if
                prod = nleft(g) * nright(g)
                n_pairs = n_pairs + prod
                if (prod > biggest) biggest = prod
            else
                n_lunm = n_lunm + nleft(g)
            end if
            if (.not. rmatched(g)) n_runm = n_runm + nright(g)
        end do
        select case (how_id)
        case (HOW_LEFT)
            n_out = n_pairs + n_lunm
        case (HOW_RIGHT)
            n_out = n_pairs + n_runm
        case (HOW_OUTER)
            n_out = n_pairs + n_lunm + n_runm
        case (HOW_SEMI)
            n_out = nl - n_lunm
        case (HOW_ANTI)
            n_out = n_lunm
        case default
            n_out = n_pairs
        end select
    end subroutine join_count
    !
    !> The `max_rows=` abort. Split out so the counting pass reads as counting.
    subroutine join_refuse_size(n_out, max_rows, nl, nr, biggest)
        integer(int64), intent(in) :: n_out    !! rows the join would emit.
        integer(int64), intent(in) :: max_rows !! the caller's ceiling.
        integer(int64), intent(in) :: nl       !! left rows.
        integer(int64), intent(in) :: nr       !! right rows.
        integer(int64), intent(in) :: biggest  !! the largest group's pair count.
        character(len=32) :: tn, tm, tl, tr, tb
        !
        write(tn, "(I0)") n_out
        write(tm, "(I0)") max_rows
        write(tl, "(I0)") nl
        write(tr, "(I0)") nr
        write(tb, "(I0)") biggest
        error stop EP // "join: the result would have " // trim(tn) // " rows, over the " // &
            "max_rows=" // trim(tm) // " ceiling (" // trim(tl) // " left rows, " // trim(tr) // &
            " right rows, and one key value alone contributing " // trim(tb) // " rows). Add " // &
            "require='m:1' if the right key was meant to be unique."
    end subroutine join_refuse_size
    !
    !> Fills `matched` over the PRE-join left rows -- the diagnostic a cross-match script prints.
    subroutine join_fill_matched(group_of_left, lmatched, nl, matched)
        integer(int64), intent(in) :: group_of_left(:) !! per left row: its group.
        logical, intent(in) :: lmatched(:)             !! per group: its left rows found a match.
        integer(int64), intent(in) :: nl               !! left rows.
        logical, intent(out) :: matched(:)             !! per left row: it found a counterpart.
        integer(int64) :: i
        !
        do i = 1_int64, nl
            matched(i) = lmatched(group_of_left(i))
        end do
    end subroutine join_fill_matched
    !
    !> Phase D, `order="left"`: the left table's rows in their own order, matches within each.
    !!
    !! This is STILTS' and pandas' ordering, and it is what makes an array a caller computed
    !! against the pre-join left table still line up row for row when the join is one-to-at-most-
    !! one. Unmatched RIGHT rows have no place in a left-major walk, so they follow at the end in
    !! right-table order.
    subroutine join_emit_left_order(how_id, nl, group_of_left, lmatched, rmatched, &
            rg_off, rg_idx, ngroups, il, ir)
        integer, intent(in) :: how_id                  !! HOW_* token.
        integer(int64), intent(in) :: nl               !! left rows.
        integer(int64), intent(in) :: group_of_left(:) !! per left row: its group.
        logical, intent(in) :: lmatched(:)             !! per group: its left rows found a match.
        logical, intent(in) :: rmatched(:)             !! per group: its right rows found a match.
        integer(int64), intent(in) :: rg_off(:)        !! group -> slice of rg_idx.
        integer(int64), intent(in) :: rg_idx(:)        !! right rows, grouped, ascending.
        integer(int64), intent(in) :: ngroups          !! number of groups.
        integer(int64), intent(out) :: il(:)           !! per output row: left row, or 0.
        integer(int64), intent(out) :: ir(:)           !! per output row: right row, or 0.
        integer(int64) :: i, g, t, o
        !
        o = 0_int64
        do i = 1_int64, nl
            g = group_of_left(i)
            if (lmatched(g)) then
                if (how_id == HOW_ANTI) cycle
                if (how_id == HOW_SEMI) then
                    o = o + 1_int64
                    il(o) = i
                    ir(o) = 0_int64
                    cycle
                end if
                do t = rg_off(g), rg_off(g + 1_int64) - 1_int64
                    o = o + 1_int64
                    il(o) = i
                    ir(o) = rg_idx(t)
                end do
            else
                if (how_id == HOW_INNER .or. how_id == HOW_RIGHT .or. how_id == HOW_SEMI) cycle
                o = o + 1_int64
                il(o) = i
                ir(o) = 0_int64
            end if
        end do
        if (how_id /= HOW_RIGHT .and. how_id /= HOW_OUTER) return
        ! The unmatched right rows, in right-table order. Walked group by group and then sorted
        ! back into row order by construction: rg_idx is ascending WITHIN a group but not across
        ! groups, so a plain walk would emit them in key order instead. A counting sweep over the
        ! right rows is the cheapest way to keep the promise the argument name makes.
        call join_emit_unmatched_right(rmatched, rg_off, rg_idx, ngroups, o, il, ir)
    end subroutine join_emit_left_order
    !
    !> Appends every right row with no counterpart, in RIGHT-TABLE order.
    subroutine join_emit_unmatched_right(rmatched, rg_off, rg_idx, ngroups, o, il, ir)
        logical, intent(in) :: rmatched(:)       !! per group: its right rows found a match.
        integer(int64), intent(in) :: rg_off(:)  !! group -> slice of rg_idx.
        integer(int64), intent(in) :: rg_idx(:)  !! right rows, grouped.
        integer(int64), intent(in) :: ngroups    !! number of groups.
        integer(int64), intent(inout) :: o       !! output cursor; advanced by what is emitted.
        integer(int64), intent(out) :: il(:)     !! per output row: left row, or 0.
        integer(int64), intent(out) :: ir(:)     !! per output row: right row, or 0.
        logical, allocatable :: unm(:)
        integer(int64) :: g, t, r
        !
        allocate(unm(max(size(rg_idx, kind=int64), 1_int64)))
        unm = .false.
        do g = 1_int64, ngroups
            if (rmatched(g)) cycle
            do t = rg_off(g), rg_off(g + 1_int64) - 1_int64
                unm(rg_idx(t)) = .true.
            end do
        end do
        do r = 1_int64, size(rg_idx, kind=int64)
            if (.not. unm(r)) cycle
            o = o + 1_int64
            il(o) = 0_int64
            ir(o) = r
        end do
    end subroutine join_emit_unmatched_right
    !
    !> Phase D, `order="key"`: the sort's own order, which is astropy's.
    !!
    !! Cheaper than `order="left"` by one sweep -- this is the engine's natural output -- and
    !! every unmatched row appears in its own group's position rather than being relegated to the
    !! end, which is the visible difference between the two orderings.
    subroutine join_emit_key_order(how_id, ngroups, lmatched, rmatched, lg_off, lg_idx, &
            rg_off, rg_idx, il, ir)
        integer, intent(in) :: how_id            !! HOW_* token.
        integer(int64), intent(in) :: ngroups    !! number of groups.
        logical, intent(in) :: lmatched(:)       !! per group: its left rows found a match.
        logical, intent(in) :: rmatched(:)       !! per group: its right rows found a match.
        integer(int64), intent(in) :: lg_off(:)  !! group -> slice of lg_idx.
        integer(int64), intent(in) :: lg_idx(:)  !! left rows, grouped, ascending.
        integer(int64), intent(in) :: rg_off(:)  !! group -> slice of rg_idx.
        integer(int64), intent(in) :: rg_idx(:)  !! right rows, grouped, ascending.
        integer(int64), intent(out) :: il(:)     !! per output row: left row, or 0.
        integer(int64), intent(out) :: ir(:)     !! per output row: right row, or 0.
        integer(int64) :: g, s, t, o
        !
        o = 0_int64
        do g = 1_int64, ngroups
            do s = lg_off(g), lg_off(g + 1_int64) - 1_int64
                if (lmatched(g)) then
                    if (how_id == HOW_ANTI) cycle
                    if (how_id == HOW_SEMI) then
                        o = o + 1_int64
                        il(o) = lg_idx(s)
                        ir(o) = 0_int64
                        cycle
                    end if
                    do t = rg_off(g), rg_off(g + 1_int64) - 1_int64
                        o = o + 1_int64
                        il(o) = lg_idx(s)
                        ir(o) = rg_idx(t)
                    end do
                else
                    if (how_id == HOW_INNER .or. how_id == HOW_RIGHT .or. how_id == HOW_SEMI) cycle
                    o = o + 1_int64
                    il(o) = lg_idx(s)
                    ir(o) = 0_int64
                end if
            end do
            if (rmatched(g)) cycle
            if (how_id /= HOW_RIGHT .and. how_id /= HOW_OUTER) cycle
            do t = rg_off(g), rg_off(g + 1_int64) - 1_int64
                o = o + 1_int64
                il(o) = 0_int64
                ir(o) = rg_idx(t)
            end do
        end do
    end subroutine join_emit_key_order
    !
    !> Turns the three vocabulary arguments into their tokens, naming every accepted value.
    subroutine join_resolve_tokens(how, require, order, how_id, req_id, ord_id)
        character(len=*), intent(in), optional :: how     !! join kind.
        character(len=*), intent(in), optional :: require !! cardinality assertion.
        character(len=*), intent(in), optional :: order   !! output ordering.
        integer, intent(out) :: how_id                    !! HOW_* token.
        integer, intent(out) :: req_id                    !! REQ_* token.
        integer, intent(out) :: ord_id                    !! ORD_* token.
        character(len=:), allocatable :: tok
        !
        how_id = HOW_INNER
        if (present(how)) then
            call join_fold(how, tok)
            select case (tok)
            case ("inner")
                how_id = HOW_INNER
            case ("left")
                how_id = HOW_LEFT
            case ("right")
                how_id = HOW_RIGHT
            case ("outer")
                how_id = HOW_OUTER
            case ("semi")
                how_id = HOW_SEMI
            case ("anti")
                how_id = HOW_ANTI
            case default
                error stop EP // "join: how='" // tok // "' is not one of 'inner', 'left', " // &
                    "'right', 'outer', 'semi' or 'anti'"
            end select
        end if
        req_id = REQ_MM
        if (present(require)) then
            call join_fold(require, tok)
            select case (tok)
            case ("m:m")
                req_id = REQ_MM
            case ("1:1")
                req_id = REQ_11
            case ("1:m")
                req_id = REQ_1M
            case ("m:1")
                req_id = REQ_M1
            case default
                error stop EP // "join: require='" // tok // "' is not one of 'm:m', '1:1', " // &
                    "'1:m' or 'm:1' (read left-side-first, so 'm:1' asserts the RIGHT key is unique)"
            end select
        end if
        ord_id = ORD_LEFT
        if (present(order)) then
            call join_fold(order, tok)
            select case (tok)
            case ("left")
                ord_id = ORD_LEFT
            case ("key")
                ord_id = ORD_KEY
            case default
                error stop EP // "join: order='" // tok // "' is not one of 'left' or 'key'"
            end select
        end if
    end subroutine join_resolve_tokens
    !
    !> Trims and lower-cases one vocabulary token, the one case-folding site this file has.
    subroutine join_fold(text, tok)
        character(len=*), intent(in) :: text              !! the raw token.
        character(len=:), allocatable, intent(out) :: tok !! trimmed and lower-cased.
        integer :: i
        !
        tok = trim(adjustl(text))
        do i = 1, len(tok)
            if (tok(i:i) >= "A" .and. tok(i:i) <= "Z") tok(i:i) = achar(iachar(tok(i:i)) + 32)
        end do
    end subroutine join_fold
    !
end submodule parquet_tables_join ! GCOVR_EXCL_LINE
