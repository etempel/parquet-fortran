!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Introspection and name resolution for `parquet_table`: the row/column counts, the per-column
!! queries (`%kind`, `%width`, `%unit`, `%residency`, `%is_null`), and the shared lookup and
!! guard helpers every value accessor is built on.
!!
!! Two conventions run through the whole file:
!!
!! * **The lookup key is always the column's INTERNAL name**, never the physical name in the
!!   file. They are the same today; keeping every lookup in `table_find` is what will let a
!!   read-time remap change that in one place.
!! * **`found=` decides whether a miss is fatal.** Absent, a missing column is a hard
!!   `error stop`; present, the miss is reported through it and the call returns quietly.
submodule (parquet_tables) parquet_tables_query
    use, intrinsic :: iso_fortran_env, only : output_unit
    implicit none
    !
    !> One column's statistics as text, gathered by `table_print_stat` before it prints anything:
    !! the scan that fills these runs across columns on a team, and the printing that reads them
    !! stays serial and in column order.
    type :: stat_cell
        character(len=:), allocatable :: nulls !! null count as text.
        character(len=:), allocatable :: mn    !! smallest value as text, or "-".
        character(len=:), allocatable :: mx    !! largest value as text, or "-".
    end type stat_cell
    !
contains
    !
    module procedure table_scope_of
        sc%regime = self%regime
        sc%row_lo = self%row_lo
        sc%row_hi = self%row_hi
        sc%nrows = self%row_count
        sc%detached = self%detached
    end procedure table_scope_of
    !
    module procedure table_nrows
        call table_check_open(self, "nrows")
        n = self%row_count
    end procedure table_nrows
    !
    module procedure table_nrows_unfiltered
        call table_check_open(self, "nrows_unfiltered")
        n = self%cache%unfiltered_rows
    end procedure table_nrows_unfiltered
    !
    module procedure table_row_group_extent
        call table_check_open(self, "row_group_extent")
        n = self%cache%rg_extent_rows
    end procedure table_row_group_extent
    !
    module procedure table_ncols
        integer :: i
        !
        call table_check_open(self, "ncols")
        n = self%cache%ncols
        if (.not. present(resident_only)) return
        if (.not. resident_only) return
        n = 0
        do i = 1, self%cache%ncols
            if (self%cache%cols(i)%residency == RES_FULL) n = n + 1
        end do
    end procedure table_ncols
    !
    module procedure table_column_names
        integer :: i, maxlen, n
        logical :: only_res
        !
        call table_check_open(self, "column_names")
        only_res = .false.
        if (present(resident_only)) only_res = resident_only
        ! A fixed-length array cannot hold ragged names, so the width is the longest name
        ! present; len=1 keeps a zero-column table's result well-formed rather than len=0. Sized
        ! over the columns actually being reported, so a filtered list is not padded to the width
        ! of a name it leaves out.
        maxlen = 1
        n = 0
        do i = 1, self%cache%ncols
            if (only_res .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            n = n + 1
            if (len(self%cache%cols(i)%name) > maxlen) maxlen = len(self%cache%cols(i)%name)
        end do
        allocate(character(len=maxlen) :: names(n))
        n = 0
        do i = 1, self%cache%ncols
            if (only_res .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            n = n + 1
            names(n) = self%cache%cols(i)%name
        end do
    end procedure table_column_names
    !
    module procedure table_column_index
        call table_lookup_or_fail(self, name, "column_index", j, found)
        ! `table_lookup_or_fail` already leaves 0 on a reported miss and aborts otherwise, so the
        ! slot index IS the answer -- the two concepts coincide, and saying so here is what keeps
        ! %column_index and every index-form query agreeing on what a position means.
    end procedure table_column_index
    !
    module procedure table_column_name
        integer :: idx
        !
        nm = ""
        call table_slot_or_fail(self, j, "column_name", idx, found)
        if (idx == 0) return
        nm = self%cache%cols(idx)%name
    end procedure table_column_name
    !
    !> Validates a 1-based column position, the way `table_lookup_or_fail` validates a name.
    !!
    !! Deliberately reports the table's width in the message: an out-of-range position is almost
    !! always a loop bound that has gone stale against `%ncols()` after a `%drop_column` or an
    !! `%add_column`, and the two numbers side by side say that immediately.
    module procedure table_slot_or_fail
        character(len=:), allocatable :: sfx
        character(len=32) :: got, have
        !
        call table_check_open(self, proc)
        idx = 0
        if (j >= 1 .and. j <= self%cache%ncols) then
            idx = j
            if (present(found)) found = .true.
            return
        end if
        if (present(found)) then
            found = .false.
            return
        end if
        write(got, "(I0)") j
        write(have, "(I0)") self%cache%ncols
        call table_context_suffix(self%cache, "", sfx)
        error stop EP // trim(proc) // ": column position " // trim(got) // " is outside this " // &
            "table's 1.." // trim(have) // " columns" // sfx
    end procedure table_slot_or_fail
    !
    module procedure table_has_column
        call table_check_open(self, "has_column")
        found = table_find(self, name) > 0
        ! The automatic row-index column exists whether or not it has been asked for yet, so this
        ! answers .true. for it while it is still virtual. That is the ONE place %has_column and
        ! %column_names disagree, and deliberately: %has_column asks "can I use this name?", which
        ! is yes, while %column_names lists what the table is holding, which does not include a
        ! column nobody has asked for.
        !
        ! `row_index_live` is what separates "not asked for yet" from "asked for and then dropped".
        ! Materializing it is ONE-SHOT -- table_make_row_index returns immediately once the flag is
        ! set -- so after %drop_column the name cannot be used again, exactly as for any other
        ! dropped column, and answering .true. here would promise something %get refuses.
        if (.not. found .and. name == PARQUET_ROW_INDEX) then
            found = self%cache%file_backed .and. .not. self%cache%row_index_live
        end if
    end procedure table_has_column
    !
    module procedure missing_columns_array
        integer :: i, n, maxlen
        !
        call table_check_open(self, "missing_columns")
        ! Two passes, exactly as %column_names does it: the first sizes the result to the longest
        ! name actually being reported, so a packed array cannot truncate one. len=1 rather than
        ! len=0 keeps an empty result well-formed.
        n = 0
        maxlen = 1
        do i = 1, size(names)
            if (self%has_column(trim(names(i)))) cycle
            n = n + 1
            maxlen = max(maxlen, len_trim(names(i)))
        end do
        allocate(character(len=maxlen) :: absent(n))
        n = 0
        do i = 1, size(names)
            if (self%has_column(trim(names(i)))) cycle
            n = n + 1
            absent(n) = trim(names(i))
        end do
    end procedure missing_columns_array
    !
    module procedure missing_columns_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%missing_columns(toks, absent)
    end procedure missing_columns_string
    !
    module procedure require_columns_array
        character(len=:), allocatable :: absent(:), want_txt, absent_txt, sfx
        !
        call table_check_open(self, "require_columns")
        call self%missing_columns(names, absent)
        if (size(absent) == 0) return
        ! Both lists are previews, not the whole thing: this message interpolates text the CALLER
        ! supplied, and ifx's ERROR STOP runtime corrupts the heap once the composed message
        ! reaches 8192 bytes -- which a "these columns are missing" message is guaranteed to
        ! approach on exactly the input that triggers it.
        call preview_name_list(absent, absent_txt)
        call preview_name_list(names, want_txt)
        call table_context_suffix(self%cache, "", sfx)
        error stop EP // "require_columns: this table does not have " // absent_txt // &
            " (asked for " // want_txt // ")" // sfx
    end procedure require_columns_array
    !
    module procedure require_columns_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(names, toks)
        call self%require_columns(toks)
    end procedure require_columns_string
    !
    !> Joins a name list into one short, bounded string for an `error stop` message.
    !!
    !! Bounded in both directions -- at most `PREVIEW_MAX` names, and each name itself clipped --
    !! because the list comes from the caller and an unbounded `error stop` message is a heap
    !! corruption on ifx, not merely an unreadable one. See CLAUDE.md, "Never interpolate
    !! unbounded caller-supplied text into an `error stop` message".
    subroutine preview_name_list(names, text)
        character(len=*), intent(in) :: names(:)              !! the names to join.
        character(len=:), allocatable, intent(out) :: text    !! quoted, comma-separated preview.
        integer, parameter :: PREVIEW_MAX = 10                !! names shown before "and N more".
        integer, parameter :: NAME_MAX = 64                   !! characters shown of one name.
        integer :: i, shown
        character(len=32) :: rest
        !
        shown = min(size(names), PREVIEW_MAX)
        text = ""
        do i = 1, shown
            if (i > 1) text = text // ", "
            if (len_trim(names(i)) > NAME_MAX) then
                text = text // "'" // names(i)(1:NAME_MAX) // "...'"
            else
                text = text // "'" // trim(names(i)) // "'"
            end if
        end do
        if (size(names) > shown) then
            write(rest, "(I0)") size(names) - shown
            text = text // " and " // trim(rest) // " more"
        end if
        ! Only reachable for an empty list, which `require_columns_array` never passes: both of its
        ! calls sit past the `size(absent) == 0` early return, and a request naming no columns has
        ! nothing missing either, so each carries at least one name. Kept as a fallback for a future
        ! caller rather than deleted -- an empty preview would otherwise render the message as
        ! "this table does not have  (asked for )".
        !
        ! Written as a block rather than a one-line `if` so that gcov can attribute the two halves
        ! separately: the condition is evaluated on every call and rightly counts as covered, while
        ! only the body needs excluding. Sharing one line gives them one hit count, which reports a
        ! live condition as a stale exclusion.
        if (len(text) == 0) then
            text = "(none)"  ! GCOVR_EXCL_LINE
        end if
    end subroutine preview_name_list
    !
    module procedure table_find
        idx = 0
        ! Defensive only: every call site (table_has_column, table_resolve, table_lookup_or_fail,
        ! table_new_slot's own callers) runs table_check_open first, which already aborts on an
        ! unassociated cache -- so this branch guards an invariant nothing reachable can violate.
        if (.not. associated(self%cache)) return ! GCOVR_EXCL_LINE
        idx = cache_find(self%cache, name)
    end procedure table_find
    !
    !> The first 7 bytes of `s`, packed big-endian into an integer and blank-padded to 7 -- the
    !! bisection key `cache%name_key` holds. Blank padding (32) matches how Fortran itself pads the
    !! shorter operand when comparing two names, so ordering by this key agrees with ordering by
    !! the names for every pair whose first 7 bytes differ. `iachar`/`llt` are used throughout
    !! rather than `<`, so the key and the string comparisons share one collating sequence.
    pure function name_sort_key(s) result(k)
        character(len=*), intent(in) :: s !! the (already trimmed) column name.
        integer(int64) :: k               !! packed prefix key, always positive.
        integer :: i, n
        !
        k = 0_int64
        n = min(len(s), 7)
        do i = 1, n
            k = k*256_int64 + int(iachar(s(i:i)), int64)
        end do
        do i = n + 1, 7
            k = k*256_int64 + 32_int64
        end do
    end function name_sort_key
    !
    !> `.true.` when column `a`'s name sorts before name `b` under the (key, name) order the
    !! index is built and searched with. Taking the key first is what lets the search compare
    !! integers; the name breaks a 7-byte-prefix tie.
    pure function name_before(ka, na, kb, nb) result(res)
        integer(int64), intent(in) :: ka  !! first name's prefix key.
        character(len=*), intent(in) :: na !! first name.
        integer(int64), intent(in) :: kb  !! second name's prefix key.
        character(len=*), intent(in) :: nb !! second name.
        logical :: res                    !! .true. if (ka, na) sorts before (kb, nb).
        !
        if (ka /= kb) then
            res = ka < kb
        else
            res = llt(na, nb)
        end if
    end function name_before
    !
    module procedure cache_find
        integer :: lo, hi, mid, j, nq
        integer(int64) :: qkey
        !
        idx = 0
        if (cache%ncols <= 0) return
        ! Hoisted: `trim(name)` is loop-invariant, and leaving it inside the comparison relies on
        ! the optimiser noticing (gfortran does at -O3; a -O0 build does not).
        nq = len_trim(name)
        ! Bisect the name-ordered index when there is one. This deliberately does NOT trust it:
        ! the name at the slot the search lands on is compared before the index is returned, and a
        ! search that finds nothing falls through to the linear scan below. So an index left stale
        ! by a column-set mutation that forgot to maintain it costs a scan and never returns the
        ! wrong column -- see parquet_table_cache%name_order for why that safety net is the design
        ! rather than an afterthought.
        if (allocated(cache%name_order) .and. allocated(cache%name_key)) then
            if (size(cache%name_order) >= cache%ncols .and. size(cache%name_key) >= cache%ncols) then
                qkey = name_sort_key(name(1:nq))
                lo = 1
                hi = cache%ncols
                do while (lo <= hi)
                    mid = (lo + hi)/2
                    ! Integer probe first: on a table whose names differ within 7 bytes -- almost
                    ! all of them -- this resolves every step but the last without touching a
                    ! name at all.
                    if (cache%name_key(mid) < qkey) then
                        lo = mid + 1
                    else if (cache%name_key(mid) > qkey) then
                        hi = mid - 1
                    else
                        j = cache%name_order(mid)
                        if (j < 1 .or. j > cache%ncols) exit
                        if (cache%cols(j)%name == name(1:nq)) then
                            idx = j
                            return
                        else if (llt(cache%cols(j)%name, name(1:nq))) then
                            lo = mid + 1
                        else
                            hi = mid - 1
                        end if
                    end if
                end do
            end if
        end if
        do j = 1, cache%ncols
            if (cache%cols(j)%name == name(1:nq)) then
                idx = j
                return
            end if
        end do
    end procedure cache_find
    !
    module procedure parquet_debug_table_drop_name_index
        ! Written through the cache POINTER, so `table` is intent(in) for the same reason
        ! parquet_debug_table_set_inflight's is: the index is not part of the table's own value.
        had_index = .false.
        if (.not. associated(table%cache)) return
        had_index = allocated(table%cache%name_order) .or. allocated(table%cache%name_key)
        if (allocated(table%cache%name_order)) deallocate(table%cache%name_order)
        if (allocated(table%cache%name_key)) deallocate(table%cache%name_key)
    end procedure parquet_debug_table_drop_name_index
    !
    module procedure cache_name_index_rebuild
        integer :: n, i, k, lo, hi, mid
        integer(int64) :: key
        !
        n = cache%ncols
        if (allocated(cache%name_order)) then
            if (size(cache%name_order) < n) deallocate(cache%name_order)
        end if
        if (allocated(cache%name_key)) then
            if (size(cache%name_key) < n) deallocate(cache%name_key)
        end if
        if (.not. allocated(cache%name_order)) allocate(cache%name_order(max(n, 1)))
        if (.not. allocated(cache%name_key)) allocate(cache%name_key(max(n, 1)))
        if (n <= 0) return
        ! Binary insertion sort: O(n log n) key/name comparisons -- the expensive part -- against
        ! O(n^2) moves of two integers, which for a column count never matter. Written out here
        ! rather than reached for from parquet_sorting, so the table layer does not gain a
        ! dependency on the sorting module for one internal index.
        cache%name_order(1) = 1
        cache%name_key(1) = name_sort_key(cache%cols(1)%name)
        do i = 2, n
            key = name_sort_key(cache%cols(i)%name)
            lo = 1
            hi = i - 1
            do while (lo <= hi)
                mid = (lo + hi)/2
                if (name_before(cache%name_key(mid), cache%cols(cache%name_order(mid))%name, &
                                key, cache%cols(i)%name)) then
                    lo = mid + 1
                else
                    hi = mid - 1
                end if
            end do
            do k = i - 1, lo, -1
                cache%name_order(k + 1) = cache%name_order(k)
                cache%name_key(k + 1) = cache%name_key(k)
            end do
            cache%name_order(lo) = i
            cache%name_key(lo) = key
        end do
    end procedure cache_name_index_rebuild
    !
    module procedure cache_name_index_reserve
        integer :: n
        integer, allocatable :: bigger(:)
        integer(int64), allocatable :: bigkey(:)
        !
        if (.not. allocated(cache%name_order) .or. .not. allocated(cache%name_key)) then
            call cache_name_index_rebuild(cache)
        end if
        if (size(cache%name_order) >= cap .and. size(cache%name_key) >= cap) return
        ! Only the entries actually indexed are carried over; the rest is spare room, which is the
        ! whole point of the call.
        n = min(cache%ncols, size(cache%name_order), size(cache%name_key))
        allocate(bigger(cap), bigkey(cap))
        if (n > 0) then
            bigger(1:n) = cache%name_order(1:n)
            bigkey(1:n) = cache%name_key(1:n)
        end if
        call move_alloc(bigger, cache%name_order)
        call move_alloc(bigkey, cache%name_key)
    end procedure cache_name_index_reserve
    !
    module procedure table_column_capacity
        logical :: want_free
        !
        call table_check_open(self, "column_capacity")
        want_free = .false.
        if (present(free)) want_free = free
        n = 0
        if (allocated(self%cache%cols)) n = size(self%cache%cols)
        ! The spare count is what a caller sizing a reservation actually wants; the total is what
        ! %reserve_columns takes, since that argument is a TOTAL rather than an increment.
        if (want_free) n = n - self%cache%ncols
    end procedure table_column_capacity
    !
    module procedure cache_name_index_insert
        integer :: n, lo, hi, mid, k, newcap
        integer(int64) :: key
        integer, allocatable :: bigger(:)
        integer(int64), allocatable :: bigkey(:)
        !
        n = cache%ncols
        ! Only the "appended the last slot, and the index already describes the ones before it"
        ! case can be handled incrementally. Anything else means the caller is out of step with
        ! the index, and a full rebuild is both correct and cheap at that point.
        ! Defensive: both callers (table_new_slot and add_file_slot) pass the slot they have just
        ! appended, i.e. cache%ncols, so neither condition can hold. Kept because the incremental
        ! path is only correct under that precondition, and a future caller that inserts elsewhere
        ! must land here rather than corrupt the index.
        ! gcov attribution artifact: the condition is evaluated on every call, so the `if` line
        ! registers hits while its body reliably shows zero.
        if (slot /= n .or. n < 1) then  ! GCOVR_EXCL_START
            call cache_name_index_rebuild(cache)
            return
        end if                          ! GCOVR_EXCL_STOP
        if (.not. allocated(cache%name_order) .or. .not. allocated(cache%name_key)) then
            call cache_name_index_rebuild(cache)
            return
        end if
        if (size(cache%name_order) < n .or. size(cache%name_key) < n) then
            ! Grow geometrically, mirroring how cols(:) itself grows in table_new_slot -- growing
            ! by one would make a long %add_column loop quadratic in allocations, and would send
            ! every append down the rebuild path above.
            newcap = max(8, 2*size(cache%name_order), n)
            allocate(bigger(newcap), bigkey(newcap))
            bigger(1:n - 1) = cache%name_order(1:n - 1)
            bigkey(1:n - 1) = cache%name_key(1:n - 1)
            call move_alloc(bigger, cache%name_order)
            call move_alloc(bigkey, cache%name_key)
        end if
        key = name_sort_key(cache%cols(slot)%name)
        lo = 1
        hi = n - 1
        do while (lo <= hi)
            mid = (lo + hi)/2
            if (name_before(cache%name_key(mid), cache%cols(cache%name_order(mid))%name, &
                            key, cache%cols(slot)%name)) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
        do k = n - 1, lo, -1
            cache%name_order(k + 1) = cache%name_order(k)
            cache%name_key(k + 1) = cache%name_key(k)
        end do
        cache%name_order(lo) = slot
        cache%name_key(lo) = key
    end procedure cache_name_index_insert
    !
    !> A resolved slot's kind. Shared by `%kind`'s name and position forms so the two can never
    !! drift; the same pattern is used for every query that has both.
    !!
    !! Answers from the DESCRIPTOR, not from the value store, because a column that has not been
    !! touched yet holds no values -- and %kind is precisely how a caller decides which %col
    !! specific to call, so it has to work before the first read, not after it.
    function slot_kind(self, idx) result(k)
        class(parquet_table), intent(in) :: self !! the table.
        integer, intent(in) :: idx               !! a validated slot index.
        integer :: k                             !! the PK_* constant.
        !
        ! For a plain LIST/LARGE_LIST column the kind is not known until the width is, and a
        ! metadata query must not answer with a guess -- so this resolves it for real (proven),
        ! which does read data for that one column type. Every other column was classified from
        ! the schema at open and this is a no-op. See table_resolve_width.
        call table_resolve_width(self%cache, table_scope_of(self), idx, .true., "kind")
        k = self%cache%cols(idx)%declared_kind
    end function slot_kind
    !
    module procedure table_column_kind
        integer :: idx
        !
        k = PK_NONE
        call table_lookup_or_fail(self, name, "kind", idx, found)
        if (idx == 0) return
        k = slot_kind(self, idx)
    end procedure table_column_kind
    !
    module procedure table_column_kind_at
        integer :: idx
        !
        k = PK_NONE
        call table_slot_or_fail(self, j, "kind", idx, found)
        if (idx == 0) return
        k = slot_kind(self, idx)
    end procedure table_column_kind_at
    !
    !> A resolved slot's values-per-row. Proven, not guessed, exactly as `slot_kind` explains.
    function slot_width(self, idx) result(wdt)
        class(parquet_table), intent(in) :: self !! the table.
        integer, intent(in) :: idx               !! a validated slot index.
        integer :: wdt                           !! values per row.
        !
        call table_resolve_width(self%cache, table_scope_of(self), idx, .true., "width")
        wdt = self%cache%cols(idx)%width
    end function slot_width
    !
    module procedure table_column_width
        integer :: idx
        !
        wdt = 1
        call table_lookup_or_fail(self, name, "width", idx, found)
        if (idx == 0) return
        wdt = slot_width(self, idx)
    end procedure table_column_width
    !
    module procedure table_column_width_at
        integer :: idx
        !
        wdt = 1
        call table_slot_or_fail(self, j, "width", idx, found)
        if (idx == 0) return
        wdt = slot_width(self, idx)
    end procedure table_column_width_at
    !
    !> A resolved slot's unit string ("" when it has none) -- `table_slot_unit` on this table's
    !! cache, kept as the shape the queries below were written against.
    subroutine slot_unit(self, idx, u)
        class(parquet_table), intent(in) :: self        !! the table.
        integer, intent(in) :: idx                      !! a validated slot index.
        character(len=:), allocatable, intent(out) :: u !! the unit, or "".
        !
        call table_slot_unit(self%cache, idx, u)
    end subroutine slot_unit
    !
    !> The descriptor first, because it answers for a column nothing has read yet -- a file-backed
    !! column's unit comes from the read-in MAML at open, not from its values. The values are asked
    !! only for a column that has no descriptor unit, which is every column built with
    !! %add_column(unit=). Shared with the schema-less write (see the interface's doc-comment).
    module procedure table_slot_unit
        if (allocated(cache%cols(idx)%unit)) then
            u = cache%cols(idx)%unit
            return
        end if
        call cache%cols(idx)%values%unit_string(u)
    end procedure table_slot_unit
    !
    module procedure table_column_unit
        integer :: idx
        !
        u = ""
        call table_lookup_or_fail(self, name, "unit", idx, found)
        if (idx == 0) return
        call slot_unit(self, idx, u)
    end procedure table_column_unit
    !
    module procedure table_column_unit_at
        integer :: idx
        !
        u = ""
        call table_slot_or_fail(self, j, "unit", idx, found)
        if (idx == 0) return
        call slot_unit(self, idx, u)
    end procedure table_column_unit_at
    !
    module procedure table_column_residency
        integer :: idx
        !
        r = RES_EMPTY
        call table_lookup_or_fail(self, name, "residency", idx, found)
        if (idx == 0) return
        r = self%cache%cols(idx)%residency
    end procedure table_column_residency
    !
    module procedure table_column_residency_at
        integer :: idx
        !
        r = RES_EMPTY
        call table_slot_or_fail(self, j, "residency", idx, found)
        if (idx == 0) return
        r = self%cache%cols(idx)%residency
    end procedure table_column_residency_at
    !
    module procedure table_is_supported
        integer :: idx
        !
        ok = .false.
        call table_lookup_or_fail(self, name, "is_supported", idx, found)
        if (idx == 0) return
        ok = self%cache%cols(idx)%supported
    end procedure table_is_supported
    !
    module procedure table_is_supported_at
        integer :: idx
        !
        ok = .false.
        call table_slot_or_fail(self, j, "is_supported", idx, found)
        if (idx == 0) return
        ok = self%cache%cols(idx)%supported
    end procedure table_is_supported_at
    !
    module procedure table_generation
        call table_check_open(self, "generation")
        g = self%cache%generation
    end procedure table_generation
    !
    module procedure table_has_nulls
        integer :: idx
        !
        any_null = .false.
        call table_lookup_or_fail(self, name, "has_nulls", idx, found)
        if (idx == 0) return
        any_null = slot_has_nulls(self, idx)
    end procedure table_has_nulls
    !
    module procedure table_has_nulls_at
        integer :: idx
        !
        any_null = .false.
        call table_slot_or_fail(self, j, "has_nulls", idx, found)
        if (idx == 0) return
        any_null = slot_has_nulls(self, idx)
    end procedure table_has_nulls_at
    !
    !> Whether a resolved slot holds (or may hold) a null. Shared by `%has_nulls`' two forms.
    function slot_has_nulls(self, idx) result(any_null)
        class(parquet_table), intent(in) :: self !! the table.
        integer, intent(in) :: idx               !! a validated slot index.
        logical :: any_null                      !! .true. if it holds (or may hold) a null.
        integer(int64) :: rg_lo, rg_hi
        !
        any_null = .false.
        ! A resident column knows the answer exactly. A file-backed one that has NOT been read is
        ! answered from the file's footer instead of by reading it -- the whole reason this exists
        ! rather than the caller reading the column and scanning it. The footer answer is
        ! conservative (see the interface's own note), and it is not asked for a column that is
        ! not file-backed or a table that has detached, since neither has a footer to ask.
        if (self%cache%cols(idx)%residency == RES_FULL) then
            ! parquet_column_any_null, NOT the type-bound %any_null(): the latter is
            ! `intent(inout)` and refreshes a temporal column's null cache while answering, which
            ! makes this read accessor a WRITER on a column any number of threads may be reading.
            ! Reaching the column through `cache` (a pointer) is what lets that compile under
            ! `self` being intent(in), so nothing but this comment stands between the two forms.
            ! The cost of the safe one is that a dirty temporal column rescans per call instead of
            ! memoising. See feature_risks.md Risk-136.
            any_null = parquet_column_any_null(self%cache%cols(idx)%values)
            return
        end if
        if (.not. self%cache%cols(idx)%file_source) return
        if (.not. self%cache%file_backed) return
        ! Scoped to the row groups this table actually covers, so a slice is not told about nulls
        ! in rows it does not hold. 0/0 asks about the whole file, which is right for a whole-file
        ! table; a slice narrows it to its covering row groups.
        rg_lo = 0_int64
        rg_hi = 0_int64
        if (allocated(self%cache%rg_bounds)) then
            call rg_covering_range(self%cache%rg_bounds, self%row_lo, self%row_hi, rg_lo, rg_hi)
        end if
        any_null = parquet_column_has_nulls(self%cache%reader, self%cache%cols(idx)%file_name, rg_lo, rg_hi)
    end function slot_has_nulls
    !
    module procedure table_get_valid_mask
        integer :: idx
        !
        call table_resolve(self, name, "get_valid_mask", idx, found)
        if (idx == 0) then
            allocate(mask(0))
            return
        end if
        call fill_row_mask(self%cache, idx, mask)
    end procedure table_get_valid_mask
    !
    module procedure table_get_valid_mask_elem
        integer :: idx
        !
        call table_resolve(self, name, "get_valid_mask", idx, found)
        if (idx == 0) then
            allocate(mask(0, 0))
            return
        end if
        ! A CONTAINER column is not special-cased here, and used to be refused. Its `width` is 1
        ! (adopt_container fixes it there and says why), so the mask this builds is `(1, nrows)`
        ! and is the row answer in the element shape -- which is what `element_validity`'s own
        ! container arm already computes, and is consistent with `%width` reporting 1 and with
        ! `%is_null(name, i, 1)` answering about the row. See feature_doc_tables.md's S1/S2.
        call fill_elem_mask(self%cache, idx, mask)
    end procedure table_get_valid_mask_elem
    !
    !> Fills `mask` with slot `idx`'s per-ROW validity, always allocated.
    !!
    !! **The bulk builder, not a per-row `is_null` loop.** `parquet_column%row_validity` walks the
    !! bitmap a 64-bit word at a time and iterates the SET bits within a nonzero word (`trailz` +
    !! `ibclr`), so it costs a pass proportional to the NUMBER OF NULLS rather than one call per
    !! row -- and a zero word is 64 valid rows skipped whole. A loop here can only ask one row at a
    !! time, through a call the compiler cannot inline.
    !!
    !! **The one thing that has to be translated is the contract**, and it is inverted between the
    !! two: the bulk builder deliberately hands back an UNALLOCATED mask for a null-free column, so
    !! that passing it straight on as an `optional` `is_valid=` makes the argument absent (F2018
    !! 15.5.2.12) and costs nothing. `%get_valid_mask` promises the opposite -- always allocated,
    !! always filled -- because the whole point of it is an array the caller can use without
    !! testing `allocated()` first. So the unallocated answer becomes an all-`.true.` mask here,
    !! and that fallback branch is the null-free case, i.e. the common one.
    !!
    !! Shared by the public `%get_valid_mask` and by `table_valid_mask_of`, which differ only in
    !! whether they resolve the name themselves.
    subroutine fill_row_mask(cache, idx, mask)
        type(parquet_table_cache), intent(inout) :: cache !! the table's store.
        integer, intent(in) :: idx                        !! slot index.
        logical, allocatable, intent(out) :: mask(:)      !! one entry per row; .true. = value.
        logical, allocatable :: got(:)
        !
        call cache%cols(idx)%values%row_validity(got)
        if (allocated(got)) then
            call move_alloc(got, mask)
            return
        end if
        allocate(mask(cache%cols(idx)%values%length()))
        mask = .true.
    end subroutine fill_row_mask
    !
    !> Fills `mask` with slot `idx`'s per-ELEMENT validity, shaped (width, nrows), always
    !! allocated. The rank-2 counterpart of `fill_row_mask`, with the same reasoning and the same
    !! contract translation -- see its doc-comment.
    subroutine fill_elem_mask(cache, idx, mask)
        type(parquet_table_cache), intent(inout) :: cache !! the table's store.
        integer, intent(in) :: idx                        !! slot index.
        logical, allocatable, intent(out) :: mask(:,:)    !! (element, row); .true. = value.
        logical, allocatable :: got(:,:)
        !
        call cache%cols(idx)%values%element_validity(got)
        if (allocated(got)) then
            call move_alloc(got, mask)
            return
        end if
        allocate(mask(cache%cols(idx)%values%colwidth(), cache%cols(idx)%values%length()))
        mask = .true.
    end subroutine fill_elem_mask
    !
    module procedure table_is_detached
        call table_check_open(self, "is_detached")
        d = self%detached
    end procedure table_is_detached
    !
    module procedure table_filename
        ! Deliberately does NOT go through table_check_open: asking an unopened table where it
        ! came from is a fair question with a good answer ("nowhere"), unlike asking it for values.
        fname = ""
        if (.not. associated(self%cache)) return
        if (allocated(self%cache%source_file)) fname = self%cache%source_file
    end procedure table_filename
    !
    module procedure table_get_file_metadata
        character(len=:), allocatable :: sfx
        logical :: got
        integer :: i
        !
        call table_check_open(self, "get_file_metadata")
        value = ""
        ! Answered from the snapshot taken at open, NOT through the reader -- which is what makes
        ! it keep working after a row mutation has detached the table and released that reader.
        ! `meta_keys` is allocated (possibly zero-size) for every file-backed open, so its being
        ! unallocated is exactly "this table never had a file", which is the question here; by
        ! this point `file_backed` cannot answer it, since detaching clears that too.
        if (.not. allocated(self%cache%meta_keys)) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "get_file_metadata: this table was not opened from a file" // sfx
        end if
        got = .false.
        do i = 1, size(self%cache%meta_keys)
            if (trim(self%cache%meta_keys(i)) == trim(key)) then
                value = trim(self%cache%meta_values(i))
                got = .true.
                exit
            end if
        end do
        if (present(found)) then
            found = got
            return
        end if
        if (.not. got) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "get_file_metadata: no metadata key '" // trim(key) // "'" // sfx
        end if
    end procedure table_get_file_metadata
    !
    module procedure table_is_null_i32
        isnull = self%is_null(name, int(i, int64), found)
    end procedure table_is_null_i32
    !
    module procedure table_is_null_i64
        integer :: idx
        !
        ! .false. on a reported miss, not .true.: "there is no such column" is not "that row is
        ! null", and a caller ignoring `found` should not read a missing column as all-null.
        isnull = .false.
        call table_resolve(self, name, "is_null", idx, found)
        if (idx == 0) return
        call table_require_row(self, i, "is_null")
        isnull = parquet_column_is_null(self%cache%cols(idx)%values, i)
    end procedure table_is_null_i64
    !
    module procedure table_is_null_e32
        isnull = self%is_null(name, int(i, int64), int(e, int64), found)
    end procedure table_is_null_e32
    !
    module procedure table_is_null_e64
        integer :: idx
        !
        ! .false. on a reported miss, for the same reason the row form gives .false.
        isnull = .false.
        call table_resolve(self, name, "is_null", idx, found)
        if (idx == 0) return
        call table_require_row(self, i, "is_null")
        ! A CONTAINER column is not special-cased here either -- `width` is 1, so `e` can only be
        ! 1 and parquet_column_is_null_elem answers about the row, which is the only granularity a
        ! container's nullness has. That keeps this spelling and the column handle's `c%is_null(i,
        ! e)` in agreement; they disagreed while this refused and the handle did not.
        !
        ! The element index is bounds-checked by parquet_column itself (check_element), which
        ! names the element axis in its message -- the mistake this guards is passing a FLAT
        ! element position where a row and an element were wanted.
        isnull = parquet_column_is_null(self%cache%cols(idx)%values, i, e)
    end procedure table_is_null_e64
    !
    module procedure table_is_null_at_i32
        isnull = self%is_null(j, int(i, int64), found)
    end procedure table_is_null_at_i32
    !
    module procedure table_is_null_at_i64
        integer :: idx
        !
        ! .false. on a reported miss, not .true.: "there is no such column" is not "that row is
        ! null", exactly as the name form reasons.
        isnull = .false.
        call table_slot_or_fail(self, j, "is_null", idx, found)
        if (idx == 0) return
        call table_resolve_slot(self, idx, "is_null", found)
        if (idx == 0) return
        call table_require_row(self, i, "is_null")
        isnull = parquet_column_is_null(self%cache%cols(idx)%values, i)
    end procedure table_is_null_at_i64
    !
    module procedure table_is_null_at_e32
        isnull = self%is_null(j, int(i, int64), int(e, int64), found)
    end procedure table_is_null_at_e32
    !
    module procedure table_is_null_at_e64
        integer :: idx
        !
        isnull = .false.
        call table_slot_or_fail(self, j, "is_null", idx, found)
        if (idx == 0) return
        call table_resolve_slot(self, idx, "is_null", found)
        if (idx == 0) return
        call table_require_row(self, i, "is_null")
        isnull = parquet_column_is_null(self%cache%cols(idx)%values, i, e)
    end procedure table_is_null_at_e64
    !
    module procedure table_require_row
        character(len=32) :: got, want
        !
        if (i >= 1_int64 .and. i <= self%row_count) return
        write(got, "(I0)") i
        write(want, "(I0)") self%row_count
        error stop EP // trim(proc) // ": row index " // trim(got) // " is outside this " // &
            "table's 1.." // trim(want) // " rows"
    end procedure table_require_row
    !
    module procedure table_resolve
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        ! Every value accessor -- %col, %get, %set, %is_null, %get_element, a row handle's %get,
        ! %get_slice -- reaches its slot through here, which makes this the one place the cheap
        ! half of the append/read contract has to be written for all of them to have it, including
        ! any accessor added later. One atomic read of a counter, against a call that is about to
        ! copy or point at a whole column.
        call table_check_no_append(self%cache, proc)
        ! The reserved name resolves to the automatic column, materializing it on this first use.
        ! Done here, in the one lookup every value accessor goes through, so every one of them
        ! reaches it without knowing it exists.
        !
        ! LOOK UP FIRST, COMPARE ONLY ON A MISS -- and it must stay that way. `name ==
        ! PARQUET_ROW_INDEX` is a CHARACTER comparison, which gfortran compiles to a call to
        ! `__gfortran_compare_string` (confirmed by disassembly, not inferred): ~5.3 ns, ~12% of a
        ! per-cell read, paid by every value accessor for a column that almost no caller is asking
        ! for. Only a lookup MISS can be the reserved name in its not-yet-materialized state, so
        ! testing after the lookup takes the comparison off the hot path entirely rather than
        ! merely making it cheaper -- and unlike a length or first-character pre-filter it does
        ! not depend on what the caller's column names happen to look like.
        !
        ! Equivalent to the pre-lookup form in all four cases: the name is the reserved one and is
        ! already materialized (both find it), is the reserved one and is not (both materialize,
        ! both re-find, both call `table_find` exactly twice), is the reserved one on a table that
        ! never had a file (both fall through to the ordinary miss), or is an ordinary name (both
        ! resolve it, and only the old form paid for the comparison).
        idx = table_find(self, name)
        if (idx == 0) then
            if (name == PARQUET_ROW_INDEX) then
                ! `meta_keys` is allocated for every table that was opened from a file and stays
                ! so after a detach, which is exactly the question here: a table that HAD a file
                ! gets the row index (or, once detached, the message explaining why it can no
                ! longer have it), while one built in memory never had a file row to name and
                ! falls through to the ordinary "no column of this name".
                if (allocated(self%cache%meta_keys)) then
                    call table_make_row_index(self, proc)
                    idx = table_find(self, name)
                end if
            end if
        end if
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        call table_resolve_slot(self, idx, proc, found, writing)
    end procedure table_resolve
    !
    module procedure table_resolve_slot
        ! A column whose physical type this library cannot read got a slot at open time so it
        ! would still show up in %column_names -- but there is nothing to hand back, so any
        ! attempt to reach its values stops here rather than returning an empty column.
        if (.not. self%cache%cols(idx)%supported) then
            if (present(found)) then
                found = .false.
                idx = 0
                return
            end if
            call table_unsupported_column_abort(self%cache, idx, trim(proc))
        end if
        ! The lazy first touch, and the ONLY place it happens: every value accessor -- %col,
        ! %get, %set, %is_null, a row handle's %get, %get_slice -- reaches its slot through here,
        ! so residency is settled once, in one place. A column already resident costs the
        ! comparison below and nothing else.
        if (self%cache%cols(idx)%residency /= RES_FULL) then
            ! Registered as a read in flight for its duration, so a concurrent %append refuses
            ! rather than reallocating storage underneath it. Only this window is registered, not
            ! the whole accessor: a first touch reads from the file and takes real time, while the
            ! copy the accessor performs afterwards is a memcpy over already-resident memory. The
            ! cheap check above covers that shorter window, and keeping the counter off the
            ! resident path is what leaves a read of a resident column costing the ONE atomic read
            ! table_resolve already made and nothing further -- the property this whole design
            ! exists to protect. See parquet_table_cache's `readers_active` comment.
            call table_read_enter(self%cache, proc)
            call table_touch(self%cache, table_scope_of(self), idx, proc)
            call table_read_exit(self%cache)
        end if
        ! Checked here, after `idx` is known good, so every %set/%set_element specific inherits
        ! the string-column rule from one place rather than each carrying its own copy -- and so
        ! a specific added later gets it by copying its neighbour's table_resolve call.
        if (present(writing)) then
            if (writing) call table_check_shared_write(self, idx, proc, nulling=.false.)
        end if
        if (present(found)) found = .true.
    end procedure table_resolve_slot
    !
    module procedure table_check_not_detached
        character(len=:), allocatable :: sfx
        !
        if (.not. sc%detached) return
        call table_context_suffix(cache, name, sfx)
        error stop EP // trim(proc) // ": this table has been detached from its file by " // &
            "a row-structural change; materialize a column before mutating rows" // sfx
    end procedure table_check_not_detached
    !
    module procedure table_print_stat
        logical :: want_all, want_stats, any_edited
        integer :: i, j, nshown, nscan, wname, wkind, nt
        integer :: u
        integer, allocatable, dimension(:) :: slots, cell_of
        type(stat_cell), allocatable, dimension(:) :: cells
        character(len=:), allocatable :: kname, unit_s, fname, tail
        character(len=32) :: rows_s, nulls_s, wdt_s
        !
        call table_check_open(self, "print_stat")
        ! Solicited output: verbosity="silent" and below turn this into a no-op. The open check
        ! stays ABOVE it, so a print_stat on an unopened table still reports that mistake rather
        ! than silently doing nothing for the wrong reason.
        if (parquet_output_is_suppressed()) return
        ! Wherever `message_stream` names, unless the caller names a unit -- the one rule every
        ! printer and every emit channel in this library follows, through the one resolver.
        u = parquet_message_unit()
        if (present(unit)) u = unit
        want_all = .false.
        if (present(all)) want_all = all
        want_stats = .true.
        if (present(stats)) want_stats = stats
        !
        nshown = 0
        ! At least as wide as the header word, or the header line would be wider than the rows
        ! under it and nothing would line up.
        wname = len("column")
        ! The kind column's floor, and its width for every table that has no unsupported column --
        ! which is every table this library's own writer produced. Only the loop below widens it.
        wkind = 18
        ! Gathered in the pass that is happening anyway, so the legend below costs nothing on a
        ! table with no edited columns -- which is every table read straight from a file.
        any_edited = .false.
        do i = 1, self%cache%ncols
            if (.not. want_all .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            nshown = nshown + 1
            wname = max(wname, len(self%cache%cols(i)%name))
            ! An unsupported column reports Arrow's own spelling of its stored type, which is
            ! longer than any kind name and has no bound in principle (a struct type prints its
            ! whole field list). Sizing the column from the rows keeps such a listing aligned;
            ! 18 stays the floor, so every table without one is byte-identical to before.
            if (.not. self%cache%cols(i)%supported) then
                call table_stat_kind_text(self%cache, i, kname)
                wkind = max(wkind, len(kname))
            end if
            if (self%cache%cols(i)%user_populated) any_edited = .true.
        end do
        call self%filename(fname)
        write(rows_s, "(I0)") self%row_count
        if (len_trim(fname) > 0) then
            write(u, "(a)") "parquet_table: " // trim(fname)
        else
            write(u, "(a)") "parquet_table: (built in memory)"
        end if
        write(nulls_s, "(I0)") self%cache%ncols
        write(wdt_s, "(I0)") count_resident(self%cache)
        write(u, "(a)") "  rows: " // trim(rows_s) // "   columns: " // trim(nulls_s) // &
            " (" // trim(wdt_s) // " materialized)"
        if (nshown == 0) then
            write(u, "(a)") "  (no materialized columns; pass all=.true. to list every column)"
            return
        end if
        ! The statistics pass, done BEFORE anything below the header is printed: every resident
        ! column whose width is settled is scanned once, one column per thread on the team
        ! table_stat_threads resolves (capped by parquet_set_table_threads, serial inside an
        ! existing parallel region or on a small table), into its own text cell. Printing then
        ! only reads the cells, serially and in column order, so the listing is byte-identical
        ! whatever the team was. A deferred plain-LIST column is left out: measuring it would
        ! read the data, and a report must not change what it reports on.
        allocate(cell_of(self%cache%ncols))
        cell_of = 0
        nscan = 0
        if (want_stats) then
            allocate(slots(self%cache%ncols))
            do i = 1, self%cache%ncols
                if (self%cache%cols(i)%residency /= RES_FULL) cycle
                if (self%cache%cols(i)%width_pending) cycle
                nscan = nscan + 1
                slots(nscan) = i
                cell_of(i) = nscan
            end do
            allocate(cells(nscan))
            if (nscan > 0) then
                nt = table_stat_threads(self%cache, slots(1:nscan))
                ! Each iteration reads one column and writes one cell that no other iteration
                ! touches; the columns are distinct allocations. Dynamic, because a string
                ! column costs several times what a numeric one does and the cheap ones should
                ! not wait behind it.
                !$omp parallel do default(shared) private(j) schedule(dynamic) num_threads(nt) &
                !$omp if (nt > 1)
                do j = 1, nscan
                    call fill_stat_cell(self%cache%cols(slots(j))%values, cells(j))
                end do
                !$omp end parallel do
            end if
        end if
        if (want_stats) then
            write(u, "(a)") "  " // pad("column", wname) // "  " // pad("kind", wkind) // "  " // &
                pad("width", 6) // "  " // pad("nulls", 10) // "  " // pad("min", 22) // "  max"
        else
            write(u, "(a)") "  " // pad("column", wname) // "  " // pad("kind", wkind) // "  width"
        end if
        do i = 1, self%cache%ncols
            if (.not. want_all .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            ! A deferred plain-LIST column is reported as pending rather than resolved: measuring
            ! its width would read the data, and a report must not change what it reports on.
            if (self%cache%cols(i)%width_pending) then
                if (want_stats) then
                    write(u, "(a)") "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                        pad("pending", wkind) // "  " // pad("-", 6) // "  " // pad("-", 10) // &
                        "  " // pad("-", 22) // "  -"
                else
                    write(u, "(a)") "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                        pad("pending", wkind) // "  -"
                end if
                cycle
            end if
            call table_stat_kind_text(self%cache, i, kname)
            write(wdt_s, "(I0)") self%cache%cols(i)%width
            if (self%cache%cols(i)%residency /= RES_FULL) then
                if (want_stats) then
                    write(u, "(a)") "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                        pad(kname, wkind) // "  " // pad(trim(wdt_s), 6) // "  " // pad("-", 10) // &
                        "  " // pad("-", 22) // "  -"
                else
                    write(u, "(a)") "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                        pad(kname, wkind) // "  " // trim(wdt_s)
                end if
                cycle
            end if
            call self%cache%cols(i)%values%unit_string(unit_s)
            if (allocated(self%cache%cols(i)%unit)) unit_s = self%cache%cols(i)%unit
            if (len_trim(unit_s) > 0) kname = kname // " [" // trim(unit_s) // "]"
            ! The edited marker goes AFTER the row's last field, which is the only one that is
            ! not padded -- so it cannot overflow anything, and the layout stays byte-identical
            ! for a table with nothing marked. The `kind` field would have been the obvious
            ! place and is already taken: a unit is appended into the same pad(kname, 18). Only
            ! this branch can carry it. A width_pending or non-resident column is never
            ! user_populated, because %evict_column and %reload both clear the flag as they
            ! empty the slot and %set_user_populated refuses to set it on one.
            tail = ""
            if (self%cache%cols(i)%user_populated) tail = " *"
            if (want_stats) then
                j = cell_of(i)
                write(u, "(a)") "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                    pad(kname, wkind) // "  " // pad(trim(wdt_s), 6) // "  " // &
                    pad(cells(j)%nulls, 10) // "  " // pad(cells(j)%mn, 22) // "  " // &
                    cells(j)%mx // tail
            else
                write(u, "(a)") "  " // pad(self%cache%cols(i)%name, wname) // "  " // &
                    pad(kname, wkind) // "  " // trim(wdt_s) // tail
            end if
        end do
        if (any_edited) then
            write(u, "(a)") "  * values written into the table, not the file's own -- %evict_column " // &
                "and %reload need force=."
        end if
    end procedure table_print_stat
    !
    !> One column's statistics into its text cell: the body of `table_print_stat`'s scan loop.
    !!
    !! A subroutine rather than inline in the loop so the parallel region's body is one call with
    !! no allocatable temporaries of its own (feature_risks.md Risk-45 is about a finalizable local
    !! inside a region; a deferred-length string is not one, but keeping the region's body to a
    !! call is the shape every other region in this layer has).
    subroutine fill_stat_cell(values, cell)
        type(parquet_column), intent(in), target :: values !! the resident column to scan.
        type(stat_cell), intent(out) :: cell               !! its statistics as text.
        integer(int64) :: nulls
        character(len=32) :: buf
        !
        call table_column_stat_text(values, nulls, cell%mn, cell%mx)
        write(buf, "(I0)") nulls
        cell%nulls = trim(buf)
    end subroutine fill_stat_cell
    !
    !> The text of a column's `kind` cell in %print_stat. For a column whose stored type this
    !> layer cannot read, that is Arrow's own spelling of the stored type -- the only place such a
    !> listing can say what the column actually holds, since `declared_kind` is PK_NONE for every
    !> one of them and the kind name alone would report them all identically. Every other column
    !> reports its PK_* kind name, exactly as before.
    subroutine table_stat_kind_text(cache, i, text)
        type(parquet_table_cache), intent(in) :: cache      !! the column store.
        integer, intent(in) :: i                            !! 1-based slot.
        character(len=:), allocatable, intent(out) :: text  !! the cell's text.
        !
        ! Guarded the same way table_unsupported_column_abort's own lookup is: a column with no
        ! file behind it has no stored type, and table_detach releases the reader.
        if (.not. cache%cols(i)%supported .and. cache%cols(i)%file_source .and. allocated(cache%reader)) then
            call parquet_get_column_arrow_type(cache%reader, cache%cols(i)%file_name, text)
            return
        end if
        call parquet_kind_name(cache%cols(i)%declared_kind, text)
    end subroutine table_stat_kind_text
    !
    !> How many of a table's columns are resident.
    integer function count_resident(cache) result(n)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        integer :: i
        !
        n = 0
        do i = 1, cache%ncols
            if (cache%cols(i)%residency == RES_FULL) n = n + 1
        end do
    end function count_resident
    !
    module procedure pad
        logical :: rt
        !
        rt = .false.
        if (present(right)) rt = right
        if (rt) then
            ! Blank-filled first, then the text placed against the right-hand edge. `len(res)` is
            ! at least len_trim(text), so the start index is always >= 1, and an all-blank `text`
            ! gives a zero-length substring at res(len+1:len) rather than an invalid one.
            res = ""
            res(len(res) - len_trim(text) + 1:) = trim(text)
        else
            res = trim(text)
        end if
    end procedure pad
    !
    module procedure table_valid_mask_of
        call fill_row_mask(cache, idx, mask)
    end procedure table_valid_mask_of
    !
    module procedure table_valid_mask_rows
        integer(int64) :: k
        !
        allocate(mask(size(rows, kind=int64)))
        do k = 1_int64, size(rows, kind=int64)
            mask(k) = .not. parquet_column_is_null(cache%cols(idx)%values, rows(k))
        end do
    end procedure table_valid_mask_rows
    !
    module procedure table_apply_valid
        integer(int64) :: i
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        if (size(is_valid, kind=int64) /= self%row_count) then
            write(got, "(I0)") size(is_valid, kind=int64)
            write(want, "(I0)") self%row_count
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid has " // trim(got) // " entries but the " // &
                "table has " // trim(want) // " rows" // sfx
        end if
        ! The rank-1 (per-ROW) %set_validity writes the bitmap a word at a time; the loop this
        ! replaces was one type-bound call per null row.
        call self%cache%cols(idx)%values%set_validity(is_valid)
    end procedure table_apply_valid
    !
    module procedure table_apply_valid_rows
        integer(int64) :: k
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        if (size(is_valid, kind=int64) /= size(rows, kind=int64)) then
            write(got, "(I0)") size(is_valid, kind=int64)
            write(want, "(I0)") size(rows, kind=int64)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid has " // trim(got) // " entries but the " // &
                "selection has " // trim(want) // " rows" // sfx
        end if
        ! Kept per row, unlike table_apply_valid above: `rows` selects a SCATTERED set, so there
        ! is no contiguous mask over the column for the bulk setter to take.
        do k = 1_int64, size(rows, kind=int64)
            if (.not. is_valid(k)) call parquet_column_set_null(self%cache%cols(idx)%values, rows(k))
        end do
    end procedure table_apply_valid_rows
    !
    module procedure table_valid_mask_of_elem
        call fill_elem_mask(cache, idx, mask)
    end procedure table_valid_mask_of_elem
    !
    module procedure table_valid_mask_rows_elem
        integer(int64) :: k, e
        integer :: wdt
        !
        wdt = cache%cols(idx)%values%colwidth()
        allocate(mask(wdt, size(rows, kind=int64)))
        do k = 1_int64, size(rows, kind=int64)
            do e = 1_int64, int(wdt, int64)
                mask(e, k) = .not. parquet_column_is_null(cache%cols(idx)%values, rows(k), e)
            end do
        end do
    end procedure table_valid_mask_rows_elem
    !
    module procedure table_apply_valid_elem
        integer(int64) :: i, e, w
        character(len=64) :: got, want
        character(len=:), allocatable :: sfx
        !
        w = int(self%cache%cols(idx)%values%colwidth(), int64)
        if (size(is_valid, 1, kind=int64) /= w .or. size(is_valid, 2, kind=int64) /= self%row_count) then
            write(got, "(I0,A,I0)") size(is_valid, 1, kind=int64), " x ", size(is_valid, 2, kind=int64)
            write(want, "(I0,A,I0)") w, " x ", self%row_count
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid is shaped " // trim(got) // " but the " // &
                "column is " // trim(want) // " (width x rows)" // sfx
        end if
        call self%cache%cols(idx)%values%set_validity(is_valid)
    end procedure table_apply_valid_elem
    !
    module procedure table_apply_valid_rows_elem
        integer(int64) :: k, e, w
        character(len=64) :: got, want
        character(len=:), allocatable :: sfx
        !
        w = int(self%cache%cols(idx)%values%colwidth(), int64)
        if (size(is_valid, 1, kind=int64) /= w .or. &
            size(is_valid, 2, kind=int64) /= size(rows, kind=int64)) then
            write(got, "(I0,A,I0)") size(is_valid, 1, kind=int64), " x ", size(is_valid, 2, kind=int64)
            write(want, "(I0,A,I0)") w, " x ", size(rows, kind=int64)
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": is_valid is shaped " // trim(got) // " but the " // &
                "selection is " // trim(want) // " (width x rows)" // sfx
        end if
        do k = 1_int64, size(rows, kind=int64)
            do e = 1_int64, w
                if (.not. is_valid(e, k)) call parquet_column_set_null(self%cache%cols(idx)%values, rows(k), e)
            end do
        end do
    end procedure table_apply_valid_rows_elem
    !
    module procedure table_require_slice_size
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        if (n_arr == n_rows) return
        write(got, "(I0)") n_arr
        write(want, "(I0)") n_rows
        call table_context_suffix(self%cache, name, sfx)
        error stop EP // trim(proc) // ": the array has " // trim(got) // " values but the " // &
            "selection picks " // trim(want) // " rows" // sfx
    end procedure table_require_slice_size
    !
    module procedure table_require_kind
        call cache_require_kind(self%cache, idx, kind, proc)
    end procedure table_require_kind
    !
    module procedure cache_require_kind
        character(len=:), allocatable :: sfx, got, want
        !
        if (cache%cols(idx)%values%kindof() == kind) return
        call table_context_suffix(cache, cache%cols(idx)%name, sfx)
        call parquet_kind_name(cache%cols(idx)%values%kindof(), got)
        call parquet_kind_name(kind, want)
        error stop EP // trim(proc) // ": column kind is " // got // ", not " // want // sfx
    end procedure cache_require_kind
    !
    module procedure cache_require_ptr_kind
        character(len=:), allocatable :: sfx, kname
        !
        if (cache%cols(idx)%values%kindof() == kind) return
        call table_context_suffix(cache, cache%cols(idx)%name, sfx)
        call parquet_kind_name(cache%cols(idx)%values%kindof(), kname)
        error stop EP // trim(proc) // ": pointer kind does not match the stored kind (" // kname // &
            "); the pointer path never widens -- use %get to copy with widening" // sfx
    end procedure cache_require_ptr_kind
    !
    module procedure table_require_length
        character(len=:), allocatable :: sfx
        character(len=32) :: gots, wants
        !
        if (self%cache%cols(idx)%values%length() == n) return
        call table_context_suffix(self%cache, self%cache%cols(idx)%name, sfx)
        write(gots, "(I0)") n
        write(wants, "(I0)") self%cache%cols(idx)%values%length()
        error stop EP // trim(proc) // ": array has " // trim(gots) // " rows but the column " // &
            "has " // trim(wants) // "; this replaces values, never the row set" // sfx
    end procedure table_require_length
    !
    module procedure table_check_open
        if (.not. associated(self%cache)) then
            ! Exempt from the file/column context suffix by CLAUDE.md's own rule: there is no
            ! file context to name yet.
            error stop EP // trim(proc) // ": table has not been opened"
        end if
    end procedure table_check_open
    !
    module procedure table_context_suffix
        character(len=:), allocatable :: parts
        !
        parts = ""
        if (allocated(cache%source_file)) then
            if (len(cache%source_file) > 0) parts = " (file '" // cache%source_file // "'"
        end if
        if (len(parts) == 0 .and. len_trim(name) > 0) then
            suffix = " (column '" // trim(name) // "')"
            return
        end if
        if (len(parts) == 0) then
            suffix = ""
            return
        end if
        if (len_trim(name) > 0) then
            suffix = parts // ", column '" // trim(name) // "')"
        else
            suffix = parts // ")"
        end if
    end procedure table_context_suffix
    !
    module procedure table_unsupported_column_abort
        character(len=:), allocatable :: sfx, stored
        !
        ! The slot's OWN name, which is what the caller asked for under any entry point -- by name
        ! it is the name they passed, by position it is the one they meant.
        call table_context_suffix(cache, cache%cols(idx)%name, sfx)
        ! Arrow's own spelling of the stored type is the one thing a caller looking at such a
        ! column actually needs: "not supported" says which column is the problem but not what it
        ! is, and every other query answers "unknown" for exactly these columns. Guarded rather
        ! than unconditional -- a column with no file behind it has no stored type to report, and
        ! `table_detach` closes and releases the reader while leaving the slots in place, so
        ! neither can be assumed here.
        if (cache%cols(idx)%file_source .and. allocated(cache%reader)) then
            call parquet_get_column_arrow_type(cache%reader, cache%cols(idx)%file_name, stored)
            sfx = " (stored as " // stored // ")" // sfx
        end if
        error stop EP // trim(proc) // ": this column's type is not supported by " // &
            "parquet_table, so its values were never read" // sfx
    end procedure table_unsupported_column_abort
    !
    module procedure table_lookup_or_fail
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        idx = table_find(self, name)
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        if (present(found)) found = .true.
    end procedure table_lookup_or_fail
    !
end submodule parquet_tables_query
