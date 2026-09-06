!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Row-set verbs built on ONE grouping pass: `%duplicated`, `%drop_duplicates`,
!! `%sort_by_values`, `%argsort_by_values` and `%value_counts`.
!!
!! **No verb here mutates a column of THIS table.** Each works out which rows should survive, or
!! what order they should be in, and then calls exactly one of the two shared back halves in
!! `parquet_tables_rowmutate.f90` -- `table_apply_keep` or `apply_permutation` -- which is what
!! keeps every detach in one file. So "does this detach?" is answered by whether the verb ends in
!! one of those two calls, not by reading each verb's body: `%drop_duplicates` and
!! `%sort_by_values` do, and `%duplicated`, `%argsort_by_values` and `%value_counts` do not.
!!
!! `%value_counts` is the one verb that produces a COLUMN rather than a mask or a permutation,
!! and it does it without knowing an element type: it deep-copies the counted column, gathers the
!! copy at one row per distinct value, and hands that to `%add_column`, which reads the kind,
!! width and unit back off it. That is why one binding covers all eighteen column kinds where
!! `%sort_by_values` needs five specifics -- the caller of `%sort_by_values` supplies values, so
!! its type is in the signature, while nothing crosses `%value_counts`\' signature but a name.
!!
!! **Equality is the SORT COMPARATOR'S, deliberately, and it is not a rule this file owns.**
!! `%duplicated` groups rows with one `%argsort_by(keys, perm, group_offsets=go)` -- the grouping
!! primitive the sorting layer already provides and already documents -- so all nulls are one
!! value and all NaNs are one value, which is what pandas' `duplicated` does. Writing a second
!! notion of equality here is exactly the drift that would let `%drop_duplicates` and
!! `%argsort_by` disagree about which rows are the same.
!!
!! **`keep="first"` does NOT rest on the sort engine's tie rule.** The plan for this file
!! proposed taking the group's first row as `perm(go(g))`, which is the lowest row index only
!! because the engine holds file order for ties -- a dependency that would break silently if that
!! rule ever changed, since the kept row would still be *a* row of the group and only a fixture
!! whose other columns differ across a group could tell. The group's lowest and highest row
!! indices are taken from `perm(lo:hi)` directly instead. It is one extra pass over each group,
!! O(nrows) in total against a mask fill that is already O(nrows), and it removes the dependency
!! rather than documenting it.
!!
!! **`%sort_by_values` is not `%take(indices)`.** The caller hands over VALUES, one per row, and
!! the table computes the permutation with `pf_argsort` and applies it through the same trusted
!! reindex path `%sort_by` uses -- so the two cannot order the same values differently, and a
!! caller never gets to hand this library a row order it has not checked.
submodule (parquet_tables) parquet_tables_verbs
    use parquet_sorting, only : pf_argsort
    implicit none
    !
    !> Which row of a group of equal rows `%duplicated` spares. Resolved from `keep` once, before
    !! any grouping happens, so an unknown token is refused whether or not the table has any
    !! duplicate at all -- a guard that fires only when there is something to drop is a guard
    !! nothing tests.
    integer, parameter :: KEEP_FIRST = 1 !! keep="first": spare the LOWEST row index of each group.
    integer, parameter :: KEEP_LAST = 2  !! keep="last": spare the HIGHEST row index of each group.
    integer, parameter :: KEEP_NONE = 3  !! keep="none": spare nothing in a group of more than one.
    !
contains
    !
    ! ---- %duplicated: the three entry points --------------------------------------------------
    !
    module procedure table_duplicated
        call duplicated_apply(self, keys, "duplicated", mask, keep, threads)
    end procedure table_duplicated
    !
    module procedure table_duplicated_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(keys, toks)
        call duplicated_apply(self, toks, "duplicated", mask, keep, threads)
    end procedure table_duplicated_string
    !
    module procedure table_duplicated_all
        character(len=:), allocatable :: toks(:)
        !
        call table_check_open(self, "duplicated")
        call resident_key_names(self, "duplicated", toks)
        call duplicated_apply(self, toks, "duplicated", mask, keep, threads)
    end procedure table_duplicated_all
    !
    ! ---- %drop_duplicates: the three entry points ---------------------------------------------
    !
    module procedure table_drop_duplicates
        logical, allocatable :: mask(:)
        !
        call table_check_not_shared(self, "drop_duplicates")
        call duplicated_apply(self, keys, "drop_duplicates", mask, keep, threads)
        call table_apply_keep(self, .not. mask, "drop_duplicates")
    end procedure table_drop_duplicates
    !
    module procedure table_drop_duplicates_string
        character(len=:), allocatable :: toks(:)
        !
        call parquet_split_name_list(keys, toks)
        call self%drop_duplicates(toks, keep, threads)
    end procedure table_drop_duplicates_string
    !
    module procedure table_drop_duplicates_all
        character(len=:), allocatable :: toks(:)
        !
        call table_check_not_shared(self, "drop_duplicates")
        call table_check_open(self, "drop_duplicates")
        call resident_key_names(self, "drop_duplicates", toks)
        call self%drop_duplicates(toks, keep, threads)
    end procedure table_drop_duplicates_all
    !
    ! ---- shared plumbing ----------------------------------------------------------------------
    !
    !> The whole of `%duplicated`, for all three name forms and for `%drop_duplicates` too.
    !!
    !! `proc` names the caller in every message, so a `%drop_duplicates` failure does not blame
    !! `duplicated`.
    subroutine duplicated_apply(self, keys, proc, mask, keep, threads)
        class(parquet_table), intent(in) :: self       !! the table.
        character(len=*), intent(in) :: keys(:)        !! the key columns.
        character(len=*), intent(in) :: proc           !! calling procedure, for messages.
        logical, allocatable, intent(out) :: mask(:)   !! .true. on each row a drop would remove.
        character(len=*), intent(in), optional :: keep !! "first" (default), "last" or "none".
        integer, intent(in), optional :: threads       !! sort thread request; absent = automatic.
        integer(int64), allocatable :: perm(:), go(:)
        integer(int64) :: g, lo, hi, spare
        integer :: mode, k, idx
        !
        ! Resolved BEFORE the sort, so that a bad token costs nothing and is refused on a table
        ! with no duplicates just as loudly as on one with them.
        call resolve_keep(keep, proc, mode)
        ! Every key is resolved HERE rather than left to %argsort_by, for one reason: the message.
        ! %argsort_by names itself in its refusals, so a caller who wrote %drop_duplicates would be
        ! told that `argsort_by` could not find their column. The guard itself is not duplicated --
        ! this is table_lookup_sort_key, the same one %sort_by reaches -- so there is still one
        ! answer to "which column can be a key", and %argsort_by re-resolving below costs a name
        ! lookup on a column that is by then certainly resident.
        do k = 1, size(keys)
            call table_lookup_sort_key(self, keys(k), proc, idx)
        end do
        ! One %argsort_by, which is where every key refusal, every lazy first touch and the
        ! grouping itself live. `group_offsets` is what makes this a grouping rather than a sort:
        ! group g is perm(go(g) : go(g+1) - 1), and go has ngroups + 1 entries with a sentinel
        ! last, so there is no last-iteration special case.
        call self%argsort_by(keys, perm, group_offsets=go, threads=threads)
        allocate(mask(self%row_count), source=.false.)
        do g = 1_int64, size(go, kind=int64) - 1_int64
            lo = go(g)
            hi = go(g + 1_int64) - 1_int64
            if (hi <= lo) cycle              ! a group of one row is nobody's duplicate
            if (mode == KEEP_NONE) then
                mask(perm(lo:hi)) = .true.
                cycle
            end if
            ! The lowest (or highest) ROW INDEX in the group, read off the group itself rather
            ! than assumed to be at one end of it -- see this file's header for why that
            ! independence from the engine's tie rule is worth one pass.
            if (mode == KEEP_FIRST) then
                spare = minval(perm(lo:hi))
            else
                spare = maxval(perm(lo:hi))
            end if
            mask(perm(lo:hi)) = .true.
            mask(spare) = .false.
        end do
    end subroutine duplicated_apply
    !
    !> Turns the caller's `keep` token into one of the three KEEP_* modes, refusing anything else.
    subroutine resolve_keep(keep, proc, mode)
        character(len=*), intent(in), optional :: keep !! the caller's token, if any.
        character(len=*), intent(in) :: proc           !! calling procedure, for messages.
        integer, intent(out) :: mode                   !! the resolved KEEP_* constant.
        !
        mode = KEEP_FIRST
        if (.not. present(keep)) return
        select case (trim(adjustl(keep)))
        case ("first")
            mode = KEEP_FIRST
        case ("last")
            mode = KEEP_LAST
        case ("none")
            mode = KEEP_NONE
        case default
            error stop EP // proc // ": keep=""" // trim(adjustl(keep)) // """ is not a policy; " // &
                "it is ""first"" (keep the lowest row index of each group, the default), " // &
                """last"" (the highest) or ""none"" (keep no row of any repeated group)"
        end select
    end subroutine resolve_keep
    !
    !> The names of every RESIDENT column, for the two forms that name none.
    !!
    !! **A resident column that cannot be a sort key is not filtered out here.** It is named like
    !! any other and refused by `duplicated_apply`'s key loop, which is `table_lookup_sort_key` --
    !! the same guard `%sort_by` reaches. Leaving such a column out instead would silently answer
    !! about a different set of columns than this form promises, and drop rows that differ only in
    !! the column that was left out, which nothing downstream could detect.
    subroutine resident_key_names(self, proc, names)
        class(parquet_table), intent(in) :: self                 !! the table.
        character(len=*), intent(in) :: proc                     !! calling procedure, for messages.
        character(len=:), allocatable, intent(out) :: names(:)   !! one name per resident column.
        integer, allocatable :: slots(:)
        integer :: k, wid
        !
        ! Exactly the set every row-structural operation rewrites (resident AND supported), so
        ! "the columns this looked at" and "the columns a drop would rewrite" cannot come apart.
        call table_mutable_slots(self, slots)
        ! Nothing resident is refused rather than answered. Comparing rows on NO columns would make
        ! every row equal to every other and collapse the table to one row, which is the opposite of
        ! what a caller asking to remove duplicates wants -- and `%dropna()`'s "drops no row" answer
        ! is not available here either, since that is a silent no-op where this would be a silent
        ! wrong answer. Without this the abort still happens, one level down, as `argsort_by: no
        ! sort key was given`, which names the wrong verb and does not say what to do.
        if (size(slots) == 0) then
            error stop EP // proc // ": no column of this table has been read yet, so there is " // &
                "nothing to compare rows on; %prefetch or %materialize_all first, or name the " // &
                "key columns"
        end if
        wid = 1
        do k = 1, size(slots)
            wid = max(wid, len(self%cache%cols(slots(k))%name))
        end do
        allocate(character(len=wid) :: names(size(slots, kind=int64)))
        ! Assigned element by element, and deliberately NOT blanked with `names = ""` first: a
        ! whole-array assignment from a scalar reallocates every element of a deferred-length
        ! allocatable array to length zero, after which each `names(k) = ...` below writes the
        ! declared width into a zero-length allocation. ifx does that and gfortran hides it (see
        ! CLAUDE.md's general Fortran gotchas), so the blanking step must not be added back.
        do k = 1, size(slots)
            names(k) = self%cache%cols(slots(k))%name
        end do
    end subroutine resident_key_names
    !
    ! ---- %sort_by_values and %argsort_by_values ------------------------------------------------
    !
    !> Refuses a value list that is not one entry per row, before any sorting happens.
    subroutine check_values_length(self, n, proc)
        class(parquet_table), intent(in) :: self !! the table.
        integer(int64), intent(in) :: n          !! the value list's length.
        character(len=*), intent(in) :: proc     !! calling procedure, for messages.
        character(len=32) :: got, want
        !
        call table_check_open(self, proc)
        if (n /= self%row_count) then
            write(got, "(I0)") n
            write(want, "(I0)") self%row_count
            error stop EP // proc // ": the value list has " // trim(got) // " entries but the " // &
                "table has " // trim(want) // " rows; it takes one value per row"
        end if
    end subroutine check_values_length
    !
    ! Each specific is the same three steps -- refuse a shared table (the mutating half
    ! only), refuse a value list of the wrong length, then hand the values to pf_argsort --
    ! and differs only in the type of `values` and, for the argsort half, the kind of
    ! `perm`. They are written out because Fortran has no way to be generic over either.
    !
    module procedure table_sort_by_values_i32
        integer(int64), allocatable :: perm(:)
        !
        call table_check_not_shared(self, "sort_by_values")
        call check_values_length(self, size(values, kind=int64), "sort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
        call apply_permutation(self, perm)
    end procedure table_sort_by_values_i32
    !
    module procedure table_sort_by_values_i64
        integer(int64), allocatable :: perm(:)
        !
        call table_check_not_shared(self, "sort_by_values")
        call check_values_length(self, size(values, kind=int64), "sort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
        call apply_permutation(self, perm)
    end procedure table_sort_by_values_i64
    !
    module procedure table_sort_by_values_f32
        integer(int64), allocatable :: perm(:)
        !
        call table_check_not_shared(self, "sort_by_values")
        call check_values_length(self, size(values, kind=int64), "sort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
        call apply_permutation(self, perm)
    end procedure table_sort_by_values_f32
    !
    module procedure table_sort_by_values_f64
        integer(int64), allocatable :: perm(:)
        !
        call table_check_not_shared(self, "sort_by_values")
        call check_values_length(self, size(values, kind=int64), "sort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
        call apply_permutation(self, perm)
    end procedure table_sort_by_values_f64
    !
    module procedure table_sort_by_values_chr
        integer(int64), allocatable :: perm(:)
        !
        call table_check_not_shared(self, "sort_by_values")
        call check_values_length(self, size(values, kind=int64), "sort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
        call apply_permutation(self, perm)
    end procedure table_sort_by_values_chr
    !
    module procedure table_argsort_by_values_i32_i32
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_i32_i32
    !
    module procedure table_argsort_by_values_i32_i64
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_i32_i64
    !
    module procedure table_argsort_by_values_i64_i32
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_i64_i32
    !
    module procedure table_argsort_by_values_i64_i64
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_i64_i64
    !
    module procedure table_argsort_by_values_f32_i32
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_f32_i32
    !
    module procedure table_argsort_by_values_f32_i64
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_f32_i64
    !
    module procedure table_argsort_by_values_f64_i32
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_f64_i32
    !
    module procedure table_argsort_by_values_f64_i64
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_f64_i64
    !
    module procedure table_argsort_by_values_chr_i32
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_chr_i32
    !
    module procedure table_argsort_by_values_chr_i64
        call check_values_length(self, size(values, kind=int64), "argsort_by_values")
        call pf_argsort(values, perm, descending=descending, nulls_first=nulls_first, &
            is_valid=is_valid, threads=threads)
    end procedure table_argsort_by_values_chr_i64
    !
    ! ---- %value_counts: how often each distinct value occurs ----------------------------------
    !
    module procedure table_value_counts
        integer(int64), allocatable :: perm(:), go(:), cnt(:), reps(:), order(:)
        integer(int64), allocatable :: sel(:), out_counts(:)
        integer(int64) :: g, ng, k, m, nout, nnull, null_rep, rep
        integer :: idx
        logical :: drop, desc, has_null
        type(parquet_column) :: vals
        character(len=:), allocatable :: cname
        !
        call table_check_open(self, "value_counts")
        drop = .true.
        if (present(dropna)) drop = dropna
        desc = .true.
        if (present(descending)) desc = descending
        cname = "count"
        if (present(count_name)) cname = trim(adjustl(count_name))
        ! Resolved HERE, exactly as duplicated_apply resolves its keys, so an unknown or
        ! unorderable column is refused naming THIS verb -- %argsort_by below names itself. The
        ! same guard, not a second one.
        call table_lookup_sort_key(self, name, "value_counts", idx)
        if (self%cache%cols(idx)%name == cname) then
            error stop EP // "value_counts: the counted column is already called """ // cname // &
                """, so the result would carry two columns of that name; pass count_name= to " // &
                "call the count column something else"
        end if
        ! The grouping primitive %duplicated uses, for the same reason: equality is the sort
        ! comparator's, so every NaN is one value and every null is one. Group g is
        ! perm(go(g) : go(g+1) - 1), and the groups arrive in ASCENDING VALUE order -- which is
        ! what makes the stable count sort below break ties by value ascending for free.
        call self%argsort_by([name], perm, group_offsets=go, threads=threads)
        ng = size(go, kind=int64) - 1_int64
        allocate(cnt(max(ng, 1_int64)), reps(max(ng, 1_int64)))
        m = 0_int64
        nnull = 0_int64
        null_rep = 0_int64
        has_null = .false.
        do g = 1_int64, ng
            rep = perm(go(g))
            ! Every null is in ONE group, since nulls compare equal to each other -- so this fires
            ! at most once and the null count is that group's size rather than a second scan. The
            ! group is found by asking the column rather than by assuming which end %argsort_by
            ! puts nulls at, so nulls_first is nobody's business here.
            if (parquet_column_is_null(self%cache%cols(idx)%values, rep)) then
                has_null = .true.
                null_rep = rep
                nnull = go(g + 1_int64) - go(g)
                cycle
            end if
            m = m + 1_int64
            cnt(m) = go(g + 1_int64) - go(g)
            reps(m) = rep
        end do
        ! Stable, and that stability IS the documented tie-break: equal counts keep the ascending
        ! value order they arrived in, in both directions of `descending`.
        call pf_argsort(cnt(1:m), order, descending=desc)
        nout = m
        if (has_null .and. .not. drop) nout = m + 1_int64
        allocate(sel(max(nout, 1_int64)), out_counts(max(nout, 1_int64)))
        do k = 1_int64, m
            sel(k) = reps(order(k))
            out_counts(k) = cnt(order(k))
        end do
        ! The null row is GATHERED like every other, from a row of the source column that really
        ! is null -- so the result's null state comes from the column itself and nothing here has
        ! to know how this kind stores one (the temporal kinds and the string kinds do not use the
        ! bitmap at all). Placed last whatever its count, per this binding's contract.
        if (nout > m) then
            sel(nout) = null_rep
            out_counts(nout) = nnull
        end if
        ! A copy, then a gather on the copy: `self` is intent(in) and this is a read, so the
        ! source column must come out of it unchanged.
        call self%cache%cols(idx)%values%deep_copy(vals)
        call vals%gather(sel(1:nout))
        call parquet_new_table(out)
        ! %add_column's parquet_column form reads kind, width and unit off the column it is given,
        ! which is what lets ONE binding answer for all eighteen column kinds -- nothing above
        ! this line knows the element type either.
        call out%add_column(self%cache%cols(idx)%name, vals)
        call out%add_column(cname, out_counts(1:nout))
    end procedure table_value_counts
    !
end submodule parquet_tables_verbs
