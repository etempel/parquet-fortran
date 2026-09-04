!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Turns a `parquet_table`'s key columns into a row permutation, using the library's own Fortran
!! radix engine -- the SAME engine the read-time `sort_by=` runs on.
!!
!! **That sharing is the whole point of this file.** A table can be ordered two ways: by reading
!! a file with `parquet_open_reader(..., sort_by=...)`, or by `%sort_by` on an already-assembled
!! table. If those two used different comparators they could disagree about null placement, NaN
!! placement or tie order, and nothing would report it -- the rows would simply come back in a
!! different order depending on which path a program happened to take.
!!
!! **The sharing now goes through `parquet_sorting`'s public API rather than the raw C bindings.**
!! `parquet_column` is one of the eleven element types `pf_sort_keys%add` accepts, so a table's
!! key columns are handed over as ordinary sort keys and `pf_argsort` does the rest -- which is
!! how the value extraction, the (seconds, nanoseconds) split every timestamp key needs, and the
!! per-row validity handling all came to live in exactly one place. Before `parquet_sorting`
!! existed this file carried its own copy of all three.
!!
!! What this file therefore does NOT do: decide anything about order, or know anything about how
!! a value reaches the engine. It resolves key names to columns, refuses the columns that cannot
!! be sort keys, and passes the caller's `descending`/`nulls_first` flags through. **The refusals
!! stay here** rather than being left to `pf_argsort`'s own equivalents, because a table can say
!! which column and which file the problem is in and `pf_argsort`, handed a bare column, cannot.
submodule (parquet_tables) parquet_tables_sort
    use parquet_sorting, only : pf_sort_keys, pf_argsort, pf_is_sorted, pf_partial_argsort
    implicit none
    !
contains
    !
    module procedure table_build_sort_permutation
        type(pf_sort_keys) :: skeys
        !
        call sort_collect_keys(self, keys, descending, nulls_first, "sort_by", skeys)
        call pf_argsort(skeys, perm)
    end procedure table_build_sort_permutation
    !
    module procedure table_argsort_by_i64
        type(pf_sort_keys) :: skeys
        !
        call sort_collect_keys(self, keys, descending, nulls_first, "argsort_by", skeys)
        ! group_offsets and group_nkeys are forwarded whether or not they were given: an absent
        ! optional dummy passed on as an optional actual stays absent (F2018 15.5.2.13), which is
        ! what lets pf_argsort keep its one-shot path for callers who asked for no boundaries.
        call pf_argsort(skeys, perm, group_offsets=group_offsets, group_nkeys=group_nkeys)
    end procedure table_argsort_by_i64
    !
    module procedure table_argsort_by_i32
        type(pf_sort_keys) :: skeys
        !
        call sort_collect_keys(self, keys, descending, nulls_first, "argsort_by", skeys)
        ! A local cannot be conditionally absent, so the branch has to be here rather than one
        ! level down -- passing an unconditional group_offsets would give up the one-shot path.
        if (present(group_offsets)) then
            call pf_argsort(skeys, perm, group_offsets=group_offsets, group_nkeys=group_nkeys)
        else
            call pf_argsort(skeys, perm, group_nkeys=group_nkeys)
        end if
    end procedure table_argsort_by_i32
    !
    module procedure table_argsort_partial_i64
        type(pf_sort_keys) :: skeys
        !
        call sort_partial_check_n(n, "argsort_partial")
        call sort_collect_keys(self, keys, descending, nulls_first, "argsort_partial", skeys)
        call pf_partial_argsort(skeys, perm, n)
    end procedure table_argsort_partial_i64
    !
    module procedure table_argsort_partial_i32
        type(pf_sort_keys) :: skeys
        !
        call sort_partial_check_n(n, "argsort_partial")
        call sort_collect_keys(self, keys, descending, nulls_first, "argsort_partial", skeys)
        call pf_partial_argsort(skeys, perm, n)
    end procedure table_argsort_partial_i32
    !
    module procedure table_build_top_n_permutation
        type(pf_sort_keys) :: skeys
        !
        ! "top_n" rather than "argsort_partial" here, so every refusal names the binding the caller
        ! actually used -- which is the whole reason sort_collect_keys takes a procedure name.
        call sort_partial_check_n(n, "top_n")
        call sort_collect_keys(self, keys, descending, nulls_first, "top_n", skeys)
        call pf_partial_argsort(skeys, perm, n)
    end procedure table_build_top_n_permutation
    !
    module procedure table_split_key_list
        character(len=:), allocatable :: toks(:), name, errmsg, sfx, preview
        logical, allocatable :: parsed(:)
        logical :: any_explicit, ok, desc
        integer :: i
        !
        ! One tokenizer for the whole library, so a key list and a name list agree about
        ! punctuation -- commas and semicolons alike, blanks trimmed, empty tokens dropped.
        call parquet_split_name_list(keys, toks)
        if (size(toks) < 1) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // trim(proc) // ": no sort key was given" // sfx
        end if
        allocate(character(len=len(toks)) :: names(size(toks, kind=int64)))
        allocate(parsed(size(toks, kind=int64)))
        any_explicit = .false.
        do i = 1, size(toks)
            ! parquet_core's own sort-key parser, the same one a read-time parquet_sortkey key
            ! goes through -- so "-dec" and "dec desc" mean here exactly what they mean there,
            ! and a future grammar change lands on both at once.
            call parquet_parse_sort_key(toks(i), name, desc, ok, errmsg)
            if (.not. ok) then
                call table_context_suffix(self%cache, "", sfx)
                error stop EP // trim(proc) // ": " // trim(errmsg) // sfx
            end if
            names(i) = name
            parsed(i) = desc
            ! A token carried a direction exactly when the parser had to remove something to get
            ! the name out. Derived rather than reported separately, so it cannot go stale if the
            ! grammar gains another spelling.
            if (name /= trim(toks(i))) any_explicit = .true.
        end do
        if (any_explicit) then
            if (have_descending) then
                ! Refused for the WHOLE call, not per key, and including a redundant "asc": the
                ! two are ways of saying one thing, and a per-key rule would leave descending(1)
                ! governing one key while a token governs another.
                call preview_key_list(keys, preview)
                call table_context_suffix(self%cache, "", sfx)
                error stop EP // trim(proc) // ": the key list " // preview // " already says " // &
                    "which way to sort, so descending= cannot be given as well; use one or the " // &
                    "other" // sfx
            end if
            call move_alloc(parsed, descending)
        end if
        ! `descending` deliberately left UNALLOCATED when no token asked for a direction: passed
        ! on as an optional actual it then makes that dummy absent, so the caller's own
        ! descending= is forwarded untouched.
    end procedure table_split_key_list
    !
    !> Clips a caller-supplied key string to a short quoted preview for an `error stop` message.
    !!
    !! Bounded because the text is the caller's: ifx's ERROR STOP runtime corrupts the heap once
    !! the composed message reaches 8192 bytes, and a key list has no length limit. See CLAUDE.md,
    !! "Never interpolate unbounded caller-supplied text into an `error stop` message".
    subroutine preview_key_list(keys, text)
        character(len=*), intent(in) :: keys               !! the raw key string.
        character(len=:), allocatable, intent(out) :: text !! quoted, clipped preview.
        integer, parameter :: KEYS_MAX = 100               !! characters shown of the key list.
        !
        if (len_trim(keys) > KEYS_MAX) then
            text = "'" // keys(1:KEYS_MAX) // "...'"
        else
            text = "'" // trim(keys) // "'"
        end if
    end subroutine preview_key_list
    !
    module procedure table_sort_by_string
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "sort_by", present(descending), k, d)
        if (allocated(d)) then
            call self%sort_by(k, d, nulls_first)
        else
            call self%sort_by(k, descending, nulls_first)
        end if
    end procedure table_sort_by_string
    !
    module procedure table_top_n_string
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "top_n", present(descending), k, d)
        if (allocated(d)) then
            call self%top_n(k, n, d, nulls_first)
        else
            call self%top_n(k, n, descending, nulls_first)
        end if
    end procedure table_top_n_string
    !
    module procedure table_argsort_by_string_i32
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "argsort_by", present(descending), k, d)
        if (allocated(d)) then
            call self%argsort_by(k, perm, d, nulls_first, group_offsets, group_nkeys)
        else
            call self%argsort_by(k, perm, descending, nulls_first, group_offsets, group_nkeys)
        end if
    end procedure table_argsort_by_string_i32
    !
    module procedure table_argsort_by_string_i64
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "argsort_by", present(descending), k, d)
        if (allocated(d)) then
            call self%argsort_by(k, perm, d, nulls_first, group_offsets, group_nkeys)
        else
            call self%argsort_by(k, perm, descending, nulls_first, group_offsets, group_nkeys)
        end if
    end procedure table_argsort_by_string_i64
    !
    module procedure table_argsort_partial_string_i32
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "argsort_partial", present(descending), k, d)
        if (allocated(d)) then
            call self%argsort_partial(k, perm, n, d, nulls_first)
        else
            call self%argsort_partial(k, perm, n, descending, nulls_first)
        end if
    end procedure table_argsort_partial_string_i32
    !
    module procedure table_argsort_partial_string_i64
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "argsort_partial", present(descending), k, d)
        if (allocated(d)) then
            call self%argsort_partial(k, perm, n, d, nulls_first)
        else
            call self%argsort_partial(k, perm, n, descending, nulls_first)
        end if
    end procedure table_argsort_partial_string_i64
    !
    module procedure table_is_sorted_by_string
        character(len=:), allocatable :: k(:)
        logical, allocatable :: d(:)
        !
        call table_split_key_list(self, keys, "is_sorted_by", present(descending), k, d)
        if (allocated(d)) then
            answer = self%is_sorted_by(k, d, nulls_first)
        else
            answer = self%is_sorted_by(k, descending, nulls_first)
        end if
    end procedure table_is_sorted_by_string
    !
    module procedure table_is_sorted_by
        type(pf_sort_keys) :: skeys
        !
        call sort_collect_keys(self, keys, descending, nulls_first, "is_sorted_by", skeys)
        call pf_is_sorted(skeys, answer)
    end procedure table_is_sorted_by
    !
    !> Rejects a negative `n` here rather than letting `pf_partial_argsort` do it, so the message
    !! names the binding the caller actually used. Too LARGE an `n` is not an error -- it clamps.
    subroutine sort_partial_check_n(n, proc)
        integer, intent(in) :: n             !! the requested row count.
        character(len=*), intent(in) :: proc !! calling procedure, for the message.
        character(len=32) :: n_str
        !
        if (n < 0) then
            write (n_str, "(i0)") n
            error stop EP // proc // ": n is " // trim(n_str) // "; a negative number of rows " // &
                "cannot be ordered"
        end if
    end subroutine sort_partial_check_n
    !
    !> Turns the caller's key NAMES into the `pf_sort_keys` object every sorting binding runs on.
    !!
    !! Shared by `%sort_by`, `%argsort_by`, `%is_sorted_by` and `%argsort_partial` so that all four
    !! refuse exactly the same columns and map `descending`/`nulls_first` exactly the same way. Two
    !! copies of "which columns can be a sort key" is precisely the drift that would let a table
    !! answer `%is_sorted_by` for a column `%argsort_by` rejects.
    !!
    !! `proc` names the caller in every message, so an `%argsort_by` failure does not blame
    !! `sort_by`.
    subroutine sort_collect_keys(self, keys, descending, nulls_first, proc, skeys)
        class(parquet_table), intent(in) :: self        !! the table.
        character(len=*), intent(in) :: keys(:)         !! key columns, primary first.
        logical, intent(in), optional :: descending(:)  !! per key: .true. for descending.
        logical, intent(in), optional :: nulls_first(:) !! per key: .true. to put nulls first.
        character(len=*), intent(in) :: proc            !! calling procedure, for messages.
        type(pf_sort_keys), intent(out) :: skeys        !! the assembled key list.
        integer :: ik, idx
        logical :: desc, nulls_lo
        character(len=32) :: got, want
        !
        call table_check_open(self, proc)
        if (size(keys) < 1) error stop EP // proc // ": no sort key was given"
        if (present(descending)) then
            if (size(descending) /= size(keys)) then
                write(got, "(I0)") size(descending)
                write(want, "(I0)") size(keys)
                error stop EP // proc // ": descending= has " // trim(got) // " entries but " // &
                    trim(want) // " keys were given; it takes one entry per key"
            end if
        end if
        if (present(nulls_first)) then
            if (size(nulls_first) /= size(keys)) then
                write(got, "(I0)") size(nulls_first)
                write(want, "(I0)") size(keys)
                error stop EP // proc // ": nulls_first= has " // trim(got) // " entries but " // &
                    trim(want) // " keys were given; it takes one entry per key"
            end if
        end if
        do ik = 1, size(keys)
            call sort_lookup_key(self, keys(ik), proc, idx)
            desc = .false.
            if (present(descending)) desc = descending(ik)
            nulls_lo = .false.
            if (present(nulls_first)) nulls_lo = nulls_first(ik)
            call skeys%add(self%cache%cols(idx)%values, descending=desc, nulls_first=nulls_lo)
        end do
    end subroutine sort_collect_keys
    !
    module procedure table_lookup_sort_key
        call sort_lookup_key(self, name, proc, idx)
    end procedure table_lookup_sort_key
    !
    !> Resolves one key name to its slot, refusing every column that cannot be a sort key.
    !!
    !! **A key column that has not been read yet is read here**, by the ordinary lazy first touch
    !! every value accessor already goes through. Sorting used to refuse instead, on the grounds
    !! that the memory a `%sort_by` costs should not depend on which columns happened to be
    !! resident -- but that made sorting the one operation on this type that would not fetch what
    !! it plainly needs, and a caller who asks to sort by a column has said what they want read.
    !!
    !! Going through `table_resolve` rather than a bare lookup brings two more things with it, both
    !! wanted here: the cheap half of the append/read contract (a permutation built while another
    !! thread reallocates every column's storage is meaningless), and the reserved
    !! `parquet_row_index` column materialized on demand -- which is what makes
    !! `%argsort_by([PARQUET_ROW_INDEX], perm)` the way back to file order rather than an abort.
    subroutine sort_lookup_key(self, name, proc, idx)
        class(parquet_table), intent(in) :: self !! the table.
        character(len=*), intent(in) :: name     !! the key column's name.
        character(len=*), intent(in) :: proc     !! calling procedure, for messages.
        integer, intent(out) :: idx              !! its slot index.
        character(len=:), allocatable :: sfx, kname
        !
        call table_resolve(self, name, proc, idx)
        if (.not. sort_kind_is_orderable(self%cache%cols(idx)%values%kindof())) then
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            call table_context_suffix(self%cache, name, sfx)
            ! Two reasons, one refusal. A *_VEC row is several values and has no single one to
            ! order by; a CONTAINER row is a list, a map or a struct, for which no total order
            ! exists that this library could have picked -- and the sort must keep reproducing
            ! arrow::compute::SortIndices, which has none for them either. Naming the kind is what
            ! makes the two distinguishable to a reader of the message.
            if (parquet_kind_is_container(self%cache%cols(idx)%values%kindof())) then
                error stop EP // proc // ": a " // kname // " column cannot be a sort key; there " // &
                    "is no defined order on a list, a map or a struct -- sort by a scalar column " // &
                    "and the container is carried along with it" // sfx
            end if
            error stop EP // proc // ": a " // kname // " column cannot be a sort key; there is " // &
                "no defined order on a whole vector row" // sfx
        end if
    end subroutine sort_lookup_key
    !
    !> Whether a PK_* kind can be a sort key: every SCALAR kind can, no *_VEC kind can.
    !!
    !! Kept here, rather than deferred to `pf_argsort`'s own vector-column refusal, so the message
    !! can name the offending column and its file -- see this file's header.
    pure logical function sort_kind_is_orderable(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. &
            kind == PK_FLOAT64 .or. kind == PK_LOGICAL .or. kind == PK_STRING .or. &
            kind == PK_DATE .or. kind == PK_TIME .or. kind == PK_TIMESTAMP
    end function sort_kind_is_orderable
    !
end submodule parquet_tables_sort
