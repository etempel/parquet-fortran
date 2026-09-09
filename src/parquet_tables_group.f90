!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `%group_by` and the `parquet_grouping` object: a table's rows partitioned by the values of
!! one or more key columns, kept as an object that answers per group and refuses loudly once the
!! table has changed underneath it.
!!
!! **NOT a generated file** -- `tools/generate_parquet_tables.py` emits this module's spec (the
!! type, its bindings and every interface body here), so a signature change is a generator edit
!! and a body change is not.
!!
!! **The partition is `%argsort_by`'s, and nothing here decides an order.** `%group_by` is one
!! `%argsort_by(keys, perm, group_offsets=)` -- the grouping primitive `%duplicated` and
!! `%value_counts` already run on -- so equality is the sort comparator's (every NaN one value,
!! every null one value), the groups come in ascending key order and the rows within a group in
!! ascending row order, whatever engine built it. What this file adds is the OBJECT: the row
!! order and the CSR offsets kept together with the table's `%generation()` stamped at build, the
!! `dropna` pass that leaves a null-keyed row out of every group, and the queries.
!!
!! **`dropna` asks the column, never the sort.** Whether a group is a null-keyed one is read off
!! each key column at the group's representative row (`parquet_column_is_null`), not off where the
!! sort placed the group -- the second half of feature_risks.md Risk-208, for several keys: the
!! null group is last today, and a version that assumed so would break the day a `nulls_first`
!! reaches this verb.
!!
!! **The generation is compared on EVERY per-group query, never cached** (Risk-210's rule,
!! restated for this object). A stale grouping hands out in-range row numbers naming the wrong
!! rows, and anything computed from those is a plausible answer about the wrong rows; one
!! `integer(int64)` comparison per query is the whole cost of not having it. The five
!! introspection bindings describe the object itself and skip it, as their interfaces say.
!!
!! **`%first_rows`/`%last_rows` are computed, not read off the permutation's ends** -- the choice
!! `%drop_duplicates` made (`src/parquet_tables_verbs.f90`, header), so that "first" does not rest
!! on the engine's tie rule.
!!
!! **`%key_table` gathers COPIES** (Risk-208): a gather on the source column would reorder the
!! caller's table with nothing to show it.
submodule (parquet_tables) parquet_tables_group
    ! The ASCII fold the direction-token refusal uses. parquet_utils is already in this module's
    ! footprint (the module itself imports it), so a submodule import costs a consumer nothing.
    use parquet_utils, only : pf_to_lower
    implicit none
    !
    !> Error-message prefix for every `error stop` a query raises. The build's messages carry the
    !! table's own `EP`, since a caller wrote `t%group_by`; a query's caller wrote `grp%rows`.
    character(len=*), parameter :: GP = "parquet_grouping: "
    !
contains
    !
    ! ---- %group_by -------------------------------------------------------------------------------
    !
    module procedure table_group_by
        call group_build(self, keys, grp, dropna, threads)
    end procedure table_group_by
    !
    module procedure table_group_by_string
        character(len=:), allocatable :: toks(:)
        !
        ! The library's one name-list tokenizer, so a key list and a %prefetch column list agree
        ! about punctuation. NOT parquet_parse_sort_key: a key here carries no direction, and
        ! group_check_key_name refuses one that tries to.
        call parquet_split_name_list(keys, toks)
        call group_build(self, toks, grp, dropna, threads)
    end procedure table_group_by_string
    !
    !> The whole of `%group_by`, for both name forms.
    subroutine group_build(self, keys, grp, dropna, threads)
        class(parquet_table), intent(in) :: self   !! the table.
        character(len=*), intent(in) :: keys(:)    !! key column names, primary first.
        type(parquet_grouping), intent(out) :: grp !! receives the grouping.
        logical, intent(in), optional :: dropna    !! .false. keeps rows with a null key.
        integer, intent(in), optional :: threads   !! sort thread request; absent = automatic.
        character(len=*), parameter :: PROC = "group_by"
        character(len=:), allocatable :: sfx
        integer(int64), allocatable :: perm(:), go(:)
        logical, allocatable :: keep(:)
        integer(int64) :: g, ng, rep, nkeep, m, w, sz, j
        integer :: k, idx, wid
        !
        call table_check_open(self, PROC)
        if (size(keys) < 1) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // PROC // ": no key column was given" // sfx
        end if
        grp%drop = .true.
        if (present(dropna)) grp%drop = dropna
        ! Every key is resolved HERE rather than left to %argsort_by, so an unknown or unorderable
        ! column is refused naming THIS verb -- %argsort_by below names itself. The guard itself
        ! is not duplicated: table_lookup_sort_key is the one %sort_by, %join and %value_counts
        ! reach, so there is still one answer to "which column can be a key", and %argsort_by
        ! re-resolving below costs a name lookup on a column that is by then certainly resident.
        wid = 1
        do k = 1, size(keys)
            wid = max(wid, len_trim(keys(k)))
        end do
        allocate(grp%slots(size(keys, kind=int64)))
        allocate(character(len=wid) :: grp%keys(size(keys, kind=int64)))
        do k = 1, size(keys)
            call group_check_key_name(self, keys(k))
            call table_lookup_sort_key(self, trim(keys(k)), PROC, idx, key_kind="group")
            grp%slots(k) = idx
            grp%keys(k) = trim(keys(k))
        end do
        ! One %argsort_by, which is where the partition itself lives: group g is
        ! perm(go(g) : go(g+1) - 1), go has ngroups + 1 entries with a sentinel last, the groups
        ! arrive in ascending key order and every null of a key is one group, placed last. That
        ! placement is handed on as the contract and relied on for nothing below.
        call self%argsort_by(grp%keys, perm, group_offsets=go, threads=threads)
        ng = size(go, kind=int64) - 1_int64
        allocate(keep(ng))
        keep = .true.
        if (grp%drop) then
            do g = 1_int64, ng
                rep = perm(go(g))
                ! ANY null key drops the group (pandas' rule). Asked of the column at the group's
                ! representative row -- every row of a group shares its key tuple, nulls comparing
                ! equal -- and never inferred from the sort's null placement (Risk-208).
                do k = 1, size(grp%slots)
                    if (parquet_column_is_null(self%cache%cols(grp%slots(k))%values, rep)) then
                        keep(g) = .false.
                        exit
                    end if
                end do
            end do
        end if
        if (all(keep)) then
            call move_alloc(perm, grp%perm)
            call move_alloc(go, grp%offsets)
        else
            ! Compact: the kept groups' rows in their order, the offsets renumbered. A dropped
            ! row appears nowhere, which is what makes `%group_ids` answer 0 for it.
            nkeep = count(keep, kind=int64)
            m = 0_int64
            do g = 1_int64, ng
                if (keep(g)) m = m + go(g + 1_int64) - go(g)
            end do
            allocate(grp%perm(m), grp%offsets(nkeep + 1_int64))
            grp%offsets(1) = 1_int64
            w = 0_int64
            j = 0_int64
            do g = 1_int64, ng
                if (.not. keep(g)) cycle
                sz = go(g + 1_int64) - go(g)
                grp%perm(w + 1_int64 : w + sz) = perm(go(g) : go(g + 1_int64) - 1_int64)
                w = w + sz
                j = j + 1_int64
                grp%offsets(j + 1_int64) = w + 1_int64
            end do
        end if
        grp%ngrp = size(grp%offsets, kind=int64) - 1_int64
        grp%nin = grp%offsets(grp%ngrp + 1_int64) - 1_int64
        grp%maxsz = 0_int64
        do g = 1_int64, grp%ngrp
            grp%maxsz = max(grp%maxsz, grp%offsets(g + 1_int64) - grp%offsets(g))
        end do
        grp%ntab = self%row_count
        grp%cache => self%cache
        grp%scope = table_scope_of(self)
        grp%gen = self%cache%generation
        grp%built = .true.
    end subroutine group_build
    !
    !> Refuses one key name that is blank or reads as a sort key rather than a column name.
    !!
    !! The join's rule (`join_check_key_name`, `src/parquet_tables_join.f90`), deliberately
    !! narrower than `parquet_parse_sort_key`'s grammar: only a leading `-` and a trailing
    !! direction WORD are refused, so a column whose name genuinely contains a blank stays usable
    !! as a key.
    subroutine group_check_key_name(self, name)
        class(parquet_table), intent(in) :: self !! the table, for the message suffix.
        character(len=*), intent(in) :: name     !! the raw key name.
        character(len=:), allocatable :: nm, word, sfx
        integer :: sep
        logical :: bad
        !
        nm = trim(adjustl(name))
        if (len(nm) < 1) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "group_by: a key name is blank" // sfx
        end if
        bad = nm(1:1) == "-"
        if (.not. bad) then
            sep = index(nm, " ", back=.true.)
            if (sep > 0) then
                call pf_to_lower(nm(sep + 1:len(nm)), word)
                bad = word == "asc" .or. word == "ascending" .or. word == "desc" .or. &
                    word == "descending"
            end if
        end if
        if (bad) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "group_by: key '" // nm // "' reads as a sort key, and a group key " // &
                "has no direction: the groups come in ascending key order, which is the " // &
                "grouping's contract rather than an option. Name the column on its own" // sfx
        end if
    end subroutine group_check_key_name
    !
    ! ---- the two guards every per-group query runs ----------------------------------------------
    !
    !> Aborts unless the grouping has been built and the table has not changed structurally
    !! since, naming the table and both generations -- the cause of a stale grouping is usually
    !! several statements away from where it is noticed, so the remedy is named too.
    !!
    !! The comparison is made HERE, on every query, and the answer is never cached: a "still
    !! valid" flag would be the stale-flag hazard `table-mutate.md` refuses for sortedness, and
    !! the check is one integer comparison (feature_risks.md Risk-210).
    subroutine grp_resolve(self, proc)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        character(len=:), allocatable :: sfx
        character(len=32) :: g_now, g_then
        !
        if (.not. self%built) then
            error stop GP // trim(proc) // ": no grouping has been built into this object -- " // &
                "call t%group_by(keys, grp) first (or it was cleared with %clear)"
        end if
        if (self%cache%generation == self%gen) return
        write(g_now, "(I0)") self%cache%generation
        write(g_then, "(I0)") self%gen
        call table_context_suffix(self%cache, "", sfx)
        error stop GP // trim(proc) // ": this table has changed structurally since the " // &
            "grouping was built (generation " // trim(g_now) // ", grouping " // trim(g_then) // &
            "); rebuild it with %group_by" // sfx
    end subroutine grp_resolve
    !
    !> Aborts unless `g` names a group, naming the range.
    subroutine grp_check_group(self, g, proc)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        integer(int64), intent(in) :: g             !! the group number offered.
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        character(len=32) :: got, n
        !
        if (g >= 1_int64 .and. g <= self%ngrp) return
        write(got, "(I0)") g
        write(n, "(I0)") self%ngrp
        error stop GP // trim(proc) // ": group " // trim(got) // " is out of range; this " // &
            "grouping has " // trim(n) // " groups, numbered from 1"
    end subroutine grp_check_group
    !
    !> Narrows an `int64` answer array to the `int32` one a caller asked for, aborting rather
    !! than wrapping when an entry does not fit.
    subroutine grp_narrow(v64, proc, what, v32)
        integer(int64), intent(in) :: v64(:)                !! the answers.
        character(len=*), intent(in) :: proc                !! calling binding, for the message.
        character(len=*), intent(in) :: what                !! "row", "count" or "group number", for the message.
        integer(int32), allocatable, intent(out) :: v32(:)  !! the same answers.
        character(len=32) :: txt
        !
        if (size(v64) > 0) then
            if (maxval(v64) > int(huge(0_int32), int64)) then
                ! No test-sized fixture reaches this: it needs a table above 2**31 rows.
                ! GCOVR_EXCL_START
                write(txt, "(I0)") maxval(v64)
                error stop GP // trim(proc) // ": " // what // " " // trim(txt) // " does not " // &
                    "fit the int32 answer asked for; use an int64 variable"
                ! GCOVR_EXCL_STOP
            end if
        end if
        allocate(v32(size(v64, kind=int64)))
        v32 = int(v64, int32)
    end subroutine grp_narrow
    !
    !> Resolves `name` for a per-group read through the grouping's own cache pointer, triggering
    !! the same lazy first touch the table's accessors do -- the row handle's rule
    !! (`row_resolve`, `src/parquet_tables_row.f90`), which is why the grouping carries the
    !! table's scope by value. Aborts on a missing or unsupported column.
    subroutine grp_resolve_column(self, name, proc, idx)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        character(len=*), intent(in) :: name        !! column name.
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        integer, intent(out) :: idx                 !! slot index.
        character(len=:), allocatable :: sfx
        !
        idx = cache_find(self%cache, name)
        if (idx == 0) then
            call table_context_suffix(self%cache, name, sfx)
            error stop GP // trim(proc) // ": no column of this name" // sfx
        end if
        if (.not. self%cache%cols(idx)%supported) then
            call table_unsupported_column_abort(self%cache, idx, "grouping " // trim(proc))
        end if
        if (self%cache%cols(idx)%residency /= RES_FULL) then
            call table_touch(self%cache, self%scope, idx, "grouping " // trim(proc))
        end if
    end subroutine grp_resolve_column
    !
    ! ---- introspection: the object itself, no guard --------------------------------------------
    !
    module procedure grp_ngroups
        n = self%ngrp
    end procedure grp_ngroups
    !
    module procedure grp_nrows
        n = self%nin
    end procedure grp_nrows
    !
    module procedure grp_max_size
        n = self%maxsz
    end procedure grp_max_size
    !
    module procedure grp_nkeys
        n = 0_int64
        if (allocated(self%slots)) n = size(self%slots, kind=int64)
    end procedure grp_nkeys
    !
    module procedure grp_key_names
        integer :: k
        !
        if (.not. allocated(self%keys)) then
            allocate(character(len=0) :: names(0))
            return
        end if
        ! Element by element, never a whole-array assignment from a scalar (the deferred-length
        ! array trap in .claude/rules/fortran-gotchas.md).
        allocate(character(len=len(self%keys)) :: names(size(self%keys, kind=int64)))
        do k = 1, size(self%keys)
            names(k) = self%keys(k)
        end do
    end procedure grp_key_names
    !
    module procedure grp_is_current
        ok = .false.
        if (.not. self%built) return
        ok = self%cache%generation == self%gen
    end procedure grp_is_current
    !
    module procedure grp_clear
        nullify(self%cache)
        self%gen = -1_int64
        self%built = .false.
        self%drop = .true.
        self%ngrp = 0_int64
        self%nin = 0_int64
        self%ntab = 0_int64
        self%maxsz = 0_int64
        if (allocated(self%slots)) deallocate(self%slots)
        if (allocated(self%keys)) deallocate(self%keys)
        if (allocated(self%perm)) deallocate(self%perm)
        if (allocated(self%offsets)) deallocate(self%offsets)
    end procedure grp_clear
    !
    ! ---- the partition, per group ---------------------------------------------------------------
    !
    module procedure grp_size_i64
        integer(int64) :: g
        !
        call grp_resolve(self, "size")
        allocate(counts(self%ngrp))
        do g = 1_int64, self%ngrp
            counts(g) = self%offsets(g + 1_int64) - self%offsets(g)
        end do
    end procedure grp_size_i64
    !
    module procedure grp_size_i32
        integer(int64), allocatable :: c64(:)
        !
        call grp_size_i64(self, c64)
        call grp_narrow(c64, "size", "count", counts)
    end procedure grp_size_i32
    !
    module procedure grp_rows_i64
        integer(int64) :: lo, hi
        !
        call grp_resolve(self, "rows")
        call grp_check_group(self, g, "rows")
        lo = self%offsets(g)
        hi = self%offsets(g + 1_int64) - 1_int64
        allocate(rows(hi - lo + 1_int64))
        rows = self%perm(lo:hi)
    end procedure grp_rows_i64
    !
    module procedure grp_rows_i32
        integer(int64), allocatable :: r64(:)
        !
        call grp_rows_i64(self, g, r64)
        call grp_narrow(r64, "rows", "row", rows)
    end procedure grp_rows_i32
    !
    module procedure grp_csr
        call grp_resolve(self, "csr")
        allocate(offsets(size(self%offsets, kind=int64)), rows(size(self%perm, kind=int64)))
        offsets = self%offsets
        rows = self%perm
    end procedure grp_csr
    !
    module procedure grp_first_rows_i64
        integer(int64) :: g
        !
        call grp_resolve(self, "first_rows")
        allocate(rows(self%ngrp))
        ! The LOWEST row of each group, read off the group itself rather than assumed to sit at
        ! its start -- see this file's header for why that independence from the engine's tie
        ! rule is worth one pass.
        do g = 1_int64, self%ngrp
            rows(g) = minval(self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64))
        end do
    end procedure grp_first_rows_i64
    !
    module procedure grp_first_rows_i32
        integer(int64), allocatable :: r64(:)
        !
        call grp_first_rows_i64(self, r64)
        call grp_narrow(r64, "first_rows", "row", rows)
    end procedure grp_first_rows_i32
    !
    module procedure grp_last_rows_i64
        integer(int64) :: g
        !
        call grp_resolve(self, "last_rows")
        allocate(rows(self%ngrp))
        do g = 1_int64, self%ngrp
            rows(g) = maxval(self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64))
        end do
    end procedure grp_last_rows_i64
    !
    module procedure grp_last_rows_i32
        integer(int64), allocatable :: r64(:)
        !
        call grp_last_rows_i64(self, r64)
        call grp_narrow(r64, "last_rows", "row", rows)
    end procedure grp_last_rows_i32
    !
    module procedure grp_group_ids_i64
        integer(int64) :: g, i
        !
        call grp_resolve(self, "group_ids")
        allocate(codes(self%ntab))
        codes = 0_int64
        do g = 1_int64, self%ngrp
            do i = self%offsets(g), self%offsets(g + 1_int64) - 1_int64
                codes(self%perm(i)) = g
            end do
        end do
    end procedure grp_group_ids_i64
    !
    module procedure grp_group_ids_i32
        integer(int64), allocatable :: c64(:)
        !
        call grp_group_ids_i64(self, c64)
        call grp_narrow(c64, "group_ids", "group number", codes)
    end procedure grp_group_ids_i32
    !
    ! ---- %key_table and %count ------------------------------------------------------------------
    !
    module procedure grp_key_table
        character(len=*), parameter :: PROC = "key_table"
        integer(int64), allocatable :: first(:), counts(:)
        type(parquet_column) :: vals
        character(len=:), allocatable :: cname
        integer :: k
        !
        call grp_resolve(self, PROC)
        if (present(size_name)) then
            cname = trim(adjustl(size_name))
            do k = 1, size(self%slots)
                if (self%keys(k) == cname) then
                    error stop GP // PROC // ": size_name=""" // cname // """ is already a key " // &
                        "column's name, so the result would carry two columns of that name; " // &
                        "call the count column something else"
                end if
            end do
        end if
        call grp_first_rows_i64(self, first)
        call parquet_new_table(out)
        do k = 1, size(self%slots)
            ! A copy, then a gather on the copy: the source column must come out of this
            ! unchanged (Risk-208). The gather carries each row's null state with it, which is
            ! how a `dropna=.false.` null group's key comes out null without this code knowing
            ! how the kind stores one.
            call self%cache%cols(self%slots(k))%values%deep_copy(vals)
            call vals%gather(first)
            ! %add_column's parquet_column form reads kind, width and unit off the column it is
            ! given, which is what lets ONE binding answer for every column kind.
            call out%add_column(trim(self%keys(k)), vals)
        end do
        if (present(size_name)) then
            call grp_size_i64(self, counts)
            call out%add_column(cname, counts)
        end if
    end procedure grp_key_table
    !
    module procedure grp_count_i64
        character(len=*), parameter :: PROC = "count"
        integer :: idx
        integer(int64) :: g, i, n
        !
        call grp_resolve(self, PROC)
        call grp_resolve_column(self, name, PROC, idx)
        allocate(out(self%ngrp))
        ! parquet_column_any_null and parquet_column_is_null answer for all three storage classes
        ! -- the bitmap kinds, the temporal elements and the string store alike -- which is what
        ! lets one binding count a column of any kind. A column without a null is counted by
        ! size alone.
        if (.not. parquet_column_any_null(self%cache%cols(idx)%values)) then
            do g = 1_int64, self%ngrp
                out(g) = self%offsets(g + 1_int64) - self%offsets(g)
            end do
            return
        end if
        do g = 1_int64, self%ngrp
            n = 0_int64
            do i = self%offsets(g), self%offsets(g + 1_int64) - 1_int64
                if (.not. parquet_column_is_null(self%cache%cols(idx)%values, self%perm(i))) then
                    n = n + 1_int64
                end if
            end do
            out(g) = n
        end do
    end procedure grp_count_i64
    !
    module procedure grp_count_i32
        integer(int64), allocatable :: c64(:)
        !
        call grp_count_i64(self, name, c64)
        call grp_narrow(c64, "count", "count", out)
    end procedure grp_count_i32
    !
end submodule parquet_tables_group
