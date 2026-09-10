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
!!
!! **`%apply` calls the caller's procedure once per group with the group's rows and nothing
!! else**, in group order when serial, and stores each result at its own `g` when on a team, so
!! the answer does not depend on the schedule. The loop is SERIAL unless `threads=` is given:
!! the library cannot know whether a caller's procedure is re-entrant, so the serial default is
!! a decision made at the call (`group_team`), not a floor that declined, and an explicit
!! request is the caller's declaration. The team is recorded on every call for the test-only
!! hook, because a loop that resolved a team it never opened, or opened one nobody asked for,
!! is invisible in every answer (feature_risks.md Risk-189's shape).
!!
!! **`%agg` is `parquet_stats` called once per group, and nothing else.** Every token delegates
!! to the named `pf_*` procedure over the group's values gathered into a real64 buffer -- the
!! widening that module applies to every kind itself -- so a group's answer is that procedure's
!! answer over the same rows, bit for bit, whatever the thread count; a second moment or
!! quantile engine here is the copy `feature_pandas_S4.md`'s one-engine rule forbids. Validity
!! is passed as `is_valid=` only when the group holds a null and weights only when the caller
!! gave some, so a null-free, unweighted group takes each procedure's fast path. The exact
!! `int64` family never touches the buffer. The first group of a threaded loop is computed
!! BEFORE the team opens, so an argument the statistics module refuses (an unknown `method=`, a
!! `q=` outside 0..1) aborts serially and names itself; the data-dependent aborts inside the
!! team (an `int64` sum overflowing, a group with no exact answer) go through one `critical`.
!!
!! **`%nunique` is one more sort**, over the keys and then the column, so that distinctness is
!! the comparator's own equality and its outer groups are this grouping's; a walk over the
!! finest runs counts them per group, skipping the null run unless `dropna=.false.`.
submodule (parquet_tables) parquet_tables_group
    ! The ASCII fold the direction-token refusal and the statistic tokens use. parquet_utils is
    ! already in this module's footprint (the module itself imports it), so a submodule import
    ! costs a consumer nothing.
    use parquet_utils, only : pf_to_lower
    ! The affinity clamp every resolved thread count goes through and the library's one automatic
    ! thread rule (.claude/rules/api-conventions.md, "Thread counts"), plus the table tier's cap
    ! that rule is given; both modules are in this module's footprint already.
    use parquet_settings_base, only : parquet_clamp_to_affinity, parquet_auto_thread_count
    use parquet_settings, only : parquet_get_table_threads
    ! The statistics tier: %agg's vocabulary IS these procedures called per group. This import is
    ! the six files `use parquet_tables` compiles beyond what it did without %agg
    ! (tools/module_footprints.txt; feature_pandas_S5.md, answer 3).
    use parquet_stats, only : pf_count_valid, pf_sum, pf_mean, pf_variance, pf_stddev, pf_sem, pf_moments, &
        pf_median, pf_quantile, pf_iqr, pf_mad
    ! The NaN "first" and "last" answer over a group with no non-null value.
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    implicit none
    !
    !> Error-message prefix for every `error stop` a query raises. The build's messages carry the
    !! table's own `EP`, since a caller wrote `t%group_by`; a query's caller wrote `grp%rows`.
    character(len=*), parameter :: GP = "parquet_grouping: "
    !
    !> The statistic tokens, as codes: one per token of the real64 vocabulary, the exact family
    !! reusing the codes of the tokens it shares.
    integer, parameter :: AG_SIZE = 1, AG_COUNT = 2, AG_SUM = 3, AG_MEAN = 4, AG_VAR = 5, AG_STD = 6, &
        AG_SEM = 7, AG_MIN = 8, AG_MAX = 9, AG_RANGE = 10, AG_MEDIAN = 11, AG_QUANTILE = 12, AG_IQR = 13, &
        AG_MAD = 14, AG_FIRST = 15, AG_LAST = 16, AG_NUNIQUE = 17
    !> The two vocabularies, as the refusal of an unknown token lists them.
    character(len=*), parameter :: AG_REAL_TOKENS = "size, count, sum, mean, var, std, sem, min, max, range, " // &
        "median, quantile, iqr, mad, first, last, nunique"
    character(len=*), parameter :: AG_INT_TOKENS = "size, count, nunique, sum, min, max, first, last"
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
    ! ---- %apply: the per-group callback ---------------------------------------------------------
    !
    !> The team one per-group loop runs on, and the record of it. `threads` ABSENT is a DECISION
    !! made here, and it differs by loop: on a callback loop (`auto` false) it means SERIAL,
    !! because the library cannot know whether the caller's procedure is re-entrant, so only an
    !! explicit request opens a team; on the vocabulary loop (`auto` true, library code that is
    !! re-entrant by construction) it means the library's one automatic rule under the table
    !! tier's cap, serial inside a parallel region. A request is refused below 1, clamped to this
    !! process's CPU affinity by the one clamp every resolved count goes through
    !! (.claude/rules/api-conventions.md, "Thread counts"), and honoured inside an existing
    !! parallel region as the index tier honours its own; a loop of fewer than two groups is
    !! serial whatever was asked. The answer is recorded for
    !! `parquet_debug_get_group_threads_used` on EVERY route, 1 included, so a test can tell
    !! "declined" from "ran" (feature_risks.md Risk-189).
    subroutine group_team(n, threads, proc, auto, nt)
        integer(int64), intent(in) :: n          !! groups in the loop.
        integer, intent(in), optional :: threads !! the caller's request, or absent.
        character(len=*), intent(in) :: proc     !! calling binding, for the message.
        logical, intent(in) :: auto              !! absent `threads`: .true. resolves automatically, .false. is serial.
        integer, intent(out) :: nt               !! the team; 1 means serial.
        character(len=32) :: txt
        !
        nt = 1
        if (auto .and. .not. present(threads)) then
            nt = parquet_auto_thread_count(parquet_get_table_threads(), "grouping")
        end if
        if (present(threads)) then
            if (threads < 1) then
                write(txt, "(I0)") threads
                error stop GP // trim(proc) // ": threads= must be at least 1, got " // trim(txt) // &
                    "; leave it absent for a serial loop"
            end if
            nt = parquet_clamp_to_affinity(threads, "grouping")
        end if
        if (n < 2_int64) nt = 1
        call group_note_threads(nt)
    end subroutine group_team
    !
    !> The chunk of groups a thread takes at a time under `schedule(dynamic)`: groups are
    !! unequal, so the schedule is dynamic, and the chunk amortises a scheduling step over many
    !! small groups without starving a team of a few large ones.
    integer function group_chunk(n, nt) result(c)
        integer(int64), intent(in) :: n !! groups in the loop.
        integer, intent(in) :: nt       !! the team.
        c = int(max(1_int64, min(16_int64, n / (4_int64 * int(nt, int64)))))
    end function group_chunk
    !
    !> Records the team a per-group loop ran on, for the test-only
    !! `parquet_debug_get_group_threads_used` (src/parquet_wrapper.cpp), through a local
    !! `bind(C)` interface as every `parquet_debug_*` hook is reached.
    subroutine group_note_threads(nt)
        use iso_c_binding, only : c_int64_t
        integer, intent(in) :: nt !! the team; 1 means serial.
        interface
            subroutine set_used(n) bind(C, name="parquet_debug_set_group_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: n
            end subroutine set_used
        end interface
        !
        call set_used(int(nt, c_int64_t))
    end subroutine group_note_threads
    !
    !> Aborts unless `nout` is at least 1: a result row with no entries is a mistake, not a
    !! zero-length answer. Impure on purpose (a `pure` guard-only call is deleted by one
    !! supported compiler at -O0; .claude/rules/api-conventions.md).
    subroutine grp_check_nout(nout, proc)
        integer, intent(in) :: nout          !! results per group asked for.
        character(len=*), intent(in) :: proc !! calling binding, for the message.
        character(len=32) :: txt
        !
        if (nout >= 1) return
        write(txt, "(I0)") nout
        error stop GP // trim(proc) // ": nout = " // trim(txt) // " is not positive; a result row " // &
            "needs at least one entry (the one-value form takes no nout)"
    end subroutine grp_check_nout
    !
    ! Each body below has the same two arms, written out rather than folded into one loop under
    ! an `if (nt > 1)` clause: the serial arm is the DEFAULT and runs the groups in order, by
    ! contract, with no OpenMP runtime between it and the caller's procedure; the team arm deals
    ! the groups out in no particular order and each result lands at its own g. Passing
    ! `self%perm(lo:hi)` is a contiguous section, so no copy is made per group.
    !
    ! The two procedure-form bodies restate their interfaces in full, unlike every other body in
    ! this file: in the abbreviated `module procedure` form gfortran 15 treats the dummy
    ! PROCEDURE `func` as having an implicit interface (-Werror=implicit-interface), although the
    ! spec declares it `procedure(parquet_group_reduce_i)` (.claude/rules/fortran-gotchas.md).
    !
    !> %apply specific, procedure, one value per group; the contract is on the interface in the
    !! module spec.
    module subroutine grp_apply_proc_scalar(self, func, out, threads)
        class(parquet_grouping), intent(in) :: self      !! the grouping.
        procedure(parquet_group_reduce_i) :: func        !! called once per group; see the interface.
        real(real64), allocatable, intent(out) :: out(:) !! one value per group, in group order.
        integer, intent(in), optional :: threads         !! the team; absent = serial.
        character(len=*), parameter :: PROC = "apply"
        integer(int64) :: g
        integer :: nt, chunk
        !
        call grp_resolve(self, PROC)
        call group_team(self%ngrp, threads, PROC, .false., nt)
        allocate(out(self%ngrp))
        if (nt > 1) then
            chunk = group_chunk(self%ngrp, nt)
            !$omp parallel do num_threads(nt) default(shared) private(g) schedule(dynamic, chunk)
            do g = 1_int64, self%ngrp
                out(g) = func(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64))
            end do
            !$omp end parallel do
        else
            do g = 1_int64, self%ngrp
                out(g) = func(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64))
            end do
        end if
    end subroutine grp_apply_proc_scalar
    !
    !> %apply specific, procedure, `nout` values per group; the contract is on the interface in
    !! the module spec.
    module subroutine grp_apply_proc_matrix(self, func, nout, out, threads)
        class(parquet_grouping), intent(in) :: self         !! the grouping.
        procedure(parquet_group_apply_i) :: func            !! called once per group; see the interface.
        integer, intent(in) :: nout                         !! results per group; at least 1.
        real(real64), allocatable, intent(out) :: out(:, :) !! (nout, ngroups): group g's results are out(:, g).
        integer, intent(in), optional :: threads            !! the team; absent = serial.
        character(len=*), parameter :: PROC = "apply"
        integer(int64) :: g
        integer :: nt, chunk
        !
        call grp_resolve(self, PROC)
        call grp_check_nout(nout, PROC)
        call group_team(self%ngrp, threads, PROC, .false., nt)
        allocate(out(nout, self%ngrp))
        if (nt > 1) then
            chunk = group_chunk(self%ngrp, nt)
            !$omp parallel do num_threads(nt) default(shared) private(g) schedule(dynamic, chunk)
            do g = 1_int64, self%ngrp
                call func(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64), out(:, g))
            end do
            !$omp end parallel do
        else
            do g = 1_int64, self%ngrp
                call func(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64), out(:, g))
            end do
        end if
    end subroutine grp_apply_proc_matrix
    !
    module procedure grp_apply_obj_scalar
        character(len=*), parameter :: PROC = "apply"
        integer(int64) :: g
        integer :: nt, chunk
        !
        call grp_resolve(self, PROC)
        call group_team(self%ngrp, threads, PROC, .false., nt)
        allocate(out(self%ngrp))
        if (nt > 1) then
            chunk = group_chunk(self%ngrp, nt)
            !$omp parallel do num_threads(nt) default(shared) private(g) schedule(dynamic, chunk)
            do g = 1_int64, self%ngrp
                call reducer%reduce(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64), out(g:g))
            end do
            !$omp end parallel do
        else
            do g = 1_int64, self%ngrp
                call reducer%reduce(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64), out(g:g))
            end do
        end if
    end procedure grp_apply_obj_scalar
    !
    module procedure grp_apply_obj_matrix
        character(len=*), parameter :: PROC = "apply"
        integer(int64) :: g
        integer :: nt, chunk
        !
        call grp_resolve(self, PROC)
        call grp_check_nout(nout, PROC)
        call group_team(self%ngrp, threads, PROC, .false., nt)
        allocate(out(nout, self%ngrp))
        if (nt > 1) then
            chunk = group_chunk(self%ngrp, nt)
            !$omp parallel do num_threads(nt) default(shared) private(g) schedule(dynamic, chunk)
            do g = 1_int64, self%ngrp
                call reducer%reduce(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64), out(:, g))
            end do
            !$omp end parallel do
        else
            do g = 1_int64, self%ngrp
                call reducer%reduce(g, self%perm(self%offsets(g) : self%offsets(g + 1_int64) - 1_int64), out(:, g))
            end do
        end if
    end procedure grp_apply_obj_matrix
    !
    ! ---- %nunique -------------------------------------------------------------------------------
    !
    !> A non-owning table handle over the grouping's cache, for the one table binding a
    !! grouping query needs (`%argsort_by`, which resolves and orders columns through the
    !! table). The row handle holds exactly these scalars for the same reason. **The handle's
    !! finalizer would free the cache**, so every caller nullifies `t%cache` before the handle
    !! goes out of scope; an abort in between finalizes nothing.
    subroutine grp_table_handle(self, t)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        type(parquet_table), intent(out) :: t       !! the handle; nullify its cache before leaving.
        !
        t%detached = self%scope%detached
        t%regime = self%scope%regime
        t%row_lo = self%scope%row_lo
        t%row_hi = self%scope%row_hi
        t%row_count = self%ntab
        t%cache => self%cache
    end subroutine grp_table_handle
    !
    module procedure grp_nunique_i64
        character(len=*), parameter :: PROC = "nunique"
        type(parquet_table) :: t
        character(len=:), allocatable :: names(:)
        integer(int64), allocatable :: perm(:), go(:), codes(:)
        integer(int64) :: r, rep, g
        integer :: idx, k, wid
        logical :: drop
        !
        call grp_resolve(self, PROC)
        drop = .true.
        if (present(dropna)) drop = dropna
        call grp_table_handle(self, t)
        ! Resolved through the sort's own key lookup, as the group keys were: one rule for which
        ! column can be ordered, worded for this verb; the column is read here if it is not yet.
        call table_lookup_sort_key(t, trim(name), PROC, idx, key_kind="nunique")
        wid = max(len(self%keys), len_trim(name))
        allocate(character(len=wid) :: names(size(self%keys, kind=int64) + 1_int64))
        do k = 1, size(self%keys)
            names(k) = self%keys(k)
        end do
        names(size(self%keys) + 1) = trim(name)
        ! One sort over the keys and then the column: its finest runs are the distinct values
        ! within each key tuple, by the comparator's own equality (every NaN one value, every
        ! null one value), and each run lies inside one of this grouping's groups -- the keys
        ! come first in both sorts.
        call t%argsort_by(names, perm, group_offsets=go)
        nullify(t%cache)
        call grp_group_ids_i64(self, codes)
        allocate(out(self%ngrp))
        out = 0_int64
        do r = 1_int64, size(go, kind=int64) - 1_int64
            rep = perm(go(r))
            g = codes(rep)
            if (g == 0_int64) cycle          ! a row in no group: a null key under the build's dropna
            if (drop) then
                ! The null run of the column, asked of the column (Risk-208), never of its place.
                if (parquet_column_is_null(self%cache%cols(idx)%values, rep)) cycle
            end if
            out(g) = out(g) + 1_int64
        end do
    end procedure grp_nunique_i64
    !
    module procedure grp_nunique_i32
        integer(int64), allocatable :: c64(:)
        !
        call grp_nunique_i64(self, name, c64, dropna)
        call grp_narrow(c64, "nunique", "count", out)
    end procedure grp_nunique_i32
    !
    ! ---- %agg: the vocabulary, the exact family and the per-column procedure ---------------------
    !
    !> Aborts inside a team through one critical section, so that exactly one thread aborts
    !! (.claude/rules/api-conventions.md, "Errors and diagnostics"); harmless outside one.
    subroutine agg_abort(msg)
        character(len=*), intent(in) :: msg !! the whole message.
        !
        !$omp critical (parquet_grouping_abort)
        error stop msg
        !$omp end critical (parquet_grouping_abort)
    end subroutine agg_abort
    !
    !> Turns a statistic token into its code, for one of the two vocabularies, refusing an
    !! unknown token with the whole vocabulary in the message.
    subroutine agg_parse_stat(stat, exact, proc, code)
        character(len=*), intent(in) :: stat !! the caller's token.
        logical, intent(in) :: exact         !! .true. for the int64 family.
        character(len=*), intent(in) :: proc !! calling binding, for the message.
        integer, intent(out) :: code         !! the AG_* code.
        character(len=:), allocatable :: tok
        !
        call pf_to_lower(trim(adjustl(stat)), tok)
        code = 0
        select case (tok)
        case ("size");     code = AG_SIZE
        case ("count");    code = AG_COUNT
        case ("sum");      code = AG_SUM
        case ("min");      code = AG_MIN
        case ("max");      code = AG_MAX
        case ("first");    code = AG_FIRST
        case ("last");     code = AG_LAST
        case ("nunique");  code = AG_NUNIQUE
        end select
        if (.not. exact) then
            select case (tok)
            case ("mean");     code = AG_MEAN
            case ("var");      code = AG_VAR
            case ("std");      code = AG_STD
            case ("sem");      code = AG_SEM
            case ("range");    code = AG_RANGE
            case ("median");   code = AG_MEDIAN
            case ("quantile"); code = AG_QUANTILE
            case ("iqr");      code = AG_IQR
            case ("mad");      code = AG_MAD
            end select
        end if
        if (code /= 0) return
        if (exact) then
            error stop GP // proc // ": '" // tok // "' is not a statistic of the exact int64 family, " // &
                "which is: " // AG_INT_TOKENS // "; every other statistic answers real64 -- declare " // &
                "out real(real64)"
        end if
        error stop GP // proc // ": unknown statistic '" // tok // "'; the tokens are: " // AG_REAL_TOKENS
    end subroutine agg_parse_stat
    !
    !> Refuses an option given to a token that does not take it, and `"quantile"` without its
    !! `q=`: an option that would be silently ignored is a mistake, not a default.
    subroutine agg_check_options(code, proc, q, ddof, method, scale, weighted)
        integer, intent(in) :: code                      !! the AG_* code.
        character(len=*), intent(in) :: proc             !! calling binding, for the message.
        real(real64), intent(in), optional :: q          !! the quantile's probability.
        integer, intent(in), optional :: ddof            !! degrees of freedom.
        character(len=*), intent(in), optional :: method !! the quantile method.
        character(len=*), intent(in), optional :: scale  !! the mad's scale.
        logical, intent(in) :: weighted                  !! whether weights were given.
        !
        if (code == AG_QUANTILE .and. .not. present(q)) then
            error stop GP // proc // ': "quantile" needs q=, the probability on a 0-1 scale'
        end if
        if (present(q) .and. code /= AG_QUANTILE) then
            error stop GP // proc // ': q= belongs to "quantile" alone; this statistic takes no probability'
        end if
        if (present(method)) then
            if (code /= AG_MEDIAN .and. code /= AG_QUANTILE .and. code /= AG_IQR) then
                error stop GP // proc // ': method= belongs to "median", "quantile" and "iqr"; this ' // &
                    'statistic resolves no fractional position'
            end if
        end if
        if (present(ddof)) then
            if (code /= AG_VAR .and. code /= AG_STD .and. code /= AG_SEM) then
                error stop GP // proc // ': ddof= belongs to "var", "std" and "sem"; this statistic ' // &
                    'charges no degrees of freedom'
            end if
        end if
        if (present(scale) .and. code /= AG_MAD) then
            error stop GP // proc // ': scale= belongs to "mad" alone'
        end if
        if (weighted) then
            if (code == AG_SIZE .or. code == AG_NUNIQUE .or. code == AG_FIRST .or. code == AG_LAST) then
                error stop GP // proc // ': weights have no effect on "size", "nunique", "first" or ' // &
                    '"last"; leave weights= and weight_column= off for them'
            end if
        end if
    end subroutine agg_check_options
    !
    !> Resolves the column a statistic is over and refuses every kind that has none: the scalar
    !! numeric kinds and logical are accepted (a logical as 0 and 1); `exact` narrows that to
    !! the integer kinds and logical, since a real column's statistics are real.
    subroutine agg_value_column(self, name, proc, exact, idx, kind)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        character(len=*), intent(in) :: name        !! the column.
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        logical, intent(in) :: exact                !! .true. for the int64 family.
        integer, intent(out) :: idx                 !! its slot.
        integer, intent(out) :: kind                !! its PK_* kind.
        character(len=:), allocatable :: kname, sfx
        !
        call grp_resolve_column(self, name, proc, idx)
        kind = self%cache%cols(idx)%values%kindof()
        select case (kind)
        case (PK_INT32, PK_INT64, PK_LOGICAL)
            return
        case (PK_FLOAT32, PK_FLOAT64)
            if (.not. exact) return
            call parquet_kind_name(kind, kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop GP // proc // ": the exact int64 family takes an integer or logical column, and " // &
                "this is a " // kname // " column; its statistics are real64 -- declare out real(real64)" // sfx
        end select
        call parquet_kind_name(kind, kname)
        call table_context_suffix(self%cache, name, sfx)
        error stop GP // proc // ": a " // kname // " column has no numeric statistics; %agg takes a " // &
            "scalar numeric or logical column -- for the first or last value of any kind use " // &
            "%first_rows or %last_rows with %get_slice, for the counts %count and %nunique" // sfx
    end subroutine agg_value_column
    !
    !> Resolves `weights=` or `weight_column=` into one real64 weight per TABLE row, or leaves
    !! `wall` unallocated when neither was given. Both together are refused; a weight column is
    !! a scalar numeric one, widened as `%get` widens it, a null weight zero (a missing
    !! membership probability is no membership; answer F3 of feature_pandas_S5.md). Then every
    !! GROUPED row's weight is checked once, here, serially: a NaN, negative or infinite weight
    !! aborts naming its table row -- the statistics module's own rule, applied before the loop
    !! so that the abort is one and names a row of the table rather than a position in a buffer.
    subroutine agg_weights(self, proc, weights, weight_column, wall)
        class(parquet_grouping), intent(in) :: self             !! the grouping.
        character(len=*), intent(in) :: proc                    !! calling binding, for the message.
        real(real64), intent(in), optional :: weights(:)        !! one per table row.
        character(len=*), intent(in), optional :: weight_column !! a column of weights.
        real(real64), allocatable, intent(out) :: wall(:)       !! one per table row; unallocated when unweighted.
        character(len=:), allocatable :: kname, sfx
        integer(int32), pointer :: pi32(:)
        integer(int64), pointer :: pi64(:)
        real(real32), pointer :: pf32(:)
        real(real64), pointer :: pf64(:)
        character(len=32) :: got, want, rtxt
        integer(int64) :: i, r
        integer :: widx, wkind
        real(real64) :: w
        !
        if (present(weights) .and. present(weight_column)) then
            error stop GP // proc // ": weights= and weight_column= were both given; the population " // &
                "has one weight per row, so give one of the two"
        end if
        if (present(weights)) then
            if (size(weights, kind=int64) /= self%ntab) then
                write(got, "(I0)") size(weights, kind=int64)
                write(want, "(I0)") self%ntab
                error stop GP // proc // ": weights has " // trim(got) // " entries but the table has " // &
                    trim(want) // " rows; weights are per table row, not per group"
            end if
            allocate(wall(self%ntab))
            wall = weights
        else if (present(weight_column)) then
            call grp_resolve_column(self, weight_column, proc, widx)
            wkind = self%cache%cols(widx)%values%kindof()
            allocate(wall(self%ntab))
            select case (wkind)
            case (PK_INT32)
                call parquet_column_data_ptr(self%cache%cols(widx)%values, pi32)
                wall = real(pi32(1:self%ntab), real64)
            case (PK_INT64)
                call parquet_column_data_ptr(self%cache%cols(widx)%values, pi64)
                wall = real(pi64(1:self%ntab), real64)
            case (PK_FLOAT32)
                call parquet_column_data_ptr(self%cache%cols(widx)%values, pf32)
                wall = real(pf32(1:self%ntab), real64)
            case (PK_FLOAT64)
                call parquet_column_data_ptr(self%cache%cols(widx)%values, pf64)
                wall = pf64(1:self%ntab)
            case default
                call parquet_kind_name(wkind, kname)
                call table_context_suffix(self%cache, weight_column, sfx)
                error stop GP // proc // ": weight_column= names a " // kname // " column; a weight " // &
                    "column is a scalar numeric one" // sfx
            end select
            if (parquet_column_any_null(self%cache%cols(widx)%values)) then
                do i = 1_int64, self%ntab
                    if (parquet_column_is_null(self%cache%cols(widx)%values, i)) wall(i) = 0.0_real64
                end do
            end if
        else
            return
        end if
        do i = 1_int64, self%nin
            r = self%perm(i)
            w = wall(r)
            ! Per row, so the hot-path NaN test (`x /= x`, .claude/rules/fortran-gotchas.md); the
            ! -Wcompare-reals hit on it is the accepted one.
            if (w /= w) then
                write(rtxt, "(I0)") r
                error stop GP // proc // ": the weight of row " // trim(rtxt) // " is NaN; weights must be " // &
                    "finite and non-negative"
            end if
            if (w < 0.0_real64) then
                write(rtxt, "(I0)") r
                error stop GP // proc // ": the weight of row " // trim(rtxt) // " is negative; weights " // &
                    "must be finite and non-negative"
            end if
            if (w > huge(w)) then
                write(rtxt, "(I0)") r
                error stop GP // proc // ": the weight of row " // trim(rtxt) // " is infinite; weights " // &
                    "must be finite and non-negative"
            end if
        end do
    end subroutine agg_weights
    !
    !> Gathers one group's values of the column into `v(1:n)`, widened to real64 in row order,
    !! its validity into `m(1:n)` when the column holds any null (and says whether this GROUP
    !! does), and its weights into `w(1:n)` when there are any. The buffers are per thread and
    !! sized once; nothing is allocated here.
    subroutine agg_gather(self, idx, kind, anynull, wall, g, v, m, w, n, has_null)
        class(parquet_grouping), intent(in) :: self   !! the grouping.
        integer, intent(in) :: idx                    !! the column's slot.
        integer, intent(in) :: kind                   !! its PK_* kind, one of the five accepted.
        logical, intent(in) :: anynull                !! whether the column holds any null at all.
        real(real64), allocatable, intent(in) :: wall(:) !! per-row weights, or unallocated.
        integer(int64), intent(in) :: g               !! the group.
        real(real64), intent(inout) :: v(:)           !! receives the values in v(1:n).
        logical, intent(inout) :: m(:)                !! receives the validity in m(1:n) when anynull.
        real(real64), intent(inout) :: w(:)           !! receives the weights in w(1:n) when weighted.
        integer(int64), intent(out) :: n              !! the group's size.
        logical, intent(out) :: has_null              !! whether the group holds a null.
        integer(int32), pointer :: pi32(:)
        integer(int64), pointer :: pi64(:)
        real(real32), pointer :: pf32(:)
        real(real64), pointer :: pf64(:)
        logical, pointer :: pb(:)
        integer(int64) :: lo, hi, i
        !
        lo = self%offsets(g)
        hi = self%offsets(g + 1_int64) - 1_int64
        n = hi - lo + 1_int64
        select case (kind)
        case (PK_INT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pi32)
            v(1:n) = real(pi32(self%perm(lo:hi)), real64)
        case (PK_INT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pi64)
            v(1:n) = real(pi64(self%perm(lo:hi)), real64)
        case (PK_FLOAT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pf32)
            v(1:n) = real(pf32(self%perm(lo:hi)), real64)
        case (PK_FLOAT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pf64)
            v(1:n) = pf64(self%perm(lo:hi))
        case (PK_LOGICAL)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pb)
            v(1:n) = merge(1.0_real64, 0.0_real64, pb(self%perm(lo:hi)))
        end select
        has_null = .false.
        if (anynull) then
            do i = 1_int64, n
                m(i) = .not. parquet_column_is_null(self%cache%cols(idx)%values, self%perm(lo + i - 1_int64))
            end do
            has_null = .not. all(m(1:n))
        end if
        if (allocated(wall)) w(1:n) = wall(self%perm(lo:hi))
    end subroutine agg_gather
    !
    !> One statistic over one group's gathered values: the named `parquet_stats` procedure, with
    !! `is_valid`/`weights` passed on exactly as they arrived here (an absent optional stays
    !! absent), so the four presence combinations are decided once by the caller.
    subroutine agg_eval(code, v, r, is_valid, weights, q, ddof, method, scale)
        integer, intent(in) :: code                       !! the AG_* code; never FIRST, LAST or NUNIQUE.
        real(real64), intent(in) :: v(:)                  !! the group's values.
        real(real64), intent(out) :: r                    !! the statistic.
        logical, intent(in), optional :: is_valid(:)      !! present when the group holds a null.
        real(real64), intent(in), optional :: weights(:)  !! present when weighted.
        real(real64), intent(in), optional :: q           !! the quantile's probability.
        integer, intent(in), optional :: ddof             !! degrees of freedom.
        character(len=*), intent(in), optional :: method  !! the quantile method.
        character(len=*), intent(in), optional :: scale   !! the mad's scale.
        real(real64) :: lo, hi
        integer(int64) :: cnt
        !
        select case (code)
        case (AG_SIZE)
            r = real(size(v, kind=int64), real64)
        case (AG_COUNT)
            call pf_count_valid(v, cnt, is_valid=is_valid, weights=weights)
            r = real(cnt, real64)
        case (AG_SUM)
            call pf_sum(v, r, is_valid=is_valid, weights=weights)
        case (AG_MEAN)
            call pf_mean(v, r, is_valid=is_valid, weights=weights)
        case (AG_VAR)
            call pf_variance(v, r, is_valid=is_valid, weights=weights, ddof=ddof)
        case (AG_STD)
            call pf_stddev(v, r, is_valid=is_valid, weights=weights, ddof=ddof)
        case (AG_SEM)
            call pf_sem(v, r, is_valid=is_valid, weights=weights, ddof=ddof)
        case (AG_MIN)
            call pf_moments(v, vmin=r, is_valid=is_valid, weights=weights)
        case (AG_MAX)
            call pf_moments(v, vmax=r, is_valid=is_valid, weights=weights)
        case (AG_RANGE)
            call pf_moments(v, vmin=lo, vmax=hi, is_valid=is_valid, weights=weights)
            r = hi - lo
        case (AG_MEDIAN)
            call pf_median(v, r, is_valid=is_valid, weights=weights, method=method)
        case (AG_QUANTILE)
            call pf_quantile(v, q, r, is_valid=is_valid, weights=weights, method=method)
        case (AG_IQR)
            call pf_iqr(v, r, is_valid=is_valid, weights=weights, method=method)
        case (AG_MAD)
            call pf_mad(v, r, is_valid=is_valid, weights=weights, scale=scale)
        case default
            ! FIRST, LAST and NUNIQUE are answered by their callers; no token code reaches here.
            error stop GP // "agg: internal: no procedure for this statistic code"   ! GCOVR_EXCL_LINE
        end select
    end subroutine agg_eval
    !
    !> One group's real64 statistic from its gathered buffers: `"first"` and `"last"` are the
    !! first and last non-null value in row order (NaN when there is none, as pandas answers);
    !! everything else is `agg_eval` with the presence of `is_valid` and `weights` decided here.
    subroutine agg_one(code, v, m, w, n, has_null, weighted, q, ddof, method, scale, r)
        integer, intent(in) :: code                      !! the AG_* code.
        real(real64), intent(in) :: v(:)                 !! the values, v(1:n).
        logical, intent(in) :: m(:)                      !! the validity, m(1:n), meaningful when has_null.
        real(real64), intent(in) :: w(:)                 !! the weights, w(1:n), meaningful when weighted.
        integer(int64), intent(in) :: n                  !! the group's size.
        logical, intent(in) :: has_null                  !! whether the group holds a null.
        logical, intent(in) :: weighted                  !! whether weights were given.
        real(real64), intent(in), optional :: q          !! the quantile's probability.
        integer, intent(in), optional :: ddof            !! degrees of freedom.
        character(len=*), intent(in), optional :: method !! the quantile method.
        character(len=*), intent(in), optional :: scale  !! the mad's scale.
        real(real64), intent(out) :: r                   !! the statistic.
        integer(int64) :: i
        !
        if (code == AG_FIRST .or. code == AG_LAST) then
            r = ieee_value(r, ieee_quiet_nan)
            if (.not. has_null) then
                if (code == AG_FIRST) then
                    r = v(1)
                else
                    r = v(n)
                end if
                return
            end if
            if (code == AG_FIRST) then
                do i = 1_int64, n
                    if (m(i)) then
                        r = v(i)
                        return
                    end if
                end do
            else
                do i = n, 1_int64, -1_int64
                    if (m(i)) then
                        r = v(i)
                        return
                    end if
                end do
            end if
            return
        end if
        if (has_null) then
            if (weighted) then
                call agg_eval(code, v(1:n), r, is_valid=m(1:n), weights=w(1:n), q=q, ddof=ddof, method=method, &
                    scale=scale)
            else
                call agg_eval(code, v(1:n), r, is_valid=m(1:n), q=q, ddof=ddof, method=method, scale=scale)
            end if
        else if (weighted) then
            call agg_eval(code, v(1:n), r, weights=w(1:n), q=q, ddof=ddof, method=method, scale=scale)
        else
            call agg_eval(code, v(1:n), r, q=q, ddof=ddof, method=method, scale=scale)
        end if
    end subroutine agg_one
    !
    !> One group's value from the caller's per-column procedure, with `is_valid` and `weights`
    !! present exactly when the group holds a null and when weights were given.
    subroutine agg_call_func(func, v, m, w, n, has_null, weighted, r)
        procedure(parquet_group_column_reduce_i) :: func !! the caller's procedure.
        real(real64), intent(in) :: v(:)                 !! the values, v(1:n).
        logical, intent(in) :: m(:)                      !! the validity, m(1:n), meaningful when has_null.
        real(real64), intent(in) :: w(:)                 !! the weights, w(1:n), meaningful when weighted.
        integer(int64), intent(in) :: n                  !! the group's size.
        logical, intent(in) :: has_null                  !! whether the group holds a null.
        logical, intent(in) :: weighted                  !! whether weights were given.
        real(real64), intent(out) :: r                   !! the procedure's answer.
        !
        if (has_null) then
            if (weighted) then
                r = func(v(1:n), is_valid=m(1:n), weights=w(1:n))
            else
                r = func(v(1:n), is_valid=m(1:n))
            end if
        else if (weighted) then
            r = func(v(1:n), weights=w(1:n))
        else
            r = func(v(1:n))
        end if
    end subroutine agg_call_func
    !
    module procedure grp_agg_stat_real
        character(len=*), parameter :: PROC = "agg"
        real(real64), allocatable :: wall(:), v0(:), w0(:)
        logical, allocatable :: m0(:)
        integer(int64), allocatable :: nu(:)
        integer(int64) :: g, n, cap
        integer :: code, idx, kind, nt, chunk
        logical :: anynull, weighted, has_null
        !
        call grp_resolve(self, PROC)
        call agg_parse_stat(stat, .false., PROC, code)
        call agg_value_column(self, name, PROC, .false., idx, kind)
        call agg_weights(self, PROC, weights, weight_column, wall)
        weighted = allocated(wall)
        call agg_check_options(code, PROC, q, ddof, method, scale, weighted)
        if (code == AG_NUNIQUE) then
            ! The one token with its own engine: the sort's, through %nunique.
            call grp_nunique_i64(self, name, nu)
            allocate(out(self%ngrp))
            out = real(nu, real64)
            call group_note_threads(1)
            return
        end if
        call group_team(self%ngrp, threads, PROC, .true., nt)
        allocate(out(self%ngrp))
        anynull = parquet_column_any_null(self%cache%cols(idx)%values)
        cap = max(self%maxsz, 1_int64)
        allocate(v0(cap), m0(cap), w0(cap))
        if (nt > 1 .and. self%ngrp > 1_int64) then
            ! The first group BEFORE the team opens (see the header): an argument the statistics
            ! module refuses aborts here, serially, naming itself.
            call agg_gather(self, idx, kind, anynull, wall, 1_int64, v0, m0, w0, n, has_null)
            call agg_one(code, v0, m0, w0, n, has_null, weighted, q, ddof, method, scale, out(1))
            chunk = group_chunk(self%ngrp, nt)
            !$omp parallel num_threads(nt) default(shared) private(g, n, has_null)
            block
                real(real64), allocatable :: v(:), w(:)
                logical, allocatable :: m(:)
                ! Per thread, sized once: plain arrays, so neither the finalizer rule nor the
                ! block rule of .claude/rules/fortran-gotchas.md applies.
                allocate(v(cap), m(cap), w(cap))
                !$omp do schedule(dynamic, chunk)
                do g = 2_int64, self%ngrp
                    call agg_gather(self, idx, kind, anynull, wall, g, v, m, w, n, has_null)
                    call agg_one(code, v, m, w, n, has_null, weighted, q, ddof, method, scale, out(g))
                end do
                !$omp end do
            end block
            !$omp end parallel
        else
            do g = 1_int64, self%ngrp
                call agg_gather(self, idx, kind, anynull, wall, g, v0, m0, w0, n, has_null)
                call agg_one(code, v0, m0, w0, n, has_null, weighted, q, ddof, method, scale, out(g))
            end do
        end if
    end procedure grp_agg_stat_real
    !
    module procedure grp_agg_stat_int
        character(len=*), parameter :: PROC = "agg"
        integer(int32), pointer :: pi32(:)
        integer(int64), pointer :: pi64(:)
        logical, pointer :: pb(:)
        integer(int64) :: g, i, r, x, acc, cnt
        integer :: code, idx, kind, nt, chunk
        logical :: anynull, seen
        character(len=32) :: gtxt
        !
        call grp_resolve(self, PROC)
        call agg_parse_stat(stat, .true., PROC, code)
        call agg_value_column(self, name, PROC, .true., idx, kind)
        select case (code)
        case (AG_NUNIQUE)
            call grp_nunique_i64(self, name, out)
            call group_note_threads(1)
            return
        case (AG_SIZE)
            call grp_size_i64(self, out)
            call group_note_threads(1)
            return
        case (AG_COUNT)
            call grp_count_i64(self, name, out)
            call group_note_threads(1)
            return
        end select
        call group_team(self%ngrp, threads, PROC, .true., nt)
        allocate(out(self%ngrp))
        anynull = parquet_column_any_null(self%cache%cols(idx)%values)
        nullify(pi32, pi64, pb)
        select case (kind)
        case (PK_INT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pi32)
        case (PK_INT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pi64)
        case (PK_LOGICAL)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, pb)
        end select
        chunk = group_chunk(self%ngrp, nt)
        ! One walk per group over its rows in row order, nulls skipped: a running sum with the
        ! overflow test made before each addition (never on a wrapped result), a running
        ! min/max, the first or last non-null value. `seen` says whether any non-null row was
        ! met, which "min", "max", "first" and "last" need at least one of.
        !$omp parallel do num_threads(nt) default(shared) private(g, i, r, x, acc, cnt, seen, gtxt) &
        !$omp schedule(dynamic, chunk) if (nt > 1)
        do g = 1_int64, self%ngrp
            acc = 0_int64
            cnt = 0_int64
            seen = .false.
            do i = self%offsets(g), self%offsets(g + 1_int64) - 1_int64
                r = self%perm(i)
                if (anynull) then
                    if (parquet_column_is_null(self%cache%cols(idx)%values, r)) cycle
                end if
                if (associated(pi32)) then
                    x = int(pi32(r), int64)
                else if (associated(pi64)) then
                    x = pi64(r)
                else
                    x = merge(1_int64, 0_int64, pb(r))
                end if
                select case (code)
                case (AG_SUM)
                    if (x > 0_int64) then
                        if (acc > huge(acc) - x) call agg_overflow(g)
                    else if (x < 0_int64) then
                        if (acc < (-huge(acc) - x) - 1_int64) call agg_overflow(g)
                    end if
                    acc = acc + x
                case (AG_MIN)
                    if (.not. seen .or. x < acc) acc = x
                case (AG_MAX)
                    if (.not. seen .or. x > acc) acc = x
                case (AG_FIRST)
                    if (.not. seen) acc = x
                case (AG_LAST)
                    acc = x
                end select
                seen = .true.
                cnt = cnt + 1_int64
            end do
            if (.not. seen .and. code /= AG_SUM) then
                write(gtxt, "(I0)") g
                call agg_abort(GP // PROC // ": group " // trim(gtxt) // " has no non-null value of '" // &
                    trim(name) // "', so its exact " // trim(stat) // " does not exist; the real64 form " // &
                    "answers NaN there")
            end if
            out(g) = acc
        end do
        !$omp end parallel do
    contains
        !> The overflow abort of the exact sum, naming the group.
        subroutine agg_overflow(gg)
            integer(int64), intent(in) :: gg !! the group.
            character(len=32) :: t
            write(t, "(I0)") gg
            call agg_abort(GP // PROC // ": the int64 sum of '" // trim(name) // "' over group " // trim(t) // &
                " overflows; it is refused rather than wrapped -- use the real64 form for an " // &
                "approximate sum")
        end subroutine agg_overflow
    end procedure grp_agg_stat_int
    !
    !> %agg specific, the caller's per-column procedure; the contract is on the interface in the
    !! module spec. Restated in full for the reason the two procedure-form `%apply` bodies are.
    module subroutine grp_agg_func(self, name, func, out, weights, weight_column, threads)
        class(parquet_grouping), intent(in) :: self             !! the grouping.
        character(len=*), intent(in) :: name                    !! the column; read if not resident.
        procedure(parquet_group_column_reduce_i) :: func        !! called once per group; see the interface.
        real(real64), allocatable, intent(out) :: out(:)        !! one value per group, in group order.
        real(real64), intent(in), optional :: weights(:)        !! one weight per table row.
        character(len=*), intent(in), optional :: weight_column !! a scalar numeric column of weights.
        integer, intent(in), optional :: threads                !! the team; absent = serial.
        character(len=*), parameter :: PROC = "agg"
        real(real64), allocatable :: wall(:), v0(:), w0(:)
        logical, allocatable :: m0(:)
        integer(int64) :: g, n, cap
        integer :: idx, kind, nt, chunk
        logical :: anynull, weighted, has_null
        !
        call grp_resolve(self, PROC)
        call agg_value_column(self, name, PROC, .false., idx, kind)
        call agg_weights(self, PROC, weights, weight_column, wall)
        weighted = allocated(wall)
        call group_team(self%ngrp, threads, PROC, .false., nt)
        allocate(out(self%ngrp))
        anynull = parquet_column_any_null(self%cache%cols(idx)%values)
        cap = max(self%maxsz, 1_int64)
        if (nt > 1) then
            chunk = group_chunk(self%ngrp, nt)
            !$omp parallel num_threads(nt) default(shared) private(g, n, has_null)
            block
                real(real64), allocatable :: v(:), w(:)
                logical, allocatable :: m(:)
                allocate(v(cap), m(cap), w(cap))
                !$omp do schedule(dynamic, chunk)
                do g = 1_int64, self%ngrp
                    call agg_gather(self, idx, kind, anynull, wall, g, v, m, w, n, has_null)
                    call agg_call_func(func, v, m, w, n, has_null, weighted, out(g))
                end do
                !$omp end do
            end block
            !$omp end parallel
        else
            allocate(v0(cap), m0(cap), w0(cap))
            do g = 1_int64, self%ngrp
                call agg_gather(self, idx, kind, anynull, wall, g, v0, m0, w0, n, has_null)
                call agg_call_func(func, v0, m0, w0, n, has_null, weighted, out(g))
            end do
        end if
    end subroutine grp_agg_func
    !
end submodule parquet_tables_group
