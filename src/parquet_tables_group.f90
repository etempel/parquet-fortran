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
!! sort placed the group -- the second half of the same rule, for several keys: the null group is
!! last today, and a version that assumed so would break the day a `nulls_first` reaches this verb.
!!
!! **The generation is compared on EVERY per-group query, never cached**, restated here for this
!! object. A stale grouping hands out in-range row numbers naming the wrong rows, and anything
!! computed from those is a plausible answer about the wrong rows; one `integer(int64)` comparison
!! per query is the whole cost of not having it. The five introspection bindings describe the object
!! itself and skip it, as their interfaces say.
!!
!! **`%first_rows`/`%last_rows` are computed, not read off the permutation's ends** -- the choice
!! `%drop_duplicates` made (`src/parquet_tables_verbs.f90`, header), so that "first" does not rest
!! on the engine's tie rule.
!!
!! **`%key_table` gathers COPIES**: a gather on the source column would reorder the caller's
!! table with nothing to show it.
!!
!! **`%apply` calls the caller's procedure once per group with the group's rows and nothing
!! else**, in group order when serial, and stores each result at its own `g` when on a team, so
!! the answer does not depend on the schedule. The loop is SERIAL unless `threads=` is given:
!! the library cannot know whether a caller's procedure is re-entrant, so the serial default is
!! a decision made at the call (`group_team`), not a floor that declined, and an explicit
!! request is the caller's declaration. The team is recorded on every call for the test-only
!! hook, because a loop that resolved a team it never opened, or opened one nobody asked for,
!! is invisible in every answer.
!!
!! **`%agg` is `parquet_stats` called once per group, and nothing else.** Every token delegates
!! to the named `pf_*` procedure over the group's values gathered into a real64 buffer -- the
!! widening that module applies to every kind itself -- so a group's answer is that procedure's
!! answer over the same rows, bit for bit, whatever the thread count; a second moment or
!! quantile engine here is the copy the one-engine rule forbids. Validity is passed as `is_valid=`
!! only when the group holds a null and weights only when the caller gave some, so a null-free,
!! unweighted group takes each procedure's fast path. The exact `int64` family never touches the
!! buffer. The first group of a threaded loop is computed BEFORE the team opens, so an argument the
!! statistics module refuses (an unknown `method=`, a `q=` outside 0..1) aborts serially and names
!! itself; the data-dependent aborts inside the team (an `int64` sum overflowing, a group with no
!! exact answer) go through one `critical`.
!!
!! **`%nunique` is one more sort**, over the keys and then the column, so that distinctness is
!! the comparator's own equality and its outer groups are this grouping's; a walk over the
!! finest runs counts them per group, skipping the null run unless `dropna=.false.`.
!!
!! **`%broadcast` and `%gather` are the two ends of the hot loop, and neither allocates per
!! group.** `%broadcast` is one fill of `per_row` and one scatter over the partition: every
!! grouped row is written exactly once, by its own group, and the fill lands before the team
!! opens, so the loop threads automatically with nothing to order and the same bits at every
!! team. `%gather` copies one group's run of the permutation into a buffer the caller sized once
!! by `%max_size()`, widening as `%get` does, and ABORTS rather than truncates when the buffer is
!! shorter than the group: a statistic over the first `size(buf)` rows of a group is a plausible
!! wrong answer.
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
    ! (tools/module_footprints.txt).
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
                ! equal -- and never inferred from the sort's null placement.
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
    !! the check is one integer comparison.
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
    !> The scalar twin of `grp_narrow`, for `%gather`'s `n`: the same refusal, one value. Kept
    !! beside it so that the two texts cannot drift apart.
    subroutine grp_narrow_scalar(v64, proc, what, v32)
        integer(int64), intent(in) :: v64       !! the answer.
        character(len=*), intent(in) :: proc    !! calling binding, for the message.
        character(len=*), intent(in) :: what    !! "row", "count" or "group number", for the message.
        integer(int32), intent(out) :: v32      !! the same answer.
        character(len=32) :: txt
        !
        if (v64 > int(huge(0_int32), int64)) then
            ! No test-sized fixture reaches this: it needs a group above 2**31 rows.
            ! GCOVR_EXCL_START
            write(txt, "(I0)") v64
            error stop GP // trim(proc) // ": " // what // " " // trim(txt) // " does not " // &
                "fit the int32 answer asked for; use an int64 variable"
            ! GCOVR_EXCL_STOP
        end if
        v32 = int(v64, int32)
    end subroutine grp_narrow_scalar
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
        n = 0
        if (allocated(self%slots)) n = size(self%slots)
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
    ! The `int32` forms below compute NOTHING: each widens `g` with `int(g, int64)` BEFORE the
    ! range check -- so a refusal prints the number the caller wrote -- forwards every other
    ! argument by keyword, and narrows the answer through `grp_narrow` or its scalar twin, which
    ! abort naming the value rather than wrapping. One walk, one set of rules, whichever kind was
    ! asked for.
    !
    module procedure grp_rows_g32_i64
        call grp_rows_i64(self, int(g, int64), rows)
    end procedure grp_rows_g32_i64
    !
    module procedure grp_rows_g32_i32
        call grp_rows_i32(self, int(g, int64), rows)
    end procedure grp_rows_g32_i32
    !
    module procedure grp_csr_i32
        integer(int64), allocatable :: o64(:), r64(:)
        !
        call grp_csr(self, o64, r64)
        call grp_narrow(o64, "csr", "offset", offsets)
        call grp_narrow(r64, "csr", "row", rows)
    end procedure grp_csr_i32
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
        character(len=32) :: rtxt
        integer :: k, nsize
        !
        call grp_resolve(self, PROC)
        if (present(reserve)) then
            if (reserve < 0) then
                write(rtxt, "(I0)") reserve
                error stop GP // PROC // ": reserve= must be at least 0, got " // trim(rtxt)
            end if
        end if
        nsize = 0
        if (present(size_name)) then
            nsize = 1
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
        ! The reservation is made BEFORE the first add and over the TOTAL this call will reach,
        ! `reserve` being the caller's INCREMENT on top of it: %reserve_columns takes a total and
        ! is a no-op at or below the current capacity, so the key adds themselves never relocate
        ! either, and the caller's next `reserve` adds cannot.
        if (present(reserve)) call out%reserve_columns(size(self%slots) + nsize + reserve)
        do k = 1, size(self%slots)
            ! A copy, then a gather on the copy: the source column must come out of this
            ! unchanged. The gather carries each row's null state with it, which is how a
            ! `dropna=.false.` null group's key comes out null without this code knowing how
            ! the kind stores one.
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
    !! "declined" from "ran".
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
        call apply_proc_matrix_impl(self, func, nout, out, "apply", threads)
    end subroutine grp_apply_proc_matrix
    !
    !> The matrix `%apply` over a procedure, with the CALLING BINDING's name as an argument
    !! instead of a parameter of its own, so that the one body serves every binding that runs
    !! this loop and each refusal names the binding the caller actually wrote.
    subroutine apply_proc_matrix_impl(self, func, nout, out, proc, threads)
        class(parquet_grouping), intent(in) :: self         !! the grouping.
        procedure(parquet_group_apply_i) :: func            !! called once per group; see the interface.
        integer, intent(in) :: nout                         !! results per group; at least 1.
        real(real64), allocatable, intent(out) :: out(:, :) !! (nout, ngroups): group g's results are out(:, g).
        character(len=*), intent(in) :: proc                !! calling binding, for every message.
        integer, intent(in), optional :: threads            !! the team; absent = serial.
        integer(int64) :: g
        integer :: nt, chunk
        !
        call grp_resolve(self, proc)
        call grp_check_nout(nout, proc)
        call group_team(self%ngrp, threads, proc, .false., nt)
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
    end subroutine apply_proc_matrix_impl
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
        call apply_obj_matrix_impl(self, reducer, nout, out, "apply", threads)
    end procedure grp_apply_obj_matrix
    !
    !> The matrix `%apply` over a reducer object, with the calling binding's name as an
    !! argument; the procedure form's twin, for the reason given there.
    subroutine apply_obj_matrix_impl(self, reducer, nout, out, proc, threads)
        class(parquet_grouping), intent(in) :: self         !! the grouping.
        class(parquet_group_reducer), intent(in) :: reducer !! the caller's extension; its %reduce is called once per group.
        integer, intent(in) :: nout                         !! results per group; at least 1.
        real(real64), allocatable, intent(out) :: out(:, :) !! (nout, ngroups): group g's results are out(:, g).
        character(len=*), intent(in) :: proc                !! calling binding, for every message.
        integer, intent(in), optional :: threads            !! the team; absent = serial.
        integer(int64) :: g
        integer :: nt, chunk
        !
        call grp_resolve(self, proc)
        call grp_check_nout(nout, proc)
        call group_team(self%ngrp, threads, proc, .false., nt)
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
    end subroutine apply_obj_matrix_impl
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
                ! The null run of the column, asked of the column, never of its place.
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
    !! membership probability is no membership; answer F3). Then every GROUPED row's weight is
    !! checked once, here, serially: a NaN, negative or infinite weight aborts naming its table
    !! row -- the statistics module's own rule, applied before the loop so that the abort is one
    !! and names a row of the table rather than a position in a buffer.
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
        call agg_stat_real_impl(self, name, stat, out, "agg", weights, weight_column, q, ddof, method, scale, threads)
    end procedure grp_agg_stat_real
    !
    !> One token of the real64 vocabulary per group, with the CALLING BINDING's name as an
    !! argument instead of a parameter of its own, so that the one body serves every binding
    !! that asks for a statistic and each refusal names the binding the caller actually wrote.
    subroutine agg_stat_real_impl(self, name, stat, out, proc, weights, weight_column, q, ddof, method, scale, threads)
        class(parquet_grouping), intent(in) :: self             !! the grouping.
        character(len=*), intent(in) :: name                    !! the column; read if not resident.
        character(len=*), intent(in) :: stat                    !! the statistic's token, case-insensitive.
        real(real64), allocatable, intent(out) :: out(:)        !! one value per group, in group order.
        character(len=*), intent(in) :: proc                    !! calling binding, for every message.
        real(real64), intent(in), optional :: weights(:)        !! one weight per table row.
        character(len=*), intent(in), optional :: weight_column !! a scalar numeric column of weights.
        real(real64), intent(in), optional :: q                 !! the probability, 0 to 1, for "quantile".
        integer, intent(in), optional :: ddof                   !! degrees of freedom charged; default 1.
        character(len=*), intent(in), optional :: method        !! the quantile method, as pf_quantile's.
        character(len=*), intent(in), optional :: scale         !! "raw" for an unscaled "mad", as pf_mad's.
        integer, intent(in), optional :: threads                !! the team; absent = automatic.
        real(real64), allocatable :: wall(:), v0(:), w0(:)
        logical, allocatable :: m0(:)
        integer(int64), allocatable :: nu(:)
        integer(int64) :: g, n, cap
        integer :: code, idx, kind, nt, chunk
        logical :: anynull, weighted, has_null
        !
        call grp_resolve(self, proc)
        call agg_parse_stat(stat, .false., proc, code)
        call agg_value_column(self, name, proc, .false., idx, kind)
        call agg_weights(self, proc, weights, weight_column, wall)
        weighted = allocated(wall)
        call agg_check_options(code, proc, q, ddof, method, scale, weighted)
        if (code == AG_NUNIQUE) then
            ! The one token with its own engine: the sort's, through %nunique.
            call grp_nunique_i64(self, name, nu)
            allocate(out(self%ngrp))
            out = real(nu, real64)
            call group_note_threads(1)
            return
        end if
        call group_team(self%ngrp, threads, proc, .true., nt)
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
    end subroutine agg_stat_real_impl
    !
    module procedure grp_agg_stat_int
        call agg_stat_int_impl(self, name, stat, out, "agg", threads)
    end procedure grp_agg_stat_int
    !
    !> The exact `int64` family per group, with the calling binding's name as an argument; the
    !! real64 form's twin, for the reason given there.
    subroutine agg_stat_int_impl(self, name, stat, out, proc, threads)
        class(parquet_grouping), intent(in) :: self        !! the grouping.
        character(len=*), intent(in) :: name               !! the column; read if not resident.
        character(len=*), intent(in) :: stat               !! the statistic's token, case-insensitive.
        integer(int64), allocatable, intent(out) :: out(:) !! one exact value per group, in group order.
        character(len=*), intent(in) :: proc               !! calling binding, for every message.
        integer, intent(in), optional :: threads           !! the team; absent = automatic.
        integer(int32), pointer :: pi32(:)
        integer(int64), pointer :: pi64(:)
        logical, pointer :: pb(:)
        integer(int64) :: g, i, r, x, acc, cnt
        integer :: code, idx, kind, nt, chunk
        logical :: anynull, seen
        character(len=32) :: gtxt
        !
        call grp_resolve(self, proc)
        call agg_parse_stat(stat, .true., proc, code)
        call agg_value_column(self, name, proc, .true., idx, kind)
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
        call group_team(self%ngrp, threads, proc, .true., nt)
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
                call agg_abort(GP // trim(proc) // ": group " // trim(gtxt) // " has no non-null value of '" // &
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
            call agg_abort(GP // trim(proc) // ": the int64 sum of '" // trim(name) // "' over group " // trim(t) // &
                " overflows; it is refused rather than wrapped -- use the real64 form for an " // &
                "approximate sum")
        end subroutine agg_overflow ! GCOVR_EXCL_LINE -- unreachable: agg_abort never returns.
    end subroutine agg_stat_int_impl
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
        call agg_func_impl(self, name, func, out, "agg", weights, weight_column, threads)
    end subroutine grp_agg_func
    !
    !> The caller's per-column procedure per group, with the CALLING BINDING's name as an
    !! argument instead of a parameter of its own, so that the one body serves every binding
    !! that takes such a procedure and each refusal names the binding the caller actually wrote.
    subroutine agg_func_impl(self, name, func, out, proc, weights, weight_column, threads)
        class(parquet_grouping), intent(in) :: self             !! the grouping.
        character(len=*), intent(in) :: name                    !! the column; read if not resident.
        procedure(parquet_group_column_reduce_i) :: func        !! called once per group; see the interface.
        real(real64), allocatable, intent(out) :: out(:)        !! one value per group, in group order.
        character(len=*), intent(in) :: proc                    !! calling binding, for every message.
        real(real64), intent(in), optional :: weights(:)        !! one weight per table row.
        character(len=*), intent(in), optional :: weight_column !! a scalar numeric column of weights.
        integer, intent(in), optional :: threads                !! the team; absent = serial.
        real(real64), allocatable :: wall(:), v0(:), w0(:)
        logical, allocatable :: m0(:)
        integer(int64) :: g, n, cap
        integer :: idx, kind, nt, chunk
        logical :: anynull, weighted, has_null
        !
        call grp_resolve(self, proc)
        call agg_value_column(self, name, proc, .false., idx, kind)
        call agg_weights(self, proc, weights, weight_column, wall)
        weighted = allocated(wall)
        call group_team(self%ngrp, threads, proc, .false., nt)
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
    end subroutine agg_func_impl
    !
    ! ---- %add_agg, %add_apply and %add_size: the per-group answer as a COLUMN -------------------
    !
    ! The wrappers here compute NOTHING. Each runs the same body the two-step form runs -- the
    ! `*_impl` workers above, with this binding's own name for every refusal -- and then puts the
    ! answer on the target through the one helper below, which is the `%add_column` (plus
    ! `%set_null`) a caller writes by hand today. So a column added here equals the column a
    ! caller would build from `%agg`'s or `%apply`'s array bit for bit, and every rule of those
    ! two (nulls, NaN, weights, options, threading, the exact family's aborts) holds here
    ! unchanged rather than being re-earned.
    !
    !> The two refusals every table-target binding runs BEFORE it computes anything: the target
    !! may not be the table this grouping was built from, and a target that already has columns
    !! must have one row per group.
    !!
    !! The first is also the aliasing guard. `self` holds a pointer into its table's column
    !! store, so an `%add_column` on that table could relocate the slots underneath a grouping
    !! that is an argument of the same call; and a per-group column on the source table would be
    !! `%ngroups()` values spread over `%nrows()` rows, which is not an answer. The second
    !! catches the quiet case: a table of the right KIND and the wrong length, or the right
    !! length from ANOTHER grouping, gives a well-formed column about the wrong groups.
    !!
    !! A target with no columns yet is accepted at any length: its row count becomes
    !! `%ngroups()` at the first add, exactly as `%add_column` fixes a new table's row count.
    !! A closed table is refused by `%ncols()` here, under `parquet_table`'s own name, which is
    !! where every other table-side refusal of the target comes from too.
    subroutine grp_check_target(self, table, proc)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        type(parquet_table), intent(in) :: table    !! the target.
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        character(len=:), allocatable :: sfx
        character(len=32) :: r, g
        !
        if (associated(table%cache, self%cache)) then
            call table_context_suffix(self%cache, "", sfx)
            error stop GP // trim(proc) // ": the target table is the one this grouping was built " // &
                "from; a per-group column has one row per group and belongs on the key table " // &
                "(%key_table)" // sfx
        end if
        if (table%ncols() > 0) then
            if (table%nrows() /= self%ngrp) then
                write(r, "(I0)") table%nrows()
                write(g, "(I0)") self%ngrp
                call table_context_suffix(table%cache, "", sfx)
                error stop GP // trim(proc) // ": the target table has " // trim(r) // " rows and " // &
                    "this grouping has " // trim(g) // " groups; a per-group column goes on this " // &
                    "grouping's %key_table" // sfx
            end if
        end if
    end subroutine grp_check_target
    !
    !> Tokenises `as` through the library's one name-list tokenizer and refuses anything but
    !! exactly one name, for the bindings that add exactly one column. A caller who wrote a list
    !! wanted `%add_apply`, and a caller whose `as` tokenises to nothing wrote a separator.
    subroutine grp_one_name(as, proc, nm)
        character(len=*), intent(in) :: as               !! the caller's `as`.
        character(len=*), intent(in) :: proc             !! calling binding, for the message.
        character(len=:), allocatable, intent(out) :: nm !! the one name, trimmed.
        character(len=:), allocatable :: names(:), shown
        character(len=32) :: n
        !
        call parquet_split_name_list(as, names)
        if (size(names) == 1) then
            nm = trim(names(1))
            return
        end if
        write(n, "(I0)") size(names)
        shown = trim(as)
        if (len(shown) > 100) shown = shown(1:100) // "..."
        error stop GP // trim(proc) // ": as= must name exactly one column, got " // trim(n) // &
            " names in """ // shown // """"
    end subroutine grp_one_name
    !
    !> Tokenises `as` into the names a `%add_apply` call is to add, through the same tokenizer
    !! `%group_by`'s key list goes through, and refuses the two lists that cannot become columns:
    !! one that names nothing at all (a caller who wrote only separators), and one that names a
    !! column twice, which a table cannot carry. Its length is what the call passes as `nout`, so
    !! the number of names and the number of results per group cannot disagree.
    subroutine grp_name_list(as, proc, names)
        character(len=*), intent(in) :: as                     !! the caller's `as`, one name per result.
        character(len=*), intent(in) :: proc                   !! calling binding, for the message.
        character(len=:), allocatable, intent(out) :: names(:)  !! one entry per name, blank-padded.
        integer :: i, j
        !
        call parquet_split_name_list(as, names)
        if (size(names) == 0) then
            error stop GP // trim(proc) // ": as= names no column; give one name per result, " // &
                "comma-separated"
        end if
        do i = 1, size(names) - 1
            do j = i + 1, size(names)
                if (names(i) == names(j)) then
                    error stop GP // trim(proc) // ": as= names """ // trim(names(i)) // &
                        """ twice; a table cannot carry two columns of one name"
                end if
            end do
        end do
    end subroutine grp_name_list
    !
    !> Refuses a name the target already carries unless `force=.true.` was given. Called for
    !! EVERY name a binding is about to add before the FIRST column is written, so a refused
    !! call leaves the target exactly as it was.
    subroutine grp_check_free_name(table, nm, proc, force)
        type(parquet_table), intent(in) :: table    !! the target.
        character(len=*), intent(in) :: nm          !! the name about to be added.
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        logical, intent(in), optional :: force      !! .true. replaces instead of refusing.
        character(len=:), allocatable :: sfx
        !
        if (present(force)) then
            if (force) return
        end if
        if (.not. table%has_column(nm)) return
        call table_context_suffix(table%cache, nm, sfx)
        error stop GP // trim(proc) // ": """ // nm // """ is already a column of the target " // &
            "table; pass force=.true. to replace it" // sfx
    end subroutine grp_check_free_name
    !
    !> The plural of the check above, for the bindings that add several columns in one call: EVERY
    !! name is checked before the first column is written, so a call refused on its last name
    !! leaves the target exactly as it was.
    subroutine grp_check_free_names(table, names, proc, force)
        type(parquet_table), intent(in) :: table     !! the target.
        character(len=*), intent(in) :: names(:)     !! the names about to be added.
        character(len=*), intent(in) :: proc         !! calling binding, for the message.
        logical, intent(in), optional :: force       !! .true. replaces instead of refusing.
        integer :: k
        !
        do k = 1, size(names)
            call grp_check_free_name(table, trim(names(k)), proc, force)
        end do
    end subroutine grp_check_free_names
    !
    !> The unit the new column is to carry, resolved by the CALLER of the helper so that the
    !! helper has one rule and no policy: `unit=` given wins (`""` for none); absent, the SOURCE
    !! column's unit for the statistics that keep the column's dimension, and none for the ones
    !! that do not -- `"size"`, `"count"` and `"nunique"` are counts, and `"var"` is a squared
    !! quantity, which a free-text unit string cannot spell. `table_slot_unit` is the one unit
    !! resolution every query shares, so an in-memory column's `%add_column(unit=)` is seen too.
    subroutine agg_unit(self, idx, code, unit, u)
        class(parquet_grouping), intent(in) :: self      !! the grouping.
        integer, intent(in) :: idx                       !! the source column's validated slot.
        integer, intent(in) :: code                      !! the AG_* code of the statistic.
        character(len=*), intent(in), optional :: unit    !! the caller's unit, if any.
        character(len=:), allocatable, intent(out) :: u   !! the unit to store, "" for none.
        !
        if (present(unit)) then
            u = unit
            return
        end if
        u = ""
        select case (code)
        case (AG_SIZE, AG_COUNT, AG_NUNIQUE, AG_VAR)
            return
        end select
        call table_slot_unit(self%cache, idx, u)
    end subroutine agg_unit
    !
    !> The ONE route by which a table-target binding's column reaches the target, real64 half:
    !! the unit (already resolved by the caller) and the NaN-to-Null rule live here and nowhere
    !! else, so a future binding that called `%add_column` itself would silently apply neither.
    !! These are exactly the two calls a caller makes by hand after `%agg`, which is why this is the
    !! two-step form and not a new route.
    !!
    !! `%set_null` only ever ADDS nulls and is a VALUE mutation, so it neither resurrects a value
    !! nor advances the target's `%generation()`; the add under a new name within the target's
    !! spare capacity does not either, which is what `%key_table(reserve=)` buys.
    subroutine grp_put_column_f64(table, nm, vals, u, nan_to_null, force)
        type(parquet_table), intent(inout) :: table  !! the target.
        character(len=*), intent(in) :: nm           !! the new column's name.
        real(real64), intent(in) :: vals(:)          !! one value per group, in group order.
        character(len=*), intent(in) :: u            !! the unit to store, "" for none.
        logical, intent(in) :: nan_to_null           !! .true. marks a NaN answer's row null.
        logical, intent(in), optional :: force       !! .true. replaces an existing column.
        logical, allocatable :: valid(:)
        integer(int64) :: i
        !
        if (nan_to_null) then
            allocate(valid(size(vals, kind=int64)))
            do i = 1_int64, size(vals, kind=int64)
                ! `x == x` rather than ieee_is_nan: this is a per-element path, where the
                ! intrinsic is a runtime call under two of the supported compilers
                ! (.claude/rules/fortran-gotchas.md). Both spellings are quiet on a quiet NaN.
                valid(i) = (vals(i) == vals(i))
            end do
        end if
        call table%add_column(nm, vals, unit=u, force=force)
        if (nan_to_null) call table%set_null(nm, valid)
    end subroutine grp_put_column_f64
    !
    !> The helper's `int64` half: no NaN rule -- an exact answer is never NaN, and a count never
    !! is either -- and the same one `%add_column`. `%add_size` and `%add_agg(exact=.true.)` come
    !! through here, so that every column a table-target binding adds has one route.
    subroutine grp_put_column_i64(table, nm, vals, u, force)
        type(parquet_table), intent(inout) :: table  !! the target.
        character(len=*), intent(in) :: nm           !! the new column's name.
        integer(int64), intent(in) :: vals(:)        !! one value per group, in group order.
        character(len=*), intent(in) :: u            !! the unit to store, "" for none.
        logical, intent(in), optional :: force       !! .true. replaces an existing column.
        !
        call table%add_column(nm, vals, unit=u, force=force)
    end subroutine grp_put_column_i64
    !
    module procedure grp_add_agg_stat
        character(len=*), parameter :: PROC = "add_agg"
        real(real64), allocatable :: vals(:)
        integer(int64), allocatable :: ivals(:)
        character(len=:), allocatable :: nm, u
        integer :: code, idx, kind
        logical :: ex, tonull
        !
        ex = .false.
        if (present(exact)) ex = exact
        tonull = .true.
        if (present(nan_to_null)) tonull = nan_to_null
        call grp_resolve(self, PROC)
        call grp_check_target(self, table, PROC)
        call grp_one_name(as, PROC, nm)
        if (ex) then
            ! An option the form cannot honour is refused, not ignored: the exact family takes
            ! none of them, so %agg's own int64 specific does not even have the arguments.
            if (present(nan_to_null)) then
                error stop GP // PROC // ": nan_to_null= has no meaning with exact=.true.; the exact " // &
                    "family never answers NaN (it aborts where no exact answer exists)"
            end if
            if (present(weights) .or. present(weight_column)) then
                error stop GP // PROC // ": weights= and weight_column= have no meaning with " // &
                    "exact=.true.; the exact int64 family is unweighted by definition"
            end if
        end if
        call grp_check_free_name(table, nm, PROC, force)
        ! The token's code and the source column's slot: the worker validates both again, and the
        ! second run is a lookup (cache_find plus a residency check) on a column this one has
        ! already touched. They are resolved here because the unit rule needs both, and running
        ! them in this order leaves each refusal the worker's own text under this binding's name.
        call agg_parse_stat(stat, ex, PROC, code)
        call agg_value_column(self, name, PROC, ex, idx, kind)
        if (ex) call agg_check_options(code, PROC, q, ddof, method, scale, .false.)
        call agg_unit(self, idx, code, unit, u)
        if (ex) then
            call agg_stat_int_impl(self, name, stat, ivals, PROC, threads)
            call grp_put_column_i64(table, nm, ivals, u, force)
        else
            call agg_stat_real_impl(self, name, stat, vals, PROC, weights, weight_column, q, ddof, &
                method, scale, threads)
            call grp_put_column_f64(table, nm, vals, u, tonull, force)
        end if
    end procedure grp_add_agg_stat
    !
    !> %add_agg specific, the caller's per-column procedure onto a table; the contract is on the
    !! interface in the module spec. Restated in full for the reason the two procedure-form
    !! `%apply` bodies are (the gfortran implicit-interface trap).
    module subroutine grp_add_agg_func(self, name, func, table, as, weights, weight_column, unit, &
            nan_to_null, force, threads)
        class(parquet_grouping), intent(in) :: self             !! the grouping.
        character(len=*), intent(in) :: name                    !! the column; read if not resident.
        procedure(parquet_group_column_reduce_i) :: func        !! called once per group; see the interface.
        type(parquet_table), intent(inout) :: table             !! the target: one row per group.
        character(len=*), intent(in) :: as                      !! the new column's name; exactly one name.
        real(real64), intent(in), optional :: weights(:)        !! one weight per table row.
        character(len=*), intent(in), optional :: weight_column !! a scalar numeric column of weights.
        character(len=*), intent(in), optional :: unit          !! the new column's unit; absent means none.
        logical, intent(in), optional :: nan_to_null            !! .false. keeps a NaN answer a value; default .true.
        logical, intent(in), optional :: force                  !! .true. replaces an existing column of that name.
        integer, intent(in), optional :: threads                !! the team; absent = serial.
        character(len=*), parameter :: PROC = "add_agg"
        real(real64), allocatable :: vals(:)
        character(len=:), allocatable :: nm, u
        logical :: tonull
        !
        tonull = .true.
        if (present(nan_to_null)) tonull = nan_to_null
        ! No unit is inherited here: the library cannot know a callback's dimension, so only
        ! `unit=` gives the column one (3.2 of the design note; the token form is the exception).
        u = ""
        if (present(unit)) u = unit
        call grp_resolve(self, PROC)
        call grp_check_target(self, table, PROC)
        call grp_one_name(as, PROC, nm)
        call grp_check_free_name(table, nm, PROC, force)
        call agg_func_impl(self, name, func, vals, PROC, weights, weight_column, threads)
        call grp_put_column_f64(table, nm, vals, u, tonull, force)
    end subroutine grp_add_agg_func
    !
    module procedure grp_add_size
        character(len=*), parameter :: PROC = "add_size"
        integer(int64), allocatable :: counts(:)
        character(len=:), allocatable :: nm
        !
        call grp_resolve(self, PROC)
        call grp_check_target(self, table, PROC)
        call grp_one_name(as, PROC, nm)
        call grp_check_free_name(table, nm, PROC, force)
        ! %size's own answer, so the column is bit for bit %key_table(size_name=)'s. No unit: a
        ! count has none, and there is no NaN rule to apply to one.
        call grp_size_i64(self, counts)
        call grp_put_column_i64(table, nm, counts, "", force)
    end procedure grp_add_size
    !
    !> %add_apply specific, the caller's per-group procedure onto a table; the contract is on the
    !! interface in the module spec. Restated in full for the reason the two procedure-form
    !! `%apply` bodies are (the gfortran implicit-interface trap).
    module subroutine grp_add_apply_proc(self, func, table, as, unit, nan_to_null, force, threads)
        class(parquet_grouping), intent(in) :: self    !! the grouping.
        procedure(parquet_group_apply_i) :: func       !! called once per group; see the interface.
        type(parquet_table), intent(inout) :: table    !! the target: one row per group.
        character(len=*), intent(in) :: as             !! the new columns' names, one per result.
        character(len=*), intent(in), optional :: unit !! the unit of every column named; absent means none.
        logical, intent(in), optional :: nan_to_null   !! .false. keeps a NaN result a value; default .true.
        logical, intent(in), optional :: force         !! .true. replaces existing columns of those names.
        integer, intent(in), optional :: threads       !! the team; absent = serial.
        character(len=*), parameter :: PROC = "add_apply"
        real(real64), allocatable :: out(:, :), vals(:)
        character(len=:), allocatable :: names(:), u
        logical :: tonull
        integer :: k
        !
        tonull = .true.
        if (present(nan_to_null)) tonull = nan_to_null
        ! One unit for every column named, and none unless it was given: the several-result case
        ! is normally a value and its error, and the library cannot know a callback's dimension.
        u = ""
        if (present(unit)) u = unit
        call grp_resolve(self, PROC)
        call grp_check_target(self, table, PROC)
        call grp_name_list(as, PROC, names)
        call grp_check_free_names(table, names, PROC, force)
        call apply_proc_matrix_impl(self, func, size(names), out, PROC, threads)
        do k = 1, size(names)
            ! out(k, :) is a strided section of a column-major matrix, copied into a contiguous
            ! local once per NAME and never per cell; the k-th name takes the k-th result.
            vals = out(k, :)
            call grp_put_column_f64(table, trim(names(k)), vals, u, tonull, force)
        end do
    end subroutine grp_add_apply_proc
    !
    module procedure grp_add_apply_obj
        character(len=*), parameter :: PROC = "add_apply"
        real(real64), allocatable :: out(:, :), vals(:)
        character(len=:), allocatable :: names(:), u
        logical :: tonull
        integer :: k
        !
        tonull = .true.
        if (present(nan_to_null)) tonull = nan_to_null
        u = ""
        if (present(unit)) u = unit
        call grp_resolve(self, PROC)
        call grp_check_target(self, table, PROC)
        call grp_name_list(as, PROC, names)
        call grp_check_free_names(table, names, PROC, force)
        call apply_obj_matrix_impl(self, reducer, size(names), out, PROC, threads)
        do k = 1, size(names)
            vals = out(k, :)
            call grp_put_column_f64(table, trim(names(k)), vals, u, tonull, force)
        end do
    end procedure grp_add_apply_obj
    !
    !
    ! ---- %broadcast and %gather: the two ends of the hot loop ----------------------------------
    !
    !> Aborts unless `per_group` holds exactly one value per group, naming both counts.
    subroutine bcast_check(self, nper, proc)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        integer(int64), intent(in) :: nper          !! size(per_group).
        character(len=*), intent(in) :: proc        !! calling binding, for the message.
        character(len=32) :: a, b
        !
        if (nper == self%ngrp) return
        write(a, "(I0)") nper
        write(b, "(I0)") self%ngrp
        error stop GP // trim(proc) // ": per_group has " // trim(a) // " entries and this grouping has " // &
            trim(b) // " groups; it takes exactly one value per group, in group order, as %size, %agg and " // &
            "%apply give it"
    end subroutine bcast_check
    !
    module procedure grp_broadcast_f64
        character(len=*), parameter :: PROC = "broadcast"
        real(real64) :: f
        integer(int64) :: g, i
        integer :: nt, chunk
        !
        call grp_resolve(self, PROC)
        call bcast_check(self, size(per_group, kind=int64), PROC)
        call group_team(self%ngrp, threads, PROC, .true., nt)
        f = ieee_value(0.0_real64, ieee_quiet_nan)
        if (present(fill)) f = fill
        allocate(per_row(self%ntab))
        per_row = f
        chunk = group_chunk(self%ngrp, nt)
        ! Every grouped row is written exactly once, by its own group, and the fill was written
        ! before the team opened: no race, no order, the same bits at every team.
        !$omp parallel do num_threads(nt) default(shared) private(g, i) schedule(dynamic, chunk) if (nt > 1)
        do g = 1_int64, self%ngrp
            do i = self%offsets(g), self%offsets(g + 1_int64) - 1_int64
                per_row(self%perm(i)) = per_group(g)
            end do
        end do
        !$omp end parallel do
    end procedure grp_broadcast_f64
    !
    module procedure grp_broadcast_i64
        character(len=*), parameter :: PROC = "broadcast"
        integer(int64) :: f
        integer(int64) :: g, i
        integer :: nt, chunk
        !
        call grp_resolve(self, PROC)
        call bcast_check(self, size(per_group, kind=int64), PROC)
        call group_team(self%ngrp, threads, PROC, .true., nt)
        f = 0_int64
        if (present(fill)) f = fill
        allocate(per_row(self%ntab))
        per_row = f
        chunk = group_chunk(self%ngrp, nt)
        ! As in the real64 form: one writer per row, the fill in place before the team.
        !$omp parallel do num_threads(nt) default(shared) private(g, i) schedule(dynamic, chunk) if (nt > 1)
        do g = 1_int64, self%ngrp
            do i = self%offsets(g), self%offsets(g + 1_int64) - 1_int64
                per_row(self%perm(i)) = per_group(g)
            end do
        end do
        !$omp end parallel do
    end procedure grp_broadcast_i64
    !
    module procedure grp_broadcast_i32
        integer(int64), allocatable :: pg64(:), pr64(:)
        !
        ! A forward, like every other int32 form: widened in, narrowed out, one walk. The narrow
        ! can never refuse here -- every entry of the answer is one of this call's own int32
        ! inputs -- but it is the one route out, so it is the route this takes too.
        pg64 = int(per_group, int64)
        if (present(fill)) then
            call grp_broadcast_i64(self, pg64, pr64, int(fill, int64), threads)
        else
            call grp_broadcast_i64(self, pg64, pr64, threads=threads)
        end if
        call grp_narrow(pr64, "broadcast", "value", per_row)
    end procedure grp_broadcast_i32
    !
    !> `size(is_valid)` when it is present, else 0: the length `gather_prepare` checks.
    integer(int64) function valid_size(is_valid) result(n)
        logical, intent(in), optional :: is_valid(:) !! the caller's validity buffer, or absent.
        n = 0_int64
        if (present(is_valid)) n = size(is_valid, kind=int64)
    end function valid_size
    !
    !> The checks every `%gather` specific runs before it copies: the grouping is current, `g`
    !! names a group, the column resolves (read if not resident) and is of a kind this buffer
    !! takes, and the buffer -- `is_valid` too, when given -- is at least as long as the group.
    !! A shorter buffer ABORTS rather than truncates (this file's header). Hands back the
    !! column's slot and kind and the group's run in the permutation.
    subroutine gather_prepare(self, name, g, kinds, what, nbuf, has_valid, nvalid, idx, kind, lo, hi, n)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        character(len=*), intent(in) :: name        !! the column.
        integer(int64), intent(in) :: g             !! the group.
        integer, intent(in) :: kinds(:)             !! the PK_* kinds this buffer takes, its own first.
        character(len=*), intent(in) :: what        !! the buffer's declaration with its article, for the message.
        integer(int64), intent(in) :: nbuf          !! size(buf).
        logical, intent(in) :: has_valid            !! present(is_valid).
        integer(int64), intent(in) :: nvalid        !! size(is_valid), or 0 when absent.
        integer, intent(out) :: idx                 !! the column's slot.
        integer, intent(out) :: kind                !! its PK_* kind.
        integer(int64), intent(out) :: lo           !! the group's first position in perm.
        integer(int64), intent(out) :: hi           !! the group's last position in perm.
        integer(int64), intent(out) :: n            !! the group's row count.
        character(len=*), parameter :: PROC = "gather"
        character(len=:), allocatable :: kname, sfx
        character(len=32) :: a, b, c
        !
        call grp_resolve(self, PROC)
        call grp_check_group(self, g, PROC)
        call grp_resolve_column(self, name, PROC, idx)
        kind = self%cache%cols(idx)%values%kindof()
        if (.not. any(kinds == kind)) then
            call parquet_kind_name(kind, kname)
            call table_context_suffix(self%cache, name, sfx)
            error stop GP // PROC // ": column kind (" // kname // ") cannot be copied into " // what // &
                " buffer; %gather widens exactly as %get does (an int32 column into an int64 buffer, a " // &
                "float32 column into a real64 one) and nothing else -- %get_slice takes every kind" // sfx
        end if
        lo = self%offsets(g)
        hi = self%offsets(g + 1_int64) - 1_int64
        n = hi - lo + 1_int64
        write(b, "(I0)") g
        write(c, "(I0)") n
        if (nbuf < n) then
            write(a, "(I0)") nbuf
            error stop GP // PROC // ": buf is " // trim(a) // " long and group " // trim(b) // " has " // &
                trim(c) // " rows; the values are not truncated -- size the buffer once by %max_size()"
        end if
        if (has_valid) then
            if (nvalid < n) then
                write(a, "(I0)") nvalid
                error stop GP // PROC // ": is_valid is " // trim(a) // " long and group " // trim(b) // &
                    " has " // trim(c) // " rows; size it by %max_size(), as buf"
            end if
        end if
    end subroutine gather_prepare
    !
    !> Fills `is_valid(1:n)` with whether each of the group's rows is non-null: `.true.`
    !! throughout when the column holds no null at all, else asked of the column row by row.
    subroutine gather_validity(self, idx, lo, hi, is_valid)
        class(parquet_grouping), intent(in) :: self !! the grouping.
        integer, intent(in) :: idx                  !! the column's slot.
        integer(int64), intent(in) :: lo            !! the group's first position in perm.
        integer(int64), intent(in) :: hi            !! the group's last position in perm.
        logical, intent(inout) :: is_valid(:)       !! receives the validity in is_valid(1:hi-lo+1).
        integer(int64) :: i
        !
        if (parquet_column_any_null(self%cache%cols(idx)%values)) then
            do i = lo, hi
                is_valid(i - lo + 1_int64) = .not. parquet_column_is_null(self%cache%cols(idx)%values, self%perm(i))
            end do
        else
            is_valid(1:hi - lo + 1_int64) = .true.
        end if
    end subroutine gather_validity
    !
    module procedure grp_gather_i32
        integer(int32), pointer :: p(:)
        integer :: idx, kind
        integer(int64) :: lo, hi
        !
        call gather_prepare(self, name, g, [PK_INT32], "an integer(int32)", size(buf, kind=int64), &
            present(is_valid), valid_size(is_valid), idx, kind, lo, hi, n)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
        buf(1:n) = p(self%perm(lo:hi))
        if (present(is_valid)) call gather_validity(self, idx, lo, hi, is_valid)
    end procedure grp_gather_i32
    !
    module procedure grp_gather_i64
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        integer :: idx, kind
        integer(int64) :: lo, hi
        !
        call gather_prepare(self, name, g, [PK_INT64, PK_INT32], "an integer(int64)", size(buf, kind=int64), &
            present(is_valid), valid_size(is_valid), idx, kind, lo, hi, n)
        if (kind == PK_INT64) then
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p64)
            buf(1:n) = p64(self%perm(lo:hi))
        else
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p32)
            buf(1:n) = int(p32(self%perm(lo:hi)), int64)
        end if
        if (present(is_valid)) call gather_validity(self, idx, lo, hi, is_valid)
    end procedure grp_gather_i64
    !
    module procedure grp_gather_f32
        real(real32), pointer :: p(:)
        integer :: idx, kind
        integer(int64) :: lo, hi
        !
        call gather_prepare(self, name, g, [PK_FLOAT32], "a real(real32)", size(buf, kind=int64), &
            present(is_valid), valid_size(is_valid), idx, kind, lo, hi, n)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
        buf(1:n) = p(self%perm(lo:hi))
        if (present(is_valid)) call gather_validity(self, idx, lo, hi, is_valid)
    end procedure grp_gather_f32
    !
    module procedure grp_gather_f64
        real(real32), pointer :: p32(:)
        real(real64), pointer :: p64(:)
        integer :: idx, kind
        integer(int64) :: lo, hi
        !
        call gather_prepare(self, name, g, [PK_FLOAT64, PK_FLOAT32], "a real(real64)", size(buf, kind=int64), &
            present(is_valid), valid_size(is_valid), idx, kind, lo, hi, n)
        if (kind == PK_FLOAT64) then
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p64)
            buf(1:n) = p64(self%perm(lo:hi))
        else
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p32)
            buf(1:n) = real(p32(self%perm(lo:hi)), real64)
        end if
        if (present(is_valid)) call gather_validity(self, idx, lo, hi, is_valid)
    end procedure grp_gather_f64
    !
    module procedure grp_gather_bool
        logical, pointer :: p(:)
        integer :: idx, kind
        integer(int64) :: lo, hi
        !
        call gather_prepare(self, name, g, [PK_LOGICAL], "a logical", size(buf, kind=int64), &
            present(is_valid), valid_size(is_valid), idx, kind, lo, hi, n)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
        buf(1:n) = p(self%perm(lo:hi))
        if (present(is_valid)) call gather_validity(self, idx, lo, hi, is_valid)
    end procedure grp_gather_bool
    !
    module procedure grp_gather_i32_g32
        integer(int64) :: n64
        !
        call grp_gather_i32(self, name, int(g, int64), buf, n64, is_valid)
        call grp_narrow_scalar(n64, "gather", "count", n)
    end procedure grp_gather_i32_g32
    !
    module procedure grp_gather_i64_g32
        integer(int64) :: n64
        !
        call grp_gather_i64(self, name, int(g, int64), buf, n64, is_valid)
        call grp_narrow_scalar(n64, "gather", "count", n)
    end procedure grp_gather_i64_g32
    !
    module procedure grp_gather_f32_g32
        integer(int64) :: n64
        !
        call grp_gather_f32(self, name, int(g, int64), buf, n64, is_valid)
        call grp_narrow_scalar(n64, "gather", "count", n)
    end procedure grp_gather_f32_g32
    !
    module procedure grp_gather_f64_g32
        integer(int64) :: n64
        !
        call grp_gather_f64(self, name, int(g, int64), buf, n64, is_valid)
        call grp_narrow_scalar(n64, "gather", "count", n)
    end procedure grp_gather_f64_g32
    !
    module procedure grp_gather_bool_g32
        integer(int64) :: n64
        !
        call grp_gather_bool(self, name, int(g, int64), buf, n64, is_valid)
        call grp_narrow_scalar(n64, "gather", "count", n)
    end procedure grp_gather_bool_g32
    !
end submodule parquet_tables_group ! GCOVR_EXCL_LINE
