!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The join's PAIR LIST: which left row meets which right row, over nothing but row indices.
!!
!! **NOT a generated file** -- `tools/generate_parquet_tables.py` emits this module's spec, so a
!! signature change here is a generator edit and a body change is not.
!!
!! **Two engines, one answer.** `table_join_pairs` resolves the vocabulary and the key columns
!! once and hands the key SLOTS to one of two engines, which must agree to the row on every call
!! both can take (`test/test_table_join.f90` runs its whole suite under each):
!!
!!   * **The SORT engine** (`join_pairs_sort`, this file), over the two key columns
!!     CONCATENATED: for each join key one `parquet_column` holding the left table's key rows
!!     followed by the right table's, all of them handed to `pf_argsort` as a `pf_sort_keys`, and
!!     the matches read off `group_offsets` -- the run boundaries the sort reports in the same
!!     pass. A run holding rows from both halves is a match: a member at position `p <= nl` is
!!     left row `p`, one at `p > nl` is right row `p - nl`. That is astropy's join algorithm, and
!!     it needs no new sorting code at all: multi-column keys, all nine orderable kinds, per-key
!!     null handling, the threading and the stable tie rule are already what `pf_argsort` does --
!!     so "equal" here means exactly what it means in `%sort_by`, in a read-time `sort_by=`, and
!!     in `pf_match`. It takes every `order="key"` join, a `PK_LOGICAL` key, and a string key
!!     beside another key.
!!   * **The HASH engine** (`join_pairs_hash`, `parquet_tables_join_hash.f90`): a
!!     `pf_index_multimap` built over the right keys and probed once per left row, for every
!!     other call under `order="left"`. Its equality is the index tier's, which is the sort
!!     comparator's by construction (feature_risks.md Risk-211), so it is not a second null
!!     policy either.
!!
!! Which engine runs is `join_choose_engine`'s decision -- the hash engine whenever the call is
!! eligible, from the key kinds and `order=` alone and never from the data's values -- and a
!! test-only hook can force either; both engine bodies record which one ran, so a suite can be
!! run twice and assert it rather than assume it.
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
    ! An INTERMEDIATE submodule (parquet_tables_join_hash.f90 descends from it) may not
    ! reach a name by host association from the module above: nagfor then cannot compile
    ! its descendants. Every use-associated name this file references is imported here.
    use, intrinsic :: iso_fortran_env, only : int64
    ! Bare, as parquet_tables.f90 imports it: an `only:` list naming a GENERIC of parquet_columns
    ! (`parquet_column_is_null`) is rejected by gfortran when the module above already has it.
    use parquet_columns
    use parquet_core, only : parquet_split_name_list
    use parquet_sorting, only : pf_sort_keys, pf_argsort
    ! The sort's own thread rule and its tail floor, for the sort engine's passes over the
    ! runs: that engine resolves ONE team, by the call `pf_argsort` makes over the same rows,
    ! and every pass that reads the sort's result runs on it. `parquet_argsort` is already in
    ! this module's footprint through `parquet_sorting`, so a submodule import costs a
    ! consumer nothing (the arrangement `parquet_tables_index.f90` keeps for the index tier).
    use parquet_argsort, only : resolve_thread_count, tail_team
    ! The multimap's tuple ceiling, for the eligibility test; the module is already in this
    ! module's footprint through the spec's own import of the two index types.
    use parquet_index, only : pf_index_max_components
    implicit none
    !
    !> `how=` tokens, in the order the error message lists them.
    integer, parameter :: HOW_INNER = 1, HOW_LEFT = 2, HOW_RIGHT = 3, HOW_OUTER = 4, &
        HOW_SEMI = 5, HOW_ANTI = 6
    !> `require=` tokens. REQ_MM asserts nothing and is the default.
    integer, parameter :: REQ_MM = 0, REQ_11 = 1, REQ_1M = 2, REQ_M1 = 3
    !> `order=` tokens.
    integer, parameter :: ORD_LEFT = 1, ORD_KEY = 2
    !> Engine tokens: which of the two pair-list engines built the match. They double as the
    !! values the test-only hook `parquet_debug_set_join_engine` takes (0 there means automatic)
    !! and `parquet_debug_join_engine_used` reports, so nothing ever translates between the two.
    integer, parameter :: ENGINE_SORT = 1, ENGINE_HASH = 2
    !
    ! ---- The hash engine (parquet_tables_join_hash) ----
    interface
        !> The pair list built on a `pf_index_multimap` over the RIGHT keys and probed with the
        !! left ones: the same answer `join_pairs_sort` gives for every call `join_hash_eligible`
        !! accepts, and never reached for any other. Takes the key SLOTS, already resolved and
        !! kind-checked by `join_resolve_keys`, and the resolved tokens, so the two engines
        !! cannot disagree about which columns are the keys or what `how=` meant.
        module subroutine join_pairs_hash(self, other, lslots, rslots, how_id, req_id, max_rows, &
                threads, team, il, ir, n_out, matched)
            class(parquet_table), intent(in) :: self  !! the LEFT table.
            class(parquet_table), intent(in) :: other !! the RIGHT table.
            integer, intent(in) :: lslots(:)          !! this table's key slots, primary first.
            integer, intent(in) :: rslots(:)          !! `other`'s key slots, one per left one.
            integer, intent(in) :: how_id             !! HOW_* token.
            integer, intent(in) :: req_id             !! REQ_* token.
            integer(int64), intent(in), optional :: max_rows !! output-size ceiling.
            !> team for the multimap's build and probe and for this engine's own passes;
            !! absent means the index tier's automatic rule.
            integer, intent(in), optional :: threads
            !> receives the team this engine's own pass (the emission) ran on, for the caller's
            !! passes over the pair list; 1 means serial.
            integer, intent(out) :: team
            integer(int64), allocatable, intent(out) :: il(:) !! per output row: left row, or 0.
            integer(int64), allocatable, intent(out) :: ir(:) !! per output row: right row, or 0.
            integer(int64), intent(out) :: n_out              !! output rows; equals size(il).
            logical, allocatable, intent(out), optional :: matched(:) !! per PRE-join left row.
        end subroutine join_pairs_hash
    end interface
    !
contains
    !
    module procedure table_join_pairs
        character(len=*), parameter :: PROC = "join"
        integer, allocatable :: lslots(:), rslots(:)
        integer :: how_id, req_id, ord_id, engine
        integer :: eng_team !! the team the engine's own passes ran on.
        !
        call table_check_open(self, PROC)
        call table_check_open(other, PROC)
        call join_check_not_self(self, other)
        call join_resolve_tokens(how, require, order, how_id, req_id, ord_id)
        ! The keys are resolved and kind-checked ONCE, here, and both engines take the slots:
        ! which columns are the keys is not a question two engines may answer separately.
        call join_resolve_keys(self, other, on, other_on, lslots, rslots)
        engine = join_choose_engine(self, lslots, ord_id)
        if (engine == ENGINE_HASH) then
            call join_pairs_hash(self, other, lslots, rslots, how_id, req_id, max_rows, threads, &
                eng_team, il, ir, n_out, matched)
        else
            call join_pairs_sort(self, other, lslots, rslots, how_id, req_id, ord_id, max_rows, &
                threads, eng_team, il, ir, n_out, matched)
        end if
        if (present(team)) team = eng_team
    end procedure table_join_pairs
    !
    !> The SORT engine: astropy's algorithm over the two key columns concatenated.
    !!
    !! One `pf_argsort` over `nl + nr` rows with group offsets, then the matches read off the
    !! runs of equal keys (`join_classify`), the cardinality assertion, the counting pass and the
    !! emission in either order. Every `order="key"` join takes this engine, and so does every
    !! key kind the hash engine declines (`join_hash_eligible`).
    subroutine join_pairs_sort(self, other, lslots, rslots, how_id, req_id, ord_id, max_rows, &
            threads, team, il, ir, n_out, matched)
        class(parquet_table), intent(in) :: self  !! the LEFT table.
        class(parquet_table), intent(in) :: other !! the RIGHT table.
        integer, intent(in) :: lslots(:)          !! this table's key slots, primary first.
        integer, intent(in) :: rslots(:)          !! `other`'s key slots, one per left one.
        integer, intent(in) :: how_id             !! HOW_* token.
        integer, intent(in) :: req_id             !! REQ_* token.
        integer, intent(in) :: ord_id             !! ORD_* token.
        integer(int64), intent(in), optional :: max_rows !! output-size ceiling.
        integer, intent(in), optional :: threads  !! the sort's request, and this engine's own.
        integer, intent(out) :: team              !! the team this engine's passes ran on; 1 = serial.
        integer(int64), allocatable, intent(out) :: il(:) !! per output row: left row, or 0.
        integer(int64), allocatable, intent(out) :: ir(:) !! per output row: right row, or 0.
        integer(int64), intent(out) :: n_out              !! output rows; equals size(il).
        logical, allocatable, intent(out), optional :: matched(:) !! per PRE-join left row.
        type(pf_sort_keys) :: skeys
        type(parquet_column), allocatable :: kc(:)
        integer(int64), allocatable :: perm(:), go(:)
        integer(int64), allocatable :: nleft(:), nright(:)
        logical, allocatable :: lmatched(:), rmatched(:)
        integer(int64), allocatable :: group_of_left(:)
        integer(int64), allocatable :: lg_off(:), lg_idx(:), rg_off(:), rg_idx(:)
        integer(int64) :: nl, nr, ngroups
        integer(int64) :: n_pairs, n_lunm, n_runm, biggest
        integer(int64) :: nt64 !! the sort's resolved count, before the tail floor.
        !
        ! Recorded FIRST, before anything can abort, and by this engine's own body rather than
        ! by the dispatcher: a which-engine-ran observable has to be written by every route, the
        ! fallback included, or a test forced onto the other engine cannot tell a decline from a
        ! switch that never happened (.claude/rules/testing.md, "Mutation testing").
        call join_note_engine(ENGINE_SORT)
        call join_build_keys(self, other, lslots, rslots, skeys, kc)
        nl = self%nrows()
        nr = other%nrows()
        ! THE ONE TEAM of this engine. Resolved by the sort's own rule -- `threads=` honoured,
        ! else the automatic count under `parquet_set_sort_threads`, serial inside a parallel
        ! region, clamped to the affinity mask -- through the very call `pf_argsort` makes below
        ! over the same row count, so the sort and every pass that reads its result run on one
        ! team; then lowered by `tail_team`'s floor, since each of those passes is a whole-array
        ! sweep and a team is not worth opening over a few thousand rows. Recorded before
        ! anything can abort, on every route including the serial one, for the reason the
        ! engine is: the pair list is identical at every team size, so the record is the only
        ! observation that `threads=` reached these passes (feature_risks.md Risk-189).
        call resolve_thread_count(threads, nl + nr, nt64)
        team = tail_team(nt64, nl + nr)
        call join_note_group_threads(team)
        call pf_argsort(skeys, perm, group_offsets=go, threads=threads)
        ngroups = size(go, kind=int64) - 1_int64
        call join_classify(kc, perm, go, nl, nr, ngroups, team, nleft, nright, lmatched, rmatched, &
            group_of_left, lg_off, lg_idx, rg_off, rg_idx)
        call join_check_require(req_id, nleft, nright, lmatched, rmatched, lg_off, lg_idx, &
            rg_off, rg_idx, ngroups, team)
        call join_count(how_id, nleft, nright, lmatched, rmatched, ngroups, nl, team, &
            n_pairs, n_lunm, n_runm, biggest, n_out)
        if (present(max_rows)) then
            if (n_out > max_rows) call join_refuse_size(n_out, max_rows, nl, nr, biggest)
        end if
        if (present(matched)) then
            allocate(matched(nl))
            if (nl > 0_int64) call join_fill_matched(group_of_left, lmatched, nl, team, matched)
        end if
        allocate(il(n_out), ir(n_out))
        if (n_out < 1_int64) return
        if (ord_id == ORD_KEY) then
            ! Serial: a group-major walk with one cursor, and the rare ordering. Its left-major
            ! twin below is the one that is threaded.
            call join_emit_key_order(how_id, ngroups, lmatched, rmatched, lg_off, lg_idx, &
                rg_off, rg_idx, il, ir)
        else
            call join_emit_left_order(how_id, nl, group_of_left, lmatched, rmatched, &
                rg_off, rg_idx, ngroups, team, il, ir)
        end if
    end subroutine join_pairs_sort
    !
    module procedure table_join
        call join_impl(self, other, on, other_on=other_on, how=how, columns=columns, &
            other_suffix=other_suffix, require=require, order=order, matched=matched, &
            pairs=pairs, other_pairs=other_pairs, threads=threads)
    end procedure table_join
    !
    module procedure table_join_max_i32
        ! Widened here rather than given a worker of its own: `max_rows` is compared against a
        ! row count, every int32 value is an int64 one, so the conversion cannot change what the
        ! ceiling means. The two string forms below widen the same way.
        call join_impl(self, other, on, other_on=other_on, how=how, columns=columns, &
            other_suffix=other_suffix, require=require, order=order, &
            max_rows=int(max_rows, int64), matched=matched, pairs=pairs, &
            other_pairs=other_pairs, threads=threads)
    end procedure table_join_max_i32
    !
    module procedure table_join_max_i64
        call join_impl(self, other, on, other_on=other_on, how=how, columns=columns, &
            other_suffix=other_suffix, require=require, order=order, max_rows=max_rows, &
            matched=matched, pairs=pairs, other_pairs=other_pairs, threads=threads)
    end procedure table_join_max_i64
    !
    module procedure table_join_string
        call join_from_strings(self, other, on, other_on=other_on, how=how, columns=columns, &
            other_suffix=other_suffix, require=require, order=order, matched=matched, &
            pairs=pairs, other_pairs=other_pairs, threads=threads)
    end procedure table_join_string
    !
    module procedure table_join_string_max_i32
        call join_from_strings(self, other, on, other_on=other_on, how=how, columns=columns, &
            other_suffix=other_suffix, require=require, order=order, &
            max_rows=int(max_rows, int64), matched=matched, pairs=pairs, &
            other_pairs=other_pairs, threads=threads)
    end procedure table_join_string_max_i32
    !
    module procedure table_join_string_max_i64
        call join_from_strings(self, other, on, other_on=other_on, how=how, columns=columns, &
            other_suffix=other_suffix, require=require, order=order, max_rows=max_rows, &
            matched=matched, pairs=pairs, other_pairs=other_pairs, threads=threads)
    end procedure table_join_string_max_i64
    !
    !> The whole of `%join`, once the six specifics above have agreed on how `on` was spelled and
    !! what kind the ceiling was written in.
    !!
    !! There are six of them because `max_rows=` has to exist in both integer kinds (CLAUDE.md's
    !! dual-kind rule -- it bounds a row count) and an OPTIONAL dummy differing only by kind
    !! cannot disambiguate a generic, so the ceiling is required in four and absent in two. None
    !! of that is a difference in BEHAVIOUR, so none of it belongs below this line: every specific
    !! is three statements long and this procedure is the only place the order of the phases is
    !! written down.
    subroutine join_impl(self, other, on, other_on, how, columns, other_suffix, require, order, &
            max_rows, matched, pairs, other_pairs, threads)
        class(parquet_table), intent(inout) :: self !! the LEFT table; mutated in place.
        class(parquet_table), intent(in) :: other   !! the RIGHT table; only read from.
        character(len=*), intent(in) :: on(:)       !! left key columns, primary first.
        character(len=*), intent(in), optional :: other_on(:)  !! right key columns.
        character(len=*), intent(in), optional :: how          !! join kind.
        character(len=*), intent(in), optional :: columns      !! payload column list.
        character(len=*), intent(in), optional :: other_suffix !! clash suffix; default "_2".
        character(len=*), intent(in), optional :: require      !! cardinality assertion.
        character(len=*), intent(in), optional :: order        !! output ordering.
        integer(int64), intent(in), optional :: max_rows       !! output-size ceiling.
        logical, allocatable, intent(out), optional :: matched(:) !! per PRE-join left row.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.
        integer, intent(in), optional :: threads    !! forwarded to the engine; absent = auto.
        character(len=*), parameter :: PROC = "join"
        integer(int64), allocatable :: il(:), ir(:)
        integer, allocatable :: sslots(:), mlslots(:), mrslots(:)
        character(len=:), allocatable :: dnames(:)
        integer(int64) :: n_out
        integer :: how_id
        integer :: team !! the engine's team, for the passes over the pair list.
        !
        ! VALIDATE EVERYTHING, THEN MUTATE EVERYTHING -- parquet_tables_rowmutate.f90's first
        ! rule, and a join has more chance to break it than anything already there because it
        ! touches two tables. Nothing below this line writes to `self` until join_apply.
        call table_check_open(self, PROC)
        call table_check_open(other, PROC)
        call table_check_not_shared(self, PROC)
        ! Refused before anything is read, not only before anything is written: on a self-join
        ! the two dummies are already the same table by the time this procedure is entered, so
        ! the sooner it says so the less it does first.
        call join_check_not_self(self, other)
        call join_check_key_args(on, other_on)
        ! Resolved HERE as well as in the engine, because `how` decides three NAME questions the
        ! plan below has to answer: whether there is a payload at all, whether a container column
        ! on THIS side is refusable, and whether a merged key can take its value from `other`.
        ! Folding a token twice costs nothing; deriving those answers from `how` twice would not.
        call join_resolve_how(how, how_id)
        call join_check_left_containers(self, how_id)
        ! Planned before the engine runs, because every refusal it raises is a NAME question -- a
        ! column that is not there, a container that cannot be carried, a suffixed name that still
        ! clashes -- and none of them is worth a sort or a hash build first. It reads nothing.
        call join_plan_payload(self, other, on, other_on, columns, other_suffix, how_id, &
            sslots, dnames, mlslots, mrslots)
        ! The engine. Every remaining refusal is here: the self-join, the key kinds and widths,
        ! any key column that cannot be a key at all, the `require=` assertion and the `max_rows=`
        ! ceiling -- the last two both settled from the counting pass, so a join too big to build
        ! is named rather than attempted. `matched=` is filled here for the same reason it is
        ! documented as being over the PRE-join rows: this is the last point at which they exist.
        call table_join_pairs(self, other, on, other_on=other_on, how=how, require=require, &
            order=order, max_rows=max_rows, il=il, ir=ir, n_out=n_out, matched=matched, &
            threads=threads, team=team)
        ! Reads the payload columns that are not resident yet -- AFTER the engine, so a join that
        ! was going to be refused does not read a column first. A column already resident, which
        ! is every column the `columns=`-absent default selects, returns immediately.
        call join_touch_payload(other, sslots)
        call join_apply(self, other, il, ir, n_out, sslots, dnames, mlslots, mrslots, team)
        ! Handed over rather than copied, and AFTER the rewrite because the rewrite reads both.
        ! `il`/`ir` are dead from here, so move_alloc makes `pairs=` cost nothing at all on a
        ! join whose output is larger than either input -- which is the shape a caller asking
        ! for the pair list is most likely to be in.
        if (present(pairs)) call move_alloc(il, pairs)
        if (present(other_pairs)) call move_alloc(ir, other_pairs)
    end subroutine join_impl
    !
    !> The separated-string specifics' shared body: split each key string into names, then join.
    subroutine join_from_strings(self, other, on, other_on, how, columns, other_suffix, &
            require, order, max_rows, matched, pairs, other_pairs, threads)
        class(parquet_table), intent(inout) :: self !! the LEFT table; mutated in place.
        class(parquet_table), intent(in) :: other   !! the RIGHT table; only read from.
        character(len=*), intent(in) :: on          !! left key columns, separated.
        character(len=*), intent(in), optional :: other_on     !! right key columns, separated.
        character(len=*), intent(in), optional :: how          !! join kind.
        character(len=*), intent(in), optional :: columns      !! payload column list.
        character(len=*), intent(in), optional :: other_suffix !! clash suffix; default "_2".
        character(len=*), intent(in), optional :: require      !! cardinality assertion.
        character(len=*), intent(in), optional :: order        !! output ordering.
        integer(int64), intent(in), optional :: max_rows       !! output-size ceiling.
        logical, allocatable, intent(out), optional :: matched(:) !! per PRE-join left row.
        integer(int64), allocatable, intent(out), optional :: pairs(:)       !! per output row.
        integer(int64), allocatable, intent(out), optional :: other_pairs(:) !! per output row.
        integer, intent(in), optional :: threads    !! forwarded to the engine; absent = auto.
        character(len=:), allocatable :: onames(:), ronames(:)
        !
        call join_split_names(on, "on", onames)
        ! Split into two calls rather than passing an unallocated `ronames` through: an
        ! unallocated allocatable actual would make the dummy absent (F2018 15.5.2.12), which is
        ! the right answer here but reads as an accident rather than a decision.
        if (present(other_on)) then
            call join_split_names(other_on, "other_on", ronames)
            call join_impl(self, other, onames, other_on=ronames, how=how, columns=columns, &
                other_suffix=other_suffix, require=require, order=order, max_rows=max_rows, &
                matched=matched, pairs=pairs, other_pairs=other_pairs, threads=threads)
        else
            call join_impl(self, other, onames, how=how, columns=columns, &
                other_suffix=other_suffix, require=require, order=order, max_rows=max_rows, &
                matched=matched, pairs=pairs, other_pairs=other_pairs, threads=threads)
        end if
    end subroutine join_from_strings
    !
    !> Refuses a table joined to itself.
    !!
    !! `t%join(t, ...)` argument-associates one table with an `intent(inout)` and an `intent(in)`
    !! dummy, which F2018 15.5.2.13 forbids as soon as either is defined -- and which no compiler
    !! in this project's fleet diagnoses. Comparing the two CACHES rather than the two tables is
    !! what makes it detectable at all: every piece of a table's real state lives behind that
    !! pointer, so two `parquet_table` objects sharing one cache are one table however they were
    !! reached.
    subroutine join_check_not_self(self, other)
        class(parquet_table), intent(in) :: self  !! the left table.
        class(parquet_table), intent(in) :: other !! the right table.
        !
        if (associated(self%cache, other%cache)) then
            error stop EP // "join: a table cannot be joined to itself -- take a %clone " // &
                "first and join that, so the two sides are genuinely separate tables"
        end if
    end subroutine join_check_not_self
    !
    !> Turns `how=` into its HOW_* token, naming every accepted value when it is not one of them.
    !!
    !! Split out of `join_resolve_tokens` because `%join` needs this answer BEFORE the engine
    !! runs, while the other two vocabulary arguments are the engine's own business. It replaced
    !! a `join_check_how_supported` that refused `right`/`outer`/`semi`/`anti` by name until the
    !! column rewrite could carry them out; that refusal is gone, and so is its error scenario.
    subroutine join_resolve_how(how, how_id)
        character(len=*), intent(in), optional :: how !! the caller's `how=`, if any.
        integer, intent(out) :: how_id                !! HOW_* token; HOW_INNER when absent.
        character(len=:), allocatable :: tok
        !
        how_id = HOW_INNER
        if (.not. present(how)) return
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
    end subroutine join_resolve_how
    !
    !> Refuses a container column on THIS side of a `right`/`outer` join.
    !!
    !! Those two are the only `how` values that emit a row with no counterpart *here*, so they
    !! are the only ones that have to null-fill this table's own columns -- and the gather's mask
    !! has no arm for a container kind, whose null state is the container's own business rather
    !! than a bitmap's. The mirror of the refusal `join_plan_payload` raises for an incoming
    !! container, and stated the same way: by KIND and SIDE, never by whether this particular
    !! join happens to have an unmatched row, so that a program's join does not start failing the
    !! day its input gains one.
    !!
    !! A column that has not been READ is not checked, for the same reason it is not rewritten:
    !! `table_mutable_slots` skips it, so it never reaches the gather at all.
    subroutine join_check_left_containers(self, how_id)
        class(parquet_table), intent(in) :: self !! the left table.
        integer, intent(in) :: how_id            !! HOW_* token.
        character(len=:), allocatable :: kname, ctx
        integer :: i, kd
        !
        if (how_id /= HOW_RIGHT .and. how_id /= HOW_OUTER) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            kd = self%cache%cols(i)%declared_kind
            if (.not. parquet_kind_is_container(kd)) cycle
            call parquet_kind_name(kd, kname)
            call table_context_suffix(self%cache, trim(self%cache%cols(i)%name), ctx)
            error stop EP // "join: how='right' and how='outer' can emit a row with no " // &
                "counterpart in this table, so every column here has to be fillable with " // &
                "nulls -- and a " // kname // " column is not. Use how='inner' or how='left', " // &
                "or drop the column first" // ctx
        end do
    end subroutine join_check_left_containers
    !
    !> Checks the key-name arguments alone: how many there are, and that none of them is a SORT
    !! key. Reached from `join_impl` before the engine runs AND from `join_resolve_keys` inside
    !! it, so a caller who reaches the engine by either route gets the identical message.
    subroutine join_check_key_args(on, other_on)
        character(len=*), intent(in) :: on(:)                 !! left key columns.
        character(len=*), intent(in), optional :: other_on(:) !! right key columns.
        character(len=32) :: got, want
        integer :: j
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
        do j = 1, size(on)
            call join_check_key_name(on(j), "on")
            if (present(other_on)) call join_check_key_name(other_on(j), "other_on")
        end do
    end subroutine join_check_key_args
    !
    !> Refuses one key name that reads as a sort key rather than a column name.
    !!
    !! **Deliberately narrower than `parquet_parse_sort_key`'s own grammar**: only a leading `-`
    !! and a trailing direction WORD are refused, so a column whose name genuinely contains a
    !! blank stays usable as a join key. The sort-key parser cannot accept such a name at all --
    !! it reads everything after the first blank as a direction -- and there is no reason for the
    !! join to inherit that limitation when it has no directions to parse.
    subroutine join_check_key_name(name, argname)
        character(len=*), intent(in) :: name    !! the raw key name.
        character(len=*), intent(in) :: argname !! "on" or "other_on", for the message.
        character(len=:), allocatable :: nm, word
        integer :: sep
        logical :: bad
        !
        nm = trim(adjustl(name))
        if (len(nm) < 1) error stop EP // "join: " // argname // "= has a blank key name"
        bad = nm(1:1) == "-"
        if (.not. bad) then
            sep = index(nm, " ", back=.true.)
            if (sep > 0) then
                call join_fold(nm(sep + 1:), word)
                bad = word == "asc" .or. word == "ascending" .or. word == "desc" .or. &
                    word == "descending"
            end if
        end if
        if (bad) then
            error stop EP // "join: " // argname // "='" // nm // "' reads as a sort key, and a " // &
                "join key has no direction: a join is an equality test, so the order the keys " // &
                "are compared in is the engine's business and not the caller's. Name the column " // &
                "on its own."
        end if
    end subroutine join_check_key_name
    !
    !> Splits one separated key string into names, for the `%join` entry point that takes one.
    subroutine join_split_names(text, argname, names)
        character(len=*), intent(in) :: text                   !! "id" or "ra,dec".
        character(len=*), intent(in) :: argname                !! "on" or "other_on", for messages.
        character(len=:), allocatable, intent(out) :: names(:) !! one entry per key.
        !
        ! The library's one name-list tokenizer, so a join key list and a %prefetch column list
        ! agree about punctuation. NOT parquet_parse_sort_key: see join_check_key_name.
        call parquet_split_name_list(text, names)
        if (size(names) < 1) error stop EP // "join: " // argname // "= names no column"
    end subroutine join_split_names
    !
    !> Decides which of `other`'s columns come across and what each is called here.
    !!
    !! Runs BEFORE the engine and reads nothing: every refusal it raises is a question about names
    !! and declared kinds, and none of them is worth a sort or a hash build first. `declared_kind` is
    !! set when a table is opened, from the file's schema, so the container refusal below does not
    !! need the column read either.
    !!
    !! **`columns=` absent carries the columns of `other` that are already RESIDENT**, which is
    !! the rule a caller is most likely to be caught by and the one the guide page leads with. A
    !! freshly opened right table has none. Naming a column reads it (in `join_touch_payload`,
    !! once the join is known to be going ahead) on the principle a key column already follows:
    !! a caller who names a column has said what they want read.
    !!
    !! **A right key column whose name matches its left counterpart is NOT carried.** The two are
    !! one column and the left one is kept, so carrying the right one too would add `id_2` beside
    !! `id` holding the same values. A right key whose name DIFFERS is genuinely another column
    !! and is carried whatever `columns=` says, because the key it holds is not otherwise in the
    !! result at all.
    subroutine join_plan_payload(self, other, on, other_on, columns, other_suffix, how_id, &
            sslots, dnames, mlslots, mrslots)
        class(parquet_table), intent(in) :: self  !! the left table, for the name clashes.
        class(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: on(:)     !! left key columns.
        character(len=*), intent(in), optional :: other_on(:)  !! right key columns.
        character(len=*), intent(in), optional :: columns      !! separated payload column list.
        character(len=*), intent(in), optional :: other_suffix !! clash suffix; default "_2".
        integer, intent(in) :: how_id                          !! HOW_* token.
        integer, allocatable, intent(out) :: sslots(:)         !! right slots to carry, in order.
        character(len=:), allocatable, intent(out) :: dnames(:) !! this table's name for each.
        !> per MERGED key (one `on`/`other_on` pair naming the same column on both sides): this
        !! table's slot and `other`'s. Empty unless the two names match. `join_apply` needs both
        !! because an output row with no counterpart here has to take that key's value from
        !! `other` -- the one place the merge is not a free simplification.
        integer, allocatable, intent(out) :: mlslots(:)
        integer, allocatable, intent(out) :: mrslots(:)         !! ... and `other`'s slot for it.
        character(len=:), allocatable :: toks(:), sfx, nm, nm2, ctx, kname
        logical, allocatable :: merged(:)
        integer :: i, j, np, nm_pairs, idx, lidx, wid, kd
        !
        ! `semi` and `anti` answer a question about THIS table's rows and carry no payload at
        ! all, so a `columns=` list is a request the call cannot honour. Refused rather than
        ! ignored: a caller who named columns and got none would have no way to tell.
        if (how_id == HOW_SEMI .or. how_id == HOW_ANTI) then
            if (present(columns)) then
                error stop EP // "join: columns= cannot be used with how='semi' or how='anti'. " // &
                    "Those two keep or drop rows of this table and bring nothing across, so " // &
                    "there is no column for the list to name. Use how='inner' or how='left' to " // &
                    "carry columns, or drop columns= to select rows only."
            end if
            allocate(sslots(0))
            allocate(character(len=1) :: dnames(0))
            allocate(mlslots(0), mrslots(0))
            return
        end if
        sfx = "_2"
        if (present(other_suffix)) sfx = trim(other_suffix)
        if (len(sfx) < 1) then
            error stop EP // "join: other_suffix= is blank, so a clashing incoming column has " // &
                "no name left to take; give a suffix, or name the columns to carry with columns="
        end if
        allocate(merged(max(other%cache%ncols, 1)))
        merged = .false.
        allocate(sslots(max(other%cache%ncols, 1)))
        allocate(mlslots(size(on, kind=int64)), mrslots(size(on, kind=int64)))
        np = 0
        nm_pairs = 0
        do j = 1, size(on)
            nm = trim(on(j))
            if (present(other_on)) nm = trim(other_on(j))
            idx = table_find(other, nm)
            ! A key name the right table does not have is left for the engine to report: its
            ! message names the table and the file, and duplicating it here would be a second
            ! copy to keep in step. The same goes for one THIS table does not have, below.
            if (idx < 1) cycle
            if (nm == trim(on(j))) then
                merged(idx) = .true.
                lidx = table_find(self, trim(on(j)))
                if (lidx >= 1) then
                    nm_pairs = nm_pairs + 1
                    mlslots(nm_pairs) = lidx
                    mrslots(nm_pairs) = idx
                end if
            else
                call join_push_slot(sslots, np, idx)
            end if
        end do
        mlslots = mlslots(1:nm_pairs)
        mrslots = mrslots(1:nm_pairs)
        if (present(columns)) then
            call parquet_split_name_list(columns, toks)
            do i = 1, size(toks)
                idx = table_find(other, trim(toks(i)))
                if (idx < 1) then
                    call table_context_suffix(other%cache, trim(toks(i)), ctx)
                    error stop EP // "join: columns= names '" // trim(toks(i)) // "', which the " // &
                        "table being joined in has no column of" // ctx
                end if
                ! A MERGED key is already in the result -- as this table's own column, which is
                ! the same column -- so carrying `other`'s copy would put `id_2` beside `id`
                ! holding identical values. The residency default skips it (`if (merged(i))
                ! cycle` below) and this branch has to agree: `columns=` names what comes across
                ! IN ADDITION to the keys, not instead of them, so a caller who lists their key
                ! among their columns gets one key column and not two. Dropped rather than
                ! refused -- listing the key you joined on is a reasonable thing to write.
                ! A key whose names DIFFER is not merged, is already in `sslots` from the loop
                ! above, and `join_push_slot` is a set, so it needs no clause of its own.
                if (merged(idx)) cycle
                call join_push_slot(sslots, np, idx)
            end do
        else
            do i = 1, other%cache%ncols
                if (merged(i)) cycle
                ! The residency default. table_mutable_column is the same predicate a mutation
                ! uses to decide which of its OWN columns it can rewrite -- resident and of a
                ! supported type -- which is exactly "can this column be carried" here.
                if (.not. table_mutable_column(other, i)) cycle
                call join_push_slot(sslots, np, i)
            end do
        end if
        sslots = sslots(1:np)
        wid = 1
        do j = 1, np
            kd = other%cache%cols(sslots(j))%declared_kind
            if (parquet_kind_is_container(kd)) then
                call parquet_kind_name(kd, kname)
                call table_context_suffix(other%cache, trim(other%cache%cols(sslots(j))%name), ctx)
                error stop EP // "join: a " // kname // " column cannot be carried across a " // &
                    "join. An output row with no counterpart has to be filled with nulls, and a " // &
                    "container's own row gather has no established behaviour for that -- so the " // &
                    "refusal is by kind and side rather than by whether this particular join " // &
                    "happens to have an unmatched row. Name the columns to carry with columns=" // ctx
            end if
            if (.not. other%cache%cols(sslots(j))%supported) then
                call table_context_suffix(other%cache, trim(other%cache%cols(sslots(j))%name), ctx)
                error stop EP // "join: columns= names a column whose type this library cannot " // &
                    "read, so there is nothing to carry across" // ctx
            end if
            wid = max(wid, len_trim(other%cache%cols(sslots(j))%name) + len(sfx))
        end do
        ! Allocated at the width the longest suffixed name needs and then filled ELEMENT BY
        ! ELEMENT. Never `dnames = ""`: a whole-array assignment from a scalar reallocates a
        ! deferred-length allocatable array to the scalar's length, so that would leave every
        ! entry zero-length and each store below would write the declared width into it.
        allocate(character(len=wid) :: dnames(np))
        do j = 1, np
            nm = trim(other%cache%cols(sslots(j))%name)
            if (join_name_taken(self, dnames, j - 1, nm)) then
                nm2 = nm // sfx
                if (join_name_taken(self, dnames, j - 1, nm2)) then
                    call table_context_suffix(self%cache, "", ctx)
                    error stop EP // "join: the incoming column '" // nm // "' clashes with a " // &
                        "column already here, and so does '" // nm2 // "' under other_suffix='" // &
                        sfx // "'. Rename one of them, or leave the column behind with columns=" // ctx
                end if
                nm = nm2
            end if
            dnames(j) = nm
        end do
    end subroutine join_plan_payload
    !
    !> Appends `idx` to `sslots` unless it is already there. Keeps the carry list a SET: a right
    !! key whose name differs is carried for being a key, and `columns=` may name it as well.
    subroutine join_push_slot(sslots, np, idx)
        integer, intent(inout) :: sslots(:) !! the carry list being built.
        integer, intent(inout) :: np        !! how many entries it holds; bumped on an append.
        integer, intent(in) :: idx          !! the right slot to add.
        integer :: j
        !
        do j = 1, np
            if (sslots(j) == idx) return
        end do
        np = np + 1
        sslots(np) = idx
    end subroutine join_push_slot
    !
    !> Whether `nm` is already the name of a column here, or of one already coming across.
    logical function join_name_taken(self, dnames, n, nm) result(taken)
        class(parquet_table), intent(in) :: self !! the left table.
        character(len=*), intent(in) :: dnames(:) !! destination names decided so far.
        integer, intent(in) :: n                  !! how many of them are decided.
        character(len=*), intent(in) :: nm        !! the candidate name.
        integer :: j
        !
        taken = table_find(self, nm) > 0
        if (taken) return
        do j = 1, n
            if (trim(dnames(j)) == nm) then
                taken = .true.
                return
            end if
        end do
    end function join_name_taken
    !
    !> Reads the payload columns that are not resident yet -- the lazy first touch a caller who
    !! NAMED a column has asked for. Every column the residency default selects is resident
    !! already, so this is a no-op unless `columns=` was given.
    subroutine join_touch_payload(other, sslots)
        class(parquet_table), intent(in) :: other !! the right table.
        integer, intent(in) :: sslots(:)          !! the slots being carried.
        integer :: j, idx
        !
        do j = 1, size(sslots)
            if (table_mutable_column(other, sslots(j))) cycle
            ! table_resolve IS the first touch every value accessor goes through, so a column
            ! read here is read exactly as %get would have read it, with the same messages.
            call table_resolve(other, trim(other%cache%cols(sslots(j))%name), "join", idx)
        end do
    end subroutine join_touch_payload
    !
    !> Turns one half of the pair list into a gather index and, where anything on that side is
    !! unmatched, the validity mask that nulls those rows. Used for BOTH halves: the right half
    !! drives the incoming columns, the left half this table's own under `how="right"`/`"outer"`.
    !!
    !! An unmatched output row is given source row 1, whose values the mask then declares null and
    !! which nothing may read (a null row's value bytes are unspecified by `%init`'s contract). The
    !! alternative -- a gather that understood index 0 -- would be a new primitive across all
    !! eighteen kinds for a case two shipped bindings already express.
    subroutine join_side_index(side, n_out, nt, idx, valid)
        integer(int64), intent(in) :: side(:)                   !! per output row: that side's row, or 0.
        integer(int64), intent(in) :: n_out                     !! output rows.
        integer, intent(in) :: nt                               !! team for the two sweeps; 1 = serial.
        integer(int64), allocatable, intent(out) :: idx(:)      !! per output row: a row, never 0.
        !> per output row: .false. where it has no counterpart. LEFT UNALLOCATED when every output
        !! row matched -- an unallocated allocatable actual makes an optional dummy absent (F2018
        !! 15.5.2.12), so an inner join skips the gather's mask with no branch at the call site.
        !! `allocated(valid)` is part of this procedure's contract, and on the LEFT side
        !! it is also what tells `join_apply` whether a merged key needs rebuilding at all.
        logical, allocatable, intent(out) :: valid(:)
        integer(int64) :: o
        logical :: any_unmatched
        !
        ! Two element-wise sweeps with no shared state but the one flag, which is a reduction:
        ! each thread writes its own rows of `idx` and reads its own rows of `side`.
        allocate(idx(n_out))
        any_unmatched = .false.
        !$omp parallel do num_threads(nt) default(shared) private(o) reduction(.or.: any_unmatched) &
        !$omp schedule(static) if (nt > 1)
        do o = 1_int64, n_out
            if (side(o) == 0_int64) then
                idx(o) = 1_int64
                any_unmatched = .true.
            else
                idx(o) = side(o)
            end if
        end do
        !$omp end parallel do
        if (.not. any_unmatched) return
        allocate(valid(n_out))
        !$omp parallel do num_threads(nt) default(shared) private(o) schedule(static) if (nt > 1)
        do o = 1_int64, n_out
            valid(o) = side(o) /= 0_int64
        end do
        !$omp end parallel do
    end subroutine join_side_index
    !
    !> Whether the output leaves every one of this table's rows exactly where it was -- the
    !! computed condition the non-detaching join turns on.
    !!
    !! It is deliberately a property of the PAIR LIST rather than of `how=`, which is what
    !! `feature_risks.md` Risk-184 is about and what S1's third convention requires: a left join
    !! keeps every left row, but keeps it ONCE only when the right key is unique within each
    !! matched group, so "any left join" is the wrong rule and would leave a table attached to a
    !! file its rows no longer line up with. Being computed also means the condition costs nothing
    !! to extend -- an inner join that happens to match every row exactly once takes the same path,
    !! and so will `how="semi"` when P6 adds it, with no clause of their own.
    logical function join_keeps_left_rows(il, n_out, nl) result(kept)
        integer(int64), intent(in) :: il(:)     !! per output row: the left row it came from.
        integer(int64), intent(in) :: n_out     !! output rows.
        integer(int64), intent(in) :: nl        !! this table's rows before the join.
        integer(int64) :: o
        !
        kept = n_out == nl
        if (.not. kept) return
        do o = 1_int64, n_out
            if (il(o) /= o) then
                kept = .false.
                return
            end if
        end do
    end function join_keeps_left_rows
    !
    !> The rewrite: this table's own columns gathered by `il`, then `other`'s carried in beside
    !! them. The only procedure here that writes to `self`, which is what makes "validate
    !! everything, then mutate everything" checkable by looking at the call order in `join_impl`.
    subroutine join_apply(self, other, il, ir, n_out, sslots, dnames, mlslots, mrslots, team)
        class(parquet_table), intent(inout) :: self  !! the left table.
        class(parquet_table), intent(in) :: other    !! the right table.
        integer(int64), intent(in) :: il(:)          !! per output row: left row, or 0.
        integer(int64), intent(in) :: ir(:)          !! per output row: right row, or 0.
        integer(int64), intent(in) :: n_out          !! output rows.
        integer, intent(in) :: sslots(:)             !! right slots to carry.
        character(len=*), intent(in) :: dnames(:)    !! this table's name for each.
        integer, intent(in) :: mlslots(:)            !! merged keys: this table's slots.
        integer, intent(in) :: mrslots(:)            !! merged keys: `other`'s slots.
        integer, intent(in) :: team                  !! the engine's team, for the side-index passes.
        type(parquet_column), allocatable :: mkcols(:)
        integer(int64), allocatable :: lidx(:), ridx(:)
        logical, allocatable :: lvalid(:), rvalid(:)
        integer, allocatable :: lslots(:), dslots(:)
        integer(int64) :: nl
        integer :: j, nt
        logical :: rebuilt
        !
        nl = self%row_count
        ! The engine's own team, lowered by the tail floor to the OUTPUT rows the two passes
        ! below sweep -- an anti join's output can be a fraction of the input the team was
        ! sized for. Recorded on every join, whatever the engine.
        nt = tail_team(int(team, int64), n_out)
        call join_note_side_threads(nt)
        call join_side_index(il, n_out, nt, lidx, lvalid)
        call join_side_index(ir, n_out, nt, ridx, rvalid)
        ! THE MERGED KEYS FIRST, and that ordering is the whole of why this is a separate step:
        ! it reads this table's key columns while they still hold their OWN rows, before the
        ! gather below rewrites them. Only reachable when some output row has no left counterpart,
        ! i.e. under how='right'/'outer' with an unmatched right row -- everywhere else the key a
        ! merge would produce is the key the gather already produces.
        if (allocated(lvalid)) then
            call join_build_merged_keys(self, other, il, ir, n_out, nl, mlslots, mrslots, mkcols)
        end if
        ! THE ONE DECISION IN THIS PROCEDURE. When the output holds every one of this table's rows
        ! exactly once and in order, the join adds columns and changes nothing else: no row moves,
        ! so no column is rewritten, no unread column becomes unreadable, the slice scope still
        ! means what it did, and the file stays open. When it does not, this is a row-structural
        ! mutation like any other in parquet_tables_rowmutate.f90 and behaves like one.
        rebuilt = .not. join_keeps_left_rows(il, n_out, nl)
        if (rebuilt) then
            ! A column that has not been READ is skipped rather than read (table_mutable_slots),
            ! which is this file's approved policy and the reason the guide tells a caller to
            ! materialize what they need before a join that detaches: afterwards the detach guard
            ! is what answers. On the other branch there is nothing to skip and nothing to lose.
            call table_mutable_slots(self, lslots)
            if (size(lslots) > 0) call join_rewrite_left(self, lslots, lidx, lvalid, n_out, nl)
            self%row_count = n_out
        end if
        if (allocated(mkcols)) then
            ! After the gather, never before: the gather would otherwise overwrite the merged
            ! column with the very rows it was built to replace.
            do j = 1, size(mkcols)
                call self%cache%cols(mlslots(j))%values%move_from(mkcols(j))
            end do
        end if
        ! Every slot created SERIALLY and all of them before anything is written into one:
        ! table_new_slot reallocates cols(:) when it grows, which no thread may be holding a
        ! descriptor into.
        allocate(dslots(max(size(sslots, kind=int64), 1_int64)))
        do j = 1, size(sslots)
            call table_new_slot(self, trim(dnames(j)), idx=dslots(j))
        end do
        if (size(sslots) > 0) then
            call table_colwork_join(other%cache, self%cache, sslots, dslots(1:size(sslots)), &
                ridx, rvalid)
            do j = 1, size(sslots)
                ! Read back off the column the copy produced rather than off the source slot:
                ! deep_copy carries kind, width and unit across, and add_column_col reads them
                ! back the same way for the same reason.
                self%cache%cols(dslots(j))%declared_kind = self%cache%cols(dslots(j))%values%kindof()
                self%cache%cols(dslots(j))%width = self%cache%cols(dslots(j))%values%colwidth()
                self%cache%cols(dslots(j))%residency = RES_FULL
                self%cache%cols(dslots(j))%user_populated = .true.
            end do
        end if
        if (rebuilt) then
            self%cache%generation = self%cache%generation + 1_int64
            ! DETACHED LAST, per this layer's second rule: a mutation that fails should leave the
            ! table attached and diagnosable rather than detached and half-changed.
            call table_detach(self)
        end if
        ! On the other branch the counter is left to table_new_slot, which bumps it only when the
        ! slot array had to GROW -- %reserve_columns' published guarantee, and the whole reason an
        ! outstanding %col pointer can survive a join at all. Bumping here as well would take that
        ! back for every caller, since a join always adds at least as many slots as it carries.
    end subroutine join_apply
    !
    !> This table's own columns, gathered by `lidx` and null-filled where the row had no left
    !! counterpart, in the one pass `table_colwork`'s gather makes: `lvalid` rides beside `lidx`
    !! into `%gather(rows, valid=)`, so the mask is folded into each column's rebuild rather than
    !! applied in a second pass afterwards (until stage 4 of `feature_join.md` it was a serial
    !! `%set_validity` per column after the threaded gather). It only ever ADDS nulls, which is
    !! what leaves this table's own nulls exactly where the gather put them (`feature_risks.md`
    !! Risk-182). The mask exists only under `how="right"`/`"outer"` with an unmatched right row.
    subroutine join_rewrite_left(self, lslots, lidx, lvalid, n_out, nl)
        class(parquet_table), intent(inout) :: self   !! the left table.
        integer, intent(in) :: lslots(:)              !! its rewritable slots.
        integer(int64), intent(in) :: lidx(:)         !! per output row: left row, never 0.
        logical, allocatable, intent(in) :: lvalid(:) !! unmatched-row mask, or unallocated.
        integer(int64), intent(in) :: n_out           !! output rows.
        integer(int64), intent(in) :: nl              !! this table's rows before the join.
        integer :: j
        !
        if (nl < 1_int64) then
            ! A table with no rows has no row 1 for `lidx` to have named, so it cannot be gathered
            ! at all -- every output row is unmatched by construction. Appending null rows gives
            ! the same result and keeps each column's kind, width and unit. Reachable whenever an
            ! EMPTY left table is joined with how='right' or how='outer', which is the mirror of
            ! join_one_column's own zero-row guard on the incoming side.
            if (n_out > 0_int64) then
                do j = 1, size(lslots)
                    call self%cache%cols(lslots(j))%values%append_nulls(n_out)
                end do
            end if
            return
        end if
        ! An unallocated `lvalid` reaches `valid=` as an absent argument (F2018 15.5.2.12), which
        ! is how a join with no unmatched row here skips the mask without a branch.
        call table_colwork(self%cache, PCW_GATHER, lslots, rows=lidx, valid=lvalid)
    end subroutine join_rewrite_left
    !
    !> One merged key column per `on`/`other_on` pair naming the same column on both sides, taking
    !! this table's value where the output row has one and `other`'s where it does not.
    !!
    !! **Built by gathering the CONCATENATION of the two key columns**, which is the same object
    !! `join_build_keys` hands the sort: left rows 1..nl followed by right rows nl+1..nl+nr. An
    !! output row's merged index is then `il(o)` or `nl + ir(o)`, and one `%gather` produces the
    !! whole column -- with each row's nulls carried across for free, which matters because an
    !! unmatched right row may perfectly well have a null key. The alternative, patching the
    !! gathered left column at scattered positions, has no primitive at all: `%paste` copies a
    !! contiguous range, and the unmatched rows are contiguous only under `order="left"`.
    !!
    !! Reached only when some output row has no left counterpart, so an inner or left join pays
    !! nothing for it.
    subroutine join_build_merged_keys(self, other, il, ir, n_out, nl, mlslots, mrslots, mkcols)
        class(parquet_table), intent(in) :: self  !! the left table.
        class(parquet_table), intent(in) :: other !! the right table.
        integer(int64), intent(in) :: il(:)       !! per output row: left row, or 0.
        integer(int64), intent(in) :: ir(:)       !! per output row: right row, or 0.
        integer(int64), intent(in) :: n_out       !! output rows.
        integer(int64), intent(in) :: nl          !! this table's rows before the join.
        integer, intent(in) :: mlslots(:)         !! merged keys: this table's slots.
        integer, intent(in) :: mrslots(:)         !! merged keys: `other`'s slots.
        !> one rebuilt key column per pair, in the same order. LEFT UNALLOCATED when there is no
        !! merged key at all, which `allocated()` is what the caller tests.
        type(parquet_column), allocatable, intent(out) :: mkcols(:)
        integer(int64), allocatable :: mi(:)
        integer(int64) :: o
        integer :: k
        !
        if (size(mlslots) < 1) return
        allocate(mi(n_out))
        do o = 1_int64, n_out
            if (il(o) /= 0_int64) then
                mi(o) = il(o)
            else
                mi(o) = nl + ir(o)
            end if
        end do
        allocate(mkcols(size(mlslots, kind=int64)))
        do k = 1, size(mlslots)
            call self%cache%cols(mlslots(k))%values%deep_copy(mkcols(k))
            call mkcols(k)%append(other%cache%cols(mrslots(k))%values)
            call mkcols(k)%gather(mi)
        end do
    end subroutine join_build_merged_keys
    !
    !> Phase A of the sort engine: one concatenated `parquet_column` per join key, added to
    !! `skeys` in order.
    !!
    !! `kc` is returned rather than kept local because the group-nullness test in phase C reads
    !! it -- a `pf_sort_keys` does not hand its keys back, and re-deriving nullness from the two
    !! source columns would mean mapping every group member back across the concatenation.
    !!
    !! The slots arrive resolved and kind-checked from `join_resolve_keys`, so nothing here can
    !! refuse: `%append` would refuse a kind mismatch too, but with a message naming two columns
    !! where the resolver's names two columns, two kinds and two files -- and a future permissive
    !! `%append` would let the concatenation succeed with an int32 key silently compared against
    !! an int64 one, which is why the check lives ahead of both engines rather than here.
    subroutine join_build_keys(self, other, lslots, rslots, skeys, kc)
        class(parquet_table), intent(in) :: self  !! the left table.
        class(parquet_table), intent(in) :: other !! the right table.
        integer, intent(in) :: lslots(:)          !! this table's key slots, primary first.
        integer, intent(in) :: rslots(:)          !! `other`'s key slots, one per left one.
        type(pf_sort_keys), intent(out) :: skeys  !! the assembled key list.
        type(parquet_column), allocatable, intent(out) :: kc(:) !! one concatenated key per entry.
        integer :: j
        !
        allocate(kc(size(lslots, kind=int64)))
        do j = 1, size(lslots)
            call self%cache%cols(lslots(j))%values%deep_copy(kc(j))
            call kc(j)%append(other%cache%cols(rslots(j))%values)
            call skeys%add(kc(j))
        end do
    end subroutine join_build_keys
    !
    !> Resolves every key name on both sides to its slot and checks each pair's kind and width:
    !! the ONE place both engines take their key columns from.
    !!
    !! `table_lookup_sort_key` is the sorts' own resolver, which brings the refusal of every
    !! unorderable kind, the lazy first touch of a key column and the file-naming message suffix
    !! with it -- so a join accepts exactly the columns `%sort_by` does. Also reached from
    !! `join_impl` before the engine runs, through `join_check_key_args`, so a malformed key list
    !! is refused before anything is read as well as here, and both routes raise the identical
    !! message.
    subroutine join_resolve_keys(self, other, on, other_on, lslots, rslots)
        class(parquet_table), intent(in) :: self  !! the left table.
        class(parquet_table), intent(in) :: other !! the right table.
        character(len=*), intent(in) :: on(:)     !! left key columns, primary first.
        character(len=*), intent(in), optional :: other_on(:) !! right key columns.
        integer, allocatable, intent(out) :: lslots(:) !! this table's key slots, in `on`'s order.
        integer, allocatable, intent(out) :: rslots(:) !! `other`'s key slots, one per left one.
        character(len=:), allocatable :: rname
        integer :: j
        !
        call join_check_key_args(on, other_on)
        allocate(lslots(size(on, kind=int64)), rslots(size(on, kind=int64)))
        do j = 1, size(on)
            if (present(other_on)) then
                rname = trim(other_on(j))
            else
                rname = trim(on(j))
            end if
            call table_lookup_sort_key(self, on(j), "join", lslots(j), key_kind="join")
            call table_lookup_sort_key(other, rname, "join", rslots(j), key_kind="join")
            call join_check_key_kinds(self, other, on(j), rname, lslots(j), rslots(j))
        end do
    end subroutine join_resolve_keys
    !
    !> Which engine builds this call's pair list.
    !!
    !! **The rule: the hash engine whenever the call is eligible, the sort engine otherwise.** It
    !! is a function of the key kinds and `order=` alone (`join_hash_eligible`) -- never of the
    !! data's values, and not of the row counts either -- so the same call takes the same engine
    !! on every run, on every input and at every thread count (feature_risks.md Risk-218). There
    !! is deliberately no size clause: the sweep recorded in feature_join.md's stage 3 found the
    !! hash engine ahead of the sort engine on both compilers at every size down to a thousand
    !! rows against ten, so no shape exists at which a build plus a probe loses to the sort's
    !! counting path, and a clause for one would be a second rule with nothing to select.
    !!
    !! The test-only hook is read ONCE, here, and outranks nothing it should not: forcing the
    !! sort engine takes it whatever the call; forcing the hash engine takes it only where
    !! `join_hash_eligible` allows, and otherwise the sort engine runs and the observable says
    !! so -- which is what lets one test suite run twice, once per forced mode, over every join
    !! it holds, without a single test having to know which of its calls the hash engine can
    !! take. A forced engine that ABORTED on an ineligible call would instead make every such
    !! test fail under the second run, and one that BYPASSED the rule would run the multimap over
    !! a tuple it cannot represent.
    integer function join_choose_engine(self, lslots, ord_id) result(engine)
        class(parquet_table), intent(in) :: self !! the left table, whose key kinds decide.
        integer, intent(in) :: lslots(:)         !! this table's key slots.
        integer, intent(in) :: ord_id            !! ORD_* token.
        !
        engine = ENGINE_SORT
        if (join_engine_mode() == int(ENGINE_SORT, int64)) return
        if (join_hash_eligible(self, lslots, ord_id)) engine = ENGINE_HASH
    end function join_choose_engine
    !
    !> Whether the hash engine can take this call: `order="left"`, and every key column of a
    !! kind `index_extract_keys` converts -- or a single string key, which the multimap keys in
    !! place.
    !!
    !! The three exclusions are decisions rather than gaps (feature_join.md, section 11
    !! questions 3 and 4): `order="key"` has no hash-side equivalent short of a second sort; a
    !! `PK_LOGICAL` key is two groups, where the sort's counting path is already O(n); and a
    !! string key beside another key would need a string-and-integer tuple the multimap does not
    !! have. The kinds are already checked equal across the two sides, so the left ones decide.
    logical function join_hash_eligible(self, lslots, ord_id) result(ok)
        class(parquet_table), intent(in) :: self !! the left table.
        integer, intent(in) :: lslots(:)         !! this table's key slots.
        integer, intent(in) :: ord_id            !! ORD_* token.
        integer :: j, ncomp
        !
        ok = .false.
        if (ord_id /= ORD_LEFT) return
        if (size(lslots) == 1) then
            if (self%cache%cols(lslots(1))%values%kindof() == PK_STRING) then
                ok = .true.
                return
            end if
        end if
        ncomp = 0
        do j = 1, size(lslots)
            select case (self%cache%cols(lslots(j))%values%kindof())
            case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_DATE, PK_TIME)
                ncomp = ncomp + 1
            case (PK_TIMESTAMP)
                ! Two components, the unit-free (seconds, nanoseconds) pair.
                ncomp = ncomp + 2
            case default
                ! PK_LOGICAL, or a string beside another key: the sort engine's.
                return
            end select
        end do
        ok = ncomp <= pf_index_max_components
    end function join_hash_eligible
    !
    !> The engine the test-only hook asks for: 0 for automatic, else an ENGINE_* token.
    !!
    !! One `bind(C)` read per join, a coarse operation, through a local interface so that the
    !! hook stays out of `src/parquet_bindings.f90` and out of the library's own interface
    !! (.claude/rules/cpp-wrapper.md, "Debug hooks"). `colwork_gate_limits` in
    !! `parquet_tables_parallel.f90` is the same shape for the same reason.
    function join_engine_mode() result(mode)
        use iso_c_binding, only : c_int64_t
        integer(int64) :: mode !! 0, ENGINE_SORT or ENGINE_HASH.
        interface
            function get_mode() result(res) bind(C, name="parquet_debug_get_join_engine")
                import :: c_int64_t
                integer(c_int64_t) :: res
            end function get_mode
        end interface
        !
        mode = int(get_mode(), int64)
    end function join_engine_mode
    !
    !> Records which engine ran, for `parquet_debug_join_engine_used`. Called at the top of BOTH
    !! engine bodies, never from the dispatcher, so that the fallback route writes it too.
    subroutine join_note_engine(engine)
        use iso_c_binding, only : c_int64_t
        integer, intent(in) :: engine !! ENGINE_* token.
        interface
            subroutine set_used(e) bind(C, name="parquet_debug_set_join_engine_used")
                import :: c_int64_t
                integer(c_int64_t), value :: e
            end subroutine set_used
        end interface
        !
        call set_used(int(engine, c_int64_t))
    end subroutine join_note_engine
    !
    !> Records the team the sort engine's passes over the runs ran on, for
    !! `parquet_debug_get_join_group_threads_used`. Written by the sort engine's body on every
    !! route, 1 included, and as 0 by the hash engine, which has no such passes -- so a test
    !! reading it after a hash-engine join sees that, not a stale team from an earlier join.
    subroutine join_note_group_threads(nt)
        use iso_c_binding, only : c_int64_t
        integer, intent(in) :: nt !! the team; 0 for the hash engine.
        interface
            subroutine set_group(n) bind(C, name="parquet_debug_set_join_group_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: n
            end subroutine set_group
        end interface
        !
        call set_group(int(nt, c_int64_t))
    end subroutine join_note_group_threads
    !
    !> Records the team `join_apply`'s two side-index passes ran on, for
    !! `parquet_debug_get_join_side_threads_used`. Written on every join, whatever the engine.
    subroutine join_note_side_threads(nt)
        use iso_c_binding, only : c_int64_t
        integer, intent(in) :: nt !! the team; 1 means serial.
        interface
            subroutine set_side(n) bind(C, name="parquet_debug_set_join_side_threads_used")
                import :: c_int64_t
                integer(c_int64_t), value :: n
            end subroutine set_side
        end interface
        !
        call set_side(int(nt, c_int64_t))
    end subroutine join_note_side_threads
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
    !!
    !! **Serial in one pass, or on a team in three** (`nt`), and the two give the same eleven
    !! results element for element -- `a join's group passes and its side index run on the
    !! join's team` (`test/test_table_parallel.f90`) is the A/B, on every `how`.
    subroutine join_classify(kc, perm, go, nl, nr, ngroups, nt, nleft, nright, lmatched, rmatched, &
            group_of_left, lg_off, lg_idx, rg_off, rg_idx)
        type(parquet_column), intent(in) :: kc(:)   !! the concatenated key columns.
        integer(int64), intent(in) :: perm(:)       !! the concatenation's permutation.
        integer(int64), intent(in) :: go(:)         !! group offsets into `perm`, length ngroups+1.
        integer(int64), intent(in) :: nl            !! left rows.
        integer(int64), intent(in) :: nr            !! right rows.
        integer(int64), intent(in) :: ngroups       !! number of groups.
        integer, intent(in) :: nt                   !! team; 1 is the single serial pass.
        integer(int64), allocatable, intent(out) :: nleft(:)  !! left rows per group.
        integer(int64), allocatable, intent(out) :: nright(:) !! right rows per group.
        logical, allocatable, intent(out) :: lmatched(:) !! per group: its left rows found a match.
        logical, allocatable, intent(out) :: rmatched(:) !! per group: its right rows found a match.
        integer(int64), allocatable, intent(out) :: group_of_left(:) !! per left row: its group.
        integer(int64), allocatable, intent(out) :: lg_off(:) !! group -> slice of lg_idx.
        integer(int64), allocatable, intent(out) :: lg_idx(:) !! left rows, grouped, ascending.
        integer(int64), allocatable, intent(out) :: rg_off(:) !! group -> slice of rg_idx.
        integer(int64), allocatable, intent(out) :: rg_idx(:) !! right rows, grouped, ascending.
        integer(int64) :: g, t, p, cl, cr, lc, rc, c, tl, tr
        integer(int64), allocatable :: glo(:), ghi(:), tot_l(:), tot_r(:)
        logical, allocatable :: hasnull(:)
        integer :: j
        logical :: isnull, anynull
        !
        allocate(nleft(max(ngroups, 1_int64)), nright(max(ngroups, 1_int64)))
        allocate(lmatched(max(ngroups, 1_int64)), rmatched(max(ngroups, 1_int64)))
        allocate(group_of_left(max(nl, 0_int64)))
        allocate(lg_off(ngroups + 1_int64), rg_off(ngroups + 1_int64))
        allocate(lg_idx(max(nl, 0_int64)), rg_idx(max(nr, 0_int64)))
        lg_off(1) = 1_int64
        rg_off(1) = 1_int64
        ! Asked once per key column rather than once per group: a column with no null at all
        ! cannot make any group null-bearing, and on a key without nulls -- the usual case --
        ! this removes one `parquet_column_is_null` per group, which was 1.7% of a 10M x 10M
        ! join. On a temporal kind whose cached answer is stale it is one serial sweep, made
        ! here rather than by whichever thread asks first inside the team below.
        allocate(hasnull(size(kc, kind=int64)))
        do j = 1, size(kc)
            hasnull(j) = parquet_column_any_null(kc(j))
        end do
        ! And hoisted once more: with no null in any key column no group can be null-bearing,
        ! so neither arm below makes the call at all -- measured at 5% of a 10M-row lookup join
        ! at one thread as a call per group that looped over `hasnull` and returned.
        anynull = any(hasnull)
        if (nt <= 1 .or. ngroups < int(nt, int64)) then
            ! ONE pass, not two. `lg_off(g)` and `rg_off(g)` are already final when group `g` is
            ! entered -- the previous iteration set them -- so each group can fill its own slice of
            ! `lg_idx`/`rg_idx` as it counts, instead of a second walk re-reading `perm` afterwards.
            ! `perm(t)` is a scattered read over nl+nr elements, so the second walk was a full extra
            ! random-access pass over the whole concatenation; removing it is worth several percent
            ! of the join on a high-cardinality key, where there is nearly one group per row.
            do g = 1_int64, ngroups
                cl = 0_int64
                cr = 0_int64
                lc = lg_off(g) - 1_int64
                rc = rg_off(g) - 1_int64
                do t = go(g), go(g + 1_int64) - 1_int64
                    p = perm(t)
                    if (p <= nl) then
                        cl = cl + 1_int64
                        group_of_left(p) = g
                        lc = lc + 1_int64
                        lg_idx(lc) = p
                    else
                        cr = cr + 1_int64
                        rc = rc + 1_int64
                        rg_idx(rc) = p - nl
                    end if
                end do
                nleft(g) = cl
                nright(g) = cr
                isnull = .false.
                if (anynull) isnull = join_group_null(kc, hasnull, perm(go(g)))
                ! A null-bearing group matches nothing in either direction: a null is UNKNOWN, and
                ! `unknown = unknown` is not true. Its left rows are unmatched left rows and its
                ! right rows are unmatched right rows.
                lmatched(g) = (.not. isnull) .and. cr > 0_int64
                rmatched(g) = (.not. isnull) .and. cl > 0_int64
                lg_off(g + 1_int64) = lg_off(g) + cl
                rg_off(g + 1_int64) = rg_off(g) + cr
            end do
            return
        end if
        ! ON A TEAM, three passes over contiguous ranges of groups, one range per thread, cut by
        ! ROWS of `perm` rather than by group count so that a run of duplicates costs its thread
        ! no more than its share of the concatenation (`join_chunk_groups`). Pass 1 walks each
        ! group's slice of `perm` for its two counts, writes `group_of_left` (every left row is
        ! named exactly once in `perm`, so no two threads write one element), settles the null
        ! test, and totals its range's left and right rows. A serial prefix over the ranges then
        ! gives each its first slot in `lg_idx`/`rg_idx`, and pass 3 walks `perm` again to fill
        ! them and to write `lg_off`/`rg_off` for its own groups; the sentinels are set once at
        ! the end. The second walk over `perm` is what the single pass above avoids -- on a team
        ! it is a sequential sweep split `nt` ways, and the scatter into `group_of_left` is paid
        ! once, not twice.
        call join_chunk_groups(go, ngroups, nl + nr, nt, glo, ghi)
        allocate(tot_l(nt), tot_r(nt))
        !$omp parallel do num_threads(nt) default(shared) private(c, g, t, p, cl, cr, tl, tr, isnull) &
        !$omp schedule(static)
        do c = 1_int64, int(nt, int64)
            tl = 0_int64
            tr = 0_int64
            do g = glo(c), ghi(c)
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
                if (anynull) isnull = join_group_null(kc, hasnull, perm(go(g)))
                lmatched(g) = (.not. isnull) .and. cr > 0_int64
                rmatched(g) = (.not. isnull) .and. cl > 0_int64
                tl = tl + cl
                tr = tr + cr
            end do
            tot_l(c) = tl
            tot_r(c) = tr
        end do
        !$omp end parallel do
        lc = 0_int64
        rc = 0_int64
        do c = 1_int64, int(nt, int64)
            tl = tot_l(c)
            tr = tot_r(c)
            tot_l(c) = lc
            tot_r(c) = rc
            lc = lc + tl
            rc = rc + tr
        end do
        !$omp parallel do num_threads(nt) default(shared) private(c, g, t, p, lc, rc) schedule(static)
        do c = 1_int64, int(nt, int64)
            lc = tot_l(c)
            rc = tot_r(c)
            do g = glo(c), ghi(c)
                lg_off(g) = lc + 1_int64
                rg_off(g) = rc + 1_int64
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
        end do
        !$omp end parallel do
        ! Every left row is in exactly one group, and so is every right row.
        lg_off(ngroups + 1_int64) = nl + 1_int64
        rg_off(ngroups + 1_int64) = nr + 1_int64
    end subroutine join_classify
    !
    !> Whether the group whose first member is `row` is null-bearing: any key column that holds
    !! a null at all (`hasnull`) is asked about that row. Shared by both arms of `join_classify`
    !! so the two cannot answer differently.
    logical function join_group_null(kc, hasnull, row) result(isnull)
        type(parquet_column), intent(in) :: kc(:)  !! the concatenated key columns.
        logical, intent(in) :: hasnull(:)          !! per key column: it holds at least one null.
        integer(int64), intent(in) :: row          !! the group's first member, a concatenation row.
        integer :: j
        !
        isnull = .false.
        do j = 1, size(kc)
            if (.not. hasnull(j)) cycle
            if (parquet_column_is_null(kc(j), row)) then
                isnull = .true.
                return
            end if
        end do
    end function join_group_null
    !
    !> Cuts groups `1..ngroups` into `nt` contiguous ranges of about equal ROW count: range `c`
    !! starts at the first group whose slice of `perm` begins at or past row `(c-1)*n/nt + 1`,
    !! found by binary search over `go`. A range may be empty (`glo > ghi`) when one group spans
    !! several cut points; the loops over it then do not run, and the prefix passes carry a 0.
    subroutine join_chunk_groups(go, ngroups, n, nt, glo, ghi)
        integer(int64), intent(in) :: go(:)                    !! group offsets, length ngroups+1.
        integer(int64), intent(in) :: ngroups                  !! number of groups.
        integer(int64), intent(in) :: n                        !! rows of `perm`, `go(ngroups+1) - 1`.
        integer, intent(in) :: nt                              !! ranges to cut.
        integer(int64), allocatable, intent(out) :: glo(:)     !! per range: its first group.
        integer(int64), allocatable, intent(out) :: ghi(:)     !! per range: its last group.
        integer(int64) :: c, target, lo, hi, mid
        !
        allocate(glo(nt), ghi(nt))
        glo(1) = 1_int64
        do c = 2_int64, int(nt, int64)
            target = ((c - 1_int64) * n) / int(nt, int64) + 1_int64
            ! The smallest g at or after the previous range's start with go(g) >= target; the
            ! sentinel go(ngroups+1) = n + 1 is always past `target`, so the search ends by then.
            lo = glo(c - 1_int64)
            hi = ngroups + 1_int64
            do while (lo < hi)
                mid = (lo + hi) / 2_int64
                if (go(mid) < target) then
                    lo = mid + 1_int64
                else
                    hi = mid
                end if
            end do
            glo(c) = lo
        end do
        do c = 1_int64, int(nt, int64) - 1_int64
            ghi(c) = glo(c + 1_int64) - 1_int64
        end do
        ghi(nt) = ngroups
    end subroutine join_chunk_groups
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
            rg_off, rg_idx, ngroups, nt)
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
        integer, intent(in) :: nt                !! team; 1 is the serial loop.
        integer(int64) :: g, c, glo, ghi, bad_l, bad_r
        integer(int64), allocatable :: first_l(:), first_r(:)
        !
        if (req_id == REQ_MM) return
        ! Each thread notes the FIRST group of its range that violates each side and stops there;
        ! the abort is raised after the region for the lowest of them -- the group the serial
        ! loop stopped at, with the left side's message where one group violates both, exactly
        ! as the order of the two tests inside the loop gives. An `error stop` inside the region
        ! would name whichever thread got there first.
        !
        ! A null-bearing group has lmatched .false. AND cannot violate anything, so the
        ! same test excludes both it and a group whose right side is empty. The latter
        ! genuinely has duplicate left keys -- but with nothing to match, they contribute
        ! no output row at all, so they cannot produce the wrong-sized result this
        ! assertion exists to catch. That is the whole reason, and it is worth stating
        ! plainly: `require=` asserts something about the rows that TOOK PART in the
        ! match, not about either table's own uniqueness, so its outcome legitimately
        ! depends on both tables. (An earlier version of this comment argued the reverse
        ! -- that refusing them would make the assertion depend on the other table --
        ! which is backwards and invites a reader to "fix" the lmatched test away.)
        allocate(first_l(nt), first_r(nt))
        !$omp parallel do num_threads(nt) default(shared) private(c, g, glo, ghi, bad_l, bad_r) &
        !$omp schedule(static) if (nt > 1)
        do c = 1_int64, int(nt, int64)
            glo = ((c - 1_int64) * ngroups) / int(nt, int64) + 1_int64
            ghi = (c * ngroups) / int(nt, int64)
            bad_l = 0_int64
            bad_r = 0_int64
            do g = glo, ghi
                if (req_id == REQ_11 .or. req_id == REQ_1M) then
                    if (lmatched(g) .and. nleft(g) > 1_int64) bad_l = g
                end if
                if (req_id == REQ_11 .or. req_id == REQ_M1) then
                    if (rmatched(g) .and. nright(g) > 1_int64) bad_r = g
                end if
                if (bad_l /= 0_int64 .or. bad_r /= 0_int64) exit
            end do
            first_l(c) = bad_l
            first_r(c) = bad_r
        end do
        !$omp end parallel do
        bad_l = 0_int64
        bad_r = 0_int64
        do c = 1_int64, int(nt, int64)
            if (first_l(c) /= 0_int64 .or. first_r(c) /= 0_int64) then
                bad_l = first_l(c)
                bad_r = first_r(c)
                exit
            end if
        end do
        if (bad_l == 0_int64 .and. bad_r == 0_int64) return
        if (bad_l /= 0_int64 .and. (bad_r == 0_int64 .or. bad_l <= bad_r)) then
            call join_refuse_require("1:m", "left", "on=", lg_idx(lg_off(bad_l)), &
                lg_idx(lg_off(bad_l) + 1_int64), nleft(bad_l))
        else
            call join_refuse_require("m:1", "right", "other_on=", rg_idx(rg_off(bad_r)), &
                rg_idx(rg_off(bad_r) + 1_int64), nright(bad_r))
        end if
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
    subroutine join_count(how_id, nleft, nright, lmatched, rmatched, ngroups, nl, nt, &
            n_pairs, n_lunm, n_runm, biggest, n_out)
        integer, intent(in) :: how_id            !! HOW_* token.
        integer(int64), intent(in) :: nleft(:)   !! left rows per group.
        integer(int64), intent(in) :: nright(:)  !! right rows per group.
        logical, intent(in) :: lmatched(:)       !! per group: its left rows found a match.
        logical, intent(in) :: rmatched(:)       !! per group: its right rows found a match.
        integer(int64), intent(in) :: ngroups    !! number of groups.
        integer(int64), intent(in) :: nl         !! left rows.
        integer, intent(in) :: nt                !! team; 1 is the serial loop.
        integer(int64), intent(out) :: n_pairs   !! matched left-right pairs.
        integer(int64), intent(out) :: n_lunm    !! left rows with no counterpart.
        integer(int64), intent(out) :: n_runm    !! right rows with no counterpart.
        integer(int64), intent(out) :: biggest   !! the largest group's pair count.
        integer(int64), intent(out) :: n_out     !! rows this `how` will emit.
        integer(int64) :: g, prod, c, glo, ghi, pp, pl, pr, pb, bad
        integer(int64), allocatable :: part_pairs(:), part_lunm(:), part_runm(:), part_big(:), part_bad(:)
        character(len=32) :: tl, tr
        !
        ! One contiguous range of groups per thread, each summed on its own through the same
        ! guarded addition the total then goes through, so no addition anywhere is unguarded;
        ! the overflowing PRODUCT is noted rather than raised inside the region, and raised
        ! afterwards for the lowest such group, as the serial loop would have.
        allocate(part_pairs(nt), part_lunm(nt), part_runm(nt), part_big(nt), part_bad(nt))
        !$omp parallel do num_threads(nt) default(shared) private(c, g, glo, ghi, pp, pl, pr, pb, bad, prod) &
        !$omp schedule(static) if (nt > 1)
        do c = 1_int64, int(nt, int64)
            glo = ((c - 1_int64) * ngroups) / int(nt, int64) + 1_int64
            ghi = (c * ngroups) / int(nt, int64)
            pp = 0_int64
            pl = 0_int64
            pr = 0_int64
            pb = 0_int64
            bad = 0_int64
            do g = glo, ghi
                if (lmatched(g)) then
                    ! Guarded rather than trusted: a wrapped product would make n_out negative,
                    ! the allocation fail somewhere unrelated, and the cause invisible. The
                    ! division is one per group and only on the groups that contribute pairs.
                    if (nleft(g) > huge(0_int64) / nright(g)) then
                        if (bad == 0_int64) bad = g
                        cycle
                    end if
                    prod = nleft(g) * nright(g)
                    ! The SUM is guarded as well as the product: n_lunm and n_runm are bounded by
                    ! the two row counts, but n_pairs is not, and a wrapped total would make n_out
                    ! negative and the join return an empty pair list without a word.
                    pp = join_add_checked(pp, prod)
                    if (prod > pb) pb = prod
                else
                    pl = pl + nleft(g)
                end if
                if (.not. rmatched(g)) pr = pr + nright(g)
            end do
            part_pairs(c) = pp
            part_lunm(c) = pl
            part_runm(c) = pr
            part_big(c) = pb
            part_bad(c) = bad
        end do
        !$omp end parallel do
        do c = 1_int64, int(nt, int64)
            if (part_bad(c) /= 0_int64) then
                g = part_bad(c)
                write(tl, "(I0)") nleft(g)
                write(tr, "(I0)") nright(g)
                error stop EP // "join: one key value has " // trim(tl) // " rows on the " // &
                    "left and " // trim(tr) // " on the right; their product overflows a " // &
                    "64-bit row count. Deduplicate a side, or add require='m:1'."
            end if
        end do
        n_pairs = 0_int64
        n_lunm = 0_int64
        n_runm = 0_int64
        biggest = 0_int64
        do c = 1_int64, int(nt, int64)
            n_pairs = join_add_checked(n_pairs, part_pairs(c))
            n_lunm = n_lunm + part_lunm(c)
            n_runm = n_runm + part_runm(c)
            if (part_big(c) > biggest) biggest = part_big(c)
        end do
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
    end subroutine join_count
    !
    !> `a + b` over two row counts, refusing the sum that would wrap.
    !!
    !! `join_count` refuses a group whose PRODUCT would overflow; this is the same refusal on the
    !! running SUM and on the `n_out` totals, which used to accumulate unguarded: a wrapped total
    !! made `n_out` negative, `allocate(il(n_out), ir(n_out))` gave two zero-size arrays, and the
    !! join returned an empty pair list with no error. Unreachable through any fixture -- it needs
    !! more than 9.2e18 pairs across several groups -- which is exactly why it is a separate
    !! helper: `parquet_debug_join_add_checked` reaches it from a test at the boundary. Both
    !! arguments are counts, so `huge - a` cannot itself overflow. Deliberately not `pure`, so
    !! that no compiler may elide the call.
    function join_add_checked(a, b) result(s)
        integer(int64), intent(in) :: a  !! the running total (>= 0).
        integer(int64), intent(in) :: b  !! the count to add (>= 0).
        integer(int64) :: s              !! `a + b`.
        character(len=32) :: ta, tb
        !
        if (huge(0_int64) - a < b) then
            write(ta, "(I0)") a
            write(tb, "(I0)") b
            error stop EP // "join: the output would have more rows than a 64-bit count can " // &
                "hold (" // trim(ta) // " so far, plus " // trim(tb) // "). Deduplicate a side, " // &
                "add require='m:1', or pass max_rows=."
        end if
        s = a + b
    end function join_add_checked
    !
    module procedure parquet_debug_join_add_checked
        s = join_add_checked(a, b)
    end procedure parquet_debug_join_add_checked
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
    subroutine join_fill_matched(group_of_left, lmatched, nl, nt, matched)
        integer(int64), intent(in) :: group_of_left(:) !! per left row: its group.
        logical, intent(in) :: lmatched(:)             !! per group: its left rows found a match.
        integer(int64), intent(in) :: nl               !! left rows.
        integer, intent(in) :: nt                      !! team; 1 is the serial loop.
        logical, intent(out) :: matched(:)             !! per left row: it found a counterpart.
        integer(int64) :: i
        !
        !$omp parallel do num_threads(nt) default(shared) private(i) schedule(static) if (nt > 1)
        do i = 1_int64, nl
            matched(i) = lmatched(group_of_left(i))
        end do
        !$omp end parallel do
    end subroutine join_fill_matched
    !
    !> Phase D, `order="left"`: the left table's rows in their own order, matches within each.
    !!
    !! This is STILTS' and pandas' ordering, and it is what makes an array a caller computed
    !! against the pre-join left table still line up row for row when the join is one-to-at-most-
    !! one. Unmatched RIGHT rows have no place in a left-major walk, so they follow at the end in
    !! right-table order.
    subroutine join_emit_left_order(how_id, nl, group_of_left, lmatched, rmatched, &
            rg_off, rg_idx, ngroups, nt, il, ir)
        integer, intent(in) :: how_id                  !! HOW_* token.
        integer(int64), intent(in) :: nl               !! left rows.
        integer(int64), intent(in) :: group_of_left(:) !! per left row: its group.
        logical, intent(in) :: lmatched(:)             !! per group: its left rows found a match.
        logical, intent(in) :: rmatched(:)             !! per group: its right rows found a match.
        integer(int64), intent(in) :: rg_off(:)        !! group -> slice of rg_idx.
        integer(int64), intent(in) :: rg_idx(:)        !! right rows, grouped, ascending.
        integer(int64), intent(in) :: ngroups          !! number of groups.
        integer, intent(in) :: nt                      !! team; 1 is the single-cursor loop.
        integer(int64), intent(out) :: il(:)           !! per output row: left row, or 0.
        integer(int64), intent(out) :: ir(:)           !! per output row: right row, or 0.
        integer(int64) :: i, g, t, o, c, lo, hi, cnt
        integer(int64), allocatable :: base(:)
        !
        if (nt <= 1 .or. nl < int(nt, int64)) then
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
        else
            ! ON A TEAM: one contiguous range of left rows per thread, its output rows counted
            ! first (the rule below is the one the single-cursor loop above applies), a serial
            ! prefix over the ranges giving each its first output slot, then each range emitted
            ! by its own cursor into its own slice. The count is a second sweep over the range,
            ! reading `lmatched` and `rg_off` through `group_of_left` as the fill does -- on a
            ! team it is split `nt` ways, which is why the serial arm is not written this way.
            allocate(base(nt))
            !$omp parallel do num_threads(nt) default(shared) private(c, lo, hi, i, g, cnt) schedule(static)
            do c = 1_int64, int(nt, int64)
                lo = ((c - 1_int64) * nl) / int(nt, int64) + 1_int64
                hi = (c * nl) / int(nt, int64)
                cnt = 0_int64
                do i = lo, hi
                    g = group_of_left(i)
                    if (lmatched(g)) then
                        if (how_id == HOW_ANTI) cycle
                        if (how_id == HOW_SEMI) then
                            cnt = cnt + 1_int64
                        else
                            cnt = cnt + (rg_off(g + 1_int64) - rg_off(g))
                        end if
                    else
                        if (how_id == HOW_INNER .or. how_id == HOW_RIGHT .or. how_id == HOW_SEMI) cycle
                        cnt = cnt + 1_int64
                    end if
                end do
                base(c) = cnt
            end do
            !$omp end parallel do
            o = 0_int64
            do c = 1_int64, int(nt, int64)
                cnt = base(c)
                base(c) = o
                o = o + cnt
            end do
            !$omp parallel do num_threads(nt) default(shared) private(c, lo, hi, i, g, t, cnt) schedule(static)
            do c = 1_int64, int(nt, int64)
                lo = ((c - 1_int64) * nl) / int(nt, int64) + 1_int64
                hi = (c * nl) / int(nt, int64)
                cnt = base(c)
                do i = lo, hi
                    g = group_of_left(i)
                    if (lmatched(g)) then
                        if (how_id == HOW_ANTI) cycle
                        if (how_id == HOW_SEMI) then
                            cnt = cnt + 1_int64
                            il(cnt) = i
                            ir(cnt) = 0_int64
                            cycle
                        end if
                        do t = rg_off(g), rg_off(g + 1_int64) - 1_int64
                            cnt = cnt + 1_int64
                            il(cnt) = i
                            ir(cnt) = rg_idx(t)
                        end do
                    else
                        if (how_id == HOW_INNER .or. how_id == HOW_RIGHT .or. how_id == HOW_SEMI) cycle
                        cnt = cnt + 1_int64
                        il(cnt) = i
                        ir(cnt) = 0_int64
                    end if
                end do
            end do
            !$omp end parallel do
        end if
        if (how_id /= HOW_RIGHT .and. how_id /= HOW_OUTER) return
        ! The unmatched right rows, in right-table order. Walked group by group and then sorted
        ! back into row order by construction: rg_idx is ascending WITHIN a group but not across
        ! groups, so a plain walk would emit them in key order instead. A counting sweep over the
        ! right rows is the cheapest way to keep the promise the argument name makes.
        call join_emit_unmatched_right(rmatched, rg_off, rg_idx, ngroups, nt, o, il, ir)
    end subroutine join_emit_left_order
    !
    !> Appends every right row with no counterpart, in RIGHT-TABLE order.
    subroutine join_emit_unmatched_right(rmatched, rg_off, rg_idx, ngroups, nt, o, il, ir)
        logical, intent(in) :: rmatched(:)       !! per group: its right rows found a match.
        integer(int64), intent(in) :: rg_off(:)  !! group -> slice of rg_idx.
        integer(int64), intent(in) :: rg_idx(:)  !! right rows, grouped.
        integer(int64), intent(in) :: ngroups    !! number of groups.
        integer, intent(in) :: nt                !! team for the mark pass; the sweep is serial.
        integer(int64), intent(inout) :: o       !! output cursor; advanced by what is emitted.
        integer(int64), intent(out) :: il(:)     !! per output row: left row, or 0.
        integer(int64), intent(out) :: ir(:)     !! per output row: right row, or 0.
        logical, allocatable :: unm(:)
        integer(int64) :: g, t, r
        !
        allocate(unm(max(size(rg_idx, kind=int64), 1_int64)))
        unm = .false.
        ! Every right row is in exactly one group, so the marks are disjoint writes.
        !$omp parallel do num_threads(nt) default(shared) private(g, t) schedule(static) if (nt > 1)
        do g = 1_int64, ngroups
            if (rmatched(g)) cycle
            do t = rg_off(g), rg_off(g + 1_int64) - 1_int64
                unm(rg_idx(t)) = .true.
            end do
        end do
        !$omp end parallel do
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
        call join_resolve_how(how, how_id)
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
