!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> `%build_index` and the `parquet_table_index` wrapper: "which row holds this key?" over one
!! column of a table, answered by `parquet_index`'s own engines and refused loudly once the table
!! has changed underneath.
!!
!! **NOT a generated file** -- `tools/generate_parquet_tables.py` emits this module's spec (the
!! type, its bindings and every interface body here), so a signature change is a generator edit
!! and a body change is not.
!!
!! **One lookup engine in this library, and it is `parquet_index`.** The maintainer's rule for
!! this file (feature_pandas_S7.md, section H): the table layer adds nothing that answers a lookup
!! itself. `%build_index` turns a column into the integer keys the engines take and fills one --
!! a `pf_index_map` under `unique=.true.`, a `pf_index_multimap` under `unique=.false.` -- with the
!! table's row numbers as the stored values; every query is one kind check, one generation check
!! and one call into that engine. What the wrapper adds, and the one job it exists for (answer
!! F7), is the STALENESS CHECK: a `parquet_index` object cannot know a table's `%generation()`
!! without importing `parquet_tables`, which is the wrong direction for the tier, so the stamp
!! and the comparison live here.
!!
!! **The generation is compared on EVERY query, never cached** (feature_risks.md Risk-210). A
!! stale index answers in-range row numbers that name the wrong rows, which is exactly the
!! silent failure `table-mutate.md` refuses a "still sorted" flag for; one `integer(int64)`
!! comparison per query is the whole cost of not having it.
!!
!! **The key is converted by parquet_core's helpers and by nothing here** (Risk-211): the same
!! `parquet_date_key`, `parquet_time_key` and `parquet_timestamp_key` the filter's `in` leaf
!! uses, and `parquet_index_real_key` -- the CANONICALISING real key, every NaN one value -- where
!! the filter takes `parquet_filter_real_key`. An index answers the sort comparator's question,
!! "which row holds this value", and under that equality every NaN is one value, as
!! `%duplicated` and `pf_match` already have it; the filter's IEEE rule (a NaN matches nothing)
!! is the one place the two paths part, and it is by design.
!!
!! **A null row is never indexed.** The column's validity becomes the engine's `valid=` mask, so
!! the values stay the ORIGINAL row numbers (nothing is compacted), a null key row can never be
!! found, and two null rows are not a repeat under `unique=.true.`. A null temporal ELEMENT
!! offered as a query key answers 0 for the same reason -- its raw storage is 0, which is
!! 1970-01-01 or midnight to the engine, and a lookup must not find those by accident.
!!
!! **A string column is handed to the engine as it is.** Its `parquet_string_column` goes to the
!! engine's own string `%build`, which reads the packed bytes in place, keys each element by its
!! exact bytes and skips a null element itself -- so no key array is extracted here, no
!! conversion is applied, and the equality is the sort engine's (`"ab"` and `"ab "` are two
!! keys), which is what makes `%find_all` agree with `pf_match_all` over the same column. A
!! `character` array offered to `%find_many` is trimmed per element, the library's rule for
!! every character array argument; a scalar key is taken as written.
submodule (parquet_tables) parquet_tables_index
    ! The key conversion and the column-acceptance rule, from parquet_core rather than repeated
    ! here: one helper per key kind for the filter, the index and the join's hash engine, or a
    ! table's `in` filter and its index disagree about a row. Both facades hide these names again.
    use parquet_core, only : parquet_index_real_key, parquet_date_key, parquet_time_key, &
        parquet_timestamp_key, parquet_set_family_for_column, parquet_filter_column_tokens, &
        FSET_NONE, FSET_STRING
    ! The thread rule `index_team` forwards to -- the one the index tier's own build and bulk
    ! lookups follow -- and the affinity clamp every resolved count goes through
    ! (.claude/rules/api-conventions.md, "Thread counts"). Both modules are already in this
    ! module's footprint, so a submodule import costs a consumer nothing.
    use parquet_index, only : pf_index_threads
    use parquet_settings_base, only : parquet_clamp_to_affinity
    implicit none
    !
    !> Error-message prefix for every `error stop` a query raises. The build's messages carry the
    !! table's own `EP`, since a caller wrote `t%build_index`; a query's caller wrote `ix%find`.
    character(len=*), parameter :: IP = "parquet_table_index: "
    !
    !> Which key KINDS may query an index over which column kinds: one class per family, so the
    !! check is one integer comparison. Both integer widths are one class (a key widens exactly
    !! as a bound set's members do), both real widths are one class, each temporal element
    !! type is its own, and a string is its own.
    integer, parameter :: KC_INT = 1, KC_REAL = 2, KC_DATE = 3, KC_TIME = 4, KC_TS = 5, KC_STR = 6
    !
contains
    !
    ! ---- %build_index -------------------------------------------------------------------------
    !
    module procedure table_build_index
        character(len=*), parameter :: PROC = "build_index"
        integer :: idx, kind
        integer(int8) :: fam
        character(len=:), allocatable :: ttok, stok, sfx
        integer(int64), allocatable :: keys(:), pairs(:, :)
        logical, allocatable :: valid(:)
        type(parquet_string_column), pointer :: sc
        !
        nullify(sc)
        call table_check_open(self, PROC)
        ! table_resolve is the ordinary lazy first touch every value accessor runs: the column is
        ! READ here if the table has not read it yet, and an unsupported type is refused with the
        ! same message %get would give. A read, so no shared-table guard: several threads may
        ! build indexes over one shared table at once, as they may %get from it.
        call table_resolve(self, name, PROC, idx)
        associate (slot => self%cache%cols(idx))
            kind = slot%declared_kind
            call parquet_filter_column_tokens(kind, slot%width, ttok, stok)
        end associate
        ! The acceptance rule is the filter's, through the same two helpers the `in` leaf runs,
        ! so exactly the columns that can carry a set clause can be indexed, and no other.
        call parquet_set_family_for_column(ttok, fam)
        if (fam == FSET_NONE) then
            call table_context_suffix(self%cache, name, sfx)
            if (stok /= "scalar") then
                error stop EP // PROC // ": column '" // trim(name) // "' is a " // stok // &
                    " column; an index needs one value per row, so only a scalar column can be " // &
                    "indexed" // sfx
            end if
            error stop EP // PROC // ": column '" // trim(name) // "' is a " // ttok // &
                " column, which cannot be indexed -- a boolean key is `==` with extra steps; " // &
                "index an integer, real, date, time or timestamp column" // sfx
        end if
        !
        ix%cache => self%cache
        ix%gen = self%cache%generation
        ix%colkind = kind
        ix%uniq = .true.
        if (present(unique)) ix%uniq = unique
        ix%colname = trim(name)
        ! Values default to 1..n, the key's own position, which is exactly the table's row number
        ! in the full regime -- so nothing is passed as `values=`. The map's own duplicate
        ! refusal names a repeated key and both of its positions, which are table rows.
        if (fam == FSET_STRING) then
            ! The column's own string store, in place: the engine keys and null-masks it itself,
            ! and the store is exactly `row_count` elements long, every column of a table being.
            call parquet_column_string_column(self%cache%cols(idx)%values, sc)
            if (ix%uniq) then
                call ix%map%build(sc, threads=threads)
            else
                call ix%mmap%build(sc, threads=threads)
            end if
            ix%built = .true.
            return
        end if
        call index_extract_keys(self%cache%cols(idx)%values, kind, self%row_count, keys, pairs, valid, &
            threads)
        if (kind == PK_TIMESTAMP) then
            if (ix%uniq) then
                call ix%map%build(pairs, valid=valid, threads=threads)
            else
                call ix%mmap%build(pairs, valid=valid, threads=threads)
            end if
        else
            if (ix%uniq) then
                call ix%map%build(keys, valid=valid, threads=threads)
            else
                call ix%mmap%build(keys, valid=valid, threads=threads)
            end if
        end if
        ix%built = .true.
    end procedure table_build_index
    !
    ! ---- the key conversion, shared with the join's hash engine ------------------------------
    !
    module procedure index_extract_keys
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: r32(:)
        real(real64), pointer :: r64(:)
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        type(parquet_timestamp), pointer :: pts(:)
        integer(int64) :: i
        integer :: nt
        !
        nullify(p32, p64, r32, r64, pd, pt, pts)
        select case (kind)
        case (PK_INT32)
            call parquet_column_data_ptr(col, p32)
            keys = int(p32(1_int64:nrows), int64)
        case (PK_INT64)
            call parquet_column_data_ptr(col, p64)
            keys = p64(1_int64:nrows)
        case (PK_FLOAT32)
            call parquet_column_data_ptr(col, r32)
            keys = parquet_index_real_key(real(r32(1_int64:nrows), real64))
        case (PK_FLOAT64)
            call parquet_column_data_ptr(col, r64)
            keys = parquet_index_real_key(r64(1_int64:nrows))
        case (PK_DATE)
            call parquet_column_data_ptr(col, pd)
            keys = parquet_date_key(pd(1_int64:nrows))
        case (PK_TIME)
            call parquet_column_data_ptr(col, pt)
            keys = parquet_time_key(pt(1_int64:nrows))
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(col, pts)
            allocate(pairs(nrows, 2))
            call parquet_timestamp_key(pts(1_int64:nrows), pairs(:, 1), pairs(:, 2))
        case (PK_STRING)
            ! No key at all: the engines take the column's own string store in place, by its
            ! exact bytes (see the file header), so only the mask below is wanted here.
            continue
        case default
            ! Defensive: every caller has already accepted the kind through the filter's rule
            ! (`%build_index`) or the join's eligibility test, so no fixture reaches this.
            ! GCOVR_EXCL_START
            error stop EP // "index_extract_keys: no key conversion exists for this column " // &
                "kind; the caller was to have refused it first"
            ! GCOVR_EXCL_STOP
        end select
        ! parquet_column_any_null answers for all three storage classes -- the bitmap kinds, the
        ! temporal elements and the string store alike -- which is why the row loop asks the
        ! column rather than the pointer. One `parquet_column_is_null` per row, on a team: a
        ! read-only pass over a resident column, so the rows split with no shared state at all.
        if (parquet_column_any_null(col)) then
            allocate(valid(nrows))
            nt = index_team(nrows, threads)
            !$omp parallel do num_threads(nt) if (nt > 1) schedule(static)
            do i = 1_int64, nrows
                valid(i) = .not. parquet_column_is_null(col, i)
            end do
            !$omp end parallel do
        end if
    end procedure index_extract_keys
    !
    module procedure index_team
        if (present(threads)) then
            ! An explicit request is honoured whatever the size and wherever it is made, as the
            ! index tier honours it -- but clamped to the affinity mask, since a team wider than
            ! the processors is slower than no team at all. The area name is the index tier's,
            ! because that is whose rule this is.
            nt = parquet_clamp_to_affinity(max(threads, 1), "index")
        else
            nt = pf_index_threads(n)
        end if
        if (n < 2_int64) nt = 1
    end procedure index_team
    !
    ! ---- the two guards every query runs ------------------------------------------------------
    !
    !> Aborts unless the index has been built and the table has not changed structurally since,
    !! naming the table, the column and both generations -- the cause of a stale index is usually
    !! several statements away from where it is noticed, so the remedy is named too.
    !!
    !! The comparison is made HERE, on every query, and the answer is never cached: a "still
    !! valid" flag would be the stale-flag hazard `table-mutate.md` refuses for sortedness, and
    !! the check is one integer comparison (feature_risks.md Risk-210).
    subroutine tix_resolve(self, proc)
        class(parquet_table_index), intent(in) :: self !! the index.
        character(len=*), intent(in) :: proc          !! calling binding, for the message.
        character(len=:), allocatable :: sfx
        character(len=32) :: g_now, g_then
        !
        if (.not. self%built) then
            error stop IP // trim(proc) // ": no index has been built into this object -- call " // &
                "t%build_index(name, ix) first (or it was cleared with %clear)"
        end if
        if (self%cache%generation == self%gen) return
        write(g_now, "(I0)") self%cache%generation
        write(g_then, "(I0)") self%gen
        call table_context_suffix(self%cache, self%colname, sfx)
        error stop IP // trim(proc) // ": this table has changed structurally since the index " // &
            "was built (generation " // trim(g_now) // ", index " // trim(g_then) // "); rebuild " // &
            "it with %build_index" // sfx
    end subroutine tix_resolve
    !
    !> Aborts unless a key of class `want` may query this index's column, naming both. One rule
    !! for all four query families, so `%find` and `%count` cannot accept different keys.
    subroutine tix_check_key(self, proc, want)
        class(parquet_table_index), intent(in) :: self !! the index.
        character(len=*), intent(in) :: proc          !! calling binding, for the message.
        integer, intent(in) :: want                   !! the key's KC_* class.
        character(len=:), allocatable :: ttok, stok, sfx, kword
        integer :: have
        !
        select case (self%colkind)
        case (PK_INT32, PK_INT64)
            have = KC_INT
        case (PK_FLOAT32, PK_FLOAT64)
            have = KC_REAL
        case (PK_DATE)
            have = KC_DATE
        case (PK_TIME)
            have = KC_TIME
        case (PK_STRING)
            have = KC_STR
        case default
            have = KC_TS
        end select
        if (have == want) return
        select case (want)
        case (KC_INT)
            kword = "an integer"
        case (KC_REAL)
            kword = "a real"
        case (KC_DATE)
            kword = "a parquet_date"
        case (KC_TIME)
            kword = "a parquet_time"
        case (KC_STR)
            kword = "a string"
        case default
            kword = "a parquet_timestamp"
        end select
        call parquet_filter_column_tokens(self%colkind, 1, ttok, stok)
        call table_context_suffix(self%cache, self%colname, sfx)
        error stop IP // trim(proc) // ": this index is over " // ttok // " column '" // &
            self%colname // "', and was asked for " // kword // " key" // sfx
    end subroutine tix_check_key
    !
    ! ---- the engine calls, once per answer shape ----------------------------------------------
    !
    !> The first row holding a scalar key, from whichever engine is built.
    function tix_first_of(self, key) result(row)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: key             !! the engine's key.
        integer(int64) :: row                         !! the lowest row holding it, or 0.
        if (self%uniq) then
            row = self%map%get(key)
        else
            row = self%mmap%get_first(key)
        end if
    end function tix_first_of
    !
    !> The first row holding a timestamp's (seconds, nanoseconds) pair.
    function tix_first_pair(self, pair) result(row)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: pair(2)         !! the two key components.
        integer(int64) :: row                         !! the lowest row holding them, or 0.
        if (self%uniq) then
            row = self%map%get(pair)
        else
            row = self%mmap%get_first(pair)
        end if
    end function tix_first_pair
    !
    !> How many rows hold a scalar key: 0 or 1 from the map, the group size from the multimap.
    function tix_count_of(self, key) result(n)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: key             !! the engine's key.
        integer(int64) :: n                           !! rows holding it.
        if (self%uniq) then
            n = merge(1_int64, 0_int64, self%map%get(key) > 0_int64)
        else
            n = self%mmap%count(key)
        end if
    end function tix_count_of
    !
    !> How many rows hold a timestamp's pair.
    function tix_count_pair(self, pair) result(n)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: pair(2)         !! the two key components.
        integer(int64) :: n                           !! rows holding them.
        if (self%uniq) then
            n = merge(1_int64, 0_int64, self%map%get(pair) > 0_int64)
        else
            n = self%mmap%count(pair)
        end if
    end function tix_count_pair
    !
    !> Every row holding a scalar key, as int64 rows: one or none from the map, the group from
    !! the multimap (ascending by position, the multimap's contract).
    subroutine tix_all_of_i64(self, key, rows)
        class(parquet_table_index), intent(in) :: self       !! the index.
        integer(int64), intent(in) :: key                   !! the engine's key.
        integer(int64), allocatable, intent(out) :: rows(:) !! the rows, ascending; zero-length when none.
        integer(int64) :: r
        if (self%uniq) then
            r = self%map%get(key)
            if (r > 0_int64) then
                rows = [r]
            else
                allocate(rows(0))
            end if
        else
            call self%mmap%get_all(key, rows)
        end if
    end subroutine tix_all_of_i64
    !
    !> Every row holding a scalar key, as int32 rows; the multimap refuses a row above the int32
    !! range itself, and the map's one row is narrowed through the same check.
    subroutine tix_all_of_i32(self, key, rows, proc)
        class(parquet_table_index), intent(in) :: self       !! the index.
        integer(int64), intent(in) :: key                   !! the engine's key.
        integer(int32), allocatable, intent(out) :: rows(:) !! the rows, ascending; zero-length when none.
        character(len=*), intent(in) :: proc                !! calling binding, for the message.
        integer(int64) :: r
        if (self%uniq) then
            r = self%map%get(key)
            if (r > 0_int64) then
                rows = [tix_narrow(r, proc)]
            else
                allocate(rows(0))
            end if
        else
            call self%mmap%get_all(key, rows)
        end if
    end subroutine tix_all_of_i32
    !
    !> Every row holding a timestamp's pair, as int64 rows.
    subroutine tix_all_pair_i64(self, pair, rows)
        class(parquet_table_index), intent(in) :: self       !! the index.
        integer(int64), intent(in) :: pair(2)               !! the two key components.
        integer(int64), allocatable, intent(out) :: rows(:) !! the rows, ascending; zero-length when none.
        integer(int64) :: r
        if (self%uniq) then
            r = self%map%get(pair)
            if (r > 0_int64) then
                rows = [r]
            else
                allocate(rows(0))
            end if
        else
            call self%mmap%get_all(pair, rows)
        end if
    end subroutine tix_all_pair_i64
    !
    !> Every row holding a timestamp's pair, as int32 rows.
    subroutine tix_all_pair_i32(self, pair, rows, proc)
        class(parquet_table_index), intent(in) :: self       !! the index.
        integer(int64), intent(in) :: pair(2)               !! the two key components.
        integer(int32), allocatable, intent(out) :: rows(:) !! the rows, ascending; zero-length when none.
        character(len=*), intent(in) :: proc                !! calling binding, for the message.
        integer(int64) :: r
        if (self%uniq) then
            r = self%map%get(pair)
            if (r > 0_int64) then
                rows = [tix_narrow(r, proc)]
            else
                allocate(rows(0))
            end if
        else
            call self%mmap%get_all(pair, rows)
        end if
    end subroutine tix_all_pair_i32
    !
    !> Bulk first-row lookup over the engine's own threaded form, int64 answers. The multimap
    !! counts the hits itself; the map's `%get_many` does not carry `n_found`, so it is counted
    !! here, one pass over the answers that the caller asked for by naming the argument.
    subroutine tix_many_of_i64(self, keys, rows, valid, threads, n_found)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: keys(:)         !! the engine's keys.
        integer(int64), intent(out) :: rows(:)        !! one per key, 0 where absent.
        logical, allocatable, intent(in) :: valid(:)  !! null query elements masked; unallocated = none.
        integer, intent(in), optional :: threads      !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(keys, rows, valid=valid, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int64, kind=int64)
        else
            call self%mmap%get_first_many(keys, rows, valid=valid, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_of_i64
    !
    !> Bulk first-row lookup, int32 answers; the engines refuse a row above the int32 range.
    subroutine tix_many_of_i32(self, keys, rows, valid, threads, n_found)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: keys(:)         !! the engine's keys.
        integer(int32), intent(out) :: rows(:)        !! one per key, 0 where absent.
        logical, allocatable, intent(in) :: valid(:)  !! null query elements masked; unallocated = none.
        integer, intent(in), optional :: threads      !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(keys, rows, valid=valid, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int32, kind=int64)
        else
            call self%mmap%get_first_many(keys, rows, valid=valid, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_of_i32
    !
    !> Bulk first-row lookup over `(n, 2)` timestamp pairs, int64 answers.
    subroutine tix_many_pair_i64(self, pairs, rows, valid, threads, n_found)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: pairs(:, :)     !! the key tuples, one per row.
        integer(int64), intent(out) :: rows(:)        !! one per key, 0 where absent.
        logical, allocatable, intent(in) :: valid(:)  !! null query elements masked; unallocated = none.
        integer, intent(in), optional :: threads      !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(pairs, rows, valid=valid, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int64, kind=int64)
        else
            call self%mmap%get_first_many(pairs, rows, valid=valid, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_pair_i64
    !
    !> Bulk first-row lookup over `(n, 2)` timestamp pairs, int32 answers.
    subroutine tix_many_pair_i32(self, pairs, rows, valid, threads, n_found)
        class(parquet_table_index), intent(in) :: self !! the index.
        integer(int64), intent(in) :: pairs(:, :)     !! the key tuples, one per row.
        integer(int32), intent(out) :: rows(:)        !! one per key, 0 where absent.
        logical, allocatable, intent(in) :: valid(:)  !! null query elements masked; unallocated = none.
        integer, intent(in), optional :: threads      !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(pairs, rows, valid=valid, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int32, kind=int64)
        else
            call self%mmap%get_first_many(pairs, rows, valid=valid, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_pair_i32
    !
    !> The first row holding a string key.
    function tix_first_str(self, key) result(row)
        class(parquet_table_index), intent(in) :: self !! the index.
        character(len=*), intent(in) :: key           !! the key, as written.
        integer(int64) :: row                         !! the lowest row holding it, or 0.
        if (self%uniq) then
            row = self%map%get(key)
        else
            row = self%mmap%get_first(key)
        end if
    end function tix_first_str
    !
    !> How many rows hold a string key.
    function tix_count_str(self, key) result(n)
        class(parquet_table_index), intent(in) :: self !! the index.
        character(len=*), intent(in) :: key           !! the key, as written.
        integer(int64) :: n                           !! rows holding it.
        if (self%uniq) then
            n = merge(1_int64, 0_int64, self%map%get(key) > 0_int64)
        else
            n = self%mmap%count(key)
        end if
    end function tix_count_str
    !
    !> Every row holding a string key, as int64 rows.
    subroutine tix_all_str_i64(self, key, rows)
        class(parquet_table_index), intent(in) :: self       !! the index.
        character(len=*), intent(in) :: key                 !! the key, as written.
        integer(int64), allocatable, intent(out) :: rows(:) !! the rows, ascending; zero-length when none.
        integer(int64) :: r
        if (self%uniq) then
            r = self%map%get(key)
            if (r > 0_int64) then
                rows = [r]
            else
                allocate(rows(0))
            end if
        else
            call self%mmap%get_all(key, rows)
        end if
    end subroutine tix_all_str_i64
    !
    !> Every row holding a string key, as int32 rows.
    subroutine tix_all_str_i32(self, key, rows, proc)
        class(parquet_table_index), intent(in) :: self       !! the index.
        character(len=*), intent(in) :: key                 !! the key, as written.
        integer(int32), allocatable, intent(out) :: rows(:) !! the rows, ascending; zero-length when none.
        character(len=*), intent(in) :: proc                !! calling binding, for the message.
        integer(int64) :: r
        if (self%uniq) then
            r = self%map%get(key)
            if (r > 0_int64) then
                rows = [tix_narrow(r, proc)]
            else
                allocate(rows(0))
            end if
        else
            call self%mmap%get_all(key, rows)
        end if
    end subroutine tix_all_str_i32
    !
    !> Bulk first-row lookup over a character array (each element trimmed), int64 answers.
    subroutine tix_many_of_chr_i64(self, keys, rows, threads, n_found)
        class(parquet_table_index), intent(in) :: self !! the index.
        character(len=*), intent(in) :: keys(:)       !! the keys, one per element.
        integer(int64), intent(out) :: rows(:)        !! one per key, 0 where absent.
        integer, intent(in), optional :: threads      !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(keys, rows, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int64, kind=int64)
        else
            call self%mmap%get_first_many(keys, rows, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_of_chr_i64
    !
    !> Bulk first-row lookup over a character array, int32 answers.
    subroutine tix_many_of_chr_i32(self, keys, rows, threads, n_found)
        class(parquet_table_index), intent(in) :: self !! the index.
        character(len=*), intent(in) :: keys(:)       !! the keys, one per element.
        integer(int32), intent(out) :: rows(:)        !! one per key, 0 where absent.
        integer, intent(in), optional :: threads      !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(keys, rows, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int32, kind=int64)
        else
            call self%mmap%get_first_many(keys, rows, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_of_chr_i32
    !
    !> Bulk first-row lookup over a parquet_string_column (verbatim, a null element answering
    !! 0), int64 answers.
    subroutine tix_many_strcol_i64(self, keys, rows, threads, n_found)
        class(parquet_table_index), intent(in) :: self         !! the index.
        type(parquet_string_column), intent(in), target :: keys !! the keys, one per element.
        integer(int64), intent(out) :: rows(:)                !! one per key, 0 where absent.
        integer, intent(in), optional :: threads              !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found      !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(keys, rows, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int64, kind=int64)
        else
            call self%mmap%get_first_many(keys, rows, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_strcol_i64
    !
    !> Bulk first-row lookup over a parquet_string_column, int32 answers.
    subroutine tix_many_strcol_i32(self, keys, rows, threads, n_found)
        class(parquet_table_index), intent(in) :: self         !! the index.
        type(parquet_string_column), intent(in), target :: keys !! the keys, one per element.
        integer(int32), intent(out) :: rows(:)                !! one per key, 0 where absent.
        integer, intent(in), optional :: threads              !! forwarded to the engine.
        integer(int64), intent(out), optional :: n_found      !! non-zero answers.
        if (self%uniq) then
            call self%map%get_many(keys, rows, threads=threads)
            if (present(n_found)) n_found = count(rows > 0_int32, kind=int64)
        else
            call self%mmap%get_first_many(keys, rows, threads=threads, n_found=n_found)
        end if
    end subroutine tix_many_strcol_i32
    !
    !> One row number narrowed to int32, or an abort naming it: the engines refuse the same case
    !! on their own int32 forms, and a scalar `%find` into an int32 variable must not differ.
    function tix_narrow(row, proc) result(r32)
        integer(int64), intent(in) :: row    !! the row to narrow.
        character(len=*), intent(in) :: proc !! calling binding, for the message.
        integer(int32) :: r32                !! the same row.
        character(len=32) :: txt
        ! Deliberately untested, and not reachable at test scale: `row` is a TABLE row number, so
        ! tripping this needs an indexed table of more than 2**31 rows -- some 2.1 billion -- which
        ! is a fixture no suite can build. The guard itself is not dead: it is the reason a caller
        ! who asks for an int32 answer on a table that large is told so rather than handed a
        ! wrapped negative row, and the engines refuse the same case on their own int32 forms.
        if (row > int(huge(0_int32), int64)) then
            ! GCOVR_EXCL_START -- see the note above.
            write(txt, "(I0)") row
            error stop IP // trim(proc) // ": row " // trim(txt) // " does not fit the int32 " // &
                "answer asked for; use an int64 row variable"
            ! GCOVR_EXCL_STOP
        end if
        r32 = int(row, int32)
    end function tix_narrow
    !
    !> Aborts unless `rows` has one entry per key, naming both counts.
    subroutine tix_check_many_len(nkeys, nrows, proc)
        integer(int64), intent(in) :: nkeys  !! keys offered.
        integer(int64), intent(in) :: nrows  !! answer slots given.
        character(len=*), intent(in) :: proc !! calling binding, for the message.
        character(len=32) :: got, want
        if (nkeys == nrows) return
        write(got, "(I0)") nrows
        write(want, "(I0)") nkeys
        error stop IP // trim(proc) // ": rows has " // trim(got) // " entries but keys has " // &
            trim(want) // " -- give one answer slot per key"
    end subroutine tix_check_many_len
    !
    ! ---- %find ---------------------------------------------------------------------------------
    !
    ! Each specific converts its key exactly as %build_index converted the column -- an integer
    ! widened, a real through the CANONICALISING key, a temporal element through its raw storage
    ! -- and hands it to the engine. A null temporal element answers 0 without a lookup.
    !
    module procedure tix_find_i32_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_INT)
        row = tix_narrow(tix_first_of(self, int(key, int64)), "find")
    end procedure tix_find_i32_i32
    !
    module procedure tix_find_i32_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_INT)
        row = tix_first_of(self, int(key, int64))
    end procedure tix_find_i32_i64
    !
    module procedure tix_find_i64_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_INT)
        row = tix_narrow(tix_first_of(self, key), "find")
    end procedure tix_find_i64_i32
    !
    module procedure tix_find_i64_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_INT)
        row = tix_first_of(self, key)
    end procedure tix_find_i64_i64
    !
    module procedure tix_find_r32_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_REAL)
        row = tix_narrow(tix_first_of(self, parquet_index_real_key(real(key, real64))), "find")
    end procedure tix_find_r32_i32
    !
    module procedure tix_find_r32_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_REAL)
        row = tix_first_of(self, parquet_index_real_key(real(key, real64)))
    end procedure tix_find_r32_i64
    !
    module procedure tix_find_r64_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_REAL)
        row = tix_narrow(tix_first_of(self, parquet_index_real_key(key)), "find")
    end procedure tix_find_r64_i32
    !
    module procedure tix_find_r64_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_REAL)
        row = tix_first_of(self, parquet_index_real_key(key))
    end procedure tix_find_r64_i64
    !
    module procedure tix_find_date_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_DATE)
        row = 0_int32
        if (key%is_null()) return
        row = tix_narrow(tix_first_of(self, parquet_date_key(key)), "find")
    end procedure tix_find_date_i32
    !
    module procedure tix_find_date_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_DATE)
        row = 0_int64
        if (key%is_null()) return
        row = tix_first_of(self, parquet_date_key(key))
    end procedure tix_find_date_i64
    !
    module procedure tix_find_time_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_TIME)
        row = 0_int32
        if (key%is_null()) return
        row = tix_narrow(tix_first_of(self, parquet_time_key(key)), "find")
    end procedure tix_find_time_i32
    !
    module procedure tix_find_time_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_TIME)
        row = 0_int64
        if (key%is_null()) return
        row = tix_first_of(self, parquet_time_key(key))
    end procedure tix_find_time_i64
    !
    module procedure tix_find_ts_i32
        integer(int64) :: pair(2)
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_TS)
        row = 0_int32
        if (key%is_null()) return
        call parquet_timestamp_key(key, pair(1), pair(2))
        row = tix_narrow(tix_first_pair(self, pair), "find")
    end procedure tix_find_ts_i32
    !
    module procedure tix_find_ts_i64
        integer(int64) :: pair(2)
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_TS)
        row = 0_int64
        if (key%is_null()) return
        call parquet_timestamp_key(key, pair(1), pair(2))
        row = tix_first_pair(self, pair)
    end procedure tix_find_ts_i64
    !
    module procedure tix_find_chr_i32
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_STR)
        row = tix_narrow(tix_first_str(self, key), "find")
    end procedure tix_find_chr_i32
    !
    module procedure tix_find_chr_i64
        call tix_resolve(self, "find")
        call tix_check_key(self, "find", KC_STR)
        row = tix_first_str(self, key)
    end procedure tix_find_chr_i64
    !
    ! ---- %count --------------------------------------------------------------------------------
    !
    module procedure tix_count_i32
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_INT)
        n = tix_count_of(self, int(key, int64))
    end procedure tix_count_i32
    !
    module procedure tix_count_i64
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_INT)
        n = tix_count_of(self, key)
    end procedure tix_count_i64
    !
    module procedure tix_count_r32
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_REAL)
        n = tix_count_of(self, parquet_index_real_key(real(key, real64)))
    end procedure tix_count_r32
    !
    module procedure tix_count_r64
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_REAL)
        n = tix_count_of(self, parquet_index_real_key(key))
    end procedure tix_count_r64
    !
    module procedure tix_count_date
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_DATE)
        n = 0_int64
        if (key%is_null()) return
        n = tix_count_of(self, parquet_date_key(key))
    end procedure tix_count_date
    !
    module procedure tix_count_time
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_TIME)
        n = 0_int64
        if (key%is_null()) return
        n = tix_count_of(self, parquet_time_key(key))
    end procedure tix_count_time
    !
    module procedure tix_count_ts
        integer(int64) :: pair(2)
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_TS)
        n = 0_int64
        if (key%is_null()) return
        call parquet_timestamp_key(key, pair(1), pair(2))
        n = tix_count_pair(self, pair)
    end procedure tix_count_ts
    !
    module procedure tix_count_chr
        call tix_resolve(self, "count")
        call tix_check_key(self, "count", KC_STR)
        n = tix_count_str(self, key)
    end procedure tix_count_chr
    !
    ! ---- %find_all -----------------------------------------------------------------------------
    !
    module procedure tix_all_i32_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_INT)
        call tix_all_of_i32(self, int(key, int64), rows, "find_all")
    end procedure tix_all_i32_i32
    !
    module procedure tix_all_i32_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_INT)
        call tix_all_of_i64(self, int(key, int64), rows)
    end procedure tix_all_i32_i64
    !
    module procedure tix_all_i64_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_INT)
        call tix_all_of_i32(self, key, rows, "find_all")
    end procedure tix_all_i64_i32
    !
    module procedure tix_all_i64_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_INT)
        call tix_all_of_i64(self, key, rows)
    end procedure tix_all_i64_i64
    !
    module procedure tix_all_r32_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_REAL)
        call tix_all_of_i32(self, parquet_index_real_key(real(key, real64)), rows, "find_all")
    end procedure tix_all_r32_i32
    !
    module procedure tix_all_r32_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_REAL)
        call tix_all_of_i64(self, parquet_index_real_key(real(key, real64)), rows)
    end procedure tix_all_r32_i64
    !
    module procedure tix_all_r64_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_REAL)
        call tix_all_of_i32(self, parquet_index_real_key(key), rows, "find_all")
    end procedure tix_all_r64_i32
    !
    module procedure tix_all_r64_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_REAL)
        call tix_all_of_i64(self, parquet_index_real_key(key), rows)
    end procedure tix_all_r64_i64
    !
    module procedure tix_all_date_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_DATE)
        if (key%is_null()) then
            allocate(rows(0))
            return
        end if
        call tix_all_of_i32(self, parquet_date_key(key), rows, "find_all")
    end procedure tix_all_date_i32
    !
    module procedure tix_all_date_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_DATE)
        if (key%is_null()) then
            allocate(rows(0))
            return
        end if
        call tix_all_of_i64(self, parquet_date_key(key), rows)
    end procedure tix_all_date_i64
    !
    module procedure tix_all_time_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_TIME)
        if (key%is_null()) then
            allocate(rows(0))
            return
        end if
        call tix_all_of_i32(self, parquet_time_key(key), rows, "find_all")
    end procedure tix_all_time_i32
    !
    module procedure tix_all_time_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_TIME)
        if (key%is_null()) then
            allocate(rows(0))
            return
        end if
        call tix_all_of_i64(self, parquet_time_key(key), rows)
    end procedure tix_all_time_i64
    !
    module procedure tix_all_ts_i32
        integer(int64) :: pair(2)
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_TS)
        if (key%is_null()) then
            allocate(rows(0))
            return
        end if
        call parquet_timestamp_key(key, pair(1), pair(2))
        call tix_all_pair_i32(self, pair, rows, "find_all")
    end procedure tix_all_ts_i32
    !
    module procedure tix_all_ts_i64
        integer(int64) :: pair(2)
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_TS)
        if (key%is_null()) then
            allocate(rows(0))
            return
        end if
        call parquet_timestamp_key(key, pair(1), pair(2))
        call tix_all_pair_i64(self, pair, rows)
    end procedure tix_all_ts_i64
    !
    module procedure tix_all_chr_i32
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_STR)
        call tix_all_str_i32(self, key, rows, "find_all")
    end procedure tix_all_chr_i32
    !
    module procedure tix_all_chr_i64
        call tix_resolve(self, "find_all")
        call tix_check_key(self, "find_all", KC_STR)
        call tix_all_str_i64(self, key, rows)
    end procedure tix_all_chr_i64
    !
    ! ---- %find_many ----------------------------------------------------------------------------
    !
    ! The keys are converted once, in bulk, and the engine's own threaded lookup does the rest;
    ! `valid` is left unallocated -- read by the engine as absent -- for every key kind but the
    ! temporal three, whose null elements are masked out of the lookup.
    !
    module procedure tix_many_i32_i32
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_INT)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i32(self, int(keys, int64), rows, valid, threads, n_found)
    end procedure tix_many_i32_i32
    !
    module procedure tix_many_i32_i64
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_INT)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i64(self, int(keys, int64), rows, valid, threads, n_found)
    end procedure tix_many_i32_i64
    !
    module procedure tix_many_i64_i32
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_INT)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i32(self, keys, rows, valid, threads, n_found)
    end procedure tix_many_i64_i32
    !
    module procedure tix_many_i64_i64
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_INT)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i64(self, keys, rows, valid, threads, n_found)
    end procedure tix_many_i64_i64
    !
    module procedure tix_many_r32_i32
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_REAL)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i32(self, parquet_index_real_key(real(keys, real64)), rows, valid, threads, n_found)
    end procedure tix_many_r32_i32
    !
    module procedure tix_many_r32_i64
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_REAL)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i64(self, parquet_index_real_key(real(keys, real64)), rows, valid, threads, n_found)
    end procedure tix_many_r32_i64
    !
    module procedure tix_many_r64_i32
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_REAL)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i32(self, parquet_index_real_key(keys), rows, valid, threads, n_found)
    end procedure tix_many_r64_i32
    !
    module procedure tix_many_r64_i64
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_REAL)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_i64(self, parquet_index_real_key(keys), rows, valid, threads, n_found)
    end procedure tix_many_r64_i64
    !
    module procedure tix_many_date_i32
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_DATE)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        valid = .not. keys%is_null()
        call tix_many_of_i32(self, parquet_date_key(keys), rows, valid, threads, n_found)
    end procedure tix_many_date_i32
    !
    module procedure tix_many_date_i64
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_DATE)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        valid = .not. keys%is_null()
        call tix_many_of_i64(self, parquet_date_key(keys), rows, valid, threads, n_found)
    end procedure tix_many_date_i64
    !
    module procedure tix_many_time_i32
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_TIME)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        valid = .not. keys%is_null()
        call tix_many_of_i32(self, parquet_time_key(keys), rows, valid, threads, n_found)
    end procedure tix_many_time_i32
    !
    module procedure tix_many_time_i64
        logical, allocatable :: valid(:)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_TIME)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        valid = .not. keys%is_null()
        call tix_many_of_i64(self, parquet_time_key(keys), rows, valid, threads, n_found)
    end procedure tix_many_time_i64
    !
    module procedure tix_many_ts_i32
        logical, allocatable :: valid(:)
        integer(int64), allocatable :: pairs(:, :)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_TS)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        valid = .not. keys%is_null()
        allocate(pairs(size(keys, kind=int64), 2))
        call parquet_timestamp_key(keys, pairs(:, 1), pairs(:, 2))
        call tix_many_pair_i32(self, pairs, rows, valid, threads, n_found)
    end procedure tix_many_ts_i32
    !
    module procedure tix_many_ts_i64
        logical, allocatable :: valid(:)
        integer(int64), allocatable :: pairs(:, :)
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_TS)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        valid = .not. keys%is_null()
        allocate(pairs(size(keys, kind=int64), 2))
        call parquet_timestamp_key(keys, pairs(:, 1), pairs(:, 2))
        call tix_many_pair_i64(self, pairs, rows, valid, threads, n_found)
    end procedure tix_many_ts_i64
    !
    module procedure tix_many_chr_i32
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_STR)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_chr_i32(self, keys, rows, threads, n_found)
    end procedure tix_many_chr_i32
    !
    module procedure tix_many_chr_i64
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_STR)
        call tix_check_many_len(size(keys, kind=int64), size(rows, kind=int64), "find_many")
        call tix_many_of_chr_i64(self, keys, rows, threads, n_found)
    end procedure tix_many_chr_i64
    !
    module procedure tix_many_str_i32
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_STR)
        call tix_check_many_len(keys%size(), size(rows, kind=int64), "find_many")
        call tix_many_strcol_i32(self, keys, rows, threads, n_found)
    end procedure tix_many_str_i32
    !
    module procedure tix_many_str_i64
        call tix_resolve(self, "find_many")
        call tix_check_key(self, "find_many", KC_STR)
        call tix_check_many_len(keys%size(), size(rows, kind=int64), "find_many")
        call tix_many_strcol_i64(self, keys, rows, threads, n_found)
    end procedure tix_many_str_i64
    !
    ! ---- introspection and release -------------------------------------------------------------
    !
    module procedure tix_is_current
        ok = .false.
        if (.not. self%built) return
        ok = self%cache%generation == self%gen
    end procedure tix_is_current
    !
    module procedure tix_is_unique
        u = self%uniq
    end procedure tix_is_unique
    !
    module procedure tix_nkeys
        n = 0_int64
        if (.not. self%built) return
        if (self%uniq) then
            n = self%map%nkeys()
        else
            n = self%mmap%ngroups()
        end if
    end procedure tix_nkeys
    !
    module procedure tix_kind
        k = self%colkind
    end procedure tix_kind
    !
    module procedure tix_name
        nm = ""
        if (allocated(self%colname)) nm = self%colname
    end procedure tix_name
    !
    module procedure tix_clear
        ! Both engines are cleared whichever was built: %clear on an empty one is a no-op, and
        ! asking `uniq` first would leave a rebuilt object's other engine holding stale arrays.
        call self%map%clear()
        call self%mmap%clear()
        nullify(self%cache)
        self%gen = -1_int64
        self%colkind = PK_NONE
        self%uniq = .true.
        self%built = .false.
        if (allocated(self%colname)) deallocate(self%colname)
    end procedure tix_clear
end submodule parquet_tables_index
