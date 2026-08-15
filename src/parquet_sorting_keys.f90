!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> Turns each supported element type into the canonical key form the C++ engine takes, and
!! implements `pf_sort_keys`.
!!
!! **This file decides nothing about order.** It extracts values, says which rows are null, and
!! passes the caller's `descending`/`nulls_first` flags through -- every ordering decision is made
!! in one place (`sort_compare_key`, `src/parquet_wrapper.cpp`), which is what stops a raw-array
!! sort, a table sort and a read-time `sort_by=` from ever disagreeing.
!!
!! The canonical form is deliberately narrow: an integer key, a real key, or a packed
!! (offsets, data) string key, each with an optional per-row validity array. Everything else
!! reduces to one of those three -- a `logical` and every temporal kind order exactly as their
!! stored integers do, and a `parquet_timestamp` becomes two integer keys rather than one.
submodule (parquet_sorting) parquet_sorting_keys
    implicit none
    !
contains
    !
    module procedure extract_i32
        integer(int64) :: k, n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%ints(k) = 0_int64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%ints(k) = int(values(k), int64)
            end do
            !$omp end parallel do
        else
            buf(1)%ints = 0_int64
            do k = 1_int64, n
            buf(1)%ints(k) = int(values(k), int64)
            end do
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_i32
    !
    module procedure extract_i64
        integer(int64) :: k, n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%ints(k) = 0_int64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%ints(k) = values(k)
            end do
            !$omp end parallel do
        else
            buf(1)%ints = 0_int64
            do k = 1_int64, n
            buf(1)%ints(k) = values(k)
            end do
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_i64
    !
    module procedure extract_f32
        integer(int64) :: k, n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%reals(k) = 0.0_real64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%reals(k) = real(values(k), real64)
            end do
            !$omp end parallel do
        else
            buf(1)%reals = 0.0_real64
            do k = 1_int64, n
            buf(1)%reals(k) = real(values(k), real64)
            end do
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_f32
    !
    module procedure extract_f64
        integer(int64) :: k, n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%reals(k) = 0.0_real64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%reals(k) = values(k)
            end do
            !$omp end parallel do
        else
            buf(1)%reals = 0.0_real64
            do k = 1_int64, n
            buf(1)%reals(k) = values(k)
            end do
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_f64
    !
    module procedure extract_bool
        integer(int64) :: k, n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%ints(k) = 0_int64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%ints(k) = merge(1_int64, 0_int64, values(k))
            end do
            !$omp end parallel do
        else
            buf(1)%ints = 0_int64
            do k = 1_int64, n
            buf(1)%ints(k) = merge(1_int64, 0_int64, values(k))
            end do
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_bool
    !
    module procedure extract_chr
        integer(int64) :: k, n, total, pos, j, ln
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        ! Sorted on the FULL declared length, trailing blanks included, which is exactly
        ! Fortran's own `<` for equal-length strings -- so pf_is_sorted agrees with a
        ! hand-written a(k) <= a(k+1) loop rather than quietly trimming behind it.
        ln = int(len(values), int64)
        allocate(buf(1)%offsets(n + 1_int64))
        total = 0_int64
        do k = 1_int64, n + 1_int64
            buf(1)%offsets(k) = total
            total = total + ln
        end do
        buf(1)%offsets(n + 1_int64) = n * ln
        allocate(buf(1)%data(max(n * ln, 1_int64)))
        pos = 0_int64
        do k = 1_int64, n
            do j = 1_int64, ln
                buf(1)%data(pos + j) = values(k)(j:j)
            end do
            pos = pos + ln
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_chr
    !
    module procedure extract_date
        integer(int64) :: k, n, nth
        integer :: team
        logical, allocatable :: mask(:)
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%ints(k) = 0_int64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%ints(k) = int(values(k)%raw(), int64)
            end do
            !$omp end parallel do
        else
            buf(1)%ints = 0_int64
            do k = 1_int64, n
            buf(1)%ints(k) = int(values(k)%raw(), int64)
            end do
        end if
        allocate(mask(max(n, 1_int64)))
        ! Kept blanket for the same measured reason as the value buffer above.
        mask = .true.
        do k = 1_int64, n
            mask(k) = .not. values(k)%is_null()
        end do
        call valid_from_mask(mask, n, proc, buf(1)%valid)
    end procedure extract_date
    !
    module procedure extract_time
        integer(int64) :: k, n, nth
        integer :: team
        logical, allocatable :: mask(:)
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time. See feature_sort_report.md.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                buf(1)%ints(k) = 0_int64
            end do
            !$omp end parallel do
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
            buf(1)%ints(k) = values(k)%raw()
            end do
            !$omp end parallel do
        else
            buf(1)%ints = 0_int64
            do k = 1_int64, n
            buf(1)%ints(k) = values(k)%raw()
            end do
        end if
        allocate(mask(max(n, 1_int64)))
        ! Kept blanket for the same measured reason as the value buffer above.
        mask = .true.
        do k = 1_int64, n
            mask(k) = .not. values(k)%is_null()
        end do
        call valid_from_mask(mask, n, proc, buf(1)%valid)
    end procedure extract_time
    !
    module procedure extract_ts
        integer(int64) :: k, n, s
        integer(int32) :: ns
        logical, allocatable :: mask(:)
        !
        n = size(values, kind=int64)
        ! A timestamp is TWO integer keys: folding (seconds, nanoseconds) into one int64
        ! as s*10**9 + ns overflows outside roughly 1678-2262, well inside the range this
        ! library handles. Seconds lead, nanoseconds break their ties, and both carry the
        ! same validity so the pair can never disagree about which rows are null.
        allocate(buf(2))
        buf(:)%family = SK_INT
        buf(:)%descending = descending
        buf(:)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        buf(2)%ints = 0_int64
        allocate(mask(max(n, 1_int64)))
        mask = .true.
        do k = 1_int64, n
            call values(k)%get_raw(s, ns)
            buf(1)%ints(k) = s
            buf(2)%ints(k) = int(ns, int64)
            mask(k) = .not. values(k)%is_null()
        end do
        call valid_from_mask(mask, n, proc, buf(1)%valid)
        if (allocated(buf(1)%valid)) buf(2)%valid = buf(1)%valid
    end procedure extract_ts
    !
    module procedure extract_strcol
        integer(int64) :: n
        !
        n = values%size()
        call pack_string_store(values, n, descending, nulls_first, buf)
        call string_store_valid_flags(values, n, buf)
    end procedure extract_strcol
    !
    module procedure extract_col
        integer(int64) :: n
        integer :: kind
        !
        n = values%length()
        ! One column, whose element type is only known at runtime -- so this reduces to
        ! whichever of the four extractors above matches, and a PK_TIMESTAMP column
        ! produces two keys exactly as the bare type does.
        kind = values%kindof()
        if (values%colwidth() /= 1) then
            error stop EP // proc // ": a vector column cannot be a sort key; there is no " // &
                "defined order on a whole vector row"
        end if
        select case (kind)
        case (PK_INT32, PK_INT64, PK_LOGICAL, PK_DATE, PK_TIME)
            call extract_col_integer(values, n, descending, nulls_first, buf)
        case (PK_FLOAT32, PK_FLOAT64)
            call extract_col_real(values, n, descending, nulls_first, buf)
        case (PK_STRING)
            call extract_col_string(values, n, descending, nulls_first, buf)
        case (PK_TIMESTAMP)
            call extract_col_timestamp(values, n, descending, nulls_first, buf)
        case default
            block
                character(len=:), allocatable :: kname
                call parquet_kind_name(kind, kname)
                error stop EP // proc // ": a " // kname // " column cannot be a sort key"
            end block
        end select
        call col_valid_flags(values, n, buf)
    end procedure extract_col
    !
    !> Reads every integer-valued scalar kind of a column as int64 -- including logical and the
    !! two date/time kinds, whose stored values order exactly as the values they represent.
    !!
    !! **The kind switch is above the loop, and each arm is one whole-array assignment through
    !! `%data_ptr`** rather than a per-row `%get_at`. The obvious shape -- switch inside the loop --
    !! costs a `%kindof()` call, a `select case`, and an un-inlinable `%get_at` (which itself calls
    !! `check_kind` and `check_index`) on every row, for a decision that cannot change between rows.
    !! `col` needs the `target` attribute for `%data_ptr`'s own `target` dummy to yield a pointer
    !! that stays associated after it returns; the pointer is used only within this procedure, which
    !! is what the standard guarantees when the ultimate actual argument is not itself a target.
    subroutine extract_col_integer(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in), target :: col            !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        logical, pointer :: pb(:)
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        !
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! No pre-zero pass: every one of the n elements is written below, and the single padding
        ! element that `max(n, 1)` adds when n == 0 is the only one that needs initialising.
        if (n <= 0_int64) then
            buf(1)%ints = 0_int64
            return
        end if
        select case (col%kindof())
        case (PK_INT32)
            call col%data_ptr(p32)
            buf(1)%ints(1:n) = int(p32(1:n), int64)
        case (PK_INT64)
            call col%data_ptr(p64)
            buf(1)%ints(1:n) = p64(1:n)
        case (PK_LOGICAL)
            call col%data_ptr(pb)
            buf(1)%ints(1:n) = merge(1_int64, 0_int64, pb(1:n))
        case (PK_DATE)
            call col%data_ptr(pd)
            buf(1)%ints(1:n) = int(pd(1:n)%raw(), int64)   ! %raw is elemental
        case default
            call col%data_ptr(pt)
            buf(1)%ints(1:n) = pt(1:n)%raw()               ! %raw is elemental
        end select
    end subroutine extract_col_integer
    !
    !> Reads a float32 or float64 column as real64. NaNs pass straight through: the engine tiers
    !! them itself, exactly as it does for a read-time sort.
    !!
    !! Kind switch above the loop and one whole-array assignment per arm -- see
    !! `extract_col_integer` for why, and for why `col` carries `target`.
    subroutine extract_col_real(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in), target :: col            !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        real(real32), pointer :: p32(:)
        real(real64), pointer :: p64(:)
        !
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        if (n <= 0_int64) then
            buf(1)%reals = 0.0_real64
            return
        end if
        if (col%kindof() == PK_FLOAT32) then
            call col%data_ptr(p32)
            buf(1)%reals(1:n) = real(p32(1:n), real64)
        else
            call col%data_ptr(p64)
            buf(1)%reals(1:n) = p64(1:n)
        end if
    end subroutine extract_col_real
    !
    !> Packs a string column into the (offsets, data) pair the engine takes: row k occupies
    !! `data(offsets(k)+1 : offsets(k+1))`, with `offsets` 0-based because the C++ side indexes
    !! with it directly.
    !!
    !! Only ever reached for `PK_STRING` (the dispatch above checks the kind), and that kind stores a
    !! `parquet_string_column` -- so this hands the whole job to `pack_string_store`, which is the
    !! same worker the `parquet_string_column` entry point uses. Two entry points, one body: a sort
    !! key must not depend on which of the two types the caller happened to hold.
    subroutine extract_col_string(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        type(parquet_string_column), pointer :: store
        !
        call col%string_column(store)
        call pack_string_store(store, n, descending, nulls_first, buf)
    end subroutine extract_col_string
    !
    !> Shared body behind both string-key entry points: packs `store` into `buf(1)`.
    !!
    !! **Two bulk copies, not a loop.** A `parquet_string_column` already holds exactly the layout a
    !! sort key wants -- int64 offsets with `offsets(1) = 0`, and one packed payload -- so
    !! `%copy_buffers` moves both in two `memcpy`s. The obvious implementation instead walks the
    !! column calling `%get` per element, which allocates a deferred-length string, fills it, copies
    !! it out and frees it, **once per row**; that measured at roughly 0.11 s per allocation per 4 M
    !! elements, i.e. 19 % of a `parquet_string_column` sort and 33 % of a `parquet_column` one,
    !! because the latter paid it twice. Do not reintroduce a per-element `%get` here; see
    !! `feature_risks.md` Risk-60.
    !!
    !! **A null is zero-width in both layouts** (`set_null` compacts the payload), so the copy needs
    !! no null special-casing -- a null row simply occupies an empty range, which is what the old
    !! `%get(..., allow_null=.true.)` produced for it. Validity is carried separately, below.
    subroutine pack_string_store(store, n, descending, nulls_first, buf)
        type(parquet_string_column), intent(in) :: store           !! the packed string storage.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int64) :: total
        !
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        total = store%character_size()
        allocate(buf(1)%offsets(n + 1_int64))
        allocate(buf(1)%data(max(total, 1_int64)))
        call store%copy_buffers(buf(1)%offsets, buf(1)%data)
    end subroutine pack_string_store
    !
    !> Fills `buf(1)%valid` from a string store's nulls, if it has any.
    !!
    !! **Separate from `pack_string_store` on purpose.** The `parquet_column` entry point already
    !! runs `col_valid_flags` after its extractor, so a packer that also filled `valid` would
    !! allocate it twice and abort -- which is exactly what happened when the two string entry points
    !! were first merged. Only the `parquet_string_column` entry point, which has no such follow-up,
    !! calls this.
    !!
    !! A bit walk rather than a copy, because the engine takes one int8 per row where the column
    !! packs eight rows per byte. `%null_count()` answers in O(1), so a null-free column skips the
    !! array entirely -- and an unallocated `valid` is what tells the engine there are none.
    subroutine string_store_valid_flags(store, n, buf)
        type(parquet_string_column), intent(in) :: store !! the packed string storage.
        integer(int64), intent(in) :: n                  !! row count.
        type(sort_key_buf), intent(inout) :: buf(:)      !! the key to attach validity to.
        integer(int64) :: k
        !
        if (store%null_count() <= 0_int64) return
        allocate(buf(1)%valid(max(n, 1_int64)))
        buf(1)%valid = 1_c_int8_t
        do k = 1_int64, n
            if (store%is_null(k)) buf(1)%valid(k) = 0_c_int8_t
        end do
    end subroutine string_store_valid_flags
    !
    !> Splits a timestamp column into its (seconds, nanoseconds) pair of integer keys -- see
    !! `extract_ts` for why a timestamp becomes two keys rather than one.
    !!
    !! Reaches the elements through `%data_ptr` rather than a per-row `%get_at` -- see
    !! `extract_col_integer` for why, and for why `col` carries `target`. This one keeps a row loop
    !! (rather than one whole-array assignment per key) because `%get_raw` yields the two halves
    !! together: splitting it into two elemental array calls would either walk the column twice or
    !! need an int32 temporary the whole length of the column, to save an elemental call the
    !! compiler can already inline.
    subroutine extract_col_timestamp(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in), target :: col            !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives two keys.
        integer(int64) :: k, s
        integer(int32) :: ns
        type(parquet_timestamp), pointer :: pts(:)
        !
        allocate(buf(2))
        buf(:)%family = SK_INT
        buf(:)%descending = descending
        buf(:)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))
        if (n <= 0_int64) then
            buf(1)%ints = 0_int64
            buf(2)%ints = 0_int64
            return
        end if
        call col%data_ptr(pts)
        do k = 1_int64, n
            call pts(k)%get_raw(s, ns)
            buf(1)%ints(k) = s
            buf(2)%ints(k) = int(ns, int64)
        end do
    end subroutine extract_col_timestamp
    !
    !> Copies a column's own per-row validity onto every key extracted from it, leaving it
    !! UNALLOCATED for a null-free column -- which is the engine's no-nulls fast path, and the same
    !! convention `%row_validity` itself uses by leaving its own result unallocated.
    !!
    !! **Deliberately built from per-row `%is_null` rather than `%row_validity`**, which would be
    !! the obvious choice: `%row_validity` is `intent(inout)` (a temporal column rescans and
    !! caches its null count there), so reaching it from a public `pf_argsort(column)` would mean
    !! either making that argument `intent(inout)` -- wrong for a query, and it would stop a caller
    !! passing their own `intent(in)` dummy -- or taking a copy of the column, which is a full deep
    !! copy of every value on the way to sorting it. `%is_null` is `intent(in)` and is always
    !! correct without consulting any cache, so it costs one extra O(n) pass in front of an
    !! O(n log n) sort and nothing else.
    subroutine col_valid_flags(col, n, buf)
        type(parquet_column), intent(in) :: col                 !! the key column.
        integer(int64), intent(in) :: n                         !! row count.
        type(sort_key_buf), intent(inout) :: buf(:)             !! the keys extracted from it.
        integer :: ik
        logical, allocatable :: rowmask(:)
        !
        ! O(1), and it answers the question the scan below was asking. A bitmap kind that has
        ! never had a null set allocates no bitmap at all, so there is nothing to look at -- which
        ! turns the null-free case, the common one, from a full pass of n un-inlinable %is_null
        ! calls into a single test. The temporal kinds answer .true. unconditionally (their null
        ! state lives inside the element, not in a bitmap), so they fall through to the scan on
        ! their own without needing a kind test here; that is the right answer for them, since a
        ! scan is genuinely the only way to know.
        if (.not. col%has_validity_storage()) return
        ! ONE bulk call, not two per-row scans. This used to walk the column twice with
        ! `col%is_null(k)` -- once to find out whether any null existed, once to record which --
        ! i.e. up to 2n un-inlinable cross-module calls, each re-checking the index and
        ! re-dispatching the kind. `%row_validity` does the same work by walking the validity
        ! bitmap a 64-bit WORD at a time, skipping 64 valid rows whenever a word is zero, and it
        ! specialises the string and temporal kinds above their loops rather than inside them.
        !
        ! It was unreachable from here until it became `intent(in)`: `pf_argsort(column)` holds its
        ! column by `intent(in)`, and `%row_validity` was `intent(inout)` purely so a temporal
        ! column could refresh a null cache in passing. Measured on a 4M-row int32 sort key, this
        ! phase went from ~20 ms to ~2 ms, which at 0.1% null density is most of what separated a
        ! null-bearing sort from a null-free one. THIS CALL IS ALSO THE ENFORCEMENT: narrowing
        ! %row_validity back to intent(inout) does not fail a test, it fails the build, here.
        !
        ! An unallocated result means "no nulls" (F2018 15.5.2.12 is why that convention exists
        ! throughout this library), which is exactly the early return the old `any_null` scan gave.
        call col%row_validity(rowmask)
        if (.not. allocated(rowmask)) return
        allocate(buf(1)%valid(max(n, 1_int64)))
        ! `merge` rather than a loop: one vectorisable pass over a LOGICAL array, against n
        ! branches. The two arrays are distinct, so no aliasing temporary is involved.
        buf(1)%valid(1:n) = merge(1_c_int8_t, 0_c_int8_t, rowmask(1:n))
        do ik = 2, size(buf)
            buf(ik)%valid = buf(1)%valid
        end do
    end subroutine col_valid_flags
    !
    module procedure add_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i32(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_i32
    !
    module procedure add_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i64(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_i64
    !
    module procedure add_f32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f32(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_f32
    !
    module procedure add_f64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f64(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_f64
    !
    module procedure add_bool
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_bool(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_bool
    !
    module procedure add_chr
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_chr(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_chr
    !
    module procedure add_date
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_date(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_date
    !
    module procedure add_time
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_time(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_time
    !
    module procedure add_ts
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_ts(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_ts
    !
    module procedure add_strcol
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_strcol(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_strcol
    !
    module procedure add_col
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_col(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_col
    !
    module procedure keys_count
        ! The CALLER's count -- one per %add call -- not self%nkeys, which counts engine keys and
        ! answers 2 for a lone parquet_timestamp. See the add_ekeys component.
        n = 0
        if (allocated(self%add_ekeys)) n = size(self%add_ekeys)
    end procedure keys_count
    !
    module procedure keys_clear
        self%nkeys = 0
        self%nrows = -1_int64
        if (allocated(self%keys)) deallocate(self%keys)
        if (allocated(self%add_ekeys)) deallocate(self%add_ekeys)
    end procedure keys_clear
    !
    module procedure resolve_group_nkeys
        integer :: ncaller, k
        character(len=32) :: got_str, have_str
        !
        ncaller = 0
        if (allocated(keys%add_ekeys)) ncaller = size(keys%add_ekeys)
        if (.not. present(group_nkeys)) then
            group_ekeys = keys%nkeys
            return
        end if
        if (.not. want_offsets) then
            error stop EP // proc // ": group_nkeys was given without group_offsets; on its own " // &
                "it changes nothing, so ask for the boundaries too or drop it"
        end if
        if (group_nkeys < 1 .or. group_nkeys > ncaller) then
            write (got_str, "(i0)") group_nkeys
            write (have_str, "(i0)") ncaller
            error stop EP // proc // ": group_nkeys is " // trim(got_str) // ", which is not " // &
                "between 1 and the " // trim(have_str) // " keys given"
        end if
        ! Caller keys to engine keys. A prefix of the caller's keys is a prefix of the engine's,
        ! because %add appends its engine keys contiguously and in order.
        group_ekeys = 0
        do k = 1, group_nkeys
            group_ekeys = group_ekeys + keys%add_ekeys(k)
        end do
    end procedure resolve_group_nkeys
    !
    module procedure keys_append
        type(sort_key_buf), allocatable :: bigger(:)
        integer, allocatable :: more_ekeys(:)
        integer(int64) :: n
        integer :: ik
        character(len=32) :: got_str, want_str
        !
        n = key_rows(buf(1))
        if (self%nkeys == 0) then
            self%nrows = n
        else if (n /= self%nrows) then
            write (got_str, "(i0)") n
            write (want_str, "(i0)") self%nrows
            error stop EP // proc // ": every key must describe the same number of rows; this " // &
                "one has " // trim(got_str) // " where the first key has " // trim(want_str)
        end if
        ! Grown exactly, not geometrically: a key list is a handful of entries, and each entry
        ! holds only allocatable descriptors (the value buffers themselves are moved, not copied).
        if (.not. allocated(self%keys)) allocate(self%keys(0))
        allocate(bigger(self%nkeys + size(buf)))
        do ik = 1, self%nkeys
            call move_key(self%keys(ik), bigger(ik))
        end do
        do ik = 1, size(buf)
            call move_key(buf(ik), bigger(self%nkeys + ik))
        end do
        ! One entry per %add call, holding how many engine keys THIS call contributed -- the only
        ! record of the caller-versus-engine key distinction, and what %nkeys_added and group_nkeys
        ! both read. Recorded here rather than in each %add specific so a new key type cannot
        ! forget it.
        if (.not. allocated(self%add_ekeys)) allocate(self%add_ekeys(0))
        allocate(more_ekeys(size(self%add_ekeys) + 1))
        more_ekeys(1:size(self%add_ekeys)) = self%add_ekeys
        more_ekeys(size(more_ekeys)) = size(buf)
        call move_alloc(more_ekeys, self%add_ekeys)
        self%nkeys = self%nkeys + size(buf)
        call move_alloc(bigger, self%keys)
        deallocate(buf)
    end procedure keys_append
    !
    !> Moves one key's buffers from `src` to `dst` without copying them.
    subroutine move_key(src, dst)
        type(sort_key_buf), intent(inout) :: src !! the key to move from; left empty.
        type(sort_key_buf), intent(inout) :: dst !! the key to move into.
        dst%family = src%family
        dst%descending = src%descending
        dst%nulls_first = src%nulls_first
        if (allocated(src%ints)) call move_alloc(src%ints, dst%ints)
        if (allocated(src%reals)) call move_alloc(src%reals, dst%reals)
        if (allocated(src%offsets)) call move_alloc(src%offsets, dst%offsets)
        if (allocated(src%data)) call move_alloc(src%data, dst%data)
        if (allocated(src%valid)) call move_alloc(src%valid, dst%valid)
    end subroutine move_key
    !
    !> How many rows one extracted key describes.
    pure function key_rows(buf) result(n)
        type(sort_key_buf), intent(in) :: buf !! the key.
        integer(int64) :: n                   !! its row count.
        select case (buf%family)
        case (SK_REAL)
            n = size(buf%reals, kind=int64)
        case (SK_STR)
            n = size(buf%offsets, kind=int64) - 1_int64
        case default
            n = size(buf%ints, kind=int64)
        end select
    end function key_rows
    !
    module procedure valid_from_mask
        integer(int64) :: k, m
        character(len=32) :: got_str, want_str
        !
        m = size(mask, kind=int64)
        if (m /= n) then
            write (got_str, "(i0)") m
            write (want_str, "(i0)") n
            error stop EP // proc // ": is_valid has " // trim(got_str) // " elements but the " // &
                "values have " // trim(want_str)
        end if
        ! Left UNALLOCATED when nothing is null: the caller turns that into a null pointer, which
        ! the engine reads as "no nulls" and takes its own fast path for. Answering `.true.` for
        ! every row instead would be correct and measurably slower.
        if (all(mask(1:n))) return
        allocate(valid(max(n, 1_int64)))
        valid = 1_c_int8_t
        do k = 1_int64, n
            if (.not. mask(k)) valid(k) = 0_c_int8_t
        end do
    end procedure valid_from_mask
    !
    module procedure drive_engine
        type(c_ptr) :: builder
        integer(int64) :: status, nthreads
        integer :: ik
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once" ! GCOVR_EXCL_LINE
        end if
        ! Sized EXACTLY, never max(nrows, 1): a zero-row sort must hand back a zero-length
        ! permutation, or `size(perm)` lies and a caller's `do k = 1, size(perm)` reads element 1
        ! of an empty array. The engine is not called at all below two rows, so nothing downstream
        ! needs the one-element floor the extraction buffers use.
        allocate(perm(nrows))
        ! Resolved BEFORE the identity fill, not after, so the fill can be threaded too. It is one
        ! of three whole-column serial loops that together were 48% of a 64-thread end-to-end sort
        ! before they were threaded -- see `app/benchmark_sort_tail.f90`, which sizes each one.
        call resolve_thread_count(threads, nrows, nthreads)
        call fill_identity(perm, nrows, nthreads)
        if (nrows < 2_int64) return
        if (dbg_fortran_engine) then
            ! Stage 2 scaffolding -- see `dbg_fortran_engine`'s declaration. **Stage 4 made this
            ! branch honour `threads`**, and the resolution above is deliberately SHARED with the
            ! C++ path below rather than repeated here, so the two engines are handed the same
            ! number by the same procedure and an A/B compares engines rather than policies.
            !
            ! The A/B stays valid at every thread count for a reason that is about the ordering and
            ! not about either implementation: both comparators end in a row-index tiebreaker, so no
            ! two distinct rows compare equal, exactly one permutation is correct, and a threaded
            ! answer that differs from a serial one is WRONG rather than merely different.
            call sort_build_permutation_threaded(keys, nrows, nthreads, perm)
            return
        end if
        if (size(keys) == 1) then
            ! One key needs no builder at all: the one-shot entry points BORROW the buffer that
            ! was just extracted, so this saves a handle allocation and a second copy of every
            ! value. Multi-key has to go through the builder, which owns its keys.
            call engine_one_shot(keys(1), nrows, nthreads, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build(builder, nthreads, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects -- kept
            ! because silently ignoring a nonzero status is how a real failure goes unnoticed.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine
    !
    module procedure drive_engine_partial
        type(c_ptr) :: builder
        integer(int64) :: status, ik
        integer :: jk
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(count))
        do ik = 1_int64, count
            perm(ik) = ik
        end do
        if (count < 1_int64 .or. nrows < 2_int64) return
        if (dbg_fortran_engine) then
            call sort_partial_permutation(keys, nrows, count, perm)
            return
        end if
        if (size(keys) == 1) then
            call engine_one_shot_partial(keys(1), nrows, count, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do jk = 1, size(keys)
            call engine_add_key(builder, keys(jk), nrows)
        end do
        status = parquet_sort_builder_build_partial(builder, count, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine_partial
    !
    module procedure engine_nth_index
        type(c_ptr) :: builder
        integer :: ik
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        if (dbg_fortran_engine) then
            call sort_nth_index(keys, nrows, nth, idx)
            return
        end if
        if (size(keys) == 1) then
            call engine_one_shot_nth(keys(1), nrows, nth, idx)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        idx = parquet_sort_builder_nth_element(builder, nth)
        call parquet_sort_builder_free(builder)
        if (idx < 1_int64) then
            ! The C side answers 0 for an empty key list or an out-of-range rank; both are already
            ! rejected above and by the caller's own bounds check.
            error stop EP // proc // ": the sort engine could not resolve that rank" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_nth_index
    !
    module procedure resolve_count
        character(len=32) :: n_str
        !
        ! Clamped, not refused: `n` is very often derived (a fraction of a row count, a config
        ! value, a post-filter survivor count), and aborting would put min(n, size(v)) at every
        ! call site. A NEGATIVE n is a different thing -- a caller error, not a boundary.
        if (n < 0) then
            write (n_str, "(i0)") n
            error stop EP // proc // ": n is " // trim(n_str) // ", which is negative"
        end if
        count = min(int(n, int64), nrows)
    end procedure resolve_count
    !
    !> Partially argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_partial(key, nrows, count, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: count           !! leading entries to order.
        integer(int64), intent(inout) :: perm(:)      !! receives `count` 1-based indices.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_partial_argsort_double(nrows, key%reals, vp, df, nf, count, perm)
        case (SK_STR)
            call parquet_sort_partial_argsort_string(nrows, key%offsets, key%data, vp, df, nf, count, perm)
        case default
            call parquet_sort_partial_argsort_int64(nrows, key%ints, vp, df, nf, count, perm)
        end select
    end subroutine engine_one_shot_partial
    !
    !> Resolves one already-extracted key's nth index through the matching one-shot entry point.
    subroutine engine_one_shot_nth(key, nrows, nth, idx)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nth             !! 1-based rank wanted.
        integer(int64), intent(out) :: idx            !! 1-based row index at that rank.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            idx = parquet_sort_nth_index_double(nrows, key%reals, vp, df, nf, nth)
        case (SK_STR)
            idx = parquet_sort_nth_index_string(nrows, key%offsets, key%data, vp, df, nf, nth)
        case default
            idx = parquet_sort_nth_index_int64(nrows, key%ints, vp, df, nf, nth)
        end select
    end subroutine engine_one_shot_nth
    !
    module procedure engine_is_sorted
        type(c_ptr) :: builder
        integer(int64) :: res
        integer :: ik
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once" ! GCOVR_EXCL_LINE
        end if
        answer = .true.
        if (nrows < 2_int64) return
        if (dbg_fortran_engine) then
            answer = sort_is_sorted(keys, nrows)
            return
        end if
        if (size(keys) == 1) then
            call engine_one_shot_is_sorted(keys(1), nrows, answer)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        res = parquet_sort_builder_is_sorted(builder)
        call parquet_sort_builder_free(builder)
        if (res < 0_int64) then
            error stop EP // proc // ": the sort engine had no key to test" ! GCOVR_EXCL_LINE
        end if
        answer = res == 1_int64
    end procedure engine_is_sorted
    !
    !> Argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot(key, nrows, nthreads, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nthreads        !! resolved thread count; 1 sorts serially.
        integer(int64), intent(inout) :: perm(:)      !! receives the 1-based permutation.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_argsort_double(nrows, key%reals, vp, df, nf, nthreads, perm)
        case (SK_STR)
            call parquet_sort_argsort_string(nrows, key%offsets, key%data, vp, df, nf, nthreads, perm)
        case default
            call parquet_sort_argsort_int64(nrows, key%ints, vp, df, nf, nthreads, perm)
        end select
    end subroutine engine_one_shot
    !
    !> Tests one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_is_sorted(key, nrows, answer)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        logical, intent(out) :: answer                !! .true. when already in order.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        integer(int64) :: res
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            res = parquet_sort_is_sorted_double(nrows, key%reals, vp, df, nf)
        case (SK_STR)
            res = parquet_sort_is_sorted_string(nrows, key%offsets, key%data, vp, df, nf)
        case default
            res = parquet_sort_is_sorted_int64(nrows, key%ints, vp, df, nf)
        end select
        answer = res == 1_int64
    end subroutine engine_one_shot_is_sorted
    !
    !> Adds one already-extracted key to a C++ builder.
    subroutine engine_add_key(builder, key, nrows)
        type(c_ptr), intent(in) :: builder            !! the builder handle.
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_builder_add_key_double(builder, key%reals, vp, df, nf)
        case (SK_STR)
            call parquet_sort_builder_add_key_string(builder, key%offsets, key%data, vp, df, nf)
        case default
            call parquet_sort_builder_add_key_int64(builder, key%ints, vp, df, nf)
        end select
    end subroutine engine_add_key
    !
    !> The three scalars every engine call needs: a pointer to the validity array (or a null
    !! pointer when the key has no nulls) and the two order flags as int8.
    subroutine key_flags(key, valid_ptr, desc_flag, nulls_flag)
        type(sort_key_buf), intent(in), target :: key   !! the key.
        type(c_ptr), intent(out) :: valid_ptr           !! its validity array, or C_NULL_PTR.
        integer(c_int8_t), intent(out) :: desc_flag     !! nonzero for descending.
        integer(c_int8_t), intent(out) :: nulls_flag    !! nonzero to place nulls first.
        !
        valid_ptr = c_null_ptr
        if (allocated(key%valid)) valid_ptr = c_loc(key%valid)
        desc_flag = merge(1_c_int8_t, 0_c_int8_t, key%descending)
        nulls_flag = merge(1_c_int8_t, 0_c_int8_t, key%nulls_first)
    end subroutine key_flags
    !
    module procedure check_rank
        character(len=32) :: a_str, b_str
        !
        if (nth < 1_int64 .or. nth > nrows) then
            write (a_str, "(i0)") nth
            write (b_str, "(i0)") nrows
            error stop EP // proc // ": nth is " // trim(a_str) // ", which is outside 1.." // &
                trim(b_str)
        end if
    end procedure check_rank
    !
    module procedure key_valid_count
        integer(int64) :: k
        !
        ! An unallocated `valid` is the module's "no nulls at all" convention, so the whole array
        ! counts -- the same fast path the engine itself takes.
        if (.not. allocated(keys(1)%valid)) then
            n_valid = nrows
            return
        end if
        n_valid = 0_int64
        do k = 1_int64, nrows
            if (keys(1)%valid(k) /= 0_c_int8_t) n_valid = n_valid + 1_int64
        end do
    end procedure key_valid_count
    !
    module procedure fold_token
        integer :: k, ic
        !
        tok = trim(adjustl(text))
        do k = 1, len(tok)
            ic = iachar(tok(k:k))
            if (ic >= iachar("A") .and. ic <= iachar("Z")) tok(k:k) = achar(ic + 32)
        end do
    end procedure fold_token
    !
    module procedure resolve_rounding
        character(len=:), allocatable :: tok, shown
        !
        mode = RND_NEAREST
        if (.not. present(rounding)) return
        call fold_token(rounding, tok)
        select case (tok)
        case ("nearest")
            mode = RND_NEAREST
        case ("down")
            mode = RND_DOWN
        case ("up")
            mode = RND_UP
        case default
            ! Capped to a short preview: the caller controls this string's length, and ifx's
            ! ERROR STOP runtime corrupts the heap once the composed message reaches 8192 bytes
            ! (CLAUDE.md). Same shape as parquet_filter_add's own rule preview.
            shown = trim(adjustl(rounding))
            if (len(shown) > 100) shown = shown(1:100) // "..."
            error stop EP // proc // ": rounding='" // shown // "' is not recognized; use " // &
                "'nearest' (the default), 'down' or 'up'"
        end select
    end procedure resolve_rounding
    !
    module procedure quantile_rank
        real(real64) :: pos
        !
        if (quantile < 0.0_real64 .or. quantile > 1.0_real64 .or. quantile /= quantile) then
            ! The NaN arm is what the self-comparison catches; ieee_is_nan would need another
            ! import here for one test, and this expression is exact with no arithmetic drift.
            error stop EP // proc // ": quantile must lie on a 0-1 scale (note: NOT 0-100)"
        end if
        if (n_valid < 1_int64) then
            error stop EP // proc // ": every value is null, so no quantile exists; guard with " // &
                "count(is_valid) (or the column's own null count) if that can happen"
        end if
        ! Position on the 0-based index scale of the non-null values, so quantile=0 gives the
        ! smallest and quantile=1 the largest exactly, with no rounding involved at either end.
        pos = quantile * real(n_valid - 1_int64, real64)
        select case (mode)
        case (RND_DOWN)
            rank = int(floor(pos), int64) + 1_int64
        case (RND_UP)
            rank = int(ceiling(pos), int64) + 1_int64
        case default
            rank = int(nint(pos, int64), int64) + 1_int64
        end select
        if (rank < 1_int64) rank = 1_int64
        if (rank > n_valid) rank = n_valid
    end procedure quantile_rank
    !
    module procedure check_permutation
        integer(int8), allocatable :: seen(:)
        integer(int64) :: k, v, word
        logical :: do_scan
        character(len=32) :: a_str, b_str
        !
        do_scan = .true.
        if (present(scan)) do_scan = scan
        if (size(perm, kind=int64) /= n) then
            write (a_str, "(i0)") size(perm, kind=int64)
            write (b_str, "(i0)") n
            error stop EP // proc // ": perm has " // trim(a_str) // " elements but the values " // &
                "have " // trim(b_str)
        end if
        if (n < 1_int64) return
        ! The length check above is unconditional; only the walk below is skippable. See the
        ! interface's own note for why a caller's promise cannot cover a wrong length.
        if (.not. do_scan) return
        ! A BIT-PACKED seen-set, not a LOGICAL array: gfortran's default LOGICAL is 32 bits, so a
        ! plain seen(n) would cost 4n bytes of scratch to validate a permutation whose own payload
        ! is 8n -- a 50% overhead on an operation whose whole point is to be cheap. This is n/8.
        allocate(seen((n + 7_int64) / 8_int64))
        seen = 0_int8
        do k = 1_int64, n
            v = perm(k)
            if (v < 1_int64 .or. v > n) then
                write (a_str, "(i0)") k
                write (b_str, "(i0)") v
                error stop EP // proc // ": perm(" // trim(a_str) // ") is " // trim(b_str) // &
                    ", which is outside the valid index range"
            end if
            word = (v - 1_int64) / 8_int64 + 1_int64
            if (btest(seen(word), int(mod(v - 1_int64, 8_int64)))) then
                write (a_str, "(i0)") v
                error stop EP // proc // ": perm is not a permutation -- the index " // &
                    trim(a_str) // " appears more than once"
            end if
            seen(word) = ibset(seen(word), int(mod(v - 1_int64, 8_int64)))
        end do
    end procedure check_permutation
    !
    module procedure pf_sort_threads
        use parquet_settings, only : parquet_get_sort_threads
#ifdef _OPENMP
        use omp_lib, only : omp_get_max_threads, omp_in_parallel
#endif
        integer :: cap
        !
        n = 1
#ifdef _OPENMP
        ! Serial inside a parallel region, deliberately. This is not a refusal and not a
        ! correctness guard -- it picks a DEFAULT, exactly as parallel_prefetch_ok
        ! (parquet_tables_read.f90) does for the table's own internally-parallel read, whose
        ! comment states the reason: nested regions are the caller's business. Without it, T
        ! OpenMP threads would each ask for T more, and T*T oversubscription is slower than not
        ! threading at all. An EXPLICIT threads= is still honoured there -- see
        ! resolve_thread_count, which only consults this when the caller said nothing.
        if (.not. omp_in_parallel()) n = omp_get_max_threads()
#endif
        ! parquet_set_sort_threads CAPS the automatic answer; it never raises it, and it never
        ! overrides the parallel-region rule above -- a caller who capped sorting at 8 said nothing
        ! about what should happen inside someone else's parallel region, and lifting the serial
        ! answer back to 8 there is exactly the T*T oversubscription that rule exists to prevent.
        ! This is the ONE place the setting is read: Risk-40 records that pf_sort_threads is public
        ! precisely so a read-time sort_by= and a raw-array sort ask the same question, and a second
        ! reader is how the two would come to disagree.
        cap = parquet_get_sort_threads()
        if (cap > 0 .and. cap < n) n = cap
    end procedure pf_sort_threads
    !
    module procedure resolve_thread_count
        !
        if (present(threads)) then
            ! An explicit request is honoured wherever it is made, including inside a parallel
            ! region: the caller has said what they want, and refusing it there would leave no way
            ! to thread a sort at all from code that is itself parallel.
            count = max(1_int64, int(threads, int64))
        else
            count = int(pf_sort_threads(), int64)
        end if
        ! Never more threads than rows; the C++ side clamps again by its own minimum chunk size.
        if (count > nrows) count = max(nrows, 1_int64)
    end procedure resolve_thread_count
    !
    module procedure tail_team
        use parquet_settings, only : parquet_get_sort_parallel_min_rows
        !
        team = 1
        ! The floor. A whole-column loop of a few thousand elements is dominated by the cost of
        ! opening the team, and the same setting already governs whether the RADIX threads at all,
        ! so the two decline together rather than on separate rules.
        if (nthreads <= 1_int64) return
        if (n < parquet_get_sort_parallel_min_rows()) return
        team = int(min(nthreads, int(huge(0), int64)))
        if (team < 1) team = 1
    end procedure tail_team
    !
    module procedure fill_identity
        integer(int64) :: ik
        integer :: team
        !
        team = tail_team(nthreads, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(ik) schedule(static)
            do ik = 1_int64, n
                perm(ik) = ik
            end do
            !$omp end parallel do
            return
        end if
        do ik = 1_int64, n
            perm(ik) = ik
        end do
    end procedure fill_identity
    !
    module procedure narrow_perm
        integer(int64) :: n, nthreads, k
        character(len=32) :: n_str
        integer :: team
        !
        n = size(perm64, kind=int64)
        if (n > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array that
            ! would trip it cannot be built by any fixture this repository can run. Kept because
            ! the alternative is a silent truncation into a plausible wrong index.
            write (n_str, "(i0)") n
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, which " // &
                "does not fit an int32 permutation; declare perm as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        allocate(perm32(n))
        ! Threaded for the same reason `fill_identity` is: at 64 threads this whole-column copy was
        ! ~13% of an end-to-end argsort while every other phase had been parallelised around it.
        call resolve_thread_count(threads, n, nthreads)
        team = tail_team(nthreads, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                perm32(k) = int(perm64(k), int32)
            end do
            !$omp end parallel do
            return
        end if
        perm32 = int(perm64, int32)
    end procedure narrow_perm
    !
    module procedure narrow_offsets
        integer(int64) :: sentinel
        character(len=32) :: n_str
        !
        ! The SENTINEL, not the length -- and the difference is exactly one row. A permutation's
        ! largest entry is n, but this array's is n + 1, so at n == huge(int32) narrow_perm's own
        ! test passes while this one must not: a wrapped sentinel makes the last group's
        ! o(g+1) - 1 a huge negative bound, i.e. a silently wrong slice instead of an abort.
        sentinel = offsets64(size(offsets64))
        if (sentinel > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-row sort; the same reason
            ! narrow_perm's own guard has no test either. Kept because the failure it prevents is
            ! silent, which is precisely when an untestable guard earns its place. The marker sits
            ! INSIDE the test: the test itself runs on every call, and excluding it too would file
            ! it forever as a stale-exclusion candidate in tools/coverage.sh's own report.
            write (n_str, "(i0)") sentinel - 1_int64
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, so the " // &
                "group offsets do not fit int32; declare group_offsets as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        allocate(offsets32(size(offsets64)))
        offsets32 = int(offsets64, int32)
    end procedure narrow_offsets
    !
    module procedure narrow_i64
        character(len=32) :: v_str
        !
        if (value > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array that
            ! would trip it cannot be built by any fixture this repository can run. Kept because
            ! the alternative is a silent truncation into a plausible wrong index.
            write (v_str, "(i0)") value
            error stop EP // proc // ": the " // noun // " is " // trim(v_str) // ", which does " // &
                "not fit an int32; declare that argument as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        dst = int(value, int32)
    end procedure narrow_i64
    !
    module procedure narrow_i64_array
        integer(int64) :: n, biggest
        character(len=32) :: v_str
        !
        n = size(src, kind=int64)
        if (n > 0_int64) then
            biggest = maxval(src)
            if (biggest > int(huge(1_int32), int64)) then
                ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; see narrow_perm.
                write (v_str, "(i0)") biggest
                error stop EP // proc // ": the largest " // noun // " is " // trim(v_str) // &
                    ", which does not fit an int32; declare that argument as integer(int64)"
                ! GCOVR_EXCL_STOP
            end if
        end if
        allocate(dst(n))
        dst = int(src, int32)
    end procedure narrow_i64_array
    !
    module procedure resolve_rank_method
        character(len=:), allocatable :: tok, shown
        !
        mode = RANK_COMPETITION
        if (.not. present(method)) return
        call fold_token(method, tok)
        select case (tok)
        case ("competition")
            mode = RANK_COMPETITION
        case ("dense")
            mode = RANK_DENSE
        case ("ordinal")
            mode = RANK_ORDINAL
        case default
            ! Capped to a short preview, exactly as resolve_rounding is: the caller controls this
            ! string's length and ifx's ERROR STOP runtime corrupts the heap at 8192 bytes.
            shown = trim(adjustl(method))
            if (len(shown) > 100) shown = shown(1:100) // "..."
            error stop EP // proc // ": method='" // shown // "' is not recognized; use " // &
                "'competition' (the default), 'dense' or 'ordinal'"
        end select
    end procedure resolve_rank_method
    !
    module procedure key_null_mask
        integer(int64) :: k
        !
        allocate(isnull(max(nrows, 1_int64)))
        isnull = .false.
        if (.not. allocated(keys(1)%valid)) return
        do k = 1_int64, nrows
            isnull(k) = keys(1)%valid(k) == 0_c_int8_t
        end do
    end procedure key_null_mask
    !
    module procedure key_value_count
        integer(int64) :: k
        logical :: has_valid, is_real
        !
        ! Tier 0 of sort_tier_of, counted on the Fortran side rather than asked of the engine: it
        ! is the same two questions (is this row null, and -- for a real key only -- is it a NaN)
        ! and neither needs a comparison. A NaN is skipped because it is not a minimum or a maximum
        ! of anything, while remaining an ordinary value everywhere else in this module.
        has_valid = allocated(keys(1)%valid)
        is_real = keys(1)%family == SK_REAL
        n_value = 0_int64
        do k = 1_int64, nrows
            if (has_valid) then
                if (keys(1)%valid(k) == 0_c_int8_t) cycle
            end if
            if (is_real) then
                if (ieee_is_nan(keys(1)%reals(k))) cycle
            end if
            n_value = n_value + 1_int64
        end do
    end procedure key_value_count
    !
    module procedure check_sorted_input
        logical :: ok
        !
        call engine_is_sorted(keys, nrows, proc, ok)
        if (.not. ok) then
            error stop EP // proc // ": " // what // " is not sorted in the order given by " // &
                "descending/nulls_first; sort it first, or pass assume_sorted=.true. only for " // &
                "an order you have already established"
        end if
    end procedure check_sorted_input
    !
    module procedure buf_append
        integer :: ik
        integer(int64) :: total_d, total_s, k, n
        integer(int64), allocatable :: newoff(:), newints(:)
        real(real64), allocatable :: newreals(:)
        character(kind=c_char), allocatable :: newdata(:)
        integer(c_int8_t), allocatable :: newvalid(:)
        !
        if (.not. allocated(dst) .or. .not. allocated(src)) then
            ! Both come straight from an extract_* call, which always allocates.
            error stop EP // proc // ": internal error: a sort key was not extracted" ! GCOVR_EXCL_LINE
        end if
        if (size(dst) /= size(src)) then
            ! Only reachable if two different types were extracted into one pair, which no
            ! generated caller does -- every one extracts both sides with the same extractor.
            error stop EP // proc // ": internal error: mismatched key counts" ! GCOVR_EXCL_LINE
        end if
        n = nd + ns
        do ik = 1, size(dst)
            select case (dst(ik)%family)
            case (SK_REAL)
                allocate(newreals(max(n, 1_int64)))
                newreals = 0.0_real64
                if (nd > 0_int64) newreals(1:nd) = dst(ik)%reals(1:nd)
                if (ns > 0_int64) newreals(nd + 1_int64:n) = src(ik)%reals(1:ns)
                call move_alloc(newreals, dst(ik)%reals)
            case (SK_STR)
                ! The offsets are byte positions into `data`, so the appended half's have to be
                ! rebased by however many bytes the first half occupies -- this is the one family
                ! where concatenating two keys is not just concatenating two arrays.
                total_d = dst(ik)%offsets(nd + 1_int64)
                total_s = src(ik)%offsets(ns + 1_int64)
                allocate(newoff(n + 1_int64))
                newoff(1:nd + 1_int64) = dst(ik)%offsets(1:nd + 1_int64)
                do k = 1_int64, ns
                    newoff(nd + 1_int64 + k) = total_d + src(ik)%offsets(k + 1_int64)
                end do
                allocate(newdata(max(total_d + total_s, 1_int64)))
                if (total_d > 0_int64) newdata(1:total_d) = dst(ik)%data(1:total_d)
                if (total_s > 0_int64) newdata(total_d + 1_int64:total_d + total_s) = src(ik)%data(1:total_s)
                call move_alloc(newoff, dst(ik)%offsets)
                call move_alloc(newdata, dst(ik)%data)
            case default
                allocate(newints(max(n, 1_int64)))
                newints = 0_int64
                if (nd > 0_int64) newints(1:nd) = dst(ik)%ints(1:nd)
                if (ns > 0_int64) newints(nd + 1_int64:n) = src(ik)%ints(1:ns)
                call move_alloc(newints, dst(ik)%ints)
            end select
            ! Materialized only when at least one side has nulls, so a null-free append stays on
            ! the engine's no-nulls fast path. An absent half is all-valid, which is exactly what
            ! the unallocated convention means.
            if (allocated(dst(ik)%valid) .or. allocated(src(ik)%valid)) then
                allocate(newvalid(max(n, 1_int64)))
                newvalid = 1_c_int8_t
                if (allocated(dst(ik)%valid) .and. nd > 0_int64) newvalid(1:nd) = dst(ik)%valid(1:nd)
                if (allocated(src(ik)%valid) .and. ns > 0_int64) newvalid(nd + 1_int64:n) = src(ik)%valid(1:ns)
                call move_alloc(newvalid, dst(ik)%valid)
            end if
        end do
    end procedure buf_append
    !
    ! ---- The M3 engine drivers ----
    !
    ! Unlike drive_engine above, these three always go through the builder, even for a single key.
    ! The one-shot entry points exist to skip a copy on the hottest path in the library, and none
    ! of these is it -- run detection, binary search and merging each cost one extra copy of an
    ! already-extracted buffer in exchange for one entry point per operation instead of three.
    !
    module procedure engine_build_runs
        type(c_ptr) :: builder
        integer(int64) :: status, k, nthreads, gek
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(nrows))
        allocate(tie(max(nrows, 1_int64)))
        tie = 0_c_int8_t
        do k = 1_int64, nrows
            perm(k) = k
        end do
        if (nrows < 2_int64) return
        call resolve_thread_count(threads, nrows, nthreads)
        ! Resolved HERE, never in C++: the boundary carries a real count, never a "0 means all"
        ! sentinel, so the C++ side obeys rather than interprets what a prefix of zero would mean.
        gek = int(size(keys), int64)
        if (present(group_ekeys)) gek = int(group_ekeys, int64)
        if (dbg_fortran_engine) then
            call sort_build_runs_permutation(keys, nrows, gek, perm, tie)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build_runs(builder, nthreads, gek, perm, tie)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_build_runs
    !
    module procedure drive_engine_grouped
        integer(c_int8_t), allocatable :: tie(:)
        !
        if (.not. present(group_offsets)) then
            ! Nothing to report, so nothing is given up: this is drive_engine exactly, one-shot
            ! borrow and all. The branch is what keeps asking for boundaries the only thing that
            ! costs anything.
            call drive_engine(keys, nrows, proc, perm, threads=threads)
            return
        end if
        call engine_build_runs(keys, nrows, proc, perm, tie, threads=threads, group_ekeys=group_ekeys)
        call runs_to_offsets(tie, nrows, group_offsets)
    end procedure drive_engine_grouped
    !
    module procedure runs_to_offsets
        integer(int64) :: k, ngroups, pos
        !
        ! Bounded by nrows, NEVER by size(tie): engine_build_runs allocates tie with a
        ! max(nrows, 1) floor, so a zero-row sort leaves one element in it that describes no row
        ! and would otherwise be counted as a group.
        ngroups = 0_int64
        do k = 1_int64, nrows
            if (tie(k) == 0_c_int8_t) ngroups = ngroups + 1_int64
        end do
        allocate(offsets(ngroups + 1_int64))
        pos = 0_int64
        do k = 1_int64, nrows
            if (tie(k) == 0_c_int8_t) then
                pos = pos + 1_int64
                offsets(pos) = k
            end if
        end do
        ! The sentinel. It is what lets group g be perm(o(g) : o(g+1) - 1) for EVERY g including
        ! the last -- and for a zero-row sort it is the array's only entry, so `[1]` means "no
        ! groups" rather than an unallocated result every caller would have to test for.
        offsets(ngroups + 1_int64) = nrows + 1_int64
    end procedure runs_to_offsets
    !
    module procedure engine_search
        type(c_ptr) :: builder
        integer(c_int8_t) :: wflag
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        if (dbg_fortran_engine) then
            pos = sort_search_position(keys, n_search, upper)
            return
        end if
        wflag = merge(1_c_int8_t, 0_c_int8_t, upper)
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        pos = parquet_sort_builder_search(builder, n_search, wflag)
        call parquet_sort_builder_free(builder)
        if (pos < 1_int64) then
            ! The C side answers -1 only for an empty key list, rejected above.
            error stop EP // proc // ": the sort engine had no key to search" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_search
    !
    module procedure engine_merge
        type(c_ptr) :: builder
        integer(int64) :: status, k
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(nrows))
        do k = 1_int64, nrows
            perm(k) = k
        end do
        if (nrows < 2_int64) return
        if (dbg_fortran_engine) then
            call sort_merge_permutation(keys, nrows, na, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_merge(builder, na, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not merge" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_merge
    !
end submodule parquet_sorting_keys ! GCOVR_EXCL_LINE
