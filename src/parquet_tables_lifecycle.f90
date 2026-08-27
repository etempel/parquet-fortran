!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Lifecycle of a `parquet_table`: opening one from a file, starting an empty in-memory one,
!! destroying one, the blocked intrinsic assignment, and the slot/row bookkeeping that adding a
!! column goes through.
!!
!! The one structural rule everything here exists to protect: the column store lives behind the
!! `cache` POINTER, never an allocatable component. That is what lets a read accessor stay
!! `intent(in)`, and what keeps a pointer handed out by `%col` valid without the caller having to
!! declare the table `target` -- the pointer targets heap owned by the cache, not the dummy
!! argument, so F2018 15.5.2.4's "pointer to a dummy's target becomes undefined on return" rule
!! never applies to it. The price is that intrinsic assignment must be blocked (two tables sharing
!! one cache would double-free it), which `table_assign_guard` does.
submodule (parquet_tables) parquet_tables_lifecycle
    implicit none
    !
contains
    !
    module procedure open_table_full
        call open_table_impl(table, filename, .false., 0_int64, 0_int64, maml, filter, sort, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed, list_columns)
    end procedure open_table_full
    !
    ! Both slice specifics pass no `sort` at all -- there is no argument to pass, which is how the
    ! slice regime's "no sorting" rule is enforced (see the parquet_open_table generic's own doc).
    module procedure open_table_slice_i32
        call open_table_impl(table, filename, .true., int(row_lo, int64), int(row_hi, int64), maml, &
            filter, qc=qc, qc_soft=qc_soft, use_threads=use_threads, &
            sample_fraction=sample_fraction, sample_seed=sample_seed, list_columns=list_columns)
    end procedure open_table_slice_i32
    !
    module procedure open_table_slice_i64
        call open_table_impl(table, filename, .true., row_lo, row_hi, maml, &
            filter, qc=qc, qc_soft=qc_soft, use_threads=use_threads, &
            sample_fraction=sample_fraction, sample_seed=sample_seed, list_columns=list_columns)
    end procedure open_table_slice_i64
    !
    !> The one open path: both regimes differ only in which rows the table claims, and both
    !! classify without reading. Shared rather than duplicated so the slice regime cannot drift
    !! from the full one on anything but its row scope.
    subroutine open_table_impl(table, filename, sliced, row_lo, row_hi, maml, filter, sort, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed, list_columns)
        type(parquet_table), intent(out) :: table !! the table to fill.
        character(len=*), intent(in) :: filename  !! parquet file to open.
        logical, intent(in) :: sliced             !! .true. for the slice regime.
        integer(int64), intent(in) :: row_lo      !! first file row (slice regime only).
        integer(int64), intent(in) :: row_hi      !! last file row (slice regime only).
        character(len=*), intent(in), optional :: maml !! read-in (Role-B) MAML describing `filename`.
        type(parquet_filter), intent(in), optional :: filter !! row filter, in INTERNAL column names.
        type(parquet_sortkey), intent(in), optional :: sort !! sort keys, in INTERNAL column names.
        type(parquet_read_qc), intent(in), optional :: qc !! read-time qc, in INTERNAL column names.
        logical, intent(in), optional :: qc_soft  !! warn on a qc violation instead of aborting.
        logical, intent(in), optional :: use_threads !! forwarded to parquet_open_reader.
        real(real64), intent(in), optional :: sample_fraction !! keep each row with this probability.
        integer(int64), intent(in), optional :: sample_seed !! seed for that draw; `42_int64`, not `42`.
        character(len=*), intent(in), optional :: list_columns !! `"auto"` (default) or `"container"`.
        integer :: i, j, ncol, n_remap, n_qc, n_units
        integer(int64) :: file_rows
        logical :: masked
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: remap_internal(:), remap_physical(:)
        character(len=:), allocatable :: unit_cols(:), unit_vals(:)
        type(parquet_filter) :: comp_filter
        type(parquet_sortkey) :: comp_sort
        type(parquet_schema) :: comp_qc
        !
        ! `table` is intent(out) on a finalizable type, so table_finalize has already run on any
        ! previous contents by the time we get here, and every component below is reassigned
        ! explicitly -- INCLUDING `detached`, deliberately, even though it also has a default
        ! initializer (`= .false.`) that intent(out) is specified to apply on its own. Confirmed
        ! by direct instrumentation (thread-tagged prints around this exact reopen, gfortran 13/14,
        ! both the actual GitLab CI image and a local from-scratch reproduction): reopening a
        ! `parquet_table` variable that was previously detached (e.g. by a `%sort_by` call) can
        ! come back from this intent(out) reopen with `%detached` still `.true.`, immediately, with
        ! no other component affected and no concurrency involved at all -- `detached` was the ONE
        ! component this procedure never assigned explicitly, unlike `regime`/`row_lo`/`row_hi`/
        ! `row_count`/`cache` below, all of which ARE explicitly reassigned regardless of intent(out)
        ! and were never seen to misbehave. This one-line explicit reset is what actually closed the
        ! bug (see CLAUDE.md); do not remove it on the assumption that intent(out) alone suffices.
        table%detached = .false.
        !
        ! The whole read-time transform is composed FIRST, before the parquet file is opened at
        ! all: the read-in MAML is loaded and its extra: remap:/filter:/sort: parsed, the caller's
        ! internal-name filter/sort/qc are translated to file names through that remap, and each is
        ! merged with its MAML counterpart. Nothing here needs the file (the one remap rule that
        ! does -- "the column exists" -- stays in validate_remap, below), and doing it first is what
        ! lets the full regime hand everything to parquet_open_reader as constructor arguments,
        ! and means a malformed MAML aborts with no reader -- and so no live Arrow object -- in
        ! scope. See compose_read_transform (parquet_tables_maml) for the composition rules.
        call compose_read_transform(sliced, maml, filter, sort, qc, remap_internal, remap_physical, &
            n_remap, comp_filter, comp_sort, comp_qc, n_qc, unit_cols, unit_vals, n_units)
        !
        allocate(table%cache)
        call record_open_thread(table%cache)
        ! A lock is a HANDLE, not a value: a clone must build its own rather than copy
        ! the source's, and every cache must have one before any thread can reach it.
        call table_init_lock(table%cache)
        table%cache%file_backed = .true.
        table%cache%source_file = trim(filename)
        ! Validated and stored BEFORE the reader opens, so a mistyped token aborts with no live
        ! Arrow object in scope -- the same reason the read-time transform is composed first.
        table%cache%list_columns = resolve_list_columns_token(list_columns, filename)
        ! Retained only so %clone can reattach the SAME transform when it reopens the file. Stored
        ! as the composed, already-translated values rather than the caller's originals: a clone
        ! opens the same file, so re-deriving them would only risk the two drifting. They live on
        ! the CACHE, not on `table` -- see parquet_table_cache's own comment for why that placement
        ! is load-bearing rather than incidental.
        table%cache%read_qc_soft = .false.
        if (present(qc_soft)) table%cache%read_qc_soft = qc_soft
        if (comp_filter%n > 0) table%cache%read_filter = comp_filter
        if (comp_sort%n > 0) table%cache%read_sort = comp_sort
        if (n_qc > 0) table%cache%read_qc_schema = comp_qc
        if (present(sample_fraction)) table%cache%read_sample_fraction = sample_fraction
        if (present(sample_seed)) table%cache%read_sample_seed = sample_seed
        ! THE SEED IS SETTLED HERE, BEFORE ANY READER EXISTS -- see parquet_table_cache's own note
        ! on read_sample_seed. An unseeded sample_fraction= used to leave every reader this table
        ! opens to draw its own subset, so a %clone (whose reader is reopened from these same
        ! fields) held DIFFERENT rows from its source. That surfaces as an abort only when the two
        ! draws happen to keep a different number of rows; when the counts coincide it is a silent
        ! wrong answer, since parquet_check_read_row_count compares counts and not membership.
        !
        ! Settled with pf_random_seed rather than with the intrinsic RANDOM_NUMBER because
        ! parquet_open_table is reachable from several threads at once (the documented
        ! per-thread-slice shape) and gfortran's own RNG state is not thread-safe. pf_random_seed
        ! folds a process-wide counter, incremented by an atomic capture, into the clock, so two
        ! threads settling a seed in the same tick still get different ones.
        !
        ! Unconditional whenever a fraction was given, including for a fraction the reader will
        ! install no draw for (>= 1.0) and one it will reject (negative/NaN): a seed is inert in
        ! both cases, and the invariant is worth more as one sentence with no exceptions than as a
        ! saved entropy draw. `sample_seed <= 0` is parquet_open_reader's own spelling of "draw a
        ! fresh one", so it counts as unseeded here too.
        if (present(sample_fraction)) then
            if (.not. allocated(table%cache%read_sample_seed)) then
                table%cache%read_sample_seed = pf_random_seed()
            else if (table%cache%read_sample_seed <= 0_int64) then
                table%cache%read_sample_seed = pf_random_seed()
            end if
        end if
        if (sliced) then
            table%cache%slice_row_lo = row_lo
            table%cache%slice_row_hi = row_hi
        end if
        !
        ! Which of the two slice paths this is (see parquet_table_cache's own note on the two
        ! coordinate systems). The fast path is the common one and stays exactly what it was: no
        ! mask, physical bounds, the slice trimmed out of the covering row groups in memory.
        masked = table_slice_is_masked(table%cache)
        if (masked) then
            ! The file's own row-group geometry, which this table's reader will not be able to
            ! answer for once it opens: a sample_fraction= draw installs itself at open, and
            ! parquet_get_chunk_size then reports survivors. Hence its own footer-only reader --
            ! the same cheap call a caller makes to plan a slice in the first place. Only this
            ! path pays for it.
            call parquet_table_row_group_bounds(filename, table%cache%rg_bounds_physical)
            call check_slice_range(row_lo, row_hi, &
                table%cache%rg_bounds_physical(2, size(table%cache%rg_bounds_physical, 2, kind=int64)), &
                filename)
        end if
        !
        allocate(table%cache%reader)
        call table_open_reader_with_transform(table%cache, trim(filename), use_threads=use_threads)
        call parquet_get_nrows(table%cache%reader, file_rows)
        if (sliced) then
            table%regime = REGIME_SLICE
            if (masked) then
                ! The reader was handed the slice's own row range as part of its filter, so what it
                ! reports IS the slice: `file_rows` here is the number of rows of [row_lo, row_hi]
                ! that survived, and the table counts those from 1. Nothing downstream needs to
                ! know -- rg_bounds (built by the open helper) counts survivors too, so
                ! materialize_slice's arithmetic lands on exactly the same rows it always did.
                table%row_lo = 1_int64
                table%row_hi = file_rows
                table%row_count = file_rows
            else
                ! Validated only now, because on this path the reader is the cheapest thing that
                ! knows how many rows the file has.
                call check_slice_range(row_lo, row_hi, file_rows, filename)
                table%row_lo = row_lo
                table%row_hi = row_hi
                table%row_count = row_hi - row_lo + 1_int64
                ! Every column's read walks these, so they are worked out once here rather than
                ! per column.
                call reader_row_group_bounds(table%cache%reader, table%cache%rg_bounds)
            end if
        else
            table%regime = REGIME_FULL
            table%row_lo = 1
            table%row_hi = file_rows
            table%row_count = file_rows
        end if
        !
        ! The file's own key/value metadata, taken now rather than asked for later: the reader is
        ! released by the first row mutation, and the file's metadata does not stop being true
        ! when that happens. Costs a copy of what parquet_open_reader already holds in memory.
        call parquet_get_metadata_items(table%cache%reader, table%cache%meta_keys, table%cache%meta_values)
        !
        ! Physical row geometry, captured now because it stops being askable later: a filtered
        ! reader reports survivors, and a detached table has no reader at all.
        call capture_row_geometry(table, sliced, row_lo, row_hi, file_rows)
        !
        call parquet_get_column_names(table%cache%reader, names)
        call table_enumerate_columns(table%cache, names, remap_internal, remap_physical, n_remap, filename)
        call drop_shadowed_row_index(table%cache, filename)
        ncol = table%cache%ncols
        !
        ! Classify everything up front (schema only, no column data), so %kind/%width/%nrows
        ! answer for every column while none of them is resident.
        do i = 1, ncol
            call table_classify(table%cache, i)
        end do
        ! Units come from the read-in MAML, matched on the FILE name: a read-in MAML describes the
        ! physical file, so `extra: remap:` gives a column a table-facing name without changing
        ! what the MAML calls it. Stored on the descriptor, so %unit answers for a column nothing
        ! has read; table_materialize copies it onto the values when they arrive. The parquet file
        ! itself carries no unit for a column, so this is the only source there is.
        do i = 1, ncol
            do j = 1, n_units
                if (trim(unit_cols(j)) == table%cache%cols(i)%file_name) then
                    table%cache%cols(i)%unit = trim(unit_vals(j))
                    exit
                end if
            end do
        end do
        ! ...then drop anything classification itself had to decode. Only one case can: a
        ! foreign plain LIST column, whose per-row width has no schema-level answer, so the
        ! reader measures it from the data (see table_classify). Releasing a name nothing
        ! decoded is a no-op, which is what makes this sweep cheap enough to do unconditionally.
        do i = 1, ncol
            call table_release_one(table%cache, table%cache%cols(i)%file_name)
        end do
    end subroutine open_table_impl
    !
    !> Validates `parquet_open_table`'s `list_columns=` and returns the token to store, defaulting
    !! to `"auto"` when the argument is absent.
    !!
    !! A token rather than a logical because a third policy is foreseeable, and an unrecognised one
    !! aborts naming BOTH accepted values -- a caller who mistyped `"containers"` needs to be told
    !! what was expected, not merely that this was not it.
    function resolve_list_columns_token(list_columns, filename) result(token)
        character(len=*), intent(in), optional :: list_columns !! the caller's argument, if given.
        character(len=*), intent(in) :: filename !! for the error message's file context.
        character(len=:), allocatable :: token !! `"auto"` or `"container"`.
        !
        token = "auto"
        if (.not. present(list_columns)) return
        token = trim(adjustl(list_columns))
        if (token == "auto" .or. token == "container") return
        error stop "parquet_open_table: unrecognized list_columns value '" // token // &
            "'; expected 'auto' or 'container' (file '" // trim(filename) // "')"
    end function resolve_list_columns_token
    !
    !> Removes a file column that would occupy the reserved `parquet_row_index` name, with a
    !! warning.
    !!
    !! The reserved name belongs to the automatic row-index column, so a file column that would
    !! answer to it is unreachable -- and leaving it in place would be worse than removing it,
    !! because `%get("parquet_row_index")` would then quietly return the file's values where the
    !! caller expected row numbers. A read-in MAML's `extra: remap:` is the way to reach such a
    !! column: it is matched on the INTERNAL name, so a remapped one has already been renamed by
    !! the time this runs and is left alone.
    !!
    !! A warning rather than an abort, because the caller may not own the file -- the same choice
    !! `parquet_get_metadata` makes for a missing key.
    subroutine drop_shadowed_row_index(cache, filename)
        type(parquet_table_cache), intent(inout) :: cache !! the column store, freshly enumerated.
        character(len=*), intent(in) :: filename          !! the file, for the warning.
        integer :: i, k
        !
        do i = 1, cache%ncols
            if (cache%cols(i)%name /= PARQUET_ROW_INDEX) cycle
            cache%row_index_shadowed = .true.
            call parquet_emit_warning("parquet_open_table: this file has a column called '" // &
                PARQUET_ROW_INDEX // "', which is the reserved name of the automatic row-index " // &
                "column; the file's own column is unreachable unless a read-in MAML remaps it " // &
                "(file '" // trim(filename) // "')")
            do k = i, cache%ncols - 1
                call move_table_column(cache%cols(k), cache%cols(k + 1))
            end do
            cache%ncols = cache%ncols - 1
            call cache_name_index_rebuild(cache)
            return
        end do
    end subroutine drop_shadowed_row_index
    !
    !> Records how many physical rows this table was cut from, and how many its row groups hold.
    !!
    !! Both are answered from what the open has already established rather than by asking the
    !! reader again, which is the point: once a filter or a sample is attached the reader counts
    !! survivors, so the same question put to it later returns `%nrows()`. A whole-file table with
    !! a narrowing transform is the one case with nothing to hand, and it takes the file's own
    !! footer -- the same cheap footer-only read `%row_group_bounds(physical=.true.)` makes.
    subroutine capture_row_geometry(table, sliced, row_lo, row_hi, file_rows)
        type(parquet_table), intent(inout) :: table !! the table being opened.
        logical, intent(in) :: sliced               !! .true. for a slice-regime open.
        integer(int64), intent(in) :: row_lo        !! slice's first FILE row.
        integer(int64), intent(in) :: row_hi        !! slice's last FILE row.
        integer(int64), intent(in) :: file_rows     !! what the reader reported at open.
        integer(int64), allocatable :: bounds(:,:)
        integer(int64) :: rg, rg_lo, rg_hi
        !
        if (sliced) then
            table%cache%unfiltered_rows = row_hi - row_lo + 1_int64
            ! The covering row groups, in the file's own numbering. rg_bounds is already physical
            ! on the unmasked path; the masked one captured rg_bounds_physical before the mask
            ! went on, for exactly this reason.
            if (allocated(table%cache%rg_bounds_physical)) then
                bounds = table%cache%rg_bounds_physical
            else
                bounds = table%cache%rg_bounds
            end if
            call rg_covering_range(bounds, row_lo, row_hi, rg_lo, rg_hi)
            table%cache%rg_extent_rows = 0_int64
            do rg = rg_lo, rg_hi
                if (rg < 1_int64) cycle
                table%cache%rg_extent_rows = table%cache%rg_extent_rows + &
                    bounds(2, rg) - bounds(1, rg) + 1_int64
            end do
            return
        end if
        ! Whole file: every row group is covered, so the two answers coincide -- but `file_rows`
        ! is only the file's own count when nothing narrowed it.
        if (table_transform_narrows(table%cache)) then
            call parquet_table_row_group_bounds(table%cache%source_file, bounds)
            table%cache%unfiltered_rows = bounds(2, size(bounds, 2, kind=int64))
        else
            table%cache%unfiltered_rows = file_rows
        end if
        table%cache%rg_extent_rows = table%cache%unfiltered_rows
    end subroutine capture_row_geometry
    !
    !> error stops unless `[row_lo, row_hi]` is a non-empty range inside `1..file_rows`.
    !!
    !! Raised before the table is usable either way, so a bad slice fails while the table is still
    !! obviously unusable rather than half-built. Shared by the two slice paths, which learn the
    !! file's row count at different moments but must reject the same ranges with the same words.
    subroutine check_slice_range(row_lo, row_hi, file_rows, filename)
        integer(int64), intent(in) :: row_lo     !! first file row asked for.
        integer(int64), intent(in) :: row_hi     !! last file row asked for.
        integer(int64), intent(in) :: file_rows  !! rows the file physically has.
        character(len=*), intent(in) :: filename !! the file, for the message.
        character(len=32) :: lo_s, hi_s, n_s
        !
        if (row_lo >= 1_int64 .and. row_hi <= file_rows .and. row_lo <= row_hi) return
        write(lo_s, "(I0)") row_lo
        write(hi_s, "(I0)") row_hi
        write(n_s, "(I0)") file_rows
        error stop EP // "parquet_open_table: row slice [" // trim(lo_s) // ", " // &
            trim(hi_s) // "] is not inside this file's 1.." // trim(n_s) // " rows " // &
            "(file '" // trim(filename) // "')"
    end subroutine check_slice_range
    !
    !> .true. when this table is a slice whose row range has to be carried in the reader's mask.
    !!
    !! The condition is the presence of a row-narrowing transform, and nothing else -- in
    !! particular a slice that cuts through the middle of a row group is NOT a reason to mask.
    !! Trimming such a slice in memory is what `materialize_slice` already does, with no reader
    !! involvement at all; masking it instead would replace a working sub-range copy with a bitmap
    !! plus an Arrow filter pass per column, for nothing.
    !!
    logical function table_slice_is_masked(cache) result(masked)
        type(parquet_table_cache), intent(in) :: cache !! the table's store, with its transform stored.
        !
        masked = cache%slice_row_lo > 0_int64 .and. table_transform_narrows(cache)
    end function table_slice_is_masked
    !
    module procedure table_transform_narrows
        ! `sample_fraction >= 1` keeps every row, and parquet_open_reader installs no draw for it,
        ! so it is not narrowing either. A negative or NaN fraction is not judged here at all: it
        ! reaches parquet_open_reader, which rejects it with the message that names the argument.
        narrows = .false.
        if (allocated(cache%read_filter)) then
            narrows = .true.
        else if (allocated(cache%read_sample_fraction)) then
            narrows = cache%read_sample_fraction < 1.0_real64
        end if
    end procedure table_transform_narrows
    !
    module procedure rg_covering_range
        integer(int64) :: rg
        !
        rg_lo = 0_int64
        rg_hi = 0_int64
        do rg = 1_int64, size(bounds, 2, kind=int64)
            ! An empty row group (bounds(1) > bounds(2), which is how a row group contributing
            ! nothing is spelled) fails both tests and is stepped over.
            if (bounds(2, rg) < row_lo .or. bounds(1, rg) > row_hi) cycle
            if (rg_lo == 0_int64) rg_lo = rg
            rg_hi = rg
        end do
    end procedure rg_covering_range
    !
    module procedure table_open_reader_with_transform
        type(parquet_filter), allocatable :: pass_filter
        type(parquet_sortkey), allocatable :: pass_sort
        type(parquet_schema), allocatable :: pass_schema
        logical, allocatable :: pass_qc_soft
        logical :: masked
        integer(int64) :: rg_lo, rg_hi
        ! Which reader this call is opening, resolved once so that every statement below drives the
        ! same one. A pointer rather than two written-out arms (the shape `table_materialize` uses
        ! for the same choice): there are three reader statements here, not one, and three pairs of
        ! near-identical arms is exactly the drift this helper exists to remove. Both dummies carry
        ! `target` so the association is standard-conforming; neither pointer outlives the call.
        type(parquet_reader), pointer :: r
        !
        if (present(rdr)) then
            r => rdr
        else
            r => cache%reader
        end if
        masked = table_slice_is_masked(cache)
        ! qc_soft only ever matters when a qc schema is actually attached, and passing it on its
        ! own would be a no-op the reader still has to reason about -- so it travels with the
        ! schema or not at all.
        if (allocated(cache%read_qc_schema)) then
            pass_schema = cache%read_qc_schema
            pass_qc_soft = cache%read_qc_soft
        end if
        !
        ! AN ADDITIONAL READER ADOPTS THE TABLE'S TRANSFORM RATHER THAN REDERIVING IT. The filter
        ! mask and the sort permutation are immutable Arrow arrays the table's own reader has
        ! already built, so handing them over costs two atomic refcount increments -- where
        ! rebuilding them means re-decoding the filter's key columns and re-running the whole sort,
        ! per reader. That difference is what lets the parallel read paths accept a filtered or
        ! sorted table at all; before it, both were refused on measured cost.
        !
        ! The sample rides along inside the same mask, so it is not passed either: a reader that
        ! adopts cannot draw a different subset, because it never draws. And a masked slice needs no
        ! second `parquet_reader_set_filter` call, because the row range is already part of the mask
        ! being adopted. qc is the one thing that must still be installed per reader -- its rules are
        ! per reader but its CHECKS run per column read, and each column is read by exactly one
        ! thread, so nothing is checked or warned twice.
        if (present(rdr)) then
            call parquet_open_reader(r, filename, schema=pass_schema, qc_soft=pass_qc_soft, &
                use_threads=use_threads)
            call parquet_reader_adopt_transform(r, cache%reader)
            return
        end if
        !
        ! On the masked path the filter is attached after the open instead, because only
        ! parquet_reader_set_filter can carry the slice's row range with it -- see below.
        if (allocated(cache%read_filter) .and. .not. masked) pass_filter = cache%read_filter
        if (allocated(cache%read_sort)) pass_sort = cache%read_sort
        call parquet_open_reader(r, filename, filter=pass_filter, &
            sort_by=pass_sort, schema=pass_schema, qc_soft=pass_qc_soft, use_threads=use_threads, &
            sample_fraction=cache%read_sample_fraction, &
            sample_seed=cache%read_sample_seed)
        if (.not. masked) return
        !
        ! The slice's own row range becomes part of the reader's mask, so that everything the
        ! reader hands back afterwards -- row counts, per-row-group chunk sizes, the chunks
        ! themselves -- is already restricted to [slice_row_lo, slice_row_hi] and the table can
        ! work in one coordinate system instead of two. Without this the reader would return the
        ! covering row groups' survivors in full, and there is no way back from a filtered chunk to
        ! "which of these rows were inside the slice": that needs the per-row mask, which is not
        ! exposed. The row-GROUP range scopes the evaluation itself, so no row group outside the
        ! slice is even read; the row range is the finer cut inside them.
        !
        ! An unallocated read_filter here is the sample-only case, and the empty local below is
        ! deliberate rather than a missing branch: a rule-less filter carrying a row range installs
        ! an all-true-within-range mask, which is exactly what that case needs, and folds the
        ! already-installed sample draw into it.
        ! rg_covering_range answers 0/0 when the slice covers no row group at all (every candidate
        ! row group empty), and 0 means "all row groups" to parquet_reader_set_filter -- so that
        ! degenerate case takes the memory-bounded engine over the whole file instead of the
        ! caching one. Harmless: a slice covering no row group has no rows to read either way, and
        ! the row range below still restricts the mask to exactly [slice_row_lo, slice_row_hi].
        ! Every real slice yields rg_lo >= 1 and is unaffected.
        call rg_covering_range(cache%rg_bounds_physical, cache%slice_row_lo, &
            cache%slice_row_hi, rg_lo, rg_hi)
        block
            type(parquet_filter) :: attach
            if (allocated(cache%read_filter)) attach = cache%read_filter
            call parquet_reader_set_filter(r, attach, rg_lo, rg_hi, &
                cache%slice_row_lo, cache%slice_row_hi)
        end block
        ! Rebuilt from the reader now that it is masked, so these count each row group's SURVIVING
        ! rows within the slice -- the table's own coordinates. Row groups outside the slice
        ! contribute nothing and come back as empty ranges, which materialize_slice's existing
        ! skip test steps over unchanged.
        !
        ! ONLY for the cache's own reader. `rg_bounds` describes that reader, it is already correct
        ! by the time any other reader over the same file is opened, and it is shared state that
        ! several threads would otherwise write at once -- see this procedure's own doc-comment.
        if (.not. present(rdr)) call reader_row_group_bounds(r, cache%rg_bounds)
    end procedure table_open_reader_with_transform
    !
    module procedure parquet_new_table
        ! Explicit, not relied-upon-implicitly, for the same reason as open_table_impl's own
        ! `table%detached = .false.` -- see that assignment's comment.
        table%detached = .false.
        allocate(table%cache)
        call record_open_thread(table%cache)
        ! A lock is a HANDLE, not a value: a clone must build its own rather than copy
        ! the source's, and every cache must have one before any thread can reach it.
        call table_init_lock(table%cache)
        table%cache%file_backed = .false.
        table%cache%source_file = ""
        table%regime = REGIME_FULL
        table%row_count = 0
        table%row_lo = 1
        table%row_hi = 0
        allocate(table%cache%cols(COL_HEADROOM))
        table%cache%ncols = 0
        call cache_name_index_rebuild(table%cache)
    end procedure parquet_new_table
    !
    module procedure reader_row_group_bounds
        integer(int64) :: nrg, rg, rows, next
        !
        ! Built entirely from the footer: the row-group count and each group's own row count.
        ! Row groups are NOT guaranteed uniform, so each is asked rather than the first one
        ! scaled -- assuming uniformity here would misplace every boundary after the first
        ! short group.
        call parquet_get_num_row_groups(reader, nrg)
        allocate(bounds(2, nrg))
        next = 1_int64
        do rg = 1_int64, nrg
            call parquet_get_chunk_size(reader, rows, row_group=rg)
            bounds(1, rg) = next
            bounds(2, rg) = next + rows - 1_int64
            next = next + rows
        end do
    end procedure reader_row_group_bounds
    !
    module procedure parquet_table_row_group_bounds
        type(parquet_reader) :: reader
        !
        ! Opening a reader reads the footer and schema only, so this planning call is cheap
        ! enough to make before deciding anything -- which is the point: a thread cannot open
        ! its slice table until it knows which slice to ask for.
        call parquet_open_reader(reader, trim(filename))
        call reader_row_group_bounds(reader, bounds)
        call parquet_close_reader(reader)
    end procedure parquet_table_row_group_bounds
    !
    module procedure table_row_group_bounds
        character(len=:), allocatable :: sfx
        logical :: want_physical
        !
        want_physical = .false.
        if (present(physical)) want_physical = physical
        call table_check_open(self, "row_group_bounds")
        ! Checked before the file_backed test below, which detaching also clears: a detached
        ! table needs the reason it cannot answer, not "it was never opened from a file".
        call table_check_not_detached(self%cache, table_scope_of(self), "", "row_group_bounds")
        if (.not. self%cache%file_backed) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "row_group_bounds: this table was not opened from a file, so it " // &
                "has no row groups" // sfx
        end if
        ! A sort is the one transform that leaves NO row-group structure in this table's own
        ! numbering: it reorders rows across the whole file, so row 5 of the table can come from
        ! any row group and no range of table rows belongs to one. The file's own geometry is
        ! untouched by it, though, so physical=.true. still answers -- see below. The refusal is
        ! made here, at the table level, because otherwise it surfaced from
        ! parquet_get_chunk_size's own guard, naming a procedure and a reader the caller never
        ! used. (A sort implies a whole-file table: the slice forms have no `sort` argument, and a
        ! maml= sort list is rejected for a slice at open.)
        if (allocated(self%cache%read_sort) .and. .not. want_physical) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "row_group_bounds: this table was opened with a sort, so a row of " // &
                "it can come from any row group and its rows have no row-group ranges; ask for " // &
                "the file's own row groups instead (physical=.true.)" // sfx
        end if
        ! Four sources, and which one answers depends on both the coordinate system asked for and
        ! how the table was opened. The two coincide for a table that holds every row of the file,
        ! which is why an unsliced, unfiltered table takes the same branch either way.
        if (want_physical) then
            if (allocated(self%cache%rg_bounds_physical)) then
                ! Masked slice: the only case where the file's numbering was captured separately,
                ! because the reader stopped being able to answer for it the moment it opened.
                bounds = self%cache%rg_bounds_physical
            else if (table_transform_narrows(self%cache) .or. allocated(self%cache%read_sort)) then
                ! Whole file with a filter, a sample or a sort: its own reader would answer in
                ! surviving rows (or, under a sort, refuse), so the file's own numbering comes from
                ! a footer-only reader instead. Cheap, and only on this path. physical=.true.
                ! therefore answers for EVERY file-backed table, whatever transform it carries --
                ! which of them happens to be present must not decide whether the question is
                ! answerable.
                call parquet_table_row_group_bounds(self%cache%source_file, bounds)
            else if (allocated(self%cache%rg_bounds)) then
                ! Unmasked slice: its bounds ARE the file's, so there is nothing else to consult.
                bounds = self%cache%rg_bounds
            else
                call reader_row_group_bounds(self%cache%reader, bounds)
            end if
        else if (allocated(self%cache%rg_bounds)) then
            ! Either slice path: rg_bounds is in this table's coordinates by construction.
            bounds = self%cache%rg_bounds
        else
            ! Whole file. The reader answers in the table's coordinates already -- surviving rows
            ! when a transform narrows them, every row when nothing does -- so one call covers
            ! both, and a row group filtered away entirely comes back as the empty range the
            ! cumulative walk produces for a zero-row group.
            call reader_row_group_bounds(self%cache%reader, bounds)
        end if
    end procedure table_row_group_bounds
    !
    module procedure table_assign_guard
        ! Deliberately unconditional. `lhs`/`rhs` exist only to give the assignment the right
        ! shape; neither is ever touched, because there is no correct thing to do with them --
        ! a shallow copy would leave two tables sharing (and double-freeing) one cache, and a
        ! deep copy is %clone's job, which arrives with the mutation milestone.
        error stop EP // "assignment is not supported (it would leave two tables sharing one " // &
            "column store); use call a%clone(b) to copy a table"
    end procedure table_assign_guard
    !
    !
    module procedure table_new_slot
        integer :: existing, n, i
        logical :: grew
        type(parquet_table_column), allocatable :: bigger(:)
        character(len=:), allocatable :: sfx
        type(parquet_table_cache), pointer :: cache
        !
        ! Adding a column reallocates cols(:), so every descriptor another thread may be holding
        ! moves. This is the choke point every %add_column specific goes through, which is why the
        ! guard is here rather than repeated across the generated per-kind entry points.
        call table_check_not_shared(self, "add_column")
        ! ifx (unlike gfortran) refuses MOVE_ALLOC's TO argument when it is reached through an
        ! intent(in) dummy, even though self%cache%cols is definable (self%cache is a POINTER
        ! component, so its target is a distinct entity from self -- see table_new_slot's own
        ! doc-comment on parquet_tables.f90). A local pointer alias sidesteps ifx's stricter check
        ! without weakening self's own intent(in).
        cache => self%cache
        existing = table_find(self, name)
        if (existing > 0) then
            if (.not. present(force)) then
                call table_context_suffix(self%cache, name, sfx)
                error stop EP // "add_column: a column of this name already exists; pass " // &
                    "force=.true. to replace it" // sfx
            else if (.not. force) then
                call table_context_suffix(self%cache, name, sfx)
                error stop EP // "add_column: a column of this name already exists; pass " // &
                    "force=.true. to replace it" // sfx
            end if
            ! Replacing in place keeps every other slot's index stable, so pointers into other
            ! columns survive -- the broad contract still says they may not, but there is no
            ! reason to invalidate them here.
            call cache%cols(existing)%values%clear()
            cache%cols(existing)%residency = RES_EMPTY
            cache%cols(existing)%file_source = .false.
            cache%cols(existing)%supported = .true.
            cache%generation = cache%generation + 1_int64
            idx = existing
            return
        end if
        !
        n = cache%ncols
        grew = n >= size(cache%cols)
        if (grew) then
            ! Growth doubles rather than adding one, so a long add_column loop is not quadratic.
            ! This DOES relocate every descriptor, which is why it -- and only it -- bumps the
            ! generation below.
            allocate(bigger(max(2 * size(cache%cols, kind=int64), int(n, int64) + 1_int64)))
            ! Moved, not assigned: an intrinsic array assignment here would deep-copy every
            ! column's storage into the new array and then free the old one, so growing the slot
            ! array would cost a full copy of everything the table holds.
            do i = 1, n
                call move_table_column(bigger(i), cache%cols(i))
            end do
            call move_alloc(bigger, cache%cols)
        end if
        n = n + 1
        cache%ncols = n
        ! Every path that adds, replaces or relocates a slot comes through here, so this is the
        ! one place a column-structural change decides whether to bump the generation.
        !
        ! CONDITIONAL, and that is %reserve_columns' whole guarantee: an add that fits in the
        ! capacity already allocated moves no descriptor, reallocates no storage and renumbers no
        ! slot, so every outstanding %col pointer, column handle and row handle is still exactly
        ! as correct as it was -- and bumping would force a needless re-fetch on the commonest
        ! derived-column idiom there is. An add that GREW the array relocated every descriptor and
        ! must bump.
        !
        ! The test is deliberately one predicate in one place rather than a decision each branch
        ! makes for itself: the counter's rule is otherwise a total, exception-free one
        ! (feature_risks.md Risk-71), and a single computed exception is checkable where a
        ! scattered one is not. The replace-in-place branch above bumps on its way out for a
        ! different reason -- it CLEARS a column's values, which no held pointer can survive.
        if (grew) cache%generation = cache%generation + 1_int64
        cache%cols(n)%name = trim(name)
        cache%cols(n)%file_name = trim(name)
        cache%cols(n)%file_source = .false.
        cache%cols(n)%supported = .true.
        cache%cols(n)%residency = RES_EMPTY
        ! Eagerly, here rather than in cache_find: the lookup takes the cache intent(in) so that
        ! concurrent readers need no atomics, and rebuilding inside it would break that.
        call cache_name_index_insert(cache, n)
        idx = n
    end procedure table_new_slot
    !
    module procedure table_fix_nrows
        character(len=:), allocatable :: sfx
        character(len=32) :: got, want
        !
        if (self%cache%ncols == 0 .and. self%row_count == 0) then
            self%row_count = n
            self%row_lo = 1
            self%row_hi = n
            return
        end if
        if (n /= self%row_count) then
            call table_context_suffix(self%cache, name, sfx)
            write(got, "(I0)") n
            write(want, "(I0)") self%row_count
            error stop EP // "add_column: every column must have the same number of rows (got " // &
                trim(got) // ", table has " // trim(want) // ")" // sfx
        end if
    end procedure table_fix_nrows
    !
end submodule parquet_tables_lifecycle
