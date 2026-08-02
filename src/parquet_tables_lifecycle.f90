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
            qc_soft, use_threads, sample_fraction, sample_seed)
    end procedure open_table_full
    !
    ! Both slice specifics pass no `sort` at all -- there is no argument to pass, which is how the
    ! slice regime's "no sorting" rule is enforced (see the parquet_open_table generic's own doc).
    module procedure open_table_slice_i32
        call open_table_impl(table, filename, .true., int(row_lo, int64), int(row_hi, int64), maml, &
            filter, qc=qc, qc_soft=qc_soft, use_threads=use_threads, &
            sample_fraction=sample_fraction, sample_seed=sample_seed)
    end procedure open_table_slice_i32
    !
    module procedure open_table_slice_i64
        call open_table_impl(table, filename, .true., row_lo, row_hi, maml, &
            filter, qc=qc, qc_soft=qc_soft, use_threads=use_threads, &
            sample_fraction=sample_fraction, sample_seed=sample_seed)
    end procedure open_table_slice_i64
    !
    !> The one open path: both regimes differ only in which rows the table claims, and both
    !! classify without reading. Shared rather than duplicated so the slice regime cannot drift
    !! from the full one on anything but its row scope.
    subroutine open_table_impl(table, filename, sliced, row_lo, row_hi, maml, filter, sort, qc, &
            qc_soft, use_threads, sample_fraction, sample_seed)
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
        integer(int32), intent(in), optional :: sample_seed !! seed for that draw.
        integer :: i, ncol, n_remap, n_qc
        integer(int64) :: file_rows
        logical :: masked
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: remap_internal(:), remap_physical(:)
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
            n_remap, comp_filter, comp_sort, comp_qc, n_qc)
        !
        allocate(table%cache)
        call record_open_thread(table%cache)
        table%cache%file_backed = .true.
        table%cache%source_file = trim(filename)
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
        call table_open_reader_with_transform(table, trim(filename), use_threads)
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
        call parquet_get_column_names(table%cache%reader, names)
        call table_enumerate_columns(table%cache, names, remap_internal, remap_physical, n_remap, filename)
        ncol = table%cache%ncols
        !
        ! Classify everything up front (schema only, no column data), so %kind/%width/%nrows
        ! answer for every column while none of them is resident.
        do i = 1, ncol
            call table_classify(table%cache, i)
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
    !> .true. when this table's read-time transform removes rows, so that the reader's own row
    !! numbering is the surviving rows rather than the file's.
    !!
    !! A sort does not count: it reorders rows without removing any. `sample_fraction >= 1` keeps
    !! every row, and `parquet_open_reader` installs no draw for it, so it is not narrowing either.
    !! A negative or NaN fraction is not judged here at all: it reaches `parquet_open_reader`,
    !! which rejects it with the message that names the argument.
    logical function table_transform_narrows(cache) result(narrows)
        type(parquet_table_cache), intent(in) :: cache !! the table's store, with its transform stored.
        !
        narrows = .false.
        if (allocated(cache%read_filter)) then
            narrows = .true.
        else if (allocated(cache%read_sample_fraction)) then
            narrows = cache%read_sample_fraction < 1.0_real64
        end if
    end function table_transform_narrows
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
        !
        masked = table_slice_is_masked(table%cache)
        ! On the masked path the filter is attached after the open instead, because only
        ! parquet_reader_set_filter can carry the slice's row range with it -- see below.
        if (allocated(table%cache%read_filter) .and. .not. masked) pass_filter = table%cache%read_filter
        if (allocated(table%cache%read_sort)) pass_sort = table%cache%read_sort
        ! qc_soft only ever matters when a qc schema is actually attached, and passing it on its
        ! own would be a no-op the reader still has to reason about -- so it travels with the
        ! schema or not at all.
        if (allocated(table%cache%read_qc_schema)) then
            pass_schema = table%cache%read_qc_schema
            pass_qc_soft = table%cache%read_qc_soft
        end if
        call parquet_open_reader(table%cache%reader, filename, filter=pass_filter, &
            sort_by=pass_sort, schema=pass_schema, qc_soft=pass_qc_soft, use_threads=use_threads, &
            sample_fraction=table%cache%read_sample_fraction, &
            sample_seed=table%cache%read_sample_seed)
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
        call rg_covering_range(table%cache%rg_bounds_physical, table%cache%slice_row_lo, &
            table%cache%slice_row_hi, rg_lo, rg_hi)
        block
            type(parquet_filter) :: attach
            if (allocated(table%cache%read_filter)) attach = table%cache%read_filter
            call parquet_reader_set_filter(table%cache%reader, attach, rg_lo, rg_hi, &
                table%cache%slice_row_lo, table%cache%slice_row_hi)
        end block
        ! Rebuilt from the reader now that it is masked, so these count each row group's SURVIVING
        ! rows within the slice -- the table's own coordinates. Row groups outside the slice
        ! contribute nothing and come back as empty ranges, which materialize_slice's existing
        ! skip test steps over unchanged.
        call reader_row_group_bounds(table%cache%reader, table%cache%rg_bounds)
    end procedure table_open_reader_with_transform
    !
    module procedure parquet_new_table
        ! Explicit, not relied-upon-implicitly, for the same reason as open_table_impl's own
        ! `table%detached = .false.` -- see that assignment's comment.
        table%detached = .false.
        allocate(table%cache)
        call record_open_thread(table%cache)
        table%cache%file_backed = .false.
        table%cache%source_file = ""
        table%regime = REGIME_FULL
        table%row_count = 0
        table%row_lo = 1
        table%row_hi = 0
        allocate(table%cache%cols(COL_HEADROOM))
        table%cache%ncols = 0
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
        ! Four sources, and which one answers depends on both the coordinate system asked for and
        ! how the table was opened. The two coincide for a table that holds every row of the file,
        ! which is why an unsliced, unfiltered table takes the same branch either way.
        if (want_physical) then
            if (allocated(self%cache%rg_bounds_physical)) then
                ! Masked slice: the only case where the file's numbering was captured separately,
                ! because the reader stopped being able to answer for it the moment it opened.
                bounds = self%cache%rg_bounds_physical
            else if (table_transform_narrows(self%cache)) then
                ! Whole file with a filter or a sample: its reader would answer in surviving rows,
                ! so the file's own numbering comes from a footer-only reader instead. Cheap, and
                ! only on this path.
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
    module procedure table_finalize
        ! An implicit finalizer runs at unpredictable points -- scope exit, an intent(out)
        ! reopen, an early return -- with no caller able to see or handle a failure, so it must
        ! always succeed silently and validate nothing (CLAUDE.md). Deallocating the cache runs
        ! parquet_reader's own finalizer on the reader, which abandons rather than closes it.
        if (associated(self%cache)) then
            deallocate(self%cache)
            nullify(self%cache)
        end if
    end procedure table_finalize
    !
    module procedure table_new_slot
        integer :: existing, n
        type(parquet_table_column), allocatable :: bigger(:)
        character(len=:), allocatable :: sfx
        !
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
            call self%cache%cols(existing)%values%clear()
            self%cache%cols(existing)%residency = RES_EMPTY
            self%cache%cols(existing)%file_source = .false.
            self%cache%cols(existing)%supported = .true.
            idx = existing
            return
        end if
        !
        n = self%cache%ncols
        if (n >= size(self%cache%cols)) then
            ! Growth doubles rather than adding one, so a long add_column loop is not quadratic.
            ! This DOES relocate every descriptor, which is exactly why the documented rule is
            ! that any column-structural mutation invalidates every outstanding pointer.
            allocate(bigger(max(2 * size(self%cache%cols), n + 1)))
            bigger(1:n) = self%cache%cols(1:n)
            call move_alloc(bigger, self%cache%cols)
        end if
        n = n + 1
        self%cache%ncols = n
        self%cache%cols(n)%name = trim(name)
        self%cache%cols(n)%file_name = trim(name)
        self%cache%cols(n)%file_source = .false.
        self%cache%cols(n)%supported = .true.
        self%cache%cols(n)%residency = RES_EMPTY
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
