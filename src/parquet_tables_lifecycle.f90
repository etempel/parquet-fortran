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
    module procedure open_table_slice_i32
        call open_table_impl(table, filename, .true., int(row_lo, int64), int(row_hi, int64), maml)
    end procedure open_table_slice_i32
    !
    module procedure open_table_slice_i64
        call open_table_impl(table, filename, .true., row_lo, row_hi, maml)
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
        character(len=:), allocatable :: names(:)
        character(len=:), allocatable :: remap_internal(:), remap_physical(:)
        type(parquet_filter) :: comp_filter
        type(parquet_sortkey) :: comp_sort
        type(parquet_schema) :: comp_qc
        character(len=32) :: lo_s, hi_s, n_s
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
        call compose_read_transform(maml, filter, sort, qc, remap_internal, remap_physical, &
            n_remap, comp_filter, comp_sort, comp_qc, n_qc)
        ! Retained only so %clone can reattach the SAME transform when it reopens the file. Stored
        ! as the composed, already-translated values rather than the caller's originals: a clone
        ! opens the same file, so re-deriving them would only risk the two drifting.
        table%read_qc_soft = .false.
        if (present(qc_soft)) table%read_qc_soft = qc_soft
        if (comp_filter%n > 0) table%read_filter = comp_filter
        if (comp_sort%n > 0) table%read_sort = comp_sort
        if (n_qc > 0) table%read_qc_schema = comp_qc
        if (present(sample_fraction)) table%read_sample_fraction = sample_fraction
        if (present(sample_seed)) table%read_sample_seed = sample_seed
        !
        allocate(table%cache)
        call record_open_thread(table%cache)
        table%cache%file_backed = .true.
        table%cache%source_file = trim(filename)
        !
        allocate(table%cache%reader)
        call table_open_reader_with_transform(table, trim(filename), use_threads)
        call parquet_get_nrows(table%cache%reader, file_rows)
        if (sliced) then
            ! Validated before anything else is set up, so a bad slice fails while the table is
            ! still obviously unusable rather than half-built.
            if (row_lo < 1_int64 .or. row_hi > file_rows .or. row_lo > row_hi) then
                write(lo_s, "(I0)") row_lo
                write(hi_s, "(I0)") row_hi
                write(n_s, "(I0)") file_rows
                error stop EP // "parquet_open_table: row slice [" // trim(lo_s) // ", " // &
                    trim(hi_s) // "] is not inside this file's 1.." // trim(n_s) // " rows " // &
                    "(file '" // trim(filename) // "')"
            end if
            table%regime = REGIME_SLICE
            table%row_lo = row_lo
            table%row_hi = row_hi
            table%row_count = row_hi - row_lo + 1_int64
            ! Every column's read walks these, so they are worked out once here rather than
            ! per column.
            call reader_row_group_bounds(table%cache%reader, table%cache%rg_bounds)
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
    module procedure table_open_reader_with_transform
        type(parquet_filter), allocatable :: pass_filter
        type(parquet_sortkey), allocatable :: pass_sort
        type(parquet_schema), allocatable :: pass_schema
        logical, allocatable :: pass_qc_soft
        !
        if (allocated(table%read_filter)) pass_filter = table%read_filter
        if (allocated(table%read_sort)) pass_sort = table%read_sort
        ! qc_soft only ever matters when a qc schema is actually attached, and passing it on its
        ! own would be a no-op the reader still has to reason about -- so it travels with the
        ! schema or not at all.
        if (allocated(table%read_qc_schema)) then
            pass_schema = table%read_qc_schema
            pass_qc_soft = table%read_qc_soft
        end if
        call parquet_open_reader(table%cache%reader, filename, filter=pass_filter, &
            sort_by=pass_sort, schema=pass_schema, qc_soft=pass_qc_soft, use_threads=use_threads, &
            sample_fraction=table%read_sample_fraction, sample_seed=table%read_sample_seed)
    end procedure table_open_reader_with_transform
    !
    module procedure parquet_new_table
        ! Explicit, not relied-upon-implicitly, for the same reason as open_table_impl's own
        ! `table%detached = .false.` -- see that assignment's comment. `read_qc_soft` gets the same
        ! treatment for the same reason: it is the other scalar component with nothing but its
        ! default initializer behind it.
        table%detached = .false.
        table%read_qc_soft = .false.
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
        !
        call table_check_open(self, "row_group_bounds")
        ! Checked before the file_backed test below, which detaching also clears: a detached
        ! table needs the reason it cannot answer, not "it was never opened from a file".
        call table_check_not_detached(self%cache, table_scope_of(self), "", "row_group_bounds")
        if (.not. self%cache%file_backed) then
            call table_context_suffix(self%cache, "", sfx)
            error stop EP // "row_group_bounds: this table was not opened from a file, so it " // &
                "has no row groups" // sfx
        end if
        if (allocated(self%cache%rg_bounds)) then
            bounds = self%cache%rg_bounds
        else
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
