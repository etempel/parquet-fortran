!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Copying a `parquet_table`: `%clone` (an independent deep copy) and `%clone_structure` (an
!! empty table with the same columns).
!!
!! **`%clone` is the only rollback mechanism this type has.** Mutation is in place, and there is
!! no undo log -- deliberately, since an implicit one would double the memory of every table on
!! the chance that someone wants to go back. The documented substitute is to copy before mutating,
!! which is only a real option if copying is a first-class, obvious operation. Hence this file.
!!
!! Both take `class(parquet_table), intent(out) :: out` rather than returning an allocatable
!! result. Fortran resolves a type-bound procedure reference against the DECLARED type, so a
!! `class(parquet_table), allocatable` result would make `out%mass()` a compile error on a
!! generated table and force `select type` around every use of a clone -- defeating the point of
!! the generated accessors. With `intent(out)` the caller declares the concrete type and
!! everything resolves normally; `intent(out)` on a finalizable type also runs the finalizer and
!! resets every component for free, which is exactly what a clone target wants.
submodule (parquet_tables) parquet_tables_clone
    implicit none
    !
contains
    !
    module procedure table_clone
        integer :: i
        !
        call table_check_open(self, "clone")
        call clone_check_same_type(self, out, "clone")
        call clone_new_cache(self, out)
        do i = 1, self%cache%ncols
            call clone_copy_descriptor(self%cache%cols(i), out%cache%cols(i))
            ! A column that was never read stays unread in the clone: a clone costs what the
            ! source actually holds, not what its file contains. The clone's own reader (below)
            ! is what lets it read those columns later.
            if (self%cache%cols(i)%residency == RES_FULL) then
                call self%cache%cols(i)%values%deep_copy(out%cache%cols(i)%values)
            end if
        end do
        out%cache%ncols = self%cache%ncols
        out%detached = self%detached
        out%regime = self%regime
        out%row_lo = self%row_lo
        out%row_hi = self%row_hi
        out%row_count = self%row_count
        out%cache%reads_started = self%cache%reads_started
        if (allocated(self%cache%rg_bounds)) out%cache%rg_bounds = self%cache%rg_bounds
        ! A detached source has no file left to reopen, and an in-memory one never had one, so
        ! both produce a clone with no reader. Only a live file-backed table opens its own.
        if (self%cache%file_backed .and. .not. self%detached) call clone_reopen_reader(self, out)
    end procedure table_clone
    !
    module procedure table_clone_structure
        integer :: i, n
        !
        call table_check_open(self, "clone_structure")
        call clone_check_same_type(self, out, "clone_structure")
        call clone_new_cache(self, out)
        n = 0
        do i = 1, self%cache%ncols
            ! An unsupported column is skipped: there is no kind to give it, so nothing could
            ! ever fill it. %append refuses a table holding one for the same reason, and points
            ! at %drop_column.
            if (.not. self%cache%cols(i)%supported) cycle
            ! A plain-LIST column's kind is not known until its width is, and a batch column has
            ! no file to measure it from later -- so it is measured now, for real. That reads
            ! data for that one column, exactly as %kind already does.
            call table_resolve_width(self%cache, table_scope_of(self), i, .true., "clone_structure")
            n = n + 1
            call clone_copy_descriptor(self%cache%cols(i), out%cache%cols(n))
            call clone_empty_column(self%cache%cols(i)%values, out%cache%cols(n)%values)
            out%cache%cols(n)%file_source = .false.
            out%cache%cols(n)%residency = RES_FULL
        end do
        out%cache%ncols = n
        ! The result is an in-memory table, not a detached one: it was never attached to a file
        ! in the first place, which is the same state parquet_new_table leaves a table in.
        out%detached = .false.
        out%regime = REGIME_FULL
        out%row_lo = 1_int64
        out%row_hi = 0_int64
        out%row_count = 0_int64
    end procedure table_clone_structure
    !
    !> error stops unless source and destination have the same dynamic type.
    !!
    !! Cloning a generated table into a bare `parquet_table` would compile and run, and silently
    !! produce a copy without the predefined accessors the caller is about to reach for. Failing
    !! loudly here is the whole reason the guard exists.
    subroutine clone_check_same_type(self, out, proc)
        class(parquet_table), intent(in) :: self !! the source table.
        class(parquet_table), intent(in) :: out  !! the destination table.
        character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        !
        if (same_type_as(self, out)) return
        error stop EP // trim(proc) // ": source and destination must be the same table type; " // &
            "cloning into a plain parquet_table would silently drop the predefined columns"
    end subroutine clone_check_same_type
    !
    !> Gives `out` its own fresh, empty cache, sized to hold the source's columns.
    !!
    !! Never shares the source's cache. Two tables pointing at one store would double-free it and
    !! would make a mutation through either one visible through the other -- the exact hazard the
    !! blocked intrinsic assignment exists to prevent, so a clone must not reintroduce it.
    subroutine clone_new_cache(self, out)
        class(parquet_table), intent(in) :: self    !! the source table.
        class(parquet_table), intent(inout) :: out  !! the destination table.
        !
        allocate(out%cache)
        call record_open_thread(out%cache)
        allocate(out%cache%cols(max(self%cache%ncols, 1) + COL_HEADROOM))
        out%cache%ncols = 0
        out%cache%file_backed = .false.
        if (allocated(self%cache%source_file)) out%cache%source_file = self%cache%source_file
    end subroutine clone_new_cache
    !
    !> Copies everything about a column EXCEPT its values.
    subroutine clone_copy_descriptor(src, dst)
        type(parquet_table_column), intent(in) :: src  !! the source descriptor.
        type(parquet_table_column), intent(inout) :: dst !! the descriptor to fill.
        !
        dst%name = src%name
        dst%file_name = src%file_name
        dst%declared_kind = src%declared_kind
        dst%width = src%width
        dst%width_pending = src%width_pending
        dst%file_source = src%file_source
        dst%predefined = src%predefined
        dst%user_populated = src%user_populated
        dst%supported = src%supported
        dst%residency = src%residency
    end subroutine clone_copy_descriptor
    !
    !> Creates a zero-row column of the same kind, width and unit as `src`.
    subroutine clone_empty_column(src, dst)
        type(parquet_column), intent(in) :: src   !! the column to take the shape of.
        type(parquet_column), intent(inout) :: dst !! receives the empty column.
        character(len=:), allocatable :: unit
        !
        call src%unit_string(unit)
        call dst%init(src%kindof(), 0_int64, src%colwidth(), unit)
    end subroutine clone_empty_column
    !
    !> Opens the clone's own reader on the same file, so the clone stays lazy.
    !!
    !! A failure here is a hard error naming the file rather than a quiet fallback to a
    !! reader-less clone: degrading silently would leave the caller with a table that
    !! mysteriously refuses to read a column much later, far from the cause.
    !!
    !! **Obligation for whoever adds a table-level read-time filter or sort:** this reopen makes a
    !! BARE reader. Once a table can carry a read-time transform, the clone must re-apply it here,
    !! or the clone's lazily-read columns will come back with rows the source had filtered away --
    !! two different lengths inside one table, with nothing to report it.
    subroutine clone_reopen_reader(self, out)
        class(parquet_table), intent(in) :: self    !! the source table.
        class(parquet_table), intent(inout) :: out  !! the destination table.
        !
        allocate(out%cache%reader)
        call parquet_open_reader(out%cache%reader, out%cache%source_file)
        out%cache%file_backed = .true.
    end subroutine clone_reopen_reader
    !
end submodule parquet_tables_clone
