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
    module procedure parquet_open_table
        integer :: i, ncol
        character(len=:), allocatable :: names(:)
        !
        ! `table` is intent(out) on a finalizable type, so table_finalize has already run on any
        ! previous contents by the time we get here -- that, and not any code below, is what stops
        ! a reopen from leaking the old cache. Do not "simplify" this to intent(inout).
        allocate(table%cache)
        table%file_backed = .true.
        table%source_file = trim(filename)
        table%regime = REGIME_FULL
        !
        allocate(table%cache%reader)
        call parquet_open_reader(table%cache%reader, trim(filename))
        call parquet_get_nrows(table%cache%reader, table%row_count)
        table%row_lo = 1
        table%row_hi = table%row_count
        !
        call parquet_get_column_names(table%cache%reader, names)
        ncol = size(names)
        allocate(table%cache%cols(ncol + COL_HEADROOM))
        table%cache%ncols = ncol
        do i = 1, ncol
            table%cache%cols(i)%name = trim(names(i))
            table%cache%cols(i)%file_name = trim(names(i))
            table%cache%cols(i)%file_source = .true.
        end do
        !
        call table_materialize_all(table)
        table%cache%reads_started = .true.
    end procedure parquet_open_table
    !
    module procedure parquet_new_table
        allocate(table%cache)
        table%file_backed = .false.
        table%source_file = ""
        table%regime = REGIME_FULL
        table%row_count = 0
        table%row_lo = 1
        table%row_hi = 0
        allocate(table%cache%cols(COL_HEADROOM))
        table%cache%ncols = 0
    end procedure parquet_new_table
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
                call table_context_suffix(self, name, sfx)
                error stop EP // "add_column: a column of this name already exists; pass " // &
                    "force=.true. to replace it" // sfx
            else if (.not. force) then
                call table_context_suffix(self, name, sfx)
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
            call table_context_suffix(self, name, sfx)
            write(got, "(I0)") n
            write(want, "(I0)") self%row_count
            error stop EP // "add_column: every column must have the same number of rows (got " // &
                trim(got) // ", table has " // trim(want) // ")" // sfx
        end if
    end procedure table_fix_nrows
    !
end submodule parquet_tables_lifecycle
