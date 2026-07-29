!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Turning a parquet file's columns into `parquet_column` stores: deciding each column's kind
!! from the file schema, driving the per-kind readers, and freeing the Arrow buffers as it goes.
!!
!! The memory rule this file exists to enforce: the table owns the sole Fortran-side copy of each
!! column, and the reader's decoded Arrow array is released the moment that copy exists. Without
!! that release an eagerly-materialized table would hold the whole file twice for the reader's
!! entire lifetime. A struct is cached by the reader as ONE array covering all its leaves, so the
!! release is deferred until the last leaf of a struct has been read -- see `table_materialize_all`.
submodule (parquet_tables) parquet_tables_read
    implicit none
    !
contains
    !
    module procedure table_kind_from_type
        ! parquet_get_column_type reports the ELEMENT type of a vector column, so col_size is
        ! what decides between the scalar and *_VEC form of each kind.
        logical :: vec
        !
        ok = .true.
        vec = col_size > 1
        select case (trim(type_name))
        case ("int32")
            kind = merge(PK_INT32_VEC, PK_INT32, vec)
        case ("int64")
            kind = merge(PK_INT64_VEC, PK_INT64, vec)
        case ("float32")
            kind = merge(PK_FLOAT32_VEC, PK_FLOAT32, vec)
        case ("float64")
            kind = merge(PK_FLOAT64_VEC, PK_FLOAT64, vec)
        case ("boolean")
            kind = merge(PK_LOGICAL_VEC, PK_LOGICAL, vec)
        case ("string")
            kind = merge(PK_STRING_VEC, PK_STRING, vec)
        case ("date")
            kind = merge(PK_DATE_VEC, PK_DATE, vec)
        case ("time")
            kind = merge(PK_TIME_VEC, PK_TIME, vec)
        case ("timestamp")
            kind = merge(PK_TIMESTAMP_VEC, PK_TIMESTAMP, vec)
        case default
            kind = PK_NONE
            ok = .false.
        end select
    end procedure table_kind_from_type
    !
    module procedure table_materialize
        character(len=:), allocatable :: type_name
        integer :: kind, col_size
        integer(int32) :: wdt
        logical :: ok
        !
        associate (slot => self%cache%cols(idx))
            ! Ask whether the type is readable BEFORE asking what it is: parquet_get_column_type
            ! error stops on a type outside its nine canonical tokens, so probing with
            ! parquet_column_exists(types=...) first is what keeps one exotic column from making
            ! the whole file unopenable.
            if (.not. parquet_column_exists(self%cache%reader, slot%file_name, &
                    types="int32,int64,float32,float64,string,boolean,date,time,timestamp")) then
                slot%supported = .false.
                slot%declared_kind = PK_NONE
                slot%residency = RES_EMPTY
                return
            end if
            call parquet_get_column_type(self%cache%reader, slot%file_name, type_name)
            call parquet_get_col_size(self%cache%reader, slot%file_name, col_size)
            call table_kind_from_type(type_name, col_size, kind, ok)
            if (.not. ok) then
                slot%supported = .false.
                slot%declared_kind = PK_NONE
                slot%residency = RES_EMPTY
                return
            end if
            wdt = int(max(col_size, 1), int32)
            ! Units are not read from the file in this milestone: their source is a read-time
            ! MAML's `unit:` key, which does not exist yet. %unit therefore reports "" for a
            ! file-backed column, and %add_column(unit=) is the only way to set one.
            call table_materialize_kind(kind, self%cache%reader, slot%file_name, slot%values, &
                self%row_count, wdt, "")
            slot%declared_kind = kind
            slot%residency = RES_FULL
        end associate
    end procedure table_materialize
    !
    module procedure table_materialize_all
        integer :: i
        character(len=:), allocatable :: top, prev_top
        !
        prev_top = ""
        do i = 1, self%cache%ncols
            call table_materialize(self, i)
            ! Release policy. The reader caches a struct as ONE array shared by all its leaves,
            ! so releasing after every leaf would re-read the struct once per leaf. Column names
            ! arrive in schema order, which puts a struct's leaves next to each other, so
            ! releasing the PREVIOUS top-level name as soon as the top-level changes frees each
            ! array exactly once, at the earliest point it is safe to.
            top = top_level_of(self%cache%cols(i)%file_name)
            if (len(prev_top) > 0 .and. prev_top /= top) then
                call parquet_release_column(self%cache%reader, prev_top)
            end if
            prev_top = top
        end do
        if (len(prev_top) > 0) call parquet_release_column(self%cache%reader, prev_top)
    end procedure table_materialize_all
    !
    !> The part of a (possibly dotted) column path before its first "." -- i.e. the name the
    !! reader caches the decoded array under. A name with no dot is its own top level.
    function top_level_of(path) result(top)
        character(len=*), intent(in) :: path      !! column path, dotted or not.
        character(len=:), allocatable :: top      !! the top-level field name.
        integer :: dot
        !
        dot = index(path, ".")
        if (dot == 0) then
            top = trim(path)
        else
            top = path(1:dot - 1)
        end if
    end function top_level_of
    !
end submodule parquet_tables_read
