!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Per-kind `%add_column` for `parquet_table`: appends a new in-memory column, taking its
!! kind, width and row count from the values it is given.
submodule (parquet_tables) parquet_tables_addcol
    implicit none
    !
contains
    !
    module procedure add_column_i32
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_INT32, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_INT32
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_i32
    !
    module procedure add_column_i64
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_INT64, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_INT64
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_i64
    !
    module procedure add_column_f32
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_FLOAT32, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_FLOAT32
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_f32
    !
    module procedure add_column_f64
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_FLOAT64, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_FLOAT64
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_f64
    !
    module procedure add_column_bool
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_LOGICAL, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_LOGICAL
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_bool
    !
    module procedure add_column_date
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_DATE, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_DATE
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_date
    !
    module procedure add_column_time
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_TIME, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_TIME
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_time
    !
    module procedure add_column_ts
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_TIMESTAMP, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_TIMESTAMP
        self%cache%cols(idx)%width = 1_int32
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_ts
    !
    module procedure add_column_i32v
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_INT32_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_INT32_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_i32v
    !
    module procedure add_column_i64v
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_INT64_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_INT64_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_i64v
    !
    module procedure add_column_f32v
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_FLOAT32_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_FLOAT32_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_f32v
    !
    module procedure add_column_f64v
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_FLOAT64_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_FLOAT64_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_f64v
    !
    module procedure add_column_boolv
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_LOGICAL_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_LOGICAL_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_boolv
    !
    module procedure add_column_datev
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_DATE_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_DATE_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_datev
    !
    module procedure add_column_timev
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_TIME_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_TIME_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_timev
    !
    module procedure add_column_tsv
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_TIMESTAMP_VEC, size(values, 2, kind=int64), int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_TIMESTAMP_VEC
        self%cache%cols(idx)%width = int(size(values, 1), int32)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_tsv
    !
    module procedure add_column_chr
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_STRING, size(values, kind=int64), 1_int32, unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_STRING
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_chr
    !
    module procedure add_column_chrv
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, size(values, 2, kind=int64))
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_STRING_VEC, size(values, 2, kind=int64), &
            int(size(values, 1), int32), unit)
        call self%cache%cols(idx)%values%set_all(values)
        self%cache%cols(idx)%declared_kind = PK_STRING_VEC
        self%cache%cols(idx)%width = size(values, 1)
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_chrv
    !
    module procedure add_column_strcol
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, values%size())
        call table_new_slot(self, name, force, idx)
        call self%cache%cols(idx)%values%init(PK_STRING, values%size(), 1_int32, unit)
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        store = values%clone()
        self%cache%cols(idx)%declared_kind = PK_STRING
        self%cache%cols(idx)%width = 1
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_strcol
    !
    module procedure add_column_col
        integer :: idx
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, "add_column")
        ! Every other %add_column form takes its kind from the TYPE of the values it is given, so
        ! it cannot be kindless. This one reads the kind off the column, and a column that was
        ! never given one would land in the table as a PK_NONE slot that nothing can read or write
        ! -- failing later, at the first %get, with nothing to say where it came from. Checked
        ! before table_fix_nrows and table_new_slot, so a refused call changes nothing.
        if (values%kindof() == PK_NONE) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "add_column: this parquet_column has no kind yet, so there is " // &
                "nothing to add; give it one with %init, %adopt or %append_values first" // sfx
        end if
        call table_fix_nrows(self, name, values%length())
        call table_new_slot(self, name, force, idx)
        ! Copied, not moved: `values` is intent(in) like every other %add_column form's, so the
        ! caller's column is left intact and can be added to a second table.
        call values%deep_copy(self%cache%cols(idx)%values)
        ! The copy already carries the source column's unit, so this only has to run when the
        ! caller asked for a different one.
        if (present(unit)) call self%cache%cols(idx)%values%set_unit(unit)
        ! Read off the column rather than named by the caller -- which is what lets one specific
        ! stand in for all eighteen of the per-kind ones.
        self%cache%cols(idx)%declared_kind = values%kindof()
        self%cache%cols(idx)%width = values%colwidth()
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure add_column_col
    !
end submodule parquet_tables_addcol ! GCOVR_EXCL_LINE
