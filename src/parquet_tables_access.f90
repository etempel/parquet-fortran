!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Per-kind value access for `parquet_table`: the zero-copy pointer path (`%col`), the
!! widening copy-out (`%get`) and the copy-back (`%set`).
submodule (parquet_tables) parquet_tables_access
    implicit none
    !
contains
    !
    module procedure col_ptr_i32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_INT32) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i32
    !
    module procedure col_ptr_i64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_INT64) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i64
    !
    module procedure col_ptr_f32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT32) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f32
    !
    module procedure col_ptr_f64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT64) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f64
    !
    module procedure col_ptr_bool
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_LOGICAL) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_bool
    !
    module procedure col_ptr_date
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_DATE) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_date
    !
    module procedure col_ptr_time
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_TIME) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_time
    !
    module procedure col_ptr_ts
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_TIMESTAMP) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_ts
    !
    module procedure col_ptr_i32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_INT32_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i32v
    !
    module procedure col_ptr_i64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_INT64_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i64v
    !
    module procedure col_ptr_f32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT32_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f32v
    !
    module procedure col_ptr_f64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT64_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f64v
    !
    module procedure col_ptr_boolv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_LOGICAL_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_boolv
    !
    module procedure col_ptr_datev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_DATE_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_datev
    !
    module procedure col_ptr_timev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_TIME_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_timev
    !
    module procedure col_ptr_tsv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        if (self%cache%cols(idx)%values%kindof() /= PK_TIMESTAMP_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_tsv
    !
    module procedure get_arr_i32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int32), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i32
    !
    module procedure get_arr_i64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int64), pointer :: p(:)
        integer(int32), pointer :: p_i32(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT64)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case (PK_INT32)
            call self%cache%cols(idx)%values%data_ptr(p_i32)
            allocate(arr(size(p_i32)))
            arr = p_i32
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i64
    !
    module procedure get_arr_f32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real32), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f32
    !
    module procedure get_arr_f64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real64), pointer :: p(:)
        real(real32), pointer :: p_f32(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT64)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%data_ptr(p_f32)
            allocate(arr(size(p_f32)))
            arr = p_f32
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f64
    !
    module procedure get_arr_bool
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        logical, pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_bool
    !
    module procedure get_arr_date
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_date), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_date
    !
    module procedure get_arr_time
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_time), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_time
    !
    module procedure get_arr_ts
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_timestamp), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_ts
    !
    module procedure get_arr_i32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int32), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i32v
    !
    module procedure get_arr_i64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int64), pointer :: p(:,:)
        integer(int32), pointer :: p_i32v(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT64_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case (PK_INT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p_i32v)
            allocate(arr(size(p_i32v,1), size(p_i32v,2)))
            arr = p_i32v
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i64v
    !
    module procedure get_arr_f32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real32), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f32v
    !
    module procedure get_arr_f64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real64), pointer :: p(:,:)
        real(real32), pointer :: p_f32v(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT64_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case (PK_FLOAT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p_f32v)
            allocate(arr(size(p_f32v,1), size(p_f32v,2)))
            arr = p_f32v
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f64v
    !
    module procedure get_arr_boolv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        logical, pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_boolv
    !
    module procedure get_arr_datev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_date), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_datev
    !
    module procedure get_arr_timev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_time), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_timev
    !
    module procedure get_arr_tsv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_timestamp), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_tsv
    !
    module procedure get_arr_str
        integer :: idx
        type(parquet_string_column), pointer :: src
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING, "get")
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call self%cache%cols(idx)%values%string_column(src)
        arr = src%clone()
    end procedure get_arr_str
    !
    module procedure get_arr_chr
        integer :: idx, maxlen
        integer(int64) :: i, n
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call table_require_kind(self, idx, PK_STRING, "get")
        n = self%cache%cols(idx)%values%length()
        ! Two passes: the width must be the longest element present, and a fixed-length array
        ! cannot be grown per element. A null reads back as "" and so contributes length 0.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do i = 1, n
            call store%get(i, s, allow_null=.true.)
            if (len(s) > maxlen) maxlen = len(s)
        end do
        allocate(character(len=maxlen) :: arr(n))
        do i = 1, n
            call store%get(i, s, allow_null=.true.)
            arr(i) = s
        end do
    end procedure get_arr_chr
    !
    module procedure get_arr_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: i, n, flat
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call table_require_kind(self, idx, PK_STRING_VEC, "get")
        n = self%cache%cols(idx)%values%length()
        wdt = self%cache%cols(idx)%values%colwidth()
        ! A vector string column is ONE flat string store of width*nrows elements, element
        ! (e, i) living at (i-1)*width + e -- reaching it directly is what lets each element
        ! come back as an allocatable string, which the two-pass width measurement needs.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, s, allow_null=.true.)
                if (len(s) > maxlen) maxlen = len(s)
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, n))
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, s, allow_null=.true.)
                arr(e, i) = s
            end do
        end do
    end procedure get_arr_chrv
    !
    module procedure set_arr_i32
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_INT32, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i32
    !
    module procedure set_arr_i64
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_INT64, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i64
    !
    module procedure set_arr_f32
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_FLOAT32, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f32
    !
    module procedure set_arr_f64
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_FLOAT64, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f64
    !
    module procedure set_arr_bool
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_LOGICAL, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_bool
    !
    module procedure set_arr_date
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_DATE, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_date
    !
    module procedure set_arr_time
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_TIME, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_time
    !
    module procedure set_arr_ts
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_TIMESTAMP, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_ts
    !
    module procedure set_arr_i32v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_INT32_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i32v
    !
    module procedure set_arr_i64v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_INT64_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i64v
    !
    module procedure set_arr_f32v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f32v
    !
    module procedure set_arr_f64v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f64v
    !
    module procedure set_arr_boolv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_boolv
    !
    module procedure set_arr_datev
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_DATE_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_datev
    !
    module procedure set_arr_timev
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_TIME_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_timev
    !
    module procedure set_arr_tsv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_tsv
    !
    module procedure set_arr_chr
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chr
    !
    module procedure set_arr_chrv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_STRING_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chrv
    !
    module procedure get_element_i32_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i32_i32
    !
    module procedure get_element_i32_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0_int32
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_INT32, "get_element")
        end select
    end procedure get_element_i32_i64
    !
    module procedure get_element_i64_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i64_i32
    !
    module procedure get_element_i64_i64
        integer :: idx
        integer(int32) :: v_i32
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0_int64
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64)
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(i, v_i32)
            value = v_i32
        case default
            call table_require_kind(self, idx, PK_INT64, "get_element")
        end select
    end procedure get_element_i64_i64
    !
    module procedure get_element_f32_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f32_i32
    !
    module procedure get_element_f32_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0.0_real32
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_FLOAT32, "get_element")
        end select
    end procedure get_element_f32_i64
    !
    module procedure get_element_f64_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f64_i32
    !
    module procedure get_element_f64_i64
        integer :: idx
        real(real32) :: v_f32
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0.0_real64
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64)
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(i, v_f32)
            value = v_f32
        case default
            call table_require_kind(self, idx, PK_FLOAT64, "get_element")
        end select
    end procedure get_element_f64_i64
    !
    module procedure get_element_bool_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_bool_i32
    !
    module procedure get_element_bool_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = .false.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_LOGICAL, "get_element")
        end select
    end procedure get_element_bool_i64
    !
    module procedure get_element_date_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_date_i32
    !
    module procedure get_element_date_i64
        integer :: idx
        !
        type(parquet_date) :: blank
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_DATE, "get_element")
        end select
    end procedure get_element_date_i64
    !
    module procedure get_element_time_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_time_i32
    !
    module procedure get_element_time_i64
        integer :: idx
        !
        type(parquet_time) :: blank
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIME, "get_element")
        end select
    end procedure get_element_time_i64
    !
    module procedure get_element_ts_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_ts_i32
    !
    module procedure get_element_ts_i64
        integer :: idx
        !
        type(parquet_timestamp) :: blank
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIMESTAMP, "get_element")
        end select
    end procedure get_element_ts_i64
    !
    module procedure get_element_i32v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i32v_i32
    !
    module procedure get_element_i32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_INT32_VEC, "get_element")
        end select
    end procedure get_element_i32v_i64
    !
    module procedure get_element_i64v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i64v_i32
    !
    module procedure get_element_i64v_i64
        integer :: idx
        integer(int32), allocatable :: v_i32v(:)
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_INT32_VEC)
            allocate(v_i32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, v_i32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_i32v
        case default
            call table_require_kind(self, idx, PK_INT64_VEC, "get_element")
        end select
    end procedure get_element_i64v_i64
    !
    module procedure get_element_f32v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f32v_i32
    !
    module procedure get_element_f32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_FLOAT32_VEC, "get_element")
        end select
    end procedure get_element_f32v_i64
    !
    module procedure get_element_f64v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f64v_i32
    !
    module procedure get_element_f64v_i64
        integer :: idx
        real(real32), allocatable :: v_f32v(:)
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_FLOAT32_VEC)
            allocate(v_f32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, v_f32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_f32v
        case default
            call table_require_kind(self, idx, PK_FLOAT64_VEC, "get_element")
        end select
    end procedure get_element_f64v_i64
    !
    module procedure get_element_boolv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_boolv_i32
    !
    module procedure get_element_boolv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_LOGICAL_VEC, "get_element")
        end select
    end procedure get_element_boolv_i64
    !
    module procedure get_element_datev_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_datev_i32
    !
    module procedure get_element_datev_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_DATE_VEC, "get_element")
        end select
    end procedure get_element_datev_i64
    !
    module procedure get_element_timev_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_timev_i32
    !
    module procedure get_element_timev_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIME_VEC, "get_element")
        end select
    end procedure get_element_timev_i64
    !
    module procedure get_element_tsv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_tsv_i32
    !
    module procedure get_element_tsv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "get_element")
        end select
    end procedure get_element_tsv_i64
    !
    module procedure get_element_chr_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chr_i32
    !
    module procedure get_element_chr_i64
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        value = ""
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_element")
        call table_require_row(self, i, "get_element")
        call self%cache%cols(idx)%values%string_column(store)
        ! allow_null keeps a null row from aborting: it reads back as "", and %is_null is how a
        ! caller tells the two apart -- the same rule the row handle's %get follows.
        call store%get(i, value, allow_null=.true.)
    end procedure get_element_chr_i64
    !
    module procedure get_element_chrv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chrv_i32
    !
    module procedure get_element_chrv_i64
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        character(len=:), allocatable :: str1
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "get_element")
        call table_require_row(self, i, "get_element")
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, row) at
        ! (row-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%get(flat, str1, allow_null=.true.)
            if (len(str1) > maxlen) maxlen = len(str1)
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%get(flat, str1, allow_null=.true.)
            value(e) = str1
        end do
    end procedure get_element_chrv_i64
    !
    module procedure set_element_i32_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_i32_i32
    !
    module procedure set_element_i32_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_INT32, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_i32_i64
    !
    module procedure set_element_i64_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_i64_i32
    !
    module procedure set_element_i64_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_INT64, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_i64_i64
    !
    module procedure set_element_f32_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_f32_i32
    !
    module procedure set_element_f32_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_FLOAT32, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_f32_i64
    !
    module procedure set_element_f64_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_f64_i32
    !
    module procedure set_element_f64_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_FLOAT64, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_f64_i64
    !
    module procedure set_element_bool_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_bool_i32
    !
    module procedure set_element_bool_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_LOGICAL, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_bool_i64
    !
    module procedure set_element_date_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_date_i32
    !
    module procedure set_element_date_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_DATE, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_date_i64
    !
    module procedure set_element_time_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_time_i32
    !
    module procedure set_element_time_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_TIME, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_time_i64
    !
    module procedure set_element_ts_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_ts_i32
    !
    module procedure set_element_ts_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_TIMESTAMP, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_ts_i64
    !
    module procedure set_element_i32v_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_i32v_i32
    !
    module procedure set_element_i32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_INT32_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_i32v_i64
    !
    module procedure set_element_i64v_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_i64v_i32
    !
    module procedure set_element_i64v_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_INT64_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_i64v_i64
    !
    module procedure set_element_f32v_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_f32v_i32
    !
    module procedure set_element_f32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_f32v_i64
    !
    module procedure set_element_f64v_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_f64v_i32
    !
    module procedure set_element_f64v_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_f64v_i64
    !
    module procedure set_element_boolv_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_boolv_i32
    !
    module procedure set_element_boolv_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_boolv_i64
    !
    module procedure set_element_datev_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_datev_i32
    !
    module procedure set_element_datev_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_DATE_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_datev_i64
    !
    module procedure set_element_timev_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_timev_i32
    !
    module procedure set_element_timev_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_TIME_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_timev_i64
    !
    module procedure set_element_tsv_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_tsv_i32
    !
    module procedure set_element_tsv_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_tsv_i64
    !
    module procedure set_element_chr_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_chr_i32
    !
    module procedure set_element_chr_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_STRING, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_chr_i64
    !
    module procedure set_element_chrv_i32
        call self%set_element(name, int(i, int64), value)
    end procedure set_element_chrv_i32
    !
    module procedure set_element_chrv_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_element", idx)
        call table_require_kind(self, idx, PK_STRING_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_chrv_i64
    !
    module procedure row_get_i32
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i32
    !
    module procedure row_get_i64
        integer :: idx
        integer(int32) :: v_i32
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(self%irow, v_i32)
            value = v_i32
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i64
    !
    module procedure row_get_f32
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f32
    !
    module procedure row_get_f64
        integer :: idx
        real(real32) :: v_f32
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(self%irow, v_f32)
            value = v_f32
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f64
    !
    module procedure row_get_bool
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_bool
    !
    module procedure row_get_str
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING)
        call self%cache%cols(idx)%values%string_column(store)
        ! allow_null keeps a null row from aborting: it reads back as "", and %is_null is how a
        ! caller tells the two apart.
        call store%get(self%irow, value, allow_null=.true.)
    end procedure row_get_str
    !
    module procedure row_get_date
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_date
    !
    module procedure row_get_time
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_time
    !
    module procedure row_get_ts
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_ts
    !
    module procedure row_get_i32v
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i32v
    !
    module procedure row_get_i64v
        integer :: idx
        integer(int32), allocatable :: v_i32v(:)
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_INT32_VEC)
            allocate(v_i32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, v_i32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_i32v
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i64v
    !
    module procedure row_get_f32v
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f32v
    !
    module procedure row_get_f64v
        integer :: idx
        real(real32), allocatable :: v_f32v(:)
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_FLOAT32_VEC)
            allocate(v_f32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, v_f32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_f32v
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f64v
    !
    module procedure row_get_boolv
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_boolv
    !
    module procedure row_get_strv
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING_VEC)
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, i) at
        ! (i-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%get(flat, s, allow_null=.true.)
            if (len(s) > maxlen) maxlen = len(s)
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%get(flat, s, allow_null=.true.)
            value(e) = s
        end do
    end procedure row_get_strv
    !
    module procedure row_get_datev
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_datev
    !
    module procedure row_get_timev
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_timev
    !
    module procedure row_get_tsv
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_tsv
    !
    module procedure get_slice_i32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i32
    !
    module procedure get_slice_i64
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        integer(int32) :: v_i32
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case (PK_INT32)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_i32)
                arr(k) = v_i32
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i64
    !
    module procedure get_slice_f32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f32
    !
    module procedure get_slice_f64
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        real(real32) :: v_f32
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case (PK_FLOAT32)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_f32)
                arr(k) = v_f32
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f64
    !
    module procedure get_slice_bool
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_bool
    !
    module procedure get_slice_date
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_date
    !
    module procedure get_slice_time
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_time
    !
    module procedure get_slice_ts
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_ts
    !
    module procedure get_slice_i32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i32v
    !
    module procedure get_slice_i64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        integer(int32), allocatable :: v_i32v(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case (PK_INT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            allocate(v_i32v(self%cache%cols(idx)%width))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_i32v)
                arr(:, k) = v_i32v
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i64v
    !
    module procedure get_slice_f32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f32v
    !
    module procedure get_slice_f64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        real(real32), allocatable :: v_f32v(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case (PK_FLOAT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            allocate(v_f32v(self%cache%cols(idx)%width))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_f32v)
                arr(:, k) = v_f32v
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f64v
    !
    module procedure get_slice_boolv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_boolv
    !
    module procedure get_slice_datev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_datev
    !
    module procedure get_slice_timev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_timev
    !
    module procedure get_slice_tsv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_tsv
    !
    module procedure get_slice_str
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call self%cache%cols(idx)%values%string_column(store)
        ! Built element by element rather than copied and trimmed: a gather has no contiguous
        ! source range to clone from, and appending keeps the result compact.
        do k = 1, size(rows, kind=int64)
            if (store%is_null(rows(k))) then
                call arr%append_null()
            else
                call store%get(rows(k), sv)
                call arr%append_string(sv)
            end if
        end do
    end procedure get_slice_str
    !
    module procedure get_slice_chr
        integer :: idx, maxlen
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call self%cache%cols(idx)%values%string_column(store)
        ! Two passes: a fixed-length array's width must be the longest element SELECTED, which
        ! is not known until every selected row has been looked at.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            call store%get(rows(k), sv, allow_null=.true.)
            if (len(sv) > maxlen) maxlen = len(sv)
        end do
        allocate(character(len=maxlen) :: arr(size(rows)))
        do k = 1, size(rows, kind=int64)
            call store%get(rows(k), sv, allow_null=.true.)
            arr(k) = sv
        end do
    end procedure get_slice_chr
    !
    module procedure get_slice_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: k, flat
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        wdt = self%cache%cols(idx)%width
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, sv, allow_null=.true.)
                if (len(sv) > maxlen) maxlen = len(sv)
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, size(rows)))
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, sv, allow_null=.true.)
                arr(e, k) = sv
            end do
        end do
    end procedure get_slice_chrv
    !
end submodule parquet_tables_access
