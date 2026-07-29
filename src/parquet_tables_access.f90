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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_INT32) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_INT64) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT32) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT64) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_LOGICAL) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_DATE) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_TIME) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_TIMESTAMP) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_INT32_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_INT64_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT32_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT64_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_LOGICAL_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_DATE_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_TIME_VEC) then
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        if (self%cache%cols(idx)%values%kindof() /= PK_TIMESTAMP_VEC) then
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
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
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
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
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
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
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
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
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
            return
        end if
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case default
            call table_context_suffix(self, name, sfx)
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
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get")
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
            return
        end if
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
            return
        end if
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
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chrv
    !
end submodule parquet_tables_access
