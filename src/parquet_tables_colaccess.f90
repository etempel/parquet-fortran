!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Per-kind value access for `parquet_table_col`, and the shared bodies the table's own
!! `%get_element` calls so the two forms cannot answer differently.
!!
!! Every `col_fetch_<tag>` takes the resolved PIECES -- cache, slot, kind -- rather than a handle,
!! because building a handle purely to pass it costs more than the body costs to run. The handle's
!! own `%get` validates itself and then calls exactly the same body.
submodule (parquet_tables) parquet_tables_colaccess
    implicit none
    !
contains
    !
    module procedure col_fetch_i32
        !
        select case (colkind)
        case (PK_INT32)
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_INT32, proc)
        end select
    end procedure col_fetch_i32
    !
    module procedure col_fetch_i64
        integer(int32) :: v_i32
        !
        select case (colkind)
        case (PK_INT64)
            call cache%cols(slot)%values%get_at(i, value)
        case (PK_INT32)
            call cache%cols(slot)%values%get_at(i, v_i32)
            value = v_i32
        case default
            call cache_require_kind(cache, slot, PK_INT64, proc)
        end select
    end procedure col_fetch_i64
    !
    module procedure col_fetch_f32
        !
        select case (colkind)
        case (PK_FLOAT32)
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_FLOAT32, proc)
        end select
    end procedure col_fetch_f32
    !
    module procedure col_fetch_f64
        real(real32) :: v_f32
        !
        select case (colkind)
        case (PK_FLOAT64)
            call cache%cols(slot)%values%get_at(i, value)
        case (PK_FLOAT32)
            call cache%cols(slot)%values%get_at(i, v_f32)
            value = v_f32
        case default
            call cache_require_kind(cache, slot, PK_FLOAT64, proc)
        end select
    end procedure col_fetch_f64
    !
    module procedure col_fetch_bool
        !
        select case (colkind)
        case (PK_LOGICAL)
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_LOGICAL, proc)
        end select
    end procedure col_fetch_bool
    !
    module procedure col_fetch_date
        !
        select case (colkind)
        case (PK_DATE)
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_DATE, proc)
        end select
    end procedure col_fetch_date
    !
    module procedure col_fetch_time
        !
        select case (colkind)
        case (PK_TIME)
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_TIME, proc)
        end select
    end procedure col_fetch_time
    !
    module procedure col_fetch_ts
        !
        select case (colkind)
        case (PK_TIMESTAMP)
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_TIMESTAMP, proc)
        end select
    end procedure col_fetch_ts
    !
    module procedure col_get_i32_i32
        call self%get(int(i, int64), value)
    end procedure col_get_i32_i32
    !
    module procedure col_get_i32_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_i32(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_i32_i64
    !
    module procedure col_get_i64_i32
        call self%get(int(i, int64), value)
    end procedure col_get_i64_i32
    !
    module procedure col_get_i64_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_i64(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_i64_i64
    !
    module procedure col_get_f32_i32
        call self%get(int(i, int64), value)
    end procedure col_get_f32_i32
    !
    module procedure col_get_f32_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_f32(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_f32_i64
    !
    module procedure col_get_f64_i32
        call self%get(int(i, int64), value)
    end procedure col_get_f64_i32
    !
    module procedure col_get_f64_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_f64(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_f64_i64
    !
    module procedure col_get_bool_i32
        call self%get(int(i, int64), value)
    end procedure col_get_bool_i32
    !
    module procedure col_get_bool_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_bool(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_bool_i64
    !
    module procedure col_get_date_i32
        call self%get(int(i, int64), value)
    end procedure col_get_date_i32
    !
    module procedure col_get_date_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_date(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_date_i64
    !
    module procedure col_get_time_i32
        call self%get(int(i, int64), value)
    end procedure col_get_time_i32
    !
    module procedure col_get_time_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_time(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_time_i64
    !
    module procedure col_get_ts_i32
        call self%get(int(i, int64), value)
    end procedure col_get_ts_i32
    !
    module procedure col_get_ts_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_ts(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_ts_i64
    !
end submodule parquet_tables_colaccess ! GCOVR_EXCL_LINE