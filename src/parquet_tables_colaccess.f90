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
    module procedure col_fetch_str
        call cache_require_kind(cache, slot, PK_STRING, proc)
        ! %get_at reads with allow_null, so a null row comes back as "" rather than aborting;
        ! %is_null is how a caller tells an empty string from a missing one.
        call cache%cols(slot)%values%get_at(i, value)
    end procedure col_fetch_str
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
    module procedure col_fetch_i32v
        !
        select case (colkind)
        case (PK_INT32_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_INT32_VEC, proc)
        end select
    end procedure col_fetch_i32v
    !
    module procedure col_fetch_i64v
        integer(int32), allocatable :: v_i32v(:)
        !
        select case (colkind)
        case (PK_INT64_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case (PK_INT32_VEC)
            allocate(v_i32v(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, v_i32v)
            allocate(value(cache%cols(slot)%width))
            value = v_i32v
        case default
            call cache_require_kind(cache, slot, PK_INT64_VEC, proc)
        end select
    end procedure col_fetch_i64v
    !
    module procedure col_fetch_f32v
        !
        select case (colkind)
        case (PK_FLOAT32_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_FLOAT32_VEC, proc)
        end select
    end procedure col_fetch_f32v
    !
    module procedure col_fetch_f64v
        real(real32), allocatable :: v_f32v(:)
        !
        select case (colkind)
        case (PK_FLOAT64_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case (PK_FLOAT32_VEC)
            allocate(v_f32v(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, v_f32v)
            allocate(value(cache%cols(slot)%width))
            value = v_f32v
        case default
            call cache_require_kind(cache, slot, PK_FLOAT64_VEC, proc)
        end select
    end procedure col_fetch_f64v
    !
    module procedure col_fetch_boolv
        !
        select case (colkind)
        case (PK_LOGICAL_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_LOGICAL_VEC, proc)
        end select
    end procedure col_fetch_boolv
    !
    module procedure col_fetch_strv
        integer :: e, wdt, maxlen
        integer(int64) :: flat
        type(parquet_string_column), pointer :: store
        !
        call cache_require_kind(cache, slot, PK_STRING_VEC, proc)
        wdt = cache%cols(slot)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, row) at
        ! (row-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call cache%cols(slot)%values%string_column(store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%copy_to(flat, value(e), allow_null=.true.)
        end do
    end procedure col_fetch_strv
    !
    module procedure col_fetch_datev
        !
        select case (colkind)
        case (PK_DATE_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_DATE_VEC, proc)
        end select
    end procedure col_fetch_datev
    !
    module procedure col_fetch_timev
        !
        select case (colkind)
        case (PK_TIME_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_TIME_VEC, proc)
        end select
    end procedure col_fetch_timev
    !
    module procedure col_fetch_tsv
        !
        select case (colkind)
        case (PK_TIMESTAMP_VEC)
            allocate(value(cache%cols(slot)%width))
            call cache%cols(slot)%values%get_at(i, value)
        case default
            call cache_require_kind(cache, slot, PK_TIMESTAMP_VEC, proc)
        end select
    end procedure col_fetch_tsv
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
    module procedure col_get_str_i32
        call self%get(int(i, int64), value)
    end procedure col_get_str_i32
    !
    module procedure col_get_str_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_str(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_str_i64
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
    module procedure col_get_i32v_i32
        call self%get(int(i, int64), value)
    end procedure col_get_i32v_i32
    !
    module procedure col_get_i32v_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_i32v(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_i32v_i64
    !
    module procedure col_get_i64v_i32
        call self%get(int(i, int64), value)
    end procedure col_get_i64v_i32
    !
    module procedure col_get_i64v_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_i64v(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_i64v_i64
    !
    module procedure col_get_f32v_i32
        call self%get(int(i, int64), value)
    end procedure col_get_f32v_i32
    !
    module procedure col_get_f32v_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_f32v(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_f32v_i64
    !
    module procedure col_get_f64v_i32
        call self%get(int(i, int64), value)
    end procedure col_get_f64v_i32
    !
    module procedure col_get_f64v_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_f64v(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_f64v_i64
    !
    module procedure col_get_boolv_i32
        call self%get(int(i, int64), value)
    end procedure col_get_boolv_i32
    !
    module procedure col_get_boolv_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_boolv(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_boolv_i64
    !
    module procedure col_get_strv_i32
        call self%get(int(i, int64), value)
    end procedure col_get_strv_i32
    !
    module procedure col_get_strv_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_strv(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_strv_i64
    !
    module procedure col_get_datev_i32
        call self%get(int(i, int64), value)
    end procedure col_get_datev_i32
    !
    module procedure col_get_datev_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_datev(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_datev_i64
    !
    module procedure col_get_timev_i32
        call self%get(int(i, int64), value)
    end procedure col_get_timev_i32
    !
    module procedure col_get_timev_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_timev(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_timev_i64
    !
    module procedure col_get_tsv_i32
        call self%get(int(i, int64), value)
    end procedure col_get_tsv_i32
    !
    module procedure col_get_tsv_i64
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        call col_fetch_tsv(self%cache, self%slot, self%colkind, i, value, "get")
    end procedure col_get_tsv_i64
    !
    module procedure col_store_i32
        call cache_require_kind(cache, slot, PK_INT32, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_i32
    !
    module procedure col_store_i64
        call cache_require_kind(cache, slot, PK_INT64, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_i64
    !
    module procedure col_store_f32
        call cache_require_kind(cache, slot, PK_FLOAT32, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_f32
    !
    module procedure col_store_f64
        call cache_require_kind(cache, slot, PK_FLOAT64, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_f64
    !
    module procedure col_store_bool
        call cache_require_kind(cache, slot, PK_LOGICAL, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_bool
    !
    module procedure col_store_str
        call cache_require_kind(cache, slot, PK_STRING, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_str
    !
    module procedure col_store_date
        call cache_require_kind(cache, slot, PK_DATE, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_date
    !
    module procedure col_store_time
        call cache_require_kind(cache, slot, PK_TIME, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_time
    !
    module procedure col_store_ts
        call cache_require_kind(cache, slot, PK_TIMESTAMP, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_ts
    !
    module procedure col_store_i32v
        call cache_require_kind(cache, slot, PK_INT32_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_i32v
    !
    module procedure col_store_i64v
        call cache_require_kind(cache, slot, PK_INT64_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_i64v
    !
    module procedure col_store_f32v
        call cache_require_kind(cache, slot, PK_FLOAT32_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_f32v
    !
    module procedure col_store_f64v
        call cache_require_kind(cache, slot, PK_FLOAT64_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_f64v
    !
    module procedure col_store_boolv
        call cache_require_kind(cache, slot, PK_LOGICAL_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_boolv
    !
    module procedure col_store_strv
        call cache_require_kind(cache, slot, PK_STRING_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_strv
    !
    module procedure col_store_datev
        call cache_require_kind(cache, slot, PK_DATE_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_datev
    !
    module procedure col_store_timev
        call cache_require_kind(cache, slot, PK_TIME_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_timev
    !
    module procedure col_store_tsv
        call cache_require_kind(cache, slot, PK_TIMESTAMP_VEC, proc)
        call cache%cols(slot)%values%set_at(i, value)
        cache%cols(slot)%user_populated = .true.
    end procedure col_store_tsv
    !
    module procedure col_set_i32_i32
        call self%set(int(i, int64), value)
    end procedure col_set_i32_i32
    !
    module procedure col_set_i32_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_i32(self%cache, self%slot, i, value, "set")
    end procedure col_set_i32_i64
    !
    module procedure col_set_i64_i32
        call self%set(int(i, int64), value)
    end procedure col_set_i64_i32
    !
    module procedure col_set_i64_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_i64(self%cache, self%slot, i, value, "set")
    end procedure col_set_i64_i64
    !
    module procedure col_set_f32_i32
        call self%set(int(i, int64), value)
    end procedure col_set_f32_i32
    !
    module procedure col_set_f32_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_f32(self%cache, self%slot, i, value, "set")
    end procedure col_set_f32_i64
    !
    module procedure col_set_f64_i32
        call self%set(int(i, int64), value)
    end procedure col_set_f64_i32
    !
    module procedure col_set_f64_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_f64(self%cache, self%slot, i, value, "set")
    end procedure col_set_f64_i64
    !
    module procedure col_set_bool_i32
        call self%set(int(i, int64), value)
    end procedure col_set_bool_i32
    !
    module procedure col_set_bool_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_bool(self%cache, self%slot, i, value, "set")
    end procedure col_set_bool_i64
    !
    module procedure col_set_str_i32
        call self%set(int(i, int64), value)
    end procedure col_set_str_i32
    !
    module procedure col_set_str_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_str(self%cache, self%slot, i, value, "set")
    end procedure col_set_str_i64
    !
    module procedure col_set_date_i32
        call self%set(int(i, int64), value)
    end procedure col_set_date_i32
    !
    module procedure col_set_date_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_date(self%cache, self%slot, i, value, "set")
    end procedure col_set_date_i64
    !
    module procedure col_set_time_i32
        call self%set(int(i, int64), value)
    end procedure col_set_time_i32
    !
    module procedure col_set_time_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_time(self%cache, self%slot, i, value, "set")
    end procedure col_set_time_i64
    !
    module procedure col_set_ts_i32
        call self%set(int(i, int64), value)
    end procedure col_set_ts_i32
    !
    module procedure col_set_ts_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_ts(self%cache, self%slot, i, value, "set")
    end procedure col_set_ts_i64
    !
    module procedure col_set_i32v_i32
        call self%set(int(i, int64), value)
    end procedure col_set_i32v_i32
    !
    module procedure col_set_i32v_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_i32v(self%cache, self%slot, i, value, "set")
    end procedure col_set_i32v_i64
    !
    module procedure col_set_i64v_i32
        call self%set(int(i, int64), value)
    end procedure col_set_i64v_i32
    !
    module procedure col_set_i64v_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_i64v(self%cache, self%slot, i, value, "set")
    end procedure col_set_i64v_i64
    !
    module procedure col_set_f32v_i32
        call self%set(int(i, int64), value)
    end procedure col_set_f32v_i32
    !
    module procedure col_set_f32v_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_f32v(self%cache, self%slot, i, value, "set")
    end procedure col_set_f32v_i64
    !
    module procedure col_set_f64v_i32
        call self%set(int(i, int64), value)
    end procedure col_set_f64v_i32
    !
    module procedure col_set_f64v_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_f64v(self%cache, self%slot, i, value, "set")
    end procedure col_set_f64v_i64
    !
    module procedure col_set_boolv_i32
        call self%set(int(i, int64), value)
    end procedure col_set_boolv_i32
    !
    module procedure col_set_boolv_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_boolv(self%cache, self%slot, i, value, "set")
    end procedure col_set_boolv_i64
    !
    module procedure col_set_strv_i32
        call self%set(int(i, int64), value)
    end procedure col_set_strv_i32
    !
    module procedure col_set_strv_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_strv(self%cache, self%slot, i, value, "set")
    end procedure col_set_strv_i64
    !
    module procedure col_set_datev_i32
        call self%set(int(i, int64), value)
    end procedure col_set_datev_i32
    !
    module procedure col_set_datev_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_datev(self%cache, self%slot, i, value, "set")
    end procedure col_set_datev_i64
    !
    module procedure col_set_timev_i32
        call self%set(int(i, int64), value)
    end procedure col_set_timev_i32
    !
    module procedure col_set_timev_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_timev(self%cache, self%slot, i, value, "set")
    end procedure col_set_timev_i64
    !
    module procedure col_set_tsv_i32
        call self%set(int(i, int64), value)
    end procedure col_set_tsv_i32
    !
    module procedure col_set_tsv_i64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call col_store_tsv(self%cache, self%slot, i, value, "set")
    end procedure col_set_tsv_i64
    !
    module procedure col_get_i32v_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_i32v_e32
    !
    module procedure col_get_i32v_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_INT32_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_INT32_VEC, "get")
        end select
    end procedure col_get_i32v_e64
    !
    module procedure col_get_i64v_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_i64v_e32
    !
    module procedure col_get_i64v_e64
        integer(int32) :: v_i32v
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_INT64_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case (PK_INT32_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, v_i32v)
            value = v_i32v
        case default
            call cache_require_kind(self%cache, self%slot, PK_INT64_VEC, "get")
        end select
    end procedure col_get_i64v_e64
    !
    module procedure col_get_f32v_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_f32v_e32
    !
    module procedure col_get_f32v_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_FLOAT32_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_FLOAT32_VEC, "get")
        end select
    end procedure col_get_f32v_e64
    !
    module procedure col_get_f64v_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_f64v_e32
    !
    module procedure col_get_f64v_e64
        real(real32) :: v_f32v
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_FLOAT64_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case (PK_FLOAT32_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, v_f32v)
            value = v_f32v
        case default
            call cache_require_kind(self%cache, self%slot, PK_FLOAT64_VEC, "get")
        end select
    end procedure col_get_f64v_e64
    !
    module procedure col_get_boolv_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_boolv_e32
    !
    module procedure col_get_boolv_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_LOGICAL_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_LOGICAL_VEC, "get")
        end select
    end procedure col_get_boolv_e64
    !
    module procedure col_get_strv_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_strv_e32
    !
    module procedure col_get_strv_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_STRING_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_STRING_VEC, "get")
        end select
    end procedure col_get_strv_e64
    !
    module procedure col_get_datev_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_datev_e32
    !
    module procedure col_get_datev_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_DATE_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_DATE_VEC, "get")
        end select
    end procedure col_get_datev_e64
    !
    module procedure col_get_timev_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_timev_e32
    !
    module procedure col_get_timev_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_TIME_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_TIME_VEC, "get")
        end select
    end procedure col_get_timev_e64
    !
    module procedure col_get_tsv_e32
        call self%get(int(i, int64), int(e, int64), value)
    end procedure col_get_tsv_e32
    !
    module procedure col_get_tsv_e64
        !
        call col_resolve(self, "get")
        call col_require_row(self, i, "get")
        select case (self%colkind)
        case (PK_TIMESTAMP_VEC)
            call self%cache%cols(self%slot)%values%get_elem(i, e, value)
        case default
            call cache_require_kind(self%cache, self%slot, PK_TIMESTAMP_VEC, "get")
        end select
    end procedure col_get_tsv_e64
    !
    module procedure col_set_i32v_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_i32v_e32
    !
    module procedure col_set_i32v_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_INT32_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_i32v_e64
    !
    module procedure col_set_i64v_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_i64v_e32
    !
    module procedure col_set_i64v_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_INT64_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_i64v_e64
    !
    module procedure col_set_f32v_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_f32v_e32
    !
    module procedure col_set_f32v_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_FLOAT32_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_f32v_e64
    !
    module procedure col_set_f64v_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_f64v_e32
    !
    module procedure col_set_f64v_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_FLOAT64_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_f64v_e64
    !
    module procedure col_set_boolv_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_boolv_e32
    !
    module procedure col_set_boolv_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_LOGICAL_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_boolv_e64
    !
    module procedure col_set_strv_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_strv_e32
    !
    module procedure col_set_strv_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_STRING_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_strv_e64
    !
    module procedure col_set_datev_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_datev_e32
    !
    module procedure col_set_datev_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_DATE_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_datev_e64
    !
    module procedure col_set_timev_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_timev_e32
    !
    module procedure col_set_timev_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_TIME_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_timev_e64
    !
    module procedure col_set_tsv_e32
        call self%set(int(i, int64), int(e, int64), value)
    end procedure col_set_tsv_e32
    !
    module procedure col_set_tsv_e64
        call col_resolve(self, "set")
        call col_require_row(self, i, "set")
        call cache_check_shared_write(self%cache, self%slot, "set", nulling=.false.)
        call cache_require_kind(self%cache, self%slot, PK_TIMESTAMP_VEC, "set")
        call self%cache%cols(self%slot)%values%set_elem(i, e, value)
        self%cache%cols(self%slot)%user_populated = .true.
    end procedure col_set_tsv_e64
    !
    module procedure col_ref_i32
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_INT32, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_i32
    !
    module procedure col_ref_i64
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_INT64, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_i64
    !
    module procedure col_ref_f32
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_FLOAT32, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_f32
    !
    module procedure col_ref_f64
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_FLOAT64, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_f64
    !
    module procedure col_ref_bool
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_LOGICAL, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_bool
    !
    module procedure col_ref_date
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_DATE, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_date
    !
    module procedure col_ref_time
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_TIME, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_time
    !
    module procedure col_ref_ts
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_TIMESTAMP, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_ts
    !
    module procedure col_ref_i32v
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_INT32_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_i32v
    !
    module procedure col_ref_i64v
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_INT64_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_i64v
    !
    module procedure col_ref_f32v
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_FLOAT32_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_f32v
    !
    module procedure col_ref_f64v
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_FLOAT64_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_f64v
    !
    module procedure col_ref_boolv
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_LOGICAL_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_boolv
    !
    module procedure col_ref_datev
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_DATE_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_datev
    !
    module procedure col_ref_timev
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_TIME_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_timev
    !
    module procedure col_ref_tsv
        call col_resolve(self, "ref")
        nullify(p)
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, self%slot, is_valid)
        call cache_require_ptr_kind(self%cache, self%slot, PK_TIMESTAMP_VEC, "ref")
        call self%cache%cols(self%slot)%values%data_ptr(p)
    end procedure col_ref_tsv
    !
    module procedure col_ref_strcol
        call col_resolve(self, "ref")
        nullify(p)
        call cache_require_kind(self%cache, self%slot, PK_STRING, "ref")
        call self%cache%cols(self%slot)%values%string_column(p)
    end procedure col_ref_strcol
    !
end submodule parquet_tables_colaccess ! GCOVR_EXCL_LINE