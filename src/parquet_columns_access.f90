!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_columns.py
! The kind table lives in that script; edit it there, not here.
!
!> Per-kind value access for `parquet_column`: `get_at`, `set_at`, `set_all` and `data_ptr`
!! over every array kind. The string kinds live in `parquet_columns_string` instead, because
!! they delegate to `parquet_string_column` (DD1).
!!
!! Validity handling follows the RF9 rule table: writing a value CLEARS that row's null bit, so
!! a default `set_all` drops the bitmap outright (O(1), no scan); `modify_nulls=.false.` leaves
!! both the null rows and the bitmap untouched. Temporal kinds carry their null state inside
!! the element, so they only invalidate the cached null flag.
submodule (parquet_columns) parquet_columns_access
    implicit none
contains
    !
    module procedure get_at_i32
        call check_kind(self, PK_INT32, "get_at")
        call check_index(self, i, "get_at")
        value = self%i32(i)
    end procedure get_at_i32
    !
    module procedure set_at_i32
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT32, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%i32(i) = value
        if (self%has_nulls) call bit_clear(self%validity, i)
    end procedure set_at_i32
    !
    module procedure set_all_i32
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT32, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%i32(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%i32(k) = values(k)
            end do
        end if
    end procedure set_all_i32
    !
    module procedure data_ptr_i32
        call check_kind(self, PK_INT32, "data_ptr")
        p => self%i32(1:self%nrows)
    end procedure data_ptr_i32
    !
    module procedure get_at_i64
        call check_kind(self, PK_INT64, "get_at")
        call check_index(self, i, "get_at")
        value = self%i64(i)
    end procedure get_at_i64
    !
    module procedure set_at_i64
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT64, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%i64(i) = value
        if (self%has_nulls) call bit_clear(self%validity, i)
    end procedure set_at_i64
    !
    module procedure set_all_i64
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT64, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%i64(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%i64(k) = values(k)
            end do
        end if
    end procedure set_all_i64
    !
    module procedure data_ptr_i64
        call check_kind(self, PK_INT64, "data_ptr")
        p => self%i64(1:self%nrows)
    end procedure data_ptr_i64
    !
    module procedure get_at_f32
        call check_kind(self, PK_FLOAT32, "get_at")
        call check_index(self, i, "get_at")
        value = self%f32(i)
    end procedure get_at_f32
    !
    module procedure set_at_f32
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT32, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%f32(i) = value
        if (self%has_nulls) call bit_clear(self%validity, i)
    end procedure set_at_f32
    !
    module procedure set_all_f32
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT32, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%f32(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%f32(k) = values(k)
            end do
        end if
    end procedure set_all_f32
    !
    module procedure data_ptr_f32
        call check_kind(self, PK_FLOAT32, "data_ptr")
        p => self%f32(1:self%nrows)
    end procedure data_ptr_f32
    !
    module procedure get_at_f64
        call check_kind(self, PK_FLOAT64, "get_at")
        call check_index(self, i, "get_at")
        value = self%f64(i)
    end procedure get_at_f64
    !
    module procedure set_at_f64
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT64, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%f64(i) = value
        if (self%has_nulls) call bit_clear(self%validity, i)
    end procedure set_at_f64
    !
    module procedure set_all_f64
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT64, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%f64(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%f64(k) = values(k)
            end do
        end if
    end procedure set_all_f64
    !
    module procedure data_ptr_f64
        call check_kind(self, PK_FLOAT64, "data_ptr")
        p => self%f64(1:self%nrows)
    end procedure data_ptr_f64
    !
    module procedure get_at_bool
        call check_kind(self, PK_LOGICAL, "get_at")
        call check_index(self, i, "get_at")
        value = self%bool(i)
    end procedure get_at_bool
    !
    module procedure set_at_bool
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_LOGICAL, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%bool(i) = value
        if (self%has_nulls) call bit_clear(self%validity, i)
    end procedure set_at_bool
    !
    module procedure set_all_bool
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_LOGICAL, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%bool(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%bool(k) = values(k)
            end do
        end if
    end procedure set_all_bool
    !
    module procedure data_ptr_bool
        call check_kind(self, PK_LOGICAL, "data_ptr")
        p => self%bool(1:self%nrows)
    end procedure data_ptr_bool
    !
    module procedure get_at_date
        call check_kind(self, PK_DATE, "get_at")
        call check_index(self, i, "get_at")
        value = self%dt(i)
    end procedure get_at_date
    !
    module procedure set_at_date
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_DATE, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%dt(i) = value
        self%nulls_dirty = .true.
    end procedure set_at_date
    !
    module procedure set_all_date
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_DATE, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%dt(1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%dt(k) = values(k)
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_date
    !
    module procedure data_ptr_date
        call check_kind(self, PK_DATE, "data_ptr")
        p => self%dt(1:self%nrows)
    end procedure data_ptr_date
    !
    module procedure get_at_time
        call check_kind(self, PK_TIME, "get_at")
        call check_index(self, i, "get_at")
        value = self%tm(i)
    end procedure get_at_time
    !
    module procedure set_at_time
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIME, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%tm(i) = value
        self%nulls_dirty = .true.
    end procedure set_at_time
    !
    module procedure set_all_time
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIME, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%tm(1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%tm(k) = values(k)
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_time
    !
    module procedure data_ptr_time
        call check_kind(self, PK_TIME, "data_ptr")
        p => self%tm(1:self%nrows)
    end procedure data_ptr_time
    !
    module procedure get_at_ts
        call check_kind(self, PK_TIMESTAMP, "get_at")
        call check_index(self, i, "get_at")
        value = self%ts(i)
    end procedure get_at_ts
    !
    module procedure set_at_ts
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIMESTAMP, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%ts(i) = value
        self%nulls_dirty = .true.
    end procedure set_at_ts
    !
    module procedure set_all_ts
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIMESTAMP, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (mod_nulls) then
            self%ts(1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%ts(k) = values(k)
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_ts
    !
    module procedure data_ptr_ts
        call check_kind(self, PK_TIMESTAMP, "data_ptr")
        p => self%ts(1:self%nrows)
    end procedure data_ptr_ts
    !
    module procedure get_at_i32v
        call check_kind(self, PK_INT32_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%i32v(:, i)
    end procedure get_at_i32v
    !
    module procedure set_at_i32v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT32_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%i32v(:, i) = value
        if (self%has_nulls) then
            base = (i - 1_int64)*int(self%width, int64)
            do e = 1_int64, int(self%width, int64)
                call bit_clear(self%validity, base + e)
            end do
        end if
    end procedure set_at_i32v
    !
    module procedure set_all_i32v
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT32_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%i32v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%i32v(:, k) = values(:, k)
            end do
        end if
    end procedure set_all_i32v
    !
    module procedure data_ptr_i32v
        call check_kind(self, PK_INT32_VEC, "data_ptr")
        p => self%i32v(:, 1:self%nrows)
    end procedure data_ptr_i32v
    !
    module procedure get_at_i64v
        call check_kind(self, PK_INT64_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%i64v(:, i)
    end procedure get_at_i64v
    !
    module procedure set_at_i64v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT64_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%i64v(:, i) = value
        if (self%has_nulls) then
            base = (i - 1_int64)*int(self%width, int64)
            do e = 1_int64, int(self%width, int64)
                call bit_clear(self%validity, base + e)
            end do
        end if
    end procedure set_at_i64v
    !
    module procedure set_all_i64v
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT64_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%i64v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%i64v(:, k) = values(:, k)
            end do
        end if
    end procedure set_all_i64v
    !
    module procedure data_ptr_i64v
        call check_kind(self, PK_INT64_VEC, "data_ptr")
        p => self%i64v(:, 1:self%nrows)
    end procedure data_ptr_i64v
    !
    module procedure get_at_f32v
        call check_kind(self, PK_FLOAT32_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%f32v(:, i)
    end procedure get_at_f32v
    !
    module procedure set_at_f32v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT32_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%f32v(:, i) = value
        if (self%has_nulls) then
            base = (i - 1_int64)*int(self%width, int64)
            do e = 1_int64, int(self%width, int64)
                call bit_clear(self%validity, base + e)
            end do
        end if
    end procedure set_at_f32v
    !
    module procedure set_all_f32v
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT32_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%f32v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%f32v(:, k) = values(:, k)
            end do
        end if
    end procedure set_all_f32v
    !
    module procedure data_ptr_f32v
        call check_kind(self, PK_FLOAT32_VEC, "data_ptr")
        p => self%f32v(:, 1:self%nrows)
    end procedure data_ptr_f32v
    !
    module procedure get_at_f64v
        call check_kind(self, PK_FLOAT64_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%f64v(:, i)
    end procedure get_at_f64v
    !
    module procedure set_at_f64v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT64_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%f64v(:, i) = value
        if (self%has_nulls) then
            base = (i - 1_int64)*int(self%width, int64)
            do e = 1_int64, int(self%width, int64)
                call bit_clear(self%validity, base + e)
            end do
        end if
    end procedure set_at_f64v
    !
    module procedure set_all_f64v
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT64_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%f64v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%f64v(:, k) = values(:, k)
            end do
        end if
    end procedure set_all_f64v
    !
    module procedure data_ptr_f64v
        call check_kind(self, PK_FLOAT64_VEC, "data_ptr")
        p => self%f64v(:, 1:self%nrows)
    end procedure data_ptr_f64v
    !
    module procedure get_at_boolv
        call check_kind(self, PK_LOGICAL_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%boolv(:, i)
    end procedure get_at_boolv
    !
    module procedure set_at_boolv
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_LOGICAL_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%boolv(:, i) = value
        if (self%has_nulls) then
            base = (i - 1_int64)*int(self%width, int64)
            do e = 1_int64, int(self%width, int64)
                call bit_clear(self%validity, base + e)
            end do
        end if
    end procedure set_at_boolv
    !
    module procedure set_all_boolv
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_LOGICAL_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%boolv(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%boolv(:, k) = values(:, k)
            end do
        end if
    end procedure set_all_boolv
    !
    module procedure data_ptr_boolv
        call check_kind(self, PK_LOGICAL_VEC, "data_ptr")
        p => self%boolv(:, 1:self%nrows)
    end procedure data_ptr_boolv
    !
    module procedure get_at_datev
        call check_kind(self, PK_DATE_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%dtv(:, i)
    end procedure get_at_datev
    !
    module procedure set_at_datev
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_DATE_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%dtv(:, i) = value
        self%nulls_dirty = .true.
    end procedure set_at_datev
    !
    module procedure set_all_datev
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_DATE_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%dtv(:, 1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%dtv(:, k) = values(:, k)
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_datev
    !
    module procedure data_ptr_datev
        call check_kind(self, PK_DATE_VEC, "data_ptr")
        p => self%dtv(:, 1:self%nrows)
    end procedure data_ptr_datev
    !
    module procedure get_at_timev
        call check_kind(self, PK_TIME_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%tmv(:, i)
    end procedure get_at_timev
    !
    module procedure set_at_timev
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIME_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%tmv(:, i) = value
        self%nulls_dirty = .true.
    end procedure set_at_timev
    !
    module procedure set_all_timev
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIME_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%tmv(:, 1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%tmv(:, k) = values(:, k)
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_timev
    !
    module procedure data_ptr_timev
        call check_kind(self, PK_TIME_VEC, "data_ptr")
        p => self%tmv(:, 1:self%nrows)
    end procedure data_ptr_timev
    !
    module procedure get_at_tsv
        call check_kind(self, PK_TIMESTAMP_VEC, "get_at")
        call check_index(self, i, "get_at")
        call check_width(self, size(value, kind=int64), "get_at")
        value = self%tsv(:, i)
    end procedure get_at_tsv
    !
    module procedure set_at_tsv
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIMESTAMP_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        if (.not. mod_nulls) then
            if (self%is_null(i)) return
        end if
        self%tsv(:, i) = value
        self%nulls_dirty = .true.
    end procedure set_at_tsv
    !
    module procedure set_all_tsv
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIMESTAMP_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            self%tsv(:, 1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%tsv(:, k) = values(:, k)
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_tsv
    !
    module procedure data_ptr_tsv
        call check_kind(self, PK_TIMESTAMP_VEC, "data_ptr")
        p => self%tsv(:, 1:self%nrows)
    end procedure data_ptr_tsv
    !
end submodule parquet_columns_access
