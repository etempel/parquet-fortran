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
!! the null entries and the bitmap untouched. Temporal kinds carry their null state inside
!! the element, so they only invalidate the cached null flag.
!!
!! **`modify_nulls=.false.` skips null ELEMENTS, not whole rows.** On a vector kind it writes
!! every element whose own bit is clear and leaves the null ones alone, rather than refusing the
!! whole row because one element of it is null -- matching the rule that each operation acts at
!! the granularity the caller named. The default (`.true.`) path is untouched by this and stays a
!! single whole-array assignment with no per-element work, so the common case costs nothing.
submodule (parquet_columns) parquet_columns_access
    implicit none
contains
    !
    module procedure parquet_column_get_at_i32
        call parquet_column_check_kind(col, PK_INT32, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%i32(i)
    end procedure parquet_column_get_at_i32
    !
    module procedure get_at_i32
        call parquet_column_get_at_i32(self, i, value)
    end procedure get_at_i32
    !
    module procedure parquet_column_set_at_i32
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_INT32, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%i32(i) = value
        if (col%has_nulls) call bit_clear(col%validity, i)
    end procedure parquet_column_set_at_i32
    !
    module procedure set_at_i32
        call parquet_column_set_at_i32(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%i32 is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%i32(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%i32(k) = values(k)
            end do
        end if
    end procedure set_all_i32
    !
    module procedure adopt_i32
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_INT32
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%i32)
    end procedure adopt_i32
    !
    module procedure parquet_column_data_ptr_i32
        call parquet_column_check_kind(col, PK_INT32, "data_ptr")
        p => col%i32(1:col%nrows)
    end procedure parquet_column_data_ptr_i32
    !
    module procedure data_ptr_i32
        call parquet_column_data_ptr_i32(self, p)
    end procedure data_ptr_i32
    !
    module procedure parquet_column_get_at_i64
        call parquet_column_check_kind(col, PK_INT64, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%i64(i)
    end procedure parquet_column_get_at_i64
    !
    module procedure get_at_i64
        call parquet_column_get_at_i64(self, i, value)
    end procedure get_at_i64
    !
    module procedure parquet_column_set_at_i64
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_INT64, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%i64(i) = value
        if (col%has_nulls) call bit_clear(col%validity, i)
    end procedure parquet_column_set_at_i64
    !
    module procedure set_at_i64
        call parquet_column_set_at_i64(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%i64 is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%i64(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%i64(k) = values(k)
            end do
        end if
    end procedure set_all_i64
    !
    module procedure adopt_i64
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_INT64
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%i64)
    end procedure adopt_i64
    !
    module procedure parquet_column_data_ptr_i64
        call parquet_column_check_kind(col, PK_INT64, "data_ptr")
        p => col%i64(1:col%nrows)
    end procedure parquet_column_data_ptr_i64
    !
    module procedure data_ptr_i64
        call parquet_column_data_ptr_i64(self, p)
    end procedure data_ptr_i64
    !
    module procedure parquet_column_get_at_f32
        call parquet_column_check_kind(col, PK_FLOAT32, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%f32(i)
    end procedure parquet_column_get_at_f32
    !
    module procedure get_at_f32
        call parquet_column_get_at_f32(self, i, value)
    end procedure get_at_f32
    !
    module procedure parquet_column_set_at_f32
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_FLOAT32, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%f32(i) = value
        if (col%has_nulls) call bit_clear(col%validity, i)
    end procedure parquet_column_set_at_f32
    !
    module procedure set_at_f32
        call parquet_column_set_at_f32(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%f32 is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%f32(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%f32(k) = values(k)
            end do
        end if
    end procedure set_all_f32
    !
    module procedure adopt_f32
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_FLOAT32
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%f32)
    end procedure adopt_f32
    !
    module procedure parquet_column_data_ptr_f32
        call parquet_column_check_kind(col, PK_FLOAT32, "data_ptr")
        p => col%f32(1:col%nrows)
    end procedure parquet_column_data_ptr_f32
    !
    module procedure data_ptr_f32
        call parquet_column_data_ptr_f32(self, p)
    end procedure data_ptr_f32
    !
    module procedure parquet_column_get_at_f64
        call parquet_column_check_kind(col, PK_FLOAT64, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%f64(i)
    end procedure parquet_column_get_at_f64
    !
    module procedure get_at_f64
        call parquet_column_get_at_f64(self, i, value)
    end procedure get_at_f64
    !
    module procedure parquet_column_set_at_f64
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_FLOAT64, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%f64(i) = value
        if (col%has_nulls) call bit_clear(col%validity, i)
    end procedure parquet_column_set_at_f64
    !
    module procedure set_at_f64
        call parquet_column_set_at_f64(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%f64 is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%f64(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%f64(k) = values(k)
            end do
        end if
    end procedure set_all_f64
    !
    module procedure adopt_f64
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_FLOAT64
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%f64)
    end procedure adopt_f64
    !
    module procedure parquet_column_data_ptr_f64
        call parquet_column_check_kind(col, PK_FLOAT64, "data_ptr")
        p => col%f64(1:col%nrows)
    end procedure parquet_column_data_ptr_f64
    !
    module procedure data_ptr_f64
        call parquet_column_data_ptr_f64(self, p)
    end procedure data_ptr_f64
    !
    module procedure parquet_column_get_at_bool
        call parquet_column_check_kind(col, PK_LOGICAL, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%bool(i)
    end procedure parquet_column_get_at_bool
    !
    module procedure get_at_bool
        call parquet_column_get_at_bool(self, i, value)
    end procedure get_at_bool
    !
    module procedure parquet_column_set_at_bool
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_LOGICAL, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%bool(i) = value
        if (col%has_nulls) call bit_clear(col%validity, i)
    end procedure parquet_column_set_at_bool
    !
    module procedure set_at_bool
        call parquet_column_set_at_bool(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%bool is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%bool(1:self%nrows) = values
            call drop_bitmap(self)
        else
            do k = 1_int64, self%nrows
                if (self%is_null(k)) cycle
                self%bool(k) = values(k)
            end do
        end if
    end procedure set_all_bool
    !
    module procedure adopt_bool
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_LOGICAL
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%bool)
    end procedure adopt_bool
    !
    module procedure parquet_column_data_ptr_bool
        call parquet_column_check_kind(col, PK_LOGICAL, "data_ptr")
        p => col%bool(1:col%nrows)
    end procedure parquet_column_data_ptr_bool
    !
    module procedure data_ptr_bool
        call parquet_column_data_ptr_bool(self, p)
    end procedure data_ptr_bool
    !
    module procedure parquet_column_get_at_date
        call parquet_column_check_kind(col, PK_DATE, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%dt(i)
    end procedure parquet_column_get_at_date
    !
    module procedure get_at_date
        call parquet_column_get_at_date(self, i, value)
    end procedure get_at_date
    !
    module procedure parquet_column_set_at_date
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_DATE, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%dt(i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_at_date
    !
    module procedure set_at_date
        call parquet_column_set_at_date(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%dt is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%dt(1:self%nrows) = values
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
    module procedure adopt_date
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_DATE
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%dt)
        self%nulls_dirty = .true.
    end procedure adopt_date
    !
    module procedure parquet_column_data_ptr_date
        call parquet_column_check_kind(col, PK_DATE, "data_ptr")
        p => col%dt(1:col%nrows)
    end procedure parquet_column_data_ptr_date
    !
    module procedure data_ptr_date
        call parquet_column_data_ptr_date(self, p)
    end procedure data_ptr_date
    !
    module procedure parquet_column_get_at_time
        call parquet_column_check_kind(col, PK_TIME, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%tm(i)
    end procedure parquet_column_get_at_time
    !
    module procedure get_at_time
        call parquet_column_get_at_time(self, i, value)
    end procedure get_at_time
    !
    module procedure parquet_column_set_at_time
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_TIME, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%tm(i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_at_time
    !
    module procedure set_at_time
        call parquet_column_set_at_time(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%tm is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%tm(1:self%nrows) = values
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
    module procedure adopt_time
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_TIME
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%tm)
        self%nulls_dirty = .true.
    end procedure adopt_time
    !
    module procedure parquet_column_data_ptr_time
        call parquet_column_check_kind(col, PK_TIME, "data_ptr")
        p => col%tm(1:col%nrows)
    end procedure parquet_column_data_ptr_time
    !
    module procedure data_ptr_time
        call parquet_column_data_ptr_time(self, p)
    end procedure data_ptr_time
    !
    module procedure parquet_column_get_at_ts
        call parquet_column_check_kind(col, PK_TIMESTAMP, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        value = col%ts(i)
    end procedure parquet_column_get_at_ts
    !
    module procedure get_at_ts
        call parquet_column_get_at_ts(self, i, value)
    end procedure get_at_ts
    !
    module procedure parquet_column_set_at_ts
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_TIMESTAMP, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_column_is_null(col, i)) return
        end if
        col%ts(i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_at_ts
    !
    module procedure set_at_ts
        call parquet_column_set_at_ts(self, i, value, modify_nulls)
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
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%ts is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%ts(1:self%nrows) = values
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
    module procedure adopt_ts
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        call self%clear()
        self%kind = PK_TIMESTAMP
        self%width = 1_int32
        self%nrows = size(values, kind=int64)
        ! The adopted allocation IS the capacity -- leaving `cap` at 0 would make the next append
        ! reallocate a column that already has room, and would break the cap >= nrows invariant.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! move_alloc, not assignment: the point is that no element is copied and the column
        ! inherits the caller's allocation outright. `clear` above already dropped any previous
        ! bitmap, so an adopted column starts with no nulls recorded.
        call move_alloc(values, self%ts)
        self%nulls_dirty = .true.
    end procedure adopt_ts
    !
    module procedure parquet_column_data_ptr_ts
        call parquet_column_check_kind(col, PK_TIMESTAMP, "data_ptr")
        p => col%ts(1:col%nrows)
    end procedure parquet_column_data_ptr_ts
    !
    module procedure data_ptr_ts
        call parquet_column_data_ptr_ts(self, p)
    end procedure data_ptr_ts
    !
    module procedure parquet_column_get_at_i32v
        call parquet_column_check_kind(col, PK_INT32_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%i32v(:, i)
    end procedure parquet_column_get_at_i32v
    !
    module procedure get_at_i32v
        call parquet_column_get_at_i32v(self, i, value)
    end procedure get_at_i32v
    !
    module procedure parquet_column_set_at_i32v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_INT32_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%i32v(e, i) = value(e)
            end do
            return
        end if
        col%i32v(:, i) = value
        if (col%has_nulls) then
            base = (i - 1_int64)*int(col%width, int64)
            do e = 1_int64, int(col%width, int64)
                call bit_clear(col%validity, base + e)
            end do
        end if
    end procedure parquet_column_set_at_i32v
    !
    module procedure set_at_i32v
        call parquet_column_set_at_i32v(self, i, value, modify_nulls)
    end procedure set_at_i32v
    !
    module procedure parquet_column_get_elem_i32v
        call parquet_column_check_kind(col, PK_INT32_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%i32v(e, i)
    end procedure parquet_column_get_elem_i32v
    !
    module procedure get_elem_i32v
        call parquet_column_get_elem_i32v(self, i, e, value)
    end procedure get_elem_i32v
    !
    module procedure parquet_column_set_elem_i32v
        call parquet_column_check_kind(col, PK_INT32_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%i32v(e, i) = value
        if (col%has_nulls) call bit_clear(col%validity, (i - 1_int64)*int(col%width, int64) + e)
    end procedure parquet_column_set_elem_i32v
    !
    module procedure set_elem_i32v
        call parquet_column_set_elem_i32v(self, i, e, value)
    end procedure set_elem_i32v
    !
    module procedure set_all_i32v
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT32_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%i32v is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%i32v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%i32v(e, k) = values(e, k)
                end do
            end do
        end if
    end procedure set_all_i32v
    !
    module procedure adopt_i32v
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_INT32_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%i32v)
    end procedure adopt_i32v
    !
    module procedure parquet_column_data_ptr_i32v
        call parquet_column_check_kind(col, PK_INT32_VEC, "data_ptr")
        p => col%i32v(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_i32v
    !
    module procedure data_ptr_i32v
        call parquet_column_data_ptr_i32v(self, p)
    end procedure data_ptr_i32v
    !
    module procedure parquet_column_get_at_i64v
        call parquet_column_check_kind(col, PK_INT64_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%i64v(:, i)
    end procedure parquet_column_get_at_i64v
    !
    module procedure get_at_i64v
        call parquet_column_get_at_i64v(self, i, value)
    end procedure get_at_i64v
    !
    module procedure parquet_column_set_at_i64v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_INT64_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%i64v(e, i) = value(e)
            end do
            return
        end if
        col%i64v(:, i) = value
        if (col%has_nulls) then
            base = (i - 1_int64)*int(col%width, int64)
            do e = 1_int64, int(col%width, int64)
                call bit_clear(col%validity, base + e)
            end do
        end if
    end procedure parquet_column_set_at_i64v
    !
    module procedure set_at_i64v
        call parquet_column_set_at_i64v(self, i, value, modify_nulls)
    end procedure set_at_i64v
    !
    module procedure parquet_column_get_elem_i64v
        call parquet_column_check_kind(col, PK_INT64_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%i64v(e, i)
    end procedure parquet_column_get_elem_i64v
    !
    module procedure get_elem_i64v
        call parquet_column_get_elem_i64v(self, i, e, value)
    end procedure get_elem_i64v
    !
    module procedure parquet_column_set_elem_i64v
        call parquet_column_check_kind(col, PK_INT64_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%i64v(e, i) = value
        if (col%has_nulls) call bit_clear(col%validity, (i - 1_int64)*int(col%width, int64) + e)
    end procedure parquet_column_set_elem_i64v
    !
    module procedure set_elem_i64v
        call parquet_column_set_elem_i64v(self, i, e, value)
    end procedure set_elem_i64v
    !
    module procedure set_all_i64v
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_INT64_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%i64v is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%i64v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%i64v(e, k) = values(e, k)
                end do
            end do
        end if
    end procedure set_all_i64v
    !
    module procedure adopt_i64v
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_INT64_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%i64v)
    end procedure adopt_i64v
    !
    module procedure parquet_column_data_ptr_i64v
        call parquet_column_check_kind(col, PK_INT64_VEC, "data_ptr")
        p => col%i64v(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_i64v
    !
    module procedure data_ptr_i64v
        call parquet_column_data_ptr_i64v(self, p)
    end procedure data_ptr_i64v
    !
    module procedure parquet_column_get_at_f32v
        call parquet_column_check_kind(col, PK_FLOAT32_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%f32v(:, i)
    end procedure parquet_column_get_at_f32v
    !
    module procedure get_at_f32v
        call parquet_column_get_at_f32v(self, i, value)
    end procedure get_at_f32v
    !
    module procedure parquet_column_set_at_f32v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_FLOAT32_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%f32v(e, i) = value(e)
            end do
            return
        end if
        col%f32v(:, i) = value
        if (col%has_nulls) then
            base = (i - 1_int64)*int(col%width, int64)
            do e = 1_int64, int(col%width, int64)
                call bit_clear(col%validity, base + e)
            end do
        end if
    end procedure parquet_column_set_at_f32v
    !
    module procedure set_at_f32v
        call parquet_column_set_at_f32v(self, i, value, modify_nulls)
    end procedure set_at_f32v
    !
    module procedure parquet_column_get_elem_f32v
        call parquet_column_check_kind(col, PK_FLOAT32_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%f32v(e, i)
    end procedure parquet_column_get_elem_f32v
    !
    module procedure get_elem_f32v
        call parquet_column_get_elem_f32v(self, i, e, value)
    end procedure get_elem_f32v
    !
    module procedure parquet_column_set_elem_f32v
        call parquet_column_check_kind(col, PK_FLOAT32_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%f32v(e, i) = value
        if (col%has_nulls) call bit_clear(col%validity, (i - 1_int64)*int(col%width, int64) + e)
    end procedure parquet_column_set_elem_f32v
    !
    module procedure set_elem_f32v
        call parquet_column_set_elem_f32v(self, i, e, value)
    end procedure set_elem_f32v
    !
    module procedure set_all_f32v
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT32_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%f32v is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%f32v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%f32v(e, k) = values(e, k)
                end do
            end do
        end if
    end procedure set_all_f32v
    !
    module procedure adopt_f32v
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_FLOAT32_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%f32v)
    end procedure adopt_f32v
    !
    module procedure parquet_column_data_ptr_f32v
        call parquet_column_check_kind(col, PK_FLOAT32_VEC, "data_ptr")
        p => col%f32v(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_f32v
    !
    module procedure data_ptr_f32v
        call parquet_column_data_ptr_f32v(self, p)
    end procedure data_ptr_f32v
    !
    module procedure parquet_column_get_at_f64v
        call parquet_column_check_kind(col, PK_FLOAT64_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%f64v(:, i)
    end procedure parquet_column_get_at_f64v
    !
    module procedure get_at_f64v
        call parquet_column_get_at_f64v(self, i, value)
    end procedure get_at_f64v
    !
    module procedure parquet_column_set_at_f64v
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_FLOAT64_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%f64v(e, i) = value(e)
            end do
            return
        end if
        col%f64v(:, i) = value
        if (col%has_nulls) then
            base = (i - 1_int64)*int(col%width, int64)
            do e = 1_int64, int(col%width, int64)
                call bit_clear(col%validity, base + e)
            end do
        end if
    end procedure parquet_column_set_at_f64v
    !
    module procedure set_at_f64v
        call parquet_column_set_at_f64v(self, i, value, modify_nulls)
    end procedure set_at_f64v
    !
    module procedure parquet_column_get_elem_f64v
        call parquet_column_check_kind(col, PK_FLOAT64_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%f64v(e, i)
    end procedure parquet_column_get_elem_f64v
    !
    module procedure get_elem_f64v
        call parquet_column_get_elem_f64v(self, i, e, value)
    end procedure get_elem_f64v
    !
    module procedure parquet_column_set_elem_f64v
        call parquet_column_check_kind(col, PK_FLOAT64_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%f64v(e, i) = value
        if (col%has_nulls) call bit_clear(col%validity, (i - 1_int64)*int(col%width, int64) + e)
    end procedure parquet_column_set_elem_f64v
    !
    module procedure set_elem_f64v
        call parquet_column_set_elem_f64v(self, i, e, value)
    end procedure set_elem_f64v
    !
    module procedure set_all_f64v
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_FLOAT64_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%f64v is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%f64v(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%f64v(e, k) = values(e, k)
                end do
            end do
        end if
    end procedure set_all_f64v
    !
    module procedure adopt_f64v
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_FLOAT64_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%f64v)
    end procedure adopt_f64v
    !
    module procedure parquet_column_data_ptr_f64v
        call parquet_column_check_kind(col, PK_FLOAT64_VEC, "data_ptr")
        p => col%f64v(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_f64v
    !
    module procedure data_ptr_f64v
        call parquet_column_data_ptr_f64v(self, p)
    end procedure data_ptr_f64v
    !
    module procedure parquet_column_get_at_boolv
        call parquet_column_check_kind(col, PK_LOGICAL_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%boolv(:, i)
    end procedure parquet_column_get_at_boolv
    !
    module procedure get_at_boolv
        call parquet_column_get_at_boolv(self, i, value)
    end procedure get_at_boolv
    !
    module procedure parquet_column_set_at_boolv
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_LOGICAL_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%boolv(e, i) = value(e)
            end do
            return
        end if
        col%boolv(:, i) = value
        if (col%has_nulls) then
            base = (i - 1_int64)*int(col%width, int64)
            do e = 1_int64, int(col%width, int64)
                call bit_clear(col%validity, base + e)
            end do
        end if
    end procedure parquet_column_set_at_boolv
    !
    module procedure set_at_boolv
        call parquet_column_set_at_boolv(self, i, value, modify_nulls)
    end procedure set_at_boolv
    !
    module procedure parquet_column_get_elem_boolv
        call parquet_column_check_kind(col, PK_LOGICAL_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%boolv(e, i)
    end procedure parquet_column_get_elem_boolv
    !
    module procedure get_elem_boolv
        call parquet_column_get_elem_boolv(self, i, e, value)
    end procedure get_elem_boolv
    !
    module procedure parquet_column_set_elem_boolv
        call parquet_column_check_kind(col, PK_LOGICAL_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%boolv(e, i) = value
        if (col%has_nulls) call bit_clear(col%validity, (i - 1_int64)*int(col%width, int64) + e)
    end procedure parquet_column_set_elem_boolv
    !
    module procedure set_elem_boolv
        call parquet_column_set_elem_boolv(self, i, e, value)
    end procedure set_elem_boolv
    !
    module procedure set_all_boolv
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_LOGICAL_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%boolv is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%boolv(:, 1:self%nrows) = values
            call drop_bitmap(self)
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%boolv(e, k) = values(e, k)
                end do
            end do
        end if
    end procedure set_all_boolv
    !
    module procedure adopt_boolv
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_LOGICAL_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%boolv)
    end procedure adopt_boolv
    !
    module procedure parquet_column_data_ptr_boolv
        call parquet_column_check_kind(col, PK_LOGICAL_VEC, "data_ptr")
        p => col%boolv(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_boolv
    !
    module procedure data_ptr_boolv
        call parquet_column_data_ptr_boolv(self, p)
    end procedure data_ptr_boolv
    !
    module procedure parquet_column_get_at_datev
        call parquet_column_check_kind(col, PK_DATE_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%dtv(:, i)
    end procedure parquet_column_get_at_datev
    !
    module procedure get_at_datev
        call parquet_column_get_at_datev(self, i, value)
    end procedure get_at_datev
    !
    module procedure parquet_column_set_at_datev
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_DATE_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%dtv(e, i) = value(e)
            end do
            return
        end if
        col%dtv(:, i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_at_datev
    !
    module procedure set_at_datev
        call parquet_column_set_at_datev(self, i, value, modify_nulls)
    end procedure set_at_datev
    !
    module procedure parquet_column_get_elem_datev
        call parquet_column_check_kind(col, PK_DATE_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%dtv(e, i)
    end procedure parquet_column_get_elem_datev
    !
    module procedure get_elem_datev
        call parquet_column_get_elem_datev(self, i, e, value)
    end procedure get_elem_datev
    !
    module procedure parquet_column_set_elem_datev
        call parquet_column_check_kind(col, PK_DATE_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%dtv(e, i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_elem_datev
    !
    module procedure set_elem_datev
        call parquet_column_set_elem_datev(self, i, e, value)
    end procedure set_elem_datev
    !
    module procedure set_all_datev
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_DATE_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%dtv is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%dtv(:, 1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%dtv(e, k) = values(e, k)
                end do
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_datev
    !
    module procedure adopt_datev
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_DATE_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%dtv)
        self%nulls_dirty = .true.
    end procedure adopt_datev
    !
    module procedure parquet_column_data_ptr_datev
        call parquet_column_check_kind(col, PK_DATE_VEC, "data_ptr")
        p => col%dtv(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_datev
    !
    module procedure data_ptr_datev
        call parquet_column_data_ptr_datev(self, p)
    end procedure data_ptr_datev
    !
    module procedure parquet_column_get_at_timev
        call parquet_column_check_kind(col, PK_TIME_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%tmv(:, i)
    end procedure parquet_column_get_at_timev
    !
    module procedure get_at_timev
        call parquet_column_get_at_timev(self, i, value)
    end procedure get_at_timev
    !
    module procedure parquet_column_set_at_timev
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_TIME_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%tmv(e, i) = value(e)
            end do
            return
        end if
        col%tmv(:, i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_at_timev
    !
    module procedure set_at_timev
        call parquet_column_set_at_timev(self, i, value, modify_nulls)
    end procedure set_at_timev
    !
    module procedure parquet_column_get_elem_timev
        call parquet_column_check_kind(col, PK_TIME_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%tmv(e, i)
    end procedure parquet_column_get_elem_timev
    !
    module procedure get_elem_timev
        call parquet_column_get_elem_timev(self, i, e, value)
    end procedure get_elem_timev
    !
    module procedure parquet_column_set_elem_timev
        call parquet_column_check_kind(col, PK_TIME_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%tmv(e, i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_elem_timev
    !
    module procedure set_elem_timev
        call parquet_column_set_elem_timev(self, i, e, value)
    end procedure set_elem_timev
    !
    module procedure set_all_timev
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIME_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%tmv is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%tmv(:, 1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%tmv(e, k) = values(e, k)
                end do
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_timev
    !
    module procedure adopt_timev
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_TIME_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%tmv)
        self%nulls_dirty = .true.
    end procedure adopt_timev
    !
    module procedure parquet_column_data_ptr_timev
        call parquet_column_check_kind(col, PK_TIME_VEC, "data_ptr")
        p => col%tmv(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_timev
    !
    module procedure data_ptr_timev
        call parquet_column_data_ptr_timev(self, p)
    end procedure data_ptr_timev
    !
    module procedure parquet_column_get_at_tsv
        call parquet_column_check_kind(col, PK_TIMESTAMP_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        value = col%tsv(:, i)
    end procedure parquet_column_get_at_tsv
    !
    module procedure get_at_tsv
        call parquet_column_get_at_tsv(self, i, value)
    end procedure get_at_tsv
    !
    module procedure parquet_column_set_at_tsv
        logical :: mod_nulls
        integer(int64) :: e, base
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_TIMESTAMP_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: every
        ! element whose own bit is clear is written, and the null ones are left as they are.
        if (.not. mod_nulls) then
            base = int(col%width, int64)
            do e = 1_int64, base
                if (parquet_column_is_null(col, i, e)) cycle
                col%tsv(e, i) = value(e)
            end do
            return
        end if
        col%tsv(:, i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_at_tsv
    !
    module procedure set_at_tsv
        call parquet_column_set_at_tsv(self, i, value, modify_nulls)
    end procedure set_at_tsv
    !
    module procedure parquet_column_get_elem_tsv
        call parquet_column_check_kind(col, PK_TIMESTAMP_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        value = col%tsv(e, i)
    end procedure parquet_column_get_elem_tsv
    !
    module procedure get_elem_tsv
        call parquet_column_get_elem_tsv(self, i, e, value)
    end procedure get_elem_tsv
    !
    module procedure parquet_column_set_elem_tsv
        call parquet_column_check_kind(col, PK_TIMESTAMP_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        col%tsv(e, i) = value
        col%nulls_dirty = .true.
    end procedure parquet_column_set_elem_tsv
    !
    module procedure set_elem_tsv
        call parquet_column_set_elem_tsv(self, i, e, value)
    end procedure set_elem_tsv
    !
    module procedure set_all_tsv
        logical :: mod_nulls
        integer(int64) :: k, e
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_TIMESTAMP_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        if (mod_nulls) then
            ! Guarded on nrows: a ZERO-ROW column has no storage at all (`grow_storage` returns
            ! early at n == 0), so this whole-array assignment would reference an unallocated
            ! allocatable. The section is empty either way, but referencing an unallocated
            ! allocatable is not conforming -- nagfor's -C=array rejects it at run time
            ! ("ALLOCATABLE SELF%tsv is not currently allocated") while gfortran no-ops silently.
            ! Reached by the documented `%add_column(name, empty)` shape that declares a column
            ! before a parallel region appends to it.
            if (self%nrows > 0_int64) self%tsv(:, 1:self%nrows) = values
            self%nulls_dirty = .true.
        else
            ! Per ELEMENT, not per row -- see this file's header. A row with one null element
            ! still has its other elements written.
            do k = 1_int64, self%nrows
                do e = 1_int64, int(self%width, int64)
                    if (self%is_null(k, e)) cycle
                    self%tsv(e, k) = values(e, k)
                end do
            end do
            self%nulls_dirty = .true.
        end if
    end procedure set_all_tsv
    !
    module procedure adopt_tsv
        if (.not. allocated(values)) error stop EP//"adopt: the array to adopt is not allocated"
        if (size(values, 1) < 2) error stop EP//"adopt: a vector kind requires width > 1"
        call self%clear()
        self%kind = PK_TIMESTAMP_VEC
        self%width = int(size(values, 1), int32)
        self%nrows = size(values, 2, kind=int64)
        ! See the scalar adopt above: the adopted allocation IS the capacity.
        self%cap = self%nrows
        if (present(unit)) then
            if (len_trim(unit) > 0) self%unit = trim(unit)
        end if
        ! See the scalar sibling: move_alloc hands the allocation over rather than copying it.
        call move_alloc(values, self%tsv)
        self%nulls_dirty = .true.
    end procedure adopt_tsv
    !
    module procedure parquet_column_data_ptr_tsv
        call parquet_column_check_kind(col, PK_TIMESTAMP_VEC, "data_ptr")
        p => col%tsv(:, 1:col%nrows)
    end procedure parquet_column_data_ptr_tsv
    !
    module procedure data_ptr_tsv
        call parquet_column_data_ptr_tsv(self, p)
    end procedure data_ptr_tsv
    !
end submodule parquet_columns_access ! GCOVR_EXCL_LINE
