!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_columns.py
! The kind table lives in that script; edit it there, not here.
!
!> Per-kind value append for `parquet_column`, plus the three kind-dispatched storage
!! helpers (`gather_storage`, `grow_storage`, `copy_storage`) that the hand-written structural
!! operations in `parquet_columns_structural` are built on -- so `reindex`, `delete_by_mask`,
!! `append`, `append_nulls` and `deep_copy` each exist ONCE, kind-agnostically, instead of
!! eighteen times.
submodule (parquet_columns) parquet_columns_mutate
    implicit none
contains
    !
    module procedure append_values_i32
        integer(int64) :: n, old
        call check_kind(self, PK_INT32, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%i32(old+1_int64:old+n) = values
    end procedure append_values_i32
    !
    module procedure append_values_i64
        integer(int64) :: n, old
        call check_kind(self, PK_INT64, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%i64(old+1_int64:old+n) = values
    end procedure append_values_i64
    !
    module procedure append_values_f32
        integer(int64) :: n, old
        call check_kind(self, PK_FLOAT32, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%f32(old+1_int64:old+n) = values
    end procedure append_values_f32
    !
    module procedure append_values_f64
        integer(int64) :: n, old
        call check_kind(self, PK_FLOAT64, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%f64(old+1_int64:old+n) = values
    end procedure append_values_f64
    !
    module procedure append_values_bool
        integer(int64) :: n, old
        call check_kind(self, PK_LOGICAL, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%bool(old+1_int64:old+n) = values
    end procedure append_values_bool
    !
    module procedure append_values_date
        integer(int64) :: n, old
        call check_kind(self, PK_DATE, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%dt(old+1_int64:old+n) = values
    end procedure append_values_date
    !
    module procedure append_values_time
        integer(int64) :: n, old
        call check_kind(self, PK_TIME, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%tm(old+1_int64:old+n) = values
    end procedure append_values_time
    !
    module procedure append_values_ts
        integer(int64) :: n, old
        call check_kind(self, PK_TIMESTAMP, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%ts(old+1_int64:old+n) = values
    end procedure append_values_ts
    !
    module procedure append_values_i32v
        integer(int64) :: n, old
        call check_kind(self, PK_INT32_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%i32v(:, old+1_int64:old+n) = values
    end procedure append_values_i32v
    !
    module procedure append_values_i64v
        integer(int64) :: n, old
        call check_kind(self, PK_INT64_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%i64v(:, old+1_int64:old+n) = values
    end procedure append_values_i64v
    !
    module procedure append_values_f32v
        integer(int64) :: n, old
        call check_kind(self, PK_FLOAT32_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%f32v(:, old+1_int64:old+n) = values
    end procedure append_values_f32v
    !
    module procedure append_values_f64v
        integer(int64) :: n, old
        call check_kind(self, PK_FLOAT64_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%f64v(:, old+1_int64:old+n) = values
    end procedure append_values_f64v
    !
    module procedure append_values_boolv
        integer(int64) :: n, old
        call check_kind(self, PK_LOGICAL_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%boolv(:, old+1_int64:old+n) = values
    end procedure append_values_boolv
    !
    module procedure append_values_datev
        integer(int64) :: n, old
        call check_kind(self, PK_DATE_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%dtv(:, old+1_int64:old+n) = values
    end procedure append_values_datev
    !
    module procedure append_values_timev
        integer(int64) :: n, old
        call check_kind(self, PK_TIME_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%tmv(:, old+1_int64:old+n) = values
    end procedure append_values_timev
    !
    module procedure append_values_tsv
        integer(int64) :: n, old
        call check_kind(self, PK_TIMESTAMP_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        old = self%nrows
        call grow_storage(self, n)
        self%tsv(:, old+1_int64:old+n) = values
    end procedure append_values_tsv
    !
    module procedure gather_storage
        integer(int64) :: n, k
        integer(int32), allocatable :: new_i32(:)
        integer(int64), allocatable :: new_i64(:)
        real(real32), allocatable :: new_f32(:)
        real(real64), allocatable :: new_f64(:)
        logical, allocatable :: new_bool(:)
        type(parquet_date), allocatable :: new_dt(:)
        type(parquet_time), allocatable :: new_tm(:)
        type(parquet_timestamp), allocatable :: new_ts(:)
        integer(int32), allocatable :: new_i32v(:,:)
        integer(int64), allocatable :: new_i64v(:,:)
        real(real32), allocatable :: new_f32v(:,:)
        real(real64), allocatable :: new_f64v(:,:)
        logical, allocatable :: new_boolv(:,:)
        type(parquet_date), allocatable :: new_dtv(:,:)
        type(parquet_time), allocatable :: new_tmv(:,:)
        type(parquet_timestamp), allocatable :: new_tsv(:,:)
        n = size(idx, kind=int64)
        select case (self%kind)
        case (PK_INT32)
            allocate(new_i32(max(n, 1_int64)))
            do k = 1_int64, n
                new_i32(k) = self%i32(idx(k))
            end do
            call move_alloc(new_i32, self%i32)
        case (PK_INT64)
            allocate(new_i64(max(n, 1_int64)))
            do k = 1_int64, n
                new_i64(k) = self%i64(idx(k))
            end do
            call move_alloc(new_i64, self%i64)
        case (PK_FLOAT32)
            allocate(new_f32(max(n, 1_int64)))
            do k = 1_int64, n
                new_f32(k) = self%f32(idx(k))
            end do
            call move_alloc(new_f32, self%f32)
        case (PK_FLOAT64)
            allocate(new_f64(max(n, 1_int64)))
            do k = 1_int64, n
                new_f64(k) = self%f64(idx(k))
            end do
            call move_alloc(new_f64, self%f64)
        case (PK_LOGICAL)
            allocate(new_bool(max(n, 1_int64)))
            do k = 1_int64, n
                new_bool(k) = self%bool(idx(k))
            end do
            call move_alloc(new_bool, self%bool)
        case (PK_DATE)
            allocate(new_dt(max(n, 1_int64)))
            do k = 1_int64, n
                new_dt(k) = self%dt(idx(k))
            end do
            call move_alloc(new_dt, self%dt)
        case (PK_TIME)
            allocate(new_tm(max(n, 1_int64)))
            do k = 1_int64, n
                new_tm(k) = self%tm(idx(k))
            end do
            call move_alloc(new_tm, self%tm)
        case (PK_TIMESTAMP)
            allocate(new_ts(max(n, 1_int64)))
            do k = 1_int64, n
                new_ts(k) = self%ts(idx(k))
            end do
            call move_alloc(new_ts, self%ts)
        case (PK_INT32_VEC)
            allocate(new_i32v(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_i32v(:, k) = self%i32v(:, idx(k))
            end do
            call move_alloc(new_i32v, self%i32v)
        case (PK_INT64_VEC)
            allocate(new_i64v(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_i64v(:, k) = self%i64v(:, idx(k))
            end do
            call move_alloc(new_i64v, self%i64v)
        case (PK_FLOAT32_VEC)
            allocate(new_f32v(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_f32v(:, k) = self%f32v(:, idx(k))
            end do
            call move_alloc(new_f32v, self%f32v)
        case (PK_FLOAT64_VEC)
            allocate(new_f64v(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_f64v(:, k) = self%f64v(:, idx(k))
            end do
            call move_alloc(new_f64v, self%f64v)
        case (PK_LOGICAL_VEC)
            allocate(new_boolv(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_boolv(:, k) = self%boolv(:, idx(k))
            end do
            call move_alloc(new_boolv, self%boolv)
        case (PK_DATE_VEC)
            allocate(new_dtv(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_dtv(:, k) = self%dtv(:, idx(k))
            end do
            call move_alloc(new_dtv, self%dtv)
        case (PK_TIME_VEC)
            allocate(new_tmv(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_tmv(:, k) = self%tmv(:, idx(k))
            end do
            call move_alloc(new_tmv, self%tmv)
        case (PK_TIMESTAMP_VEC)
            allocate(new_tsv(self%width, max(n, 1_int64)))
            do k = 1_int64, n
                new_tsv(:, k) = self%tsv(:, idx(k))
            end do
            call move_alloc(new_tsv, self%tsv)
        case (PK_STRING, PK_STRING_VEC)
            ! the string store is reordered by its own reindex/delete_by_mask (DD1)
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"gather_storage: column has no active storage"
        end select
    end procedure gather_storage
    !
    module procedure grow_storage
        integer(int64) :: old, new
        integer(int32), allocatable :: tmp_i32(:)
        integer(int64), allocatable :: tmp_i64(:)
        real(real32), allocatable :: tmp_f32(:)
        real(real64), allocatable :: tmp_f64(:)
        logical, allocatable :: tmp_bool(:)
        type(parquet_date), allocatable :: tmp_dt(:)
        type(parquet_time), allocatable :: tmp_tm(:)
        type(parquet_timestamp), allocatable :: tmp_ts(:)
        integer(int32), allocatable :: tmp_i32v(:,:)
        integer(int64), allocatable :: tmp_i64v(:,:)
        real(real32), allocatable :: tmp_f32v(:,:)
        real(real64), allocatable :: tmp_f64v(:,:)
        logical, allocatable :: tmp_boolv(:,:)
        type(parquet_date), allocatable :: tmp_dtv(:,:)
        type(parquet_time), allocatable :: tmp_tmv(:,:)
        type(parquet_timestamp), allocatable :: tmp_tsv(:,:)
        if (n < 0_int64) error stop EP//"grow_storage: negative row count"
        if (n == 0_int64) return
        old = self%nrows
        new = old + n
        select case (self%kind)
        case (PK_INT32)
            allocate(tmp_i32(new))
            if (old > 0_int64) tmp_i32(1:old) = self%i32(1:old)
            call move_alloc(tmp_i32, self%i32)
        case (PK_INT64)
            allocate(tmp_i64(new))
            if (old > 0_int64) tmp_i64(1:old) = self%i64(1:old)
            call move_alloc(tmp_i64, self%i64)
        case (PK_FLOAT32)
            allocate(tmp_f32(new))
            if (old > 0_int64) tmp_f32(1:old) = self%f32(1:old)
            call move_alloc(tmp_f32, self%f32)
        case (PK_FLOAT64)
            allocate(tmp_f64(new))
            if (old > 0_int64) tmp_f64(1:old) = self%f64(1:old)
            call move_alloc(tmp_f64, self%f64)
        case (PK_LOGICAL)
            allocate(tmp_bool(new))
            if (old > 0_int64) tmp_bool(1:old) = self%bool(1:old)
            call move_alloc(tmp_bool, self%bool)
        case (PK_DATE)
            allocate(tmp_dt(new))
            if (old > 0_int64) tmp_dt(1:old) = self%dt(1:old)
            call move_alloc(tmp_dt, self%dt)
        case (PK_TIME)
            allocate(tmp_tm(new))
            if (old > 0_int64) tmp_tm(1:old) = self%tm(1:old)
            call move_alloc(tmp_tm, self%tm)
        case (PK_TIMESTAMP)
            allocate(tmp_ts(new))
            if (old > 0_int64) tmp_ts(1:old) = self%ts(1:old)
            call move_alloc(tmp_ts, self%ts)
        case (PK_INT32_VEC)
            allocate(tmp_i32v(self%width, new))
            if (old > 0_int64) tmp_i32v(:, 1:old) = self%i32v(:, 1:old)
            call move_alloc(tmp_i32v, self%i32v)
        case (PK_INT64_VEC)
            allocate(tmp_i64v(self%width, new))
            if (old > 0_int64) tmp_i64v(:, 1:old) = self%i64v(:, 1:old)
            call move_alloc(tmp_i64v, self%i64v)
        case (PK_FLOAT32_VEC)
            allocate(tmp_f32v(self%width, new))
            if (old > 0_int64) tmp_f32v(:, 1:old) = self%f32v(:, 1:old)
            call move_alloc(tmp_f32v, self%f32v)
        case (PK_FLOAT64_VEC)
            allocate(tmp_f64v(self%width, new))
            if (old > 0_int64) tmp_f64v(:, 1:old) = self%f64v(:, 1:old)
            call move_alloc(tmp_f64v, self%f64v)
        case (PK_LOGICAL_VEC)
            allocate(tmp_boolv(self%width, new))
            if (old > 0_int64) tmp_boolv(:, 1:old) = self%boolv(:, 1:old)
            call move_alloc(tmp_boolv, self%boolv)
        case (PK_DATE_VEC)
            allocate(tmp_dtv(self%width, new))
            if (old > 0_int64) tmp_dtv(:, 1:old) = self%dtv(:, 1:old)
            call move_alloc(tmp_dtv, self%dtv)
        case (PK_TIME_VEC)
            allocate(tmp_tmv(self%width, new))
            if (old > 0_int64) tmp_tmv(:, 1:old) = self%tmv(:, 1:old)
            call move_alloc(tmp_tmv, self%tmv)
        case (PK_TIMESTAMP_VEC)
            allocate(tmp_tsv(self%width, new))
            if (old > 0_int64) tmp_tsv(:, 1:old) = self%tsv(:, 1:old)
            call move_alloc(tmp_tsv, self%tsv)
        case (PK_STRING, PK_STRING_VEC)
            ! the string store grows through its own append path (DD1)
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"grow_storage: column has no active storage"
        end select
        self%nrows = new
        if (self%has_nulls) call ensure_bitmap(self)
    end procedure grow_storage
    !
    module procedure copy_storage
        select case (self%kind)
        case (PK_INT32)
            if (allocated(self%i32)) out%i32(1:self%nrows) = self%i32(1:self%nrows)
        case (PK_INT64)
            if (allocated(self%i64)) out%i64(1:self%nrows) = self%i64(1:self%nrows)
        case (PK_FLOAT32)
            if (allocated(self%f32)) out%f32(1:self%nrows) = self%f32(1:self%nrows)
        case (PK_FLOAT64)
            if (allocated(self%f64)) out%f64(1:self%nrows) = self%f64(1:self%nrows)
        case (PK_LOGICAL)
            if (allocated(self%bool)) out%bool(1:self%nrows) = self%bool(1:self%nrows)
        case (PK_DATE)
            if (allocated(self%dt)) out%dt(1:self%nrows) = self%dt(1:self%nrows)
        case (PK_TIME)
            if (allocated(self%tm)) out%tm(1:self%nrows) = self%tm(1:self%nrows)
        case (PK_TIMESTAMP)
            if (allocated(self%ts)) out%ts(1:self%nrows) = self%ts(1:self%nrows)
        case (PK_INT32_VEC)
            if (allocated(self%i32v)) out%i32v(:, 1:self%nrows) = self%i32v(:, 1:self%nrows)
        case (PK_INT64_VEC)
            if (allocated(self%i64v)) out%i64v(:, 1:self%nrows) = self%i64v(:, 1:self%nrows)
        case (PK_FLOAT32_VEC)
            if (allocated(self%f32v)) out%f32v(:, 1:self%nrows) = self%f32v(:, 1:self%nrows)
        case (PK_FLOAT64_VEC)
            if (allocated(self%f64v)) out%f64v(:, 1:self%nrows) = self%f64v(:, 1:self%nrows)
        case (PK_LOGICAL_VEC)
            if (allocated(self%boolv)) out%boolv(:, 1:self%nrows) = self%boolv(:, 1:self%nrows)
        case (PK_DATE_VEC)
            if (allocated(self%dtv)) out%dtv(:, 1:self%nrows) = self%dtv(:, 1:self%nrows)
        case (PK_TIME_VEC)
            if (allocated(self%tmv)) out%tmv(:, 1:self%nrows) = self%tmv(:, 1:self%nrows)
        case (PK_TIMESTAMP_VEC)
            if (allocated(self%tsv)) out%tsv(:, 1:self%nrows) = self%tsv(:, 1:self%nrows)
        case (PK_STRING, PK_STRING_VEC)
            if (allocated(self%str)) then
                if (.not. allocated(out%str)) allocate(out%str)
                out%str = self%str
            end if
        case (PK_NONE)
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case default
            error stop EP//"copy_storage: column has no active storage"
        end select
    end procedure copy_storage
    !
    module procedure append_storage
        integer(int64) :: old, n
        if (self%kind /= other%kind) error stop EP//"append_storage: column kinds differ"
        if (self%width /= other%width) error stop EP//"append_storage: column widths differ"
        n = other%nrows
        if (n == 0_int64) return
        old = self%nrows
        select case (self%kind)
        case (PK_INT32)
            call grow_storage(self, n)
            self%i32(old+1_int64:old+n) = other%i32(1:n)
        case (PK_INT64)
            call grow_storage(self, n)
            self%i64(old+1_int64:old+n) = other%i64(1:n)
        case (PK_FLOAT32)
            call grow_storage(self, n)
            self%f32(old+1_int64:old+n) = other%f32(1:n)
        case (PK_FLOAT64)
            call grow_storage(self, n)
            self%f64(old+1_int64:old+n) = other%f64(1:n)
        case (PK_LOGICAL)
            call grow_storage(self, n)
            self%bool(old+1_int64:old+n) = other%bool(1:n)
        case (PK_DATE)
            call grow_storage(self, n)
            self%dt(old+1_int64:old+n) = other%dt(1:n)
        case (PK_TIME)
            call grow_storage(self, n)
            self%tm(old+1_int64:old+n) = other%tm(1:n)
        case (PK_TIMESTAMP)
            call grow_storage(self, n)
            self%ts(old+1_int64:old+n) = other%ts(1:n)
        case (PK_INT32_VEC)
            call grow_storage(self, n)
            self%i32v(:, old+1_int64:old+n) = other%i32v(:, 1:n)
        case (PK_INT64_VEC)
            call grow_storage(self, n)
            self%i64v(:, old+1_int64:old+n) = other%i64v(:, 1:n)
        case (PK_FLOAT32_VEC)
            call grow_storage(self, n)
            self%f32v(:, old+1_int64:old+n) = other%f32v(:, 1:n)
        case (PK_FLOAT64_VEC)
            call grow_storage(self, n)
            self%f64v(:, old+1_int64:old+n) = other%f64v(:, 1:n)
        case (PK_LOGICAL_VEC)
            call grow_storage(self, n)
            self%boolv(:, old+1_int64:old+n) = other%boolv(:, 1:n)
        case (PK_DATE_VEC)
            call grow_storage(self, n)
            self%dtv(:, old+1_int64:old+n) = other%dtv(:, 1:n)
        case (PK_TIME_VEC)
            call grow_storage(self, n)
            self%tmv(:, old+1_int64:old+n) = other%tmv(:, 1:n)
        case (PK_TIMESTAMP_VEC)
            call grow_storage(self, n)
            self%tsv(:, old+1_int64:old+n) = other%tsv(:, 1:n)
        case (PK_STRING, PK_STRING_VEC)
            call self%str%append_column(other%str)
            self%nrows = old + n
        case default
            error stop EP//"append_storage: column has no active storage"
        end select
    end procedure append_storage
    !
end submodule parquet_columns_mutate
