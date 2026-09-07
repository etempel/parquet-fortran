!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_columns.py
! The kind table lives in that script; edit it there, not here.
!
!> Per-kind value append for `parquet_column`, plus the four kind-dispatched storage
!! helpers (`gather_storage`, `grow_storage`, `copy_storage`, `paste_storage`) that the
!! hand-written structural operations in `parquet_columns_structural` are built on -- so
!! `reindex`, `delete_by_mask`, `append`, `append_nulls`, `paste` and `deep_copy` each exist
!! ONCE, kind-agnostically, instead of eighteen times.
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
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! The container rebuilds itself: only it knows how a row is laid out. `cap` below is
            ! then meaningless for this kind (the container carries its own), but setting it keeps
            ! the cap >= nrows invariant that %capacity and ensure_capacity both read.
            call self%container%gather_rows(idx)
        case default
            error stop EP//"gather_storage: column has no active storage"
        end select
        ! A rebuild allocates EXACT-FIT and says so. Only grow_storage ever creates slack, which is
        ! what makes %shrink_to_fit (and parquet_table's %compact) a no-op on any column that has
        ! not been appended to -- so a %filter_rows or a %sort_by hands the memory back on its own
        ! rather than waiting for a call the caller may never make.
        self%cap = n
    end procedure gather_storage
    !
    module procedure gather_storage_from
        integer(int64) :: k, e
        if (hi < lo) return
        select case (src%kind)
        case (PK_INT32)
            do k = lo, hi
                self%i32(k) = src%i32(idx(k))
            end do
        case (PK_INT64)
            do k = lo, hi
                self%i64(k) = src%i64(idx(k))
            end do
        case (PK_FLOAT32)
            do k = lo, hi
                self%f32(k) = src%f32(idx(k))
            end do
        case (PK_FLOAT64)
            do k = lo, hi
                self%f64(k) = src%f64(idx(k))
            end do
        case (PK_LOGICAL)
            do k = lo, hi
                self%bool(k) = src%bool(idx(k))
            end do
        case (PK_DATE)
            do k = lo, hi
                self%dt(k) = src%dt(idx(k))
            end do
            if (present(valid)) then
                do k = lo, hi
                    if (.not. valid(k)) call self%dt(k)%set_null()
                end do
            end if
        case (PK_TIME)
            do k = lo, hi
                self%tm(k) = src%tm(idx(k))
            end do
            if (present(valid)) then
                do k = lo, hi
                    if (.not. valid(k)) call self%tm(k)%set_null()
                end do
            end if
        case (PK_TIMESTAMP)
            do k = lo, hi
                self%ts(k) = src%ts(idx(k))
            end do
            if (present(valid)) then
                do k = lo, hi
                    if (.not. valid(k)) call self%ts(k)%set_null()
                end do
            end if
        case (PK_INT32_VEC)
            do k = lo, hi
                self%i32v(:, k) = src%i32v(:, idx(k))
            end do
        case (PK_INT64_VEC)
            do k = lo, hi
                self%i64v(:, k) = src%i64v(:, idx(k))
            end do
        case (PK_FLOAT32_VEC)
            do k = lo, hi
                self%f32v(:, k) = src%f32v(:, idx(k))
            end do
        case (PK_FLOAT64_VEC)
            do k = lo, hi
                self%f64v(:, k) = src%f64v(:, idx(k))
            end do
        case (PK_LOGICAL_VEC)
            do k = lo, hi
                self%boolv(:, k) = src%boolv(:, idx(k))
            end do
        case (PK_DATE_VEC)
            do k = lo, hi
                self%dtv(:, k) = src%dtv(:, idx(k))
            end do
            if (present(valid)) then
                do k = lo, hi
                    if (valid(k)) cycle
                    do e = 1_int64, int(self%width, int64)
                        call self%dtv(e, k)%set_null()
                    end do
                end do
            end if
        case (PK_TIME_VEC)
            do k = lo, hi
                self%tmv(:, k) = src%tmv(:, idx(k))
            end do
            if (present(valid)) then
                do k = lo, hi
                    if (valid(k)) cycle
                    do e = 1_int64, int(self%width, int64)
                        call self%tmv(e, k)%set_null()
                    end do
                end do
            end if
        case (PK_TIMESTAMP_VEC)
            do k = lo, hi
                self%tsv(:, k) = src%tsv(:, idx(k))
            end do
            if (present(valid)) then
                do k = lo, hi
                    if (valid(k)) cycle
                    do e = 1_int64, int(self%width, int64)
                        call self%tsv(e, k)%set_null()
                    end do
                end do
            end if
        case (PK_STRING, PK_STRING_VEC)
            ! The string store is rebuilt whole by its own gather_from (gather_build hands it the
            ! element list); kept so this select stays exhaustive over every kind, like its
            ! siblings above.
            error stop EP//"gather_storage_from: the string kinds are gathered by their store" ! GCOVR_EXCL_LINE
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Refused by gather_from before anything is sized: only the container knows how a row
            ! is laid out, and it rebuilds itself in place through %gather.
            error stop EP//"gather_storage_from: a container column is gathered in place" ! GCOVR_EXCL_LINE
        case default
            error stop EP//"gather_storage_from: column has no active storage" ! GCOVR_EXCL_LINE
        end select
    end procedure gather_storage_from
    !
    module procedure ensure_capacity
#ifdef UNDEFINED_CHECK
        use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
#endif
        integer(int64) :: old, newcap
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
#ifdef UNDEFINED_CHECK
        ! Quiet NaNs, and `ieee_value` rather than a TRANSFER of a bit pattern: nagfor
        ! CONSTANT-FOLDS a transfer and then refuses the result outright (`Error: Result of
        ! TRANSFER is Not a Number`) -- as a parameter AND as a runtime local. Creating a NaN
        ! raises no flag; arithmetic and ordered comparison on it do, which is the signal.
        real(real32) :: undef_nan32
        real(real64) :: undef_nan64
#endif
        if (need_rows <= self%cap) return
        old = self%nrows
        ! Geometric, not exact-fit: see the interface's own doc-comment for why, and why the
        ! `cap + cap/2` form is the one to keep.
        newcap = max(need_rows, self%cap + self%cap/2_int64)
#ifdef UNDEFINED_CHECK
        undef_nan32 = ieee_value(0.0_real32, ieee_quiet_nan)
        undef_nan64 = ieee_value(0.0_real64, ieee_quiet_nan)
#endif
        ! The `#ifdef UNDEFINED_CHECK` fills below exist for ONE build: nagfor's `-C=undefined`
        ! (fpm.toml's `nagundef` profile). `%init`'s contract leaves a null row's value bytes
        ! unspecified, so the copy in each arm reads bytes nothing ever wrote -- which that
        ! checker reports, correctly, and which used to abort the whole `columns` suite at test
        ! 28 of 74. This is the only place storage grows, so one fill site covers every kind.
        !
        ! POISON, not zero, and the difference is the whole point: defining these bytes makes
        ! the one instrument that could see an unsound read of them blind to it. A NaN or
        ! `huge` keeps such a read visible as a WRONG ANSWER rather than a plausible zero, so
        ! the class changes instrument instead of disappearing. Do not tidy these to 0/.false.
        ! The temporal kinds need no fill at all -- their types carry default component
        ! initializers, so allocation defines them.
        select case (self%kind)
        case (PK_INT32)
            allocate(tmp_i32(newcap))
            if (old > 0_int64) tmp_i32(1:old) = self%i32(1:old)
            call move_alloc(tmp_i32, self%i32)
#ifdef UNDEFINED_CHECK
            self%i32(old + 1_int64:) = huge(0_int32)
#endif
        case (PK_INT64)
            allocate(tmp_i64(newcap))
            if (old > 0_int64) tmp_i64(1:old) = self%i64(1:old)
            call move_alloc(tmp_i64, self%i64)
#ifdef UNDEFINED_CHECK
            self%i64(old + 1_int64:) = huge(0_int64)
#endif
        case (PK_FLOAT32)
            allocate(tmp_f32(newcap))
            if (old > 0_int64) tmp_f32(1:old) = self%f32(1:old)
            call move_alloc(tmp_f32, self%f32)
#ifdef UNDEFINED_CHECK
            self%f32(old + 1_int64:) = undef_nan32
#endif
        case (PK_FLOAT64)
            allocate(tmp_f64(newcap))
            if (old > 0_int64) tmp_f64(1:old) = self%f64(1:old)
            call move_alloc(tmp_f64, self%f64)
#ifdef UNDEFINED_CHECK
            self%f64(old + 1_int64:) = undef_nan64
#endif
        case (PK_LOGICAL)
            allocate(tmp_bool(newcap))
            if (old > 0_int64) tmp_bool(1:old) = self%bool(1:old)
            call move_alloc(tmp_bool, self%bool)
#ifdef UNDEFINED_CHECK
            self%bool(old + 1_int64:) = .true.
#endif
        case (PK_DATE)
            allocate(tmp_dt(newcap))
            if (old > 0_int64) tmp_dt(1:old) = self%dt(1:old)
            call move_alloc(tmp_dt, self%dt)
        case (PK_TIME)
            allocate(tmp_tm(newcap))
            if (old > 0_int64) tmp_tm(1:old) = self%tm(1:old)
            call move_alloc(tmp_tm, self%tm)
        case (PK_TIMESTAMP)
            allocate(tmp_ts(newcap))
            if (old > 0_int64) tmp_ts(1:old) = self%ts(1:old)
            call move_alloc(tmp_ts, self%ts)
        case (PK_INT32_VEC)
            allocate(tmp_i32v(self%width, newcap))
            if (old > 0_int64) tmp_i32v(:, 1:old) = self%i32v(:, 1:old)
            call move_alloc(tmp_i32v, self%i32v)
#ifdef UNDEFINED_CHECK
            self%i32v(:, old + 1_int64:) = huge(0_int32)
#endif
        case (PK_INT64_VEC)
            allocate(tmp_i64v(self%width, newcap))
            if (old > 0_int64) tmp_i64v(:, 1:old) = self%i64v(:, 1:old)
            call move_alloc(tmp_i64v, self%i64v)
#ifdef UNDEFINED_CHECK
            self%i64v(:, old + 1_int64:) = huge(0_int64)
#endif
        case (PK_FLOAT32_VEC)
            allocate(tmp_f32v(self%width, newcap))
            if (old > 0_int64) tmp_f32v(:, 1:old) = self%f32v(:, 1:old)
            call move_alloc(tmp_f32v, self%f32v)
#ifdef UNDEFINED_CHECK
            self%f32v(:, old + 1_int64:) = undef_nan32
#endif
        case (PK_FLOAT64_VEC)
            allocate(tmp_f64v(self%width, newcap))
            if (old > 0_int64) tmp_f64v(:, 1:old) = self%f64v(:, 1:old)
            call move_alloc(tmp_f64v, self%f64v)
#ifdef UNDEFINED_CHECK
            self%f64v(:, old + 1_int64:) = undef_nan64
#endif
        case (PK_LOGICAL_VEC)
            allocate(tmp_boolv(self%width, newcap))
            if (old > 0_int64) tmp_boolv(:, 1:old) = self%boolv(:, 1:old)
            call move_alloc(tmp_boolv, self%boolv)
#ifdef UNDEFINED_CHECK
            self%boolv(:, old + 1_int64:) = .true.
#endif
        case (PK_DATE_VEC)
            allocate(tmp_dtv(self%width, newcap))
            if (old > 0_int64) tmp_dtv(:, 1:old) = self%dtv(:, 1:old)
            call move_alloc(tmp_dtv, self%dtv)
        case (PK_TIME_VEC)
            allocate(tmp_tmv(self%width, newcap))
            if (old > 0_int64) tmp_tmv(:, 1:old) = self%tmv(:, 1:old)
            call move_alloc(tmp_tmv, self%tmv)
        case (PK_TIMESTAMP_VEC)
            allocate(tmp_tsv(self%width, newcap))
            if (old > 0_int64) tmp_tsv(:, 1:old) = self%tsv(:, 1:old)
            call move_alloc(tmp_tsv, self%tsv)
        case (PK_STRING, PK_STRING_VEC)
            ! the string store carries its own capacity (parquet_strings' ensure_*_cap), so `cap`
            ! is meaningless here and %capacity/%reserve/%shrink_to_fit forward to it instead
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Same shape as the string kinds: the container owns its own capacity policy, so this
            ! is a request forwarded on rather than an allocation performed here.
            call self%container%reserve_rows(newcap)
        case default
            error stop EP//"ensure_capacity: column has no active storage"
        end select
        self%cap = newcap
    end procedure ensure_capacity
    !
    module procedure allocate_empty_storage
        ! Zero-sized, deliberately: `cap` is left alone, so %capacity() still answers 0 and
        ! size(storage) still equals cap. The point is only that the array EXISTS -- see the
        ! interface's own doc-comment for what references it when it does not.
        select case (self%kind)
        case (PK_INT32)
            if (.not. allocated(self%i32)) allocate(self%i32(0))
        case (PK_INT64)
            if (.not. allocated(self%i64)) allocate(self%i64(0))
        case (PK_FLOAT32)
            if (.not. allocated(self%f32)) allocate(self%f32(0))
        case (PK_FLOAT64)
            if (.not. allocated(self%f64)) allocate(self%f64(0))
        case (PK_LOGICAL)
            if (.not. allocated(self%bool)) allocate(self%bool(0))
        case (PK_DATE)
            if (.not. allocated(self%dt)) allocate(self%dt(0))
        case (PK_TIME)
            if (.not. allocated(self%tm)) allocate(self%tm(0))
        case (PK_TIMESTAMP)
            if (.not. allocated(self%ts)) allocate(self%ts(0))
        case (PK_INT32_VEC)
            if (.not. allocated(self%i32v)) allocate(self%i32v(self%width, 0))
        case (PK_INT64_VEC)
            if (.not. allocated(self%i64v)) allocate(self%i64v(self%width, 0))
        case (PK_FLOAT32_VEC)
            if (.not. allocated(self%f32v)) allocate(self%f32v(self%width, 0))
        case (PK_FLOAT64_VEC)
            if (.not. allocated(self%f64v)) allocate(self%f64v(self%width, 0))
        case (PK_LOGICAL_VEC)
            if (.not. allocated(self%boolv)) allocate(self%boolv(self%width, 0))
        case (PK_DATE_VEC)
            if (.not. allocated(self%dtv)) allocate(self%dtv(self%width, 0))
        case (PK_TIME_VEC)
            if (.not. allocated(self%tmv)) allocate(self%tmv(self%width, 0))
        case (PK_TIMESTAMP_VEC)
            if (.not. allocated(self%tsv)) allocate(self%tsv(self%width, 0))
        case default
            ! The string and container kinds own their storage through `str`/`container`, which
            ! `init`/`adopt_container` allocate; PK_NONE has no storage by definition. Nothing to
            ! do, and no error -- this is called unconditionally from `init`.
            continue ! GCOVR_EXCL_LINE -- gcov attribution artifact: a bare `continue` no-op
        end select
    end procedure allocate_empty_storage
    !
    module procedure shrink_storage
        integer(int64) :: n
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
        if (self%cap <= self%nrows) return
        n = self%nrows
        select case (self%kind)
        case (PK_INT32)
            allocate(tmp_i32(n))
            if (n > 0_int64) tmp_i32(1:n) = self%i32(1:n)
            call move_alloc(tmp_i32, self%i32)
        case (PK_INT64)
            allocate(tmp_i64(n))
            if (n > 0_int64) tmp_i64(1:n) = self%i64(1:n)
            call move_alloc(tmp_i64, self%i64)
        case (PK_FLOAT32)
            allocate(tmp_f32(n))
            if (n > 0_int64) tmp_f32(1:n) = self%f32(1:n)
            call move_alloc(tmp_f32, self%f32)
        case (PK_FLOAT64)
            allocate(tmp_f64(n))
            if (n > 0_int64) tmp_f64(1:n) = self%f64(1:n)
            call move_alloc(tmp_f64, self%f64)
        case (PK_LOGICAL)
            allocate(tmp_bool(n))
            if (n > 0_int64) tmp_bool(1:n) = self%bool(1:n)
            call move_alloc(tmp_bool, self%bool)
        case (PK_DATE)
            allocate(tmp_dt(n))
            if (n > 0_int64) tmp_dt(1:n) = self%dt(1:n)
            call move_alloc(tmp_dt, self%dt)
        case (PK_TIME)
            allocate(tmp_tm(n))
            if (n > 0_int64) tmp_tm(1:n) = self%tm(1:n)
            call move_alloc(tmp_tm, self%tm)
        case (PK_TIMESTAMP)
            allocate(tmp_ts(n))
            if (n > 0_int64) tmp_ts(1:n) = self%ts(1:n)
            call move_alloc(tmp_ts, self%ts)
        case (PK_INT32_VEC)
            allocate(tmp_i32v(self%width, n))
            if (n > 0_int64) tmp_i32v(:, 1:n) = self%i32v(:, 1:n)
            call move_alloc(tmp_i32v, self%i32v)
        case (PK_INT64_VEC)
            allocate(tmp_i64v(self%width, n))
            if (n > 0_int64) tmp_i64v(:, 1:n) = self%i64v(:, 1:n)
            call move_alloc(tmp_i64v, self%i64v)
        case (PK_FLOAT32_VEC)
            allocate(tmp_f32v(self%width, n))
            if (n > 0_int64) tmp_f32v(:, 1:n) = self%f32v(:, 1:n)
            call move_alloc(tmp_f32v, self%f32v)
        case (PK_FLOAT64_VEC)
            allocate(tmp_f64v(self%width, n))
            if (n > 0_int64) tmp_f64v(:, 1:n) = self%f64v(:, 1:n)
            call move_alloc(tmp_f64v, self%f64v)
        case (PK_LOGICAL_VEC)
            allocate(tmp_boolv(self%width, n))
            if (n > 0_int64) tmp_boolv(:, 1:n) = self%boolv(:, 1:n)
            call move_alloc(tmp_boolv, self%boolv)
        case (PK_DATE_VEC)
            allocate(tmp_dtv(self%width, n))
            if (n > 0_int64) tmp_dtv(:, 1:n) = self%dtv(:, 1:n)
            call move_alloc(tmp_dtv, self%dtv)
        case (PK_TIME_VEC)
            allocate(tmp_tmv(self%width, n))
            if (n > 0_int64) tmp_tmv(:, 1:n) = self%tmv(:, 1:n)
            call move_alloc(tmp_tmv, self%tmv)
        case (PK_TIMESTAMP_VEC)
            allocate(tmp_tsv(self%width, n))
            if (n > 0_int64) tmp_tsv(:, 1:n) = self%tsv(:, 1:n)
            call move_alloc(tmp_tsv, self%tsv)
        case default
            ! Nothing to shrink: the string kinds carry capacity in their own store, and a column
            ! with no kind has no storage. Neither is an error -- %shrink_to_fit is a request to
            ! release what can be released, not an assertion that there is something to release.
            return
        end select
        self%cap = n
    end procedure shrink_storage
    !
    module procedure grow_storage
        if (n < 0_int64) error stop EP//"grow_storage: negative row count"
        if (n == 0_int64) return
        ! The container kinds fork HERE rather than gaining an arm inside ensure_capacity, because
        ! the two operations are genuinely different and only share a caller: ensure_capacity
        ! RESERVES room for rows that do not exist yet, while this has to APPEND n null rows.
        ! Routing a container through ensure_capacity would reserve and then advance `nrows` past
        ! rows the container had never created.
        if (parquet_kind_is_container(self%kind)) then
            call self%container%grow_rows(n)
            self%nrows = self%nrows + n
            self%cap = max(self%cap, self%nrows)
            return
        end if
        ! Two lines on top of ensure_capacity, which is the only place capacity grows. The string
        ! kinds reach here too and ensure_capacity is a no-op for them -- their storage grows
        ! through parquet_string_column's own append path (DD1) -- but nrows must still advance.
        call ensure_capacity(self, self%nrows + n)
        self%nrows = self%nrows + n
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
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! DELIBERATELY an abort rather than an arm. copy_storage fills a destination that
            ! `deep_copy` has already sized with `out%init(...)` -- which refuses a container kind,
            ! so `deep_copy` forks to `clone_into` well before reaching here. Anything that DOES
            ! reach here is a caller that has not been taught about containers, and a silent wrong
            ! copy cannot be taken back where a loud abort can. Add an arm if a real caller turns
            ! up; do not add one speculatively.
            error stop EP//"copy_storage: a container column is copied by clone_into, not here"
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
            call parquet_string_column_append_column(self%str, other%str)
            self%nrows = old + n
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Delegated whole, exactly as gather_storage delegates: only the concrete type knows
            ! how a row is laid out, and the offsets have to be rebased onto this column's own
            ! element count rather than copied. `other`'s container is guaranteed allocated by
            ! the kind check the caller has already made -- a PK_LIST column without one cannot
            ! exist, since adopt_container is the only writer of a container kind.
            call self%container%append_from(other%container)
            ! As for the string kinds: this column's own row count is not advanced by
            ! grow_storage here, so it is set from the container that has just grown. `cap` has
            ! to follow, or the cap >= nrows invariant that %capacity and ensure_capacity both
            ! read is broken by an append.
            self%nrows = self%container%nrows()
            self%cap = max(self%cap, self%nrows)
        case default
            error stop EP//"append_storage: column has no active storage"
        end select
    end procedure append_storage
    !
    module procedure append_row_of
        integer(int64) :: at, w, e, src_base, dst_base
        if (self%kind /= other%kind) error stop EP//"append_row_of: column kinds differ"
        if (self%width /= other%width) error stop EP//"append_row_of: column widths differ"
        if (irow < 1_int64 .or. irow > other%nrows) then
            error stop EP//"append_row_of: source row index out of range"
        end if
        w = int(self%width, int64)
        select case (self%kind)
        case (PK_INT32)
            call grow_storage(self, 1_int64)
            self%i32(self%nrows) = other%i32(irow)
        case (PK_INT64)
            call grow_storage(self, 1_int64)
            self%i64(self%nrows) = other%i64(irow)
        case (PK_FLOAT32)
            call grow_storage(self, 1_int64)
            self%f32(self%nrows) = other%f32(irow)
        case (PK_FLOAT64)
            call grow_storage(self, 1_int64)
            self%f64(self%nrows) = other%f64(irow)
        case (PK_LOGICAL)
            call grow_storage(self, 1_int64)
            self%bool(self%nrows) = other%bool(irow)
        case (PK_DATE)
            call grow_storage(self, 1_int64)
            self%dt(self%nrows) = other%dt(irow)
        case (PK_TIME)
            call grow_storage(self, 1_int64)
            self%tm(self%nrows) = other%tm(irow)
        case (PK_TIMESTAMP)
            call grow_storage(self, 1_int64)
            self%ts(self%nrows) = other%ts(irow)
        case (PK_INT32_VEC)
            call grow_storage(self, 1_int64)
            self%i32v(:, self%nrows) = other%i32v(:, irow)
        case (PK_INT64_VEC)
            call grow_storage(self, 1_int64)
            self%i64v(:, self%nrows) = other%i64v(:, irow)
        case (PK_FLOAT32_VEC)
            call grow_storage(self, 1_int64)
            self%f32v(:, self%nrows) = other%f32v(:, irow)
        case (PK_FLOAT64_VEC)
            call grow_storage(self, 1_int64)
            self%f64v(:, self%nrows) = other%f64v(:, irow)
        case (PK_LOGICAL_VEC)
            call grow_storage(self, 1_int64)
            self%boolv(:, self%nrows) = other%boolv(:, irow)
        case (PK_DATE_VEC)
            call grow_storage(self, 1_int64)
            self%dtv(:, self%nrows) = other%dtv(:, irow)
        case (PK_TIME_VEC)
            call grow_storage(self, 1_int64)
            self%tmv(:, self%nrows) = other%tmv(:, irow)
        case (PK_TIMESTAMP_VEC)
            call grow_storage(self, 1_int64)
            self%tsv(:, self%nrows) = other%tsv(:, irow)
        case (PK_STRING, PK_STRING_VEC)
            ! One flat store of nrows*width elements, row i at (i-1)*width + 1 .. i*width (RF6).
            ! append_from copies one element without materializing it as a Fortran string.
            do e = 1_int64, w
                call parquet_string_column_append_from(self%str, other%str, (irow - 1_int64)*w + e)
            end do
            self%nrows = self%nrows + 1_int64
            return
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Deferred for the same reason as append_storage above, and with the same reasoning
            ! for saying so explicitly rather than falling through to a message that is not true.
            error stop EP//"append_row_of: appending a container row is not implemented yet"
        case default
            error stop EP//"append_row_of: column has no active storage"
        end select
        ! Validity, element by element, exactly as `append` carries it for a whole column: the
        ! temporal kinds hold their null state inside the element (so the value copy above already
        ! moved it, and only the cache needs invalidating), while a bitmap kind has to copy bits --
        ! and only when the source row actually has one, so a null-free append allocates nothing.
        if (is_temporal_kind(self%kind)) then
            self%nulls_dirty = .true.
        else if (other%has_nulls) then
            src_base = (irow - 1_int64)*w
            dst_base = (self%nrows - 1_int64)*w
            do e = 1_int64, w
                if (bit_test(other%validity, src_base + e)) then
                    call ensure_bitmap(self)
                    call bit_set(self%validity, dst_base + e)
                end if
            end do
        end if
    end procedure append_row_of
    !
    module procedure paste_storage
        select case (self%kind)
        case (PK_INT32)
            self%i32(at:at+n-1_int64) = src%i32(from:from+n-1_int64)
        case (PK_INT64)
            self%i64(at:at+n-1_int64) = src%i64(from:from+n-1_int64)
        case (PK_FLOAT32)
            self%f32(at:at+n-1_int64) = src%f32(from:from+n-1_int64)
        case (PK_FLOAT64)
            self%f64(at:at+n-1_int64) = src%f64(from:from+n-1_int64)
        case (PK_LOGICAL)
            self%bool(at:at+n-1_int64) = src%bool(from:from+n-1_int64)
        case (PK_DATE)
            self%dt(at:at+n-1_int64) = src%dt(from:from+n-1_int64)
        case (PK_TIME)
            self%tm(at:at+n-1_int64) = src%tm(from:from+n-1_int64)
        case (PK_TIMESTAMP)
            self%ts(at:at+n-1_int64) = src%ts(from:from+n-1_int64)
        case (PK_INT32_VEC)
            self%i32v(:, at:at+n-1_int64) = src%i32v(:, from:from+n-1_int64)
        case (PK_INT64_VEC)
            self%i64v(:, at:at+n-1_int64) = src%i64v(:, from:from+n-1_int64)
        case (PK_FLOAT32_VEC)
            self%f32v(:, at:at+n-1_int64) = src%f32v(:, from:from+n-1_int64)
        case (PK_FLOAT64_VEC)
            self%f64v(:, at:at+n-1_int64) = src%f64v(:, from:from+n-1_int64)
        case (PK_LOGICAL_VEC)
            self%boolv(:, at:at+n-1_int64) = src%boolv(:, from:from+n-1_int64)
        case (PK_DATE_VEC)
            self%dtv(:, at:at+n-1_int64) = src%dtv(:, from:from+n-1_int64)
        case (PK_TIME_VEC)
            self%tmv(:, at:at+n-1_int64) = src%tmv(:, from:from+n-1_int64)
        case (PK_TIMESTAMP_VEC)
            self%tsv(:, at:at+n-1_int64) = src%tsv(:, from:from+n-1_int64)
        case (PK_STRING, PK_STRING_VEC)
            ! Unreachable through paste, which rejects the string kinds before it gets here --
            ! kept so this select is exhaustive over every storable kind, like its siblings above.
            error stop EP//"paste_storage: the string kinds cannot be overwritten in place" ! GCOVR_EXCL_LINE
        case (PK_LIST, PK_MAP, PK_STRUCT)
            ! Unreachable through paste, which rejects the container kinds before it gets here --
            ! kept so this select stays exhaustive over every kind, like its siblings above.
            error stop EP//"paste_storage: a container column cannot be overwritten in place" ! GCOVR_EXCL_LINE
        case default
            error stop EP//"paste_storage: column has no active storage"
        end select
    end procedure paste_storage
    !
end submodule parquet_columns_mutate ! GCOVR_EXCL_LINE
