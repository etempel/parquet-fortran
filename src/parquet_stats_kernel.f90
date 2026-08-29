!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_stats.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> The per-kind entry points of `parquet_stats`.
!!
!! Each specific is the same shape: check the optional arguments against `values`, then walk the
!! array applying the family's exclusion order -- nullness, then NaN, then weight.
!!
!! **The guards these bodies call live in `parquet_stats_core`, not here**, because that submodule
!! needs them too and two sibling submodules cannot reach each other's contained procedures. Their
!! interfaces are declared in the module spec for exactly that reason; a private helper contained
!! directly in the module would compile and then fail to link (CLAUDE.md).
submodule (parquet_stats) parquet_stats_kernel
    implicit none

contains

    !> Widens a scalar numeric `parquet_column` into a `real64` population and its validity mask.
    !!
    !! Three refusals, and each is a wrong answer rather than an inconvenience if it is dropped:
    !!
    !! * a **string, temporal or container** column has no numeric statistics, and the message names
    !!   the kind it actually found;
    !! * a column **wider than one element** aborts, because flattening a width-16 column into one
    !!   population is a DIFFERENT statistic -- a caller who wants one element position across all
    !!   rows has `%get_elem`, and one who genuinely wants the flattened population can pass the
    !!   flattened array and thereby say so;
    !! * `is_valid=` **alongside** a column aborts, because the column carries its own validity and
    !!   two sources of truth that can disagree is a shape this repository has been bitten by.
    !!
    !! `mask` comes back UNALLOCATED for a null-free column, which is not a detail: an unallocated
    !! allocatable passed to an `optional` dummy is ABSENT, so the core takes its own no-mask fast
    !! path with no branch at the call site.
    !!
    !! The bulk read goes through `parquet_column_data_ptr` -- the typed accessor tier, never a
    !! type-bound procedure -- so ifx does not build a class descriptor per call. The scalar
    !! metadata queries below have no typed twin and are called once per column, which is the same
    !! trade `parquet_sorting` already makes for its key extraction.
    subroutine col_to_real64(col, what, is_valid, v, mask)
        type(parquet_column), intent(in), target :: col
        !! the column. `target` because `parquet_column_data_ptr` requires it; the pointer never
        !! leaves this procedure, so the caller's actual argument needs no `target` of its own.
        character(len=*), intent(in) :: what                  !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)          !! must be absent; see above.
        real(real64), allocatable, intent(out) :: v(:)        !! the widened population.
        logical, allocatable, intent(out) :: mask(:)          !! its validity, or unallocated.
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        real(real32), pointer :: r32(:)
        real(real64), pointer :: r64(:)
        logical, pointer :: pb(:)
        character(len=:), allocatable :: kname
        integer :: k
        integer(int64) :: n

        if (present(is_valid)) error stop what // ": is_valid= cannot be given alongside a " // &
            "parquet_column; the column carries its own validity, and two sources that can " // &
            "disagree is exactly what this refuses"
        k = col%kindof()
        select case (k)
        case (PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL)
            continue
        case (PK_INT32_VEC, PK_INT64_VEC, PK_FLOAT32_VEC, PK_FLOAT64_VEC, PK_LOGICAL_VEC)
            ! The VECTOR kinds get their own arm, and the message names the width rather than the
            ! kind, because "a column of kind PK_INT32_VEC has no numeric statistics" would be
            ! misleading -- the ELEMENTS are perfectly numeric and it is the shape that is refused.
            ! Width cannot be tested separately: `%init` already guarantees a scalar kind is width
            ! 1 and a vector kind is wider, so a `colwidth() /= 1` check would be dead code.
            error stop what // ": this column is " // trim(stats_i2s(int(col%colwidth(), int64))) // &
                " elements wide; flattening a vector column into one population is a different " // &
                "statistic, so use %get_elem for one element position or pass the flattened " // &
                "array yourself if that is what you mean"
        case default
            call parquet_kind_name(k, kname)
            error stop what // ": a column of kind " // kname // " has no numeric statistics; " // &
                "this family accepts int32, int64, float32, float64 and logical columns"
        end select

        ! The `(1:n)` bounds below are BELT AND BRACES, not what makes this correct, and saying so
        ! is worth a line: `parquet_column_data_ptr` already returns `col%i32(1:col%nrows)`, so the
        ! pointer never spans the slack that geometric growth leaves beyond the row count. Verified
        ! by mutation -- reading the whole pointer instead changes no answer. The bound stays
        ! because it states the intent and would survive a `data_ptr` that stopped slicing.
        n = col%length()
        allocate(v(n))
        select case (k)
        case (PK_INT32)
            call parquet_column_data_ptr(col, p32)
            v = real(p32(1:n), real64)
        case (PK_INT64)
            call parquet_column_data_ptr(col, p64)
            v = real(p64(1:n), real64)
        case (PK_FLOAT32)
            call parquet_column_data_ptr(col, r32)
            v = real(r32(1:n), real64)
        case (PK_FLOAT64)
            call parquet_column_data_ptr(col, r64)
            v = r64(1:n)
        case default
            call parquet_column_data_ptr(col, pb)
            v = merge(1.0_real64, 0.0_real64, pb(1:n))
        end select
        call col%row_validity(mask)
    end subroutine col_to_real64

    module procedure count_valid_i32
        integer(int64) :: i, nv

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)
        n = 0_int64
        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight. A weight is
            ! examined only for an element still in the population, which is what keeps a NaN
            ! weight beside a null value from aborting.
            if (present(is_valid)) then
                if (.not. is_valid(i)) cycle
            end if
            if (present(weights)) then
                call stats_check_weight(weights(i), i, "pf_count_valid")
                if (weights(i) <= 0.0_real64) cycle
            end if
            n = n + 1_int64
        end do
    end procedure count_valid_i32

    module procedure count_valid_i64
        integer(int64) :: i, nv

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)
        n = 0_int64
        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight. A weight is
            ! examined only for an element still in the population, which is what keeps a NaN
            ! weight beside a null value from aborting.
            if (present(is_valid)) then
                if (.not. is_valid(i)) cycle
            end if
            if (present(weights)) then
                call stats_check_weight(weights(i), i, "pf_count_valid")
                if (weights(i) <= 0.0_real64) cycle
            end if
            n = n + 1_int64
        end do
    end procedure count_valid_i64

    module procedure count_valid_f32
        integer(int64) :: i, nv
        logical :: skip

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)
        skip = .true.
        if (present(skipnan)) skip = skipnan
        n = 0_int64
        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight. A weight is
            ! examined only for an element still in the population, which is what keeps a NaN
            ! weight beside a null value from aborting.
            if (present(is_valid)) then
                if (.not. is_valid(i)) cycle
            end if
            if (skip) then
                ! `x /= x` rather than `ieee_is_nan`: this is a per-element path, and
                ! `ieee_is_nan` is a runtime call on ifx and nagfor. Both are quiet on a
                ! quiet NaN, so the only cost is a -Wcompare-reals warning under -Wall.
                if (values(i) /= values(i)) cycle
            end if
            if (present(weights)) then
                call stats_check_weight(weights(i), i, "pf_count_valid")
                if (weights(i) <= 0.0_real64) cycle
            end if
            n = n + 1_int64
        end do
    end procedure count_valid_f32

    module procedure count_valid_f64
        integer(int64) :: i, nv
        logical :: skip

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)
        skip = .true.
        if (present(skipnan)) skip = skipnan
        n = 0_int64
        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight. A weight is
            ! examined only for an element still in the population, which is what keeps a NaN
            ! weight beside a null value from aborting.
            if (present(is_valid)) then
                if (.not. is_valid(i)) cycle
            end if
            if (skip) then
                ! `x /= x` rather than `ieee_is_nan`: this is a per-element path, and
                ! `ieee_is_nan` is a runtime call on ifx and nagfor. Both are quiet on a
                ! quiet NaN, so the only cost is a -Wcompare-reals warning under -Wall.
                if (values(i) /= values(i)) cycle
            end if
            if (present(weights)) then
                call stats_check_weight(weights(i), i, "pf_count_valid")
                if (weights(i) <= 0.0_real64) cycle
            end if
            n = n + 1_int64
        end do
    end procedure count_valid_f64

    module procedure count_valid_bool
        integer(int64) :: i, nv

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, "pf_count_valid", is_valid, weights)
        n = 0_int64
        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight. A weight is
            ! examined only for an element still in the population, which is what keeps a NaN
            ! weight beside a null value from aborting.
            if (present(is_valid)) then
                if (.not. is_valid(i)) cycle
            end if
            if (present(weights)) then
                call stats_check_weight(weights(i), i, "pf_count_valid")
                if (weights(i) <= 0.0_real64) cycle
            end if
            n = n + 1_int64
        end do
    end procedure count_valid_bool

    module procedure count_valid_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_count_valid", is_valid, wide, mask)
        call count_valid_f64(wide, n, mask, weights, skipnan)
    end procedure count_valid_col

    module procedure sum_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call sum_f64(wide, s, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, threads=threads)
    end procedure sum_i32

    module procedure mean_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call mean_f64(wide, m, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, threads=threads)
    end procedure mean_i32

    module procedure variance_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call variance_f64(wide, v, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure variance_i32

    module procedure stddev_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call stddev_f64(wide, sd, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure stddev_i32

    module procedure sem_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call sem_f64(wide, se, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure sem_i32

    module procedure skewness_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call skewness_f64(wide, g, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure skewness_i32

    module procedure kurtosis_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call kurtosis_f64(wide, k, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            excess=excess, n_null=n_null, ok=ok, threads=threads)
    end procedure kurtosis_i32

    module procedure moments_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call moments_f64(wide, n_valid=n_valid, mean=mean, variance=variance, stddev=stddev, sem=sem, &
            skewness=skewness, kurtosis=kurtosis, vsum=vsum, vmin=vmin, vmax=vmax, &
            is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, n_null=n_null, threads=threads)
    end procedure moments_i32

    module procedure median_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call median_f64(wide, med, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure median_i32

    module procedure quantile_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call quantile_f64(wide, p, q, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure quantile_i32

    module procedure quantiles_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call quantiles_f64(wide, probs, out, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure quantiles_i32

    module procedure iqr_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call iqr_f64(wide, r, is_valid=is_valid, weights=weights, weight_type=weight_type, method=method, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure iqr_i32

    module procedure trim_mean_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call trim_mean_f64(wide, prop, m, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, &
            threads=threads)
    end procedure trim_mean_i32

    module procedure percentile_of_score_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call percentile_of_score_f64(wide, score, p, is_valid=is_valid, weights=weights, kind=kind, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure percentile_of_score_i32

    module procedure mad_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call mad_f64(wide, m, is_valid=is_valid, weights=weights, scale=scale, center=center, n_null=n_null, &
            ok=ok, threads=threads)
    end procedure mad_i32

    module procedure describe_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call describe_f64(wide, s, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            threads=threads)
    end procedure describe_i32

    module procedure sum_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call sum_f64(wide, s, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, threads=threads)
    end procedure sum_i64

    module procedure mean_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call mean_f64(wide, m, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, threads=threads)
    end procedure mean_i64

    module procedure variance_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call variance_f64(wide, v, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure variance_i64

    module procedure stddev_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call stddev_f64(wide, sd, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure stddev_i64

    module procedure sem_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call sem_f64(wide, se, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure sem_i64

    module procedure skewness_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call skewness_f64(wide, g, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure skewness_i64

    module procedure kurtosis_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call kurtosis_f64(wide, k, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            excess=excess, n_null=n_null, ok=ok, threads=threads)
    end procedure kurtosis_i64

    module procedure moments_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call moments_f64(wide, n_valid=n_valid, mean=mean, variance=variance, stddev=stddev, sem=sem, &
            skewness=skewness, kurtosis=kurtosis, vsum=vsum, vmin=vmin, vmax=vmax, &
            is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, n_null=n_null, threads=threads)
    end procedure moments_i64

    module procedure median_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call median_f64(wide, med, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure median_i64

    module procedure quantile_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call quantile_f64(wide, p, q, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure quantile_i64

    module procedure quantiles_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call quantiles_f64(wide, probs, out, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure quantiles_i64

    module procedure iqr_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call iqr_f64(wide, r, is_valid=is_valid, weights=weights, weight_type=weight_type, method=method, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure iqr_i64

    module procedure trim_mean_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call trim_mean_f64(wide, prop, m, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, &
            threads=threads)
    end procedure trim_mean_i64

    module procedure percentile_of_score_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call percentile_of_score_f64(wide, score, p, is_valid=is_valid, weights=weights, kind=kind, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure percentile_of_score_i64

    module procedure mad_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call mad_f64(wide, m, is_valid=is_valid, weights=weights, scale=scale, center=center, n_null=n_null, &
            ok=ok, threads=threads)
    end procedure mad_i64

    module procedure describe_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call describe_f64(wide, s, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            threads=threads)
    end procedure describe_i64

    module procedure sum_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call sum_f64(wide, s, is_valid=is_valid, weights=weights, skipnan=skipnan, n_null=n_null, &
            n_nan=n_nan, ok=ok, threads=threads)
    end procedure sum_f32

    module procedure mean_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call mean_f64(wide, m, is_valid=is_valid, weights=weights, skipnan=skipnan, n_null=n_null, &
            n_nan=n_nan, ok=ok, threads=threads)
    end procedure mean_f32

    module procedure variance_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call variance_f64(wide, v, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure variance_f32

    module procedure stddev_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call stddev_f64(wide, sd, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure stddev_f32

    module procedure sem_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call sem_f64(wide, se, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure sem_f32

    module procedure skewness_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call skewness_f64(wide, g, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure skewness_f32

    module procedure kurtosis_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call kurtosis_f64(wide, k, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            excess=excess, skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure kurtosis_f32

    module procedure moments_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call moments_f64(wide, n_valid=n_valid, mean=mean, variance=variance, stddev=stddev, sem=sem, &
            skewness=skewness, kurtosis=kurtosis, vsum=vsum, vmin=vmin, vmax=vmax, &
            is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, skipnan=skipnan, n_null=n_null, n_nan=n_nan, threads=threads)
    end procedure moments_f32

    module procedure median_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call median_f64(wide, med, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure median_f32

    module procedure quantile_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call quantile_f64(wide, p, q, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure quantile_f32

    module procedure quantiles_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call quantiles_f64(wide, probs, out, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure quantiles_f32

    module procedure iqr_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call iqr_f64(wide, r, is_valid=is_valid, weights=weights, weight_type=weight_type, skipnan=skipnan, &
            method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure iqr_f32

    module procedure trim_mean_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call trim_mean_f64(wide, prop, m, is_valid=is_valid, weights=weights, skipnan=skipnan, &
            n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure trim_mean_f32

    module procedure percentile_of_score_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call percentile_of_score_f64(wide, score, p, is_valid=is_valid, weights=weights, skipnan=skipnan, &
            kind=kind, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure percentile_of_score_f32

    module procedure mad_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call mad_f64(wide, m, is_valid=is_valid, weights=weights, skipnan=skipnan, scale=scale, &
            center=center, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure mad_f32

    module procedure describe_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call describe_f64(wide, s, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, threads=threads)
    end procedure describe_f32

    module procedure sum_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call sum_f64(wide, s, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, threads=threads)
    end procedure sum_bool

    module procedure mean_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call mean_f64(wide, m, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, threads=threads)
    end procedure mean_bool

    module procedure variance_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call variance_f64(wide, v, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure variance_bool

    module procedure stddev_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call stddev_f64(wide, sd, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure stddev_bool

    module procedure sem_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call sem_f64(wide, se, is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure sem_bool

    module procedure skewness_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call skewness_f64(wide, g, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure skewness_bool

    module procedure kurtosis_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call kurtosis_f64(wide, k, is_valid=is_valid, weights=weights, weight_type=weight_type, bias=bias, &
            excess=excess, n_null=n_null, ok=ok, threads=threads)
    end procedure kurtosis_bool

    module procedure moments_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call moments_f64(wide, n_valid=n_valid, mean=mean, variance=variance, stddev=stddev, sem=sem, &
            skewness=skewness, kurtosis=kurtosis, vsum=vsum, vmin=vmin, vmax=vmax, &
            is_valid=is_valid, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, n_null=n_null, threads=threads)
    end procedure moments_bool

    module procedure median_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call median_f64(wide, med, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure median_bool

    module procedure quantile_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call quantile_f64(wide, p, q, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure quantile_bool

    module procedure quantiles_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call quantiles_f64(wide, probs, out, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            method=method, n_null=n_null, ok=ok, threads=threads)
    end procedure quantiles_bool

    module procedure iqr_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call iqr_f64(wide, r, is_valid=is_valid, weights=weights, weight_type=weight_type, method=method, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure iqr_bool

    module procedure trim_mean_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call trim_mean_f64(wide, prop, m, is_valid=is_valid, weights=weights, n_null=n_null, ok=ok, &
            threads=threads)
    end procedure trim_mean_bool

    module procedure percentile_of_score_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call percentile_of_score_f64(wide, score, p, is_valid=is_valid, weights=weights, kind=kind, &
            n_null=n_null, ok=ok, threads=threads)
    end procedure percentile_of_score_bool

    module procedure mad_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call mad_f64(wide, m, is_valid=is_valid, weights=weights, scale=scale, center=center, n_null=n_null, &
            ok=ok, threads=threads)
    end procedure mad_bool

    module procedure describe_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call describe_f64(wide, s, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            threads=threads)
    end procedure describe_bool

    module procedure sum_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_sum", is_valid, wide, mask)
        call sum_f64(wide, s, is_valid=mask, weights=weights, skipnan=skipnan, n_null=n_null, n_nan=n_nan, &
            ok=ok, threads=threads)
    end procedure sum_col

    module procedure mean_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_mean", is_valid, wide, mask)
        call mean_f64(wide, m, is_valid=mask, weights=weights, skipnan=skipnan, n_null=n_null, n_nan=n_nan, &
            ok=ok, threads=threads)
    end procedure mean_col

    module procedure variance_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_variance", is_valid, wide, mask)
        call variance_f64(wide, v, is_valid=mask, weights=weights, weight_type=weight_type, ddof=ddof, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure variance_col

    module procedure stddev_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_stddev", is_valid, wide, mask)
        call stddev_f64(wide, sd, is_valid=mask, weights=weights, weight_type=weight_type, ddof=ddof, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure stddev_col

    module procedure sem_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_sem", is_valid, wide, mask)
        call sem_f64(wide, se, is_valid=mask, weights=weights, weight_type=weight_type, ddof=ddof, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure sem_col

    module procedure skewness_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_skewness", is_valid, wide, mask)
        call skewness_f64(wide, g, is_valid=mask, weights=weights, weight_type=weight_type, bias=bias, &
            skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure skewness_col

    module procedure kurtosis_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_kurtosis", is_valid, wide, mask)
        call kurtosis_f64(wide, k, is_valid=mask, weights=weights, weight_type=weight_type, bias=bias, &
            excess=excess, skipnan=skipnan, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure kurtosis_col

    module procedure moments_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_moments", is_valid, wide, mask)
        call moments_f64(wide, n_valid=n_valid, mean=mean, variance=variance, stddev=stddev, sem=sem, &
            skewness=skewness, kurtosis=kurtosis, vsum=vsum, vmin=vmin, vmax=vmax, &
            is_valid=mask, weights=weights, weight_type=weight_type, ddof=ddof, bias=bias, &
            excess=excess, skipnan=skipnan, n_null=n_null, n_nan=n_nan, threads=threads)
    end procedure moments_col

    module procedure median_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_median", is_valid, wide, mask)
        call median_f64(wide, med, is_valid=mask, weights=weights, weight_type=weight_type, skipnan=skipnan, &
            method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure median_col

    module procedure quantile_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_quantile", is_valid, wide, mask)
        call quantile_f64(wide, p, q, is_valid=mask, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure quantile_col

    module procedure quantiles_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_quantiles", is_valid, wide, mask)
        call quantiles_f64(wide, probs, out, is_valid=mask, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure quantiles_col

    module procedure iqr_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_iqr", is_valid, wide, mask)
        call iqr_f64(wide, r, is_valid=mask, weights=weights, weight_type=weight_type, skipnan=skipnan, &
            method=method, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure iqr_col

    module procedure trim_mean_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_trim_mean", is_valid, wide, mask)
        call trim_mean_f64(wide, prop, m, is_valid=mask, weights=weights, skipnan=skipnan, n_null=n_null, &
            n_nan=n_nan, ok=ok, threads=threads)
    end procedure trim_mean_col

    module procedure percentile_of_score_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_percentile_of_score", is_valid, wide, mask)
        call percentile_of_score_f64(wide, score, p, is_valid=mask, weights=weights, skipnan=skipnan, &
            kind=kind, n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure percentile_of_score_col

    module procedure mad_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_mad", is_valid, wide, mask)
        call mad_f64(wide, m, is_valid=mask, weights=weights, skipnan=skipnan, scale=scale, center=center, &
            n_null=n_null, n_nan=n_nan, ok=ok, threads=threads)
    end procedure mad_col

    module procedure describe_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_describe", is_valid, wide, mask)
        call describe_f64(wide, s, is_valid=mask, weights=weights, weight_type=weight_type, skipnan=skipnan, &
            threads=threads)
    end procedure describe_col

    module procedure obj_compute_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call obj_compute_f64(self, wide, retain=retain, is_valid=is_valid, weights=weights, &
            weight_type=weight_type, threads=threads)
    end procedure obj_compute_i32

    module procedure obj_update_i32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call obj_update_f64(self, wide, is_valid=is_valid, weights=weights)
    end procedure obj_update_i32

    module procedure obj_compute_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call obj_compute_f64(self, wide, retain=retain, is_valid=is_valid, weights=weights, &
            weight_type=weight_type, threads=threads)
    end procedure obj_compute_i64

    module procedure obj_update_i64
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call obj_update_f64(self, wide, is_valid=is_valid, weights=weights)
    end procedure obj_update_i64

    module procedure obj_compute_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call obj_compute_f64(self, wide, retain=retain, is_valid=is_valid, weights=weights, &
            weight_type=weight_type, skipnan=skipnan, threads=threads)
    end procedure obj_compute_f32

    module procedure obj_update_f32
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = real(values, real64)
        call obj_update_f64(self, wide, is_valid=is_valid, weights=weights)
    end procedure obj_update_f32

    module procedure obj_compute_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call obj_compute_f64(self, wide, retain=retain, is_valid=is_valid, weights=weights, &
            weight_type=weight_type, threads=threads)
    end procedure obj_compute_bool

    module procedure obj_update_bool
        real(real64), allocatable :: wide(:)
        allocate(wide(size(values, kind=int64)))
        wide = merge(1.0_real64, 0.0_real64, values)
        call obj_update_f64(self, wide, is_valid=is_valid, weights=weights)
    end procedure obj_update_bool

    module procedure obj_compute_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_stats%compute", is_valid, wide, mask)
        call obj_compute_f64(self, wide, retain=retain, is_valid=mask, weights=weights, &
            weight_type=weight_type, skipnan=skipnan, threads=threads)
    end procedure obj_compute_col

    module procedure obj_update_col
        real(real64), allocatable :: wide(:)
        logical, allocatable :: mask(:)
        call col_to_real64(values, "pf_stats%update", is_valid, wide, mask)
        call obj_update_f64(self, wide, is_valid=mask, weights=weights)
    end procedure obj_update_col

end submodule parquet_stats_kernel ! GCOVR_EXCL_LINE
