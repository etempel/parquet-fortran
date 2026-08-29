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

end submodule parquet_stats_kernel ! GCOVR_EXCL_LINE
