!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_stats.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> The per-kind entry points of `parquet_stats`, and the argument guards they share.
!!
!! Each specific is the same shape: check the optional arguments against `values`, then walk the
!! array applying the family's exclusion order -- nullness, then NaN, then weight. The guards live
!! here, once, rather than being emitted into every body.
submodule (parquet_stats) parquet_stats_kernel
    implicit none

contains

    !> Renders an index for an error message without a deferred-length result.
    !!
    !! A fixed-length function result is fine; a `character(len=:), allocatable` one is the shape
    !! this project forbids outright, because gfortran's hidden length temporary for it is not
    !! reliably thread-local.
    pure function stats_i2s(v) result(res)
        integer(int64), intent(in) :: v !! the value to render.
        character(len=24) :: res !! `v` in decimal, blank-padded.
        write(res, '(i0)') v
    end function stats_i2s

    !> Aborts unless every present optional array matches `values` in size.
    subroutine stats_check_sizes(nv, what, is_valid, weights)
        integer(int64), intent(in) :: nv !! the number of elements in `values`.
        character(len=*), intent(in) :: what !! the public procedure's name, for the message.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:) !! per element weight.

        if (present(is_valid)) then
            if (size(is_valid, kind=int64) /= nv) &
                error stop what // ": is_valid has " // trim(stats_i2s(size(is_valid, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(nv))
        end if
        if (present(weights)) then
            if (size(weights, kind=int64) /= nv) &
                error stop what // ": weights has " // trim(stats_i2s(size(weights, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(nv))
        end if
    end subroutine stats_check_sizes

    !> Aborts unless `w` is a usable weight: finite and non-negative.
    !!
    !! **The three tests are separate statements on purpose.** Fortran does not short-circuit, so
    !! `w /= w .or. w < 0` would evaluate the `<` on a NaN -- and a NaN comparison with `<` raises
    !! `IEEE_INVALID`, which nagfor's default `-ieee=stop` turns into a dead process. Only `==` and
    !! `/=` are quiet, so the NaN test has to run first and alone.
    subroutine stats_check_weight(w, i, what)
        real(real64), intent(in) :: w !! the weight to validate.
        integer(int64), intent(in) :: i !! its index, for the message.
        character(len=*), intent(in) :: what !! the public procedure's name, for the message.

        if (w /= w) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is NaN; weights must be finite and non-negative"
        if (w < 0.0_real64) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is negative; weights must be finite and non-negative"
        if (w > huge(0.0_real64)) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is infinite; weights must be finite and non-negative"
    end subroutine stats_check_weight

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
