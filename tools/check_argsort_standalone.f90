!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Driver for tools/check_argsort_standalone.sh: sorts a few arrays through `pf_argsort` and
!> prints one line per case, so that a compiler-only build of the argsort tier is proved to RUN
!> and not merely to link.
!>
!> **This program's only library imports are `parquet_argsort` and `parquet_settings_base`, and
!> that is the whole point.** It is compiled by a bare `gfortran`/`nagfor`/... invocation with no
!> fpm, no dependency resolver and no Arrow anywhere -- so if the argsort tier ever acquires an
!> import that reaches `parquet_bindings`, this program stops building. See the shell script's
!> header for what to do then, and for why adding the missing file to its source list is the
!> wrong answer.
!>
!> Kept deliberately small: it is a build gate first and a correctness check second, since the
!> real sorting tests live in test/test_sorting.f90 and run under fpm.
program check_argsort_standalone
    use iso_fortran_env, only: int32, int64, real64, output_unit
    use parquet_argsort, only: pf_argsort, pf_sort_threads
    use parquet_settings_base, only: parquet_set_sort_threads, parquet_get_sort_threads, &
        parquet_set_sort_counting_path, parquet_get_sort_counting_path
    implicit none

    real(real64) :: x(8)
    integer(int32) :: id(8)
    integer(int32), allocatable :: perm(:)
    integer(int64), allocatable :: perm64(:)
    logical :: valid(8)
    integer :: i, bad
    logical :: counting_was

    bad = 0

    ! 1. Ascending real64, int32 permutation. Chosen so that no two elements tie and the sorted
    !    order is not the input order, which a permutation that simply returned the identity would
    !    otherwise satisfy.
    x = [5.0_real64, 1.0_real64, 4.0_real64, 8.0_real64, 2.0_real64, 7.0_real64, 3.0_real64, 6.0_real64]
    call pf_argsort(x, perm)
    do i = 2, size(perm)
        if (x(perm(i)) < x(perm(i-1))) bad = bad + 1
    end do
    if (perm(1) /= 2_int32) bad = bad + 1
    write(output_unit, '(a,i0)') "ascending_real64 bad=", bad

    ! 2. Descending int32, int64 permutation, with the counting fast path forced OFF so that the
    !    comparator path is the one exercised here. Both paths answer the same question and only
    !    a cross-check can tell them apart, so this arm names which one it took.
    counting_was = parquet_get_sort_counting_path()
    call parquet_set_sort_counting_path(.false.)
    id = [3_int32, 1_int32, 4_int32, 1_int32, 5_int32, 9_int32, 2_int32, 6_int32]
    call pf_argsort(id, perm64, descending=.true.)
    bad = 0
    do i = 2, size(perm64)
        if (id(perm64(i)) > id(perm64(i-1))) bad = bad + 1
    end do
    if (id(perm64(1)) /= 9_int32) bad = bad + 1
    call parquet_set_sort_counting_path(counting_was)
    write(output_unit, '(a,i0)') "descending_int32 bad=", bad

    ! 3. Nulls, which the tier places last by default. A null-carrying sort is the case where an
    !    extractor and the engine have to agree about a value they never compare.
    valid = .true.
    valid(3) = .false.
    valid(6) = .false.
    call pf_argsort(x, perm, is_valid=valid)
    bad = 0
    if (valid(perm(size(perm))) .or. valid(perm(size(perm) - 1))) bad = bad + 1
    do i = 2, size(perm) - 2
        if (x(perm(i)) < x(perm(i-1))) bad = bad + 1
    end do
    write(output_unit, '(a,i0)') "nulls_last bad=", bad

    ! 4. The thread rule, which lives in this tier with the engine it serves. Without OpenMP it
    !    must resolve to 1 whatever is asked for, and this program is always built without it.
    call parquet_set_sort_threads(4)
    bad = 0
    if (pf_sort_threads() < 1) bad = bad + 1
    if (parquet_get_sort_threads() /= 4) bad = bad + 1
    write(output_unit, '(a,i0)') "thread_rule bad=", bad

end program check_argsort_standalone
