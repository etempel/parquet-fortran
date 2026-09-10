!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The callback `bench/benchmark_table.f90`'s `--mode=group` hands to `parquet_grouping%apply`,
!! and the payload pointer it reads through. A MODULE procedure with its context in a module
!! variable, because `benchmark_table` is one program and an internal procedure of a program
!! passed as a callback crashes under one supported compiler before it is called
!! (.claude/rules/fortran-gotchas.md, flang) -- the same reason `test/test_group_callbacks.f90`
!! exists beside `test/error_scenarios.f90`.
!!
!! The callback is deliberately TRIVIAL -- one pass over the group's rows through a `%col`
!! pointer -- because the thread ladder measures what the LOOP costs, not what a caller's
!! procedure costs. A heavier callback would make every rung look better.
module benchmark_group_callbacks
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: bench_group_payload, bench_group_sum

    !> The payload column the callback sums, set once before the ladder runs. A pointer into the
    !! table's own store, which nothing in the ladder moves.
    real(real64), pointer :: bench_group_payload(:) => null()

contains

    !> One value per group: the sum of the group's payload values.
    function bench_group_sum(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number; read only by the poison guard below.
        integer(int64), intent(in) :: rows(:) !! the group's table rows, ascending.
        real(real64) :: r                     !! the group's sum.
        integer(int64) :: i
        r = 0.0_real64
        do i = 1_int64, size(rows, kind=int64)
            r = r + bench_group_payload(rows(i))
        end do
        if (g < 1_int64) r = -huge(r)   ! a group number below 1 is a library defect: poison the answer
    end function bench_group_sum

end module benchmark_group_callbacks
