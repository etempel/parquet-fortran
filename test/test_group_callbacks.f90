!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The callbacks the grouping's error scenarios hand to `%apply`: MODULE procedures and a
!! reducer object, because `test/error_scenarios.f90` is one program, and an internal procedure
!! of a program passed as a callback crashes under one supported compiler before it is called
!! (.claude/rules/fortran-gotchas.md, flang) -- so no scenario may pass one of its own. Nothing
!! here reads a table: every answer is arithmetic over the group number and the rows, so a
!! scenario has no context to hold. Not a test suite (no `collect_tests_*`).
module test_group_callbacks
    use parquet_tables, only : parquet_group_reducer
    use iso_fortran_env, only : int64, real64
    implicit none
    private
    public :: scenario_group_row_sum, scenario_group_two, scenario_group_reducer

    !> A reducer whose only context is a scale: `out(1)` is the scaled sum of the group's row
    !! numbers and `out(2)`, when there is room, the group number.
    type, extends(parquet_group_reducer) :: scenario_group_reducer
        real(real64) :: scale = 1.0_real64 !! multiplies the row sum.
    contains
        procedure :: reduce => scenario_group_reduce !! See the type.
    end type scenario_group_reducer

contains

    !> One value per group: the sum of the group's row numbers.
    function scenario_group_row_sum(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! their sum.
        r = real(sum(rows), real64)
        if (g < 1_int64) r = -huge(r)   ! a group number below 1 is a library defect: poison the answer
    end function scenario_group_row_sum

    !> Two values per group: the sum of the rows and the group number.
    subroutine scenario_group_two(g, rows, out)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64), intent(out) :: out(:)   !! receives the two values.
        out(1) = real(sum(rows), real64)
        out(2) = real(g, real64)
    end subroutine scenario_group_two

    !> `scenario_group_reducer%reduce`; see the type.
    subroutine scenario_group_reduce(self, g, rows, out)
        class(scenario_group_reducer), intent(in) :: self !! the reducer.
        integer(int64), intent(in) :: g                   !! the group number.
        integer(int64), intent(in) :: rows(:)             !! the group's rows.
        real(real64), intent(out) :: out(:)               !! receives the results.
        out(1) = self%scale * real(sum(rows), real64)
        if (size(out) > 1) out(2) = real(g, real64)
    end subroutine scenario_group_reduce

end module test_group_callbacks
