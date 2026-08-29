!=========================
! Playground for testing Fortran code
!=========================
module playground_helpers
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    !
    integer, parameter :: nelem = 3
    !
    type my_type
        integer :: id
        real,dimension(:), allocatable :: vec
        real,dimension(1:nelem) :: vec2
        integer,dimension(:), allocatable :: vec3
    end type my_type
    !
    interface my_array_to_matrix
        module procedure my_array_to_matrix_real
        module procedure my_array_to_matrix_int
    end interface my_array_to_matrix
    !
public :: do_something_index

contains
    !
    subroutine test_logging()
        use parquet_logging
        integer :: i, sink
        type(pf_logger) :: lg
        type(pf_logger) :: lg1, lg2
        character(len=:),allocatable:: msg1,msg2
        !
        call pf_log_info("Starting test_logging subroutine")
        !
        call pf_log_init(level = PF_LEVEL_debug, name="general", console=.false.)
        !call pf_log_init(level = PF_LEVEL_DEBUG)
        !
        call pf_log_add_console(stream = PF_LOG_STDOUT, level = PF_LEVEL_all, sink = sink)
        print*, "Added console sink ", sink
        call pf_log_add_console(stream = PF_LOG_STDERR, level = PF_LEVEL_ERROR, sink = sink)
        print*, "Added console sink ", sink
        !
        call pf_log_warning("This is a warning message")
        call pf_log_info("This is an info message")
        !
        call pf_log_error("no statistics in row group 4", once = .true.)
        call pf_log_error("no statistics in row group 4", once = .true.)
        !
        call pf_log_color("nice!", "38;5;208", msg1)
        call pf_log_color("also nice!", PF_LOG_C_MAGENTA, msg2)
        !
        call pf_log_trace("trace message")
        !
        call pf_log_set_level(PF_LEVEL_trace, name="deep") ! <-- set lowering
        call pf_log_trace(msg1//" and "//msg2, "deep")
        !call pf_log_info(msg1//" and "//msg2, "x")
        call pf_log_trace("Both outputs...", "deep")
        call pf_log_set_level(PF_LEVEL_debug, name="deep") ! <-- unset lowering
        !
        call pf_log_push_context("outer")
        call pf_log_set_format("{time} [{level}] {name} {thread|t}: {context|c:}{message| }")
        !$OMP PARALLEL DO
        do i = 1, 5
            call pf_log_debug("debug message for item " // trim(pf_str(i)), every=2)
            call pf_log_debug("debug message for item EMPTY", every=3)
        end do
        !OMP END PARALLEL DO
        call pf_log_info(" test 1")
        call pf_log_push_context("inner")
        call pf_log_info(" test 1")
        !call pf_log_pop_context()
        call pf_log_clear_context()
        call pf_log_info("test none")
        call pf_log_info(" test 1", context="test")
        call pf_log_pop_context()
        call pf_log_info("test 1")
        !
        call pf_log_blank()
        call pf_log_set_name("final")
        call pf_log_set_level(PF_LEVEL_ERROR, sink=2)
        call pf_log_set_thread_mode(PF_LOG_THREAD_BUFFERED)   ! BEFORE the region
        !$omp parallel do
        do i = 1, 5
            call pf_log_info("step 1")
            call pf_log_info("step 2")
        end do
        !$omp end parallel do
        call pf_log_flush()
        call pf_log_set_thread_mode(PF_LOG_THREAD_DIRECT)
        !
        call pf_log_info("After flush 1")
        !
        ! test push and pop name
        call pf_log_set_format("{time} [{level}] {name}: {message}")
        call pf_log_info("After flush 2")
        !
        call pf_log_blank()
        call lg1%init(level = PF_LEVEL_DEBUG, name="lg1")
        call lg2%init(level = PF_LEVEL_DEBUG, name="lg2")
        call pf_log_info("After flush 3") ! <-- why this do not print?
        call lg2%flush()
        call lg1%set_format("{time} [{level}] {name}: {message}")
        call lg2%set_format("{time} [{level}] {name}: {message}")
        call pf_log_push_name("io")
        call lg1%get_name(msg1)
        call lg2%get_name(msg2)
        print*, "Logger 1 name: ", msg1
        print*, "Logger 2 name: ", msg2
        call pf_log_get_name(msg1)
        print*, "Current logger name: ", msg1
        ! how to retrieve the current active name?
        call pf_log_info("main logger info")
        call lg1%info("logger 1 info")
        call lg2%info("logger 2 info")
        call lg2%info("logger 2 myname", name="myname")
        call pf_log_pop_name()
        call pf_log_info("main logger info")
        call lg1%info("logger 1 info")
        call lg2%info("logger 2 info")
        !
        call pf_log_fatal("This is a fatal message")
        call pf_log_close()
        !
    end subroutine test_logging
    !
    !> Wall-clock seconds since an arbitrary origin.
    function now() result(t)
        real(real64) :: t !! seconds.
        integer(int64) :: c, r
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
    end function now
    !
    subroutine my_array_to_matrix_real(arr, col, mat)
        type(my_type), dimension(:), intent(in) :: arr
        character(len=*), intent(in) :: col
        real, dimension(:, :), allocatable, intent(out) :: mat
        integer :: i, lens(size(arr))
        !
        select case (col)
        case ("vec")
            lens = [(size(arr(i)%vec), i = 1, size(arr))]
            call check_uniform_length(lens, col)
            allocate(mat(lens(1), size(arr)))
            do i = 1, size(arr)
                mat(:, i) = arr(i)%vec
            end do
        case ("vec2")
            lens = [(size(arr(i)%vec2), i = 1, size(arr))]
            call check_uniform_length(lens, col)
            allocate(mat(lens(1), size(arr)))
            do i = 1, size(arr)
                mat(:, i) = arr(i)%vec2
            end do
        case default
            error stop "my_array_to_matrix: unknown or non-real column '"//col//"'"
        end select
        !
    end subroutine my_array_to_matrix_real
    !
    subroutine my_array_to_matrix_int(arr, col, mat)
        type(my_type), dimension(:), intent(in) :: arr
        character(len=*), intent(in) :: col
        integer, dimension(:, :), allocatable, intent(out) :: mat
        integer :: i, lens(size(arr))
        !
        select case (col)
        case ("vec3")
            lens = [(size(arr(i)%vec3), i = 1, size(arr))]
            call check_uniform_length(lens, col)
            allocate(mat(lens(1), size(arr)))
            do i = 1, size(arr)
                mat(:, i) = arr(i)%vec3
            end do
        case default
            error stop "my_array_to_matrix: unknown or non-integer column '"//col//"'"
        end select
        !
    end subroutine my_array_to_matrix_int
    !
    subroutine check_uniform_length(lens, col)
        integer, dimension(:), intent(in) :: lens
        character(len=*), intent(in) :: col
        !
        if (any(lens /= lens(1))) then
            error stop "check_uniform_length: column '"//col//"' has non-uniform length across elements"
        end if
        !
    end subroutine check_uniform_length
    !
    subroutine print_string_vector(vec)
        character(*), intent(in) :: vec(:)
        integer :: i
        !
        print *, "String vector: "
        do i = 1, size(vec)
            print *, "Row  ", i, ": ", vec(i)
        end do
        !
    end subroutine print_string_vector
    !
    subroutine do_something_index(ind, cind, option)
        integer,intent(in) :: ind
        character(len=*), intent(in) :: cind
        integer,intent(in) :: option
        integer,parameter:: ncycles = 10
        integer,parameter:: nlen = 100000
        real(real64),dimension(:), allocatable :: arr
        integer :: i
        real(real64) :: tstart, tend
        !
        allocate(arr(nlen))
        !
        tstart = now()
        select case (option)
        case (1)
            do i = 1, ncycles
                call do_something(arr, ind)
            end do
        case (2)
            select case (cind)
            case ("first")
                do i = 1, ncycles
                    call do_something(arr, 1)
                end do
            case ("last")
                do i = 1, ncycles
                    call do_something(arr, nlen)
                end do
            case ("second")
                do i = 1, ncycles
                    call do_something(arr, 2)
                end do
            case ("third")
                do i = 1, ncycles
                    call do_something(arr, 3)
                end do
            case default
                error stop "do_something_index: unknown cind"
            end select
        case default
            error stop "do_something_index: unknown option"
        end select
        tend = now()
        print *, "Time taken: ", tend - tstart, " seconds"
        !
        contains
            subroutine do_something(arr, ind)
                real(real64), dimension(:), intent(inout) :: arr
                integer, intent(in) :: ind
                integer :: i
                !
                arr(ind) = arr(ind) + 1
                !
            end subroutine do_something
        !
    end subroutine do_something_index
    !
end module playground_helpers
!
program playground
    use parquet
    use playground_helpers
    use iso_fortran_env, only : int64, real64
    implicit none
    !
    !call test_vector_column() is a playground for testing the conversions
    !call test_print_string_vector() ! test printing a vector of strings
    !call test_select_case() ! test select case with string
    call test_logging() ! test logging
    !
contains
    subroutine run_do_something_index(ind, cind, option)
        integer, intent(in) :: ind
        character(len=*), intent(in) :: cind
        integer, intent(in) :: option
        integer, parameter :: ncycles = 100000000
        integer, parameter :: nlen = 100000
        real(real64), dimension(:), allocatable :: arr
        integer :: i
        ! int64, per SYSTEM_CLOCK's own recommendation: a default-integer count and rate
        ! can wrap mid-measurement and turn a timing negative.
        integer(int64) :: cstart, cend, cr
        real(real64) :: tstart, tend
        !
        allocate(arr(nlen))
        !
        call system_clock(count=cstart, count_rate=cr)
        select case (option)
        case (1)
            do i = 1, ncycles
                arr(ind) = arr(ind) + 1
            end do
        case (2)
            select case (cind)
            case ("first")
                do i = 1, ncycles
                    arr(1) = arr(1) + 1
                end do
            case ("last")
                do i = 1, ncycles
                    arr(nlen) = arr(nlen) + 1
                end do
            case ("second")
                do i = 1, ncycles
                    arr(2) = arr(2) + 1
                end do
            case ("third")
                do i = 1, ncycles
                    arr(3) = arr(3) + 1
                end do
            case default
                error stop "do_something_index: unknown cind"
            end select
        case default
            error stop "do_something_index: unknown option"
        end select
        call system_clock(count=cend)
        tstart = real(cstart, real64) / real(cr, real64)
        tend = real(cend, real64) / real(cr, real64)
        print *, "Time taken: ", tend - tstart, " seconds"
        !
    end subroutine run_do_something_index
    !
    subroutine test_print_string_vector()
        character(len=:), allocatable :: vec(:)
        character(len=:),allocatable:: fortran
        integer :: i
        !
        fortran = "Fortran"
        !allocate(character(len=7) :: vec(5))
        vec = [character(len=6) :: "a", "bb", "ccc", "dddd", fortran]
        !
        call print_string_vector(vec)
        call print_string_vector([character(len=6) :: "a", "bb", "ccc", "dddd", fortran])
        !
    end subroutine test_print_string_vector
    !
    subroutine test_select_case()
        character(len=:), allocatable :: col
        integer:: i
        !
        col = "first"
        do i = 1, 3
            call run_do_something_index(1, col, option=1)
        end do
        do i = 1, 3
            call run_do_something_index(1, col, option=2)
        end do
        !
        print*, "--------------------"
        col = "third"
        do i = 1, 3
            call run_do_something_index(3, col, option=1)
        end do
        do i = 1, 3
            call run_do_something_index(3, col, option=2)
        end do
        !
    end subroutine test_select_case
    !
    subroutine test_vector_column()
        integer,parameter :: nrows = 5
        real,dimension(:,:),allocatable :: vector_column ! (nelem, nrows)
        type(my_type), dimension(:), allocatable :: my_array
        real,dimension(:,:),allocatable :: rmat
        integer :: i
        !
        allocate(vector_column(nelem, nrows))
        allocate(my_array(nrows))
        !
        block
            integer :: j
            do i = 1, nrows
                vector_column(:, i) = [(real(j), j=1, nelem)] * i
                my_array(i)%id = i
                allocate(my_array(i)%vec(nelem))
                my_array(i)%vec = [(real(j), j=1, nelem)] * i
                my_array(i)%vec2 = [(real(j), j=1, nelem)] * i
                allocate(my_array(i)%vec3(nelem))
                my_array(i)%vec3 = [(real(j), j=1, nelem)] * i
            end do
        end block
        !
        call print_vector_column(vector_column)
        call print_my_array(my_array)
        !call print_my_array_vec(my_array(:)%vec2) ! not allowed
        !
        !call print_test(my_array, "vec2")
        call my_array_to_matrix(my_array, "vec", rmat)
        call print_vector_column(rmat)
        call my_array_to_matrix(my_array, "vec2", rmat)
        call print_vector_column(rmat)
    end subroutine test_vector_column
    !
    subroutine print_vector_column(vec_col)
        real,dimension(:,:), intent(in) :: vec_col
        integer :: i
        !
        print *, "Vector column: "
        do i = 1, size(vec_col, 2)
            print *, "Row  ", i, ": ", vec_col(:, i)
        end do
        !
    end subroutine print_vector_column
    !
    subroutine print_my_array(arr)
        type(my_type), dimension(:), intent(in) :: arr
        integer :: i
        !
        print *, "My array: "
        do i = 1, size(arr)
            print *, "Elem ", i, ": ", arr(i)%vec
        end do
        !
    end subroutine print_my_array
    !
    subroutine print_my_array_vec(vec)
        real,dimension(:), intent(in) :: vec
        integer :: i
        !
        print *, "Vector column: "
        do i = 1, size(vec, 1)
            print *, "Row  ", i, ": ", vec(i)
        end do
        !
    end subroutine print_my_array_vec
    !
    subroutine print_test(typ,col)
        class(*),dimension(:), intent(in) :: typ
        character(len=*), intent(in) :: col
        integer :: i
        !
        print *, "Column: ", col
        !
        do i = 1, size(typ)
            ! can I access typ(i)%col here? col is the column name, which can be any
        end do
        !
    end subroutine print_test
    !
end program playground
