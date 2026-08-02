!=========================
! Playground for testing Fortran code
!=========================
module playground_helpers
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
contains
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
end module playground_helpers
!
program playground
    use parquet
    use playground_helpers
    implicit none
    !
    !call test_vector_column() is a playground for testing the conversions
    call test_print_string_vector() ! test printing a vector of strings
    !
contains
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
