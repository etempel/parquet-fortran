!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the independent parquet_strings module (parquet_string_column +
!> parquet_string handle). Runs in isolation from the rest of the library.
module test_parquet_string
    use parquet_strings
    use iso_fortran_env, only : int8, int32, int64
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_associated
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    !
    implicit none
    private
    public :: collect_tests_parquet_string
    !
    !> Array-of-structs row type used to exercise the handle-into-a-shared-column pattern.
    type :: t_row
        integer :: id = 0
        type(parquet_string) :: name
    end type t_row
    !
contains
    !
    subroutine collect_tests_parquet_string(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("append and get round-trips", test_append_and_get), &
            new_unittest("empty string vs null are distinct", test_empty_vs_null), &
            new_unittest("null_value and allow_null on get", test_null_options), &
            new_unittest("strip/trim options on append", test_strip_trim), &
            new_unittest("set (same-length, longer, shorter, clears null)", test_set), &
            new_unittest("set_null (column-level and handle write-through)", test_set_null), &
            new_unittest("erase preserves order and compacts", test_erase), &
            new_unittest("append_column merges payload and nulls", test_append_column), &
            new_unittest("find (exact, trimmed, reverse, absent)", test_find), &
            new_unittest("equals/contains/startswith/endswith", test_compare_ops), &
            new_unittest("view handle basics", test_view_handle), &
            new_unittest("handle survives appends (AoS pattern)", test_handle_survives_append), &
            new_unittest("view_all fills one handle per element", test_view_all), &
            new_unittest("view_slice fills one handle per row of a range", test_view_slice), &
            new_unittest("slice extracts an owning copy of a row range", test_slice), &
            new_unittest("build_from gathers an array of handles into a column", test_build_from), &
            new_unittest("to_character materialization + null_value", test_to_character), &
            new_unittest("clone is an independent deep copy", test_clone_independence), &
            new_unittest("move_from empties source; swap exchanges", test_move_and_swap), &
            new_unittest("reserve/capacity/shrink_to_fit", test_reserve_capacity), &
            new_unittest("strip_all / trim_all in place", test_strip_all_trim_all), &
            new_unittest("validate and statistics", test_validate_stats), &
            new_unittest("interop append_buffers (int64/int32/validity/merge)", test_interop_buffers), &
            new_unittest("interop append_buffers validity_offset_bits (sliced-source rebase)", &
                test_interop_buffers_validity_offset), &
            new_unittest("interop raw_buffers export counts", test_raw_buffers), &
            new_unittest("growth over many rows", test_large_growth), &
            new_unittest("first element shortest regression", test_first_shortest), &
            new_unittest("dual-kind int32/int64 index arguments", test_dual_kind), &
            new_unittest("diagnostics: clear, memory_usage, print, validity growth", test_diagnostics_extra), &
            new_unittest("capacity edge cases (empty, shrink, validity shrink)", test_capacity_edges) &
            ]
    end subroutine collect_tests_parquet_string
    !
    ! ------------------------------------------------------------------------------
    !
    subroutine test_append_and_get(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("abc")
        call col%append_string("de")
        call col%append_string("fghij")
        call check(error, col%size() == 3, "size should be 3")
        if (allocated(error)) return
        call check(error, col%character_size() == 10, "character_size should be 10")
        if (allocated(error)) return
        call col%get(1, s)
        call check(error, s == "abc", "get(1)")
        if (allocated(error)) return
        call col%get(2, s)
        call check(error, s == "de", "get(2)")
        if (allocated(error)) return
        call col%get(3, s)
        call check(error, s == "fghij", "get(3)")
        if (allocated(error)) return
        call check(error, col%length(3) == 5, "length(3)")
        if (allocated(error)) return
        call check(error, .not. col%empty(), "not empty")
        if (allocated(error)) return
        call check(error, col%null_count() == 0, "no nulls")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
    end subroutine test_append_and_get
    !
    subroutine test_empty_vs_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("")
        call col%append_null()
        call check(error, col%is_empty(1), "empty string is_empty")
        if (allocated(error)) return
        call check(error, .not. col%is_null(1), "empty string is not null")
        if (allocated(error)) return
        call check(error, col%length(1) == 0, "empty length 0")
        if (allocated(error)) return
        call col%get(1, s)
        call check(error, allocated(s), "empty get is allocated")
        if (allocated(error)) return
        call check(error, len(s) == 0, "empty get has length 0")
        if (allocated(error)) return
        call check(error, col%is_null(2), "null is_null")
        if (allocated(error)) return
        call check(error, col%is_empty(2), "null is_empty defaults true")
        if (allocated(error)) return
        call check(error, col%length(2) == 0, "null length defaults 0")
        if (allocated(error)) return
        call check(error, col%length(2, check_null=.false.) == 0, "null length with check_null=.false.")
        if (allocated(error)) return
        call check(error, col%is_empty(2, check_null=.false.), "null is_empty with check_null=.false.")
        if (allocated(error)) return
        call check(error, col%null_count() == 1, "one null")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
    end subroutine test_empty_vs_null
    !
    subroutine test_null_options(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("x")
        call col%append_null()
        call col%get(2, s, null_value="<NA>")
        call check(error, s == "<NA>", "null_value substitute")
        if (allocated(error)) return
        ! allow_null suppresses the error stop and yields an empty string; null detection is
        ! via is_null(), not via checking whether s is allocated (see get_i64's own doc-comment).
        call col%get(2, s, allow_null=.true.)
        call check(error, len(s) == 0, "allow_null suppresses error, yields empty string")
        if (allocated(error)) return
        ! both given: null_value takes precedence, no error
        call col%get(2, s, null_value="P", allow_null=.true.)
        call check(error, s == "P", "null_value precedence over allow_null")
    end subroutine test_null_options
    !
    subroutine test_strip_trim(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("  ab  ")                 ! verbatim (default)
        call col%append_string("  ab  ", strip=.true.)   ! both ends
        call col%append_string("  ab  ", trim=.true.)    ! trailing only
        call col%append_string("  ab  ", strip=.true., trim=.true.) ! strip wins
        call col%get(1, s)
        call check(error, s == "  ab  ", "verbatim default")
        if (allocated(error)) return
        call col%get(2, s)
        call check(error, s == "ab", "strip both ends")
        if (allocated(error)) return
        call col%get(3, s)
        call check(error, s == "  ab", "trim trailing only")
        if (allocated(error)) return
        call col%get(4, s)
        call check(error, s == "ab", "strip dominates trim")
    end subroutine test_strip_trim
    !
    subroutine test_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s1, s2
        call col%append_string("aa")
        call col%append_string("bb")
        call col%append_string("cc")
        call col%append_null()
        ! same length
        call col%set(2, "BB")
        call col%get(2, s1)
        call check(error, s1 == "BB", "same-length set")
        if (allocated(error)) return
        call col%get(1, s1)
        call col%get(3, s2)
        call check(error, s1 == "aa" .and. s2 == "cc", "neighbours intact")
        if (allocated(error)) return
        ! longer
        call col%set(2, "BBBBB")
        call col%get(2, s1)
        call check(error, s1 == "BBBBB", "longer set")
        if (allocated(error)) return
        call col%get(1, s1)
        call col%get(3, s2)
        call check(error, s1 == "aa" .and. s2 == "cc", "neighbours intact after grow")
        if (allocated(error)) return
        ! shorter
        call col%set(2, "b")
        call col%get(2, s1)
        call check(error, s1 == "b", "shorter set")
        if (allocated(error)) return
        call col%get(3, s1)
        call check(error, s1 == "cc", "neighbour intact after shrink")
        if (allocated(error)) return
        ! set clears null
        call col%set(4, "now")
        call check(error, .not. col%is_null(4), "set clears null")
        if (allocated(error)) return
        call check(error, col%null_count() == 0, "null_count decremented")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
    end subroutine test_set
    !
    subroutine test_set_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: col
        type(parquet_string) :: h
        character(len=:), allocatable :: s1, s3
        call col%append_string("aa")
        call col%append_string("bb")
        call col%append_string("cc")
        call col%set_null(2_int64)
        call check(error, col%is_null(2), "set_null marks element null")
        if (allocated(error)) return
        call check(error, col%length(2) == 0, "set_null shrinks length to 0")
        if (allocated(error)) return
        call check(error, col%null_count() == 1, "null_count reflects set_null")
        if (allocated(error)) return
        call col%get(1, s1)
        call col%get(3, s3)
        call check(error, s1 == "aa" .and. s3 == "cc", "neighbours intact after set_null")
        if (allocated(error)) return
        ! idempotent: calling set_null again on an already-null element doesn't double-count
        call col%set_null(2_int64)
        call check(error, col%null_count() == 1, "set_null idempotent, no double count")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold after set_null")
        if (allocated(error)) return
        ! handle write-through: mutating via a view mutates the underlying column, not just the view
        h = col%view(1_int64)
        call h%set_null()
        call check(error, col%is_null(1), "handle set_null writes through to the column")
        if (allocated(error)) return
        call check(error, h%is_null(), "handle itself now observes null too")
        if (allocated(error)) return
        call check(error, col%null_count() == 2, "null_count after handle set_null")
        if (allocated(error)) return
        ! dual-kind: int32 index
        call col%set_null(3_int32)
        call check(error, col%is_null(3), "set_null with int32 index")
    end subroutine test_set_null
    !
    subroutine test_erase(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        call col%append_string("aa")
        call col%append_string("bbb")
        call col%append_null()
        call col%append_string("dddd")
        call col%erase(2)                 ! remove "bbb"
        call check(error, col%size() == 3, "size after erase")
        if (allocated(error)) return
        call col%get(1, s)
        call check(error, s == "aa", "elem 1 intact")
        if (allocated(error)) return
        call check(error, col%is_null(2), "null shifted into slot 2")
        if (allocated(error)) return
        call col%get(3, s)
        call check(error, s == "dddd", "elem 3 shifted")
        if (allocated(error)) return
        call check(error, col%character_size() == 6, "nchars after erase (aa+dddd)")
        if (allocated(error)) return
        call col%erase(2)                 ! remove the null
        call check(error, col%null_count() == 0, "null erased")
        if (allocated(error)) return
        call col%get(2, s)
        call check(error, s == "dddd", "elem shifted after null erase")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
    end subroutine test_erase
    !
    subroutine test_append_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: a, b
        character(len=:), allocatable :: s1, s2
        call a%append_string("one")
        call a%append_null()
        call b%append_string("two")
        call b%append_string("three")
        call b%append_null()
        call a%append_column(b)
        call check(error, a%size() == 5, "combined size")
        if (allocated(error)) return
        call a%get(1, s1)
        call check(error, s1 == "one", "a[1]")
        if (allocated(error)) return
        call check(error, a%is_null(2), "a[2] null")
        if (allocated(error)) return
        call a%get(3, s1)
        call check(error, s1 == "two", "b[1] appended")
        if (allocated(error)) return
        call a%get(4, s1)
        call check(error, s1 == "three", "b[2] appended")
        if (allocated(error)) return
        call check(error, a%is_null(5), "b[3] null appended")
        if (allocated(error)) return
        call check(error, a%null_count() == 2, "merged null count")
        if (allocated(error)) return
        call check(error, b%size() == 3, "source unchanged")
        if (allocated(error)) return
        call check(error, a%validate(), "invariants hold")
        if (allocated(error)) return
        ! append a null-free column onto a null-containing one (self has_nulls, other does not)
        block
            type(parquet_string_column) :: c
            call c%append_string("z1")
            call c%append_string("z2")
            call a%append_column(c)
            call a%get(6, s1)
            call a%get(7, s2)
            call check(error, s1 == "z1" .and. s2 == "z2", "null-free column appended")
            if (allocated(error)) return
            call check(error, .not. a%is_null(6), "appended rows valid")
            if (allocated(error)) return
            call check(error, a%null_count() == 2, "null count unchanged by null-free append")
            if (allocated(error)) return
            call check(error, a%validate(), "invariants hold after mixed append")
        end block
    end subroutine test_append_column
    !
    subroutine test_find(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        call col%append_string("cat")
        call col%append_string("dog ")     ! trailing space
        call col%append_null()
        call col%append_string("cat")
        call check(error, col%find("cat") == 1, "find first exact")
        if (allocated(error)) return
        call check(error, col%find("cat", reverse=.true.) == 4, "find reverse last")
        if (allocated(error)) return
        call check(error, col%find("dog") == 0, "exact miss (trailing space)")
        if (allocated(error)) return
        call check(error, col%find("dog", exact=.false.) == 2, "trimmed match")
        if (allocated(error)) return
        call check(error, col%find("zzz") == 0, "absent returns 0")
    end subroutine test_find
    !
    subroutine test_compare_ops(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        call col%append_string("hello ")   ! trailing space
        call col%append_null()
        call col%append_string("")
        call check(error, col%equals(1, "hello ", exact=.true.), "exact equals with space")
        if (allocated(error)) return
        call check(error, .not. col%equals(1, "hello", exact=.true.), "exact differs on space")
        if (allocated(error)) return
        call check(error, col%equals(1, "hello", exact=.false.), "trimmed equals")
        if (allocated(error)) return
        call check(error, col%startswith(1, "hel"), "startswith")
        if (allocated(error)) return
        call check(error, col%endswith(1, "lo "), "endswith with space")
        if (allocated(error)) return
        call check(error, col%contains(1, "ell"), "contains substring")
        if (allocated(error)) return
        call check(error, col%contains(1, ""), "contains empty is true")
        if (allocated(error)) return
        call check(error, col%startswith(1, ""), "startswith empty is true")
        if (allocated(error)) return
        ! null defaults to .false. for comparisons
        call check(error, .not. col%equals(2, ""), "null not equal to empty")
        if (allocated(error)) return
        call check(error, .not. col%contains(2, "x"), "null contains false")
        if (allocated(error)) return
        ! empty string
        call check(error, col%equals(3, ""), "empty equals empty")
        if (allocated(error)) return
        call check(error, col%startswith(3, ""), "empty startswith empty")
        if (allocated(error)) return
        ! trimmed equals with differing trimmed lengths
        call check(error, .not. col%equals(1, "hell", exact=.false.), "trimmed equals, lengths differ")
        if (allocated(error)) return
        ! comparison null guard exercised with an explicit check_null=.false.
        call check(error, .not. col%equals(2, "x", check_null=.false.), "null equals with check_null=.false.")
        if (allocated(error)) return
        ! contains: element shorter than substring, and no-match after scanning
        call check(error, .not. col%contains(3, "abc"), "contains: element shorter than substring")
        if (allocated(error)) return
        call check(error, .not. col%contains(1, "zzz"), "contains: no match")
        if (allocated(error)) return
        ! startswith: null, element shorter than prefix, byte mismatch
        call check(error, .not. col%startswith(2, "x"), "startswith on null is false")
        if (allocated(error)) return
        call check(error, .not. col%startswith(3, "abc"), "startswith: element shorter than prefix")
        if (allocated(error)) return
        call check(error, .not. col%startswith(1, "xyz"), "startswith: byte mismatch")
        if (allocated(error)) return
        ! endswith: null, empty suffix, element shorter than suffix, byte mismatch
        call check(error, .not. col%endswith(2, "x"), "endswith on null is false")
        if (allocated(error)) return
        call check(error, col%endswith(1, ""), "endswith empty suffix is true")
        if (allocated(error)) return
        call check(error, .not. col%endswith(3, "abc"), "endswith: element shorter than suffix")
        if (allocated(error)) return
        call check(error, .not. col%endswith(1, "xy"), "endswith: byte mismatch")
    end subroutine test_compare_ops
    !
    subroutine test_view_handle(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: col
        type(parquet_string) :: h, h2
        character(len=:), allocatable :: s
        integer :: u
        call col%append_string("alpha")
        call col%append_null()
        h = col%view(1)
        call check(error, h%length() == 5, "handle length")
        if (allocated(error)) return
        call h%to_string(s)
        call check(error, s == "alpha", "handle to_string")
        if (allocated(error)) return
        call check(error, h%equals("alpha"), "handle equals")
        if (allocated(error)) return
        call check(error, h%startswith("alp"), "handle startswith")
        if (allocated(error)) return
        call check(error, h%endswith("pha"), "handle endswith")
        if (allocated(error)) return
        call check(error, h%contains("lph"), "handle contains")
        if (allocated(error)) return
        call check(error, .not. h%is_null(), "handle not null")
        if (allocated(error)) return
        h2 = col%view(2)
        call check(error, h2%is_null(), "handle on null")
        if (allocated(error)) return
        call check(error, h2%is_empty(), "null handle is_empty true")
        if (allocated(error)) return
        ! handle print (value branch and null branch) to a scratch unit
        open(newunit=u, status='scratch')
        call h%print(unit=u)
        call h2%print(unit=u)
        close(u)
    end subroutine test_view_handle
    !
    subroutine test_handle_survives_append(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: col
        type(t_row) :: rows(3)
        character(len=:), allocatable :: s
        integer :: i
        character(len=8) :: names(3)
        names = [character(len=8) :: "aaa", "bb", "cccccccc"]
        do i = 1, 3
            call col%append_string(trim(names(i)))
        end do
        do i = 1, 3
            rows(i)%id = i
            rows(i)%name = col%view(int(i, int64))
        end do
        ! force reallocation by appending many more rows
        do i = 1, 1000
            call col%append_string("filler")
        end do
        call rows(1)%name%to_string(s)
        call check(error, s == "aaa", "handle 1 still valid after realloc")
        if (allocated(error)) return
        call rows(3)%name%to_string(s)
        call check(error, s == "cccccccc", "handle 3 still valid after realloc")
        if (allocated(error)) return
        call check(error, rows(2)%name%length() == 2, "handle 2 length after realloc")
    end subroutine test_handle_survives_append
    !
    subroutine test_view_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: col
        type(t_row) :: rows(4)
        character(len=:), allocatable :: s
        integer :: i
        call col%append_string("alice")
        call col%append_string("bob")
        call col%append_null()
        call col%append_string("dave")
        do i = 1, 4
            rows(i)%id = i
        end do
        call col%view_all(rows(:)%name)
        call rows(1)%name%to_string(s)
        call check(error, s == "alice", "view_all element 1")
        if (allocated(error)) return
        call rows(2)%name%to_string(s)
        call check(error, s == "bob", "view_all element 2")
        if (allocated(error)) return
        call check(error, rows(3)%name%is_null(), "view_all element 3 is null")
        if (allocated(error)) return
        call rows(4)%name%to_string(s)
        call check(error, s == "dave", "view_all element 4")
    end subroutine test_view_all
    !
    subroutine test_view_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: col
        type(parquet_string) :: hs(3), href
        character(len=:), allocatable :: s1, s2
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_null()
        call col%append_string("dddd")
        call col%view_slice(2_int64, 4_int64, hs)
        call check(error, hs(1)%length() == 2, "view_slice row 1 length")
        if (allocated(error)) return
        call check(error, hs(2)%is_null(), "view_slice row 2 null")
        if (allocated(error)) return
        call hs(3)%to_string(s1)
        call check(error, s1 == "dddd", "view_slice row 3 content")
        if (allocated(error)) return
        ! matches view(i) for the same index
        href = col%view(4_int64)
        call href%to_string(s2)
        call check(error, s1 == s2, "view_slice matches view(i) for the same index")
        if (allocated(error)) return
        ! int32 kind entry point
        call col%view_slice(1_int32, 2_int32, hs(1:2))
        call check(error, hs(1)%length() == 1, "view_slice int32 kind row 1")
        if (allocated(error)) return
        call hs(2)%to_string(s1)
        call check(error, s1 == "bb", "view_slice int32 kind row 2")
    end subroutine test_view_slice
    !
    subroutine test_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col, chunk
        character(len=:), allocatable :: s
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_null()
        call col%append_string("dddd")
        call col%append_string("e")
        ! mid-range slice [2,4]
        call col%slice(2_int64, 4_int64, chunk)
        call check(error, chunk%size() == 3, "slice size")
        if (allocated(error)) return
        call chunk%get(1, s)
        call check(error, s == "bb", "slice row 1")
        if (allocated(error)) return
        call check(error, chunk%is_null(2), "slice row 2 null")
        if (allocated(error)) return
        call chunk%get(3, s)
        call check(error, s == "dddd", "slice row 3")
        if (allocated(error)) return
        call check(error, chunk%validate(), "slice invariants hold")
        if (allocated(error)) return
        call check(error, col%size() == 5, "source unchanged by slice")
        if (allocated(error)) return
        ! full-column slice
        call col%slice(1_int64, col%size(), chunk)
        call check(error, chunk%size() == 5, "full-range slice size")
        if (allocated(error)) return
        ! single-element slice
        call col%slice(5_int64, 5_int64, chunk)
        call check(error, chunk%size() == 1, "single-element slice size")
        if (allocated(error)) return
        call chunk%get(1, s)
        call check(error, s == "e", "single-element slice content")
        if (allocated(error)) return
        ! int32 kind entry point
        call col%slice(2_int32, 3_int32, chunk)
        call check(error, chunk%size() == 2, "slice int32 kind size")
        if (allocated(error)) return
        call chunk%get(1, s)
        call check(error, s == "bb", "slice int32 kind content")
    end subroutine test_slice
    !
    subroutine test_build_from(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: src, src2, dest
        type(parquet_string) :: handles(4)
        character(len=:), allocatable :: s
        call src%append_string("alice")
        call src%append_null()
        call src%append_string("carol")
        call src2%append_string("zeta")
        handles(1) = src%view(3_int64)   ! "carol"
        handles(2) = src%view(2_int64)   ! null
        handles(3) = src%view(1_int64)   ! "alice"
        handles(4) = src2%view(1_int64)  ! "zeta" -- from a different source column
        ! pre-populate dest with unrelated content to verify build_from clears it first
        call dest%append_string("stale")
        call dest%build_from(handles)
        call check(error, dest%size() == 4, "build_from gathered size")
        if (allocated(error)) return
        call dest%get(1, s)
        call check(error, s == "carol", "build_from row 1")
        if (allocated(error)) return
        call check(error, dest%is_null(2), "build_from row 2 null")
        if (allocated(error)) return
        call dest%get(3, s)
        call check(error, s == "alice", "build_from row 3")
        if (allocated(error)) return
        call dest%get(4, s)
        call check(error, s == "zeta", "build_from row 4, from a different source column")
        if (allocated(error)) return
        call check(error, dest%null_count() == 1, "build_from null_count")
        if (allocated(error)) return
        call check(error, dest%validate(), "build_from invariants hold")
    end subroutine test_build_from
    !
    subroutine test_to_character(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: arr(:)
        call col%append_string("ab")
        call col%append_string("hello")
        call col%append_null()
        call col%to_character(arr, null_value="NA")
        call check(error, size(arr) == 3, "array size")
        if (allocated(error)) return
        call check(error, len(arr) == 5, "padded to maxlen 5")
        if (allocated(error)) return
        call check(error, arr(1) == "ab", "arr(1) trimmed compare")
        if (allocated(error)) return
        call check(error, arr(2) == "hello", "arr(2)")
        if (allocated(error)) return
        call check(error, arr(3) == "NA", "arr(3) null_value")
    end subroutine test_to_character
    !
    subroutine test_clone_independence(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: a, b
        character(len=:), allocatable :: s
        call a%append_string("orig")
        call a%append_null()
        b = a%clone()
        call a%set(1, "changed")
        call b%get(1, s)
        call check(error, s == "orig", "clone unaffected by source mutation")
        if (allocated(error)) return
        call check(error, b%is_null(2), "clone keeps null")
        if (allocated(error)) return
        call check(error, b%size() == 2, "clone size")
        if (allocated(error)) return
        call check(error, b%validate(), "clone invariants")
    end subroutine test_clone_independence
    !
    subroutine test_move_and_swap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: a, b
        character(len=:), allocatable :: s
        call a%append_string("aaa")
        call a%append_string("bbb")
        call b%move_from(a)
        call check(error, b%size() == 2, "dest got data")
        if (allocated(error)) return
        call b%get(2, s)
        call check(error, s == "bbb", "dest content")
        if (allocated(error)) return
        call check(error, a%size() == 0 .and. a%empty(), "source emptied")
        if (allocated(error)) return
        call check(error, a%validate() .and. b%validate(), "both valid after move")
        if (allocated(error)) return
        ! swap
        call a%append_string("x")
        call b%swap(a)
        call check(error, a%size() == 2 .and. b%size() == 1, "swap exchanged sizes")
        if (allocated(error)) return
        call b%get(1, s)
        call check(error, s == "x", "swap content")
    end subroutine test_move_and_swap
    !
    subroutine test_reserve_capacity(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer(int64) :: cap0
        integer :: i
        call col%reserve(100, 1000)
        call check(error, col%capacity() >= 100, "row capacity reserved")
        if (allocated(error)) return
        call check(error, col%character_capacity() >= 1000, "char capacity reserved")
        if (allocated(error)) return
        cap0 = col%capacity()
        do i = 1, 50
            call col%append_string("abc")
        end do
        call check(error, col%capacity() == cap0, "no realloc within reserve")
        if (allocated(error)) return
        call col%shrink_to_fit()
        call check(error, col%capacity() == col%size(), "shrink_to_fit row cap == size")
        if (allocated(error)) return
        call check(error, col%character_capacity() == col%character_size(), "shrink char cap == size")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
    end subroutine test_reserve_capacity
    !
    subroutine test_strip_all_trim_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: a, b
        character(len=:), allocatable :: s
        call a%append_string("  x  ")
        call a%append_string(" yy ")
        call a%append_null()
        call a%strip_all()
        call a%get(1, s)
        call check(error, s == "x", "strip_all elem 1")
        if (allocated(error)) return
        call a%get(2, s)
        call check(error, s == "yy", "strip_all elem 2")
        if (allocated(error)) return
        call check(error, a%is_null(3), "strip_all leaves null")
        if (allocated(error)) return
        call check(error, a%validate(), "strip_all invariants")
        if (allocated(error)) return
        call b%append_string("  x  ")
        call b%trim_all()
        call b%get(1, s)
        call check(error, s == "  x", "trim_all trailing only")
    end subroutine test_strip_all_trim_all
    !
    subroutine test_validate_stats(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: smry
        integer(int64) :: nr, nn, mn, mx
        call col%append_string("ab")
        call col%append_string("cdef")
        call col%append_null()
        call col%statistics(nrows=nr, n_null=nn, min_len=mn, max_len=mx)
        call check(error, nr == 3, "stats nrows")
        if (allocated(error)) return
        call check(error, nn == 1, "stats nulls")
        if (allocated(error)) return
        call check(error, mn == 2, "stats min_len")
        if (allocated(error)) return
        call check(error, mx == 4, "stats max_len")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
        if (allocated(error)) return
        call col%summary(smry)
        call check(error, len(smry) > 0, "summary non-empty")
    end subroutine test_validate_stats
    !
    subroutine test_interop_buffers(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer(int64), target :: off64(6), off64b(3)
        integer(int32), target :: off32(5)
        character(len=1), target :: dat1(10), dat2(3), datb(2)
        integer(int8), target :: val2(1)
        character(len=:), allocatable :: s1, s2
        integer :: i
        ! batch 1 (int64 offsets, all valid): "abc","de","","fghi","j"
        off64 = [0_int64, 3_int64, 5_int64, 5_int64, 9_int64, 10_int64]
        do i = 1, 10
            dat1(i) = achar(iachar('a') + i - 1)   ! "abcdefghij"
        end do
        call col%append_buffers(5_int64, 10_int64, c_loc(off64), c_loc(dat1), c_null_ptr, .false.)
        call check(error, col%size() == 5, "batch1 size")
        if (allocated(error)) return
        call col%get(1, s1)
        call check(error, s1 == "abc", "batch1 get(1)")
        if (allocated(error)) return
        call col%get(4, s1)
        call check(error, s1 == "fghi", "batch1 get(4)")
        if (allocated(error)) return
        ! batch 2 (int32 offsets, nulls at local rows 2,3): "xy","","","q" (4 rows -> 5 offsets)
        off32 = [0_int32, 2_int32, 2_int32, 2_int32, 3_int32]
        dat2 = [character(len=1) :: "x", "y", "q"]
        val2 = [ibits_byte()]   ! bits: row1=1,row2=0,row3=0,row4=1 -> 0b1001 = 9
        call col%append_buffers(4_int64, 3_int64, c_loc(off32), c_loc(dat2), c_loc(val2), .true.)
        call check(error, col%size() == 9, "merged size")
        if (allocated(error)) return
        call col%get(6, s1)
        call check(error, s1 == "xy", "batch2 get(6)")
        if (allocated(error)) return
        call check(error, col%is_null(7) .and. col%is_null(8), "batch2 nulls at non-byte boundary")
        if (allocated(error)) return
        call col%get(9, s1)
        call check(error, s1 == "q", "batch2 get(9)")
        if (allocated(error)) return
        call check(error, col%null_count() == 2, "merged null count")
        if (allocated(error)) return
        call check(error, .not. col%is_null(1), "batch1 rows stay valid")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold after merge")
        if (allocated(error)) return
        ! nrows_in = 0 is a no-op (early return)
        call col%append_buffers(0_int64, 0_int64, c_null_ptr, c_null_ptr, c_null_ptr, .false.)
        call check(error, col%size() == 9, "append_buffers with nrows_in=0 is a no-op")
        if (allocated(error)) return
        ! batch 3 (no validity) onto a column that already has nulls: "m","n"
        off64b = [0_int64, 1_int64, 2_int64]
        datb = [character(len=1) :: "m", "n"]
        call col%append_buffers(2_int64, 2_int64, c_loc(off64b), c_loc(datb), c_null_ptr, .false.)
        call check(error, col%size() == 11, "batch3 appended")
        if (allocated(error)) return
        call col%get(10, s1)
        call col%get(11, s2)
        call check(error, s1 == "m" .and. s2 == "n", "batch3 content")
        if (allocated(error)) return
        call check(error, .not. col%is_null(10) .and. .not. col%is_null(11), "batch3 rows valid")
        if (allocated(error)) return
        call check(error, col%null_count() == 2, "null count unchanged by null-free batch")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold after batch3")
    end subroutine test_interop_buffers
    !
    !> Returns the validity byte 0b00001001 (rows 1 and 4 valid, rows 2 and 3 null).
    integer(int8) function ibits_byte() result(b)
        b = 0_int8
        b = ibset(b, 0)
        b = ibset(b, 3)
    end function ibits_byte
    !
    !> append_buffers' validity_offset_bits argument (feature_doc.md point 4's "Rebase validity"
    !! fix): when a source array is a genuinely sliced child (e.g. a struct-nested leaf resolved
    !! through a non-zero-offset StructArray::field(), see parquet_wrapper.cpp's
    !! extract_string_buffers/unwrap_struct_path comments), Arrow's validity bitmap is not
    !! pre-rebased the way offsets/data are -- element 1 of the logical slice starts at bit
    !! `validity_offset_bits`, not bit 0. This test constructs one validity byte where the first 3
    !! bits (deliberately the extreme/leading case, per this project's "sized/typed from the first
    !! element" fixture convention) belong to elements *before* the slice and are set to the
    !! opposite pattern of the real data, so a caller that ignored validity_offset_bits (reading
    !! from bit 0 instead of bit 3) would read back the wrong null pattern rather than merely
    !! landing on a coincidentally-correct answer.
    subroutine test_interop_buffers_validity_offset(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer(int64), target :: off(5)
        character(len=1), target :: dat(4)
        integer(int8), target :: valbyte
        character(len=:), allocatable :: s
        ! Bits 0-2 (before the slice) = 1,1,1 -- the opposite of the real elements 1/4 (null) --
        ! so an implementation that forgot the offset and read from bit 0 would see rows 1/4 as
        ! valid instead of null. Bits 3-6 (the real elements) = 0(null),1(valid),1(valid),0(null).
        valbyte = 0_int8
        valbyte = ibset(valbyte, 0)
        valbyte = ibset(valbyte, 1)
        valbyte = ibset(valbyte, 2)
        valbyte = ibset(valbyte, 4)
        valbyte = ibset(valbyte, 5)
        ! 4 elements: "" (null), "de", "fg", "" (null).
        off = [0_int64, 0_int64, 2_int64, 4_int64, 4_int64]
        dat = [character(len=1) :: "d", "e", "f", "g"]
        call col%append_buffers(4_int64, 4_int64, c_loc(off), c_loc(dat), c_loc(valbyte), .false., &
            validity_offset_bits=3_int64)
        call check(error, col%size() == 4, "validity_offset_bits: size")
        if (allocated(error)) return
        call check(error, col%is_null(1) .and. (.not. col%is_null(2)) .and. (.not. col%is_null(3)) &
            .and. col%is_null(4), "validity_offset_bits: null pattern honors the bit offset, not bit 0")
        if (allocated(error)) return
        call col%get(2, s)
        call check(error, s == "de", "validity_offset_bits: row 2 content")
        if (allocated(error)) return
        call col%get(3, s)
        call check(error, s == "fg", "validity_offset_bits: row 3 content")
    end subroutine test_interop_buffers_validity_offset
    !
    subroutine test_raw_buffers(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: col
        type(c_ptr) :: op, dp, vp
        integer(int64) :: nr, nc
        logical :: hasv
        call col%append_string("abc")
        call col%append_string("de")
        call col%raw_buffers(op, dp, vp, nr, nc, hasv)
        call check(error, nr == 2, "raw_buffers nrows")
        if (allocated(error)) return
        call check(error, nc == 5, "raw_buffers nchars")
        if (allocated(error)) return
        call check(error, .not. hasv, "no validity when no nulls")
        if (allocated(error)) return
        call check(error, c_associated(dp), "data ptr set when payload present")
        if (allocated(error)) return
        call check(error, .not. c_associated(vp), "validity ptr null when no nulls")
        if (allocated(error)) return
        ! empty column: offsets/data/validity pointers all null
        block
            type(parquet_string_column), target :: empty_col
            call empty_col%raw_buffers(op, dp, vp, nr, nc, hasv)
            call check(error, nr == 0 .and. nc == 0, "empty raw_buffers counts")
            if (allocated(error)) return
            call check(error, .not. c_associated(op) .and. .not. c_associated(dp), "empty offsets/data ptrs null")
            if (allocated(error)) return
        end block
        ! null-containing column: validity pointer is set
        block
            type(parquet_string_column), target :: with_null
            call with_null%append_string("a")
            call with_null%append_null()
            call with_null%raw_buffers(op, dp, vp, nr, nc, hasv)
            call check(error, hasv, "has_validity true with nulls")
            if (allocated(error)) return
            call check(error, c_associated(vp), "validity ptr set with nulls")
        end block
    end subroutine test_raw_buffers
    !
    subroutine test_large_growth(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        integer :: i
        character(len=16) :: buf
        do i = 1, 20000
            write(buf, '(a,i0)') "row", i
            call col%append_string(trim(buf))
        end do
        call check(error, col%size() == 20000, "20k rows")
        if (allocated(error)) return
        call col%get(1, s)
        call check(error, s == "row1", "first row")
        if (allocated(error)) return
        call col%get(20000, s)
        call check(error, s == "row20000", "last row")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold")
    end subroutine test_large_growth
    !
    subroutine test_first_shortest(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        ! first element deliberately shortest; a later one longer (sizing-from-first-element bug guard)
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_string("cccccccccc")
        call check(error, col%length(1) == 1, "len first")
        if (allocated(error)) return
        call check(error, col%length(3) == 10, "len last (longest)")
        if (allocated(error)) return
        call col%get(3, s)
        call check(error, s == "cccccccccc", "get longest")
        if (allocated(error)) return
        call check(error, col%find("cccccccccc") == 3, "find longest")
    end subroutine test_first_shortest
    !
    subroutine test_dual_kind(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        integer(int32) :: i32
        integer(int64) :: i64
        call col%append_string("p")
        call col%append_string("qq")
        i32 = 2_int32
        i64 = 2_int64
        call check(error, col%length(i32) == 2, "length with int32 index")
        if (allocated(error)) return
        call check(error, col%length(i64) == 2, "length with int64 index")
        if (allocated(error)) return
        call col%get(1_int32, s)
        call check(error, s == "p", "get with int32 literal")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "p", "get with int64 literal")
    end subroutine test_dual_kind
    !
    subroutine test_diagnostics_extra(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: msg
        integer :: u, i
        ! validity bitmap growth: a null followed by >8 more rows forces the bitmap to grow
        call col%append_null()
        do i = 1, 20
            call col%append_string("r")
        end do
        call check(error, col%null_count() == 1, "one null among 21 rows")
        if (allocated(error)) return
        call check(error, col%validate(), "invariants hold after validity growth")
        if (allocated(error)) return
        ! memory_usage
        call check(error, col%memory_usage() > 0, "memory_usage is positive")
        if (allocated(error)) return
        ! validate with the optional message argument on a valid column
        call check(error, col%validate(msg), "validate ok with message arg")
        if (allocated(error)) return
        call check(error, len(msg) == 0, "no message on a valid column")
        if (allocated(error)) return
        ! print to a scratch unit: exercises the null row, value rows and the "... more" tail
        open(newunit=u, status='scratch')
        call col%print(unit=u, max_rows=3_int64)
        close(u)
        ! clear releases all memory
        call col%clear()
        call check(error, col%empty(), "clear empties the column")
        if (allocated(error)) return
        call check(error, col%capacity() == 0, "clear frees row capacity")
        if (allocated(error)) return
        call check(error, col%character_capacity() == 0, "clear frees char capacity")
    end subroutine test_diagnostics_extra
    !
    subroutine test_capacity_edges(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: a, b
        ! a fresh column reports zero capacity (offsets/data unallocated)
        call check(error, a%capacity() == 0, "empty row capacity is 0")
        if (allocated(error)) return
        call check(error, a%character_capacity() == 0, "empty char capacity is 0")
        if (allocated(error)) return
        ! reserve character space but append nothing, then shrink -> data freed (nchars == 0)
        call a%reserve(0, 100)
        call check(error, a%character_capacity() >= 100, "char capacity reserved")
        if (allocated(error)) return
        call a%shrink_to_fit()
        call check(error, a%character_capacity() == 0, "shrink frees an empty data buffer")
        if (allocated(error)) return
        ! validity over-allocated then shrunk: a null, then reserve many rows, then shrink
        call b%append_null()
        call b%reserve(64, 0)          ! grows the validity bitmap well beyond one row
        call b%shrink_to_fit()         ! validity buffer larger than needed -> shrinks
        call check(error, b%validate(), "invariants hold after validity shrink")
        if (allocated(error)) return
        call check(error, b%is_null(1), "the null survives reserve+shrink")
    end subroutine test_capacity_edges
    !
end module test_parquet_string
