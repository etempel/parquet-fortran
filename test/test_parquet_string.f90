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
            new_unittest("reindex permutes payload and nulls", test_reindex), &
            new_unittest("gather subsets, reorders and recounts nulls", test_gather), &
            new_unittest("delete_by_mask compacts in one pass", test_delete_by_mask), &
            new_unittest("set_validity nulls by mask in one pass and only adds", test_set_validity_bulk), &
            new_unittest("set_where writes a value by mask in one pass", test_set_where_bulk), &
            new_unittest("gather_from builds from another column with a mask, in one pass", test_gather_from), &
            new_unittest("append_nulls appends n null elements", test_append_nulls), &
            new_unittest("find (exact, trimmed, reverse, absent)", test_find), &
            new_unittest("equals/contains/startswith/endswith", test_compare_ops), &
            new_unittest("view handle basics", test_view_handle), &
            new_unittest("handle survives appends (AoS pattern)", test_handle_survives_append), &
            new_unittest("view_all fills one handle per element", test_view_all), &
            new_unittest("view_slice fills one handle per row of a range", test_view_slice), &
            new_unittest("slice extracts an owning copy of a row range", test_slice), &
            new_unittest("build_from gathers an array of handles into a column", test_build_from), &
            new_unittest("build_from builds from a character array, trimming each element", &
                test_build_from_character), &
            new_unittest("build_from and append_values take a strided character array", &
                test_strided_character_arrays), &
            new_unittest("append_values bulk-appends a character array onto a non-empty column", &
                test_append_values_character), &
            new_unittest("build_from: empty, all-null, zero-length and repeated handles", &
                test_build_from_edges), &
            new_unittest("to_character materialization + null_value", test_to_character), &
            new_unittest("to_character matches %get element for element, padded with blanks", &
                test_to_character_bytes), &
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
            new_unittest("capacity edge cases (empty, shrink, validity shrink)", test_capacity_edges), &
            new_unittest("thread row ranges cover every row and never share a validity byte", &
                test_thread_row_ranges), &
            new_unittest("compare orders elements exactly as Fortran does", &
                test_compare_matches_fortran), &
            new_unittest("slice copies validity correctly from every bit phase", &
                test_slice_validity_alignment), &
            new_unittest("append_column copies validity correctly onto every bit phase", &
                test_append_column_validity_alignment), &
            new_unittest("statistics reports 0/0 when no element is valid", &
                test_statistics_no_valid_elements), &
            new_unittest("to_character pads to null_value when it is the longest value", &
                test_to_character_long_null_value), &
            new_unittest("copy_to matches get-then-assign, padding and truncation included", &
                test_copy_to_matches_get), &
            new_unittest("append_from matches get-then-append, null state included", &
                test_append_from_matches_get), &
            new_unittest("copy_to/append_from/reindex_trusted accept a default-kind integer", &
                test_default_integer_forms), &
            new_unittest("comparison orders a trailing byte below a blank correctly", &
                test_compare_trailing_byte_below_blank), &
            new_unittest("copy_buffers exports the offsets and payload, empty column included", &
                test_copy_buffers), &
            new_unittest("every typed parquet_string_column_* form agrees with its own binding", &
                test_typed_tier_agrees_with_bindings), &
            new_unittest("argminmax picks the elements a compare scan picks, ties to the earliest", &
                test_argminmax_matches_compare) &
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
    !
    !> reindex is the bulk counterpart of a permutation applied with erase/append: it rebuilds
    !> payload, offsets and validity in one pass. The fixture puts the SHORTEST element first
    !> (`.claude/rules/testing.md`'s "Assertions" rule), so a length derived from element 1
    !> would truncate the later, longer ones.
    subroutine test_reindex(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        !
        call col%append_string("a")
        call col%append_string("bcdef")
        call col%append_null()
        call col%append_string("gh")
        call col%reindex([4_int64, 3_int64, 2_int64, 1_int64])
        call check(error, col%size() == 4_int64, "reindex must not change the element count")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "gh", "reindex should move element perm(1) to position 1")
        if (allocated(error)) return
        call col%get(3_int64, s)
        call check(error, s == "bcdef", "a longer element must survive reindex intact")
        if (allocated(error)) return
        call check(error, col%is_null(2_int64), "reindex should carry a null to its new position")
        if (allocated(error)) return
        call check(error, col%null_count() == 1_int64, "reindex must preserve the null count")
        if (allocated(error)) return
        call check(error, col%validate(), "reindex must leave the column's invariants intact")
        if (allocated(error)) return
        ! the int32 convenience specific must behave identically to the int64 one
        call col%reindex([4, 3, 2, 1])
        call col%get(1_int64, s)
        call check(error, s == "a", "the int32 reindex specific should permute the same way")
    end subroutine test_reindex
    !
    !> gather rebuilds the column from an index list of any length, in that list's order.
    !>
    !> The property that separates it from `reindex`, and the one easiest to get wrong, is that the
    !> element count can CHANGE -- so the null count has to be recounted rather than carried over,
    !> and the payload sized to what is actually selected. `%validate()` is asserted after every
    !> shape here because it is what checks `n_null` against the bitmap and `offsets(nrows+1)`
    !> against `nchars`; a gather that got either wrong would still return the right strings.
    !>
    !> The fixture puts the SHORTEST element first (CLAUDE.md), so a length taken from element 1
    !> would truncate the later ones.
    subroutine test_gather(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        !
        call col%append_string("a")
        call col%append_string("bcdef")
        call col%append_null()
        call col%append_string("gh")
        !
        ! Shrink and reorder at once, dropping the null: the count must fall to 0, not stay at 1.
        call col%gather([4_int64, 2_int64])
        call check(error, col%size() == 2_int64, "gather must set the element count to the index count")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "gh", "gather should take elements in the order the list gives")
        if (allocated(error)) return
        call col%get(2_int64, s)
        call check(error, s == "bcdef", "a longer element must survive a gather intact")
        if (allocated(error)) return
        call check(error, col%null_count() == 0_int64, &
            "gather must recount the nulls, not carry the old count over")
        if (allocated(error)) return
        call check(error, col%validate(), "gather must leave the column's invariants intact")
        if (allocated(error)) return
        !
        ! Repeats are permitted, so a gather can also grow the column -- and a repeated NULL must be
        ! counted once per position it lands in.
        call col%clear()
        call col%append_string("x")
        call col%append_null()
        call col%gather([2_int64, 2_int64, 1_int64])
        call check(error, col%size() == 3_int64, "a repeated index must lengthen the column")
        if (allocated(error)) return
        call check(error, col%null_count() == 2_int64, &
            "a null taken twice must be counted twice")
        if (allocated(error)) return
        call check(error, col%is_null(1_int64) .and. col%is_null(2_int64), &
            "both copies of a repeated null element must be null")
        if (allocated(error)) return
        call col%get(3_int64, s)
        call check(error, s == "x", "a non-null element must survive alongside repeated nulls")
        if (allocated(error)) return
        call check(error, col%validate(), "a growing gather must leave the invariants intact")
        if (allocated(error)) return
        !
        ! An empty index list empties the column and leaves it usable.
        call col%gather([integer(int64) ::])
        call check(error, col%size() == 0_int64, "an empty index list must empty the column")
        if (allocated(error)) return
        call check(error, col%null_count() == 0_int64, "emptying must clear the null count")
        if (allocated(error)) return
        call check(error, col%validate(), "an emptied column must still satisfy its invariants")
        if (allocated(error)) return
        !
        ! the int32 convenience specific must behave identically to the int64 one
        call col%clear()
        call col%append_string("p")
        call col%append_string("qr")
        call col%gather([2, 1])
        call col%get(1_int64, s)
        call check(error, s == "qr", "the int32 gather specific should select the same way")
    end subroutine test_gather
    !
    !> delete_by_mask is the bulk counterpart of erase: deleting m elements one at a time is
    !> O(m*nchars), this is O(nchars) once.
    subroutine test_delete_by_mask(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        logical :: none_keep(4)
        !
        call col%append_string("a")
        call col%append_string("bcdef")
        call col%append_null()
        call col%append_string("gh")
        call col%delete_by_mask([.false., .true., .true., .true.])
        call check(error, col%size() == 3_int64, "delete_by_mask should keep exactly the .true. elements")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "bcdef", "the surviving payload must be intact and in order")
        if (allocated(error)) return
        call check(error, col%is_null(2_int64), "a surviving null must still be null")
        if (allocated(error)) return
        call check(error, col%null_count() == 1_int64, "delete_by_mask should recount the nulls")
        if (allocated(error)) return
        call col%get(3_int64, s)
        call check(error, s == "gh", "the last surviving element should keep its content")
        if (allocated(error)) return
        call check(error, col%validate(), "delete_by_mask must leave the column's invariants intact")
        if (allocated(error)) return
        none_keep = .false.
        call col%delete_by_mask(none_keep(1:3))
        call check(error, col%size() == 0_int64, "deleting everything should leave a valid empty column")
        if (allocated(error)) return
        call check(error, col%validate(), "an emptied column must still satisfy its invariants")
    end subroutine test_delete_by_mask
    !
    !> `set_validity` is `set_null` over a whole mask in one rebuild, so the oracle is the loop it
    !! replaces, run on a clone. The fixture puts its SHORTEST element first and already holds a
    !! null before the call, so the add-only rule is exercised rather than assumed; the mask nulls
    !! the FIRST and the LAST element, where a byte cursor is easiest to get wrong, and a
    !! zero-length element sits in the middle.
    subroutine test_set_validity_bulk(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col, oracle, empty
        integer(int64), allocatable :: off_a(:), off_b(:)
        character(len=1), allocatable :: dat_a(:), dat_b(:)
        character(len=:), allocatable :: s
        logical :: valid(6), none(0)
        integer(int64) :: k
        !
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_null()
        call col%append_string("dddd")
        call col%append_string("")
        call col%append_string("ffffffff")
        oracle = col%clone()
        valid = [.false., .true., .true., .false., .true., .false.]
        do k = 1_int64, 6_int64
            if (.not. valid(k)) call oracle%set_null(k)
        end do
        call col%set_validity(valid)
        call check(error, col%validate(), "set_validity must leave the column's invariants intact")
        if (allocated(error)) return
        call check(error, col%null_count() == 4_int64, "three new nulls beside the one already there")
        if (allocated(error)) return
        call check_columns_agree(error, col, oracle, "set_validity against a set_null per .false. entry")
        if (allocated(error)) return
        call check(error, col%is_null(3_int64), &
            "a .true. entry must not resurrect the null that was already there")
        if (allocated(error)) return
        call col%get(2_int64, s)
        call check(error, s == "bb", "a kept element must keep its bytes after the first one was dropped")
        if (allocated(error)) return
        call col%get(5_int64, s)
        call check(error, s == "" .and. .not. col%is_null(5_int64), &
            "a zero-length element that stays valid stays valid")
        if (allocated(error)) return
        call check(error, col%character_size() == 2_int64, "the payload must shrink to the kept bytes")
        if (allocated(error)) return
        !
        ! An all-true mask is a no-op that touches no buffer: offsets and payload byte-identical.
        allocate(off_a(col%size() + 1_int64), dat_a(max(col%character_size(), 1_int64)))
        allocate(off_b(col%size() + 1_int64), dat_b(max(col%character_size(), 1_int64)))
        call col%copy_buffers(off_a, dat_a)
        valid = .true.
        call col%set_validity(valid)
        call col%copy_buffers(off_b, dat_b)
        call check(error, all(off_a == off_b), "an all-true mask must leave the offsets byte-identical")
        if (allocated(error)) return
        call check(error, all(dat_a == dat_b), "an all-true mask must leave the payload byte-identical")
        if (allocated(error)) return
        call check(error, col%null_count() == 4_int64, "an all-true mask must change no null")
        if (allocated(error)) return
        !
        ! The same mask again: an element that is already null is not counted twice.
        valid = [.false., .true., .true., .false., .true., .false.]
        call col%set_validity(valid)
        call check(error, col%null_count() == 4_int64, "re-nulling a null must not double-count")
        if (allocated(error)) return
        !
        ! A column with no rows takes an empty mask.
        call empty%set_validity(none)
        call check(error, empty%size() == 0_int64 .and. empty%validate(), "an empty column accepts an empty mask")
    end subroutine test_set_validity_bulk
    !
    !> `set_where` is `set` over a whole mask in one rebuild, so the oracle is that loop on a
    !! clone. The value is LONGER than every element (the payload grows, which is what rules an
    !! in-place walk out), the mask names an element that already holds a value (the MASK selects,
    !! not the null state), a null one, and the first and the last elements.
    subroutine test_set_where_bulk(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col, oracle, empty
        integer(int64), allocatable :: off_a(:), off_b(:)
        character(len=1), allocatable :: dat_a(:), dat_b(:)
        character(len=:), allocatable :: s
        logical :: mask(6), none(0)
        integer(int64) :: k
        !
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_null()
        call col%append_string("dddd")
        call col%append_string("")
        call col%append_string("ffffffff")
        oracle = col%clone()
        mask = [.true., .false., .true., .false., .false., .true.]
        do k = 1_int64, 6_int64
            if (mask(k)) call oracle%set(k, "replacement value")
        end do
        call col%set_where(mask, "replacement value")
        call check(error, col%validate(), "set_where must leave the column's invariants intact")
        if (allocated(error)) return
        call check(error, col%null_count() == 0_int64, "the selected null must be valid afterwards")
        if (allocated(error)) return
        call check_columns_agree(error, col, oracle, "set_where against a set per .true. entry")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "replacement value", "an element that held a value takes the value too")
        if (allocated(error)) return
        call col%get(3_int64, s)
        call check(error, s == "replacement value" .and. .not. col%is_null(3_int64), &
            "the null element takes the value and becomes valid")
        if (allocated(error)) return
        call col%get(2_int64, s)
        call check(error, s == "bb", "an unselected element keeps its bytes after its predecessor grew")
        if (allocated(error)) return
        call check(error, col%character_size() == 3_int64*17_int64 + 2_int64 + 4_int64, &
            "the payload is the kept bytes plus one value per selected element")
        if (allocated(error)) return
        !
        ! The empty value: the selected element becomes zero-length AND valid.
        call col%set_null(2_int64)
        call col%set_where([.false., .true., .false., .false., .false., .false.], "")
        call check(error, col%is_empty(2_int64) .and. .not. col%is_null(2_int64), &
            "an empty value leaves a valid zero-length element")
        if (allocated(error)) return
        ! A value with trailing blanks is stored verbatim -- a scalar is never trimmed.
        call col%set_where([.false., .false., .false., .false., .true., .false.], "x  ")
        call check(error, col%length(5_int64) == 3_int64, "a scalar value is stored verbatim, blanks included")
        if (allocated(error)) return
        !
        ! An all-false mask is a no-op that touches no buffer.
        allocate(off_a(col%size() + 1_int64), dat_a(max(col%character_size(), 1_int64)))
        allocate(off_b(col%size() + 1_int64), dat_b(max(col%character_size(), 1_int64)))
        call col%copy_buffers(off_a, dat_a)
        mask = .false.
        call col%set_where(mask, "never written")
        call col%copy_buffers(off_b, dat_b)
        call check(error, all(off_a == off_b), "an all-false mask must leave the offsets byte-identical")
        if (allocated(error)) return
        call check(error, all(dat_a == dat_b), "an all-false mask must leave the payload byte-identical")
        if (allocated(error)) return
        !
        ! A column that never held a null needs no bitmap for this and gets none.
        call oracle%clear()
        call oracle%append_string("p")
        call oracle%append_string("qq")
        call oracle%set_where([.true., .false.], "rrr")
        call check(error, .not. oracle%has_validity(), "a null-free column stays bitmap-free through set_where")
        if (allocated(error)) return
        call oracle%get(1_int64, s)
        call check(error, s == "rrr" .and. oracle%null_count() == 0_int64, "and still takes the value")
        if (allocated(error)) return
        !
        ! A column with no rows takes an empty mask.
        call empty%set_where(none, "v")
        call check(error, empty%size() == 0_int64 .and. empty%validate(), "an empty column accepts an empty mask")
    end subroutine test_set_where_bulk
    !
    !> append_nulls is the bulk counterpart of calling append_null n times -- one capacity
    !> growth instead of n.
    subroutine test_append_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        !
        call col%append_string("a")
        call col%append_nulls(3_int64)
        call check(error, col%size() == 4_int64, "append_nulls should add the requested element count")
        if (allocated(error)) return
        call check(error, col%null_count() == 3_int64, "append_nulls should count every appended null")
        if (allocated(error)) return
        call check(error, col%is_null(4_int64), "the appended elements should read back as null")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "a", "append_nulls must not disturb the existing payload")
        if (allocated(error)) return
        call col%append_nulls(0_int64)
        call check(error, col%size() == 4_int64, "append_nulls(0) should be a no-op")
        if (allocated(error)) return
        call check(error, col%validate(), "append_nulls must leave the column's invariants intact")
        if (allocated(error)) return
        call col%append_nulls(2)
        call check(error, col%size() == 6_int64, "the int32 append_nulls specific should append too")
    end subroutine test_append_nulls
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
    !> `%build_from` over a character ARRAY: the bulk fill that `parquet_column%set_all` routes
    !> through, and the reason `%set_all` on a 1M-row column went from 65 ms to 10 ms (S7-5).
    !>
    !> It reaches the payload by re-seeing the caller's `character(len=w)` array as one `w*n` byte
    !> block through a sequence-associated `character(len=1) :: src(*)` dummy, so **every assertion
    !> here is really about arithmetic on that block**: get `base = (k-1)*w` or the section bounds
    !> wrong and elements come back shifted, truncated, or holding a neighbour's bytes. Nothing
    !> aborts when that happens -- the column still validates -- so the values are checked one by
    !> one rather than through a size or a null count.
    !>
    !> **The first element is deliberately the SHORTEST.** `.claude/rules/testing.md`'s "sized/typed from the first
    !> element" bug class applies directly: a length derived from `values(1)` instead of per element
    !> would truncate everything after it, and a fixture whose first element is longest passes such
    !> a bug happily.
    !>
    !> An `is_null` mask is checked with elements that still need trimming, because the mask path is
    !> a separate branch of the sizing pass -- a mutation dropping `len_trim` there survived a suite
    !> that only ever combined the mask with already-trimmed values.
    subroutine test_build_from_character(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col, ref
        character(len=6) :: vals(5)
        logical :: mask(5)
        character(len=:), allocatable :: s
        integer :: k

        ! shortest first, one empty, trailing blanks everywhere by construction (len=6)
        vals = [character(len=6) :: "a", "bb", "", "dddd", "eeeee"]
        call col%append_string("stale")          ! must be cleared, as the handles form is
        call col%build_from(vals)
        call check(error, col%size() == 5_int64, "build_from(character) must replace, not append")
        if (allocated(error)) return
        do k = 1, 5
            call col%get(int(k, int64), s)
            call check(error, s == trim(vals(k)), &
                "build_from(character) element "//char(48+k)//" must be its trimmed source value")
            if (allocated(error)) return
        end do
        call check(error, col%character_size() == 12_int64, &
            "build_from(character) must store 1+2+0+4+5 = 12 bytes, not the padded 30")
        if (allocated(error)) return
        call check(error, col%null_count() == 0_int64, &
            "build_from(character) without a mask must produce no nulls")
        if (allocated(error)) return

        ! Equivalence with the per-element form it replaced: same bytes, same offsets, same values.
        do k = 1, 5
            call ref%append_string(vals(k), trim=.true.)
        end do
        call check(error, col%character_size() == ref%character_size() .and. &
            col%size() == ref%size(), &
            "build_from(character) must agree with a column built by %append_string(trim=.true.)")
        if (allocated(error)) return
        do k = 1, 5
            call ref%get(int(k, int64), s)
            call check(error, col%equals(int(k, int64), s), &
                "build_from(character) element "//char(48+k)//" must equal the appended column's")
            if (allocated(error)) return
        end do

        ! is_null: masked elements become zero-width nulls, and the UNMASKED ones still trim.
        mask = [.false., .true., .false., .true., .false.]
        call col%build_from(vals, is_null=mask)
        call check(error, col%null_count() == 2_int64, &
            "build_from(character) with a mask must record the null count")
        if (allocated(error)) return
        call check(error, col%is_null(2_int64) .and. col%is_null(4_int64), &
            "build_from(character) must mark exactly the masked elements null")
        if (allocated(error)) return
        call check(error, .not. col%is_null(1_int64) .and. .not. col%is_null(5_int64), &
            "build_from(character) must leave the unmasked elements non-null")
        if (allocated(error)) return
        call col%get(5_int64, s)
        call check(error, s == "eeeee", &
            "build_from(character) must still trim an unmasked element when a mask is present")
        if (allocated(error)) return
        call check(error, col%character_size() == 6_int64, &
            "build_from(character) with a mask must store only the unmasked bytes (1+0+5)")
        if (allocated(error)) return
        call check(error, col%validate(), "build_from(character) with a mask: invariants hold")
        if (allocated(error)) return

        ! degenerate shapes: an all-null mask, an all-empty array, and a zero-length array
        mask = .true.
        call col%build_from(vals, is_null=mask)
        call check(error, col%size() == 5_int64 .and. col%null_count() == 5_int64 .and. &
            col%character_size() == 0_int64, "build_from(character): an all-null mask")
        if (allocated(error)) return
        vals = ""
        call col%build_from(vals)
        call check(error, col%size() == 5_int64 .and. col%character_size() == 0_int64 .and. &
            col%null_count() == 0_int64, "build_from(character): an all-empty array is 5 empty rows")
        if (allocated(error)) return
        block
            character(len=6) :: none(0)
            call col%build_from(none)
            call check(error, col%size() == 0_int64 .and. col%empty(), &
                "build_from(character): a zero-length array leaves an empty column")
        end block
    end subroutine test_build_from_character
    !
    !> `build_from` and `append_values` (the binding and the module procedure) take a STRIDED
    !> character array and `is_null` -- stride-2 sections, with junk between their elements that
    !> reads as a blank-padded "junk" value and a `.true.` null flag -- and store what contiguous
    !> ones hold.
    !>
    !> Neither declares its `values` contiguous: a strided one is copied into an allocatable by the
    !> procedure itself. A `contiguous` dummy would have the compiler copy it at the CALLER's call,
    !> on the stack under ifx, and gfortran copies even a contiguous array passed to one from an
    !> assumed-shape dummy.
    subroutine test_strided_character_arrays(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=6) :: vals(10)
        logical :: mask(10)
        character(len=:), allocatable :: want(:)
        integer :: k

        vals = "junk"
        mask = .true.
        do k = 1, 5
            write (vals(2*k - 1), "(a,i0)") "v", k
            mask(2*k - 1) = k == 2
        end do

        call col%build_from(vals(1::2))
        want = [character(len=2) :: "v1", "v2", "v3", "v4", "v5"]
        call expect_column(col, want, [integer :: ], "build_from", error)
        if (allocated(error)) return

        call col%build_from(vals(1::2), is_null=mask(1::2))
        call expect_column(col, want, [2], "build_from(is_null=)", error)
        if (allocated(error)) return

        call col%append_values(vals(1:5:2))
        call col%append_values(vals(1:5:2), is_null=mask(1:5:2))
        call parquet_string_column_append_values(col, vals(7:9:2))
        want = [character(len=2) :: "v1", "v2", "v3", "v4", "v5", "v1", "v2", "v3", "v1", "v2", "v3", "v4", "v5"]
        call expect_column(col, want, [2, 10], "append_values", error)
    end subroutine test_strided_character_arrays
    !
    !> Checks that `col` holds exactly `want`, element by element, with the elements listed in
    !> `nulls` null and every other one non-null.
    subroutine expect_column(col, want, nulls, what, error)
        type(parquet_string_column), intent(in) :: col !! the column under test.
        character(len=*), intent(in) :: want(:) !! every element's value; a null one's is not compared.
        integer, intent(in) :: nulls(:) !! the 1-based indices of the null elements.
        character(len=*), intent(in) :: what !! the call that built `col`, for the message.
        type(error_type), allocatable, intent(inout) :: error !! set on the first difference.
        character(len=:), allocatable :: s
        integer :: k

        call check(error, col%size() == size(want, kind=int64), what // ": wrong element count")
        if (allocated(error)) return
        call check(error, col%null_count() == size(nulls, kind=int64), what // ": wrong null count")
        if (allocated(error)) return
        do k = 1, size(want)
            if (any(nulls == k)) then
                call check(error, col%is_null(int(k, int64)), what // ": an element passed as null is not null")
            else
                call col%get(int(k, int64), s)
                call check(error, s == trim(want(k)), what // ": an element read back wrong")
            end if
            if (allocated(error)) return
        end do
    end subroutine expect_column
    !
    !> `%append_values`: the appending counterpart of `build_from`'s character form, and the entry
    !> point `parquet_column%append_values` now routes through.
    !>
    !> **Everything here is about appending to a column that is not empty**, because that is the
    !> only thing separating it from `build_from`: it continues the offset scan from `self%nchars`
    !> and writes validity bits at `nrows + k` rather than `k`. Both of those look right when the
    !> destination starts empty — every base is 0 — so a test that only appends to a fresh column
    !> passes against an implementation that ignores the base entirely.
    !>
    !> The `is_null` cases are here rather than left to the `parquet_column` layer because that
    !> layer never passes a mask: `%append_values` on a `parquet_column` has no null argument, so
    !> this optional is reachable only through `parquet_string_column` directly, and is untested
    !> anywhere else.
    subroutine test_append_values_character(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=5) :: first(3), more(2)
        logical :: mask(2)
        character(len=:), allocatable :: s

        first = [character(len=5) :: "a", "bb", "ccc"]
        more  = [character(len=5) :: "dddd", "e"]

        ! --- append onto a NON-EMPTY column: offsets must continue, not restart ---
        call col%build_from(first)
        call col%append_values(more)
        call check(error, col%size() == 5_int64, "append_values must extend, not replace")
        if (allocated(error)) return
        call check(error, col%character_size() == 1+2+3+4+1, &
            "append_values must add its bytes to the existing payload, not replace the count")
        if (allocated(error)) return
        call col%get(1_int64, s)
        call check(error, s == "a", "append_values must leave the pre-existing element 1 intact")
        if (allocated(error)) return
        call col%get(3_int64, s)
        call check(error, s == "ccc", "append_values must leave the pre-existing element 3 intact")
        if (allocated(error)) return
        call col%get(4_int64, s)
        call check(error, s == "dddd", "append_values element 4 must be the first appended value")
        if (allocated(error)) return
        call col%get(5_int64, s)
        call check(error, s == "e", "append_values element 5 must be trimmed")
        if (allocated(error)) return
        call check(error, col%validate(), "append_values onto a non-empty column: invariants hold")
        if (allocated(error)) return

        ! --- append onto a column that ALREADY has nulls: the new rows must come back valid ---
        call col%clear()
        call col%build_from(first, is_null=[.false., .true., .false.])
        call col%append_values(more)
        call check(error, col%null_count() == 1_int64, &
            "appending non-null values must not change the existing null count")
        if (allocated(error)) return
        call check(error, col%is_null(2_int64), "the pre-existing null must stay null")
        if (allocated(error)) return
        call check(error, .not. col%is_null(4_int64) .and. .not. col%is_null(5_int64), &
            "rows appended onto a null-carrying column must be valid, not whatever the bitmap held")
        if (allocated(error)) return

        ! --- append WITH a mask onto a non-empty column: bits go at nrows + k ---
        call col%clear()
        call col%build_from(first)
        mask = [.true., .false.]
        call col%append_values(more, is_null=mask)
        call check(error, col%size() == 5_int64 .and. col%null_count() == 1_int64, &
            "append_values with a mask must record exactly one new null")
        if (allocated(error)) return
        call check(error, col%is_null(4_int64), &
            "append_values must mark the masked element at nrows+k, not at k")
        if (allocated(error)) return
        call check(error, .not. col%is_null(1_int64) .and. .not. col%is_null(5_int64), &
            "append_values must leave every other element non-null")
        if (allocated(error)) return
        call col%get(5_int64, s)
        call check(error, s == "e", "append_values must still trim an unmasked element under a mask")
        if (allocated(error)) return
        call check(error, col%character_size() == 1+2+3+1, &
            "a masked appended element must contribute no bytes")
        if (allocated(error)) return

        ! --- accumulate across several appends, each carrying its own null ---
        call col%clear()
        call col%append_values(more, is_null=[.true., .false.])
        call col%append_values(more, is_null=[.false., .true.])
        call check(error, col%size() == 4_int64 .and. col%null_count() == 2_int64, &
            "append_values must ACCUMULATE the null count across calls, not overwrite it")
        if (allocated(error)) return
        call check(error, col%is_null(1_int64) .and. col%is_null(4_int64), &
            "each append's own null must land in its own range")
        if (allocated(error)) return
        call check(error, col%validate(), "repeated masked appends: invariants hold")
        if (allocated(error)) return

        ! --- stale validity bits ABOVE nrows, which is the only case the "already has nulls"
        !     branch exists for. ensure_validity_cap fills NEW bytes all-ones, so appending into
        !     fresh capacity is valid whether or not the branch runs; what it protects is a bit
        !     that was cleared while the column was longer and never re-set when it shrank. Without
        !     it the appended row inherits that bit and comes back null with no other symptom.
        call col%clear()
        call col%build_from(first)                              ! 3 rows
        call col%append_values(more, is_null=[.true., .true.])  ! rows 4,5 null -> bits cleared
        call col%erase(5_int64)
        call col%erase(4_int64)                                 ! back to 3 rows; bits 4,5 stale
        call col%append_values(more)                            ! rows 4,5 again, no mask
        call check(error, .not. col%is_null(4_int64) .and. .not. col%is_null(5_int64), &
            "an append must write its own rows valid, not inherit a stale cleared bit")
        if (allocated(error)) return
        call check(error, col%null_count() == 0_int64, &
            "the null count must not carry over bits belonging to erased rows")
        if (allocated(error)) return

        ! --- a zero-length append is a no-op ---
        block
            character(len=5) :: none(0)
            integer(int64) :: before
            before = col%size()
            call col%append_values(none)
            call check(error, col%size() == before, "a zero-length append_values must change nothing")
        end block

        ! --- every element blank, onto an EMPTY column: the payload is never allocated ---
        ! `ensure_data_cap` deliberately allocates nothing for a zero-byte payload, so `self%data`
        ! is still unallocated when `pack_character_bytes` is called -- and that routine takes its
        ! destination as an ordinary (non-allocatable) dummy, so a stand-in must be passed instead.
        ! **Both conditions are needed**: every element blank AND the column empty, since any
        ! earlier append has already allocated the payload, which is why the two blocks below run
        ! in this order rather than reusing `col` from above.
        !
        ! gfortran runs the non-conforming version of this silently; nagfor's `-C=array` is what
        ! reports it ("ALLOCATABLE SELF%DATA is not currently allocated"). So the values asserted
        ! here are not what this is guarding -- passing at all is.
        block
            type(parquet_string_column) :: blank_col
            character(len=5) :: blanks(3)
            blanks = [character(len=5) :: "     ", "     ", "     "]
            call blank_col%append_values(blanks)
            call check(error, blank_col%size() == 3_int64, &
                "appending three blank elements onto an empty column must give three rows")
            if (allocated(error)) return
            call check(error, blank_col%character_size() == 0_int64, &
                "three blank elements must contribute no bytes at all")
            if (allocated(error)) return
            call blank_col%get(2_int64, s)
            call check(error, len(s) == 0, "a blank element must read back as the empty string")
            if (allocated(error)) return
            call check(error, .not. blank_col%is_null(2_int64), &
                "a blank element is EMPTY, not null -- the two are distinct here")
            if (allocated(error)) return
            call check(error, blank_col%validate(), "all-blank append onto an empty column: invariants hold")
            if (allocated(error)) return
            ! The control: the same append onto a column that already holds bytes takes the OTHER
            ! branch, with `self%data` allocated. Without it a stand-in passed unconditionally
            ! would satisfy every assertion above.
            call blank_col%clear()
            call blank_col%build_from(first)
            call blank_col%append_values(blanks)
            call check(error, blank_col%size() == 6_int64 .and. blank_col%character_size() == 1+2+3, &
                "blank elements appended onto a non-empty column must add rows but no bytes")
            if (allocated(error)) return
            call blank_col%get(1_int64, s)
            call check(error, s == "a", "the pre-existing bytes must survive an all-blank append")
            if (allocated(error)) return
            call check(error, blank_col%validate(), "all-blank append onto a non-empty column: invariants hold")
        end block
    end subroutine test_append_values_character
    !
    !> `build_from` sizes the destination from a validation pass and then copies each element's
    !! bytes straight out of ITS OWN source column, so these are the cases that separate a correct
    !! fill from a plausible one: an **empty** handle array (which must leave the destination as
    !! `%clear()` did, allocating nothing), an **all-null** array (no payload at all, so the data
    !! buffer is never allocated and every validity bit must still be written), **zero-length**
    !! elements, and **one element gathered twice**, which a length sum computed per source row
    !! rather than per handle would get wrong.
    !!
    !! `character_size()` is asserted throughout, because it is the one field a wrong length sum
    !! corrupts silently -- the strings still read back correctly right up until the column is
    !! written or appended to.
    !!
    !! Shortest element first, per CLAUDE.md's rule for this bug class.
    subroutine test_build_from_edges(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column), target :: src, other, dest
        type(parquet_string) :: none(0), nulls(3), mixed(6)
        character(len=:), allocatable :: s
        integer :: i
        logical :: ok
        !
        call src%append_string("a")       ! shortest first
        call src%append_string("")        ! zero length
        call src%append_null()
        call src%append_string("bcdef")
        call other%append_string("XY")
        !
        ! --- an empty handle array leaves the destination exactly as %clear() would ---
        call dest%append_string("stale")
        call dest%build_from(none)
        call check(error, dest%size() == 0, "build_from of an empty array empties the destination")
        if (allocated(error)) return
        call check(error, dest%character_size() == 0, "and its payload is empty")
        if (allocated(error)) return
        call check(error, dest%capacity() == 0, "and it holds no capacity")
        if (allocated(error)) return
        call check(error, dest%validate(), "empty build_from leaves a valid column")
        if (allocated(error)) return
        !
        ! --- all null: no payload is ever allocated, every validity bit must still be written ---
        do i = 1, 3
            nulls(i) = src%view(3_int64)
        end do
        call dest%build_from(nulls)
        call check(error, dest%size() == 3, "all-null build_from row count")
        if (allocated(error)) return
        call check(error, dest%null_count() == 3, "all-null build_from null_count")
        if (allocated(error)) return
        call check(error, dest%character_size() == 0, "all-null build_from stores no bytes")
        if (allocated(error)) return
        ok = dest%is_null(1_int64) .and. dest%is_null(2_int64) .and. dest%is_null(3_int64)
        call check(error, ok, "every row of an all-null build_from reads back null")
        if (allocated(error)) return
        call check(error, dest%validate(), "all-null build_from leaves a valid column")
        if (allocated(error)) return
        !
        ! --- zero-length elements, a repeat, a null, and a second source column ---
        mixed(1) = src%view(1_int64)      ! "a"
        mixed(2) = src%view(2_int64)      ! ""
        mixed(3) = src%view(4_int64)      ! "bcdef"
        mixed(4) = src%view(2_int64)      ! "" again
        mixed(5) = src%view(4_int64)      ! "bcdef" AGAIN -- the same element twice
        mixed(6) = other%view(1_int64)    ! "XY" from a different column
        call dest%build_from(mixed)
        call check(error, dest%size() == 6, "mixed build_from row count")
        if (allocated(error)) return
        ! 1 + 0 + 5 + 0 + 5 + 2 -- the repeat is counted twice, as a gather must
        call check(error, dest%character_size() == 13, "mixed build_from payload sums every handle")
        if (allocated(error)) return
        call check(error, dest%null_count() == 0, "mixed build_from has no nulls")
        if (allocated(error)) return
        call dest%get(1, s)
        call check(error, s == "a", "mixed row 1")
        if (allocated(error)) return
        call check(error, dest%length(2_int64) == 0, "mixed row 2 is zero-length")
        if (allocated(error)) return
        call dest%get(3, s)
        call check(error, s == "bcdef", "mixed row 3")
        if (allocated(error)) return
        call check(error, dest%length(4_int64) == 0, "mixed row 4 is zero-length")
        if (allocated(error)) return
        call dest%get(5, s)
        call check(error, s == "bcdef", "mixed row 5 -- the repeated element")
        if (allocated(error)) return
        call dest%get(6, s)
        call check(error, s == "XY", "mixed row 6, from a different source column")
        if (allocated(error)) return
        call check(error, dest%validate(), "mixed build_from invariants hold")
    end subroutine test_build_from_edges
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
    !> `to_character` copies each element's payload bytes directly rather than through `%get`, so
    !! `%get` is now an INDEPENDENT oracle for it rather than the same code path -- this asserts
    !! the two agree element for element, and that every byte past an element's own length is a
    !! blank.
    !!
    !! Three things this covers that `test_to_character` does not, each of which a plausible
    !! defect in the direct-copy loop would pass: the **no-`null_value`** path (the common case,
    !! and the one that reaches the copy loop for every row); a **zero-length** element, which the
    !! copy skips entirely and must therefore leave wholly blank; and a **`null_value` longer than
    !! every real element**, which is what makes it rather than the data set `maxlen`.
    !!
    !! The first element is deliberately the SHORTEST, per CLAUDE.md's rule for this bug class --
    !! a fixture whose first element is longest passes even when a width is taken from element one.
    subroutine test_to_character_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: arr(:), one
        integer(int64) :: i, n, elen
        logical :: ok
        !
        ! Shortest first, an empty element in the middle, longest last.
        call col%append_string("a")
        call col%append_string("bc")
        call col%append_string("")
        call col%append_string("defgh")
        ! Lengths 1-4, deliberately all SHORTER than "defgh", so the longest element stays 5 and
        ! the null_value tested below (7) is genuinely longer than anything in the data.
        do i = 1_int64, 40_int64
            call col%append_string(repeat("z", int(mod(i, 4_int64)) + 1))
        end do
        n = col%size()
        !
        ! --- no null_value: every row goes through the direct copy ---
        call col%to_character(arr)
        call check(error, size(arr, kind=int64) == n, "to_character returns one row per element")
        if (allocated(error)) return
        call check(error, len(arr) == 5, "padded to the longest element (5)")
        if (allocated(error)) return
        ok = .true.
        do i = 1_int64, n
            call col%get(i, one)
            elen = col%length(i)
            if (arr(i)(1:elen) /= one) ok = .false.
            ! Everything past the element's own length must be blank, not stale payload.
            if (elen < len(arr)) then
                if (arr(i)(elen+1:) /= repeat(" ", len(arr) - int(elen))) ok = .false.
            end if
        end do
        call check(error, ok, "every row matches %get, blank-padded to the right of its own length")
        if (allocated(error)) return
        call check(error, arr(3) == repeat(" ", len(arr)), "a zero-length element is wholly blank")
        if (allocated(error)) return
        !
        ! --- a null_value longer than any real element sets maxlen ---
        call col%append_null()
        call col%to_character(arr, null_value="missing")
        call check(error, len(arr) == 7, "null_value longer than the data sets maxlen")
        if (allocated(error)) return
        call check(error, arr(1) == "a", "a real element still compares equal under the wider pad")
        if (allocated(error)) return
        call check(error, arr(1)(2:) == repeat(" ", 6), "and its padding really is blanks")
        if (allocated(error)) return
        call check(error, arr(size(arr)) == "missing", "the null row carries null_value")
    end subroutine test_to_character_bytes
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
    !> append_buffers' validity_offset_bits argument (the "Rebase validity" fix): when a source
    !! array is a genuinely sliced child (e.g. a struct-nested leaf resolved through a
    !! non-zero-offset StructArray::field(), see parquet_wrapper.cpp's
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
        ! print writes each element straight from the payload, so the one column shape that has
        ! NO payload has to be covered separately: when every element is zero-length, `data` is
        ! never allocated at all and an unguarded slice of it would reference an unallocated
        ! array. gfortran tolerates that and ifx need not, so this is a portability guard rather
        ! than something a local run would catch failing.
        block
            type(parquet_string_column) :: allempty
            call allempty%append_string("")
            call allempty%append_string("")
            call check(error, allempty%character_size() == 0, "an all-empty column stores no bytes")
            if (allocated(error)) return
            open(newunit=u, status='scratch')
            call allempty%print(unit=u)
            close(u)
            call check(error, allempty%validate(), "printing an all-empty column leaves it valid")
            if (allocated(error)) return
        end block
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

    !> **The one correctness rule of the threading layer, tested before anything threads.**
    !!
    !! The validity bitmap packs 8 rows per byte, so two threads whose ranges meet inside a byte
    !! race on that byte -- a read-modify-write each, one lost. Nothing aborts and the column still
    !! validates; the nulls are just wrong. `thread_row_ranges` exists to make that impossible by
    !! splitting on byte boundaries, and this asserts the three properties that makes true:
    !!
    !!   * **coverage** -- every row 1..n appears in exactly one range, in order, so no row is
    !!     dropped or done twice;
    !!   * **alignment** -- every range starts at `1 mod 8`, which is what guarantees no two threads
    !!     share a byte;
    !!   * **empty ranges are legal** -- a trailing thread may get none, and callers must tolerate
    !!     that rather than assume every thread has work.
    !!
    !! Swept over row counts that straddle byte boundaries (7, 8, 9, ...) rather than round numbers,
    !! since an off-by-one in the byte arithmetic is invisible at multiples of 8.
    subroutine test_thread_row_ranges(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: n, next
        integer :: nt, k
        logical :: ok_cover, ok_align, saw_empty
        !
        ok_cover = .true.
        ok_align = .true.
        saw_empty = .false.
        do n = 0_int64, 40_int64
            do nt = 1, 6
                call parquet_debug_string_row_ranges(n, nt, lo, hi)
                if (size(lo) /= nt .or. size(hi) /= nt) then
                    ok_cover = .false.
                    cycle
                end if
                next = 1_int64
                do k = 1, nt
                    if (hi(k) < lo(k)) then
                        saw_empty = .true.
                        cycle          ! an empty range contributes nothing and must not move `next`
                    end if
                    if (lo(k) /= next) ok_cover = .false.
                    ! Every non-empty range must BEGIN on a validity byte boundary. The last one may
                    ! end mid-byte -- there is no thread after it to collide with.
                    if (mod(lo(k) - 1_int64, 8_int64) /= 0_int64) ok_align = .false.
                    next = hi(k) + 1_int64
                end do
                if (next /= n + 1_int64) ok_cover = .false.
            end do
        end do
        call check(error, ok_cover, "every row is covered exactly once, in order, for n = 0..40 and 1..6 ranges")
        if (allocated(error)) return
        call check(error, ok_align, "every non-empty range starts on a validity byte boundary")
        if (allocated(error)) return
        call check(error, saw_empty, "the sweep really did produce an empty range (else it proves nothing about them)")
        if (allocated(error)) return
        !
        ! A single range must be the whole column, whatever n is -- the serial path.
        call parquet_debug_string_row_ranges(37_int64, 1, lo, hi)
        call check(error, lo(1) == 1_int64 .and. hi(1) == 37_int64, "one range spans the whole column")
    end subroutine test_thread_row_ranges
    !

    !> `%compare(i, j)` must agree with Fortran's own `<` on the two values, for every pair.
    !!
    !! **`%get` is the independent oracle here**, and legitimately so: `%compare` reads the payload
    !! bytes directly and shares no code with it, so agreement between the two is a real cross-check
    !! rather than a tautology. That is the whole point of the procedure -- callers replace
    !! `get(i, a); get(j, b); a < b` with it to avoid two allocations per comparison, so anything
    !! less than exact agreement is a behaviour change.
    !!
    !! The fixture is built around the cases where byte comparison and Fortran comparison DIFFER, and
    !! those are all about blanks: "ab" and "ab  " are equal to Fortran and unequal byte-wise, and
    !! "ab" sorts before "abc" only because the shorter is padded. A zero-length element and a null
    !! (also zero-width) are included for the same reason.
    subroutine test_compare_matches_fortran(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: a, b
        integer(int64) :: i, j, n
        integer :: got, want
        logical :: ok
        !
        call col%append_string("ab")        ! shortest-ish first, per the project rule
        call col%append_string("ab  ")      ! equal to "ab" under Fortran padding
        call col%append_string("abc")
        call col%append_string("")          ! zero length
        call col%append_string("aa")
        call col%append_string("b")
        call col%append_string("ab a")      ! a blank INSIDE, which trimming would not remove
        call col%append_null()              ! zero-width, so it compares as ""
        n = col%size()
        !
        ok = .true.
        do i = 1_int64, n
            do j = 1_int64, n
                call col%get(i, a, allow_null=.true.)
                call col%get(j, b, allow_null=.true.)
                want = 0
                if (a < b) want = -1
                if (b < a) want = 1
                got = col%compare(i, j)
                if (got /= want) ok = .false.
            end do
        end do
        call check(error, ok, "compare(i,j) agrees with Fortran's < over every ordered pair")
        if (allocated(error)) return
        !
        ! The specific pairs the fixture exists for, asserted individually so a failure names itself.
        call check(error, col%compare(1_int64, 2_int64) == 0, "ab and 'ab  ' compare EQUAL, as Fortran does")
        if (allocated(error)) return
        call check(error, col%compare(1_int64, 3_int64) < 0, "ab sorts before abc (the shorter is blank-padded)")
        if (allocated(error)) return
        call check(error, col%compare(4_int64, 1_int64) < 0, "a zero-length element sorts before a non-empty one")
        if (allocated(error)) return
        call check(error, col%compare(8_int64, 4_int64) == 0, "a null is zero-width, so it compares equal to empty")
        if (allocated(error)) return
        call check(error, col%compare(7_int64, 3_int64) < 0, "'ab a' sorts before abc -- an interior blank is a real byte")
        if (allocated(error)) return
        call check(error, col%compare(2_int64, 2_int64) == 0, "an element compares equal to itself")
    end subroutine test_compare_matches_fortran
    !
    !> Builds a column of `n` deterministic elements, null at every `null_every`-th row.
    !!
    !! Lengths vary and the FIRST element is the shortest, per this project's standing rule for any
    !! string fixture -- a fixture whose first element is longest passes even when a length is being
    !! derived from element 1.
    subroutine build_marked(c, n, null_every)
        type(parquet_string_column), intent(inout) :: c !! receives the column.
        integer(int64), intent(in) :: n                 !! element count.
        integer(int64), intent(in) :: null_every        !! null stride; <= 0 for none.
        integer(int64) :: k, l
        character(len=32) :: buf
        call c%clear()
        do k = 1_int64, n
            if (null_every > 0_int64) then
                if (mod(k, null_every) == 0_int64) then
                    call c%append_null()
                    cycle
                end if
            end if
            l = 1_int64 + mod(k*5_int64, 11_int64)
            write(buf, '(a,i0)') repeat(char(ichar("a") + int(mod(k, 26_int64))), int(l)), k
            call c%append_string(trim(buf))
        end do
    end subroutine build_marked
    !
    !> **`slice` moves the validity bitmap in whole BYTES when its source start is 8-aligned, and
    !! bit by bit when it is not — so every bit phase of `first` has to be exercised.**
    !!
    !! The byte path has three parts (a ragged head, whole bytes, a ragged tail) and a fallback for
    !! a run whose two sides disagree on phase. `first = 1` takes the all-byte path and is what a
    !! benchmark measures; `first = 2..9` walks every other phase, where the destination starts at
    !! bit 0 and the source does not, so the whole run goes through the fallback. Lengths are swept
    !! independently so the tail lands at every position within a byte.
    !!
    !! Asserted against the SOURCE element by element, not against a second slice — an oracle built
    !! from the same code would agree with itself.
    subroutine test_slice_validity_alignment(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, dst
        character(len=:), allocatable :: a, b
        integer(int64) :: first, n, k, last, expect_nulls
        logical :: ok_null, ok_val
        !
        call build_marked(src, 200_int64, 3_int64)
        ok_null = .true.
        ok_val = .true.
        do first = 1_int64, 17_int64
            do n = 1_int64, 40_int64
                last = first + n - 1_int64
                call src%slice(first, last, dst)
                if (dst%size() /= n) then
                    call check(error, .false., "slice must produce exactly last-first+1 rows")
                    return
                end if
                expect_nulls = 0_int64
                do k = 1_int64, n
                    if (src%is_null(first+k-1_int64) .neqv. dst%is_null(k)) ok_null = .false.
                    if (src%is_null(first+k-1_int64)) then
                        expect_nulls = expect_nulls + 1_int64
                    else
                        call src%get(first+k-1_int64, a)
                        call dst%get(k, b)
                        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
                        if (len(a) /= len(b)) then
                            ok_val = .false.
                        else if (a /= b) then
                            ok_val = .false.
                        end if
                    end if
                end do
                if (dst%null_count() /= expect_nulls) then
                    call check(error, .false., "slice must recount nulls to match the rows it took")
                    return
                end if
                if (.not. dst%validate()) then
                    call check(error, .false., "a slice must satisfy the class invariants at every bit phase")
                    return
                end if
            end do
        end do
        call check(error, ok_null, "every sliced row must carry the null state of the row it came from")
        if (allocated(error)) return
        call check(error, ok_val, "every sliced row must carry the value of the row it came from")
    end subroutine test_slice_validity_alignment
    !
    !> **`append_column` writes the incoming validity run at the DESTINATION's current row count**,
    !! so the bit phase that decides between the byte path and the fallback is the destination's,
    !! not the source's.
    !!
    !! The destination is swept from empty to 17 rows so every phase is hit, in both the
    !! source-has-nulls arm (a copied run) and the source-is-clean arm (a fill, which must still
    !! write bits because the destination's bitmap can carry stale nulls). Both are asserted against
    !! the two originals, element by element.
    subroutine test_append_column_validity_alignment(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: dst, head, tail
        character(len=:), allocatable :: a, b
        integer(int64) :: d, k, expect_nulls, stride
        logical :: ok_null, ok_val
        !
        ok_null = .true.
        ok_val = .true.
        do stride = 0_int64, 4_int64, 4_int64      ! 0 = a clean source, 4 = one with nulls
            do d = 0_int64, 17_int64
                call build_marked(head, d, 3_int64)
                call build_marked(tail, 37_int64, stride)
                dst = head%clone()
                call dst%append_column(tail)
                if (dst%size() /= d + 37_int64) then
                    call check(error, .false., "append_column must add exactly the source's row count")
                    return
                end if
                expect_nulls = head%null_count() + tail%null_count()
                if (dst%null_count() /= expect_nulls) then
                    call check(error, .false., "append_column must carry both columns' null counts")
                    return
                end if
                do k = 1_int64, d
                    if (head%is_null(k) .neqv. dst%is_null(k)) ok_null = .false.
                end do
                do k = 1_int64, 37_int64
                    if (tail%is_null(k) .neqv. dst%is_null(d+k)) ok_null = .false.
                    if (.not. tail%is_null(k)) then
                        call tail%get(k, a)
                        call dst%get(d+k, b)
                        if (len(a) /= len(b)) then
                            ok_val = .false.
                        else if (a /= b) then
                            ok_val = .false.
                        end if
                    end if
                end do
                if (.not. dst%validate()) then
                    call check(error, .false., "an appended column must satisfy the class invariants")
                    return
                end if
            end do
        end do
        call check(error, ok_null, "appending must preserve both sides' null states at every bit phase")
        if (allocated(error)) return
        call check(error, ok_val, "appending must preserve the source's values at every bit phase")
    end subroutine test_append_column_validity_alignment
    !
    !> **`statistics`' min/max are computed as reductions, so "no valid element" needs a sentinel
    !! rather than a first-iteration special case** — and the sentinel is the part a reduction can
    !! get wrong silently.
    !!
    !! Two ways to have nothing to measure: a column of nothing but nulls, and an empty column. Both
    !! must report `min_len == 0` and `max_len == 0`, which is what the flag-based loop this replaced
    !! produced. A sentinel left unconverted would surface as `huge(0_int64)` and `-1`, which no
    !! caller could tell from a real answer without knowing to look.
    subroutine test_statistics_no_valid_elements(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer(int64) :: mn, mx, i
        !
        do i = 1_int64, 20_int64
            call col%append_null()
        end do
        call col%statistics(min_len=mn, max_len=mx)
        call check(error, mn == 0, "an all-null column reports min_len 0, not the sentinel")
        if (allocated(error)) return
        call check(error, mx == 0, "an all-null column reports max_len 0, not the sentinel")
        if (allocated(error)) return
        !
        call col%clear()
        call col%statistics(min_len=mn, max_len=mx)
        call check(error, mn == 0, "an empty column reports min_len 0")
        if (allocated(error)) return
        call check(error, mx == 0, "an empty column reports max_len 0")
        if (allocated(error)) return
        !
        ! ...and one valid element among the nulls must be measured normally, so the sentinel is not
        ! simply swallowing every answer.
        call col%clear()
        call col%append_null()
        call col%append_string("abcd")
        call col%append_null()
        call col%statistics(min_len=mn, max_len=mx)
        call check(error, mn == 4, "a single valid element sets min_len")
        if (allocated(error)) return
        call check(error, mx == 4, "a single valid element sets max_len")
    end subroutine test_statistics_no_valid_elements
    !
    !> **The sizing pass is a `max` reduction seeded with `len(null_value)`**, and an OpenMP
    !! reduction combining only the threads' own partial results — never the variable's incoming
    !! value — would drop that seed.
    !!
    !! The failure is specific and quiet: with a `null_value` LONGER than every real element, the
    !! result would be padded to the longest element instead, and the substituted string would come
    !! back truncated. Every other test here uses a short `null_value`, where the seed is dominated
    !! by a real element and the bug cannot show.
    subroutine test_to_character_long_null_value(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: arr(:)
        integer(int64) :: k
        character(len=*), parameter :: NV = "MISSING-VALUE-PLACEHOLDER"
        !
        ! Short elements only, and the first is the shortest, per the project's fixture rule.
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_null()
        do k = 1_int64, 300_int64                 ! enough rows to reach the threaded path
            call col%append_string("ccc")
        end do
        call col%append_null()
        !
        call col%to_character(arr, null_value=NV)
        call check(error, len(arr) == len(NV), &
            "the padded width must be the null_value's length when it is the longest value present")
        if (allocated(error)) return
        call check(error, arr(3) == NV, "the null element must carry the whole null_value, untruncated")
        if (allocated(error)) return
        call check(error, arr(1) == "a", "a real element is still blank-padded to the same width")
        if (allocated(error)) return
        call check(error, size(arr) == int(col%size()), "to_character returns one row per element")
    end subroutine test_to_character_long_null_value
    !
    !> **`%copy_to` exists to replace `call c%get(i, s); dest = s`, so what it must reproduce is that
    !! pair exactly** — including the two parts of it that are easy to get wrong because Fortran does
    !! them silently: blank-padding a short value, and TRUNCATING one too long for the slot.
    !!
    !! Asserted against the pair itself, element by element, over slots deliberately shorter than,
    !! equal to and longer than the values. A `%copy_to` that aborted on a long value instead of
    !! truncating would be defensible in isolation and wrong as a replacement, which is why the
    !! oracle is the expression it replaces rather than a hand-written expectation.
    subroutine test_copy_to_matches_get(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        character(len=:), allocatable :: s
        character(len=1) :: d1
        character(len=3) :: d3
        character(len=8) :: d8
        character(len=1) :: e1
        character(len=3) :: e3
        character(len=8) :: e8
        character(len=16) :: canary
        integer(int64) :: i
        logical :: ok
        !
        call col%append_string("a")            ! shortest first, per the project fixture rule
        call col%append_string("ab")
        call col%append_string("abc")
        call col%append_string("abcdefgh")
        call col%append_string("")
        call col%append_null()
        !
        ok = .true.
        do i = 1_int64, col%size()
            call col%get(i, s, allow_null=.true.)
            e1 = s
            e3 = s
            e8 = s
            call col%copy_to(i, d1, allow_null=.true.)
            call col%copy_to(i, d3, allow_null=.true.)
            call col%copy_to(i, d8, allow_null=.true.)
            if (d1 /= e1) ok = .false.
            if (d3 /= e3) ok = .false.
            if (d8 /= e8) ok = .false.
        end do
        call check(error, ok, "copy_to must equal get-then-assign at every slot width")
        if (allocated(error)) return
        !
        ! The specific behaviours the sweep above depends on, asserted by name so a failure says
        ! which one broke.
        call col%copy_to(1_int64, d8)
        call check(error, d8 == "a       ", "a short value is blank-padded to the slot")
        if (allocated(error)) return
        ! **Truncation is asserted with a canary, not by reading the slot.** Checking only
        ! `d3 == "abc"` passes just as happily against a `copy_to` that writes all eight bytes and
        ! overruns the slot — the first three are still correct. Handing it a SUBSTRING of a longer
        ! buffer makes the overrun observable: the bytes past the slot must be untouched. Verified by
        ! mutation: removing the length clamp survives the plain assertion and fails this one.
        canary = repeat("#", len(canary))
        call col%copy_to(4_int64, canary(1:3))
        call check(error, canary(1:3) == "abc", "a value longer than the slot is truncated, as assignment would")
        if (allocated(error)) return
        call check(error, canary(4:) == repeat("#", len(canary) - 3), &
            "copy_to must not write a single byte past the slot it was given")
        if (allocated(error)) return
        call col%copy_to(6_int64, d3, allow_null=.true.)
        call check(error, d3 == "   ", "a null with allow_null yields blanks")
        if (allocated(error)) return
        call col%copy_to(5_int64, d3, allow_null=.true.)
        call check(error, d3 == "   ", "an empty element yields blanks")
    end subroutine test_copy_to_matches_get
    !
    !> **`%append_from` replaces `call src%get(i, s); call dst%append_string(s)`, and additionally
    !! carries the null state** — which is what lets a copy loop drop its `is_null` fork.
    !!
    !! The oracle is a second column built the old way, compared element for element. Nulls are
    !! placed so that the destination crosses a validity byte boundary while being built, since a
    !! destination that only ever grows into fresh bytes would not exercise the append path's own
    !! bookkeeping.
    subroutine test_append_from_matches_get(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, viaget, viafrom
        character(len=:), allocatable :: a, b
        integer(int64) :: k, n
        logical :: ok
        !
        n = 37_int64
        do k = 1_int64, n
            if (mod(k, 5_int64) == 0_int64) then
                call src%append_null()
            else
                call src%append_string(repeat(char(ichar("a") + int(mod(k, 26_int64))), int(1 + mod(k, 7_int64))))
            end if
        end do
        !
        do k = 1_int64, n
            ! The old shape, kept here as the oracle rather than in the library.
            if (src%is_null(k)) then
                call viaget%append_null()
            else
                call src%get(k, a)
                call viaget%append_string(a)
            end if
            call viafrom%append_from(src, k)
        end do
        !
        call check(error, viafrom%size() == viaget%size(), "append_from must append exactly one element per call")
        if (allocated(error)) return
        call check(error, viafrom%null_count() == viaget%null_count(), &
            "append_from must carry the source element's null state")
        if (allocated(error)) return
        call check(error, viafrom%character_size() == viaget%character_size(), &
            "append_from must copy exactly the source bytes")
        if (allocated(error)) return
        ok = .true.
        do k = 1_int64, n
            if (viafrom%is_null(k) .neqv. viaget%is_null(k)) ok = .false.
            if (viafrom%is_null(k)) cycle
            call viaget%get(k, a)
            call viafrom%get(k, b)
            if (len(a) /= len(b)) then
                ok = .false.
            else if (a /= b) then
                ok = .false.
            end if
        end do
        call check(error, ok, "append_from must equal get-then-append element for element")
        if (allocated(error)) return
        call check(error, viafrom%validate(), "a column built with append_from satisfies the class invariants")
    end subroutine test_append_from_matches_get
    !

    !> Every element index in this module's public surface comes in an int32 and an int64 form, so
    !! that a plain `integer` loop variable compiles (CLAUDE.md's both-kinds rule). Three of those
    !! int32 converters had no caller at all -- the suite reached them only through `_int64`
    !! literals -- so a converter that dropped or mistyped its argument would not have been caught.
    !!
    !! Each is checked against its int64 twin on the same data rather than against a hand-written
    !! expectation, which is what makes "the two forms are the same operation" the actual assertion.
    subroutine test_default_integer_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_string_column) :: c, d, e
        character(len=8) :: slot32, slot64
        integer :: perm32(3)
        integer(int64) :: perm64(3)
        character(len=:), allocatable :: s
        !
        call c%append_string("alpha")
        call c%append_string("bravo")
        call c%append_string("charlie")
        !
        ! copy_to: the int32 form must land the same bytes in the same slot as the int64 one.
        slot32 = "########"
        slot64 = "########"
        call c%copy_to(2, slot32)
        call c%copy_to(2_int64, slot64)
        call check(error, slot32 == slot64, "copy_to's int32 form must agree with its int64 twin")
        if (allocated(error)) return
        call check(error, trim(slot32) == "bravo", "and must copy the element it names, not another")
        if (allocated(error)) return
        !
        ! append_from: same element, appended by each form; the two results must be identical.
        call d%append_from(c, 3)
        call e%append_from(c, 3_int64)
        call check(error, d%size() == 1_int64 .and. e%size() == 1_int64, &
            "append_from must append exactly one element through either form")
        if (allocated(error)) return
        call d%get(1_int64, s)
        call check(error, s == "charlie", "append_from's int32 form must take the element it names")
        if (allocated(error)) return
        call e%get(1_int64, s)
        call check(error, s == "charlie", "and its int64 twin must take the same one")
        if (allocated(error)) return
        !
        ! reindex_trusted: a reversal, applied through each form to an identical column.
        perm32 = [3, 1, 2]
        perm64 = int(perm32, int64)
        call e%clear()
        call e%append_string("alpha")
        call e%append_string("bravo")
        call e%append_string("charlie")
        call c%reindex_trusted(perm32)
        call e%reindex_trusted(perm64)
        call c%get(1_int64, s)
        call check(error, s == "charlie", "reindex_trusted's int32 form must apply the permutation")
        if (allocated(error)) return
        call e%get(1_int64, s)
        call check(error, s == "charlie", "and its int64 twin must apply the same one")
        if (allocated(error)) return
        call c%get(3_int64, s)
        call check(error, s == "bravo", "the whole permutation must be applied, not just its first entry")
        if (allocated(error)) return
        call check(error, c%validate(), "the reindexed column must still satisfy the class invariants")
    end subroutine test_default_integer_forms
    !
    !> When two elements agree over their common prefix, the longer one's remaining bytes are
    !! compared against BLANKS -- which is what Fortran's own padding rule does, so `"ab" < "ab "`
    !! is false and `"ab" < "abx"` is true. Every fixture in the suite uses printable trailing
    !! bytes, which are all ABOVE a blank, so the below-a-blank arm of that comparison never ran on
    !! either side.
    !!
    !! A tab (below a blank) makes the longer element sort BEFORE the shorter one, which is the
    !! opposite of what a length-only rule would give -- so this is a case where getting the arm
    !! wrong reverses an order rather than merely tying it.
    subroutine test_compare_trailing_byte_below_blank(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_string_column) :: c
        integer :: cmp_low, cmp_high, cmp_rev
        !
        call c%append_string("ab")             ! 1: the short one
        call c%append_string("ab" // char(9))  ! 2: longer, trailing byte BELOW a blank
        call c%append_string("abx")            ! 3: longer, trailing byte ABOVE a blank
        !
        cmp_low = c%compare(1_int64, 2_int64)
        cmp_high = c%compare(1_int64, 3_int64)
        cmp_rev = c%compare(2_int64, 1_int64)
        !
        call check(error, cmp_low > 0, &
            "a trailing byte below a blank must make the LONGER element sort first, so the " // &
            "shorter one compares greater")
        if (allocated(error)) return
        call check(error, cmp_high < 0, &
            "a trailing byte above a blank must make the longer element sort last -- the two arms " // &
            "must disagree, or the comparison is length-only")
        if (allocated(error)) return
        call check(error, cmp_rev < 0, "the comparison must be antisymmetric across the same pair")
        if (allocated(error)) return
        call check(error, c%compare(1_int64, 1_int64) == 0, "an element must compare equal to itself")
    end subroutine test_compare_trailing_byte_below_blank
    !
    !> `%copy_buffers` exports the column into caller-supplied offsets and payload arrays -- the
    !! copying counterpart of `%raw_buffers`, for an interop caller who wants its own memory. Its
    !! empty-column path is separate, because a column that never had an element may not have
    !! allocated its offsets at all and still owes the caller the leading zero.
    subroutine test_copy_buffers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_string_column) :: c, empty
        integer(int64) :: offs(4), eoffs(2)
        character(len=1) :: data(9), edata(1)
        integer :: i
        !
        call c%append_string("ab")
        call c%append_string("cde")
        call c%append_string("fghi")
        call c%copy_buffers(offs, data)
        !
        call check(error, offs(1) == 0_int64, "the offsets must start at zero")
        if (allocated(error)) return
        call check(error, offs(2) == 2_int64 .and. offs(3) == 5_int64 .and. offs(4) == 9_int64, &
            "the offsets must be the running element ends")
        if (allocated(error)) return
        call check(error, all([(data(i), i = 1, 9)] == ["a", "b", "c", "d", "e", "f", "g", "h", "i"]), &
            "the payload must be the concatenated bytes, in element order")
        if (allocated(error)) return
        !
        ! An empty column: nothing was ever appended, so the offsets array may never have been
        ! allocated -- but the caller must still be handed the leading zero.
        eoffs = 99_int64
        call empty%copy_buffers(eoffs, edata)
        call check(error, eoffs(1) == 0_int64, &
            "an empty column must still write its leading zero offset, whether or not it ever " // &
            "allocated an offsets array")
    end subroutine test_copy_buffers
    !

    !> **Every `parquet_string_column_*` procedure and the binding of the same name must be one
    !! implementation, not two.**
    !!
    !! Each binding is now a one-line forwarder onto the typed form, which is what keeps ifx from
    !! building a runtime type descriptor in a caller's prologue when `parquet_columns` reaches a
    !! column's `str` component (`check_no_type_bound_string_column_access` is the static half of
    !! the guard). A forwarder is exactly the shape that can be miswired without failing anything:
    !! swap two arguments of the same type, drop an `optional`, or forward to the wrong kind
    !! specific, and the code still compiles.
    !!
    !! **What makes this a test rather than a restatement of the forwarder** is that it drives both
    !! halves on ONE column and compares them, on a fixture built to the project's own rule --
    !! shortest element first, a null present, and row-distinct values, so a length taken from the
    !! first element and a swapped index are both visible. The mutators are asserted by their
    !! EFFECT: each is applied through the typed form and then through the binding, and the two
    !! columns must agree element for element.
    subroutine test_typed_tier_agrees_with_bindings(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col, a, b, src
        character(len=:), allocatable :: s1, s2
        character(len=8) :: d1, d2
        integer(int64) :: i
        logical :: same
        !
        call seed_agreement_column(col)
        !
        ! ---- Queries: same column, both routes, element by element. ----
        same = .true.
        do i = 1_int64, col%size()
            if (col%is_null(i) .neqv. parquet_string_column_is_null(col, i)) same = .false.
            call col%get(i, s1, allow_null=.true.)
            call parquet_string_column_get(col, i, s2, allow_null=.true.)
            if (s1 /= s2) same = .false.
            call col%copy_to(i, d1, allow_null=.true.)
            call parquet_string_column_copy_to(col, i, d2, allow_null=.true.)
            if (d1 /= d2) same = .false.
        end do
        call check(error, same, "the typed is_null/get/copy_to must answer as their bindings do")
        if (allocated(error)) return
        call check(error, col%size() == parquet_string_column_size(col), &
            "the typed size must answer as %size() does")
        if (allocated(error)) return
        call check(error, col%null_count() == parquet_string_column_null_count(col), &
            "the typed null_count must answer as %null_count() does")
        if (allocated(error)) return
        call check(error, col%capacity() == parquet_string_column_capacity(col), &
            "the typed capacity must answer as %capacity() does")
        if (allocated(error)) return
        call check(error, col%character_size() == parquet_string_column_character_size(col), &
            "the typed character_size must answer as %character_size() does")
        if (allocated(error)) return
        call check(error, col%character_capacity() == parquet_string_column_character_capacity(col), &
            "the typed character_capacity must answer as %character_capacity() does")
        if (allocated(error)) return
        call check(error, col%has_validity() .eqv. parquet_string_column_has_validity(col), &
            "the typed has_validity must answer as %has_validity() does")
        if (allocated(error)) return
        !
        ! ---- Mutators: two identical columns, one per route, compared afterwards. ----
        call seed_agreement_column(a)
        call seed_agreement_column(b)
        call a%set(2_int64, "REPLACED")
        call parquet_string_column_set(b, 2_int64, "REPLACED")
        call a%set_null(3_int64)
        call parquet_string_column_set_null(b, 3_int64)
        call a%append_null()
        call parquet_string_column_append_null(b)
        call a%append_nulls(2_int64)
        call parquet_string_column_append_nulls(b, 2_int64)
        call a%append_values(["yy", "z "])
        call parquet_string_column_append_values(b, ["yy", "z "])
        call seed_agreement_column(src)
        call a%append_column(src)
        call parquet_string_column_append_column(b, src)
        call a%append_from(src, 2_int64)
        call parquet_string_column_append_from(b, src, 2_int64)
        call a%reserve(64_int64, 512_int64)
        call parquet_string_column_reserve(b, 64_int64, 512_int64)
        call a%reserve_validity()
        call parquet_string_column_reserve_validity(b)
        call a%shrink_to_fit()
        call parquet_string_column_shrink_to_fit(b)
        call check_columns_agree(error, a, b, "set/set_null/append_*/reserve/shrink_to_fit")
        if (allocated(error)) return
        !
        ! ---- The reordering family, on its own pair. ----
        call seed_agreement_column(a)
        call seed_agreement_column(b)
        call a%reindex([5_int64, 4_int64, 3_int64, 2_int64, 1_int64])
        call parquet_string_column_reindex(b, [5_int64, 4_int64, 3_int64, 2_int64, 1_int64])
        call check_columns_agree(error, a, b, "reindex")
        if (allocated(error)) return
        call a%reindex_trusted([2_int64, 1_int64, 4_int64, 3_int64, 5_int64])
        call parquet_string_column_reindex_trusted(b, [2_int64, 1_int64, 4_int64, 3_int64, 5_int64])
        call check_columns_agree(error, a, b, "reindex_trusted")
        if (allocated(error)) return
        call a%gather([3_int64, 3_int64, 1_int64])
        call parquet_string_column_gather(b, [3_int64, 3_int64, 1_int64])
        call check_columns_agree(error, a, b, "gather")
        if (allocated(error)) return
        !
        call seed_agreement_column(a)
        call seed_agreement_column(b)
        call a%delete_by_mask([.true., .false., .true., .false., .true.])
        call parquet_string_column_delete_by_mask(b, [.true., .false., .true., .false., .true.])
        call check_columns_agree(error, a, b, "delete_by_mask")
        if (allocated(error)) return
        !
        call seed_agreement_column(a)
        call seed_agreement_column(b)
        call a%set_validity([.true., .false., .true., .true., .false.])
        call parquet_string_column_set_validity(b, [.true., .false., .true., .true., .false.])
        call check_columns_agree(error, a, b, "set_validity")
        if (allocated(error)) return
        call a%set_where([.false., .true., .true., .false., .false.], "typed")
        call parquet_string_column_set_where(b, [.false., .true., .true., .false., .false.], "typed")
        call check_columns_agree(error, a, b, "set_where")
        if (allocated(error)) return
        !
        call a%clear()
        call parquet_string_column_clear(b)
        call check_columns_agree(error, a, b, "clear")
    end subroutine test_typed_tier_agrees_with_bindings
    !
    !> The fixture both routes above are driven on: five elements, SHORTEST FIRST (so a length
    !! derived from element 1 truncates something), row-distinct values (so a swapped index shows),
    !! and one null (so the validity half is exercised at all).
    subroutine seed_agreement_column(col)
        type(parquet_string_column), intent(out) :: col !! the seeded column.
        call col%append_string("a")
        call col%append_string("bb")
        call col%append_null()
        call col%append_string("dddd")
        call col%append_string("eeeeeeee")
    end subroutine seed_agreement_column
    !
    !> Fails unless two columns hold the same elements, null states and counts.
    subroutine check_columns_agree(error, a, b, what)
        type(error_type), allocatable, intent(out) :: error !! set when they disagree.
        type(parquet_string_column), intent(in) :: a        !! the binding-driven column.
        type(parquet_string_column), intent(in) :: b        !! the typed-form-driven column.
        character(len=*), intent(in) :: what                !! the family being compared.
        character(len=:), allocatable :: sa, sb
        integer(int64) :: i
        logical :: same
        !
        same = a%size() == b%size() .and. a%null_count() == b%null_count() &
            .and. a%character_size() == b%character_size()
        if (same) then
            do i = 1_int64, a%size()
                if (a%is_null(i) .neqv. b%is_null(i)) then
                    same = .false.
                    exit
                end if
                call a%get(i, sa, allow_null=.true.)
                call b%get(i, sb, allow_null=.true.)
                if (sa /= sb) then
                    same = .false.
                    exit
                end if
            end do
        end if
        call check(error, same, "the typed form of "//what//" must leave the same column as the binding")
    end subroutine check_columns_agree
    !
    !> `gather_from` is `clone` + `gather` + `set_null` per masked element as ONE rebuild, so that
    !! composition on a clone is the oracle. The index repeats an element and skips the source's
    !! own null; the mask nulls two selected elements, which must come back as ZERO-WIDTH slots
    !! (the payload holds the kept bytes only); the source must come back byte-identical; a
    !! populated destination is cleared first; the int32 form agrees; an all-true mask equals the
    !! plain gather in every buffer; an empty list gives an empty column.
    subroutine test_gather_from(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: src, dst, oracle, keep
        integer(int64), allocatable :: off_a(:), off_b(:)
        character(len=1), allocatable :: dat_a(:), dat_b(:)
        integer(int64), parameter :: IDX(5) = [4_int64, 1_int64, 4_int64, 6_int64, 2_int64]
        logical, parameter :: MASK(5) = [.true., .false., .true., .true., .false.]
        character(len=:), allocatable :: s
        integer(int64) :: k
        !
        call src%append_string("a")
        call src%append_string("bb")
        call src%append_null()
        call src%append_string("dddd")
        call src%append_string("")
        call src%append_string("ffffff")
        keep = src%clone()
        oracle = src%clone()
        call oracle%gather(IDX)
        do k = 1_int64, 5_int64
            if (.not. MASK(k)) call oracle%set_null(k)
        end do
        call dst%gather_from(src, IDX, valid=MASK)
        call check(error, dst%validate(), "gather_from must leave the column's invariants intact")
        if (allocated(error)) return
        call check_columns_agree(error, dst, oracle, "gather_from against clone + gather + set_null")
        if (allocated(error)) return
        call check(error, dst%null_count() == 2_int64, "the two masked elements are the result's nulls")
        if (allocated(error)) return
        call check(error, dst%length(2_int64) == 0_int64 .and. dst%length(5_int64) == 0_int64, &
            "a masked element must take a zero-width slot")
        if (allocated(error)) return
        call check(error, dst%character_size() == 14_int64, &
            "the payload must hold the kept bytes only: 'dddd', 'dddd' and 'ffffff'")
        if (allocated(error)) return
        call dst%get(3_int64, s)
        call check(error, s == "dddd", "a repeated element must arrive intact at each position naming it")
        if (allocated(error)) return
        call check_columns_agree(error, src, keep, "the source after gather_from")
        if (allocated(error)) return
        !
        ! A populated destination is cleared first, and the int32 index form agrees.
        call dst%gather_from(src, [6_int64])
        call dst%get(1_int64, s)
        call check(error, dst%size() == 1_int64 .and. s == "ffffff" .and. dst%null_count() == 0_int64, &
            "a populated destination must be cleared before the rebuild")
        if (allocated(error)) return
        call dst%gather_from(src, [4_int32, 1_int32])
        call dst%get(2_int64, s)
        call check(error, dst%size() == 2_int64 .and. s == "a", "the int32 index form must gather the same elements")
        if (allocated(error)) return
        !
        ! An all-true mask is no mask: offsets, payload and nulls identical to the plain gather.
        oracle = src%clone()
        call oracle%gather(IDX)
        call dst%gather_from(src, IDX, valid=[.true., .true., .true., .true., .true.])
        allocate(off_a(oracle%size() + 1_int64), dat_a(max(oracle%character_size(), 1_int64)))
        allocate(off_b(dst%size() + 1_int64), dat_b(max(dst%character_size(), 1_int64)))
        call oracle%copy_buffers(off_a, dat_a)
        call dst%copy_buffers(off_b, dat_b)
        call check(error, size(off_a) == size(off_b) .and. size(dat_a) == size(dat_b), &
            "an all-true mask must give the plain gather's buffer sizes")
        if (allocated(error)) return
        call check(error, all(off_a == off_b) .and. all(dat_a == dat_b) .and. &
            dst%null_count() == oracle%null_count(), &
            "an all-true mask must give the plain gather's offsets, payload and nulls")
        if (allocated(error)) return
        !
        call dst%gather_from(src, [integer(int64) ::])
        call check(error, dst%size() == 0_int64 .and. dst%validate(), "an empty list must give an empty column")
    end subroutine test_gather_from
    !

    !
    !> %argminmax must point at exactly the elements a scan keeping running winners through
    !! %compare points at: nulls skipped, the shorter operand blank-padded, and a tie kept by the
    !! EARLIER index -- since %print_stat's string extremes moved from that scan onto it. The
    !! reference is that scan, run here over the same column.
    subroutine test_argminmax_matches_compare(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_string_column) :: col
        integer(int64) :: imin, imax, emin, emax
        integer :: k
        character(len=8) :: buf

        ! Nothing to point at: an empty column, then an all-null one.
        call col%argminmax(imin, imax)
        call check(error, imin == 0_int64 .and. imax == 0_int64, "an empty column has no extremes")
        if (allocated(error)) return
        call col%append_null()
        call col%append_null()
        call col%argminmax(imin, imax)
        call check(error, imin == 0_int64 .and. imax == 0_int64, "an all-null column has no extremes")
        if (allocated(error)) return
        ! A column of empty strings only has no payload at all: every element ties, the first wins.
        call col%clear()
        call col%append_null()
        call col%append_string("")
        call col%append_string("")
        call col%argminmax(imin, imax)
        call check(error, imin == 2_int64 .and. imax == 2_int64, &
            "with no payload every non-null element is equal and the earliest wins both places")
        if (allocated(error)) return
        ! The mixed fixture: the shortest element first (the sized-from-the-first trap), an empty
        ! string, a blank-padded twin of an earlier value (a tie the earlier index must keep), nulls,
        ! a byte below blank (a tab sorts before the padding an empty string compares as), and a
        ! value above the rest that a second copy must not displace.
        call col%clear()
        call col%append_string("b")
        call col%append_string("")
        call col%append_string("b   ")
        call col%append_null()
        call col%append_string("zeta")
        call col%append_string("zeta ")
        call col%append_string("a")
        call col%append_string("a ")
        call col%append_null()
        call col%append_string("zz")
        call col%append_string("Z")
        call col%append_string(achar(9) // "x")
        call col%append_string("zz")
        call col%append_string(achar(9) // "x")
        call col%argminmax(imin, imax)
        call reference_argminmax(col, emin, emax)
        call check(error, imin == emin, "argminmax's minimum must be the compare scan's")
        if (allocated(error)) return
        call check(error, imax == emax, "argminmax's maximum must be the compare scan's")
        if (allocated(error)) return
        ! The expectation spelt out, so the reference cannot agree with argminmax by sharing a bug.
        call check(error, imin == 12_int64, "the tab-led element is the minimum, and its first copy")
        if (allocated(error)) return
        call check(error, imax == 10_int64, "'zz' is the maximum, and its first copy")
        if (allocated(error)) return
        ! Many more, in a pattern with repeats, so the running candidates change hands often.
        do k = 1, 300
            write(buf, '(a1, i3.3, a1)') achar(iachar("a") + mod(k*7, 26)), mod(k*13, 1000), " "
            call col%append_string(buf(1:1 + mod(k, 6)))
            if (mod(k, 17) == 0) call col%append_null()
        end do
        call col%argminmax(imin, imax)
        call reference_argminmax(col, emin, emax)
        call check(error, imin == emin .and. imax == emax, &
            "argminmax must match the compare scan over the long fixture")
    end subroutine test_argminmax_matches_compare
    !
    !> The scan %argminmax replaced: running winners through %compare, nulls skipped, ties kept.
    subroutine reference_argminmax(col, emin, emax)
        type(parquet_string_column), intent(in) :: col !! the column.
        integer(int64), intent(out) :: emin            !! index of the smallest element, or 0.
        integer(int64), intent(out) :: emax            !! index of the largest element, or 0.
        integer(int64) :: i

        emin = 0_int64
        emax = 0_int64
        do i = 1_int64, col%size()
            if (col%is_null(i)) cycle
            if (emin == 0_int64) then
                emin = i
                emax = i
            else
                if (col%compare(i, emin) < 0) emin = i
                if (col%compare(i, emax) > 0) emax = i
            end if
        end do
    end subroutine reference_argminmax
end module test_parquet_string
