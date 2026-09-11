!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for `parquet_map_column` and `parquet_map_row` in memory -- build, fill,
!> null, look up, copy and rebuild -- with no file and no Arrow anywhere.
!!
!! The map column's counterpart to `test_columns`/`test_list`/`test_struct`. Three properties it
!! exists to pin, beyond the ordinary accessor behaviour:
!!
!! * **A null row, a present-but-EMPTY row and a row with a null VALUE are three different
!!   states**, and the first two both report `%size() == 0`. An implementation that conflates them
!!   passes any test that only counts entries, which is why nearly every test here checks the
!!   trio rather than one of them.
!! * **Duplicate keys survive, in order.** `%get` takes the first, `occurrence=` a later one, and
!!   `%key_count` says how many there are. Nothing deduplicates.
!! * **A lookup that fails is soft only when asked.** `warn=`/`found=` are the two opt-ins; with
!!   neither, the call aborts (that half is an error scenario, since it ends the process).
module test_map
    use testdrive, only : new_unittest, unittest_type, error_type, check
    ! NARROW import, not `use parquet`. The facade would compile just as well and would let a
    ! future test in this file reach the C++ layer without anything saying so; naming the tier
    ! makes that a build error instead. The facade's own re-export of this tier is pinned by
    ! `test_facade_covers_every_layer` (test/test_examples.f90), which is where that claim lives.
    use parquet_map
    use parquet_columns
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_map

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_map(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("map init fixes the value kind", test_init_fixes_kind), &
            new_unittest("map append_row stores keys and values in order", test_append_row), &
            new_unittest("map null, empty and null-value rows are three states", test_three_states), &
            new_unittest("map duplicate keys survive with occurrence=", test_duplicate_keys), &
            new_unittest("map every value kind round-trips in memory", test_all_kinds), &
            new_unittest("map get_at and key_at walk a row positionally", test_positional_walk), &
            new_unittest("map soft-fail lookups report found=", test_soft_fail_found), &
            new_unittest("map soft-fail lookups warn without aborting", test_soft_fail_warn), &
            new_unittest("map gather_rows rebuilds keys and values together", test_gather_rows), &
            new_unittest("map append_from concatenates and keeps keys paired with values", &
                test_append_from), &
            new_unittest("map deep_copy is independent", test_deep_copy), &
            new_unittest("map move_from empties the source", test_move_from), &
            new_unittest("map set_null drops the entries and clear_null returns an empty row", &
                test_clear_null_restores), &
            new_unittest("map kind_text and summary describe the column", test_kind_text), &
            new_unittest("map adopt_rows builds from moved-in columns", test_adopt_rows), &
            new_unittest("map ensure_validity covers both null levels", test_ensure_validity), &
            new_unittest("map grow_rows appends null rows", test_grow_rows), &
            new_unittest("map adopt_container carries one into a parquet_column", test_adopt_container), &
            new_unittest("map reserve and shrink_to_fit move capacity only", test_capacity), &
            new_unittest("every row-addressing binding agrees through its int32 specific", &
                test_int32_row_indices), &
            new_unittest("a row handle reports the row it names", test_row_handle_queries), &
            new_unittest("a null value comes back invalid, for every value kind", &
                test_null_values_every_kind), &
            new_unittest("map kind_text spells every value kind", test_kind_text_every_value_kind), &
            new_unittest("the row bitmap grows without losing the nulls already set", &
                test_validity_bitmap_grows), &
            new_unittest("an uninitialized column survives the structural and base bindings", &
                test_uninitialised_and_base_bindings), &
            new_unittest("init carries a unit, and appending an empty map is a no-op", &
                test_init_unit_and_empty_append) &
            ]
    end subroutine collect_tests_parquet_map

    !> `%init` fixes the value kind and answers for an EMPTY column -- which is what the writer
    !> needs before any row exists.
    subroutine test_init_fixes_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc
        call check(error, .not. mc%is_init(), "a fresh column reports is_init .false.")
        if (allocated(error)) return
        call mc%init(PK_INT32)
        call check(error, mc%is_init(), "a column %init has run on reports is_init")
        if (allocated(error)) return
        call check(error, mc%element_kind() == PK_INT32, "%element_kind answers the declared kind")
        if (allocated(error)) return
        call check(error, mc%size() == 0_int64, "%init with no nrows leaves the column empty")
        if (allocated(error)) return
        call check(error, mc%total_entries() == 0_int64, "an empty column holds no entries")
        if (allocated(error)) return
        call check(error, mc%kindof() == PK_MAP, "%kindof is always PK_MAP")
    end subroutine test_init_fixes_kind

    !> `%append_row` stores the keys and the values index-aligned, in the order given.
    subroutine test_append_row(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        character(len=:), allocatable :: k

        call mc%init(PK_INT32)
        call mc%append_row(["alpha", "beta ", "gamma"], [10_int32, 20_int32, 30_int32])
        call check(error, mc%size() == 1_int64, "one append_row makes one row")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 3_int64, "%length reports the entry count")
        if (allocated(error)) return
        call check(error, mc%total_entries() == 3_int64, "%total_entries sums every row")
        if (allocated(error)) return

        row = mc%view(1_int64)
        call check(error, row%is_valid(), "%view of an existing row is valid")
        if (allocated(error)) return
        call check(error, row%size() == 3, "the handle reports the same entry count")
        if (allocated(error)) return
        ! Trailing blanks are trimmed on the way in, as they are wherever a character ARRAY enters
        ! a column: "beta " was declared 5 wide to match its neighbours and is stored as "beta".
        call row%key_at(2, k)
        call check(error, k == "beta", "a key array element is trimmed, got '"//k//"'")
        if (allocated(error)) return
        call row%get("gamma", v)
        call check(error, v == 30_int32, "%get finds the third key's value")
        if (allocated(error)) return
        call row%get_at(1, v)
        call check(error, v == 10_int32, "%get_at reads the first entry positionally")
    end subroutine test_append_row

    !> The three states that an implementation counting entries alone cannot tell apart: a NULL
    !> row, a present but EMPTY row, and a present row holding one entry whose VALUE is null.
    subroutine test_three_states(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        logical :: ok, got

        call mc%init(PK_INT32)
        call mc%append_null_row()
        call mc%append_empty_row()
        call mc%append_row(["solo"], [0_int32], is_valid=[.false.])

        call check(error, mc%size() == 3_int64, "three rows were appended")
        if (allocated(error)) return
        call check(error, mc%is_null(1_int64), "row 1 is a null map")
        if (allocated(error)) return
        call check(error, .not. mc%is_null(2_int64), "row 2 is present, not null")
        if (allocated(error)) return
        call check(error, .not. mc%is_null(3_int64), "row 3 is present, not null")
        if (allocated(error)) return
        ! The pair a naive implementation collapses: both are empty, only %is_null separates them.
        call check(error, mc%is_empty(1_int64), "a null row reports is_empty")
        if (allocated(error)) return
        call check(error, mc%is_empty(2_int64), "an empty row reports is_empty")
        if (allocated(error)) return
        call check(error, .not. mc%is_empty(3_int64), "a row with one entry is not empty")
        if (allocated(error)) return
        call check(error, mc%null_count() == 1_int64, "exactly one row is null")
        if (allocated(error)) return

        ! Row 3's entry EXISTS -- the key is found -- but its value is null.
        row = mc%view(3_int64)
        ! Two separate variables deliberately: `is_valid` and `found` are different questions --
        ! the entry was FOUND and its value is NULL -- and aliasing one variable onto two
        ! intent(out) dummies would be non-conforming as well as unable to tell them apart.
        call row%get("solo", v, is_valid=ok, found=got)
        call check(error, got, "the key of a null-valued entry is found")
        if (allocated(error)) return
        call check(error, .not. ok, "a null value reports is_valid .false.")
        if (allocated(error)) return
        call check(error, row%contains_key("solo"), "the key of a null-valued entry is still present")
        if (allocated(error)) return
        ! And a null ROW finds nothing at all, which is a different answer from a null value.
        row = mc%view(1_int64)
        call check(error, .not. row%contains_key("solo"), "a null row carries no keys")
        if (allocated(error)) return
        call check(error, row%key_count("solo") == 0, "a null row's key_count is 0")
    end subroutine test_three_states

    !> Duplicate keys are stored as given: `%get` takes the first, `occurrence=` a later one, and
    !> `%key_count` says how many there are. Nothing deduplicates and nothing reorders.
    subroutine test_duplicate_keys(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        logical :: got

        call mc%init(PK_INT32)
        call mc%append_row(["a", "b", "a"], [1_int32, 2_int32, 3_int32])
        row = mc%view(1_int64)

        call check(error, row%key_count("a") == 2, "%key_count sees both entries under 'a'")
        if (allocated(error)) return
        call check(error, row%key_count("b") == 1, "%key_count sees one entry under 'b'")
        if (allocated(error)) return
        call check(error, row%key_count("zz") == 0, "%key_count answers 0 for an absent key")
        if (allocated(error)) return

        call row%get("a", v)
        call check(error, v == 1_int32, "%get returns the FIRST match by default")
        if (allocated(error)) return
        call row%get("a", v, occurrence=2)
        call check(error, v == 3_int32, "occurrence=2 returns the second match")
        if (allocated(error)) return
        ! An occurrence past the count is a lookup failure, not a wrong answer.
        call row%get("a", v, occurrence=3, found=got)
        call check(error, .not. got, "occurrence past key_count reports found .false.")
        if (allocated(error)) return
        ! The order is what makes the two positional reads differ from the by-key ones.
        call row%get_at(3, v)
        call check(error, v == 3_int32, "the duplicate key's second entry is still at position 3")
    end subroutine test_duplicate_keys

    !> One row of every value kind, appended and read back. The nine-way generic fan-out is the
    !> whole surface here, so a kind that dispatches to the wrong specific shows up immediately.
    subroutine test_all_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: m_i32, m_i64, m_f32, m_f64, m_b, m_s, m_d, m_t, m_ts
        type(parquet_map_row) :: row
        integer(int32) :: vi32
        integer(int64) :: vi64
        real(real32) :: vf32
        real(real64) :: vf64
        logical :: vb
        character(len=:), allocatable :: vs
        type(parquet_date) :: vd, d1
        type(parquet_time) :: vt, t1
        type(parquet_timestamp) :: vts, ts1

        call m_i32%init(PK_INT32)
        call m_i32%append_row(["k"], [7_int32])
        row = m_i32%view(1_int64); call row%get("k", vi32)
        call check(error, vi32 == 7_int32, "int32 value round-trips")
        if (allocated(error)) return

        call m_i64%init(PK_INT64)
        call m_i64%append_row(["k"], [9000000000_int64])
        row = m_i64%view(1_int64); call row%get("k", vi64)
        call check(error, vi64 == 9000000000_int64, "int64 value round-trips")
        if (allocated(error)) return

        call m_f32%init(PK_FLOAT32)
        call m_f32%append_row(["k"], [1.5_real32])
        row = m_f32%view(1_int64); call row%get("k", vf32)
        call check(error, vf32 == 1.5_real32, "float32 value round-trips")
        if (allocated(error)) return

        call m_f64%init(PK_FLOAT64)
        call m_f64%append_row(["k"], [2.25_real64])
        row = m_f64%view(1_int64); call row%get("k", vf64)
        call check(error, vf64 == 2.25_real64, "float64 value round-trips")
        if (allocated(error)) return

        call m_b%init(PK_LOGICAL)
        call m_b%append_row(["k"], [.true.])
        row = m_b%view(1_int64); call row%get("k", vb)
        call check(error, vb, "logical value round-trips")
        if (allocated(error)) return

        call m_s%init(PK_STRING)
        call m_s%append_row(["k"], ["hello"])
        row = m_s%view(1_int64); call row%get("k", vs)
        call check(error, vs == "hello", "string value round-trips, got '"//vs//"'")
        if (allocated(error)) return

        call d1%set(2026, 8, 27)
        call m_d%init(PK_DATE)
        call m_d%append_row(["k"], [d1])
        row = m_d%view(1_int64); call row%get("k", vd)
        call check(error, vd == d1, "date value round-trips")
        if (allocated(error)) return

        call t1%set(13, 45, 30)
        call m_t%init(PK_TIME)
        call m_t%append_row(["k"], [t1])
        row = m_t%view(1_int64); call row%get("k", vt)
        call check(error, vt == t1, "time value round-trips")
        if (allocated(error)) return

        call ts1%set(d1, t1)
        call m_ts%init(PK_TIMESTAMP)
        call m_ts%append_row(["k"], [ts1])
        row = m_ts%view(1_int64); call row%get("k", vts)
        call check(error, vts == ts1, "timestamp value round-trips")
    end subroutine test_all_kinds

    !> `%key_at` and `%get_at` together walk a row without a key lookup -- the shape the guide
    !> tells a caller to use when every entry is wanted rather than one.
    subroutine test_positional_walk(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        character(len=:), allocatable :: k
        integer(int32) :: v, total
        integer :: p
        character(len=:), allocatable :: seen

        call mc%init(PK_INT32)
        call mc%append_row(["one  ", "two  ", "three"], [1_int32, 2_int32, 3_int32])
        row = mc%view(1_int64)
        total = 0
        seen = ""
        do p = 1, row%size()
            call row%key_at(p, k)
            call row%get_at(p, v)
            seen = seen//trim(k)//";"
            total = total + v
        end do
        call check(error, total == 6_int32, "walking the row sees every value once")
        if (allocated(error)) return
        call check(error, seen == "one;two;three;", "the keys come back in stored order, got '"//seen//"'")
    end subroutine test_positional_walk

    !> `found=` reports a failed lookup without aborting and without printing, on all three lookup
    !> forms. The NEGATIVE CONTROL matters as much as the failures: a guard that reported .false.
    !> unconditionally would pass every abort test ever written for it.
    subroutine test_soft_fail_found(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        character(len=:), allocatable :: k
        logical :: got

        call mc%init(PK_INT32)
        call mc%append_row(["here"], [5_int32])
        row = mc%view(1_int64)

        ! Negative control first: the permitted case must NOT report a failure.
        call row%get("here", v, found=got)
        call check(error, got, "a key that IS present reports found .true.")
        if (allocated(error)) return
        call check(error, v == 5_int32, "and the value is delivered")
        if (allocated(error)) return
        call row%get_at(1, v, found=got)
        call check(error, got, "a position that exists reports found .true.")
        if (allocated(error)) return
        call row%key_at(1, k, found=got)
        call check(error, got, "key_at on an existing position reports found .true.")
        if (allocated(error)) return

        ! Then the three failures.
        call row%get("absent", v, found=got)
        call check(error, .not. got, "a missing key reports found .false.")
        if (allocated(error)) return
        call check(error, v == 0_int32, "and leaves value at the documented default")
        if (allocated(error)) return
        call row%get_at(9, v, found=got)
        call check(error, .not. got, "a position past the end reports found .false.")
        if (allocated(error)) return
        call row%key_at(0, k, found=got)
        call check(error, .not. got, "position 0 reports found .false.")
        if (allocated(error)) return
        call check(error, k == "", "and leaves the key empty")
    end subroutine test_soft_fail_found

    !> `warn=.true.` returns instead of aborting even with no `found=` argument. The value is not
    !> asserted beyond its default: what this pins is that the call RETURNS, which without warn=
    !> and without found= it would not (that half is an error scenario).
    subroutine test_soft_fail_warn(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        character(len=:), allocatable :: saved

        ! "errors_only", not "silent": the soft-fail path emits a WARNING, and only errors_only
        ! silences one -- silent covers informational and solicited output.
        call parquet_get_verbosity(saved)
        call parquet_set_verbosity("errors_only")
        call mc%init(PK_INT32)
        call mc%append_row(["here"], [5_int32])
        row = mc%view(1_int64)
        v = 12345_int32
        call row%get("absent", v, warn=.true.)
        call check(error, v == 0_int32, "a warned lookup still resets value to the default")
        if (allocated(error)) return
        call row%get_at(9, v, warn=.true.)
        call check(error, v == 0_int32, "a warned positional lookup does the same")
        call parquet_set_verbosity(saved)
    end subroutine test_soft_fail_warn

    !> `%gather_rows` rebuilds the offsets, the row bitmap and BOTH entry columns from one index
    !> list. The keys and the values must stay aligned -- a rebuild that permuted one and not the
    !> other would still produce the right entry COUNT for every row, which is why this checks the
    !> pairing rather than the counts.
    subroutine test_gather_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v

        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_null_row()
        call mc%append_row(["b", "c"], [2_int32, 3_int32])
        ! Reverse, dropping nothing.
        call mc%gather_rows([3_int64, 2_int64, 1_int64])
        call check(error, mc%size() == 3_int64, "the rebuilt column has the same row count")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 2_int64, "the two-entry row moved to position 1")
        if (allocated(error)) return
        call check(error, mc%is_null(2_int64), "the null row moved to position 2 and stayed null")
        if (allocated(error)) return
        row = mc%view(1_int64)
        call row%get("c", v)
        call check(error, v == 3_int32, "key 'c' still pairs with value 3 after the rebuild")
        if (allocated(error)) return
        row = mc%view(3_int64)
        call row%get("a", v)
        call check(error, v == 1_int32, "key 'a' still pairs with value 1 after the rebuild")
        if (allocated(error)) return
        ! A null row's entries are unreachable and are dropped by the rebuild, so the entry total
        ! reflects only the rows that survive.
        call check(error, mc%total_entries() == 3_int64, "the rebuild compacted to the live entries")
    end subroutine test_gather_rows

    !> `%deep_copy` shares nothing: mutating the copy must not touch the source.
    subroutine test_deep_copy(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc, cp
        type(parquet_map_row) :: row
        integer(int32) :: v

        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%deep_copy(cp)
        call cp%append_row(["b"], [2_int32])
        call cp%set_null(1_int64)
        call check(error, mc%size() == 1_int64, "the source keeps its own row count")
        if (allocated(error)) return
        call check(error, .not. mc%is_null(1_int64), "the source's row is untouched by the copy's set_null")
        if (allocated(error)) return
        row = mc%view(1_int64)
        call row%get("a", v)
        call check(error, v == 1_int32, "the source's value is unchanged")
        if (allocated(error)) return
        call check(error, cp%size() == 2_int64, "the copy grew independently")
    end subroutine test_deep_copy

    !> `%move_from` hands the storage over and leaves the source uninitialized, so a source that
    !> still claimed a kind and a row count would be describing storage it no longer has.
    subroutine test_move_from(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: src, dst
        type(parquet_map_row) :: row
        integer(int32) :: v

        call src%init(PK_INT32)
        call src%append_row(["a"], [1_int32])
        call dst%move_from(src)
        call check(error, dst%size() == 1_int64, "the destination took the row")
        if (allocated(error)) return
        call check(error, dst%element_kind() == PK_INT32, "and the value kind")
        if (allocated(error)) return
        call check(error, .not. src%is_init(), "the source is left uninitialized")
        if (allocated(error)) return
        call check(error, src%size() == 0_int64, "and empty")
        if (allocated(error)) return
        row = dst%view(1_int64)
        call row%get("a", v)
        call check(error, v == 1_int32, "the moved value is intact")
    end subroutine test_move_from

    !> `%set_null` drops the row's entries, so `%clear_null` brings the row back EMPTY.
    !!
    !! The drop is the invariant Arrow's Parquet writer requires -- a null slot spanning entries is
    !! refused outright -- so `%total_entries` is asserted, not just `%length`. A following row is
    !! read back by key to prove `keys` and `values` moved down together: gathering one and not the
    !! other would leave the two columns misaligned, which reads as a wrong VALUE under the right
    !! key rather than as a missing entry.
    subroutine test_clear_null_restores(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v

        call mc%init(PK_INT32)
        call mc%append_row(["a", "b"], [1_int32, 2_int32])
        call mc%append_row(["c", "d"], [3_int32, 4_int32])
        call check(error, mc%total_entries() == 4_int64, "four entries before anything is nulled")
        if (allocated(error)) return

        call mc%set_null(1_int64)
        call check(error, mc%is_null(1_int64), "the row is null after set_null")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 0_int64, "a null row reports no entries")
        if (allocated(error)) return
        call check(error, mc%total_entries() == 2_int64, &
            "the nulled row's two entries were dropped, not merely hidden")
        if (allocated(error)) return
        ! Row 2's entries moved down by two, in both columns at once.
        row = mc%view(2_int64)
        call row%get("d", v)
        call check(error, v == 4_int32, "the row after a nulled one keeps its keys aligned to its values")
        if (allocated(error)) return

        call mc%clear_null(1_int64)
        call check(error, .not. mc%is_null(1_int64), "clear_null makes the row present again")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 0_int64, &
            "and it comes back EMPTY -- clear_null is one bit, not an undo")
        if (allocated(error)) return
        call check(error, mc%total_entries() == 2_int64, "clearing the bit restores no entry")
    end subroutine test_clear_null_restores

    !> `%kind_text` spells the type the way a MAML schema does, and `%summary` describes the
    !> column in one line. Both are what a diagnostic prints, so both are pinned.
    subroutine test_kind_text(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc
        character(len=:), allocatable :: txt

        call mc%init(PK_FLOAT64)
        call mc%kind_text(txt)
        call check(error, txt == "map<string,float64>", "%kind_text names both halves, got '"//txt//"'")
        if (allocated(error)) return
        call mc%append_row(["a"], [1.0_real64])
        call mc%append_null_row()
        call mc%summary(txt)
        call check(error, index(txt, "2 rows") > 0, "%summary reports the row count, got '"//txt//"'")
        if (allocated(error)) return
        call check(error, index(txt, "1 entries") > 0, "%summary reports the entry count, got '"//txt//"'")
        if (allocated(error)) return
        call check(error, index(txt, "1 null") > 0, "%summary reports the null count, got '"//txt//"'")
    end subroutine test_kind_text

    !> `%adopt_rows` builds a whole column from moved-in offsets and two moved-in entry columns --
    !> the bulk path the file reader is built on. Everything comes back derived rather than
    !> declared, and the source arrays are left empty.
    subroutine test_adopt_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int64), allocatable :: offs(:)
        type(parquet_column) :: keys, vals
        integer(int32) :: v

        allocate(offs(4))
        offs = [0_int64, 2_int64, 2_int64, 3_int64]
        call keys%init(PK_STRING, 0_int64)
        call keys%append_values(["a", "b", "c"])
        call vals%init(PK_INT32, 0_int64)
        call vals%append_values([1_int32, 2_int32, 3_int32])
        call mc%adopt_rows(offs, keys, vals, row_valid=[.true., .true., .false.])

        call check(error, mc%size() == 3_int64, "the row count came from the offsets")
        if (allocated(error)) return
        call check(error, mc%element_kind() == PK_INT32, "the value kind came from the values column")
        if (allocated(error)) return
        call check(error, .not. allocated(offs), "the offsets were moved in, not copied")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 2_int64, "row 1 holds its two entries")
        if (allocated(error)) return
        call check(error, mc%is_empty(2_int64) .and. .not. mc%is_null(2_int64), &
            "row 2 is present and empty")
        if (allocated(error)) return
        call check(error, mc%is_null(3_int64), "row 3 is null, as row_valid said")
        if (allocated(error)) return
        row = mc%view(1_int64)
        call row%get("b", v)
        call check(error, v == 2_int32, "the adopted keys and values are aligned")
        if (allocated(error)) return
        call check(error, mc%validate(), "the adopted column satisfies its invariants")
    end subroutine test_adopt_rows

    !> `%ensure_validity` materializes both levels that can be null up front, which is what a
    !> caller runs before a parallel region so that the lazy first allocation cannot race.
    subroutine test_ensure_validity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc

        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call check(error, .not. mc%has_validity_storage(), &
            "a column with no null row has no bitmap yet")
        if (allocated(error)) return
        call mc%ensure_validity()
        call check(error, mc%has_validity_storage(), "%ensure_validity materializes the row bitmap")
        if (allocated(error)) return
        call check(error, mc%null_count() == 0_int64, "and marks nothing null while doing it")
    end subroutine test_ensure_validity

    !> `%grow_rows` (the container-base binding) appends null rows, which is also what `%init`'s
    !> `nrows` argument creates.
    subroutine test_grow_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc, mc2

        call mc%init(PK_INT32)
        call mc%grow_rows(3_int64)
        call check(error, mc%size() == 3_int64, "%grow_rows appends the requested rows")
        if (allocated(error)) return
        call check(error, mc%null_count() == 3_int64, "and every one of them is null")
        if (allocated(error)) return
        call mc2%init(PK_INT32, nrows=2_int64)
        call check(error, mc2%size() == 2_int64, "%init's nrows creates rows")
        if (allocated(error)) return
        call check(error, mc2%null_count() == 2_int64, "which are null too")
        if (allocated(error)) return
        call check(error, mc2%total_entries() == 0_int64, "and hold no entries")
    end subroutine test_grow_rows

    !> A map column travels inside a `parquet_column` through `%adopt_container`, which is how it
    !> reaches the rest of the library without anything naming its type.
    subroutine test_adopt_container(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_column) :: col
        type(parquet_map_column), allocatable :: mc
        class(parquet_container_column), allocatable :: cc

        allocate(mc)
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        call mc%append_null_row()
        ! %adopt_container takes the abstract face, so the concrete column is moved into a
        ! class(parquet_container_column) first -- the same two steps test_list's own adopt test
        ! takes, and the route a table would use.
        call move_alloc(mc, cc)
        call col%adopt_container(cc)
        call check(error, col%kindof() == PK_MAP, "the parquet_column reports PK_MAP")
        if (allocated(error)) return
        call check(error, col%length() == 2_int64, "and the container's row count")
        if (allocated(error)) return
        call check(error, col%colwidth() == 1, "a container column has width 1, not a stride")
        if (allocated(error)) return
        call check(error, .not. allocated(cc), "adopt MOVED the container rather than copying it")
    end subroutine test_adopt_container

    !> `%reserve` and `%shrink_to_fit` move capacity without touching the rows.
    subroutine test_capacity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc

        call mc%init(PK_INT32)
        call mc%reserve(100)
        call check(error, mc%capacity() >= 100_int64, "%reserve grows the row capacity")
        if (allocated(error)) return
        call check(error, mc%size() == 0_int64, "and adds no rows")
        if (allocated(error)) return
        call mc%append_row(["a"], [1_int32])
        call mc%shrink_to_fit()
        call check(error, mc%capacity() == 1_int64, "%shrink_to_fit releases the slack")
        if (allocated(error)) return
        call check(error, mc%size() == 1_int64, "and keeps the row")
    end subroutine test_capacity

    !> `append_from` concatenates one map column onto another.
    !>
    !> A map has TWO payload columns, so it has a failure mode a list does not: appending the keys
    !> and the values in different orders, or one without the other, gives every row the right
    !> entry COUNT while pairing the wrong value with each key. So the assertions look up values
    !> BY KEY in the appended rows rather than counting entries.
    subroutine test_append_from(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: dst, src
        type(parquet_map_row) :: row
        integer(int32) :: v

        call dst%init(PK_INT32)
        call dst%append_row(["a"], [1_int32])
        call dst%append_null_row()

        call src%init(PK_INT32)
        call src%append_row(["b", "c"], [2_int32, 3_int32])
        call src%append_null_row()
        call src%append_row(["d"], [4_int32])

        call dst%append_from(src)
        call check(error, dst%size() == 5_int64, "every row of both columns survives")
        if (allocated(error)) return
        call check(error, dst%total_entries() == 4_int64, "and every entry of both")
        if (allocated(error)) return
        call check(error, dst%length(3_int64) == 2_int64, "the appended two-entry row kept its length")
        if (allocated(error)) return
        call check(error, dst%is_null(4_int64), "the appended null row arrived null, not empty")
        if (allocated(error)) return
        ! By key, which is what a keys/values misalignment breaks and an entry count does not.
        row = dst%view(3_int64)
        call row%get("b", v)
        call check(error, v == 2_int32, "key 'b' still pairs with 2 after the append")
        if (allocated(error)) return
        call row%get("c", v)
        call check(error, v == 3_int32, "key 'c' still pairs with 3 after the append")
        if (allocated(error)) return
        row = dst%view(5_int64)
        call row%get("d", v)
        call check(error, v == 4_int32, "and the last appended row's own pairing survives")
        if (allocated(error)) return
        row = dst%view(1_int64)
        call row%get("a", v)
        call check(error, v == 1_int32, "the destination's own row is untouched")
        if (allocated(error)) return
        call check(error, dst%validate(), "the concatenated column satisfies its own invariants")
        if (allocated(error)) return
        call check(error, src%size() == 3_int64, "the source column is unchanged")
    end subroutine test_append_from

    !> Every row-addressing binding answers the same through its `int32` specific.
    !!
    !! `%length`, `%is_null`, `%is_empty`, `%view`, `%set_null` and `%clear_null` are each a
    !! generic over an `int32` and an `int64` row index, and the `int32` half is a one-line
    !! forward that nothing called -- so the whole `int32` half of this type's row addressing was
    !! untested. A specific that dropped its argument, or read a fixed row, would still compile
    !! and still answer something.
    !!
    !! Each is compared against its `int64` sibling over the same row rather than against a
    !! literal, and the null row is addressed as well as the filled one, so a specific that
    !! ignored its argument could not agree by accident.
    subroutine test_int32_row_indices(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: h32, h64
        integer(int32) :: i32
        integer(int64) :: i64

        call mc%init(PK_INT32)
        call mc%append_row(["a", "b"], [1_int32, 2_int32])
        call mc%append_null_row()
        call mc%append_row(["c"], [3_int32])
        i32 = 1_int32
        i64 = 1_int64

        call check(error, mc%length(i32) == mc%length(i64), "%length disagrees across the two kinds")
        if (allocated(error)) return
        call check(error, mc%is_null(i32) .eqv. mc%is_null(i64), "%is_null disagrees")
        if (allocated(error)) return
        call check(error, mc%is_empty(i32) .eqv. mc%is_empty(i64), "%is_empty disagrees")
        if (allocated(error)) return
        h32 = mc%view(i32)
        h64 = mc%view(i64)
        call check(error, h32%row_index() == h64%row_index(), "%view hands back a different row")
        if (allocated(error)) return

        ! The null row, whose answers are the opposite of row 1's.
        i32 = 2_int32
        call check(error, mc%is_null(i32) .and. mc%is_empty(i32) .and. mc%length(i32) == 0_int64, &
            "a null row is null, empty and zero-length through the int32 specifics too")
        if (allocated(error)) return

        ! The two MUTATORS, seen through a later query rather than a return value.
        i32 = 3_int32
        call mc%set_null(i32)
        call check(error, mc%is_null(3_int64), "%set_null through the int32 specific did nothing")
        if (allocated(error)) return
        call mc%clear_null(i32)
        call check(error, .not. mc%is_null(3_int64), "%clear_null through the int32 specific did nothing")
        if (allocated(error)) return
        call check(error, mc%is_empty(3_int64), &
            "a row cleared back to present holds no entries, as the int64 form documents")
        if (allocated(error)) return
        call check(error, mc%validate(), "the column still satisfies its invariants")
    end subroutine test_int32_row_indices

    !> What a row handle reports about the row it names.
    !!
    !! `%row_index`, `%is_null`, `%is_empty`, `%element_kind` and `%nested` are the queries a
    !! caller makes before deciding which `%get` to call, and none of them had a caller. Each is
    !! guarded, so a handle that had gone stale would abort rather than answer -- which is what
    !! makes them safe to call first.
    !!
    !! `%nested` over a SCALAR-valued map is the case asserted here: `inner` comes back null and
    !! `lo > hi`, so a caller that reached for it without checking `%element_kind` gets an empty
    !! loop rather than a wrong answer. The container case is
    !! `test_container_nested.f90`'s, where there is an inner column to read.
    subroutine test_row_handle_queries(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: h
        class(parquet_container_column), pointer :: inner
        integer(int64) :: lo, hi
        character(len=0) :: nokeys(0)
        integer(int32) :: novals(0)

        call mc%init(PK_INT32)
        call mc%append_row(["a", "b"], [1_int32, 2_int32])
        call mc%append_null_row()
        call mc%append_row(nokeys, novals)

        h = mc%view(1_int64)
        call check(error, h%row_index() == 1_int64, "%row_index must name the row it was made from")
        if (allocated(error)) return
        call check(error, h%element_kind() == PK_INT32, "%element_kind must name the VALUE kind")
        if (allocated(error)) return
        call check(error, .not. h%is_null() .and. .not. h%is_empty(), &
            "a row holding two entries is neither null nor empty")
        if (allocated(error)) return

        h = mc%view(2_int64)
        call check(error, h%row_index() == 2_int64, "%row_index must follow the handle")
        if (allocated(error)) return
        call check(error, h%is_null() .and. h%is_empty(), "a null row is null AND empty")
        if (allocated(error)) return

        h = mc%view(3_int64)
        call check(error, h%is_empty() .and. .not. h%is_null(), &
            "a present row of length zero is empty and NOT null -- the two states differ")
        if (allocated(error)) return

        ! `%nested` on a map whose values are not a container at all.
        h = mc%view(1_int64)
        call h%nested(inner, lo, hi)
        call check(error, .not. associated(inner), &
            "a scalar-valued map has no inner container to hand back")
        if (allocated(error)) return
        call check(error, lo > hi, "and it reports an empty entry range rather than a wrong one")
    end subroutine test_row_handle_queries

    !> A NULL value comes back as `is_valid = .false.`, for every value kind.
    !!
    !! Two levels of nullness meet here and they are different questions: the ROW may be absent,
    !! and an entry's VALUE may be null while its key is present. Every `%get` and `%get_at`
    !! specific has its own line for the second -- a per-kind `parquet_column_is_null` probe
    !! before the payload is read -- and only the `int32` and `string` ones had a caller.
    !!
    !! A specific that skipped the probe would return whatever sits in the payload gap: a plausible
    !! number, reported as valid. So the assertion is on `is_valid` AND on the key still being
    !! found, which is what distinguishes "null value" from "no such key".
    subroutine test_null_values_every_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: h
        logical :: ok, got
        integer(int64) :: vl
        real(real32) :: vf
        real(real64) :: vd
        logical :: vb
        type(parquet_date) :: vdate
        type(parquet_time) :: vtime
        type(parquet_timestamp) :: vts

        call mc%init(PK_INT64)
        call mc%append_row(["k"], [7_int64], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vl, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "int64: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vl, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "int64 by position: same answer")
        if (allocated(error)) return

        call mc%init(PK_FLOAT32)
        call mc%append_row(["k"], [1.5_real32], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vf, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "float32: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vf, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "float32 by position: same answer")
        if (allocated(error)) return

        call mc%init(PK_FLOAT64)
        call mc%append_row(["k"], [1.5_real64], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vd, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "float64: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vd, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "float64 by position: same answer")
        if (allocated(error)) return

        call mc%init(PK_LOGICAL)
        call mc%append_row(["k"], [.true.], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vb, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "logical: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vb, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "logical by position: same answer")
        if (allocated(error)) return

        call vdate%set(2026, 9, 11)
        call mc%init(PK_DATE)
        call mc%append_row(["k"], [vdate], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vdate, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "date: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vdate, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "date by position: same answer")
        if (allocated(error)) return

        call vtime%set(12, 30, 0)
        call mc%init(PK_TIME)
        call mc%append_row(["k"], [vtime], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vtime, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "time: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vtime, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "time by position: same answer")
        if (allocated(error)) return

        call vts%set(2026, 9, 11, 12, 30, 0)
        call mc%init(PK_TIMESTAMP)
        call mc%append_row(["k"], [vts], is_valid=[.false.])
        h = mc%view(1_int64)
        call h%get("k", vts, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "timestamp: the key is found and its value is null")
        if (allocated(error)) return
        call h%get_at(1, vts, is_valid=ok, found=got)
        call check(error, got .and. .not. ok, "timestamp by position: same answer")
        if (allocated(error)) return

        ! The negative control: a PRESENT value of the same kind must report valid, or every
        ! assertion above would pass for a specific that reported .false. unconditionally.
        call mc%init(PK_INT64)
        call mc%append_row(["k"], [7_int64])
        h = mc%view(1_int64)
        call h%get("k", vl, is_valid=ok, found=got)
        call check(error, got .and. ok .and. vl == 7_int64, "a present value must report valid")
    end subroutine test_null_values_every_kind

    !> `%kind_text` spells every value kind this column accepts.
    !!
    !! The spelling is a `select case` with one arm per `PK_*`, and it is what a MAML schema and
    !! every container diagnostic are built from -- so an arm naming the wrong kind writes a wrong
    !! schema. Eight of the eleven arms had no caller.
    subroutine test_kind_text_every_value_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc
        character(len=:), allocatable :: txt
        integer, parameter :: KINDS(9) = [PK_INT32, PK_INT64, PK_FLOAT32, PK_FLOAT64, PK_LOGICAL, &
                                          PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP]
        character(len=9), parameter :: NAMES(9) = [ &
            "int32    ", "int64    ", "float32  ", "float64  ", "logical  ", &
            "string   ", "date     ", "time     ", "timestamp"]
        integer :: k

        ! The uninitialized column first: it has no value kind at all and must still describe
        ! itself rather than abort.
        call mc%kind_text(txt)
        call check(error, txt == "map<string,none>", &
            "an uninitialized column spells itself, got '"//txt//"'")
        if (allocated(error)) return

        do k = 1, size(KINDS)
            call mc%init(KINDS(k))
            call mc%kind_text(txt)
            call check(error, txt == "map<string,"//trim(NAMES(k))//">", &
                "kind_text named the values '"//txt//"' rather than map<string,"//trim(NAMES(k))//">")
            if (allocated(error)) return
        end do
    end subroutine test_kind_text_every_value_kind

    !> The row bitmap GROWS when a null lands past the block it was first sized for.
    !!
    !! Validity is a packed `int64` bitmap, so the first null allocates one 64-row block and a
    !! null in a later block has to reallocate and CARRY the existing bits across. A growth that
    !! allocated without copying would lose every earlier null silently -- those rows would come
    !! back present, holding whatever their offsets say -- so a row in the first block is nulled,
    !! the growth is forced, and the first one is read again.
    subroutine test_validity_bitmap_grows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column) :: mc
        character(len=2) :: key
        integer :: k

        ! **The order matters, and it is what makes this test reach the growth at all.** The
        ! bitmap is sized from the row count at the moment the first null arrives, so nulling a
        ! row of a column that is ALREADY 200 rows long allocates all four blocks in one go and
        ! nothing ever grows. Nulling row 3 while the column is short takes one block; the rows
        ! appended afterwards do not touch the bitmap; and the null at row 130 is then the call
        ! that has to reallocate and carry.
        call mc%init(PK_INT32)
        do k = 1, 10
            write (key, '(i2.2)') mod(k, 100)
            call mc%append_row([key], [int(k, int32)])
        end do
        call mc%set_null(3_int64)
        call check(error, mc%has_validity_storage(), "the first null must allocate the bitmap")
        if (allocated(error)) return
        do k = 11, 200
            write (key, '(i2.2)') mod(k, 100)
            call mc%append_row([key], [int(k, int32)])
        end do
        call check(error, mc%nrows() == 200_int64, "the fixture must hold 200 rows")
        if (allocated(error)) return

        call mc%set_null(130_int64)
        call check(error, mc%is_null(130_int64), "the null past the first block did not take")
        if (allocated(error)) return
        call check(error, mc%is_null(3_int64), &
            "growing the bitmap lost the null set before it -- the old bits were not carried")
        if (allocated(error)) return
        call check(error, mc%null_count() == 2_int64, "both nulls must be counted")
        if (allocated(error)) return
        call check(error, .not. mc%is_null(129_int64) .and. .not. mc%is_null(131_int64), &
            "the growth must not null the rows either side of the one asked for")
        if (allocated(error)) return
        call check(error, mc%validate(), "the column still satisfies its invariants")
    end subroutine test_validity_bitmap_grows

    !> The structural operations on an UNINITIALIZED column, and the base-class row bindings.
    !!
    !! A column that has never been `%init`ed has no value kind, and its two entry columns are
    !! `PK_NONE` `parquet_column`s that `%gather` would refuse -- so every structural operation
    !! has to notice it has no rows and return before touching them. That is a real state: it is
    !! what a declared-but-unused column looks like.
    !!
    !! `%clear_null_row` is the `parquet_container_column` binding, reached through a polymorphic
    !! pointer rather than the concrete type. It forwards to `%clear_null`, and a container walked
    !! generically -- which is how the table layer reaches one -- goes through it rather than
    !! through the specific.
    subroutine test_uninitialised_and_base_bindings(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc
        class(parquet_container_column), pointer :: base
        integer(int64) :: none(0)
        character(len=:), allocatable :: txt

        call check(error, .not. mc%is_init(), "the fixture must start uninitialized")
        if (allocated(error)) return
        call mc%gather_rows(none)
        call check(error, mc%nrows() == 0_int64, "gathering no rows leaves no rows")
        if (allocated(error)) return
        call check(error, mc%validate(), "and leaves the column well-formed")
        if (allocated(error)) return
        call mc%summary(txt)
        call check(error, index(txt, "0 rows") > 0, "it still describes itself, got '"//txt//"'")
        if (allocated(error)) return

        ! The base-class row bindings, through a polymorphic pointer.
        call mc%init(PK_INT32)
        call mc%append_row(["a"], [1_int32])
        base => mc
        call base%set_null_row(1_int64)
        call check(error, mc%is_null(1_int64), "%set_null_row through the base binding did nothing")
        if (allocated(error)) return
        call check(error, base%is_null_row(1_int64), "and the base query must agree")
        if (allocated(error)) return
        call base%clear_null_row(1_int64)
        call check(error, .not. mc%is_null(1_int64), &
            "%clear_null_row through the base binding did nothing")
        if (allocated(error)) return
        call check(error, mc%validate(), "the column still satisfies its invariants")
    end subroutine test_uninitialised_and_base_bindings

    !> `%init(..., unit=)` carries the unit into the values column, and `%append_from` an EMPTY
    !! map is a no-op.
    !!
    !! The unit is the values `parquet_column`'s own, and `%init` has a separate call for the
    !! present and absent cases, so the arm that passes it through was never taken. A map of
    !! measurements that lost its unit on the way into a file is a silent data defect.
    !!
    !! The empty `%append_from` shares this test because it is the other one-line early return in
    !! this type's lifecycle: appending a source with no rows must leave the destination
    !! untouched, INCLUDING an uninitialized destination -- the return comes before the
    !! `require_init` guard, so `%append_from` of an empty map onto a fresh column must not abort.
    subroutine test_init_unit_and_empty_append(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_map_column), target :: mc, plain, empty, dst
        type(parquet_column), pointer :: p
        character(len=:), allocatable :: u

        call mc%init(PK_FLOAT64, unit="km/s")
        call parquet_map_column_values(mc, p)
        call check(error, associated(p), "the values column must be reachable")
        if (allocated(error)) return
        call p%unit_string(u)
        call check(error, u == "km/s", "the values lost the unit %init was given, got '"//u//"'")
        if (allocated(error)) return
        call plain%init(PK_FLOAT64)
        call parquet_map_column_values(plain, p)
        call p%unit_string(u)
        call check(error, u == "", "a values column built without a unit must not have one")
        if (allocated(error)) return

        ! An empty source onto an UNINITIALIZED destination: the early return comes before the
        ! initialization guard, so this must not abort.
        call dst%append_from(empty)
        call check(error, dst%nrows() == 0_int64 .and. .not. dst%is_init(), &
            "appending an empty map must leave the destination exactly as it was")
        if (allocated(error)) return

        ! And onto a populated one, where the negative control is that the rows survive.
        call dst%init(PK_INT32)
        call dst%append_row(["a"], [1_int32])
        call dst%append_from(empty)
        call check(error, dst%nrows() == 1_int64, "an empty append must not disturb existing rows")
        if (allocated(error)) return
        call check(error, dst%validate(), "the column still satisfies its invariants")
    end subroutine test_init_unit_and_empty_append

end module test_map
