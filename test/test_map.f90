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
            new_unittest("map reserve and shrink_to_fit move capacity only", test_capacity) &
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

end module test_map
