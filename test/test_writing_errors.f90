!> Abort-path tests for `writing`, split out of `test_writing.f90` so that the
!> reader/writer runner forks no subprocesses at all.
!!
!! **Why these live apart from the tests they are about.** Every test here drives
!! `test/error_scenarios.f90` through `run_error_scenario`/`check_scenario_*`, i.e. it spawns a
!! subprocess and asserts on its exit status and stderr. That is the one thing `run_tester_cpp`
!! must not do, so they gather in `run_tester_errors` beside the 913 tests already there.
!!
!! **The cost is locality and it is real**: a test asserting that a bad writing call aborts is a
!! test *about writing*, and it no longer sits beside the writing tests. Run
!! `run_tester_errors -- writing_errors` to see them as a group.
module test_writing_errors
    use parquet
    use parquet_maml_base
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_stderr, &
        check_scenario_exit_status_and_no_output, run_error_scenario, scenario_capture_contains
    !
    implicit none
    private
    public :: collect_tests_parquet_writing_errors

contains

    subroutine collect_tests_parquet_writing_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end: a single array constructor can exceed
        ! the standard's 255-continuation-line limit, which only nagfor enforces, and the
        ! self-referential `testsuite = [testsuite, ...]` form makes nagfor double-free each
        ! entry's name string at run time. See `test_writing.f90` for the full note.
        type(unittest_type), allocatable :: p1(:)
        !
        p1 = [ &
            new_unittest("qc=.true. prints a WARNING for an out-of-range numeric value", &
                test_qc_warning_printed_for_numeric_violation), &
            new_unittest("qc=.true. WARNING for a fractional bound uses fractional formatting", &
                test_qc_warning_printed_for_fractional_bound), &
            new_unittest("a qc bound that is integral but not written as an integer is judged in " // &
                "int64", test_qc_bound_recovered_exactly_from_real), &
            new_unittest("qc: a strict min:/max: operator excludes the boundary value", &
                test_qc_int64_strict_operators), &
            new_unittest("qc: a fractional bound still checks int64 values, in real64", &
                test_qc_int64_values_fractional_bound), &
            new_unittest("qc=.true. prints a WARNING for an out-of-range float64 value", &
                test_qc_warning_float64), &
            new_unittest("qc: an int64 bound is judged in int64, not after widening to real64", &
                test_qc_int64_bound_judged_in_int64), &
            new_unittest("qc: an int64 BOUND past 2**53 constrains exactly what it says", &
                test_qc_int64_exact_bound_past_2_53), &
            new_unittest("qc: a value equal to a bound past 2**53, and a huge(int64) bound, stay silent", &
                test_qc_int64_exact_bound_no_false_violation), &
            new_unittest("qc=.true. prints a WARNING for an out-of-range string value", &
                test_qc_warning_printed_for_string_violation), &
            new_unittest("qc: min:/max: on a boolean field are accepted but never enforced", &
                test_qc_min_max_ignored_for_boolean), &
            new_unittest("qc: miss: IS enforced on a boolean field, with its control", &
                test_qc_miss_enforced_for_boolean), &
            new_unittest("qc defaults to active whenever a schema is given (no explicit qc=), " // &
                "warning on an unexpected Null", test_qc_miss_default_active_numeric_warns), &
            new_unittest("qc: miss: absent means Nulls are allowed -- no WARNING printed", &
                test_qc_miss_absent_no_warning), &
            new_unittest("qc: miss: Null allows Nulls -- no WARNING printed", &
                test_qc_miss_declared_null_no_warning), &
            new_unittest("an empty qc: miss: warns on a Null in a compact string write", &
                test_qc_miss_string_warns), &
            new_unittest("an empty qc: miss: warns on a Null in a parquet_date write", &
                test_qc_miss_temporal_warns), &
            new_unittest("schema%add_field's own qc_min/qc_max drives real read-time enforcement", &
                test_add_field_qc_drives_reader_enforcement), &
            new_unittest("an unknown compression codec aborts", test_compression_unknown_aborts), &
            new_unittest("overwrite=.false. aborts when the file already exists", &
                test_overwrite_false_existing_file_aborts), &
            new_unittest("parquet_close_reader(print_stat=.true.) does not disturb a normal close", &
                test_close_reader_print_stat_smoke), &
            new_unittest("parquet_close_reader(print_stat=.true.) covers int64/float32/float64/string/" // &
                "boolean/qc/filter columns", test_close_reader_print_stat_all_types), &
            new_unittest("parquet_close_reader(print_stat=.true.) covers format_stat_scalar's default " // &
                "(non-special-cased scalar type) branch", test_close_reader_print_stat_default_scalar_type), &
            new_unittest("parquet_close_reader(print_stat=.true.) covers a released column's report row", &
                test_close_reader_print_stat_released_column), &
            new_unittest("parquet_close_reader(print_stat=.true.) covers the sort: summary line", &
                test_close_reader_print_stat_sorted_rows), &
            new_unittest("a string/string-vector column too large for arrow::utf8() round-trips via large_utf8()", &
                test_large_string_column_roundtrip), &
            new_unittest("a STRING_VIEW column (from a file written by another Arrow-based tool) round-trips " // &
                "correctly", test_string_view_column_roundtrip), &
            new_unittest("a streamed string column is stored as large_utf8 however small it is, while the " // &
                "same strings written whole stay plain utf8", test_streamed_string_is_always_large_utf8), &
            new_unittest("a vector column whose auto-sized row-group size is clamped for the int32 " // &
                "list-element-count limit still round-trips, split across multiple row groups", &
                test_list_element_count_auto_multi_row_group_roundtrip), &
            new_unittest("qc: range violation prints a WARNING but does not abort", &
                test_qc_range_violation_warns), &
            new_unittest("qc: range violation on a string column warns but does not abort", &
                test_qc_range_violation_string_warns), &
            new_unittest("qc: range violation on a float column warns but does not abort", &
                test_qc_range_violation_float_warns), &
            new_unittest("qc: range violation on an extended (uint16) source type warns but does not abort", &
                test_extended_qc_range_violation_warns), &
            new_unittest("qc: range violation on an int64 column warns but does not abort", &
                test_qc_range_violation_int64_warns), &
            new_unittest("qc-maml: a stray no-colon line before qc: still warns correctly", &
                test_qc_maml_stray_no_colon_line_ok), &
            new_unittest("qc: unexpected Null prints a WARNING but does not abort", &
                test_qc_null_violation_warns), &
            new_unittest("qc: an omitted miss: raises no read-side violation, with its control", &
                test_qc_miss_omitted_no_read_violation), &
            new_unittest("qc: miss: Null suppresses the Null-presence WARNING", &
                test_qc_miss_null_no_warning), &
            new_unittest("qc=.false. suppresses a would-be range violation WARNING", &
                test_qc_disabled_explicit_no_warning), &
            new_unittest("qc-maml may declare a column absent from the parquet file", &
                test_qc_column_not_in_file), &
            new_unittest("a flat (1-D) write on a still-'auto' col_size column aborts", &
                test_flat_write_col_size_still_auto_aborts) &
            ]
        !
        testsuite = p1
    end subroutine collect_tests_parquet_writing_errors

    !> qc=.true. never errors -- it only ever prints a WARNING to stdout and
    !> lets the write proceed. Since test-drive can't observe an in-process
    !> print statement's stdout reliably, the actual write (error_scenarios'
    !> "qc_warning_numeric"/"qc_warning_string" scenarios) is run as a
    !> subprocess with its stdout captured to a file, which is then checked
    !> for the expected WARNING text.
    subroutine test_qc_warning_printed_for_numeric_violation(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat

        call check_qc_scenario_warns(error, "qc_warning_numeric", "a", exitstat, cmdstat)
        if (allocated(error)) return

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range numeric value should not error stop (warning only)")
    end subroutine test_qc_warning_printed_for_numeric_violation

    !> Same as test_qc_warning_printed_for_numeric_violation, but the qc:
    !> bound is fractional ("0.5") rather than a whole number, so the WARNING
    !> text's bounds_desc must go through parquet_qc_format_real's
    !> fractional-value (g0.7) formatting branch instead of its whole-number
    !> (i0) one -- checked directly by looking for "0.5" in the captured
    !> output, not just the presence of a WARNING.
    subroutine test_qc_warning_printed_for_fractional_bound(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: found_bound_text

        call run_error_scenario("qc_warning_fractional_bound", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range fractional-bound value should not error stop (warning only)")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "0.5", found_bound_text)
        call check(error, found_bound_text, &
            "expected the qc violation WARNING to include the fractionally-formatted bound '0.5'")
    end subroutine test_qc_warning_printed_for_fractional_bound

    !> `qc_bound_as_int64` recovers an exact int64 bound from a real one, and DECLINES when it
    !> cannot -- two answers, and each needs its own control.
    !!
    !! `qc_numeric_i64` takes its exact bound from the declared TEXT first, and that parser accepts
    !! an optional sign and digits only -- so `9007199254740992.0` is rejected by it although the
    !! value is a whole number, and `qc_bound_as_int64` is what recovers it. The only other
    !! scenario reaching that function passes a genuinely fractional bound, where it returns at its
    !! very first test; nothing else exercises its range guard or its conversion.
    !!
    !! **The bound is past 2**53 deliberately, and that is what makes the test discriminating
    !! rather than decorative.** A first version used `min: 10.0`, and a mutation making the
    !! recovery always fail did not fail it: `parquet_qc_format_real` itself prints an integral
    !! value below 1e15 as a bare integer, so both answers rendered `min >= 10` and the message
    !! could not tell them apart. Past 1e15 the real formatter switches to `g0.7`, so an
    !! unrecovered bound prints as `0.9007199E+16` where a recovered one prints in full.
    !!
    !! **`past53`/`at53` then prove the recovered bound is what the COMPARISON used**, not merely
    !! what the message printed. Both declare `max: 9007199254740992.0`; 2**53 + 1 violates it in
    !! int64 and does not once widened to real64 (the two are the same real), so `past53` must warn
    !! and `at53`, holding 2**53 exactly, must not. `too_big` is the range guard's own control: a
    !! bound past int64's range must be declined and compared in real64, so it prints in exponent
    !! form rather than as the wrapped integer a conversion ignoring the range would produce.
    subroutine test_qc_bound_recovered_exactly_from_real(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: found

        call run_error_scenario("qc_int64_bound_exact_via_real", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, &
            "column 'mixed': declared min >= 9007199254740992, max <= 99.5", found)
        call check(error, found, &
            "min: 9007199254740992.0 must be recovered as that exact int64 bound and printed as " // &
            "one, while the fractional max: 99.5 stays a real -- the mixed-exactness report arm")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'past53'", found)
        call check(error, found, &
            "2**53 + 1 violates max: 9007199254740992.0 in int64 and not in real64, so the " // &
            "recovered bound must be what the comparison used")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'at53'", found)
        call check(error, .not. found, &
            "2**53 satisfies that same bound exactly, so this column must stay silent -- without " // &
            "it the assertion above would pass against a checker that warned about everything")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "column 'too_big': declared min >= 0.1", found)
        call check(error, found, &
            "a bound past int64's range must be DECLINED by the range guard and compared in " // &
            "real64, so it prints in exponent form rather than as a wrapped integer")
    end subroutine test_qc_bound_recovered_exactly_from_real

    !> A STRICT qc operator (`min: > 0`, `max: < 100`) must exclude the boundary value that its
    !> inclusive twin would accept -- qc_int64_satisfies' ">" and "<" arms. The scenario carries
    !> its own negative control (`inside`, one step in from each boundary), so an operator that
    !> had silently degraded into ">=" / "<=" fails the first assertion while a checker that
    !> warned indiscriminately fails the second.
    subroutine test_qc_int64_strict_operators(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_strict, warned_inside

        call run_error_scenario("qc_int64_strict_operators", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'strict'", warned_strict)
        call check(error, warned_strict, &
            "0 violates `min: > 0` and 100 violates `max: < 100`, but no qc warning named the " // &
            "'strict' column -- a strict operator was applied as if it were inclusive")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'inside'", warned_inside)
        call check(error, .not. warned_inside, &
            "1 and 99 satisfy `min: > 0` / `max: < 100`, so the 'inside' column must not warn")
    end subroutine test_qc_int64_strict_operators

    !> A fractional bound has no exact int64 equivalent, so qc_numeric_i64 must fall back to
    !> comparing in real64 rather than skipping the check. Reached with a float64-typed schema
    !> column handed int64 values -- see scenario_qc_int64_values_fractional_bound's own comment
    !> for why that is the only route to it. `frac_ok` is the negative control.
    subroutine test_qc_int64_values_fractional_bound(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_frac, warned_ok

        call run_error_scenario("qc_int64_values_fractional_bound", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'frac'", warned_frac)
        call check(error, warned_frac, &
            "1 and 10 violate the fractional bounds [1.5, 9.5], but no qc warning named the 'frac' " // &
            "column -- a bound with no exact int64 form left the values unchecked")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'frac_ok'", warned_ok)
        call check(error, .not. warned_ok, &
            "2 and 9 satisfy the fractional bounds [1.5, 9.5], so the 'frac_ok' column must not warn")
    end subroutine test_qc_int64_values_fractional_bound

    !> Write-time qc's float64 checker must report a violation, not merely count one. Every other
    !> value kind's checker was already observed reporting; this is the one that was not.
    !> `f64_ok` is the negative control on the same declaration.
    subroutine test_qc_warning_float64(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_f64, warned_ok

        call run_error_scenario("qc_warning_float64", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'f64'", warned_f64)
        call check(error, warned_f64, &
            "0.25 and 12.5 fall outside [1, 10], but no qc warning named the 'f64' column -- " // &
            "the float64 checker never reported what it had counted")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'f64_ok'", warned_ok)
        call check(error, .not. warned_ok, &
            "every value of 'f64_ok' is within [1, 10], so it must not warn")
    end subroutine test_qc_warning_float64

    !> An int64 qc bound is judged in int64, not after widening every value to real64: a value of
    !> 2**53 + 1 against `max: 2**53` violates, and must be reported as such even though the two
    !> are indistinguishable once widened. The same scenario carries the negative control -- a
    !> second column holding 2**53 exactly, which satisfies the bound and must stay silent -- so a
    !> checker that warned about every int64 column could not pass this.
    !>
    !> Before this was fixed the `over` column produced no warning at all, which is the failure
    !> mode worth remembering: qc reported compliance for data that violated its declaration.
    subroutine test_qc_int64_bound_judged_in_int64(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_over, warned_at

        call run_error_scenario("qc_int64_beyond_float64_precision", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'over'", warned_over)
        call check(error, warned_over, &
            "2**53 + 1 violates max: 2**53, but no qc warning named the 'over' column -- the bound " // &
            "was compared after widening the value to real64, which rounds it onto the bound")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'at'", warned_at)
        call check(error, .not. warned_at, &
            "2**53 satisfies max: 2**53, so the 'at' column must not produce a qc warning")
    end subroutine test_qc_int64_bound_judged_in_int64

    !> A qc BOUND past 2**53 must constrain what it says. The bound text is parsed straight to
    !> int64 rather than through real64, so `max: 9007199254740993` is that number and not the
    !> 9007199254740992 real64 rounds it to -- and the WARNING quotes the same number the
    !> comparison used, instead of `0.9007199E+16`.
    !>
    !> Two failure modes are covered, and the second is the one that shipped: a value one PAST the
    !> bound must warn (positive half), and a value exactly EQUAL to the bound must not (negative
    !> half, scenario_qc_int64_exact_bound_no_false_violation). Under the old real64 route the
    !> equal value was reported as a violation -- qc contradicting a declaration the data honoured
    !> exactly -- and `max: 9223372036854775807` was rejected outright at validation time, so that
    !> scenario would abort rather than merely warn. See feature_risks.md Risk-68.
    subroutine test_qc_int64_exact_bound_past_2_53(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned, exact_bound, exact_range

        call run_error_scenario("qc_int64_exact_bound_violation", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'over'", warned)
        call check(error, warned, &
            "9007199254740994 exceeds max: 9007199254740993, but no qc warning named the 'over' " // &
            "column -- the bound was rounded down to 2**53 on its way through real64")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "max <= 9007199254740993", exact_bound)
        call check(error, exact_bound, &
            "the WARNING must quote the declared bound exactly (9007199254740993), not a real64 " // &
            "rendering of it such as 0.9007199E+16 -- otherwise it names a different number from " // &
            "the one the comparison actually applied")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "[9007199254740994, 9007199254740994]", exact_range)
        call check(error, exact_range, &
            "the WARNING's observed data range must be rendered in int64 too, exactly")
    end subroutine test_qc_int64_exact_bound_past_2_53

    !> Negative control for the test above: a value exactly equal to a bound past 2**53 is
    !> compliant and must stay silent, and huge(int64) must be a legal bound rather than a
    !> validation error. Without this, the positive test passes just as happily against a checker
    !> that warns about every int64 column, or against one whose bounds still round.
    subroutine test_qc_int64_exact_bound_no_false_violation(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_int64_exact_bound_no_false_violation", &
            expect_abort=.false., &
            failure_message="a value equal to its own max: past 2**53, and a max: of huge(int64), " // &
                "must both be accepted in silence", &
            forbidden_text="qc violation")
    end subroutine test_qc_int64_exact_bound_no_false_violation

    subroutine test_qc_warning_printed_for_string_violation(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat

        call check_qc_scenario_warns(error, "qc_warning_string", "s", exitstat, cmdstat)
        if (allocated(error)) return

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, &
            "qc=.true. with an out-of-range string value should not error stop (warning only)")
    end subroutine test_qc_warning_printed_for_string_violation

    !> Covers min:/max: ONLY -- a qc: miss: on a boolean IS enforced, and
    !> test_qc_miss_enforced_for_boolean is what covers that.
    subroutine test_qc_min_max_ignored_for_boolean(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: found_warning

        call run_error_scenario("qc_min_max_ignored_for_boolean", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "qc: min:/max: on a boolean field must not error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "WARNING", found_warning)
        call check(error, .not. found_warning, &
            "a qc: min:/max: bound on a boolean field must never print a WARNING -- it is not enforced")
    end subroutine test_qc_min_max_ignored_for_boolean

    !> A qc: miss: on a boolean column IS enforced -- the half its sibling test cannot see, because
    !> that scenario declares no miss: and writes no Null. The scenario carries its own control: two
    !> boolean columns over identical data, one declaring an empty miss: (must warn) and one
    !> declaring miss: Null (must not), so a checker that warned indiscriminately fails the second
    !> assertion while one that had stopped enforcing miss: on booleans fails the first.
    !>
    !> This gap is how the user guide came to claim that qc: is "never enforced" on a boolean field:
    !> the only test in the area asserted no-warning for a scenario that could not have produced one.
    subroutine test_qc_miss_enforced_for_boolean(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_banned, warned_allowed

        call run_error_scenario("qc_miss_enforced_for_boolean", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "a qc: miss: violation must warn, never error stop")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'banned'", warned_banned)
        call check(error, warned_banned, &
            "a boolean column declaring an EMPTY qc: miss: and holding a Null must produce the " // &
            "Null-presence WARNING -- miss: is enforced on boolean exactly as on any other type")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'allowed'", warned_allowed)
        call check(error, .not. warned_allowed, &
            "a boolean column declaring qc: miss: Null expects Nulls, so it must stay silent")
    end subroutine test_qc_miss_enforced_for_boolean

    subroutine test_qc_miss_default_active_numeric_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_miss_default_active_numeric_warns", &
            expect_abort=.false., &
            failure_message="a Null with qc: miss: not declared must warn, not abort, even with no explicit qc=", &
            required_stderr="WARNING: qc violation for column 'id'")
    end subroutine test_qc_miss_default_active_numeric_warns

    !> The negative control for test_qc_miss_default_active_numeric_warns: a field declaring no
    !! qc: miss: at all says nothing about Nulls, so writing one must print nothing. Asserted as an
    !! absence, which is only meaningful because its sibling asserts the presence on an otherwise
    !! identical schema -- the pair is what pins "empty declares, absent does not", and either test
    !! alone passes against a check that fires always or never.
    subroutine test_qc_miss_absent_no_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_miss_absent_no_warning", &
            expect_abort=.false., &
            failure_message="an absent qc: miss: must not make a Null a qc violation", &
            forbidden_text="WARNING: qc violation")
    end subroutine test_qc_miss_absent_no_warning

    subroutine test_qc_miss_declared_null_no_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_miss_declared_null_no_warning", &
            expect_abort=.false., &
            failure_message="qc: miss: Null scenario was expected to exit cleanly", &
            forbidden_text="WARNING: qc violation")
    end subroutine test_qc_miss_declared_null_no_warning

    subroutine test_qc_miss_string_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_miss_string_warns", expect_abort=.false., &
            failure_message="a Null in a compact string write with qc: miss: not declared must warn, not abort", &
            required_stderr="WARNING: qc violation for column 's'")
    end subroutine test_qc_miss_string_warns

    subroutine test_qc_miss_temporal_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_miss_temporal_warns", expect_abort=.false., &
            failure_message="a Null parquet_date with qc: miss: not declared must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'd'")
    end subroutine test_qc_miss_temporal_warns

    subroutine test_add_field_qc_drives_reader_enforcement(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "add_field_qc_drives_reader_enforcement", &
            expect_abort=.false., &
            failure_message="an add_field-declared qc: min/max should enforce (warn) at read time too", &
            required_stderr="WARNING: qc violation for column 'ra'")
    end subroutine test_add_field_qc_drives_reader_enforcement

    subroutine test_compression_unknown_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_unknown_compression", expect_abort=.true., &
            failure_message="an unknown compression codec name was expected to error stop")
    end subroutine test_compression_unknown_aborts

    !> parquet_open_writer(..., overwrite=.false.) must error stop rather than truncate an
    !> already-existing file (out-of-process: error stop kills the test process itself).
    subroutine test_overwrite_false_existing_file_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_overwrite_false_existing_file", expect_abort=.true., &
            failure_message="overwrite=.false. over an existing file was expected to error stop")
    end subroutine test_overwrite_false_existing_file_aborts

    !> print_stat=.true. always prints to stdout -- run out-of-process (see
    !> scenario_print_stat_smoke in error_scenarios.f90) so that output is
    !> captured/discarded by check_scenario_exit_status instead of
    !> interleaving with test-drive's own progress lines in the visible
    !> `fpm test` console output.
    subroutine test_close_reader_print_stat_smoke(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_stat_smoke", expect_abort=.false., &
            failure_message="parquet_close_reader(print_stat=.true.) was expected to exit cleanly")
    end subroutine test_close_reader_print_stat_smoke

    !> See scenario_print_stat_all_types in error_scenarios.f90 for why this exists
    !> (format_stat_scalar's INT64/FLOAT/DOUBLE/STRING cases, the boolean True/False count
    !> display, the qc bound display column, and the active-filter display column had never
    !> fired through print_stat).
    subroutine test_close_reader_print_stat_all_types(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_stat_all_types", expect_abort=.false., &
            failure_message="parquet_close_reader(print_stat=.true.) over int64/float32/float64/string/" // &
            "boolean/qc/filter columns was expected to exit cleanly")
    end subroutine test_close_reader_print_stat_all_types

    !> See scenario_print_stat_default_scalar_type in error_scenarios.f90 for why this exists
    !> (format_stat_scalar's `default: return s->ToString();` branch, for a scalar min/max type
    !> not explicitly cased).
    subroutine test_close_reader_print_stat_default_scalar_type(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_stat_default_scalar_type", expect_abort=.false., &
            failure_message="parquet_close_reader(print_stat=.true.) over a UINT64 column was expected " // &
            "to exit cleanly")
    end subroutine test_close_reader_print_stat_default_scalar_type

    !> See scenario_print_stat_released_column in error_scenarios.f90 for why this exists
    !> (parquet_reader_print_stat's "released" branch, for a column read then freed via
    !> parquet_release_column before print_stat=.true. walks the touched-column list).
    subroutine test_close_reader_print_stat_released_column(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_stat_released_column", expect_abort=.false., &
            failure_message="parquet_close_reader(print_stat=.true.) over a released column was expected " // &
            "to exit cleanly")
    end subroutine test_close_reader_print_stat_released_column

    !> See scenario_print_stat_sorted_rows in error_scenarios.f90 for why this exists
    !> (parquet_reader_print_stat's "sort: <key text>" summary line, printed only when a
    !> read-time sort is active).
    subroutine test_close_reader_print_stat_sorted_rows(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_stat_sorted_rows", expect_abort=.false., &
            failure_message="parquet_close_reader(print_stat=.true.) over a sorted reader was expected " // &
            "to exit cleanly")
    end subroutine test_close_reader_print_stat_sorted_rows

    !> A string or string-vector column whose byte payload would overflow Arrow's real int32
    !> STRING-offset limit (~2GiB) is written as arrow::large_utf8() instead of arrow::utf8()
    !> (see would_overflow_string_offset_limit in parquet_wrapper.cpp) and must still round-trip
    !> correctly. Exercising this for real would need a genuine multi-gigabyte column (tens of
    !> seconds to build), far too slow for this suite -- so the actual round-trip (write, read
    !> back, parquet_get_string_length, print_stat, and a row filter, all against a tiny fixture
    !> forced onto the large_utf8 path via a test-only threshold override) runs out-of-process
    !> as scenario_large_string_roundtrip in error_scenarios.f90; see that scenario's own
    !> comment for why the override is safe only when isolated like this.
    subroutine test_large_string_column_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "large_string_roundtrip", expect_abort=.false., &
            failure_message="a string/string-vector column forced onto the arrow::large_utf8() path " // &
            "did not round-trip correctly")
    end subroutine test_large_string_column_roundtrip

    !> Unlike LARGE_STRING (above), this library's own writer can never produce a STRING_VIEW
    !> column at all -- it only ever arrives from a Parquet file written by another Arrow-based
    !> tool whose stored Arrow schema declared the column as utf8_view() (see
    !> is_string_like_type's own comment in parquet_wrapper.cpp). So the fixture here is built
    !> directly with Arrow's own StringViewBuilder (parquet_debug_write_string_view_fixture, a
    !> test-only hook), bypassing this library's writer entirely -- run out-of-process as
    !> scenario_string_view_roundtrip in error_scenarios.f90; see that scenario's own comment.
    subroutine test_string_view_column_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "string_view_roundtrip", expect_abort=.false., &
            failure_message="a STRING_VIEW column did not round-trip correctly")
    end subroutine test_string_view_column_roundtrip

    !> doc/pages/types/supported-data-types.md's "Large string columns" says the utf8/large_utf8
    !> choice is made from the column's byte payload -- but only on the whole-column path. A
    !> STREAMED column takes large_utf8 unconditionally, because its Arrow field is fixed when the
    !> first row group locks the schema, before the later row groups' bytes exist
    !> (parquet_write_string_column_chunk, parquet_wrapper.cpp).
    !>
    !> The only observable is print_stat's `parquet_type` column on stdout, which test-drive cannot
    !> capture -- so both halves run out of process (scenario_streamed_string_is_large_utf8 and
    !> scenario_whole_string_is_plain_utf8 in error_scenarios.f90), same pattern as
    !> test_large_string_column_roundtrip above.
    !>
    !> The second scenario is the NEGATIVE CONTROL and is what makes the first mean anything: it
    !> writes the identical six short strings whole, and its capture must contain `string` but not
    !> `large_string`. Asserting only the streamed half would pass equally against a build that had
    !> started promoting every string column to large_utf8. They are two scenarios rather than one
    !> because scenario_capture_contains searches the whole capture for a substring, and "string"
    !> is a substring of "large_string" -- one capture holding both tables could not be told apart.
    subroutine test_streamed_string_is_always_large_utf8(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: streamed_is_large, whole_is_large, whole_is_string

        call run_error_scenario("streamed_string_is_large_utf8", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "streaming a small string column must not abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "large_string", streamed_is_large)
        call check(error, streamed_is_large, &
            "a streamed string column must be stored as large_utf8 even when tiny -- print_stat " // &
            "reported no large_string column")
        if (allocated(error)) return

        ! Negative control: the same strings, written whole, must NOT be promoted.
        call run_error_scenario("whole_string_is_plain_utf8", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "writing a small string column whole must not abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "large_string", whole_is_large)
        call check(error, .not. whole_is_large, &
            "a small string column written whole must stay plain utf8 -- print_stat reported " // &
            "large_string, so the size-based choice is not being made at all")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "string", whole_is_string)
        call check(error, whole_is_string, &
            "print_stat reported no string column at all for the whole-column control, so the " // &
            "control proves nothing")
    end subroutine test_streamed_string_is_always_large_utf8

    !> A vector column's flattened element count (nrows * col_size) is capped at 2^31-1 *per row
    !> group*, not per file -- close_parquet_writer's auto-sizing path silently clamps its own
    !> computed row-group size down to whatever is safe for the widest vector column present, so
    !> a column whose total nrows * col_size would otherwise exceed that limit now writes
    !> successfully, split across multiple row groups. Exercising this for real would need a
    !> genuine multi-billion-element column, far too slow/large for this suite -- so the actual
    !> round-trip runs out-of-process as scenario_list_element_count_auto_multi_row_group in
    !> error_scenarios.f90 (same pattern as test_large_string_column_roundtrip, above), against a
    !> tiny fixture forced to split via a test-only threshold override; see that scenario's own
    !> comment for why the override is safe only when isolated like this.
    subroutine test_list_element_count_auto_multi_row_group_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "list_element_count_auto_multi_row_group", expect_abort=.false., &
            failure_message="a vector column split across multiple auto-clamped row groups " // &
            "did not round-trip correctly")
    end subroutine test_list_element_count_auto_multi_row_group_roundtrip

    !> Read-time qc, like print_stat, always prints straight to stdout --
    !> run out-of-process (see scenario_qc_range_violation_warns in
    !> error_scenarios.f90) for the same reason print_stat's smoke test
    !> does: keep that output out of the visible `fpm test` console log,
    !> while still asserting on its content via the captured file.
    subroutine test_qc_range_violation_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_warns", expect_abort=.false., &
            failure_message="a qc: range violation must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'ra'")
    end subroutine test_qc_range_violation_warns

    !> Same as test_qc_range_violation_warns, but proves run_qc_range_check's
    !> STRING/LARGE_STRING/STRING_VIEW branch (see scenario_qc_range_violation_string_warns's
    !> own comment in error_scenarios.f90).
    subroutine test_qc_range_violation_string_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_string_warns", expect_abort=.false., &
            failure_message="a qc: range violation on a string column must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'sv'")
    end subroutine test_qc_range_violation_string_warns

    !> Same as test_qc_range_violation_warns, but proves run_qc_range_check's
    !> FLOAT/DOUBLE/DECIMAL* bounds-description formatting branch (see
    !> scenario_qc_range_violation_float_warns's own comment in error_scenarios.f90).
    subroutine test_qc_range_violation_float_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_float_warns", expect_abort=.false., &
            failure_message="a qc: range violation on a float column must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'fv'")
    end subroutine test_qc_range_violation_float_warns

    !> Same as test_qc_range_violation_warns, but proves run_qc_range_check's
    !> extension to the new read-time source types (see
    !> scenario_extended_qc_range_violation_warns's own comment in
    !> error_scenarios.f90) actually fires rather than silently never
    !> checking a UINT16 column.
    subroutine test_extended_qc_range_violation_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_qc_range_violation_warns", expect_abort=.false., &
            failure_message="a qc: range violation on a uint16 column must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'v_uint16'")
    end subroutine test_extended_qc_range_violation_warns

    !> Same as test_qc_range_violation_warns, but on a plain INT64 column instead of INT32 --
    !> proves run_qc_range_check's is_small_integer_family branch actually reaches
    !> small_integer_value_at's own INT64 case arm (see
    !> scenario_qc_range_violation_int64_warns's own comment in error_scenarios.f90).
    subroutine test_qc_range_violation_int64_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_int64_warns", expect_abort=.false., &
            failure_message="a qc: range violation on an int64 column must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'v'")
    end subroutine test_qc_range_violation_int64_warns

    !> A stray line with no colon inside a qc-maml field block (before its
    !> qc: sub-block) must be silently skipped rather than breaking parsing
    !> -- checked by confirming the qc: min: bound declared right after it
    !> is still correctly recognized and enforced (the WARNING fires).
    subroutine test_qc_maml_stray_no_colon_line_ok(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_stray_no_colon_line", expect_abort=.false., &
            failure_message="a qc-maml with a stray no-colon line before its qc: block must still warn, not abort", &
            required_stderr="WARNING: qc violation for column 'ra'")
    end subroutine test_qc_maml_stray_no_colon_line_ok

    subroutine test_qc_null_violation_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_null_violation_warns", expect_abort=.false., &
            failure_message="a qc: unexpected-Null violation must warn, not abort", &
            required_stderr="WARNING: qc violation for column 'id'")
    end subroutine test_qc_null_violation_warns

    !> Read side, the third qc: miss: state: a qc: block declaring min:/max: but NO miss: says
    !> nothing about Nulls, so reading a Null in that column raises nothing. The other two states
    !> already had tests (qc_miss_null_no_warning for miss: Null, qc_null_violation_warns for an
    !> explicit empty miss:); this was the gap, and it is the state the guide had backwards.
    !>
    !> The scenario's own control is the second column, which declares an EMPTY miss: over a Null in
    !> the same position and must warn -- so this cannot pass against a build where the read-side
    !> Null check never runs at all.
    subroutine test_qc_miss_omitted_no_read_violation(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: warned_omitted, warned_explicit

        call run_error_scenario("qc_miss_omitted_no_read_violation", exitstat, cmdstat, out_file, err_file)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat == 0, "qc_soft=.true. must warn rather than abort")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'omitted'", warned_omitted)
        call check(error, .not. warned_omitted, &
            "a qc: block that declares no miss: says nothing about Nulls, so reading a Null in " // &
            "that column must raise no violation -- only an explicit EMPTY miss: asks for the check")
        if (allocated(error)) return

        call scenario_capture_contains(out_file, err_file, "qc violation for column 'explicit'", warned_explicit)
        call check(error, warned_explicit, &
            "the control column declares an EMPTY qc: miss: over a Null and must warn -- without " // &
            "it this test would pass against a reader that never checks Nulls at all")
    end subroutine test_qc_miss_omitted_no_read_violation

    subroutine test_qc_miss_null_no_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_miss_null_no_warning", expect_abort=.false., &
            failure_message="qc: miss: Null scenario was expected to exit cleanly", &
            forbidden_text="WARNING: qc violation")
    end subroutine test_qc_miss_null_no_warning

    subroutine test_qc_disabled_explicit_no_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_no_output(error, "qc_disabled_explicit_no_warning", expect_abort=.false., &
            failure_message="qc=.false. scenario was expected to exit cleanly", &
            forbidden_text="WARNING: qc violation")
    end subroutine test_qc_disabled_explicit_no_warning

    subroutine test_qc_column_not_in_file(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "qc_column_not_in_file", expect_abort=.false., &
            failure_message="a qc-maml field naming a column absent from the file was expected to exit cleanly")
    end subroutine test_qc_column_not_in_file

    subroutine test_flat_write_col_size_still_auto_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "flat_write_col_size_still_auto", expect_abort=.true., &
            failure_message="a flat write on a still-'auto' col_size column should abort", &
            required_stderr="is still 'auto'")
    end subroutine test_flat_write_col_size_still_auto_aborts

    !> Runs error_scenarios' `scenario_name` (via run_error_scenario, so either from
    !> prime_error_scenarios' pre-run capture or from a fresh spawn) and asserts its output
    !> contains a WARNING mentioning `column_name`.
    subroutine check_qc_scenario_warns(error, scenario_name, column_name, exitstat, cmdstat)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario_name, column_name
        integer, intent(out) :: exitstat, cmdstat
        character(len=:), allocatable :: out_file, err_file
        logical :: found_warning, found_on_err

        call run_error_scenario(scenario_name, exitstat, cmdstat, out_file, err_file)
        if (cmdstat /= 0) return

        ! Both streams, for the same reason check_scenario_exit_status_and_stderr searches both:
        ! the capture is no longer merged, and this assertion only cares that the WARNING was
        ! printed somewhere.
        call file_contains_warning_for_column(out_file, column_name, found_warning)
        if (.not. found_warning) then
            call file_contains_warning_for_column(err_file, column_name, found_on_err)
            found_warning = found_on_err
        end if
        call check(error, found_warning, &
            "expected a WARNING message mentioning column '" // trim(column_name) // "' in stdout")
    end subroutine check_qc_scenario_warns

    subroutine file_contains_warning_for_column(filename, column_name, found)
        character(len=*), intent(in) :: filename, column_name
        logical, intent(out) :: found
        integer :: unit, ios
        character(len=512) :: line

        found = .false.
        open(newunit=unit, file=filename, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        do
            read(unit, '(a)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, "WARNING") > 0 .and. index(line, "'" // trim(column_name) // "'") > 0) found = .true.
        end do
        close(unit)
    end subroutine file_contains_warning_for_column

end module test_writing_errors
