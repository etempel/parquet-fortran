!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the independent parquet_temporal module (parquet_date, parquet_time,
!> parquet_timestamp element types). Runs in isolation from the parquet read/write layer,
!> except for the "Arrow cross-validation" test, which calls two test-only parquet_debug_*
!> hooks in src/parquet_wrapper.cpp to compare this module's pure-Fortran civil<->days
!> calendar math against Arrow's vendored copy of the same (Hinnant) algorithm.
module test_temporal
    use parquet_temporal
    use iso_fortran_env, only : int32, int64, real64
    use iso_c_binding, only : c_int32_t, c_int64_t
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_temporal
    !
contains
    !
    subroutine collect_tests_parquet_temporal(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("date civil set/get round-trips", test_date_civil_roundtrip), &
            new_unittest("date MJD anchors and dual-kind set_mjd", test_date_mjd), &
            new_unittest("date to_string/parse round-trips", test_date_strings), &
            new_unittest("date null semantics", test_date_null), &
            new_unittest("date comparison operators", test_date_operators), &
            new_unittest("time set/get fields and boundaries", test_time_fields), &
            new_unittest("time to_string fraction groups and parse", test_time_strings), &
            new_unittest("time raw accessors and null semantics", test_time_raw_null), &
            new_unittest("time comparison operators", test_time_operators), &
            new_unittest("timestamp civil set/get round-trips", test_ts_civil), &
            new_unittest("timestamp epoch anchors (J2000, Unix)", test_ts_anchors), &
            new_unittest("timestamp set_unix/to_unix all units", test_ts_unix), &
            new_unittest("timestamp MJD/JD round-trips", test_ts_mjd_jd), &
            new_unittest("timestamp to_string/parse round-trips", test_ts_strings), &
            new_unittest("timestamp date/time parts and null propagation", test_ts_parts_null), &
            new_unittest("timestamp comparison operators", test_ts_operators), &
            new_unittest("whole-array elemental operations", test_elemental_arrays), &
            new_unittest("Arrow cross-validation of civil<->days math", test_arrow_cross_validation) &
            ]
    end subroutine collect_tests_parquet_temporal
    !
    ! ------------------------------------------------------------------------------
    !
    subroutine test_date_civil_roundtrip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: d
        integer(int32) :: y, mo, dd
        ! epoch anchor
        call d%set(1970, 1, 1)
        call check(error, d%raw() == 0_int32, "1970-01-01 must be day 0")
        if (allocated(error)) return
        ! leap-day round-trips (century rules: 2000 leap, 1900/2100 not, 2024 leap)
        call d%set(2000, 2, 29)
        call d%get(y, mo, dd)
        call check(error, y == 2000 .and. mo == 2 .and. dd == 29, "2000-02-29 round-trip")
        if (allocated(error)) return
        call d%set(2024, 2, 29)
        call check(error, d%year() == 2024 .and. d%month() == 2 .and. d%day() == 29, "2024-02-29 getters")
        if (allocated(error)) return
        ! day-count continuity across the 1900 (non-leap) February
        call d%set(1900, 2, 28)
        y = d%raw()
        call d%set(1900, 3, 1)
        call check(error, d%raw() - y == 1_int32, "1900-02-28 -> 1900-03-01 must be 1 day (non-leap)")
        if (allocated(error)) return
        ! ... and across the 2000 (leap) February
        call d%set(2000, 2, 28)
        y = d%raw()
        call d%set(2000, 3, 1)
        call check(error, d%raw() - y == 2_int32, "2000-02-28 -> 2000-03-01 must be 2 days (leap)")
        if (allocated(error)) return
        ! negative (astronomical) years, proleptic Gregorian
        call d%set(-44, 3, 15)
        call d%get(y, mo, dd)
        call check(error, y == -44 .and. mo == 3 .and. dd == 15, "year -44 round-trip")
        if (allocated(error)) return
        ! extremes: the full int32 day range must round-trip through get/set
        call d%set_raw(huge(0_int32))
        call d%get(y, mo, dd)
        call d%set(y, mo, dd)
        call check(error, d%raw() == huge(0_int32), "civil round-trip at day huge(int32)")
        if (allocated(error)) return
        call d%set_raw(-huge(0_int32) - 1_int32)
        call d%get(y, mo, dd)
        call d%set(y, mo, dd)
        call check(error, d%raw() == -huge(0_int32) - 1_int32, "civil round-trip at day -huge-1")
    end subroutine test_date_civil_roundtrip
    !
    subroutine test_date_mjd(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: d
        character(len=:), allocatable :: s
        ! MJD epoch anchor: MJD 0 = 1858-11-17
        call d%set_mjd(0)
        call d%to_string(s)
        call check(error, s == "1858-11-17", "MJD 0 must be 1858-11-17, got "//s)
        if (allocated(error)) return
        ! Unix epoch anchor: 1970-01-01 = MJD 40587
        call d%set(1970, 1, 1)
        call check(error, d%to_mjd() == 40587_int64, "1970-01-01 must be MJD 40587")
        if (allocated(error)) return
        ! dual-kind set_mjd (int32 and int64 actual arguments select their specifics)
        call d%set_mjd(60507_int32)
        call d%to_string(s)
        call check(error, s == "2024-07-16", "set_mjd int32: MJD 60507, got "//s)
        if (allocated(error)) return
        call d%set_mjd(60507_int64)
        call check(error, d%to_mjd() == 60507_int64, "set_mjd int64 round-trip")
    end subroutine test_date_mjd
    !
    subroutine test_date_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: d
        character(len=:), allocatable :: s
        logical :: ok
        ! basic round-trip and 4-digit padding
        call d%set(987, 6, 5)
        call d%to_string(s)
        call check(error, s == "0987-06-05", "year padded to 4 digits, got "//s)
        if (allocated(error)) return
        call d%parse(s)
        call check(error, d%year() == 987 .and. d%month() == 6 .and. d%day() == 5, "padded year parse")
        if (allocated(error)) return
        ! negative year round-trip
        call d%set(-44, 3, 15)
        call d%to_string(s)
        call check(error, s == "-0044-03-15", "negative year to_string, got "//s)
        if (allocated(error)) return
        call d%parse(s, success=ok)
        call check(error, ok .and. d%year() == -44, "negative year parse round-trip")
        if (allocated(error)) return
        ! > 4-digit year round-trip
        call d%set(123456, 1, 2)
        call d%to_string(s)
        call check(error, s == "123456-01-02", "wide year to_string, got "//s)
        if (allocated(error)) return
        call d%parse(s, success=ok)
        call check(error, ok .and. d%year() == 123456, "wide year parse round-trip")
        if (allocated(error)) return
        ! surrounding blanks are accepted
        call d%parse("  2024-07-16  ", success=ok)
        call check(error, ok .and. d%to_mjd() == 60507_int64, "blank-padded input parses")
        if (allocated(error)) return
        ! rejections: bad day, non-leap Feb 29 (1900 and 2100), garbage, short year, bad separators
        call d%parse("2024-04-31", success=ok)
        call check(error, .not. ok, "2024-04-31 must be rejected")
        if (allocated(error)) return
        call d%parse("1900-02-29", success=ok)
        call check(error, .not. ok, "1900-02-29 must be rejected (century non-leap)")
        if (allocated(error)) return
        call d%parse("2100-02-29", success=ok)
        call check(error, .not. ok, "2100-02-29 must be rejected (century non-leap)")
        if (allocated(error)) return
        call d%parse("not-a-date", success=ok)
        call check(error, .not. ok, "garbage must be rejected")
        if (allocated(error)) return
        call d%parse("24-07-16", success=ok)
        call check(error, .not. ok, "2-digit year must be rejected")
        if (allocated(error)) return
        call d%parse("2024/07/16", success=ok)
        call check(error, .not. ok, "slash separators must be rejected")
        if (allocated(error)) return
        call d%parse("", success=ok)
        call check(error, .not. ok, "empty string must be rejected")
        if (allocated(error)) return
        ! out-of-range year (valid syntax, days beyond int32) is a parse failure, not an abort
        call d%parse("6000000-01-01", success=ok)
        call check(error, .not. ok, "beyond-range year must be rejected via success")
        if (allocated(error)) return
        ! a non-digit character inside a correctly-positioned field is rejected (str_to_int's
        ! digit-check loop, not caught by the earlier separator-position check)
        call d%parse("20x4-07-16", success=ok)
        call check(error, .not. ok, "non-digit character in year field must be rejected")
        if (allocated(error)) return
        ! a year field longer than str_to_int's 18-digit int64 safety cap is rejected
        call d%parse("123456789012345678901-07-16", success=ok)
        call check(error, .not. ok, "a >18-digit year field must be rejected")
    end subroutine test_date_strings
    !
    subroutine test_date_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: d
        logical :: ok
        ! a default-initialized element is null
        call check(error, d%is_null(), "default-initialized date must be null")
        if (allocated(error)) return
        ! interop accessor on null: placeholder 0, no abort
        call check(error, d%raw() == 0_int32, "raw() on null must return 0")
        if (allocated(error)) return
        ! set makes it valid; set_null resets
        call d%set(2024, 7, 16)
        call check(error, .not. d%is_null(), "set marks valid")
        if (allocated(error)) return
        call d%set_null()
        call check(error, d%is_null(), "set_null marks null")
        if (allocated(error)) return
        ! caught parse failure leaves the element null (a defined state)
        call d%set(2024, 7, 16)
        call d%parse("garbage", success=ok)
        call check(error, .not. ok .and. d%is_null(), "failed parse must leave the element null")
        if (allocated(error)) return
        ! set_raw marks valid
        call d%set_raw(-1_int32)
        call check(error, .not. d%is_null() .and. d%raw() == -1_int32, "set_raw marks valid")
    end subroutine test_date_null
    !
    subroutine test_date_operators(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: a, b
        call a%set(2024, 7, 16)
        call b%set(2024, 7, 17)
        call check(error, a < b .and. a <= b .and. b > a .and. b >= a .and. a /= b, "strict ordering")
        if (allocated(error)) return
        call b%set(2024, 7, 16)
        call check(error, a == b .and. a <= b .and. a >= b .and. .not. (a < b), "equality")
        if (allocated(error)) return
        call b%set(-1000, 1, 1)
        call check(error, b < a, "pre-epoch date orders before")
    end subroutine test_date_operators
    !
    ! ------------------------------------------------------------------------------
    !
    subroutine test_time_fields(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_time) :: t
        integer(int32) :: h, mi, s, ns
        ! midnight boundary
        call t%set(0, 0, 0)
        call check(error, t%raw() == 0_int64, "00:00:00 must be raw 0")
        if (allocated(error)) return
        ! end-of-day boundary
        call t%set(23, 59, 59, 999999999)
        call check(error, t%raw() == 86400_int64*1000000000_int64 - 1_int64, "23:59:59.999999999 raw")
        if (allocated(error)) return
        call t%get(h, mi, s, ns)
        call check(error, h == 23 .and. mi == 59 .and. s == 59 .and. ns == 999999999, "end-of-day get")
        if (allocated(error)) return
        ! getters and optional nanosecond defaulting
        call t%set(12, 34, 56)
        call check(error, t%hour() == 12 .and. t%minute() == 34 .and. t%second() == 56 &
            .and. t%nanosecond() == 0, "individual getters, nanosecond defaults to 0")
        if (allocated(error)) return
        call t%get(h, mi, s) ! nanosecond argument omitted
        call check(error, h == 12 .and. mi == 34 .and. s == 56, "get without nanosecond argument")
    end subroutine test_time_fields
    !
    subroutine test_time_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_time) :: t
        character(len=:), allocatable :: s
        integer(int32) :: h, mi, sec, ns
        logical :: ok
        ! no fraction
        call t%set(7, 8, 9)
        call t%to_string(s)
        call check(error, s == "07:08:09", "no-fraction to_string, got "//s)
        if (allocated(error)) return
        ! 3-digit (millisecond) group
        call t%set(23, 59, 59, 500000000)
        call t%to_string(s)
        call check(error, s == "23:59:59.500", "millisecond group, got "//s)
        if (allocated(error)) return
        ! 6-digit (microsecond) group
        call t%set(0, 0, 1, 123456000)
        call t%to_string(s)
        call check(error, s == "00:00:01.123456", "microsecond group, got "//s)
        if (allocated(error)) return
        ! 9-digit (nanosecond) group
        call t%set(0, 0, 1, 123456789)
        call t%to_string(s)
        call check(error, s == "00:00:01.123456789", "nanosecond group, got "//s)
        if (allocated(error)) return
        ! parse round-trips, including a short fraction padded right
        call t%parse("07:08:09.25", success=ok)
        call t%get(h, mi, sec, ns)
        call check(error, ok .and. h == 7 .and. mi == 8 .and. sec == 9 .and. ns == 250000000, &
            "fraction .25 must mean 250 ms")
        if (allocated(error)) return
        call t%parse("23:59:59.999999999", success=ok)
        call check(error, ok .and. t%raw() == 86400_int64*1000000000_int64 - 1_int64, "max time parses")
        if (allocated(error)) return
        ! rejections
        call t%parse("24:00:00", success=ok)
        call check(error, .not. ok, "24:00:00 must be rejected")
        if (allocated(error)) return
        call t%parse("12:60:00", success=ok)
        call check(error, .not. ok, "minute 60 must be rejected")
        if (allocated(error)) return
        call t%parse("12:00:60", success=ok)
        call check(error, .not. ok, "second 60 (leap second) must be rejected")
        if (allocated(error)) return
        call t%parse("12:00:00.", success=ok)
        call check(error, .not. ok, "empty fraction must be rejected")
        if (allocated(error)) return
        call t%parse("12:00:00.1234567890", success=ok)
        call check(error, .not. ok, "10-digit fraction must be rejected")
        if (allocated(error)) return
        call t%parse("12.00.00", success=ok)
        call check(error, .not. ok, "dot separators must be rejected")
        if (allocated(error)) return
        ! leading blanks before the time are skipped, same as parquet_date%parse
        call t%parse("  07:08:09.25", success=ok)
        call check(error, ok .and. t%hour() == 7 .and. t%nanosecond() == 250000000, &
            "leading blanks before a time must be skipped")
    end subroutine test_time_strings
    !
    subroutine test_time_raw_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_time) :: t
        logical :: ok
        call check(error, t%is_null(), "default-initialized time must be null")
        if (allocated(error)) return
        call check(error, t%raw() == 0_int64, "raw() on null must return 0")
        if (allocated(error)) return
        call t%set_raw(86399999999999_int64) ! 23:59:59.999999999
        call check(error, .not. t%is_null() .and. t%hour() == 23 .and. t%nanosecond() == 999999999, &
            "set_raw at end-of-day")
        if (allocated(error)) return
        call t%set_null()
        call check(error, t%is_null(), "set_null marks null")
        if (allocated(error)) return
        call t%parse("bad", success=ok)
        call check(error, .not. ok .and. t%is_null(), "failed parse must leave the element null")
    end subroutine test_time_raw_null
    !
    subroutine test_time_operators(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_time) :: a, b
        call a%set(12, 0, 0)
        call b%set(12, 0, 0, 1) ! one nanosecond later
        call check(error, a < b .and. a <= b .and. b > a .and. b >= a .and. a /= b, &
            "one-nanosecond ordering")
        if (allocated(error)) return
        call b%set(12, 0, 0)
        call check(error, a == b .and. .not. (a /= b), "equality")
    end subroutine test_time_operators
    !
    ! ------------------------------------------------------------------------------
    !
    subroutine test_ts_civil(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: ts
        integer(int32) :: y, mo, dd, h, mi, s, ns
        integer(int64) :: sec
        ! epoch anchor
        call ts%set(1970, 1, 1, 0, 0, 0)
        call ts%get_raw(sec, ns)
        call check(error, sec == 0_int64 .and. ns == 0, "epoch must be raw (0, 0)")
        if (allocated(error)) return
        ! full round-trip with nanoseconds
        call ts%set(2024, 7, 16, 12, 34, 56, 123456789)
        call ts%get(y, mo, dd, h, mi, s, ns)
        call check(error, y == 2024 .and. mo == 7 .and. dd == 16 .and. h == 12 .and. mi == 34 &
            .and. s == 56 .and. ns == 123456789, "civil round-trip with nanoseconds")
        if (allocated(error)) return
        ! optional nanosecond argument omitted on get
        call ts%get(y, mo, dd, h, mi, s)
        call check(error, y == 2024 .and. s == 56, "get without nanosecond argument")
        if (allocated(error)) return
        ! pre-epoch: nanoseconds stay normalized (0..999999999), civil fields exact
        call ts%set(1969, 12, 31, 23, 59, 59, 500000000)
        call ts%get_raw(sec, ns)
        call check(error, sec == -1_int64 .and. ns == 500000000, "pre-epoch normalized raw pair")
        if (allocated(error)) return
        call ts%get(y, mo, dd, h, mi, s, ns)
        call check(error, y == 1969 .and. mo == 12 .and. dd == 31 .and. h == 23 .and. mi == 59 &
            .and. s == 59 .and. ns == 500000000, "pre-epoch civil round-trip")
        if (allocated(error)) return
        ! set_raw round-trip
        call ts%set_raw(-1_int64, 999999999)
        call ts%get(y, mo, dd, h, mi, s, ns)
        call check(error, y == 1969 .and. ns == 999999999, "set_raw pre-epoch")
    end subroutine test_ts_civil
    !
    subroutine test_ts_anchors(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: ts
        ! J2000: 2000-01-01T12:00:00 = JD 2451545.0 = MJD 51544.5 = Unix 946728000
        call ts%set(2000, 1, 1, 12, 0, 0)
        call check(error, abs(ts%to_jd() - 2451545.0_real64) < 1.0e-8_real64, "J2000 must be JD 2451545.0")
        if (allocated(error)) return
        call check(error, abs(ts%to_mjd() - 51544.5_real64) < 1.0e-8_real64, "J2000 must be MJD 51544.5")
        if (allocated(error)) return
        call check(error, ts%to_unix(parquet_unit_seconds) == 946728000_int64, "J2000 Unix seconds")
        if (allocated(error)) return
        ! Unix epoch is MJD 40587.0 exactly
        call ts%set(1970, 1, 1, 0, 0, 0)
        call check(error, abs(ts%to_mjd() - 40587.0_real64) < 1.0e-12_real64, "Unix epoch must be MJD 40587.0")
    end subroutine test_ts_anchors
    !
    subroutine test_ts_unix(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: ts, ts2
        character(len=:), allocatable :: s
        ! one instant through all four units
        call ts%set(2024, 7, 16, 0, 0, 0, 123456789)
        call check(error, ts%to_unix(parquet_unit_nanos) == 1721088000123456789_int64, "to_unix nanos")
        if (allocated(error)) return
        call check(error, ts%to_unix(parquet_unit_micros, exact=.false.) == 1721088000123456_int64, &
            "to_unix micros floored")
        if (allocated(error)) return
        call check(error, ts%to_unix(parquet_unit_millis, exact=.false.) == 1721088000123_int64, &
            "to_unix millis floored")
        if (allocated(error)) return
        call check(error, ts%to_unix(parquet_unit_seconds, exact=.false.) == 1721088000_int64, &
            "to_unix seconds floored")
        if (allocated(error)) return
        ! exact conversions succeed without exact=.false. when no sub-unit precision exists
        call ts%set_unix(1721088000123_int64, parquet_unit_millis)
        call check(error, ts%to_unix(parquet_unit_millis) == 1721088000123_int64, "millis round-trip")
        if (allocated(error)) return
        call check(error, ts%to_unix(parquet_unit_micros) == 1721088000123000_int64, &
            "millis value converts exactly to micros")
        if (allocated(error)) return
        ! set_unix normalizes the same instant regardless of unit
        call ts2%set_unix(1721088000123000000_int64, parquet_unit_nanos)
        call check(error, ts == ts2, "same instant from millis and nanos must compare equal")
        if (allocated(error)) return
        ! int32 value specific
        call ts%set_unix(1721088000_int32, parquet_unit_seconds)
        call ts%to_string(s)
        call check(error, s == "2024-07-16T00:00:00", "set_unix int32 seconds, got "//s)
        if (allocated(error)) return
        ! pre-epoch flooring: -0.5 s floors to -1 s (floor, not truncation toward zero)
        call ts%set_unix(-500_int64, parquet_unit_millis)
        call check(error, ts%to_unix(parquet_unit_seconds, exact=.false.) == -1_int64, &
            "pre-epoch floor must round toward -infinity")
        if (allocated(error)) return
        call ts%to_string(s)
        call check(error, s == "1969-12-31T23:59:59.500", "-500 ms must be 1969-12-31T23:59:59.500, got "//s)
        if (allocated(error)) return
        ! pre-epoch normalization from a negative nanos value
        call ts%set_unix(-1_int64, parquet_unit_nanos)
        call ts%to_string(s)
        call check(error, s == "1969-12-31T23:59:59.999999999", "-1 ns normalization, got "//s)
    end subroutine test_ts_unix
    !
    subroutine test_ts_mjd_jd(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: ts, ts2
        type(parquet_date) :: dpart
        real(real64) :: mjd
        ! MJD round-trip within the documented ~1 us resolution
        call ts%set(2026, 7, 17, 6, 30, 15, 250000000)
        call ts2%set_mjd(ts%to_mjd())
        call check(error, abs(ts2%to_unix(parquet_unit_micros, exact=.false.) &
            - ts%to_unix(parquet_unit_micros, exact=.false.)) <= 2_int64, &
            "MJD round-trip must stay within 2 us")
        if (allocated(error)) return
        ! JD round-trip within the documented ~50 us resolution
        call ts2%set_jd(ts%to_jd())
        call check(error, abs(ts2%to_unix(parquet_unit_micros, exact=.false.) &
            - ts%to_unix(parquet_unit_micros, exact=.false.)) <= 100_int64, &
            "JD round-trip must stay within 100 us")
        if (allocated(error)) return
        ! JD - MJD offset consistency
        call check(error, abs((ts%to_jd() - ts%to_mjd()) - 2400000.5_real64) < 1.0e-6_real64, &
            "JD - MJD must be 2400000.5")
        if (allocated(error)) return
        ! integer MJD from set_mjd lands exactly at midnight
        mjd = 60000.0_real64
        call ts%set_mjd(mjd)
        call check(error, ts%get_time() == parquet_time(0, 0, 0), "whole MJD must be midnight")
        if (allocated(error)) return
        dpart = ts%get_date()
        call check(error, dpart%to_mjd() == 60000_int64, "whole MJD date part")
        if (allocated(error)) return
        ! pre-epoch (negative MJD fraction handling): MJD -0.5 = 1858-11-16T12:00:00
        call ts%set_mjd(-0.5_real64)
        call check(error, ts%get_date() == parquet_date(1858, 11, 16) &
            .and. ts%get_time() == parquet_time(12, 0, 0), "MJD -0.5 must be 1858-11-16T12:00:00")
        if (allocated(error)) return
        ! rounding-to-nanosecond edge case: a tiny negative MJD whose fractional part (after the
        ! frac<0 normalization) rounds UP to exactly one full day -- must roll over cleanly to
        ! the next day at ns_of_day=0, not leave an out-of-range ns_of_day sitting at NS_PER_DAY
        call ts%set_mjd(-1.0e-15_real64)
        call check(error, ts%get_date() == parquet_date(1858, 11, 17) &
            .and. ts%get_time() == parquet_time(0, 0, 0), "MJD rollover must land exactly at 1858-11-17T00:00:00")
    end subroutine test_ts_mjd_jd
    !
    subroutine test_ts_strings(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: ts, ts2
        character(len=:), allocatable :: s
        integer(int64) :: sec
        integer(int32) :: ns
        logical :: ok
        ! fraction groups mirror parquet_time
        call ts%set(2024, 7, 16, 12, 34, 56)
        call ts%to_string(s)
        call check(error, s == "2024-07-16T12:34:56", "no-fraction to_string, got "//s)
        if (allocated(error)) return
        call ts%set(2024, 7, 16, 12, 34, 56, 789000000)
        call ts%to_string(s)
        call check(error, s == "2024-07-16T12:34:56.789", "millisecond group, got "//s)
        if (allocated(error)) return
        ! 'T' separator, space separator, and trailing 'Z' all parse to the same instant
        call ts%parse("2024-07-16T12:34:56.789")
        call ts2%parse("2024-07-16 12:34:56.789Z", success=ok)
        call check(error, ok .and. ts == ts2, "space separator and Z must parse to the same instant")
        if (allocated(error)) return
        ! to_string -> parse round-trip at the extreme positive end of the int64 range
        call ts%set_raw(huge(0_int64), 999999999)
        call ts%to_string(s)
        call ts2%parse(s, success=ok)
        call ts2%get_raw(sec, ns)
        call check(error, ok .and. sec == huge(0_int64) .and. ns == 999999999, &
            "round-trip at huge(int64) seconds via "//s)
        if (allocated(error)) return
        ! rejections
        call ts%parse("2024-07-16", success=ok)
        call check(error, .not. ok, "date without time must be rejected")
        if (allocated(error)) return
        call ts%parse("2024-07-16T24:00:00", success=ok)
        call check(error, .not. ok, "hour 24 must be rejected")
        if (allocated(error)) return
        call ts%parse("2024-02-30T00:00:00", success=ok)
        call check(error, .not. ok, "Feb 30 must be rejected")
        if (allocated(error)) return
        ! beyond the representable range: valid syntax, caught as a parse failure
        call ts%parse("300000000000-01-01T00:00:00", success=ok)
        call check(error, .not. ok, "year beyond the parser bound must be rejected")
        if (allocated(error)) return
        ! the max civil year an int64-seconds instant can reach is 292277026596; one year
        ! beyond it is valid syntax but out of range
        call ts%parse("292277026597-01-01T00:00:00", success=ok)
        call check(error, .not. ok, "year beyond the int64 seconds range must be rejected")
        if (allocated(error)) return
        call ts%parse("292277026596-01-01T00:00:00", success=ok)
        call check(error, ok, "the max-year January 1 is still representable and must parse")
        if (allocated(error)) return
        ! leading blanks before the date part are skipped, same as parquet_date%parse
        call ts%parse("  2024-07-16T12:00:00", success=ok)
        call check(error, ok .and. ts%to_unix(parquet_unit_seconds) == 1721131200_int64, &
            "leading blanks before a timestamp must be skipped")
    end subroutine test_ts_strings
    !
    subroutine test_ts_parts_null(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: d, dpart
        type(parquet_time) :: t, tpart
        type(parquet_timestamp) :: ts
        logical :: ok
        ! composition and decomposition
        call d%set(2024, 7, 16)
        call t%set(1, 2, 3, 40000000)
        call ts%set(d, t)
        call check(error, ts%get_date() == d .and. ts%get_time() == t, "set(date, time) decomposes back")
        if (allocated(error)) return
        ! constructor generics
        ts = parquet_timestamp(d, t)
        call check(error, ts%get_date() == d, "constructor from date + time")
        if (allocated(error)) return
        ts = parquet_timestamp(2024, 7, 16, 1, 2, 3, 40000000)
        call check(error, ts%get_time() == t, "constructor from civil fields")
        if (allocated(error)) return
        call check(error, parquet_date(2024, 7, 16) == d .and. parquet_time(1, 2, 3, 40000000) == t, &
            "date and time constructors")
        if (allocated(error)) return
        ! pre-epoch decomposition: floor semantics keep the time-of-day non-negative
        call ts%set(1969, 12, 31, 23, 59, 59, 500000000)
        call check(error, ts%get_date() == parquet_date(1969, 12, 31) &
            .and. ts%get_time() == parquet_time(23, 59, 59, 500000000), "pre-epoch date/time parts")
        if (allocated(error)) return
        ! null propagation through composition (both directions) and decomposition
        call d%set_null()
        call ts%set(d, t)
        call check(error, ts%is_null(), "null date must propagate to a null timestamp")
        if (allocated(error)) return
        call d%set(2024, 7, 16)
        call t%set_null()
        ts = parquet_timestamp(d, t)
        call check(error, ts%is_null(), "null time must propagate to a null timestamp")
        if (allocated(error)) return
        dpart = ts%get_date()
        tpart = ts%get_time()
        call check(error, dpart%is_null() .and. tpart%is_null(), &
            "date/time parts of a null timestamp must be null")
        if (allocated(error)) return
        ! null bookkeeping and interop accessors
        call check(error, ts%is_null(), "default/propagated element is null")
        if (allocated(error)) return
        call ts%parse("garbage", success=ok)
        call check(error, .not. ok .and. ts%is_null(), "failed parse must leave the element null")
        if (allocated(error)) return
        call ts%set(2024, 7, 16, 0, 0, 0)
        call ts%set_null()
        call check(error, ts%is_null(), "set_null marks null")
    end subroutine test_ts_parts_null
    !
    subroutine test_ts_operators(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_timestamp) :: a, b
        ! nanosecond tie-break at equal seconds
        call a%set(2024, 7, 16, 12, 0, 0, 1)
        call b%set(2024, 7, 16, 12, 0, 0, 2)
        call check(error, a < b .and. a <= b .and. b > a .and. b >= a .and. a /= b, &
            "nanosecond tie-break ordering")
        if (allocated(error)) return
        ! cross-epoch ordering (negative vs positive seconds)
        call a%set(1969, 12, 31, 23, 59, 59, 999999999)
        call b%set(1970, 1, 1, 0, 0, 0)
        call check(error, a < b, "pre-epoch orders before the epoch")
        if (allocated(error)) return
        call a%set(1970, 1, 1, 0, 0, 0)
        call check(error, a == b .and. a >= b .and. a <= b, "equality at the epoch")
    end subroutine test_ts_operators
    !
    ! ------------------------------------------------------------------------------
    !
    subroutine test_elemental_arrays(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_date) :: dates(4)
        type(parquet_timestamp) :: ts(3)
        logical :: ok(3), mask(4)
        integer(int64) :: sec(3)
        integer(int32) :: nsec(3)
        ! whole-array setters/getters through the elemental interfaces
        call dates%set([2024_int32, 2025_int32, 2026_int32, 2027_int32], 7_int32, 16_int32)
        call check(error, all(dates(1:3) < dates(2:4)), "whole-array operator(<)")
        if (allocated(error)) return
        call check(error, .not. any(dates%is_null()), "whole-array is_null after set")
        if (allocated(error)) return
        call dates(2)%set_null()
        mask = dates%is_null()
        call check(error, count(mask) == 1 .and. mask(2), "is_null mask reflects one null")
        if (allocated(error)) return
        call check(error, all(dates%raw() == [19920_int32, 0_int32, 20650_int32, 21015_int32]), &
            "whole-array raw() with a placeholder 0 at the null slot")
        if (allocated(error)) return
        ! elemental set_unix/to_unix/get_raw over arrays
        call ts%set_unix([10_int64, 20_int64, 30_int64], parquet_unit_seconds)
        call check(error, all(ts%to_unix(parquet_unit_millis) == [10000_int64, 20000_int64, 30000_int64]), &
            "whole-array set_unix/to_unix")
        if (allocated(error)) return
        call ts%get_raw(sec, nsec)
        call check(error, all(sec == [10_int64, 20_int64, 30_int64]) .and. all(nsec == 0_int32), &
            "whole-array get_raw")
        if (allocated(error)) return
        ! elemental parse with an elemental success argument
        call ts%parse(["2024-07-16T00:00:01", "not-a-timestamp    ", "2024-07-16T00:00:03"], success=ok)
        call check(error, ok(1) .and. .not. ok(2) .and. ok(3), "elemental parse success flags")
        if (allocated(error)) return
        call check(error, .not. ts(1)%is_null() .and. ts(2)%is_null() .and. .not. ts(3)%is_null(), &
            "elemental parse null pattern")
    end subroutine test_elemental_arrays
    !
    !> Cross-validates this module's pure-Fortran civil<->days math against Arrow's vendored
    !> copy of the same (Hinnant date.h) algorithm, via two test-only parquet_debug_* hooks in
    !> src/parquet_wrapper.cpp. The sweep stays within years +-32767 -- the vendored library's
    !> `year` is a 16-bit type (this module itself goes far beyond; the algorithm is
    !> range-independent exact integer math, so agreement here validates it everywhere).
    subroutine test_arrow_cross_validation(error)
        type(error_type), allocatable, intent(out) :: error
        interface
            subroutine arrow_civil_from_days(days_in, year, month, day) &
                    bind(C, name="parquet_debug_civil_from_days")
                import :: c_int32_t, c_int64_t
                integer(c_int64_t), value :: days_in        !! days since 1970-01-01.
                integer(c_int32_t), intent(out) :: year     !! civil year (Arrow's answer).
                integer(c_int32_t), intent(out) :: month    !! civil month (Arrow's answer).
                integer(c_int32_t), intent(out) :: day      !! civil day (Arrow's answer).
            end subroutine arrow_civil_from_days
            subroutine arrow_days_from_civil(year, month, day, days_out) &
                    bind(C, name="parquet_debug_days_from_civil")
                import :: c_int32_t, c_int64_t
                integer(c_int32_t), value :: year           !! civil year.
                integer(c_int32_t), value :: month          !! civil month.
                integer(c_int32_t), value :: day            !! civil day.
                integer(c_int64_t), intent(out) :: days_out !! days since 1970-01-01 (Arrow's answer).
            end subroutine arrow_days_from_civil
        end interface
        type(parquet_date) :: d
        integer(int64) :: days
        integer(c_int64_t) :: adays
        integer(c_int32_t) :: ay, amo, add
        integer(int32) :: y, mo, dd, k
        character(len=96) :: msg
        !> Targeted civil dates: epoch, MJD epoch, leap days, century (non-)leap boundaries,
        !> the 4-digit year edges, and negative (proleptic) years including a BC leap day.
        integer(int32), parameter :: CIVIL(3, 14) = reshape([ &
            1970, 1, 1,    1858, 11, 17,  2000, 2, 29,   2024, 2, 29, &
            1900, 2, 28,   1900, 3, 1,    2100, 2, 28,   2100, 3, 1, &
            9999, 12, 31,  1, 1, 1,       -1, 12, 31,    -4, 2, 29, &
            32000, 6, 15,  -32000, 6, 15], [3, 14])
        ! broad two-way sweep across years ~ +-30000 (step is coprime to 7/400-year cycles)
        do days = -11000000_int64, 11000000_int64, 6007_int64
            call d%set_raw(int(days, int32))
            call d%get(y, mo, dd)
            call arrow_civil_from_days(int(days, c_int64_t), ay, amo, add)
            if (y /= ay .or. mo /= amo .or. dd /= add) then
                write(msg, '(a,i0,a,i0,"-",i0,"-",i0,a,i0,"-",i0,"-",i0)') &
                    "civil_from_days mismatch at day ", days, ": fortran ", y, mo, dd, " arrow ", ay, amo, add
                call check(error, .false., trim(msg))
                return
            end if
            call arrow_days_from_civil(ay, amo, add, adays)
            if (adays /= days) then
                write(msg, '(a,i0,a,i0)') "days_from_civil mismatch: expected ", days, " arrow ", adays
                call check(error, .false., trim(msg))
                return
            end if
        end do
        ! dense sweep around the epoch (every day of ~5.5 years on each side)
        do days = -2000_int64, 2000_int64
            call d%set_raw(int(days, int32))
            call d%get(y, mo, dd)
            call arrow_civil_from_days(int(days, c_int64_t), ay, amo, add)
            if (y /= ay .or. mo /= amo .or. dd /= add) then
                write(msg, '(a,i0)') "civil_from_days mismatch in the dense epoch sweep at day ", days
                call check(error, .false., trim(msg))
                return
            end if
        end do
        ! targeted civil -> days comparisons
        do k = 1, size(CIVIL, 2)
            call d%set(CIVIL(1, k), CIVIL(2, k), CIVIL(3, k))
            call arrow_days_from_civil(int(CIVIL(1, k), c_int32_t), int(CIVIL(2, k), c_int32_t), &
                int(CIVIL(3, k), c_int32_t), adays)
            if (int(d%raw(), int64) /= adays) then
                write(msg, '(a,i0,"-",i0,"-",i0,a,i0,a,i0)') "days_from_civil mismatch for ", &
                    CIVIL(1, k), CIVIL(2, k), CIVIL(3, k), ": fortran ", d%raw(), " arrow ", adays
                call check(error, .false., trim(msg))
                return
            end if
        end do
        call check(error, .true., "cross-validation completed")
    end subroutine test_arrow_cross_validation
    !
end module test_temporal
