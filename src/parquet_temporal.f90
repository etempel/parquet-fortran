!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Independent, self-contained module providing the element-level Parquet date/time value types.
!!
!! Provides three public types, each holding ONE element (not a whole column -- unlike
!! `parquet_string_column` in the `parquet_strings` module):
!!
!! * `parquet_date` -- a calendar date, stored as days since 1970-01-01 (identical to the
!!   physical value of Parquet's DATE / Arrow's date32 type).
!! * `parquet_time` -- a time of day, stored as nanoseconds since midnight (holds any Parquet
!!   TIME unit -- seconds/millis/micros/nanos -- exactly).
!! * `parquet_timestamp` -- an instant, stored losslessly as seconds since 1970-01-01T00:00:00
!!   plus a nanosecond-of-second part (holds any Parquet TIMESTAMP unit, including legacy
!!   INT96 files, exactly).
!!
!! All three are plain value types with an internal null state; a default-initialized element
!! is null ("no value yet"). Element access follows a two-tier rule: semantic accessors
!! (`get`, `year`, `to_string`, `to_mjd`, `to_unix`, comparison operators, ...) abort on a
!! null element (guard with `is_null`), interop accessors (`raw`/`set_raw`/`get_raw`) never
!! abort (they return 0 for a null element -- validity travels separately via `is_null`), and
!! type-to-type conversions (`get_date`/`get_time`, `set(date, time)`) propagate null instead
!! of aborting. Most procedures are elemental, so they apply directly to whole arrays
!! (`mask = ts%is_null()`, `call dates%set(years, months, days)`).
!!
!! Calendar math uses the proleptic Gregorian calendar (Howard Hinnant's exact-integer
!! `days_from_civil`/`civil_from_days` algorithms). Timezone interpretation is deliberately
!! out of scope: a `parquet_timestamp` always holds the stored epoch offset verbatim, and a
!! column's timezone metadata is a column-level property of the read/write layer, not of the
!! element. This module depends only on intrinsic modules (`iso_fortran_env`,
!! `ieee_arithmetic`); it has no dependency on any other module in this library (the
!! read/write integration layer depends on it, never the reverse).
!!
!! **Why the setters take `intent(inout)` rather than `intent(out)`, and the obligation that
!! creates.** A POLYMORPHIC `intent(out)` dummy is not free: the compiler default-initialises the
!! element through the runtime on entry, and a type-bound procedure's passed-object dummy has to be
!! polymorphic, so an elemental setter pays that per element. Measured on a 4M-row whole-column
!! read: the construction loop cost 12.3 ms with `intent(out)` and 3.4 ms with `intent(inout)`
!! (3.6x), taking the whole date-column read from 23.5 ms to 14.5 ms. The same change on the
!! timestamp setters took its loop from 17.2 ms to 5.7 ms.
!!
!! The cost of that is an obligation moved from the compiler to this source: `intent(out)` reset
!! EVERY component for free, whereas under `intent(inout)` a component a setter does not assign
!! keeps whatever the element held before. So **every `intent(inout)` setter here must assign every
!! component of its type**, and adding a component to one of these types means revisiting all of
!! them. `tools/check_source_conventions.py`'s `check_temporal_setters_assign_all` enforces exactly
!! that, deriving both the component list and the setter list from this file so a new component or
!! a new setter is covered without editing the check.
!!
!! The converse is equally load-bearing and is enforced too: a setter with a caught-failure path
!! that RETURNS without assigning `self` -- `%parse` with a `success` argument, and
!! `ts_set_date_time`'s null propagation -- must KEEP `intent(out)`, because that is precisely what
!! makes a failed or null-propagating call yield a null element rather than a stale one.
module parquet_temporal
    use, intrinsic :: iso_fortran_env, only : int32, int64, real64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    !
    implicit none
    private
    !
    public :: parquet_date
    public :: parquet_time
    public :: parquet_timestamp
    public :: parquet_unit_seconds
    public :: parquet_unit_millis
    public :: parquet_unit_micros
    public :: parquet_unit_nanos
    public :: parquet_ns_per_sec
    public :: parquet_ns_per_day
    public :: parquet_ns_to_sec
    public :: parquet_ns_to_day
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_temporal: "
    !
    !> Time-unit selectors for `set_unix`/`to_unix` (and reused by the read/write layer). Note:
    !! parquet_unit_seconds is meaningful only for set_unix/to_unix (Unix-time interop) -- it can
    !! never be a MAML-declared TIME/TIMESTAMP column's own stored unit, since Parquet's physical
    !! format has no seconds-resolution TIME/TIMESTAMP encoding at all (only MILLIS/MICROS/NANOS);
    !! a MAML `time[s]`/`timestamp[s]` token is rejected at parse time for exactly this reason.
    integer, parameter :: parquet_unit_seconds = 1 !! whole seconds since the epoch.
    integer, parameter :: parquet_unit_millis = 2  !! milliseconds since the epoch.
    integer, parameter :: parquet_unit_micros = 3  !! microseconds since the epoch.
    integer, parameter :: parquet_unit_nanos = 4   !! nanoseconds since the epoch.
    !
    !> Nanoseconds per second / seconds per day / nanoseconds per day.
    integer(int64), parameter :: NS_PER_SECOND = 1000000000_int64
    integer(int64), parameter :: SECONDS_PER_DAY = 86400_int64
    integer(int64), parameter :: NS_PER_DAY = SECONDS_PER_DAY*NS_PER_SECOND
    !
    !> Public unit-conversion convenience constants for the raw nanosecond values the
    !! difference/offset operators traffic in. The `_per_X` pair are exact integer(int64)
    !! divisors (for a whole-unit result, e.g. `ns_value/parquet_ns_per_day` for a whole day
    !! count); the `_to_X` pair are real(real64) multiplicative factors (for a fractional
    !! result, e.g. a fractional day count).
    integer(int64), parameter :: parquet_ns_per_sec = NS_PER_SECOND !! exact; ns in one second.
    integer(int64), parameter :: parquet_ns_per_day = NS_PER_DAY   !! exact; ns in one day.
    real(real64), parameter :: parquet_ns_to_sec = 1.0e-9_real64   !! convenience; ns -> fractional seconds.
    real(real64), parameter :: parquet_ns_to_day = 1.0_real64/86400.0e9_real64 !! convenience; ns -> fractional days.
    !
    !> Days from 1858-11-17 (MJD epoch) to 1970-01-01 (Unix epoch): MJD = unix days + 40587.
    integer(int64), parameter :: MJD_UNIX_EPOCH = 40587_int64
    !> Offset from Modified Julian Date to Julian Date: JD = MJD + 2400000.5.
    real(real64), parameter :: JD_MJD_OFFSET = 2400000.5_real64
    !
    !> The minimum int64 value, built with ibset to avoid the out-of-symmetric-range literal
    !! `-huge-1` (same idiom as the int8 case in `.claude/rules/build.md`'s "Hand-run compiles").
    !!
    !! **Never combine it with a runtime value in an expression -- not even through a local copy.**
    !! nagfor 7.2 mis-evaluates such an expression, silently: `v < INT64_MIN - delta` answers
    !! `.true.` for `v = 19920, delta = -4` (it must be `.false.`), and `INT64_MIN/scale` comes
    !! back with the WRONG SIGN, `+9223372036` instead of `-9223372036` for `scale = 1e9`. `mod()`
    !! divides the same way. Printing the expression shows the RIGHT value, so only a comparison
    !! or a stored result reveals it. Both failure directions have shipped here: a guard that fired
    !! on ordinary date arithmetic (`a + 4` aborting), and a guard that never fired, letting an
    !! out-of-range `to_unix` wrap silently instead of aborting.
    !!
    !! **Copying it into a local first does NOT work, and believing otherwise cost a second round
    !! of this bug.** The copy was measured correct -- in a probe that assigned it in a program
    !! body. In a module procedure, which is what this file actually contains, the optimiser
    !! propagates the constant straight back: correct at `-O0`, wrong at `-O2`, `-O3` and `-O4`, so
    !! `fpm test` passed and `fpm test --profile release` aborted on `date + 4`. **A workaround
    !! verified in a simplified reproducer is not verified**; reproduce it in the shape the code
    !! really has. Use `offset_floor` for the subtraction, and form any other bound from `huge`
    !! alone, as `ts_to_unix` does for the division.
    !!
    !! Safe, measured at `-O0`/`-O2`/`-O3`/`-O4`: equality (`n == INT64_MIN`), a bare constant
    !! comparison, plain assignment, a copy compared bare (`ts_diff_ns`'s `lo`), and an expression
    !! whose every operand is constant and therefore folded (`INT64_MIN + TS_DIFF_NS_BOUND_SECONDS`,
    !! also `ts_diff_ns`). gfortran, ifx and flang are unaffected throughout, so only a nagfor run
    !! -- and only an OPTIMISED one -- reports a regression here.
    integer(int64), parameter :: INT64_MIN = ibset(0_int64, 63)
    !
    !> `parquet_date%days` bounds (the int32 range), as int64 for overflow-safe checks.
    integer(int64), parameter :: DATE_DAYS_MAX = int(huge(0_int32), int64)
    integer(int64), parameter :: DATE_DAYS_MIN = -DATE_DAYS_MAX - 1_int64
    !> Year magnitude accepted by the string parsers, just above the largest year an int64
    !! timestamp can reach (+-292,277,026,596), so the era math inside days_from_civil stays
    !! within int64 while no representable value is rejected; the exact per-value range check
    !! happens against PARSE_DAYS_BOUND (timestamps) / DATE_DAYS_MAX (dates) afterwards.
    integer(int64), parameter :: PARSE_YEAR_BOUND = 292277030000_int64
    !> Day-count magnitude a parsed timestamp may reach: the largest |days| whose `days*86400`
    !! still fits int64 (floor(huge(int64)/86400)).
    integer(int64), parameter :: PARSE_DAYS_BOUND = 106751991167300_int64
    !> `abs(MJD)` bound for parquet_timestamp%set_mjd/set_jd, keeping `seconds` within int64
    !! (the exact limit is huge(int64)/86400 ~ 1.0675e14; slightly conservative on purpose).
    real(real64), parameter :: MJD_ABS_BOUND = 1.0e14_real64
    !> Elapsed-seconds magnitude bound for ts_diff_ns's result: huge(int64)/NS_PER_SECOND,
    !! minus 1 second of headroom so the largest nanosecond-of-second component (up to
    !! 999999999) can still be added/subtracted afterwards without itself overflowing int64.
    !! ~292.3 years (huge(int64)/1e9 seconds), matching pandas.Timestamp's own 1677-2262 range.
    integer(int64), parameter :: TS_DIFF_NS_BOUND_SECONDS = huge(0_int64)/NS_PER_SECOND - 1_int64
    !
    !> A calendar date (proleptic Gregorian), stored as days since 1970-01-01 -- identical to
    !! the physical value of a Parquet DATE / Arrow date32 column. A default-initialized
    !! element is null. Range: about +-5.8 million years.
    type :: parquet_date
        private
        integer(int32) :: days = 0_int32 !! days since 1970-01-01 (the Parquet DATE value).
        logical :: valid = .false.       !! .false. = null element (the default state).
    contains
        procedure :: set => date_set              !! Sets from a validated (year, month, day).
        procedure :: get => date_get              !! Returns year, month, day (aborts on null).
        procedure :: year => date_year            !! Calendar year (aborts on null).
        procedure :: month => date_month          !! Calendar month 1..12 (aborts on null).
        procedure :: day => date_day              !! Day of month 1..31 (aborts on null).
        procedure :: is_null => date_is_null      !! Whether the element is null (never aborts).
        procedure :: set_null => date_set_null    !! Marks the element null.
        procedure :: set_raw => date_set_raw      !! Sets the raw day count (interop; marks valid).
        procedure :: raw => date_raw              !! Raw day count; 0 for a null element (interop).
        procedure, private :: date_set_mjd_i32    !! int32 specific of set_mjd.
        procedure, private :: date_set_mjd_i64    !! int64 specific of set_mjd.
        generic :: set_mjd => date_set_mjd_i32, date_set_mjd_i64 !! Sets from an integer Modified Julian Date.
        procedure :: to_mjd => date_to_mjd        !! Integer Modified Julian Date (aborts on null).
        procedure :: to_string => date_to_string  !! ISO-8601 "YYYY-MM-DD" (aborts on null).
        procedure :: parse => date_parse          !! Sets from an ISO-8601 date string.
        procedure, private :: date_eq             !! == specific.
        procedure, private :: date_ne             !! /= specific.
        procedure, private :: date_lt             !! <  specific.
        procedure, private :: date_le             !! <= specific.
        procedure, private :: date_gt             !! >  specific.
        procedure, private :: date_ge             !! >= specific.
        generic :: operator(==) => date_eq        !! Equality (aborts on a null operand).
        generic :: operator(/=) => date_ne        !! Inequality (aborts on a null operand).
        generic :: operator(<) => date_lt         !! Ordering (aborts on a null operand).
        generic :: operator(<=) => date_le        !! Ordering (aborts on a null operand).
        generic :: operator(>) => date_gt         !! Ordering (aborts on a null operand).
        generic :: operator(>=) => date_ge        !! Ordering (aborts on a null operand).
        procedure, private :: date_diff           !! (date, date) specific of operator(-): whole-day difference.
        procedure, private :: date_sub_days_i32   !! (date, integer(int32)) specific of operator(-).
        procedure, private :: date_sub_days_i64   !! (date, integer(int64)) specific of operator(-).
        procedure, private :: date_add_days_i32   !! (date, integer(int32)) specific of operator(+).
        procedure, private :: date_add_days_i64   !! (date, integer(int64)) specific of operator(+).
        generic :: operator(-) => date_diff, date_sub_days_i32, date_sub_days_i64
        !! Difference or day offset (aborts on a null operand / out-of-range result).
        generic :: operator(+) => date_add_days_i32, date_add_days_i64
        !! Day offset (aborts on a null operand / out-of-range result).
    end type parquet_date
    !
    !> A time of day, stored as nanoseconds since midnight, [0, 86400e9 - 1]. Holds any
    !! Parquet TIME unit (seconds/millis/micros/nanos) exactly. A default-initialized element
    !! is null.
    type :: parquet_time
        private
        integer(int64) :: nanoseconds = 0_int64 !! nanoseconds since midnight, [0, 86400e9 - 1].
        logical :: valid = .false.              !! .false. = null element (the default state).
    contains
        procedure :: set => time_set              !! Sets from validated (hour, minute, second[, nanosecond]).
        procedure :: get => time_get              !! Returns hour, minute, second[, nanosecond] (aborts on null).
        procedure :: hour => time_hour            !! Hour 0..23 (aborts on null).
        procedure :: minute => time_minute        !! Minute 0..59 (aborts on null).
        procedure :: second => time_second        !! Second 0..59 (aborts on null).
        procedure :: nanosecond => time_nanosecond !! Sub-second part 0..999999999 (aborts on null).
        procedure :: is_null => time_is_null      !! Whether the element is null (never aborts).
        procedure :: set_null => time_set_null    !! Marks the element null.
        procedure :: set_raw => time_set_raw      !! Sets raw ns-since-midnight (interop; marks valid).
        procedure :: raw => time_raw              !! Raw ns-since-midnight; 0 for a null element (interop).
        procedure :: to_string => time_to_string  !! ISO-8601 "HH:MM:SS[.fraction]" (aborts on null).
        procedure :: parse => time_parse          !! Sets from an ISO-8601 time string.
        procedure, private :: time_eq             !! == specific.
        procedure, private :: time_ne             !! /= specific.
        procedure, private :: time_lt             !! <  specific.
        procedure, private :: time_le             !! <= specific.
        procedure, private :: time_gt             !! >  specific.
        procedure, private :: time_ge             !! >= specific.
        generic :: operator(==) => time_eq        !! Equality (aborts on a null operand).
        generic :: operator(/=) => time_ne        !! Inequality (aborts on a null operand).
        generic :: operator(<) => time_lt         !! Ordering (aborts on a null operand).
        generic :: operator(<=) => time_le        !! Ordering (aborts on a null operand).
        generic :: operator(>) => time_gt         !! Ordering (aborts on a null operand).
        generic :: operator(>=) => time_ge        !! Ordering (aborts on a null operand).
        procedure, private :: time_diff           !! (time, time) specific of operator(-): ns difference.
        procedure, private :: time_sub_ns_i32     !! (time, integer(int32)) specific of operator(-).
        procedure, private :: time_sub_ns_i64     !! (time, integer(int64)) specific of operator(-).
        procedure, private :: time_add_ns_i32     !! (time, integer(int32)) specific of operator(+).
        procedure, private :: time_add_ns_i64     !! (time, integer(int64)) specific of operator(+).
        generic :: operator(-) => time_diff, time_sub_ns_i32, time_sub_ns_i64
        !! Difference or ns offset (wraps; aborts on a null operand or a >24h offset magnitude).
        generic :: operator(+) => time_add_ns_i32, time_add_ns_i64
        !! Ns offset (wraps; aborts on a null operand or a >24h offset magnitude).
    end type parquet_time
    !
    !> An instant, stored losslessly as whole seconds since 1970-01-01T00:00:00 plus a
    !! normalized nanosecond-of-second part (always 0..999999999, also for pre-epoch
    !! instants). Holds any Parquet TIMESTAMP unit exactly over the full int64 range of the
    !! stored value. Timezone-agnostic: holds the stored epoch offset verbatim. A
    !! default-initialized element is null.
    type :: parquet_timestamp
        private
        integer(int64) :: seconds = 0_int64     !! whole seconds since 1970-01-01T00:00:00.
        integer(int32) :: nanoseconds = 0_int32 !! nanosecond-of-second part, always 0..999999999.
        logical :: valid = .false.              !! .false. = null element (the default state).
    contains
        procedure, private :: ts_set_civil        !! (year, month, day, hour, minute, second[, ns]) specific.
        procedure, private :: ts_set_date_time    !! (parquet_date, parquet_time) specific; null propagates.
        generic :: set => ts_set_civil, ts_set_date_time !! Sets from civil fields or a date + time pair.
        procedure :: get => ts_get                !! Returns all civil fields (aborts on null).
        procedure :: get_date => ts_get_date      !! The date part; null propagates (never aborts).
        procedure :: get_time => ts_get_time      !! The time-of-day part; null propagates (never aborts).
        procedure :: is_null => ts_is_null        !! Whether the element is null (never aborts).
        procedure :: set_null => ts_set_null      !! Marks the element null.
        procedure :: set_raw => ts_set_raw        !! Sets the raw (seconds, nanoseconds) pair (interop).
        procedure :: get_raw => ts_get_raw        !! Raw (seconds, nanoseconds); zeros when null (interop).
        procedure, private :: ts_set_unix_i32     !! int32 specific of set_unix.
        procedure, private :: ts_set_unix_i64     !! int64 specific of set_unix.
        generic :: set_unix => ts_set_unix_i32, ts_set_unix_i64 !! Sets from a Unix-time value in a given unit.
        procedure :: to_unix => ts_to_unix        !! Unix-time value in a given unit (aborts on null/loss).
        procedure :: set_mjd => ts_set_mjd        !! Sets from a real64 Modified Julian Date.
        procedure :: to_mjd => ts_to_mjd          !! real64 Modified Julian Date (aborts on null).
        procedure :: set_jd => ts_set_jd          !! Sets from a real64 Julian Date.
        procedure :: to_jd => ts_to_jd            !! real64 Julian Date (aborts on null).
        procedure :: to_string => ts_to_string    !! ISO-8601 "YYYY-MM-DDTHH:MM:SS[.fraction]" (aborts on null).
        procedure :: parse => ts_parse            !! Sets from an ISO-8601 date-time string.
        procedure, private :: ts_eq               !! == specific.
        procedure, private :: ts_ne               !! /= specific.
        procedure, private :: ts_lt               !! <  specific.
        procedure, private :: ts_le               !! <= specific.
        procedure, private :: ts_gt               !! >  specific.
        procedure, private :: ts_ge               !! >= specific.
        generic :: operator(==) => ts_eq          !! Equality (aborts on a null operand).
        generic :: operator(/=) => ts_ne          !! Inequality (aborts on a null operand).
        generic :: operator(<) => ts_lt           !! Ordering (aborts on a null operand).
        generic :: operator(<=) => ts_le          !! Ordering (aborts on a null operand).
        generic :: operator(>) => ts_gt           !! Ordering (aborts on a null operand).
        generic :: operator(>=) => ts_ge          !! Ordering (aborts on a null operand).
        procedure, private :: ts_diff_ns          !! (ts, ts) specific of operator(-): ns difference.
        procedure :: diff_seconds => ts_diff_seconds !! Real64-seconds difference (ts, ts); never aborts on magnitude.
        procedure, private :: ts_sub_ns_i32       !! (ts, integer(int32)) specific of operator(-).
        procedure, private :: ts_sub_ns_i64       !! (ts, integer(int64)) specific of operator(-).
        procedure, private :: ts_add_ns_i32       !! (ts, integer(int32)) specific of operator(+).
        procedure, private :: ts_add_ns_i64       !! (ts, integer(int64)) specific of operator(+).
        generic :: operator(-) => ts_diff_ns, ts_sub_ns_i32, ts_sub_ns_i64
        !! Difference or ns offset (aborts on a null operand or int64 overflow).
        generic :: operator(+) => ts_add_ns_i32, ts_add_ns_i64 !! Ns offset (aborts on a null operand or int64 overflow).
    end type parquet_timestamp
    !
    !> Constructs a valid parquet_date from (year, month, day); aborts on an invalid civil date
    !! (the structure constructor itself is unavailable outside this module -- components are
    !! private -- so this generic takes its place).
    interface parquet_date
        module procedure date_new !! elemental (year, month, day) constructor.
    end interface parquet_date
    !
    !> Constructs a valid parquet_time from (hour, minute, second[, nanosecond]); aborts on
    !! invalid fields.
    interface parquet_time
        module procedure time_new !! elemental (hour, minute, second[, nanosecond]) constructor.
    end interface parquet_time
    !
    !> Constructs a valid parquet_timestamp, either from full civil fields
    !! (year, month, day, hour, minute, second[, nanosecond]) -- aborting on invalid fields --
    !! or from a (parquet_date, parquet_time) pair, where a null input propagates to a null
    !! result.
    interface parquet_timestamp
        module procedure ts_new_civil     !! elemental civil-fields constructor.
        module procedure ts_new_date_time !! elemental (date, time) constructor; null propagates.
    end interface parquet_timestamp
    !
contains
    !
    ! ==================================================================================
    ! Internal helpers (private module procedures)
    ! ==================================================================================
    !
    !> Floor division (quotient rounded toward negative infinity; `b` must be positive).
    elemental integer(int64) function floor_div(a, b)
        integer(int64), intent(in) :: a !! dividend.
        integer(int64), intent(in) :: b !! divisor (positive).
        floor_div = a/b
        if (mod(a, b) /= 0_int64 .and. a < 0_int64) floor_div = floor_div - 1_int64
    end function floor_div
    !
    !> Returns whether year `y` is a leap year in the proleptic Gregorian calendar.
    elemental logical function is_leap_year(y)
        integer(int64), intent(in) :: y !! calendar year.
        is_leap_year = (mod(y, 4_int64) == 0_int64 .and. mod(y, 100_int64) /= 0_int64) &
            .or. mod(y, 400_int64) == 0_int64
    end function is_leap_year
    !
    !> Returns the number of days in month `m` of year `y` (proleptic Gregorian).
    elemental integer function days_in_month(y, m)
        integer(int64), intent(in) :: y !! calendar year.
        integer, intent(in) :: m        !! month, 1..12.
        integer, parameter :: DIM(12) = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        days_in_month = DIM(m)
        if (m == 2 .and. is_leap_year(y)) days_in_month = 29
    end function days_in_month
    !
    !> Returns days since 1970-01-01 for civil date (y, m, d) -- Howard Hinnant's
    !! days_from_civil, proleptic Gregorian, exact integer math. `y` must be within
    !! PARSE_YEAR_BOUND (callers guarantee this) so the internal era math cannot overflow.
    elemental integer(int64) function days_from_civil(y, m, d)
        integer(int64), intent(in) :: y !! calendar year.
        integer, intent(in) :: m        !! month, 1..12.
        integer, intent(in) :: d        !! day of month, 1..31.
        integer(int64) :: yy, era, yoe, doy, doe
        yy = y
        if (m <= 2) yy = yy - 1_int64
        era = floor_div(yy, 400_int64)
        yoe = yy - era*400_int64
        doy = (153_int64*int(merge(m - 3, m + 9, m > 2), int64) + 2_int64)/5_int64 + int(d - 1, int64)
        doe = yoe*365_int64 + yoe/4_int64 - yoe/100_int64 + doy
        days_from_civil = era*146097_int64 + doe - 719468_int64
    end function days_from_civil
    !
    !> Returns the civil date (y, m, d) for days-since-1970-01-01 `z_in` -- Howard Hinnant's
    !! civil_from_days, the exact inverse of days_from_civil over the full int64 day range
    !! used here.
    pure subroutine civil_from_days(z_in, y, m, d)
        integer(int64), intent(in) :: z_in !! days since 1970-01-01.
        integer(int64), intent(out) :: y   !! calendar year.
        integer, intent(out) :: m          !! month, 1..12.
        integer, intent(out) :: d          !! day of month, 1..31.
        integer(int64) :: z, era, doe, yoe, doy, mp
        z = z_in + 719468_int64
        era = floor_div(z, 146097_int64)
        doe = z - era*146097_int64
        yoe = (doe - doe/1460_int64 + doe/36524_int64 - doe/146096_int64)/365_int64
        y = yoe + era*400_int64
        doy = doe - (365_int64*yoe + yoe/4_int64 - yoe/100_int64)
        mp = (5_int64*doy + 2_int64)/153_int64
        d = int(doy - (153_int64*mp + 2_int64)/5_int64 + 1_int64)
        m = int(mp + merge(3_int64, -9_int64, mp < 10_int64))
        if (m <= 2) y = y + 1_int64
    end subroutine civil_from_days
    !
    !> The smallest int64 that can have `delta` added to it without underflowing, for a `delta`
    !! that is zero or negative: mathematically `INT64_MIN - delta`, and the ONLY way this module
    !! is allowed to compute it.
    !!
    !! It is formed from `huge` alone, and that is the entire point -- **an expression naming the
    !! most-negative constant is miscompiled by nagfor at `-O2` and above** (see INT64_MIN's own
    !! declaration for the measurements). Copying the constant into a local first does NOT help:
    !! the optimiser propagates it straight back, which is how a guard that had been "fixed" that
    !! way still aborted on `date + 4` under `--profile release`.
    !!
    !! Nothing here can overflow. `delta <= 0` makes `delta + 1 <= 1`, so `huge + (delta + 1)` is
    !! at most `huge`; and `delta >= INT64_MIN` makes it at least `huge + INT64_MIN + 1 == 0`. The
    !! result is therefore in `[INT64_MIN + 1, 0]` and always representable. Check the two ends by
    !! hand when reading this: `delta = -1` gives `-huge`, and `delta = INT64_MIN` gives `0`, which
    !! are exactly `INT64_MIN + 1` and `INT64_MIN - INT64_MIN`.
    elemental integer(int64) function offset_floor(delta)
        integer(int64), intent(in) :: delta !! the offset about to be added; must be <= 0.
        offset_floor = -(huge(0_int64) + (delta + 1_int64))
    end function offset_floor
    !
    !> Returns the per-second count of `unit` (1 for seconds ... 1e9 for nanos); aborts on an
    !! unrecognized unit selector.
    elemental integer(int64) function unit_scale(unit)
        integer, intent(in) :: unit !! one of the parquet_unit_* constants.
        select case (unit)
        case (parquet_unit_seconds)
            unit_scale = 1_int64
        case (parquet_unit_millis)
            unit_scale = 1000_int64
        case (parquet_unit_micros)
            unit_scale = 1000000_int64
        case (parquet_unit_nanos)
            unit_scale = NS_PER_SECOND
        case default
            error stop EP//"invalid time unit (use parquet_unit_seconds/millis/micros/nanos)"
        end select
    end function unit_scale
    !
    !> Parses a fixed run of decimal digits into an int64; `ok` is .false. for an empty run,
    !! any non-digit character, or a run too long for int64 (> 18 digits).
    pure subroutine str_to_int(str, val, ok)
        character(len=*), intent(in) :: str !! the digit run (no sign, no blanks).
        integer(int64), intent(out) :: val  !! parsed value.
        logical, intent(out) :: ok          !! .true. when the run is a valid number.
        integer :: i
        val = 0_int64
        ok = len(str) > 0 .and. len(str) <= 18
        if (.not. ok) return
        do i = 1, len(str)
            if (str(i:i) < '0' .or. str(i:i) > '9') then
                ok = .false.
                return
            end if
            val = val*10_int64 + int(iachar(str(i:i)) - 48, int64)
        end do
    end subroutine str_to_int
    !
    !> Parses an ISO-8601 date "[-]YYYY-MM-DD" (year >= 4 digits, month/day exactly 2) between
    !! bounds of `str` and validates it as a civil date; `ok` reports success. `y` is bounded
    !! by PARSE_YEAR_BOUND so downstream day math cannot overflow.
    pure subroutine parse_date_fields(str, y, m, d, ok)
        character(len=*), intent(in) :: str !! candidate date string (surrounding blanks allowed).
        integer(int64), intent(out) :: y    !! parsed calendar year.
        integer, intent(out) :: m           !! parsed month.
        integer, intent(out) :: d           !! parsed day of month.
        logical, intent(out) :: ok          !! .true. when str is a valid civil date.
        integer :: a, b
        integer(int64) :: tmp
        logical :: neg, fok
        y = 0_int64
        m = 0
        d = 0
        ok = .false.
        b = len_trim(str)
        a = 1
        do while (a <= b)
            if (str(a:a) /= ' ') exit
            a = a + 1
        end do
        neg = .false.
        if (a <= b) then
            if (str(a:a) == '-') then
                neg = .true.
                a = a + 1
            end if
        end if
        ! fixed layout from the right: <year>-MM-DD with a >= 4-digit year field
        if (b - a + 1 < 10) return
        if (str(b-2:b-2) /= '-' .or. str(b-5:b-5) /= '-') return
        call str_to_int(str(a:b-6), tmp, fok)
        if (.not. fok) return
        if (tmp > PARSE_YEAR_BOUND) return
        y = merge(-tmp, tmp, neg)
        call str_to_int(str(b-4:b-3), tmp, fok)
        if (.not. fok) return
        m = int(tmp)
        call str_to_int(str(b-1:b), tmp, fok)
        if (.not. fok) return
        d = int(tmp)
        if (m < 1 .or. m > 12) return
        if (d < 1 .or. d > days_in_month(y, m)) return
        ok = .true.
    end subroutine parse_date_fields
    !
    !> Parses an ISO-8601 time "HH:MM:SS[.fraction]" (two-digit fields; 1..9 fraction digits)
    !! between bounds of `str`, validating field ranges; `ok` reports success. The fraction is
    !! right-padded to nanoseconds.
    pure subroutine parse_time_fields(str, h, mi, s, ns, ok)
        character(len=*), intent(in) :: str !! candidate time string (surrounding blanks allowed).
        integer, intent(out) :: h           !! parsed hour.
        integer, intent(out) :: mi          !! parsed minute.
        integer, intent(out) :: s           !! parsed second.
        integer(int64), intent(out) :: ns   !! parsed sub-second part in nanoseconds.
        logical, intent(out) :: ok          !! .true. when str is a valid time of day.
        integer :: a, b, k, nfrac
        integer(int64) :: tmp
        logical :: fok
        h = 0
        mi = 0
        s = 0
        ns = 0_int64
        ok = .false.
        b = len_trim(str)
        a = 1
        do while (a <= b)
            if (str(a:a) /= ' ') exit
            a = a + 1
        end do
        if (b - a + 1 < 8) return
        if (str(a+2:a+2) /= ':' .or. str(a+5:a+5) /= ':') return
        call str_to_int(str(a:a+1), tmp, fok)
        if (.not. fok) return
        h = int(tmp)
        call str_to_int(str(a+3:a+4), tmp, fok)
        if (.not. fok) return
        mi = int(tmp)
        call str_to_int(str(a+6:a+7), tmp, fok)
        if (.not. fok) return
        s = int(tmp)
        if (b >= a + 8) then
            if (str(a+8:a+8) /= '.') return
            nfrac = b - (a + 9) + 1
            if (nfrac < 1 .or. nfrac > 9) return
            call str_to_int(str(a+9:b), tmp, fok)
            if (.not. fok) return
            do k = nfrac + 1, 9
                tmp = tmp*10_int64
            end do
            ns = tmp
        end if
        if (h < 0 .or. h > 23) return
        if (mi < 0 .or. mi > 59) return
        if (s < 0 .or. s > 59) return
        ok = .true.
    end subroutine parse_time_fields
    !
    !> Writes the ISO fraction suffix for sub-second part `ns` into `buf(pos+1:)` -- nothing
    !! for 0, else '.' plus the shortest of 3/6/9 digits that represents `ns` exactly --
    !! advancing `pos` past what was written.
    pure subroutine format_fraction(ns, buf, pos)
        integer(int64), intent(in) :: ns             !! sub-second part, 0..999999999.
        character(len=*), intent(inout) :: buf       !! output buffer.
        integer, intent(inout) :: pos                !! last used position in buf; updated.
        integer(int64) :: v
        integer :: ndig, k
        if (ns == 0_int64) return
        if (mod(ns, 1000000_int64) == 0_int64) then
            ndig = 3
            v = ns/1000000_int64
        else if (mod(ns, 1000_int64) == 0_int64) then
            ndig = 6
            v = ns/1000_int64
        else
            ndig = 9
            v = ns
        end if
        buf(pos+1:pos+1) = '.'
        do k = ndig, 1, -1
            buf(pos+1+k:pos+1+k) = achar(48 + int(mod(v, 10_int64)))
            v = v/10_int64
        end do
        pos = pos + 1 + ndig
    end subroutine format_fraction
    !
    !> Writes the ISO date "[-]YYYY-MM-DD" for (y, m, d) into `buf`, 4-digit-padding years
    !! 0..9999 and using as many digits as needed otherwise; `pos` returns the used length.
    pure subroutine format_date_fields(y, m, d, buf, pos)
        integer(int64), intent(in) :: y        !! calendar year.
        integer, intent(in) :: m               !! month.
        integer, intent(in) :: d               !! day of month.
        character(len=*), intent(inout) :: buf !! output buffer.
        integer, intent(out) :: pos            !! last used position in buf.
        character(len=16) :: digits
        integer(int64) :: v
        integer :: nd, k
        pos = 0
        v = y
        if (v < 0_int64) then
            buf(1:1) = '-'
            pos = 1
            v = -v
        end if
        nd = 0
        do
            nd = nd + 1
            digits(nd:nd) = achar(48 + int(mod(v, 10_int64)))
            v = v/10_int64
            if (v == 0_int64) exit
        end do
        do k = nd + 1, 4 ! ISO-style zero padding to at least 4 year digits
            digits(k:k) = '0'
        end do
        nd = max(nd, 4)
        do k = nd, 1, -1
            pos = pos + 1
            buf(pos:pos) = digits(k:k)
        end do
        buf(pos+1:pos+1) = '-'
        buf(pos+2:pos+2) = achar(48 + m/10)
        buf(pos+3:pos+3) = achar(48 + mod(m, 10))
        buf(pos+4:pos+4) = '-'
        buf(pos+5:pos+5) = achar(48 + d/10)
        buf(pos+6:pos+6) = achar(48 + mod(d, 10))
        pos = pos + 6
    end subroutine format_date_fields
    !
    !> Writes the ISO time "HH:MM:SS" for a nanoseconds-of-day value into `buf(pos+1:)`
    !! (without fraction -- see format_fraction), advancing `pos`.
    pure subroutine format_time_fields(nsod, buf, pos)
        integer(int64), intent(in) :: nsod     !! nanoseconds since midnight.
        character(len=*), intent(inout) :: buf !! output buffer.
        integer, intent(inout) :: pos          !! last used position in buf; updated.
        integer :: h, mi, s
        integer(int64) :: sod
        sod = nsod/NS_PER_SECOND
        h = int(sod/3600_int64)
        mi = int(mod(sod/60_int64, 60_int64))
        s = int(mod(sod, 60_int64))
        buf(pos+1:pos+1) = achar(48 + h/10)
        buf(pos+2:pos+2) = achar(48 + mod(h, 10))
        buf(pos+3:pos+3) = ':'
        buf(pos+4:pos+4) = achar(48 + mi/10)
        buf(pos+5:pos+5) = achar(48 + mod(mi, 10))
        buf(pos+6:pos+6) = ':'
        buf(pos+7:pos+7) = achar(48 + s/10)
        buf(pos+8:pos+8) = achar(48 + mod(s, 10))
        pos = pos + 8
    end subroutine format_time_fields
    !
    ! ==================================================================================
    ! parquet_date
    ! ==================================================================================
    !
    !> Sets the element from civil fields (proleptic Gregorian), validating month, day, and the
    !! representable range; marks it valid.
    impure elemental subroutine date_set(self, year, month, day)
        class(parquet_date), intent(inout) :: self !! receives the date (marked valid).
        integer(int32), intent(in) :: year       !! calendar year.
        integer(int32), intent(in) :: month      !! month, 1..12.
        integer(int32), intent(in) :: day        !! day of month, 1..days_in_month.
        integer(int64) :: d64
        if (month < 1 .or. month > 12) then
            error stop EP//"invalid month in parquet_date%set (must be 1..12)"
        end if
        if (day < 1 .or. day > days_in_month(int(year, int64), int(month))) then
            error stop EP//"invalid day of month in parquet_date%set"
        end if
        d64 = days_from_civil(int(year, int64), int(month), int(day))
        if (d64 > DATE_DAYS_MAX .or. d64 < DATE_DAYS_MIN) then
            error stop EP//"date out of range in parquet_date%set (beyond +-5.8 million years)"
        end if
        self%days = int(d64, int32)
        self%valid = .true.
    end subroutine date_set
    !
    !> Returns the civil fields of the element; aborts on a null element.
    elemental subroutine date_get(self, year, month, day)
        class(parquet_date), intent(in) :: self !! the element.
        integer(int32), intent(out) :: year     !! calendar year.
        integer(int32), intent(out) :: month    !! month, 1..12.
        integer(int32), intent(out) :: day      !! day of month.
        integer(int64) :: y
        integer :: m, d
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in get (guard with is_null)"
        end if
        call civil_from_days(int(self%days, int64), y, m, d)
        year = int(y, int32)
        month = int(m, int32)
        day = int(d, int32)
    end subroutine date_get
    !
    !> Returns the calendar year; aborts on a null element.
    elemental integer(int32) function date_year(self) result(res)
        class(parquet_date), intent(in) :: self !! the element.
        integer(int64) :: y
        integer :: m, d
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in year (guard with is_null)"
        end if
        call civil_from_days(int(self%days, int64), y, m, d)
        res = int(y, int32)
    end function date_year
    !
    !> Returns the calendar month (1..12); aborts on a null element.
    elemental integer(int32) function date_month(self) result(res)
        class(parquet_date), intent(in) :: self !! the element.
        integer(int64) :: y
        integer :: m, d
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in month (guard with is_null)"
        end if
        call civil_from_days(int(self%days, int64), y, m, d)
        res = int(m, int32)
    end function date_month
    !
    !> Returns the day of month (1..31); aborts on a null element.
    elemental integer(int32) function date_day(self) result(res)
        class(parquet_date), intent(in) :: self !! the element.
        integer(int64) :: y
        integer :: m, d
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in day (guard with is_null)"
        end if
        call civil_from_days(int(self%days, int64), y, m, d)
        res = int(d, int32)
    end function date_day
    !
    !> Returns whether the element is null (the primary null guard; never aborts).
    elemental logical function date_is_null(self) result(res)
        class(parquet_date), intent(in) :: self !! the element.
        res = .not. self%valid
    end function date_is_null
    !
    !> Marks the element null.
    impure elemental subroutine date_set_null(self)
        class(parquet_date), intent(inout) :: self !! the element (reset to the null state).
        self%days = 0_int32
        self%valid = .false.
    end subroutine date_set_null
    !
    !> Sets the raw day count directly (interop/advanced accessor; marks the element valid).
    impure elemental subroutine date_set_raw(self, days)
        class(parquet_date), intent(inout) :: self !! receives the value (marked valid).
        integer(int32), intent(in) :: days       !! days since 1970-01-01 (the Parquet DATE value).
        self%days = days
        self%valid = .true.
    end subroutine date_set_raw
    !
    !> Returns the raw day count, or 0 for a null element (interop/advanced accessor; never
    !! aborts -- validity travels separately via is_null).
    elemental integer(int32) function date_raw(self) result(res)
        class(parquet_date), intent(in) :: self !! the element.
        res = 0_int32
        if (self%valid) res = self%days
    end function date_raw
    !
    !> int32 specific of set_mjd; see the set_mjd generic.
    impure elemental subroutine date_set_mjd_i32(self, mjd)
        class(parquet_date), intent(inout) :: self !! receives the date (marked valid).
        integer(int32), intent(in) :: mjd        !! integer Modified Julian Date.
        call date_set_mjd_i64(self, int(mjd, int64))
    end subroutine date_set_mjd_i32
    !
    !> int64 specific of set_mjd: sets the element from an integer Modified Julian Date
    !! (MJD 0 = 1858-11-17); aborts if out of the representable range.
    impure elemental subroutine date_set_mjd_i64(self, mjd)
        class(parquet_date), intent(inout) :: self !! receives the date (marked valid).
        integer(int64), intent(in) :: mjd        !! integer Modified Julian Date.
        integer(int64) :: d64
        d64 = mjd - MJD_UNIX_EPOCH
        if (d64 > DATE_DAYS_MAX .or. d64 < DATE_DAYS_MIN) then
            error stop EP//"MJD out of range in parquet_date%set_mjd"
        end if
        self%days = int(d64, int32)
        self%valid = .true.
    end subroutine date_set_mjd_i64
    !
    !> Returns the integer Modified Julian Date (MJD 0 = 1858-11-17); aborts on a null element.
    elemental integer(int64) function date_to_mjd(self) result(res)
        class(parquet_date), intent(in) :: self !! the element.
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in to_mjd (guard with is_null)"
        end if
        res = int(self%days, int64) + MJD_UNIX_EPOCH
    end function date_to_mjd
    !
    !> Writes the element as ISO-8601 "YYYY-MM-DD" (years 0..9999 zero-padded to 4 digits;
    !! a leading '-' and more digits otherwise); aborts on a null element. A subroutine (not a
    !! function) so this never returns `character(len=:), allocatable` as a function result --
    !! see "Build and compiler notes" in CLAUDE.md for why.
    subroutine date_to_string(self, str)
        class(parquet_date), intent(in) :: self           !! the element.
        character(len=:), allocatable, intent(out) :: str !! receives the formatted date.
        character(len=32) :: buf
        integer(int64) :: y
        integer :: m, d, pos
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in to_string (guard with is_null)"
        end if
        call civil_from_days(int(self%days, int64), y, m, d)
        call format_date_fields(y, m, d, buf, pos)
        str = buf(1:pos)
    end subroutine date_to_string
    !
    !> Sets the element from an ISO-8601 date string "[-]YYYY-MM-DD". On failure (not parseable
    !! as a valid civil date): error stop by default; if `success` is present, no abort happens
    !! -- `success` is set .false. and the element is left null (a defined state) instead.
    !! (This procedure's header line -- and five others in this file: date_new, time_parse,
    !! time_new, ts_parse, ts_new_civil -- never register as "hit" in gcov, even though every
    !! other line of each one's body does, including their own `error stop` lines, which only
    !! execute on the actual abort path this file's error-scenario tests specifically trigger.
    !! That proves each procedure genuinely runs; the header line itself just isn't attributed
    !! a hit by gcov for these six specifically (no common dummy-argument shape distinguishes
    !! them from this file's other, correctly-attributed procedure headers -- e.g. ts_set_civil,
    !! same `impure elemental` + optional-argument shape, IS attributed normally). Treated as
    !! the same class of line-attribution artifact as the documented end module/end submodule
    !! exclusion in CLAUDE.md, not a real gap.)
    impure elemental subroutine date_parse(self, str, success) ! GCOVR_EXCL_LINE
        class(parquet_date), intent(out) :: self  !! receives the parsed date (or null on caught failure).
        character(len=*), intent(in) :: str       !! ISO-8601 date string.
        logical, intent(out), optional :: success !! .true. on success; absent => abort on failure.
        integer(int64) :: y, d64
        integer :: m, d
        logical :: ok
        call parse_date_fields(str, y, m, d, ok)
        if (ok) then
            d64 = days_from_civil(y, m, d)
            ok = d64 <= DATE_DAYS_MAX .and. d64 >= DATE_DAYS_MIN
        end if
        if (.not. ok) then
            if (present(success)) then
                success = .false.
                return
            end if
            error stop EP//"not a valid ISO-8601 date (expected YYYY-MM-DD) in parquet_date%parse"
        end if
        self%days = int(d64, int32)
        self%valid = .true.
        if (present(success)) success = .true.
    end subroutine date_parse
    !
    !> Equality specific; see the operator(==) generic. Aborts on a null operand.
    elemental logical function date_eq(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left operand.
        class(parquet_date), intent(in) :: b !! right operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"comparison with a null parquet_date element (guard with is_null)"
        end if
        res = a%days == b%days
    end function date_eq
    !
    !> Inequality specific; see the operator(/=) generic. Aborts on a null operand.
    elemental logical function date_ne(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left operand.
        class(parquet_date), intent(in) :: b !! right operand.
        res = .not. date_eq(a, b)
    end function date_ne
    !
    !> Less-than specific; see the operator(<) generic. Aborts on a null operand.
    elemental logical function date_lt(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left operand.
        class(parquet_date), intent(in) :: b !! right operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"comparison with a null parquet_date element (guard with is_null)"
        end if
        res = a%days < b%days
    end function date_lt
    !
    !> Less-or-equal specific; see the operator(<=) generic. Aborts on a null operand.
    elemental logical function date_le(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left operand.
        class(parquet_date), intent(in) :: b !! right operand.
        res = .not. date_lt(b, a)
    end function date_le
    !
    !> Greater-than specific; see the operator(>) generic. Aborts on a null operand.
    elemental logical function date_gt(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left operand.
        class(parquet_date), intent(in) :: b !! right operand.
        res = date_lt(b, a)
    end function date_gt
    !
    !> Greater-or-equal specific; see the operator(>=) generic. Aborts on a null operand.
    elemental logical function date_ge(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left operand.
        class(parquet_date), intent(in) :: b !! right operand.
        res = .not. date_lt(a, b)
    end function date_ge
    !
    !> Constructor specific for the parquet_date generic: builds a valid date element from
    !! (year, month, day); aborts on an invalid civil date.
    impure elemental function date_new(year, month, day) result(res) ! GCOVR_EXCL_LINE
        integer(int32), intent(in) :: year  !! calendar year.
        integer(int32), intent(in) :: month !! month, 1..12.
        integer(int32), intent(in) :: day   !! day of month.
        type(parquet_date) :: res           !! the constructed element (valid).
        call date_set(res, year, month, day)
    end function date_new
    !
    !> Difference specific; see the operator(-) generic. Whole days, exact; aborts on a null
    !! operand.
    elemental integer(int64) function date_diff(a, b) result(res)
        class(parquet_date), intent(in) :: a !! left (later) operand.
        class(parquet_date), intent(in) :: b !! right (earlier) operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"difference with a null parquet_date element (guard with is_null)"
        end if
        res = int(a%days, int64) - int(b%days, int64)
    end function date_diff
    !
    !> Shared worker for the day-offset operator(+)/operator(-) specifics: shifts self by
    !! `delta` whole days, aborting on a null operand or an out-of-range result (reusing
    !! date_set's own +-5.8 million year bound).
    impure elemental function date_offset_days_impl(self, delta) result(res)
        class(parquet_date), intent(in) :: self !! the element to shift.
        integer(int64), intent(in) :: delta     !! signed day offset (+ forward, - backward).
        type(parquet_date) :: res               !! self shifted by delta days.
        integer(int64) :: new_days
        if (.not. self%valid) then
            error stop EP//"null parquet_date element accessed in operator(+)/operator(-) (guard with is_null)"
        end if
        if (delta >= 0_int64) then
            if (int(self%days, int64) > huge(0_int64) - delta) then
                error stop EP//"date out of range in parquet_date operator(+)/operator(-) (beyond +-5.8 million years)"
            end if
        else
            if (int(self%days, int64) < offset_floor(delta)) then
                error stop EP//"date out of range in parquet_date operator(+)/operator(-) (beyond +-5.8 million years)"
            end if
        end if
        new_days = int(self%days, int64) + delta
        if (new_days > DATE_DAYS_MAX .or. new_days < DATE_DAYS_MIN) then
            error stop EP//"date out of range in parquet_date operator(+)/operator(-) (beyond +-5.8 million years)"
        end if
        res%days = int(new_days, int32)
        res%valid = .true.
    end function date_offset_days_impl
    !
    !> int32 specific of operator(-); see date_offset_days_impl.
    impure elemental function date_sub_days_i32(self, n) result(res)
        class(parquet_date), intent(in) :: self !! the element to shift.
        integer(int32), intent(in) :: n         !! days to subtract.
        type(parquet_date) :: res               !! self shifted back by n days.
        res = date_offset_days_impl(self, -int(n, int64))
    end function date_sub_days_i32
    !
    !> int64 specific of operator(-); see date_offset_days_impl.
    impure elemental function date_sub_days_i64(self, n) result(res)
        class(parquet_date), intent(in) :: self !! the element to shift.
        integer(int64), intent(in) :: n         !! days to subtract.
        type(parquet_date) :: res               !! self shifted back by n days.
        if (n == INT64_MIN) then
            error stop EP//"date out of range in parquet_date operator(-) (beyond +-5.8 million years)"
        end if
        res = date_offset_days_impl(self, -n)
    end function date_sub_days_i64
    !
    !> int32 specific of operator(+); see date_offset_days_impl.
    impure elemental function date_add_days_i32(self, n) result(res)
        class(parquet_date), intent(in) :: self !! the element to shift.
        integer(int32), intent(in) :: n         !! days to add.
        type(parquet_date) :: res               !! self shifted forward by n days.
        res = date_offset_days_impl(self, int(n, int64))
    end function date_add_days_i32
    !
    !> int64 specific of operator(+); see date_offset_days_impl.
    impure elemental function date_add_days_i64(self, n) result(res)
        class(parquet_date), intent(in) :: self !! the element to shift.
        integer(int64), intent(in) :: n         !! days to add.
        type(parquet_date) :: res               !! self shifted forward by n days.
        res = date_offset_days_impl(self, n)
    end function date_add_days_i64
    !
    ! ==================================================================================
    ! parquet_time
    ! ==================================================================================
    !
    !> Sets the element from validated time-of-day fields; marks it valid. Leap seconds
    !! (second == 60) are not representable (Parquet/Arrow TIME does not support them).
    impure elemental subroutine time_set(self, hour, minute, second, nanosecond)
        class(parquet_time), intent(inout) :: self         !! receives the time (marked valid).
        integer(int32), intent(in) :: hour               !! hour, 0..23.
        integer(int32), intent(in) :: minute             !! minute, 0..59.
        integer(int32), intent(in) :: second             !! second, 0..59.
        integer(int32), intent(in), optional :: nanosecond !! sub-second part, 0..999999999 (default 0).
        integer(int64) :: ns
        if (hour < 0 .or. hour > 23) error stop EP//"invalid hour in parquet_time%set (must be 0..23)"
        if (minute < 0 .or. minute > 59) error stop EP//"invalid minute in parquet_time%set (must be 0..59)"
        if (second < 0 .or. second > 59) error stop EP//"invalid second in parquet_time%set (must be 0..59)"
        ns = 0_int64
        if (present(nanosecond)) then
            if (nanosecond < 0 .or. int(nanosecond, int64) >= NS_PER_SECOND) then
                error stop EP//"invalid nanosecond in parquet_time%set (must be 0..999999999)"
            end if
            ns = int(nanosecond, int64)
        end if
        self%nanoseconds = (int(hour, int64)*3600_int64 + int(minute, int64)*60_int64 &
            + int(second, int64))*NS_PER_SECOND + ns
        self%valid = .true.
    end subroutine time_set
    !
    !> Returns the time-of-day fields of the element; aborts on a null element.
    elemental subroutine time_get(self, hour, minute, second, nanosecond)
        class(parquet_time), intent(in) :: self           !! the element.
        integer(int32), intent(out) :: hour               !! hour, 0..23.
        integer(int32), intent(out) :: minute             !! minute, 0..59.
        integer(int32), intent(out) :: second             !! second, 0..59.
        integer(int32), intent(out), optional :: nanosecond !! sub-second part, 0..999999999.
        integer(int64) :: sod
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in get (guard with is_null)"
        end if
        sod = self%nanoseconds/NS_PER_SECOND
        hour = int(sod/3600_int64, int32)
        minute = int(mod(sod/60_int64, 60_int64), int32)
        second = int(mod(sod, 60_int64), int32)
        if (present(nanosecond)) nanosecond = int(mod(self%nanoseconds, NS_PER_SECOND), int32)
    end subroutine time_get
    !
    !> Returns the hour (0..23); aborts on a null element.
    elemental integer(int32) function time_hour(self) result(res)
        class(parquet_time), intent(in) :: self !! the element.
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in hour (guard with is_null)"
        end if
        res = int(self%nanoseconds/(3600_int64*NS_PER_SECOND), int32)
    end function time_hour
    !
    !> Returns the minute (0..59); aborts on a null element.
    elemental integer(int32) function time_minute(self) result(res)
        class(parquet_time), intent(in) :: self !! the element.
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in minute (guard with is_null)"
        end if
        res = int(mod(self%nanoseconds/(60_int64*NS_PER_SECOND), 60_int64), int32)
    end function time_minute
    !
    !> Returns the second (0..59); aborts on a null element.
    elemental integer(int32) function time_second(self) result(res)
        class(parquet_time), intent(in) :: self !! the element.
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in second (guard with is_null)"
        end if
        res = int(mod(self%nanoseconds/NS_PER_SECOND, 60_int64), int32)
    end function time_second
    !
    !> Returns the sub-second part in nanoseconds (0..999999999); aborts on a null element.
    elemental integer(int32) function time_nanosecond(self) result(res)
        class(parquet_time), intent(in) :: self !! the element.
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in nanosecond (guard with is_null)"
        end if
        res = int(mod(self%nanoseconds, NS_PER_SECOND), int32)
    end function time_nanosecond
    !
    !> Returns whether the element is null (the primary null guard; never aborts).
    elemental logical function time_is_null(self) result(res)
        class(parquet_time), intent(in) :: self !! the element.
        res = .not. self%valid
    end function time_is_null
    !
    !> Marks the element null.
    impure elemental subroutine time_set_null(self)
        class(parquet_time), intent(inout) :: self !! the element (reset to the null state).
        self%nanoseconds = 0_int64
        self%valid = .false.
    end subroutine time_set_null
    !
    !> Sets the raw nanoseconds-since-midnight directly (interop/advanced accessor; marks the
    !! element valid); aborts on a value outside a day.
    impure elemental subroutine time_set_raw(self, nanoseconds)
        class(parquet_time), intent(inout) :: self  !! receives the value (marked valid).
        integer(int64), intent(in) :: nanoseconds !! nanoseconds since midnight, [0, 86400e9 - 1].
        if (nanoseconds < 0_int64 .or. nanoseconds >= NS_PER_DAY) then
            error stop EP//"nanoseconds-of-day out of range in parquet_time%set_raw"
        end if
        self%nanoseconds = nanoseconds
        self%valid = .true.
    end subroutine time_set_raw
    !
    !> Returns the raw nanoseconds-since-midnight, or 0 for a null element (interop/advanced
    !! accessor; never aborts -- validity travels separately via is_null).
    elemental integer(int64) function time_raw(self) result(res)
        class(parquet_time), intent(in) :: self !! the element.
        res = 0_int64
        if (self%valid) res = self%nanoseconds
    end function time_raw
    !
    !> Writes the element as ISO-8601 "HH:MM:SS[.fraction]" (fraction omitted when zero, else
    !! the shortest of 3/6/9 digits that is exact); aborts on a null element. A subroutine for
    !! the same reason as parquet_date%to_string.
    subroutine time_to_string(self, str)
        class(parquet_time), intent(in) :: self           !! the element.
        character(len=:), allocatable, intent(out) :: str !! receives the formatted time.
        character(len=24) :: buf
        integer :: pos
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in to_string (guard with is_null)"
        end if
        pos = 0
        call format_time_fields(self%nanoseconds, buf, pos)
        call format_fraction(mod(self%nanoseconds, NS_PER_SECOND), buf, pos)
        str = buf(1:pos)
    end subroutine time_to_string
    !
    !> Sets the element from an ISO-8601 time string "HH:MM:SS[.fraction]" (1..9 fraction
    !! digits). Failure handling matches parquet_date%parse: abort by default, or report via
    !! the optional `success` (leaving the element null).
    impure elemental subroutine time_parse(self, str, success) ! GCOVR_EXCL_LINE
        class(parquet_time), intent(out) :: self  !! receives the parsed time (or null on caught failure).
        character(len=*), intent(in) :: str       !! ISO-8601 time string.
        logical, intent(out), optional :: success !! .true. on success; absent => abort on failure.
        integer :: h, mi, s
        integer(int64) :: ns
        logical :: ok
        call parse_time_fields(str, h, mi, s, ns, ok)
        if (.not. ok) then
            if (present(success)) then
                success = .false.
                return
            end if
            error stop EP//"not a valid ISO-8601 time (expected HH:MM:SS[.fraction]) in parquet_time%parse"
        end if
        self%nanoseconds = (int(h, int64)*3600_int64 + int(mi, int64)*60_int64 &
            + int(s, int64))*NS_PER_SECOND + ns
        self%valid = .true.
        if (present(success)) success = .true.
    end subroutine time_parse
    !
    !> Equality specific; see the operator(==) generic. Aborts on a null operand.
    elemental logical function time_eq(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left operand.
        class(parquet_time), intent(in) :: b !! right operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"comparison with a null parquet_time element (guard with is_null)"
        end if
        res = a%nanoseconds == b%nanoseconds
    end function time_eq
    !
    !> Inequality specific; see the operator(/=) generic. Aborts on a null operand.
    elemental logical function time_ne(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left operand.
        class(parquet_time), intent(in) :: b !! right operand.
        res = .not. time_eq(a, b)
    end function time_ne
    !
    !> Less-than specific; see the operator(<) generic. Aborts on a null operand.
    elemental logical function time_lt(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left operand.
        class(parquet_time), intent(in) :: b !! right operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"comparison with a null parquet_time element (guard with is_null)"
        end if
        res = a%nanoseconds < b%nanoseconds
    end function time_lt
    !
    !> Less-or-equal specific; see the operator(<=) generic. Aborts on a null operand.
    elemental logical function time_le(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left operand.
        class(parquet_time), intent(in) :: b !! right operand.
        res = .not. time_lt(b, a)
    end function time_le
    !
    !> Greater-than specific; see the operator(>) generic. Aborts on a null operand.
    elemental logical function time_gt(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left operand.
        class(parquet_time), intent(in) :: b !! right operand.
        res = time_lt(b, a)
    end function time_gt
    !
    !> Greater-or-equal specific; see the operator(>=) generic. Aborts on a null operand.
    elemental logical function time_ge(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left operand.
        class(parquet_time), intent(in) :: b !! right operand.
        res = .not. time_lt(a, b)
    end function time_ge
    !
    !> Constructor specific for the parquet_time generic: builds a valid time element from
    !! (hour, minute, second[, nanosecond]); aborts on invalid fields.
    impure elemental function time_new(hour, minute, second, nanosecond) result(res) ! GCOVR_EXCL_LINE
        integer(int32), intent(in) :: hour                 !! hour, 0..23.
        integer(int32), intent(in) :: minute               !! minute, 0..59.
        integer(int32), intent(in) :: second               !! second, 0..59.
        integer(int32), intent(in), optional :: nanosecond !! sub-second part, 0..999999999 (default 0).
        type(parquet_time) :: res                          !! the constructed element (valid).
        call time_set(res, hour, minute, second, nanosecond)
    end function time_new
    !
    !> Difference specific; see the operator(-) generic. Nanoseconds, exact (bounded to +-1 day
    !! of ns since both operands lie in [0, 86400e9 - 1]); aborts on a null operand.
    elemental integer(int64) function time_diff(a, b) result(res)
        class(parquet_time), intent(in) :: a !! left (later) operand.
        class(parquet_time), intent(in) :: b !! right (earlier) operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"difference with a null parquet_time element (guard with is_null)"
        end if
        res = a%nanoseconds - b%nanoseconds
    end function time_diff
    !
    !> Shared worker for the ns-offset operator(+)/operator(-) specifics: shifts self by
    !! `offset` nanoseconds and always wraps into a valid time-of-day, [0, 86400e9 - 1] --
    !! `offset` itself must already be validated to fit in [-86400e9, 86400e9] by the caller
    !! (the 24h magnitude guard), so this worker never aborts once entered. Uses floor_div-style
    !! floored modulo, not the intrinsic MOD, so a negative offset wraps correctly.
    impure elemental function time_offset_ns_impl(self, offset) result(res)
        class(parquet_time), intent(in) :: self !! the element to shift.
        integer(int64), intent(in) :: offset    !! signed ns offset, already within [-86400e9, 86400e9].
        type(parquet_time) :: res               !! self shifted by offset ns, wrapped to a valid time-of-day.
        integer(int64) :: raw
        if (.not. self%valid) then
            error stop EP//"null parquet_time element accessed in operator(+)/operator(-) (guard with is_null)"
        end if
        raw = self%nanoseconds + offset
        res%nanoseconds = raw - floor_div(raw, NS_PER_DAY)*NS_PER_DAY
        res%valid = .true.
    end function time_offset_ns_impl
    !
    !> int32 specific of operator(-); see time_offset_ns_impl. The 24h magnitude guard is moot
    !! for this kind (int32's own range never reaches 86400e9 ns), kept only for symmetry with
    !! the int64 specific.
    impure elemental function time_sub_ns_i32(self, n) result(res)
        class(parquet_time), intent(in) :: self !! the element to shift.
        integer(int32), intent(in) :: n         !! ns to subtract.
        type(parquet_time) :: res               !! self shifted back by n ns, wrapped.
        res = time_offset_ns_impl(self, -int(n, int64))
    end function time_sub_ns_i32
    !
    !> int64 specific of operator(-); see time_offset_ns_impl. Aborts if abs(n) exceeds 24h of
    !! nanoseconds (checked on the raw, not-yet-negated input, so negating it afterwards can
    !! never overflow).
    impure elemental function time_sub_ns_i64(self, n) result(res)
        class(parquet_time), intent(in) :: self !! the element to shift.
        integer(int64), intent(in) :: n         !! ns to subtract.
        type(parquet_time) :: res               !! self shifted back by n ns, wrapped.
        if (n > NS_PER_DAY .or. n < -NS_PER_DAY) then
            error stop EP//"offset magnitude exceeds 24 hours in parquet_time operator(-)"
        end if
        res = time_offset_ns_impl(self, -n)
    end function time_sub_ns_i64
    !
    !> int32 specific of operator(+); see time_offset_ns_impl. The 24h magnitude guard is moot
    !! for this kind (int32's own range never reaches 86400e9 ns), kept only for symmetry with
    !! the int64 specific.
    impure elemental function time_add_ns_i32(self, n) result(res)
        class(parquet_time), intent(in) :: self !! the element to shift.
        integer(int32), intent(in) :: n         !! ns to add.
        type(parquet_time) :: res               !! self shifted forward by n ns, wrapped.
        res = time_offset_ns_impl(self, int(n, int64))
    end function time_add_ns_i32
    !
    !> int64 specific of operator(+); see time_offset_ns_impl. Aborts if abs(n) exceeds 24h of
    !! nanoseconds.
    impure elemental function time_add_ns_i64(self, n) result(res)
        class(parquet_time), intent(in) :: self !! the element to shift.
        integer(int64), intent(in) :: n         !! ns to add.
        type(parquet_time) :: res               !! self shifted forward by n ns, wrapped.
        if (n > NS_PER_DAY .or. n < -NS_PER_DAY) then
            error stop EP//"offset magnitude exceeds 24 hours in parquet_time operator(+)"
        end if
        res = time_offset_ns_impl(self, n)
    end function time_add_ns_i64
    !
    ! ==================================================================================
    ! parquet_timestamp
    ! ==================================================================================
    !
    !> Civil-fields specific of the set generic: sets the element from
    !! (year, month, day, hour, minute, second[, nanosecond]), validated; marks it valid.
    impure elemental subroutine ts_set_civil(self, year, month, day, hour, minute, second, nanosecond)
        class(parquet_timestamp), intent(inout) :: self      !! receives the instant (marked valid).
        integer(int32), intent(in) :: year                 !! calendar year.
        integer(int32), intent(in) :: month                !! month, 1..12.
        integer(int32), intent(in) :: day                  !! day of month.
        integer(int32), intent(in) :: hour                 !! hour, 0..23.
        integer(int32), intent(in) :: minute               !! minute, 0..59.
        integer(int32), intent(in) :: second               !! second, 0..59.
        integer(int32), intent(in), optional :: nanosecond !! sub-second part, 0..999999999 (default 0).
        integer(int64) :: d64, ns
        if (month < 1 .or. month > 12) then
            error stop EP//"invalid month in parquet_timestamp%set (must be 1..12)"
        end if
        if (day < 1 .or. day > days_in_month(int(year, int64), int(month))) then
            error stop EP//"invalid day of month in parquet_timestamp%set"
        end if
        if (hour < 0 .or. hour > 23) error stop EP//"invalid hour in parquet_timestamp%set (must be 0..23)"
        if (minute < 0 .or. minute > 59) error stop EP//"invalid minute in parquet_timestamp%set (must be 0..59)"
        if (second < 0 .or. second > 59) error stop EP//"invalid second in parquet_timestamp%set (must be 0..59)"
        ns = 0_int64
        if (present(nanosecond)) then
            if (nanosecond < 0 .or. int(nanosecond, int64) >= NS_PER_SECOND) then
                error stop EP//"invalid nanosecond in parquet_timestamp%set (must be 0..999999999)"
            end if
            ns = int(nanosecond, int64)
        end if
        d64 = days_from_civil(int(year, int64), int(month), int(day))
        self%seconds = d64*SECONDS_PER_DAY + int(hour, int64)*3600_int64 &
            + int(minute, int64)*60_int64 + int(second, int64)
        self%nanoseconds = int(ns, int32)
        self%valid = .true.
    end subroutine ts_set_civil
    !
    !> Date-plus-time specific of the set generic: combines a parquet_date and a parquet_time
    !! into an instant. A null input propagates: the result is null (never aborts).
    impure elemental subroutine ts_set_date_time(self, date, time)
        class(parquet_timestamp), intent(out) :: self !! receives the instant (or null if an input is null).
        type(parquet_date), intent(in) :: date        !! the calendar-date part.
        type(parquet_time), intent(in) :: time        !! the time-of-day part.
        if (.not. (date%valid .and. time%valid)) return
        self%seconds = int(date%days, int64)*SECONDS_PER_DAY + time%nanoseconds/NS_PER_SECOND
        self%nanoseconds = int(mod(time%nanoseconds, NS_PER_SECOND), int32)
        self%valid = .true.
    end subroutine ts_set_date_time
    !
    !> Returns all civil fields of the element; aborts on a null element, and on a year outside
    !! the integer(int32) range (practically unreachable for real data).
    elemental subroutine ts_get(self, year, month, day, hour, minute, second, nanosecond)
        class(parquet_timestamp), intent(in) :: self        !! the element.
        integer(int32), intent(out) :: year                 !! calendar year.
        integer(int32), intent(out) :: month                !! month, 1..12.
        integer(int32), intent(out) :: day                  !! day of month.
        integer(int32), intent(out) :: hour                 !! hour, 0..23.
        integer(int32), intent(out) :: minute               !! minute, 0..59.
        integer(int32), intent(out) :: second               !! second, 0..59.
        integer(int32), intent(out), optional :: nanosecond !! sub-second part, 0..999999999.
        integer(int64) :: d64, sod, y
        integer :: m, d
        if (.not. self%valid) then
            error stop EP//"null parquet_timestamp element accessed in get (guard with is_null)"
        end if
        d64 = floor_div(self%seconds, SECONDS_PER_DAY)
        sod = self%seconds - d64*SECONDS_PER_DAY
        call civil_from_days(d64, y, m, d)
        if (y > int(huge(0_int32), int64) .or. y < -int(huge(0_int32), int64) - 1_int64) then
            error stop EP//"year out of integer(int32) range in parquet_timestamp%get"
        end if
        year = int(y, int32)
        month = int(m, int32)
        day = int(d, int32)
        hour = int(sod/3600_int64, int32)
        minute = int(mod(sod/60_int64, 60_int64), int32)
        second = int(mod(sod, 60_int64), int32)
        if (present(nanosecond)) nanosecond = self%nanoseconds
    end subroutine ts_get
    !
    !> Returns the calendar-date part as a parquet_date. A null element propagates to a null
    !! result (never aborts on null); aborts only if the date falls outside parquet_date's
    !! representable range (practically unreachable for real data).
    elemental function ts_get_date(self) result(res)
        class(parquet_timestamp), intent(in) :: self !! the element.
        type(parquet_date) :: res                    !! the date part (null if the element is null).
        integer(int64) :: d64
        if (.not. self%valid) return
        d64 = floor_div(self%seconds, SECONDS_PER_DAY)
        if (d64 > DATE_DAYS_MAX .or. d64 < DATE_DAYS_MIN) then
            error stop EP//"date part out of parquet_date range in parquet_timestamp%get_date"
        end if
        res%days = int(d64, int32)
        res%valid = .true.
    end function ts_get_date
    !
    !> Returns the time-of-day part as a parquet_time. A null element propagates to a null
    !! result (never aborts).
    elemental function ts_get_time(self) result(res)
        class(parquet_timestamp), intent(in) :: self !! the element.
        type(parquet_time) :: res                    !! the time-of-day part (null if the element is null).
        integer(int64) :: d64, sod
        if (.not. self%valid) return
        d64 = floor_div(self%seconds, SECONDS_PER_DAY)
        sod = self%seconds - d64*SECONDS_PER_DAY
        res%nanoseconds = sod*NS_PER_SECOND + int(self%nanoseconds, int64)
        res%valid = .true.
    end function ts_get_time
    !
    !> Returns whether the element is null (the primary null guard; never aborts).
    elemental logical function ts_is_null(self) result(res)
        class(parquet_timestamp), intent(in) :: self !! the element.
        res = .not. self%valid
    end function ts_is_null
    !
    !> Marks the element null.
    impure elemental subroutine ts_set_null(self)
        class(parquet_timestamp), intent(inout) :: self !! the element (reset to the null state).
        self%seconds = 0_int64
        self%nanoseconds = 0_int32
        self%valid = .false.
    end subroutine ts_set_null
    !
    !> Sets the raw (seconds, nanoseconds) pair directly (interop/advanced accessor; marks the
    !! element valid); aborts on a nanosecond part outside the normalized 0..999999999 range.
    impure elemental subroutine ts_set_raw(self, seconds, nanoseconds)
        class(parquet_timestamp), intent(inout) :: self !! receives the value (marked valid).
        integer(int64), intent(in) :: seconds         !! whole seconds since 1970-01-01T00:00:00.
        integer(int32), intent(in) :: nanoseconds     !! nanosecond-of-second part, 0..999999999.
        if (nanoseconds < 0 .or. int(nanoseconds, int64) >= NS_PER_SECOND) then
            error stop EP//"nanosecond part out of range in parquet_timestamp%set_raw (must be 0..999999999)"
        end if
        self%seconds = seconds
        self%nanoseconds = nanoseconds
        self%valid = .true.
    end subroutine ts_set_raw
    !
    !> Returns the raw (seconds, nanoseconds) pair, or zeros for a null element
    !! (interop/advanced accessor; never aborts -- validity travels separately via is_null).
    elemental subroutine ts_get_raw(self, seconds, nanoseconds)
        class(parquet_timestamp), intent(in) :: self !! the element.
        integer(int64), intent(out) :: seconds       !! whole seconds since 1970-01-01T00:00:00 (0 when null).
        integer(int32), intent(out) :: nanoseconds   !! nanosecond-of-second part (0 when null).
        seconds = 0_int64
        nanoseconds = 0_int32
        if (self%valid) then
            seconds = self%seconds
            nanoseconds = self%nanoseconds
        end if
    end subroutine ts_get_raw
    !
    !> int32 specific of set_unix; see ts_set_unix_i64.
    impure elemental subroutine ts_set_unix_i32(self, value, unit)
        class(parquet_timestamp), intent(inout) :: self !! receives the value (marked valid).
        integer(int32), intent(in) :: value           !! Unix time in `unit`.
        integer, intent(in) :: unit                   !! one of the parquet_unit_* constants.
        call ts_set_unix_i64(self, int(value, int64), unit)
    end subroutine ts_set_unix_i32
    !
    !> int64 specific of set_unix: sets the element from a Unix-time value -- `value` counts
    !! time since 1970-01-01T00:00:00 in `unit`. The unit is a conversion parameter only; it
    !! is not stored (the internal representation is always the canonical seconds+nanoseconds
    !! pair). Always exact; marks the element valid.
    impure elemental subroutine ts_set_unix_i64(self, value, unit)
        class(parquet_timestamp), intent(inout) :: self !! receives the value (marked valid).
        integer(int64), intent(in) :: value           !! Unix time in `unit`.
        integer, intent(in) :: unit                   !! one of the parquet_unit_* constants.
        integer(int64) :: scale, q, r
        scale = unit_scale(unit)
        q = floor_div(value, scale)
        r = value - q*scale
        self%seconds = q
        self%nanoseconds = int(r*(NS_PER_SECOND/scale), int32)
        self%valid = .true.
    end subroutine ts_set_unix_i64
    !
    !> Returns the element as a single Unix-time integer in `unit` -- a pure query, never
    !! modifying the element. Aborts on a null element, on int64 overflow in the requested
    !! unit, and on precision loss (the value carries sub-`unit` precision) unless
    !! exact=.false., which floors toward negative infinity instead (floor, not truncation,
    !! so ordering is preserved across the epoch).
    elemental integer(int64) function ts_to_unix(self, unit, exact) result(res)
        class(parquet_timestamp), intent(in) :: self !! the element (unchanged).
        integer, intent(in) :: unit                  !! one of the parquet_unit_* constants.
        logical, intent(in), optional :: exact       !! default .true.; .false. => floor instead of abort.
        integer(int64) :: scale, ns_per_unit, q, lo_limit
        logical :: need_exact
        if (.not. self%valid) then
            error stop EP//"null parquet_timestamp element accessed in to_unix (guard with is_null)"
        end if
        need_exact = .true.
        if (present(exact)) need_exact = exact
        scale = unit_scale(unit)
        ns_per_unit = NS_PER_SECOND/scale
        if (need_exact .and. mod(int(self%nanoseconds, int64), ns_per_unit) /= 0_int64) then
            error stop EP//"precision loss in parquet_timestamp%to_unix (value has sub-unit precision;"// &
                " pass exact=.false. to floor)"
        end if
        q = int(self%nanoseconds, int64)/ns_per_unit ! 0 <= q < scale
        if (scale > 1_int64) then
            if (self%seconds >= 0_int64) then
                if (self%seconds > (huge(0_int64) - q)/scale) then
                    error stop EP//"overflow in parquet_timestamp%to_unix (value does not fit int64 in this unit)"
                end if
            else
                ! `INT64_MIN/scale`, computed without naming the constant -- see INT64_MIN's own
                ! declaration for why any expression mentioning it is unusable here. `2**63` is
                ! `huge + 1`, so the magnitude is `huge/scale`, plus one exactly when the
                ! remainder is one short of a whole divisor; negating it reproduces Fortran's
                ! truncation toward zero. `scale > 1` is guaranteed by the branch above, which is
                ! what keeps the `+ 1` from overflowing (at `scale == 1` it would be `huge + 1`).
                lo_limit = huge(0_int64)/scale
                if (mod(huge(0_int64), scale) + 1_int64 == scale) lo_limit = lo_limit + 1_int64
                if (self%seconds < -lo_limit) then
                    error stop EP//"overflow in parquet_timestamp%to_unix (value does not fit int64 in this unit)"
                end if
            end if
        end if
        res = self%seconds*scale + q
    end function ts_to_unix
    !
    !> Sets the element from a real64 Modified Julian Date (MJD 0 = 1858-11-17T00:00:00),
    !! rounding to the nearest nanosecond; marks it valid. Aborts on NaN or a magnitude beyond
    !! the representable range. real64 only by design -- real32 would silently lose precision
    !! (consecutive real32 values around a present-day MJD are ~340 s apart) and is deliberately
    !! not accepted.
    impure elemental subroutine ts_set_mjd(self, mjd)
        class(parquet_timestamp), intent(inout) :: self !! receives the instant (marked valid).
        real(real64), intent(in) :: mjd               !! Modified Julian Date (fractional days).
        real(real64) :: dd, frac
        integer(int64) :: d64, ns_of_day
        if (ieee_is_nan(mjd)) error stop EP//"NaN passed to parquet_timestamp%set_mjd"
        if (mjd >= MJD_ABS_BOUND .or. mjd <= -MJD_ABS_BOUND) then
            error stop EP//"MJD out of range in parquet_timestamp%set_mjd"
        end if
        dd = aint(mjd)
        frac = mjd - dd
        d64 = int(dd, int64)
        if (frac < 0.0_real64) then ! aint truncates toward zero; normalize to a [0,1) day fraction
            d64 = d64 - 1_int64
            frac = frac + 1.0_real64
        end if
        ns_of_day = nint(frac*real(NS_PER_DAY, real64), int64)
        if (ns_of_day >= NS_PER_DAY) then ! rounding rolled over to the next day
            d64 = d64 + 1_int64
            ns_of_day = 0_int64
        end if
        self%seconds = (d64 - MJD_UNIX_EPOCH)*SECONDS_PER_DAY + ns_of_day/NS_PER_SECOND
        self%nanoseconds = int(mod(ns_of_day, NS_PER_SECOND), int32)
        self%valid = .true.
    end subroutine ts_set_mjd
    !
    !> Returns the element as a real64 Modified Julian Date; aborts on a null element.
    !! Resolution is limited by real64: ~1 microsecond in the current era -- for exact
    !! nanosecond round-trips use the civil fields or set_unix/to_unix instead.
    elemental real(real64) function ts_to_mjd(self) result(res)
        class(parquet_timestamp), intent(in) :: self !! the element.
        integer(int64) :: d64, sod
        if (.not. self%valid) then
            error stop EP//"null parquet_timestamp element accessed in to_mjd (guard with is_null)"
        end if
        d64 = floor_div(self%seconds, SECONDS_PER_DAY)
        sod = self%seconds - d64*SECONDS_PER_DAY
        res = real(d64 + MJD_UNIX_EPOCH, real64) &
            + (real(sod, real64) + real(self%nanoseconds, real64)*1.0e-9_real64)/real(SECONDS_PER_DAY, real64)
    end function ts_to_mjd
    !
    !> Sets the element from a real64 Julian Date (JD = MJD + 2400000.5); same rounding, range
    !! and real64-only rationale as set_mjd. Note the coarser real64 resolution at JD
    !! magnitudes (~50 microseconds in the current era).
    impure elemental subroutine ts_set_jd(self, jd)
        class(parquet_timestamp), intent(inout) :: self !! receives the instant (marked valid).
        real(real64), intent(in) :: jd                !! Julian Date (fractional days).
        if (ieee_is_nan(jd)) error stop EP//"NaN passed to parquet_timestamp%set_jd"
        call ts_set_mjd(self, jd - JD_MJD_OFFSET)
    end subroutine ts_set_jd
    !
    !> Returns the element as a real64 Julian Date (JD = MJD + 2400000.5); aborts on a null
    !! element. Resolution ~50 microseconds in the current era (real64 at JD magnitudes) --
    !! prefer to_mjd, the civil fields, or to_unix when that matters.
    elemental real(real64) function ts_to_jd(self) result(res)
        class(parquet_timestamp), intent(in) :: self !! the element.
        res = ts_to_mjd(self) + JD_MJD_OFFSET
    end function ts_to_jd
    !
    !> Writes the element as ISO-8601 "YYYY-MM-DDTHH:MM:SS[.fraction]" (year formatting as in
    !! parquet_date%to_string; fraction as in parquet_time%to_string); aborts on a null
    !! element. A subroutine for the same reason as parquet_date%to_string.
    subroutine ts_to_string(self, str)
        class(parquet_timestamp), intent(in) :: self      !! the element.
        character(len=:), allocatable, intent(out) :: str !! receives the formatted date-time.
        character(len=48) :: buf
        integer(int64) :: d64, sod, y
        integer :: m, d, pos
        if (.not. self%valid) then
            error stop EP//"null parquet_timestamp element accessed in to_string (guard with is_null)"
        end if
        d64 = floor_div(self%seconds, SECONDS_PER_DAY)
        sod = self%seconds - d64*SECONDS_PER_DAY
        call civil_from_days(d64, y, m, d)
        call format_date_fields(y, m, d, buf, pos)
        buf(pos+1:pos+1) = 'T'
        pos = pos + 1
        call format_time_fields(sod*NS_PER_SECOND, buf, pos)
        call format_fraction(int(self%nanoseconds, int64), buf, pos)
        str = buf(1:pos)
    end subroutine ts_to_string
    !
    !> Sets the element from an ISO-8601 date-time string
    !! "[-]YYYY-MM-DD{T or space}HH:MM:SS[.fraction][Z]" (an optional trailing 'Z' is accepted
    !! and ignored -- values are stored as epoch offsets regardless). Failure handling matches
    !! parquet_date%parse: abort by default, or report via the optional `success` (leaving the
    !! element null).
    impure elemental subroutine ts_parse(self, str, success) ! GCOVR_EXCL_LINE
        class(parquet_timestamp), intent(out) :: self !! receives the parsed instant (or null on caught failure).
        character(len=*), intent(in) :: str           !! ISO-8601 date-time string.
        logical, intent(out), optional :: success     !! .true. on success; absent => abort on failure.
        integer :: a, b, sep, m, d, h, mi, s
        integer(int64) :: y, ns, d64, sec, sod
        logical :: ok
        ok = .false.
        d64 = 0_int64
        sec = 0_int64
        sod = 0_int64
        b = len_trim(str)
        a = 1
        do while (a <= b)
            if (str(a:a) /= ' ') exit
            a = a + 1
        end do
        if (a <= b) then
            if (str(b:b) == 'Z') b = b - 1
        end if
        sep = 0
        do while (a + sep <= b) ! find the date/time separator ('T' or a single space)
            if (str(a+sep:a+sep) == 'T' .or. str(a+sep:a+sep) == ' ') exit
            sep = sep + 1
        end do
        sep = a + sep
        if (sep > a .and. sep < b) then
            call parse_date_fields(str(a:sep-1), y, m, d, ok)
            if (ok) call parse_time_fields(str(sep+1:b), h, mi, s, ns, ok)
        end if
        if (ok) then ! exact range check: the resulting epoch seconds must fit int64
            d64 = days_from_civil(y, m, d)
            ok = d64 <= PARSE_DAYS_BOUND .and. d64 >= -PARSE_DAYS_BOUND
        end if
        if (ok) then
            sec = d64*SECONDS_PER_DAY
            sod = int(h, int64)*3600_int64 + int(mi, int64)*60_int64 + int(s, int64)
            ok = sec <= huge(0_int64) - sod
        end if
        if (.not. ok) then
            if (present(success)) then
                success = .false.
                return
            end if
            error stop EP//"not a valid ISO-8601 date-time (expected YYYY-MM-DDTHH:MM:SS[.fraction]),"// &
                " or out of the representable range, in parquet_timestamp%parse"
        end if
        self%seconds = sec + sod
        self%nanoseconds = int(ns, int32)
        self%valid = .true.
        if (present(success)) success = .true.
    end subroutine ts_parse
    !
    !> Equality specific; see the operator(==) generic. Aborts on a null operand.
    elemental logical function ts_eq(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left operand.
        class(parquet_timestamp), intent(in) :: b !! right operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"comparison with a null parquet_timestamp element (guard with is_null)"
        end if
        res = a%seconds == b%seconds .and. a%nanoseconds == b%nanoseconds
    end function ts_eq
    !
    !> Inequality specific; see the operator(/=) generic. Aborts on a null operand.
    elemental logical function ts_ne(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left operand.
        class(parquet_timestamp), intent(in) :: b !! right operand.
        res = .not. ts_eq(a, b)
    end function ts_ne
    !
    !> Less-than specific; see the operator(<) generic. Aborts on a null operand.
    elemental logical function ts_lt(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left operand.
        class(parquet_timestamp), intent(in) :: b !! right operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"comparison with a null parquet_timestamp element (guard with is_null)"
        end if
        res = a%seconds < b%seconds .or. (a%seconds == b%seconds .and. a%nanoseconds < b%nanoseconds)
    end function ts_lt
    !
    !> Less-or-equal specific; see the operator(<=) generic. Aborts on a null operand.
    elemental logical function ts_le(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left operand.
        class(parquet_timestamp), intent(in) :: b !! right operand.
        res = .not. ts_lt(b, a)
    end function ts_le
    !
    !> Greater-than specific; see the operator(>) generic. Aborts on a null operand.
    elemental logical function ts_gt(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left operand.
        class(parquet_timestamp), intent(in) :: b !! right operand.
        res = ts_lt(b, a)
    end function ts_gt
    !
    !> Greater-or-equal specific; see the operator(>=) generic. Aborts on a null operand.
    elemental logical function ts_ge(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left operand.
        class(parquet_timestamp), intent(in) :: b !! right operand.
        res = .not. ts_lt(a, b)
    end function ts_ge
    !
    !> Civil-fields constructor specific for the parquet_timestamp generic; aborts on invalid
    !! fields.
    impure elemental function ts_new_civil(year, month, day, hour, minute, second, nanosecond) result(res) ! GCOVR_EXCL_LINE
        integer(int32), intent(in) :: year                 !! calendar year.
        integer(int32), intent(in) :: month                !! month, 1..12.
        integer(int32), intent(in) :: day                  !! day of month.
        integer(int32), intent(in) :: hour                 !! hour, 0..23.
        integer(int32), intent(in) :: minute               !! minute, 0..59.
        integer(int32), intent(in) :: second               !! second, 0..59.
        integer(int32), intent(in), optional :: nanosecond !! sub-second part, 0..999999999 (default 0).
        type(parquet_timestamp) :: res                     !! the constructed element (valid).
        call ts_set_civil(res, year, month, day, hour, minute, second, nanosecond)
    end function ts_new_civil
    !
    !> Date-plus-time constructor specific for the parquet_timestamp generic; a null input
    !! propagates to a null result (never aborts).
    impure elemental function ts_new_date_time(date, time) result(res)
        type(parquet_date), intent(in) :: date !! the calendar-date part.
        type(parquet_time), intent(in) :: time !! the time-of-day part.
        type(parquet_timestamp) :: res         !! the constructed element (null if an input is null).
        call ts_set_date_time(res, date, time)
    end function ts_new_date_time
    !
    !> Difference specific; see the operator(-) generic. Nanoseconds, exact; aborts on a null
    !! operand and when the elapsed time exceeds ~292.3 years (TS_DIFF_NS_BOUND_SECONDS) -- the
    !! bound is checked on the seconds component alone, without ever forming an intermediate
    !! value that could itself overflow int64, so the "too far apart to represent" case is
    !! detected before any arithmetic that could silently wrap.
    elemental integer(int64) function ts_diff_ns(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left (later) operand.
        class(parquet_timestamp), intent(in) :: b !! right (earlier) operand.
        integer(int64) :: hi, lo, sec_diff
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"difference with a null parquet_timestamp element (guard with is_null)"
        end if
        if (b%seconds > huge(0_int64) - TS_DIFF_NS_BOUND_SECONDS) then
            hi = huge(0_int64)
        else
            hi = b%seconds + TS_DIFF_NS_BOUND_SECONDS
        end if
        if (b%seconds < INT64_MIN + TS_DIFF_NS_BOUND_SECONDS) then
            lo = INT64_MIN
        else
            lo = b%seconds - TS_DIFF_NS_BOUND_SECONDS
        end if
        if (a%seconds > hi .or. a%seconds < lo) then
            error stop EP//"parquet_timestamp difference exceeds the representable nanosecond range"// &
                " (elapsed time beyond ~292.3 years)"
        end if
        sec_diff = a%seconds - b%seconds
        res = sec_diff*NS_PER_SECOND + (int(a%nanoseconds, int64) - int(b%nanoseconds, int64))
    end function ts_diff_ns
    !
    !> Type-bound diff_seconds: real64-seconds difference between two instants. Unlike
    !! operator(-) (ts_diff_ns), this never aborts on magnitude -- only precision is lost at
    !! extreme elapsed times, matching real64's own resolution. Aborts on a null operand.
    !! Each operand's seconds component is converted to real64 *before* subtracting (rather
    !! than subtracting as int64 first) -- two instants near opposite ends of the int64 range
    !! would overflow an int64 subtraction, whereas real64 subtraction only loses precision,
    !! matching this function's own "never aborts on magnitude" contract.
    elemental real(real64) function ts_diff_seconds(a, b) result(res)
        class(parquet_timestamp), intent(in) :: a !! left (later) operand.
        class(parquet_timestamp), intent(in) :: b !! right (earlier) operand.
        if (.not. (a%valid .and. b%valid)) then
            error stop EP//"difference with a null parquet_timestamp element (guard with is_null)"
        end if
        res = (real(a%seconds, real64) - real(b%seconds, real64)) &
            + (real(a%nanoseconds, real64) - real(b%nanoseconds, real64))*parquet_ns_to_sec
    end function ts_diff_seconds
    !
    !> Shared worker for the ns-offset operator(+)/operator(-) specifics: shifts self by
    !! `n` nanoseconds, carrying the normalized nanosecond-of-second component back into
    !! [0, 999999999] via floor_div.
    !! Aborts on a null operand or if the shifted result overflows int64 seconds -- unlike
    !! parquet_date/parquet_time, there is no smaller domain-specific range to enforce, only int64
    !! itself.
    impure elemental function ts_offset_ns_impl(self, n) result(res)
        class(parquet_timestamp), intent(in) :: self !! the instant to shift.
        integer(int64), intent(in) :: n              !! signed ns offset (+ forward, - backward).
        type(parquet_timestamp) :: res                !! self shifted by n ns.
        integer(int64) :: ns_self, total_ns, carry_sec
        if (.not. self%valid) then
            error stop EP//"null parquet_timestamp element accessed in operator(+)/operator(-) (guard with is_null)"
        end if
        ns_self = int(self%nanoseconds, int64)
        if (n >= 0_int64) then
            if (ns_self > huge(0_int64) - n) then
                error stop EP//"parquet_timestamp offset arithmetic overflows int64"
            end if
        end if
        total_ns = ns_self + n
        carry_sec = floor_div(total_ns, NS_PER_SECOND)
        if (carry_sec >= 0_int64) then
            if (self%seconds > huge(0_int64) - carry_sec) then
                error stop EP//"parquet_timestamp offset arithmetic overflows int64 seconds"
            end if
        else
            if (self%seconds < offset_floor(carry_sec)) then
                error stop EP//"parquet_timestamp offset arithmetic overflows int64 seconds"
            end if
        end if
        res%seconds = self%seconds + carry_sec
        res%nanoseconds = int(total_ns - carry_sec*NS_PER_SECOND, int32)
        res%valid = .true.
    end function ts_offset_ns_impl
    !
    !> int32 specific of operator(-); see ts_offset_ns_impl.
    impure elemental function ts_sub_ns_i32(self, n) result(res)
        class(parquet_timestamp), intent(in) :: self !! the instant to shift.
        integer(int32), intent(in) :: n              !! ns to subtract.
        type(parquet_timestamp) :: res                !! self shifted back by n ns.
        res = ts_offset_ns_impl(self, -int(n, int64))
    end function ts_sub_ns_i32
    !
    !> int64 specific of operator(-); see ts_offset_ns_impl.
    impure elemental function ts_sub_ns_i64(self, n) result(res)
        class(parquet_timestamp), intent(in) :: self !! the instant to shift.
        integer(int64), intent(in) :: n              !! ns to subtract.
        type(parquet_timestamp) :: res                !! self shifted back by n ns.
        if (n == INT64_MIN) then
            error stop EP//"parquet_timestamp offset arithmetic overflows int64"
        end if
        res = ts_offset_ns_impl(self, -n)
    end function ts_sub_ns_i64
    !
    !> int32 specific of operator(+); see ts_offset_ns_impl.
    impure elemental function ts_add_ns_i32(self, n) result(res)
        class(parquet_timestamp), intent(in) :: self !! the instant to shift.
        integer(int32), intent(in) :: n              !! ns to add.
        type(parquet_timestamp) :: res                !! self shifted forward by n ns.
        res = ts_offset_ns_impl(self, int(n, int64))
    end function ts_add_ns_i32
    !
    !> int64 specific of operator(+); see ts_offset_ns_impl.
    impure elemental function ts_add_ns_i64(self, n) result(res)
        class(parquet_timestamp), intent(in) :: self !! the instant to shift.
        integer(int64), intent(in) :: n              !! ns to add.
        type(parquet_timestamp) :: res                !! self shifted forward by n ns.
        res = ts_offset_ns_impl(self, n)
    end function ts_add_ns_i64
    !
end module parquet_temporal ! GCOVR_EXCL_LINE
