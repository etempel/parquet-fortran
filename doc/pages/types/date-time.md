---
title: Date, time and timestamp columns
---

`parquet_temporal` provides three element-level value types — `parquet_date`, `parquet_time`,
`parquet_timestamp` — for reading and writing Parquet `DATE`/`TIME`/`TIMESTAMP` columns. Each
type holds **one value** (not a whole column, unlike [`parquet_string_column`](string-columns.html)),
so an array of them behaves exactly like an `integer`/`real` array: `type(parquet_timestamp) ::
ts(nrows)` for a scalar column, `type(parquet_timestamp) :: ts(col_size, nrows)` for a vector
column — passed to `parquet_write_column`/`parquet_read_column` (and their chunked/row-mode/
element-mode counterparts) exactly like any other supported type.

All three types, and the `parquet_unit_*` unit-selector constants used by `set_unix`/`to_unix`
below, are re-exported from `use parquet` — a separate `use parquet_temporal` is only needed if
you want the types without the rest of the library (it depends only on the intrinsic modules
`iso_fortran_env` and `ieee_arithmetic`).

## Quick start

```fortran
program datetime_quickstart
    use parquet
    use iso_fortran_env, only: int64
    implicit none

    type(parquet_writer) :: writer
    type(parquet_reader) :: reader
    type(parquet_date) :: observed(3)
    type(parquet_timestamp) :: taken_at(3)
    character(len=:), allocatable :: s

    call observed(1)%set(2024, 7, 16)
    call observed(2)%set(2024, 7, 17)
    call observed(3)%set_null()                        ! a missing value

    call taken_at(1)%set(2024, 7, 16, 12, 34, 56)
    call taken_at(2)%parse("2024-07-17T08:00:00.5")     ! from an ISO-8601 string
    call taken_at(3)%set_unix(1721260800_int64, parquet_unit_seconds)

    call parquet_open_writer(writer, "data.parquet")
    call parquet_write_column(writer, "observed", observed)
    call parquet_write_column(writer, "taken_at", taken_at)
    call parquet_close_writer(writer)

    call parquet_open_reader(reader, "data.parquet")
    call parquet_read_column(reader, "observed", observed)
    call parquet_read_column(reader, "taken_at", taken_at)
    call parquet_close_reader(reader)

    if (.not. taken_at(1)%is_null()) then
        call taken_at(1)%to_string(s)
        print *, s   ! 2024-07-16T12:34:56
    end if
end program datetime_quickstart
```

## The three types

| Type | Represents | Canonical storage |
|---|---|---|
| `parquet_date` | a calendar date (proleptic Gregorian) | days since 1970-01-01 (identical to Parquet's `DATE`) |
| `parquet_time` | a time of day | nanoseconds since midnight |
| `parquet_timestamp` | an instant | seconds since 1970-01-01T00:00:00 + a nanosecond-of-second part |

`parquet_timestamp`'s two-field storage is deliberate: it reads *every* valid Parquet timestamp
losslessly, unlike a single-field nanosecond count (which overflows outside 1677–2262) or a
microsecond count (which truncates a nanosecond-precision file). A `parquet_timestamp` holds the
stored value verbatim — it does not interpret or adjust for a timezone; see
[Reading files written by other tools](#reading-files-written-by-other-tools) for how a column's
own timezone is exposed separately.

## Null values are part of the element, not a separate mask

Unlike every other supported type, **these three types carry their own null state** — there is
no `is_valid=`/`null_value=` argument anywhere on the read or write side for them:

```fortran
call parquet_write_column(writer, "ev", values)          ! no is_valid= argument
call parquet_read_column(reader, "ev", values)            ! no null_value=/is_valid= argument
```

On write, whatever elements are null (`%is_null()`) become genuine Parquet Nulls. Whether the
column is *declared* nullable in the file's schema depends on how it was written. A **whole-column**
write has seen every element before it writes anything, so it declares the column non-nullable
exactly when none of them is null. A **streamed** write (`parquet_write_column_chunk`) cannot: the
first row group fixes the schema, and a null-free first row group says nothing about the seventh —
so a streamed `date`/`time`/`timestamp` column is always written nullable, whatever its elements
hold. Declaring one of them null-free is what *protecting* the column is for; see
[Null values](supported-data-types.html#null-values) for `protected_cols:` and `%set_protected`,
and `parquet_get_column_nullable` for reading back the flag a file actually ended up with.

On read, a genuine Parquet Null becomes a null element (checked with `%is_null()`) — and reading a
null-containing column **never aborts**, unlike the strict default the numeric/string readers apply
(see [Null values](supported-data-types.html#null-values)): validity always lives in the elements
themselves, so there is nothing to opt into.

A default-initialized element (`type(parquet_date) :: d`, no `%set` call yet) is also null — a
useful property for catching accidental use of an unset value, since every semantic accessor
(below) aborts on it rather than silently returning garbage.

### Two-tier element access

- **Semantic accessors abort on a null element**: `%get`, `%year`/`%month`/`%day`/`%hour`/...,
  `%to_string`, `%to_mjd`/`%to_jd`, `%to_unix`, and all six comparison operators. Guard with
  `%is_null()` first.
- **Interop accessors never abort on a null element**: `%raw`/`%get_raw` return a defined
  placeholder (`0`, or zeros for the timestamp's `(seconds, nanoseconds)` pair) instead — this is
  what lets the read/write integration layer bulk-convert whole arrays without special-casing
  nulls. `%set_raw` has no null to trip over either, but it does validate its *argument*:
  `parquet_time%set_raw` aborts on a nanosecond count outside one day and
  `parquet_timestamp%set_raw` on a nanosecond-of-second part outside 0..999999999, while
  `parquet_date%set_raw` accepts any `int32` day count.
- **Type-to-type conversions propagate null**: `ts%get_date()` on a null `parquet_timestamp`
  returns a null `parquet_date`; `parquet_timestamp(date, time)` with either input null returns a
  null timestamp. Neither aborts on a null — though `%get_date` does abort when a *non-null*
  instant's date part falls outside `parquet_date`'s own +-5.8 million year range.

## Setting and reading values

```fortran
type(parquet_date) :: d
type(parquet_time) :: t
type(parquet_timestamp) :: ts
integer :: y, mo, day, h, mi, s, ns

call d%set(2024, 7, 16)
call t%set(12, 34, 56, 500000000)              ! nanosecond is optional, defaults to 0
call ts%set(2024, 7, 16, 12, 34, 56, 500000000) ! civil fields, or...
call ts%set(d, t)                               ! ...compose from a date + a time

y = d%year(); mo = d%month(); day = d%day()
call d%get(y, mo, day)                          ! or all at once
call ts%get(y, mo, day, h, mi, s, ns)

d = ts%get_date()                               ! decompose back into parts
t = ts%get_time()
```

Every `%set` validates its fields (month 1–12, a day valid for that month/year, hour 0–23, ...)
and `error stop`s on an invalid civil date/time rather than silently normalizing it (no leap
seconds, no month-13-becomes-next-January rollover).

Each type also has a **constructor of the same name**, taking the same fields as its `%set` and
validating them identically, for when a value is wanted as an expression rather than assigned into
an existing variable. They are elemental, so they build whole arrays too:

```fortran
d  = parquet_date(2024, 7, 16)
t  = parquet_time(12, 34, 56)                    ! nanosecond optional, as in %set
ts = parquet_timestamp(2024, 7, 16, 12, 34, 56)  ! civil fields, or...
ts = parquet_timestamp(d, t)                     ! ...a date + a time (null propagates)
dates = parquet_date(years, months, days)        ! elemental: whole arrays at once
```

## Formatting and parsing (ISO-8601)

```fortran
character(len=:), allocatable :: s
logical :: ok

call ts%to_string(s)              ! "2024-07-16T12:34:56.500"
call d%to_string(s)                ! "2024-07-16"
call t%to_string(s)                ! "12:34:56.500"

call ts%parse("2024-07-16T12:34:56.5")     ! error stops on a malformed/invalid string
call ts%parse("garbage", success=ok)        ! ok=.false., element left null; no abort
```

The fractional part is omitted entirely when the sub-second value is zero, and otherwise printed
with the shortest of 3, 6 or 9 digits that reproduces it exactly — so half a second is `.500`, not
`.5`, and a nanosecond-precision value gets all nine digits.

`parse` accepts `'T'` or a space as the date/time separator and an optional trailing `'Z'`
(accepted and ignored — values are stored as epoch offsets regardless of timezone). Pass the
optional `success` argument to catch a parse failure instead of aborting; on failure the element
is left null (a defined state), and `success` is set `.false.`.

## Comparisons

All six comparison operators (`==`, `/=`, `<`, `<=`, `>`, `>=`) are defined and work directly on
whole arrays (elemental):

```fortran
if (ts(1) < ts(2)) ...
mask = dates(1:n-1) < dates(2:n)   ! whole-array comparison
```

Comparing against a null element aborts (there is no non-arbitrary answer, and silently
ordering nulls could corrupt a filter/sort) — guard with `%is_null()` first.

## Difference and offset arithmetic

All three types support `operator(-)` between two values of the *same* type (never across
types — there is no `parquet_date - parquet_time`), giving the elapsed time as a plain integer,
and `operator(+)`/`operator(-)` between a value and a plain integer offset (`integer(int32)` or
`integer(int64)`), shifting it by a count of the type's own natural unit and returning a new
value of the same type:

```fortran
integer(int64) :: days_between, ns_between
type(parquet_date) :: tomorrow
type(parquet_time) :: wrapped
type(parquet_timestamp) :: later

days_between = observed(2) - observed(1)          ! whole days, exact
ns_between = taken_at(2) - taken_at(1)              ! nanoseconds, exact (aborts past ~292 years apart)

tomorrow = observed(1) + 1                          ! one day forward
wrapped = t + 3600_int64*parquet_ns_per_sec         ! one hour forward, wraps at midnight
later = taken_at(1) + parquet_ns_per_sec            ! one second forward
```

| Type | `a - b` | `a ± n` |
|---|---|---|
| `parquet_date` | `integer(int64)` whole days | `parquet_date`, whole days; aborts if the result falls outside the +-5.8 million year range |
| `parquet_time` | `integer(int64)` nanoseconds | `parquet_time`, nanoseconds; **wraps** to a valid time-of-day rather than aborting on the result, but aborts if `abs(n)` exceeds 24h of nanoseconds |
| `parquet_timestamp` | `integer(int64)` nanoseconds; aborts if the two instants are more than ~292.3 years apart | `parquet_timestamp`, nanoseconds; aborts only on int64 overflow (a single instant already has far more range than a nanosecond offset could safely add to it) |

`parquet_timestamp` additionally has `%diff_seconds`, a type-bound function (not an operator —
`operator(-)`'s one same-type slot is already `ts_diff_ns`) returning the difference as
`real(real64)` seconds: unlike `operator(-)`, it never aborts on magnitude, only losing precision
at extreme elapsed times:

```fortran
secs = taken_at(2)%diff_seconds(taken_at(1))   ! real64 seconds; never aborts on magnitude
```

All difference/offset operators abort on a null operand (guard with `%is_null()` first), matching
the comparison operators above. Every operator is elemental, so it applies directly to whole
arrays (`gap = dates(2:n) - dates(1:n-1)`).

Four public constants convert between a raw nanosecond count and human-scale units, without a
hand-typed magic number at the call site:

```fortran
integer(int64), parameter :: parquet_ns_per_sec = 1000000000_int64      !! exact; ns in one second.
integer(int64), parameter :: parquet_ns_per_day = 86400000000000_int64  !! exact; ns in one day.
real(real64),   parameter :: parquet_ns_to_sec  = 1.0e-9_real64         !! convenience; ns -> fractional seconds.
real(real64),   parameter :: parquet_ns_to_day  = 1.0_real64/86400.0e9_real64 !! convenience; ns -> fractional days.
```

`_per_X` names an exact `integer(int64)` divisor, for a whole-unit result
(`ns_value/parquet_ns_per_day` gives an exact whole day count, discarding any remainder — the
same tradeoff `operator(-)` on two dates already makes); `_to_X` names a `real(real64)`
multiplicative factor, for a fractional result (`real(ns_value, real64)*parquet_ns_to_day` gives
a fractional day count instead). There are no millisecond or minute constants — Parquet's own
`TIME`/`TIMESTAMP` units are only ever milliseconds/microseconds/nanoseconds, and "minutes" isn't
a unit this module (or Parquet itself) otherwise models anywhere.

## Interop: Unix time, Modified Julian Date, Julian Date

`set_unix`/`to_unix` convert a `parquet_timestamp` to/from a single Unix-time integer in a
caller-chosen unit — for interop with systems that speak Unix time (instruments, other
languages, JSON APIs), **not** related to what unit a file stores (see
[Units](#units-and-schema-declared-columns) below):

```fortran
call ts%set_unix(1721133296123_int64, parquet_unit_millis)   ! both arguments required
v = ts%to_unix(parquet_unit_micros)                        ! pure query, never mutates ts
```

`to_unix` aborts if the value carries finer precision than the requested unit (e.g. asking for
milliseconds from a nanosecond-precision instant) or if the result overflows `int64` in that unit.
Pass `exact=.false.` to floor toward the requested unit instead of aborting on precision; the
overflow check applies either way.

For astronomy/scientific use, `parquet_timestamp` also converts to/from Modified Julian Date and
Julian Date (`real64`, never `real32` — consecutive `real32` values around a present-day MJD are
about 340 seconds apart, so a `real32` MJD could not even resolve minutes):

```fortran
call ts%set_mjd(60507.5_real64)     ! MJD 60507.5
mjd = ts%to_mjd()                    ! ~1 microsecond resolution in the current era
call ts%set_jd(2460508.0_real64)    ! JD = MJD + 2400000.5
jd = ts%to_jd()                      ! ~50 microsecond resolution in the current era
```

`parquet_date` has its own, exact-integer MJD conversion (`%to_mjd()`/`%set_mjd()`, no
fractional part, since a pure date *is* a whole MJD number).

## Units and schema-declared columns

Without a schema, a temporal column defaults to microseconds (timestamp/time) — the ecosystem-safe
choice, since some readers in the wider ecosystem reject nanosecond-precision timestamps; `date`
has no unit. Writing a value
with finer precision than the target unit `error stop`s (a nanosecond-precision `parquet_time`
value written into a microsecond column, say) rather than silently truncating.

To declare a specific unit, use a MAML `data_type` token — see
[Building a schema in code](../schema/building-schema-in-code.html#building-a-schema-with-init-and-add_field) or
[The MAML metadata format](../schema/maml-format.html#the-fields-section):

```yaml
fields:
- name: day
  data_type: date                    # unitless
- name: clock
  data_type: time[ms]                # milliseconds
- name: ev
  data_type: timestamp[ns,utc]       # nanoseconds, UTC-adjusted
```

- `date` — no unit suffix (a unit suffix on `date` fails validation).
- `time[unit]` / `timestamp[unit]` — `unit` is one of `ms`/`us`/`ns` (also accepted:
  `milli(s)`/`micro(s)`/`nano(s)`); bare `time`/`timestamp` (no brackets) defaults to
  microseconds.
- `timestamp[unit,utc]` (or bare `timestamp[utc]` for the default unit) additionally marks the
  column UTC-adjusted. Without `utc`, a column written by this library is timezone-naive.
- **`s`/`sec`/`seconds` is never accepted as a unit**, even though `parquet_unit_seconds` exists
  as a constant (for `set_unix`/`to_unix` only) — Parquet's physical format has no
  seconds-resolution `TIME`/`TIMESTAMP` encoding at all (only milliseconds/microseconds/
  nanoseconds), so a declared `s` unit could never actually be honored on write.

The same `schema%add_field` call used for any other type works for these tokens:

```fortran
call schema%add_field("ev", "timestamp[ns,utc]", info="event time")
```

`qc:` **bounds** (`min:`/`max:`) are not supported for `date`/`time`/`timestamp` columns yet —
`parquet_validate_maml` rejects a field declaring one with a clear message, rather than silently
ignoring it. `qc: miss:` is a separate matter and *is* supported on these columns, behaving exactly
as it does elsewhere (see [Quality control](../schema/quality-control.html)) — worth knowing here,
since a temporal column carries its nulls in the element and so is a natural place to declare
whether Nulls are expected. `parquet_filter` rules on these columns *are*
supported, comparing against a double-quoted ISO-8601 literal — see
[Filtering `date`, `time` and `timestamp` columns](../io/filter-sort-sample.html#filtering-date-time-and-timestamp-columns).

### Querying a column's stored unit and timezone

`parquet_get_column_time_info(reader, name, [unit], [timezone])` reads back a `time`/
`timestamp` column's stored unit (as a `parquet_unit_*` selector) and, for a timestamp, its
timezone string (empty for a timezone-naive column). Square brackets mark optional arguments:

```fortran
integer :: unit
character(len=:), allocatable :: tz

call parquet_get_column_time_info(reader, "ev", unit=unit, timezone=tz)
if (unit == parquet_unit_nanos .and. tz == "UTC") ...
```

Aborts if `name` is not a `time`/`timestamp` column. This is distinct from
[`parquet_get_metadata`](../io/reading.html#reading-table-metadata-with-parquet_get_metadata), which
serves user-defined key/value metadata rather than this schema-level property.

## Reading files written by other tools

This library's own writer only ever produces `date32`, `time32[ms]`/`time64[us/ns]`, and
`timestamp[ms/us/ns]` (naive or UTC-adjusted) columns. Reading is broader, since a file from
another Arrow-based tool may use representations this library's writer never emits:

- **Legacy `INT96` timestamps** (old Impala/Spark files) — read transparently as
  `parquet_timestamp` (Arrow decodes `INT96` to nanosecond precision internally).
- **An arbitrary IANA timezone string** (not just UTC/naive) — the element still holds the
  stored epoch offset verbatim; `parquet_get_column_time_info`'s `timezone` reports the actual
  string (e.g. `"America/New_York"`).
- Arrow's `date64`/second-resolution `TIME`/`TIMESTAMP` types are part of Arrow's in-memory type
  system but have no physical representation in the Parquet format itself (Parquet's `DATE`
  logical type requires an `int32` day count; its `TIME`/`TIMESTAMP` units are milliseconds/
  microseconds/nanoseconds only) — so no genuine `.parquet` file can contain them, from any
  writer.

## Vector, chunked, row-mode and element-mode reads/writes

Every access pattern the other supported types have works identically for `parquet_date`/
`parquet_time`/`parquet_timestamp`:

- **Vector columns**: `type(parquet_timestamp) :: ts(col_size, nrows)`, same
  `parquet_write_column`/`parquet_read_column` calls as a scalar column — see
  [Supported data types](supported-data-types.html).
- **Streaming/chunked writes and reads**: `parquet_new_row_group`/`parquet_write_column_chunk`/
  `parquet_finish_row_group` and `parquet_read_column_chunk` — see
  [Streaming/chunked writes](../io/writing.html#streamingchunked-writes) /
  [Streaming/chunked reads](../io/reading.html#streamingchunked-reads). As with every other type,
  `parquet_write_column_chunk` requires an exact `data_type` match (no cross-type conversion).
- **Row mode / element mode**: `parquet_read_array_row_mode`/`parquet_read_array_element_mode` —
  see [Reading only touches the columns you ask for](../io/reading.html#reading-only-touches-the-columns-you-ask-for).
- **Write-time row masks**: `parquet_write_row_mask`/`parquet_write_chunk_row_mask` drop rows from
  a temporal column exactly as they do from any other — see
  [Filtering rows with a mask](../io/writing.html#filtering-rows-with-a-mask).

None of these take an `is_valid=`/`null_value=` argument, for the same reason the whole-column
calls don't — see [Null values are part of the element](#null-values-are-part-of-the-element-not-a-separate-mask)
above.

The three types are also first-class outside the reader and writer:

- **In-memory tables**: a `parquet_table` column can hold any of them, scalar or vector — see
  [Tables in memory](../tables/table.html).
- **Sorting**: `pf_sort`/`pf_argsort` and the rest of `parquet_sorting` take arrays of all three
  directly, with their null state understood as part of the ordering — see
  [Supported types](../utilities/sorting.html#supported-types).

## Not yet supported

- `qc:` range checks (`min:`/`max:`) on `date`/`time`/`timestamp` columns (deferred; rejected at
  validation rather than silently ignored — see
  [Units and schema-declared columns](#units-and-schema-declared-columns) above, and note that
  `qc: miss:` on these columns *is* supported).
  `parquet_filter` rules on these columns are *not* on this list — they are implemented, see
  [Filtering `date`, `time` and `timestamp` columns](../io/filter-sort-sample.html#filtering-date-time-and-timestamp-columns).
- `INTERVAL`/duration values — a deliberately dropped non-goal, not a pending gap: Parquet's
  legacy `INTERVAL` converted type was never migrated to Parquet's modern `LogicalType` union and
  has no mainstream write path in Arrow/pyarrow/Hive/Trino either, and Arrow's separate
  `DURATION` type only round-trips through Parquet as a semantically-untyped plain `INT64`
  column readable through the existing numeric path — neither needs a dedicated type here. (Timestamp
  arithmetic itself is supported — see
  [Difference and offset arithmetic](#difference-and-offset-arithmetic) above.)
- `LIST`/`MAP` element types are unrelated to this page; see
  [Supported data types](supported-data-types.html#reading-a-nested-struct-field) for the library's current struct/list/map
  coverage.
