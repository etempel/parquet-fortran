# Design + implementation plan: `parquet_temporal` module

Status: **complete.** All 7 steps done and verified: `fpm test` exit 0 (both plain and
CI-equivalent `FPM_FFLAGS="-fopenmp"` builds), `tools/coverage.sh` reports **100.00%
(6541/6541)** line coverage across every `src/*.f90` file with no exclusions besides the two
documented, genuinely-unreachable-by-any-real-file/gcov-line-attribution-quirk cases (see the
GCOVR_EXCL_LINE comments in `src/parquet_temporal.f90`), `tools/run_error_scenarios.sh` clean,
`ford docs.md` clean run besides the expected Graphviz warning, `tools/check_doc_anchors.py`
clean. Module renamed `parquet_datatype` -> `parquet_temporal` mid-Step-7 (user request, to
leave room for future sibling `parquet_map`/`parquet_list` modules) and re-verified end to end
after the rename. Left uncommitted on `main` for user review per repo convention.

Process (per review): implementation proceeds one plan step at a time, with a **stop for
review after every step** before continuing to the next.

## Resolved decisions (from review)

| # | Decision | Resolution |
|---|----------|------------|
| 1 | Module name | `parquet_temporal` (singular — it holds one-element types, unlike `parquet_strings`' column type) |
| 2 | Timestamp representation | Lossless: `int64` seconds + `int32` nanoseconds |
| 3 | Strict reads | Yes — date/time columns readable only via these types, never raw integers |
| 4 | Write-side units | Fixed defaults, never auto-picked: `timestamp` → micros, `time` → micros, `date` unitless; explicit `[ms]`/`[us]`/`[ns]` override in MAML |
| 5 | Timezone metadata | Dedicated query `parquet_get_column_time_info` (separate from `parquet_get_metadata`, which is for user metadata) |
| 6 | Extras | MJD **and** JD conversions in v1 (`real64` only, never `real32` — see micro-decision 5; date's own MJD is exact integer); `parse` gets an optional `success` catch argument; `to_unix` gets `exact=.false.` truncation option |

## Answers to the raised design points

### Nulls live inside the element (design change, accepted)

Every type carries its own validity flag. Consequences, all simplifications:

- The date/time read/write specifics need **no `null_value=`/`is_valid=` arguments at all**.
- Reading a null-containing date/time column **never aborts** (unlike the strict default for
  int/real/logical/string) — nulls arrive as null elements, checked via `is_null()`. This
  intentional difference gets documented in the guide page.
- Writing: validity is gathered from the elements themselves; a column becomes nullable in the
  file only if at least one element is null (matches existing behavior). MAML
  `protected_cols:` is still enforced — a null element in a protected column is an
  `error stop`.
- A default-initialized element **is null** ("no value yet"), so uninitialized use is caught
  by the null-access abort instead of producing garbage.

Element-access semantics (two-tier rule):

- **Semantic accessors abort on null**: `get`, `year`/`month`/`day`, `hour`/..., `to_string`,
  `to_mjd`/`to_jd`, `to_unix`, and all six comparison operators (comparing against null has no
  non-arbitrary answer; silently ordering nulls could corrupt a filtering mask — against the
  library's no-silent-wrong-answer philosophy). Guard with `is_null()`. (`error stop` in a
  pure elemental procedure is legal Fortran 2018; **empirically verified** on this machine's
  gfortran with `-std=f2018`, as is the whole elemental-TBP pattern itself — whole-array
  `call ts%set(...)` / `mask = ts%is_null()` compile and run.)
- **Interop accessors never abort**: `raw`/`set_raw`/`get_raw` return a defined placeholder
  (0) for a null element — validity travels separately via `is_null()`. This is what lets the
  integration layer bulk-convert whole arrays elementally without tripping on nulls.
- **Type-to-type conversions propagate null** instead of aborting: `ts%get_date()` of a null
  timestamp is a null `parquet_date`; `ts%set(date, time)` with a null input yields a null
  timestamp.
- `set_null()` marks an element null; every successful `set`/`parse`/`set_raw`/`set_unix`/
  `set_mjd`/`set_jd` marks it valid.

### `set_epoch`/`to_epoch` → renamed `set_unix`/`to_unix`

The old name was confusing — there is no epoch parameter anywhere. The epoch (1970-01-01T00:00:00)
is the fixed reference the stored `seconds` component already counts from; these procedures
just expose that count **as a single integer in a caller-chosen unit** ("Unix time"):

- `call ts%set_unix(1721088000123_int64, parquet_unit_millis)` — construct from a Unix-time
  value, as delivered by instruments, catalogs, OS clocks, JSON APIs, other languages.
- `t = ts%to_unix(parquet_unit_micros)` — export to such systems, or do quick arithmetic
  (differences in a fixed unit) until a future duration type exists.

They are *not* used by the file read/write path itself (that uses the `raw`/`get_raw`
accessors and the schema's unit on the C++ side); they exist purely for caller interop.
`to_unix(unit)` conversion to a coarser unit than the value carries (e.g. a ns-precision value
to millis) is lossy: default is `error stop`; `exact=.false.` floors toward negative infinity
instead (floor, not truncation, so ordering is preserved across the epoch). Overflow (e.g.
year 3000 in nanos) always aborts. Not part of `to_string`/`parse` — those speak ISO-8601 only.

**How the unit works — nothing is stored (clarified in review).** The internal representation
is *always* the canonical `(seconds, nanoseconds)` pair; the `unit` argument is purely a
conversion parameter for that one call, like specifying the units of a number you are handing
over or receiving. `call ts%set_unix(123000_int64, parquet_unit_millis)` and
`call ts%set_unix(123_int64, parquet_unit_seconds)` produce the *identical* internal state,
and after either call the unit is forgotten — there is no "current unit" property on the
element. Consequently `parquet_write_column` needs **no** unit argument: the unit a *file*
stores is a property of the column (from the MAML/schema `timestamp[us]` token or the
default), and the write path converts canonical → file unit itself. `set_unix`/`to_unix` are
caller-side conveniences only, completely unrelated to the write path.

Confirmed understanding from review (both statements correct):

- `ts%set_unix(1721088000123_int64, parquet_unit_millis)` — a subroutine that sets `ts` from
  an integer + unit, both arguments required (the value is interpreted as time since
  1970-01-01T00:00:00 in that unit, converted to the canonical pair, and the element is
  marked valid).
- `ts%to_unix(parquet_unit_micros)` — a function returning the value in the requested unit;
  it never changes the internal state.

### Final interfaces: `parse`, `set_unix`, `to_unix`

`parse` (shown for `parquet_date`; `parquet_time` and `parquet_timestamp` are identical in
shape, differing only in the string format they accept):

```fortran
!> Sets the element from an ISO-8601 date string "YYYY-MM-DD". On failure (unparseable
!! or invalid fields): error stop by default; if `success` is present, no abort happens --
!! `success` is set .false. and the element is set to null (a defined state) instead.
impure elemental subroutine date_parse(self, str, success)
    class(parquet_date), intent(out) :: self  !! receives the parsed value (or null on caught failure)
    character(len=*), intent(in) :: str       !! ISO-8601 date string
    logical, intent(out), optional :: success !! .true. on success; absent => abort on failure
end subroutine date_parse
```

`set_unix` — generic over both integer kinds per the repo's both-kinds rule (a caller passing
a literal or default-`INTEGER` variable, e.g. small second counts, must not hit a kind
mismatch); both specifics share one implementation:

```fortran
!> Sets the element from a Unix-time value: `value` counts time since 1970-01-01T00:00:00
!! in `unit`. The unit is a conversion parameter only -- it is not stored. Marks the
!! element valid. Generic over integer(int32)/integer(int64) `value`.
impure elemental subroutine ts_set_unix_i64(self, value, unit)
    class(parquet_timestamp), intent(out) :: self !! receives the value (marked valid)
    integer(int64), intent(in) :: value           !! Unix time in `unit` (required)
    integer, intent(in) :: unit                   !! parquet_unit_seconds/millis/micros/nanos (required)
end subroutine ts_set_unix_i64
```

`to_unix`:

```fortran
!> Returns the element as a single Unix-time integer in `unit`. A pure query -- never
!! modifies the element. Aborts on a null element, on int64 overflow in `unit`, and on
!! precision loss (value carries sub-`unit` precision) unless exact=.false., which floors
!! toward -infinity instead (preserving ordering across the epoch).
impure elemental function ts_to_unix(self, unit, exact) result(value)
    class(parquet_timestamp), intent(in) :: self !! the element (unchanged)
    integer, intent(in) :: unit                  !! parquet_unit_seconds/millis/micros/nanos
    logical, intent(in), optional :: exact       !! default .true.; .false. => floor instead of abort
    integer(int64) :: value                      !! Unix time in `unit`
end function ts_to_unix
```

(`to_unix`'s result is always `integer(int64)` — Fortran cannot overload on the result, and an
int64 result assigned to a caller's int32 variable is ordinary Fortran assignment semantics.)


Civil↔days math (and everything built on it: `to_string`, MJD/JD) is implemented **inside the
module in pure Fortran**, not via Arrow C++:

- The module must stay standalone (layering rule: depends only on `iso_fortran_env`; usable
  before any reader/writer exists).
- `elemental` accessors cannot call through the C boundary, and a per-element C call would be
  slow anyway.
- The algorithm (Howard Hinnant's `days_from_civil`/`civil_from_days`, proleptic Gregorian,
  exact integer math, O(1), no lookup tables) is small and public domain.

The file read/write path never does civil math at all — it moves raw day counts / (sec, ns)
pairs; only the *unit* scaling happens at the C++ boundary (where the file's unit is known).

**Cross-validation (user requirement)**: Arrow C++ vendors this exact algorithm (Hinnant's
`date.h`) for its own timestamp handling, so a **test-only** `extern "C"` hook in
`parquet_wrapper.cpp` (e.g. `parquet_debug_civil_from_days`) can convert via Arrow's vendored
routines; a unit test sweeps a wide range of day counts (plus targeted leap-year/century/
negative-year cases) and asserts the Fortran results match Arrow's exactly. The hook follows
the established debug-hook pattern: `bind(C)` interface declared locally in the test file,
never in `parquet_bindings.f90`. End-to-end fixtures (below) additionally validate the full
path against files Arrow itself wrote.

## Micro-decisions made while finalizing (veto if you disagree)

1. `parse` failure catch: `logical, intent(out), optional :: success` (single failure
   semantics, simplest form; on failure with `success` present, the element is set to null —
   a defined state). Chosen over an integer `stat` code since there's no meaningful error
   taxonomy for "unparseable or invalid".
2. Comparisons involving a null element `error stop` (rationale above) rather than defining
   an arbitrary null ordering.
3. Unit constants gain `parquet_unit_seconds` (Arrow supports seconds-unit timestamps and
   `time32[s]`; also handy for `set_unix`/`to_unix`).
4. Write-side timezone: files are written **timezone-naive by default** (isAdjustedToUTC =
   false, matching pyarrow's default for tz-naive data); MAML can opt in to UTC-instant
   semantics with a `,utc` suffix: `data_type: timestamp[us,utc]` (also bare
   `timestamp[utc]` = default unit + UTC). No general timezone strings on write — UTC or
   naive only; arbitrary-tz files are still *readable* (values are epoch offsets regardless),
   with the tz string reported by `parquet_get_column_time_info`.
5. **MJD/JD kinds (updated per review): `real(real64)` only — never `real32`** (a real32
   MJD resolves only to ~2 seconds; accepting it invites silent precision loss). No real32
   specifics will exist anywhere in the MJD/JD surface.
   `parquet_timestamp%to_mjd()`/`to_jd()`/`set_mjd()`/`set_jd()` are real64-only
   (fractional day). Documented precision: real64 MJD resolves ~1 µs in the current era, JD
   only ~50 µs — for exact ns round-trips use civil fields or `set_unix`, not JD.
   One interpretation kept from the earlier draft, flagged for veto: `parquet_date`'s own
   `to_mjd()`/`set_mjd()` stay **integer** (int32/int64 generic) — a pure date *is* a whole
   MJD number, and integers are exact by construction, which is precisely the motivation
   behind the real64-only rule; a real-valued MJD on a date would need an
   exact-integer runtime check instead. (If you prefer real64 there too for uniformity,
   say so — it is a trivial change.)
   `time32[s]`; also handy for `set_unix`/`to_unix`).
4. Write-side timezone: files are written **timezone-naive by default** (isAdjustedToUTC =
   false, matching pyarrow's default for tz-naive data); MAML can opt in to UTC-instant
   semantics with a `,utc` suffix: `data_type: timestamp[us,utc]` (also bare
   `timestamp[utc]` = default unit + UTC). No general timezone strings on write — UTC or
   naive only; arbitrary-tz files are still *readable* (values are epoch offsets regardless),
   with the tz string reported by `parquet_get_column_time_info`.
5. `parquet_date%to_mjd()` returns integer MJD (a date *is* a whole MJD number;
   `set_mjd` takes int32/int64 generically per the both-kinds rule);
   `parquet_timestamp%to_mjd()`/`to_jd()`/`set_mjd()`/`set_jd()` use `real(real64)`
   (fractional day). Documented precision: real64 MJD resolves ~1 µs in the current era, JD
   only ~50 µs — for exact ns round-trips use civil fields or `set_unix`, not JD.

---

# Final design

## 1. Placement and layering

Standalone module `parquet_temporal` in `src/parquet_temporal.f90`, depending only on
`iso_fortran_env` (same layering contract as `parquet_strings`: the read/write integration
layer depends on it, never the reverse). Module is `private`; public surface: the three
types, their type-bound procedures, constructor interfaces, and four unit constants.

## 2. The three types

Plain **value types** — no pointers, no allocatables. `type(parquet_timestamp) :: ts(nelem,
nrows)` behaves exactly like an `integer(int64)` array: contiguous, assignable, sliceable, no
`target`/lifetime rules.

```fortran
type :: parquet_date
    private
    integer(int32) :: days = 0_int32   !! days since 1970-01-01 (identical to Parquet DATE)
    logical :: valid = .false.          !! .false. = null (default: null until set)
end type    ! range ±5.8 million years; 8 bytes/element

type :: parquet_time
    private
    integer(int64) :: nanoseconds = 0_int64 !! ns since midnight, [0, 86 399 999 999 999]
    logical :: valid = .false.               !! .false. = null
end type    ! holds any Parquet TIME unit exactly; 16 bytes/element

type :: parquet_timestamp
    private
    integer(int64) :: seconds = 0_int64     !! seconds since 1970-01-01T00:00:00
    integer(int32) :: nanoseconds = 0_int32  !! sub-second part, always [0, 999 999 999]
    logical :: valid = .false.               !! .false. = null
end type    ! lossless for every valid Parquet timestamp; 16 bytes/element
```

Design rules:

- **Canonical unit per type, normalized at the boundary.** The file's unit
  (seconds/millis/micros/nanos) is column-level metadata; the C++ side converts to/from the
  canonical representation on read/write.
- **Null state inside the element** (semantics in the answers section above).
- **No timezone math in the type.** Elements hold the stored epoch offset verbatim;
  interpretation metadata is column-level (`parquet_get_column_time_info`).

## 3. Coverage: what Parquet/Arrow types this holds, and how

| Source type (on disk / Arrow)                        | Fortran type        | Conversion                  | Lossless?      |
|-------------------------------------------------------|----------------------|------------------------------|-----------------|
| `DATE` / Arrow `date32`                               | `parquet_date`       | identity                     | yes             |
| Arrow `date64` (ms since epoch)                       | `parquet_date`       | ÷ 86 400 000, checked exact  | yes (validated) |
| `TIME` millis / Arrow `time32[s]`, `time32[ms]`       | `parquet_time`       | × scale to ns                | yes             |
| `TIME` micros/nanos / Arrow `time64[us]`, `time64[ns]`| `parquet_time`       | × scale to ns                | yes             |
| `TIMESTAMP` ms/us/ns, Arrow `timestamp[s]`, any tz    | `parquet_timestamp`  | split into (sec, ns)         | yes             |
| legacy `INT96` timestamps (old Impala/Spark files)    | `parquet_timestamp`  | via Arrow's ns decode        | yes             |
| `INTERVAL` / Arrow `duration`, `month_day_nano`       | — not covered        | future `parquet_duration` here | —             |

Write side produces `date32` / `time64[unit]` / `timestamp[unit]` (defaults: micros; UTC flag
per MAML `,utc`). Nulls read/written natively for all three.

## 4. Public module surface

```fortran
public :: parquet_date, parquet_time, parquet_timestamp   ! types + constructor interfaces
public :: parquet_unit_seconds, parquet_unit_millis, &     ! integer parameters for
          parquet_unit_micros, parquet_unit_nanos          !   set_unix/to_unix + write API
```

Constructor interfaces (structure constructors are unusable outside the module once
components are private): `d = parquet_date(2026, 7, 16)`, `t = parquet_time(12, 30, 5)`,
`ts = parquet_timestamp(2026, 7, 16, 12, 30, 5)` — pure functions returning the type.

## 5. Type-bound procedure surface

Getters/operators `elemental`; setters/parsers that validate are `impure elemental`
(elemental → every accessor works on whole arrays: `mask = ts%is_null()`,
`ok = d1 < d2` elementwise). `to_string` is the one non-elemental member (allocatable
`intent(out)` argument; per project convention a **subroutine**, never a
`character(len=:), allocatable` function — CLAUDE.md / GCC PR113797).

**`parquet_date`** (all bindings public; operator specifics private):

```fortran
contains
    procedure :: set => date_set            !! impure elemental; (year, month, day), validated
    procedure :: get => date_get            !! elemental sub; year, month, day out; aborts on null
    procedure :: year / month / day         !! elemental getters; abort on null
    procedure :: is_null => date_is_null    !! elemental; the null guard (never aborts)
    procedure :: set_null => date_set_null  !! elemental; mark element null
    procedure :: set_raw / raw              !! elemental; raw day count (interop; raw()=0 on null)
    procedure :: set_mjd (int32/int64 generic) / to_mjd  !! integer Modified Julian Date
    procedure :: to_string => date_to_string !! subroutine; ISO-8601 "YYYY-MM-DD"; aborts on null
    procedure :: parse => date_parse        !! impure elemental; ISO-8601; optional success out
    generic :: operator(==), (/=), (<), (<=), (>), (>=)  !! elemental; abort on null operand
```

**`parquet_time`** (same shape):

```fortran
    procedure :: set                        !! impure elemental; (hour, minute, second[, nanosecond])
    procedure :: get                        !! elemental sub; hour, minute, second, nanosecond out
    procedure :: hour / minute / second / nanosecond   !! elemental getters; abort on null
    procedure :: is_null / set_null
    procedure :: set_raw / raw              !! elemental; ns-since-midnight (interop)
    procedure :: to_string                  !! "HH:MM:SS[.fraction]" (shortest of ms/us/ns groups)
    procedure :: parse                      !! ISO-8601 time; optional success out
    generic :: operator(==), (/=), (<), (<=), (>), (>=)
```

**`parquet_timestamp`**:

```fortran
    generic :: set => ts_set_civil, &       !! (year, month, day, hour, minute, second[, ns])
                      ts_set_date_time      !! (type(parquet_date), type(parquet_time));
                                            !!   null input propagates -> null timestamp
    procedure :: get                        !! elemental sub; all civil fields out; aborts on null
    procedure :: get_date / get_time        !! elemental; null propagates (never aborts)
    procedure :: is_null / set_null
    procedure :: set_raw / get_raw          !! elemental subs; (seconds, nanoseconds) pair (interop)
    procedure :: set_unix / to_unix         !! elemental; (value, unit) Unix time; set_unix generic
                                            !!   over int32/int64 value; to_unix aborts on
                                            !!   loss/overflow unless exact=.false. (floors)
    procedure :: set_mjd / to_mjd           !! real64 ONLY (never real32) Modified Julian Date
    procedure :: set_jd / to_jd             !! real64 ONLY (never real32) Julian Date (~50 us res.)
    procedure :: parse                      !! ISO-8601, 'T' or space, optional 'Z'; optional success
    generic :: operator(==), (/=), (<), (<=), (>), (>=)
```

Private module helpers (never exposed): `days_from_civil`/`civil_from_days` (Hinnant),
`is_leap_year`, `days_in_month`, ISO parse/format workers, `EP` error-prefix parameter,
validation helpers. Every abort message names the offending field/value,
`parquet_strings`-style.

## 6. Integration with `parquet_read_*` / `parquet_write_*`

- **Dispatch**: new specifics on the existing generics, dispatched on the derived type of
  `values` (exactly how `logical` vs `int32` disambiguate today). Full set per type:
  `parquet_write_column` (1D + matrix), `parquet_write_column_chunk` (1D + matrix),
  `parquet_read_column` (1D + full-array), `parquet_read_column_chunk`,
  `parquet_read_array_row_mode` (int32 + int64 `row_index` kinds, per the both-kinds rule),
  `parquet_read_array_element_mode`. Scalar columns, `(col_size, nrows)` vector columns, and
  chunked/streaming paths all work identically to existing types. **No `null_value=`/
  `is_valid=` arguments** — nulls are in the elements.
- **C++ boundary**: new `extern "C"` entry points per type (read/append + chunk variants),
  mirroring the int32/int64 ones. Read: validate logical type (strict — a non-date/time
  column, or reading a date/time column via any other type family, aborts with the standard
  type-mismatch diagnostic), normalize unit (incl. date64 exact-division check, `time32[s]`,
  `timestamp[s]`, INT96), fill plain buffers — `int32*` (date), `int64*` (time),
  `int64* + int32*` parallel pair (timestamp) — plus the existing `valid_out` validity path.
  Write: canonical buffers + schema unit in, exact-convertibility check (ns-carrying value
  into a `[us]` column → clean abort, same class as the float→int fractional-part check),
  build the Arrow array with `valid_in`.
- **Fortran bridging**: one temporary buffer per column/chunk filled via the elemental
  interop accessors (`raw()` placeholder-0 on null + `is_null()` for the validity buffer) —
  an extra copy, same size class as the data, identical in spirit to the string path.
- **MAML/schema**: new `data_type` tokens `date`, `time[unit]`, `timestamp[unit(,utc)]`;
  bare `time`/`timestamp` default to micros. `parquet_validate_maml` learns the tokens and
  rejects malformed unit suffixes (and any suffix on `date`). Schema-less
  `parquet_write_column` infers the type family from `values` and uses the default units.
- **Timezone query**: new public `parquet_get_column_time_info(reader, name, unit, timezone)`
  — `unit` (optional `intent(out)` integer, one of the `parquet_unit_*` constants) and
  `timezone` (optional allocatable `intent(out)` character; empty = naive) for a
  TIME/TIMESTAMP column; aborts (with the reader-filename suffix) for any other column type.
  Separate from `parquet_get_metadata` on purpose: that generic serves *user-defined*
  key/value metadata, and overloading it to also answer schema-level questions would be
  ambiguous exactly as raised in review.
- **qc/filter**: deferred (decision 7); `parquet_validate_maml` rejects `qc:` on these types
  for now with a clear "not yet supported" message rather than silently ignoring it.

## 7. Deferred / future

`LIST`/`MAP` element types and `parquet_duration` (for `INTERVAL`/Arrow `duration`, and for
timestamp arithmetic `ts - ts`) slot into this same module later; nothing here blocks them.
qc/filter support per decision 7. Two-part (SOFA-style) JD accessors if real64 JD precision
ever proves insufficient.

---

# Implementation plan

Ordered so each step compiles and is testable before the next; `fpm test run_tester --
temporal` (new suite) for fast iteration, `tools/coverage.sh temporal` for gap checks.
**After every step: stop, report, and wait for review before starting the next step** (per
review instruction).
- **MAML/schema**: new `data_type` tokens `date`, `time[unit]`, `timestamp[unit(,utc)]`;
  bare `time`/`timestamp` default to micros. `parquet_validate_maml` learns the tokens and
  rejects malformed unit suffixes (and any suffix on `date`). Schema-less
  `parquet_write_column` infers the type family from `values` and uses the default units.
- **Timezone query**: new public `parquet_get_column_time_info(reader, name, unit, timezone)`
  — `unit` (optional `intent(out)` integer, one of the `parquet_unit_*` constants) and
  `timezone` (optional allocatable `intent(out)` character; empty = naive) for a
  TIME/TIMESTAMP column; aborts (with the reader-filename suffix) for any other column type.
  Separate from `parquet_get_metadata` on purpose: that generic serves *user-defined*
  key/value metadata, and overloading it to also answer schema-level questions would be
  ambiguous exactly as raised in review.
- **qc/filter**: deferred (decision 7); `parquet_validate_maml` rejects `qc:` on these types
  for now with a clear "not yet supported" message rather than silently ignoring it.

## 7. Deferred / future

`LIST`/`MAP` element types and `parquet_duration` (for `INTERVAL`/Arrow `duration`, and for
timestamp arithmetic `ts - ts`) slot into this same module later; nothing here blocks them.
qc/filter support per decision 7. Two-part (SOFA-style) JD accessors if real64 JD precision
ever proves insufficient.

---

# Implementation plan

Ordered so each step compiles and is testable before the next; `fpm test run_tester --
temporal` (new suite) for fast iteration, `tools/coverage.sh temporal` for gap checks.

## Step 1 — the module itself

- **`src/parquet_temporal.f90`** (new): everything in "Final design" §2–§5. Full FORD
  doc-comments (leading `!>`, trailing `!!` on every dummy/result and every binding), 132-col
  limit, `implicit none`, one `EP` prefix for all `error stop` messages.
- **`fpm.toml`**: nothing needed (auto-discovered source).

## Step 2 — unit tests for the module (no I/O yet)

- **`test/test_temporal.f90`** (new): suite `temporal` registered in `test/run_tester.f90`'s
  `testsuites` array (parallel-safe — pure value types). Coverage:
  - civil round-trips: epoch day 0, leap years (2000, 1900, 2024), month/century boundaries,
    negative years, ±extreme representable dates;
  - MJD/JD anchors: MJD 0 = 1858-11-17; J2000: JD 2451545.0 = 2000-01-01T12:00:00;
    Unix epoch = MJD 40587;
  - `parse`/`to_string` round-trips incl. fraction trimming, 'T'/space, 'Z'; `success=`
    behavior (failure → `.false.` + element null);
  - null semantics: default-init is null, propagation (`get_date`/`set(date,time)`),
    interop accessors on null, `is_null` masks over arrays;
  - operators incl. cross-epoch ordering (negative seconds + ns normalization);
  - `set_unix`/`to_unix` all four units, `exact=.false.` flooring (esp. pre-epoch values);
  - constructor interfaces.
- **Arrow cross-validation**: test-only `parquet_debug_civil_from_days` /
  `parquet_debug_days_from_civil` hooks in `parquet_wrapper.cpp` (Arrow's vendored Hinnant
  `date.h`); locally-declared `bind(C)` interfaces in `test_temporal.f90`; sweep a broad day
  range + targeted cases, assert exact agreement.
- **Error scenarios** (abort paths can't be tested in-process): `test/error_scenarios.f90`
  scenarios + `test/test_errors.f90` wrappers + `tools/run_error_scenarios.sh` entries for:
  invalid civil fields, `parse` failure without `success=`, value access on null, comparison
  with null, `to_unix` precision-loss and overflow.

## Step 3 — C++ boundary

- **`src/parquet_wrapper.cpp`**: `parquet_read_{date,time,timestamp}_column`,
  `_column_chunk`, `parquet_append_{...}_column`, `_column_chunk` (matrix/vector handled via
  the existing `col_size` convention); logical-type validation; unit normalization (read) and
  exact-conversion checks (write); date64/`time32[s]`/`timestamp[s]`/INT96 read support;
  `valid_in`/`valid_out` wired to the existing validity machinery; row-group-scoped access
  reusing `get_row_group_chunk_array` (and the row/element-mode plumbing) — no new
  whole-column reads.
- **`src/parquet_bindings.f90`**: matching interfaces.
- Extend the strict-type rejection: reading a DATE/TIME/TIMESTAMP column through any
  int/real/logical/string entry point aborts with the standard mismatch diagnostic (and
  vice versa).

## Step 4 — Fortran integration

- **`src/parquet.f90`**: new specifics added to the six affected generics (§6 list) + interface
  blocks with full doc-comments; `parquet_get_column_time_info` public + README API-overview
  index entry; the generics' own leading `!>` doc-comments updated to mention the new types
  (the FORD-738 workaround: generic-member arg docs don't render, so the generic's prose must
  cover them).
- **`src/parquet_write.f90` / `src/parquet_read.f90`**: specifics bridging typed arrays ↔ flat
  buffers via the elemental interop accessors; `error stop` messages use
  `writer_context_suffix`/`reader_filename_suffix`.
- **`src/parquet_metadata*.f90`**: MAML `data_type` tokens (`date`, `time[...]`,
  `timestamp[...(,utc)]`), validation incl. rejecting `qc:` on these types and any unit
  suffix on `date`; schema-building (`add_field`) acceptance; default units.
- `parquet_close_reader(print_stat=.true.)`/`parquet_close_writer` `parquet_type` display for
  the new Arrow types (cosmetic).

## Step 5 — integration tests

- Extend `test/test_writing.f90`/`test/test_reading.f90` (+ chunk paths): round-trips for all
  three types — scalar and `(col_size, nrows)` vector columns, whole-column and chunked
  write/read, row mode (both `row_index` kinds) and element mode, nulls (incl. all-null and
  none-null columns), every unit variant `[ms]`/`[us]`/`[ns]` + defaults, `,utc` flag,
  `parquet_get_column_time_info`, multi-row-group files, struct-nested date/time leaf (dotted
  path), row filter on *another* column while reading a timestamp column.
  Vector fixtures follow the "first element is the extreme case" rule from CLAUDE.md.
- Foreign-file fixtures via a debug hook (`parquet_debug_write_datetime_fixture(path,
  variant)`, pattern: `parquet_debug_write_string_view_fixture`): date64, `time32[s]`,
  `timestamp[s]`, INT96, tz-string timestamps — read back and verified against known values.
- Error scenarios: write-side unit-loss abort (stderr check), date64 non-exact value, strict
  type-mismatch both directions, `protected_cols` vs null element, malformed MAML tokens.

## Step 6 — docs

- **`doc/pages/date-time.md`** (new guide page; no top-level body heading, FORD-native links)
  + `ordered_subpage`/bullet in `doc/pages/index.md`: types, null semantics (incl. the
  "never aborts on null, unlike other types" difference), units/defaults, `,utc`, MJD/JD
  precision notes, examples.
- **`doc/pages/supported-data-types.md`**: main table rows + coverage table; note the strict
  read rule and the qc/filter deferral.
- **`doc/pages/building-schema-in-code.md` / `maml-format.md` / `writing.md` / `reading.md`**:
  token syntax + `parquet_get_column_time_info` where relevant.
- **README.md**: API-overview index entries (types + `parquet_get_column_time_info`),
  features line, limitations note (INTERVAL/duration unsupported; qc/filter deferral).
- Run `tools/check_doc_anchors.py`; `ford docs.md` clean-run check.
- CHANGELOG: none (paused pre-release). CONTRIBUTING: no workflow change expected.

## Step 7 — verification

- `fpm clean --all` + full `fpm test` with CI-equivalent `FPM_FFLAGS` (never run the GitLab
  pipeline itself); `tools/coverage.sh temporal` + full `tools/coverage.sh` for gap review;
  round-trip verify via the actual read/write path (`/verify` flow).
- Work stays uncommitted on `main` for your review (per repo rule).
