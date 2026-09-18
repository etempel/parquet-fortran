# Fortran gotchas

Language and compiler traps, filed by the compiler that exhibits them. A rule that binds regardless
of compiler is in the general group even when one compiler exposed it. Most of these are invisible
to the compiler used daily and visible to one run rarely: keep the source portable, and build under
more than one compiler. Where `tools/check_source_conventions.py` enforces a rule, the entry names
the check. Source-layout rules (submodule ordering, interface placement, continuation limits) are
in `code-style.md`.

## Contents

- [General Fortran & language gotchas](#general-fortran--language-gotchas)
- [gfortran-specific gotchas](#gfortran-specific-gotchas)
- [ifx-specific gotchas](#ifx-specific-gotchas)
- [flang-specific gotchas](#flang-specific-gotchas)
- [nagfor-specific gotchas](#nagfor-specific-gotchas)

## General Fortran & language gotchas

- **`STOP "message"` exits with status 0; only `ERROR STOP` exits non-zero.** A fatal path written
  as `stop` tells a shell, a CI job or a scheduler that the run succeeded.
- **Never write a function returning `character(len=:), allocatable`**; use a subroutine with an
  allocatable `character` `intent(out)`/`intent(inout)` argument. gfortran's hidden length variable
  is not thread-local (GCC PR113797, PR97977) and corrupts memory under concurrent calls; ifx
  rejects the automatic-length variant. Applies to test code too (test-drive dispatches tests with
  `!$omp parallel do`). For `x = obj%get(x)` shapes use one `intent(inout)` argument. Plain
  non-`character` allocatable results are unaffected.
- **`x = func()` from an unallocated allocatable result leaves `x` ALLOCATED** (empty). An API
  cannot signal "absent" that way; use a flag or sentinel (`parquet_strings`' `allow_null` returns
  `""` guarded by `is_null()`).
- **Never blank a deferred-length allocatable character ARRAY with `arr = ""`**; it reallocates
  every element to length zero (gfortran hides it, ifx shows blank strings then heap corruption).
  Loop element by element. A scalar `suffix = ""` and an array assignment whose RHS already has the
  right length are fine.
- **Build every array of a matched set of `character` array arguments at the same length.**
  Different constructor lengths for partner arguments corrupt memory under gfortran (SIGTRAP inside
  the callee, survives `-fcheck=all`); nagfor runs it correctly. Prefer a shape with no `character`
  array dummies at all.
- **`transfer(source, mold, size)` into a longer target leaves the trailing bytes undefined**;
  assign normally to blank-pad. **A per-element `transfer` into a `character(len=1)` payload
  allocates a temporary each call**; use sequence association instead (`pack_character_bytes`,
  `src/parquet_strings.f90`): a contiguous `character(len=w)` array passed to a
  `character(len=1), intent(in) :: src(*)` dummy, rank flattened for free, and `len_trim` kept on
  the element view. The public dummy stays plain assumed-shape and reaches it through an assumed-size
  worker (`build_from_elements`), never through a `contiguous` dummy (gfortran and ifx sections).
- **`.and.` does not short-circuit.** `size(a) == size(b) .and. all(a == b)` reads out of bounds,
  `lo < 0 .and. hi > huge(hi) + lo` overflows, and `cheap .and. expensive() == 0` may evaluate the
  call (ifx does, gfortran does not). Nest the tests whenever the second operand indexes, computes,
  or is more than a comparison. The mismatch can be the NORMAL case. `fpm test --profile nagdeb`
  is the sharpest detector (`-C=array` names the array and both extents); under threads the same
  defect can present as a bare SIGSEGV, so re-run with `OMP_NUM_THREADS=1` first.
- **A component and its parent cannot both be actual arguments of one call** (`t%cache` and
  `t%cache%reader`; F2018 15.5.2.13, undiagnosed by gfortran and ifx). Design around it: make the
  inner one an optional argument where absent means "reach it through the parent"
  (`table_open_reader_with_transform`), or resolve a local `type(...), pointer` with `target` on
  both dummies. A pointer to a component of a non-`target` dummy is not permitted.
- **An apparently redundant `x*1.0` on an actual argument may be an aliasing dodge** (the same
  variable passed to an `intent(out)` and an `intent(in)` dummy); check before deleting, and
  replace with an explicit local plus comment.
- **Passing an UNALLOCATED allocatable to an `optional` dummy makes it ABSENT** (F2018 15.5.2.12).
  Used deliberately by `row_validity` and the `mat_*` masks; any procedure returning such an array
  documents that `allocated()` is part of its contract.
- **`intent(out)` on a finalizable type resets every component; do not convert one to
  `intent(inout)` without auditing every component** — an open/init body that sets a handful of
  fields relies on the reset for the rest.
- **`class(t), intent(out)` is expensive per element** (runtime default-initialisation), while
  `class` + `intent(inout)` and `type` + `intent(out)` are free. A setter that assigns every
  component on every path may take `intent(inout)`; one with a caught-failure path that returns
  without assigning keeps `intent(out)`. A `pure` procedure cannot have a polymorphic `intent(out)`
  dummy at all.
- **Intrinsic assignment to or from a FINALIZABLE type runs the finalizer, twice per loop
  iteration** in `dest(i) = obj%make(i)`. Set the components directly, or drop a finalizer a
  non-owning handle does not need. Re-derive the finalizable types with `grep 'final ::' src/*.f90`.
- **An array-section assignment whose two sides are the SAME array costs a heap temporary per
  iteration** (the compiler cannot prove no overlap). Same array: scalar loop; different arrays:
  section (one `memcpy`).
- **`floor`/`ceiling`/`nint`/`int` without `kind=` return a DEFAULT integer and wrap a large
  value**, even when assigned to an `int64` or `real64` (the truncation is inside the intrinsic).
  Give `kind=int64` to every one whose argument can exceed `2**31`, and give every refusal scenario
  a control that must succeed (`random_poisson_int32_overflow`).
- **A wrapping overflow is still undefined behaviour the optimiser may reason from elsewhere**: an
  expression measured to wrap let ifx delete an `if (s < 0)` branch two functions away. Compute
  without overflowing (`sub64`/`add64`, `src/parquet_random.f90`); a deliberately overflowing site
  must be guarded by a comparison against an overflow-free implementation (`test_agreement_int`,
  `test/test_random.f90`).
- **A `huge()` sentinel standing for "unbounded" is a NUMBER: centring, scaling or differencing it
  OVERFLOWS.** The overflow delivers the infinity that was meant, so gfortran and ifx look correct
  and nagfor's `-ieee=stop` aborts on the documented call; a quiet build is no evidence. Under
  nagfor even a power that shrinks it overflows (`huge()**(-0.5)` aborts inside the runtime's
  `**`), so a closed form at an infinite bound is written out, not evaluated at `huge()`. Produce
  the infinity with `ieee_value` instead, behind a guard whose threshold cannot overflow in ANY
  evaluation order: form it from a clamped operand (`BOUNDMAX*min(sc, ONE)`, `bound_is_absent`).
  Nesting the threshold inside the sign test that makes it safe (`huge() + ctr` under `ctr < 0`,
  `huge()*scl` under `scl < 1`) is not enough, because an optimiser may form it before the test
  (next entry); `tn_standardise` and `tn_width` (`src/parquet_random.f90`) are still written the
  nested way. A caller's true `+/-Infinity` needs none of it, so the two spellings diverge unless
  a test asserts they draw the same values (`test_normal_trunc_unbounded_forms`).
- **A guard does not keep an optimiser from forming what it guards: a short guarded arithmetic
  branch is compiled without a branch -- if-converted, hoisted, or vectorised under a mask -- the
  result formed first and selected afterwards.** ifx does it at `-O2` under the default
  `-fp-model=fast` and under `-fp-model=precise` alike: `if (value == 0) then; v = 0; else;
  v = width*value; end if` became `mulsd` then a `cmpeqsd`/`andnpd` mask, raising IEEE_INVALID for
  an infinite `width` while answering zero, and a loop dividing only where two values shared a
  sign became `divpd` in every lane then an `andpd` mask, raising IEEE_DIVIDE_BY_ZERO. nagfor's
  `--profile release` does it too: `BOUNDMAX*sc` was hoisted above the `sc < ONE` test meant to
  keep it finite. The answer is right and the flag ends a program under nagfor's `-ieee=stop`.
  Make the OPERANDS harmless where the result is not taken, then select or compare
  (`merge(width, 1.0, width <= huge(width))` in `interp_1d_flat`,
  `src/parquet_interpolate_1d.f90`; the PCHIP mean in `interp_pchip_slopes`,
  `src/parquet_interpolate_core.f90`; `BOUNDMAX*min(sc, ONE)` in `bound_is_absent`,
  `src/parquet_prima_common.f90`), and read the flags around the call in a test
  (`check_golden_rows`, `test/test_interpolate.f90`). Every `-O0` profile (`debug`, `nag`,
  `nagdeb`) cannot show it.
- **A list-directed `read(text, *, iostat=ios) n` is not a strict parse**: `"5 6"` yields 5 with
  `iostat == 0`. Parse caller-supplied text by hand (trim, one optional sign, digits and nothing
  else), then convert (`env_int64`, `src/parquet_settings.f90`; `settings_env_two_numbers` scenario).
- **The leading zero of a `G0.d` rendering below one is PROCESSOR-DEPENDENT** (F2018 13.7.2.3.2
  makes it optional): gfortran and nagfor write `0.500000`, flang and ifx write `.500000`, and
  every one of them drops it for `F0.d`. Put it back in any text a user reads or a test asserts
  (`stat_real_text`, `src/parquet_tables_access.f90`; `parquet_qc_format_real`,
  `src/parquet_write_numeric.f90`; `real_text`, `src/parquet_tables_display.f90` — keep the three
  in step). `G0.d`'s EXPONENT form differs too (`0.100000E+07` under nagfor, `.100000E+7` under
  gfortran and flang), so keep an asserted value inside its fixed-point range. Audit:
  `grep -rn "[gG]0\.[0-9]" src/*.f90`.
- **`ATAN2(0.0, 0.0)` is prohibited** (F2018 16.9.16); gfortran/ifx/flang return 0, **nagfor returns
  NaN and raises `IEEE_INVALID`** (fatal under its default `-ieee=stop`). The reachable input rarely
  looks like "both zero" (a pole, a zero-length vector). `grep -n "atan2" src/*.f90` is the audit;
  guard every site whose arguments can both vanish.
- **`min`/`max`/`minval`/`maxval` and the two-`if` clamp compile to `minsd`/`maxsd`, which raise
  `IEEE_INVALID` on a quiet NaN** (so does an ordered comparison, `<`, `>`, `<=` or `>=`, under
  gfortran at `-O0` and `-O2` alike, which compiles it to `comisd`; `==` and `/=` never raise it,
  and ifx raises it on no comparison), and the answer is wrong even where it does not trap. Screen
  the NaN first, as its own statement; refuse a NaN input at a validating entry point.
  `any(x <= 0)` does NOT reject a NaN; `.not. all(x > 0)` does. Census of the instruction per
  procedure (the audit; a source grep cannot see an if-converted clamp):

```bash
for o in build/<hash>/parquet-fortran/src_*.o; do objdump -d "$o" | awk -v O="$o" \
  '/^[0-9a-f]+ <.*>:/ { s=$2; gsub(/[<>:]/,"",s) } /(minsd|maxsd|minpd|maxpd)/ { print s }'; \
done | sort | uniq -c | sort -rn
```

  Every procedure it names screens the NaN or takes only validated values; the healpix
  `pure elemental` conversions are the documented exception. **A running fold on `<`/`>`
  (`if (v > acc) acc = v`: a cumulative or windowed extremum, a running argmax) DROPS a NaN** meant
  to propagate; give it an explicit `if (v /= v)` arm ahead of the comparison (`cum_scan`,
  `src/parquet_stats_relate.f90`; `test_cumulative_null_rule`). Arithmetic folds need none.
- **`sign(1.0, x)` is not a portable negative-zero test** (processor-dependent for a zero `B`; ifx
  answers `+1`). Use `(x == 0.0_real64) .and. transfer(x, 0_int64) < 0_int64` or
  `ieee_is_negative`; build the value at runtime, and have a one-bit fixture assert its own
  precondition first.
- **Test for NaN with `ieee_is_nan` in cold code and `x /= x` on a hot path** (per-element,
  per-row, per-comparison, `elemental`), with a comment saying why. **A validation pass over every
  value of a table or column is per-element, not cold**, however rarely its procedure runs: screen a
  NaN there by `x /= x`, and a NaN and an infinity together by the exponent bits
  (`interp_all_finite`, `src/parquet_interpolate_core.f90`). `ieee_is_nan` and `ieee_is_finite` are
  runtime calls under ifx and nagfor and inlined under gfortran, so a gfortran measurement cannot
  see the difference. Both forms are quiet on a quiet NaN. `-Wcompare-reals` hits on exact-equality checks
  (`value == anint(value)`) are accepted, not epsilon-ised.
- **`-ffast-math`/`-Ofast` fold every `ieee_is_nan` guard to `.false.`**; never build with them.
- **An IEEE halting mode set inside a helper is undone when the helper returns, and a flag read
  inside one reads quiet** (F2018 17.3: the halting modes are restored on return from every
  procedure but `ieee_set_halting_mode`/`ieee_set_status`, and a flag signalling on entry is
  quietened until return). nagfor does both, so a `hold_traps()`/`read_traps()` pair leaves
  `-ieee=stop` armed and reports nothing raised; flang 22 does the flag half; gfortran does
  neither. Save, hold off, clear, read
  and restore in the test's own body; only an inquiry may live in a helper (`traps_can_be_held`,
  `test/test_prima.f90`). `ieee_get_status`/`ieee_set_status` would do it in two lines, but flang
  22 does not implement them.
- **A DIFFERENCE of two nearly-equal doubles carries ~8 digits, the rest is the compiler.** Form
  small quantities directly (exact rational, `(a²-b²)/(a+b)`, half-angle sine); a test asserting
  such a value below ~1e-8 relative pins one toolchain's rounding. An external reference computed
  the cancelling way cannot certify a cancellation-free implementation; use a high-precision oracle
  (`test_max_pixrad_high_precision`).
- **A RUNTIME `10.0_real64 ** (-k)` is not the correctly rounded decimal power** (a variable
  exponent folds nothing): measured up to 282 ulp out under nagfor and 7 under gfortran on one
  machine, so two compilers do not even agree on which value is under test. Never build a test
  point, a tolerance reference or a golden input that way; use an exact literal, a generated
  table, or `scale(1.0_real64, -k)` where a power of two will do (exact, F2018 16.9.171;
  `test_probit_round_trips`). A LITERAL exponent is folded correctly and is not in this class.
- **Two loops written to mirror each other do not round alike, so a bit-exact identity between two
  procedures is delivered by CALLING ONE of them**, never by writing the two bodies the same way.
  Whether `acc = acc + a*b` contracts into an FMA depends on how many uses `a*b` has — gfortran
  fuses a single-use product from `-O1` upwards and refuses one whose value also feeds a multiply —
  so a kernel accumulating `s2 = s2 + dd` beside `s3 = s3 + dd*d` rounds differently from its twin
  accumulating only `sxy = sxy + dx*dy`, on a target that has an FMA. Operand grouping diverges the
  same way with no FMA anywhere: `w*(d*d)` is not `(w*d)*d`. Both are silent, both give a plausible
  number one ulp out, and `-ffp-contract=off` is the diagnosis rather than the fix. A fixture whose
  centring is EXACT proves nothing about such an identity, which is how a suite stays green over
  one (`stats_pair_moments`' `diagonal` fork, `src/parquet_stats_core.f90`;
  `test_cov_identity_survives_inexact_centring`).
- **An OpenMP `reduction(+:...)` over reals is not bit-reproducible** across calls or thread counts;
  measure the tolerance floor by calling twice, or use an ordered/compensated sum.
- **Nested OpenMP needs both `omp_set_nested(.true.)` and `omp_set_max_active_levels(2)`**
  (libgomp keys on one ICV, NAG on the other), called once before any region; NAG aborts if called
  inside an active region — guard with `omp_get_level() > 0`, not `omp_in_parallel()`.
- **Every reference to an `omp_*` procedure, `use omp_lib` or `omp_lock_kind` sits inside
  `#ifdef _OPENMP` with a serial arm** (`tid = 0`, `avail = 1`); `!$omp` directives need no guard.
  The guard opens above the `!>` doc block, and a caller of an all-OpenMP procedure is guarded to
  match. Enforced by `check_openmp_calls_are_guarded` (`src/`, `test/`); `app/`/`bench/` programs
  need serial shims for the `omp_*` functions they call.
- **A kind number is not a byte count** (nagfor numbers kinds sequentially: `logical(kind=4)` is
  64 bits there). Take every kind from `iso_fortran_env`/`iso_c_binding`/`kind(.true.)`; assert a
  required width at compile time with a `1/merge(1, 0, storage_size(...) == 32)` parameter; an
  `interface` body needs the kind in its `import`.
- **`EXECUTE_COMMAND_LINE` statuses differ per compiler**: nagfor returns the raw `wait()` status
  (7 arrives as 1792); flang reports any non-zero exit through `cmdstat`. Only `exitstat == 0` /
  `/= 0` is portable; decide "the command never ran" from evidence the command leaves (a missing
  redirect file), never from `cmdstat`.
- **A derived type or procedure cannot share its module's name**; two generic specifics are not
  distinguished by `intent` or `allocatable`, only by type/kind/rank and argument count; an
  `optional` dummy never helps disambiguate and can remove the margin that did. Test an intended
  specific set in a throwaway module before designing an API around it.
- **Give every non-`pointer`, non-`allocatable` component of a new derived type a default
  initialiser**: `-finit-*` and `-nan` fill declared variables only, never an `allocate` payload.
- **A procedure handing back a pointer into its own dummy (`h%col => self`) requires `target` on
  the actual at every call site** (F2018 15.5.2.4). gfortran, ifx and flang run a violation
  happily; only nagfor `-C=dangling` sees it. `check_view_call_sites_declare_target` derives the
  handle-returning bindings from every bare `=> self` in `src/` and scans `src/`, `test/`, `app/`,
  `bench/`. A pointer into what `self%<pointer component>` points at (`parquet_table%col`) is not
  in this class — `grep -rnE "=> *self( |$)" src/*.f90` is the discriminator.
- **A zero-length case reaches storage that was never allocated**: `C_LOC` on a zero-sized array is
  non-conforming (F2018 18.2.3.6), and a whole-array assignment to, or a dummy association with, an
  unallocated allocatable is too, even when nothing would be copied. Only nagfor `-C=array`/
  `-C=pointer` sees it; the fix is at the allocator (`allocate_empty_storage`), not a guard per site.
- **`-ftrapv` traps only the wrapping arm of `src/parquet_random.f90`** (the one ifx ships); a
  non-aborting `-ftrapv` build is not evidence a site is safe (dead results are optimised away
  before instrumentation). `tools/check_random_ubsan.sh` (machine B only) drives UBSan over both
  arms and is the instrument.

## gfortran-specific gotchas

- **Minimum gfortran is 13** (older versions miscompile an optional allocatable-`character`
  argument); never refactor correct source to accommodate an older compiler.
- **Never give a FINALIZABLE type to OpenMP's `private()`; declare it in a `block` inside the loop
  body.** The `private` copy is not reliably default-initialised, so the first finalization frees
  garbage (dies in `malloc`; reproducible with `OMP_NUM_THREADS=1`). The handle types
  (`parquet_table_row`, `parquet_table_col`, `parquet_string`) have no finalizer and may be
  `private()`. ifx forbids the `block` form for a type with allocatable components — see below.
- **`-128_int8` trips the range check**; build the high bit with `ibset(0_int8, 7)`. An
  array-constructor implied-do index has no implicit type under `implicit none`; list the elements.
- **`ieee_is_finite` over an array SIGNALS `IEEE_INVALID` on a quiet NaN once vectorised**: `where`
  from `-O2` and `count` at `-O3` compile to `cmpnlepd`, while the scalar form is a quiet `ucomisd`
  and `ieee_is_nan` vectorises to the quiet `cmpunordpd`. The default and `debug` profiles cannot
  show it; `--profile release` can. A screen over values that may legitimately be NaN, on a path
  that carries on rather than aborting, reads the exponent bits instead (`is_finite_quiet`,
  `src/parquet_optimize_support.f90`; `test_de_nan_region`). To name the line: append
  `-g -ffpe-trap=invalid` to `FPM_FFLAGS` in its own `FPM_BUILD_DIR`, then run the one test under
  `lldb -b -o "process handle SIGFPE --stop true --pass false" -o run -o bt`.
- **`-finit-*` does not reach an `allocate` payload**, so a clean `-finit-*` run does not rule out
  uninitialised memory (`-finit-real=zero`, not `=0`).
- **`intent(out)`'s implicit reset has one confirmed counterexample** on a scalar `logical`
  component of a finalizable type (gfortran 13/14); add an explicit reset as the first executable
  statement (`table%detached = .false.`).
- **A `pointer`-typed intermediate component defeats `-fcheck=bounds`'s trust in an unallocated
  LHS** (`out%cache%x = self%cache%x` reports a spurious bound mismatch), and the same shape with a
  deferred-length character array segfaults on CI's gfortran while running clean locally. Use an
  explicit `allocate(character(len=len(src)) :: dst(size(src)))` plus an element-wise loop.
- **A derived-type component section (`data(:)%x`) reaches an assumed-shape dummy as a contiguous
  copy made at the call**, where ifx passes it strided. A test meant to reach a callee's strided
  path passes a stride section (`x(1::2)`), which stays strided under both
  (`test_write_strided_arguments`); `--profile debug`'s `-fcheck=array-temps` names each copying line.
- **An assumed-shape array passed to a `contiguous` dummy is copied on EVERY call, contiguous or
  not** (unless the actual has TARGET); ifx, and both compilers at an assumed-size dummy, check at
  run time and copy only a strided one. Code that needs contiguous storage from another procedure's
  assumed-shape dummy passes it to an assumed-size dummy and copies a strided one itself
  (`build_from_elements`, `src/parquet_strings.f90`).
- **A disassociated array POINTER is not an absent optional**, although F2018 15.5.2.12 says it
  is: passed to an optional assumed-shape dummy it segfaults at the call at `-O0`, or arrives
  `present` at `-O2`; ifx treats it as absent. Forward an absent optional dummy instead
  (`write_strided_file`, `test/test_writing.f90`).
- **Never pass a POINTER-VALUED FUNCTION RESULT straight to a procedure dummy**; bind it to a
  local pointer and pass that. `call sub(pick(name))` where `pick` returns
  `procedure(iface), pointer` is rejected outright by ifx (`error #6637: When a dummy argument is
  a function, the corresponding actual argument must also be a function`) and miscompiled by
  nagfor, which generates invalid C from it; gfortran accepts it, and ifx accepts it while the
  function is HOST-associated and rejects it once the same function is use-associated — so a
  refactor that moves the selector into a module is what surfaces it. The same binding rule covers
  a written `ASSOCIATE` name over such a result under nagfor.
- **A procedure POINTER passed to a generic whose specifics differ by a dummy procedure against a
  `character` dummy resolves to the CHARACTER specific under gfortran 15**, which then reads an
  empty string (`parquet_grouping%agg` and `%add_agg`: `grp%agg(name, colf, out)` with `colf` a
  procedure pointer reaches the token form); nagfor and flang resolve it to the procedure specific. Pass the
  procedure itself; a generic with no `character` competitor (`%apply`) resolves a pointer fine.
- **A TYPE-BOUND generic holding an `elemental` specific beside a non-elemental one resolves a
  reference consistent with both to the specific LISTED FIRST under gfortran 15.2**, and every module
  re-exporting the type reverses that order, so no listing reaches the non-elemental specific from
  both `use parquet_interpolate` and `use parquet`; F2018 15.5.5.2 names the non-elemental one, as
  ifx and a plain `interface` generic under gfortran do. The answers can agree to the bit while the
  wrong specific runs. Give such a generic non-elemental specifics distinguished by rank alone
  (`pf_interp_1d`'s `eval_rank0` to `eval_rank7`), and pin the resolution through a caller that
  reaches the type across a re-export, by an observable the specifics differ in
  (`interpolate_eval_array_before_init`, `test/error_scenarios.f90`).
- **A dummy PROCEDURE argument in an abbreviated `module procedure` body has an IMPLICIT interface
  under gfortran 15** (`-Werror=implicit-interface` at every call of it), although the spec
  declares it `procedure(<abstract interface>)`; nagfor accepts the body. Restate that body's
  interface in full (`module subroutine name(...)` with every dummy declared and `!!`-tagged), as
  the two procedure-form `%apply` bodies in `src/parquet_tables_group.f90` do.
- **gfortran 15.2 ICEs (bare `Segmentation fault`) on a `pure module procedure` passing a `class`
  dummy's `type` component to a `class` dummy beside an allocatable `intent(out)` argument**
  (`mm_get_method`, the `%keys` forms in `src/parquet_index_multi.f90`). Only one ICE is reported
  per compilation, naming the last such call. Workaround: repeat the callee's body over the
  `type`-dummy helpers, with a comment.

## ifx-specific gotchas

- **ifx forbids a type with ALLOCATABLE COMPONENTS in a `block` lexically inside a parallel region**
  (privatization scaffolding `for_alloc_private`/`mold_ctor` segfaults every thread at `-O1`+, only
  for a type from a separately compiled module) and is happy with `private()` — the opposite of
  gfortran. A type in that class (`parquet_reader`, `parquet_writer`, `parquet_schema`,
  `parquet_column`, `parquet_string_column`) uses **a shared array allocated before the region, one
  slot per thread, indexed by `omp_get_thread_num() + 1`** (`materialize_marked_parallel`).
  `parquet_table` stays block-local-safe only because it has no allocatable component
  (`prefetch_threads caps the parallel prefetch`, `test/test_settings.f90`). Diagnose with
  `nm <object> | grep -E "for_alloc_private|mold_ctor"` before blaming the compiler; no scaffolding
  means the crashing binary is stale.
- **`fpm test --profile debug` under ifx needs `export FOR_DISABLE_STACK_TRACE=1`**: `-check all`
  emits `warning (406)` per array temporary, `-traceback` prints a traceback per warning, and
  libifcore's traceback code is not thread safe under test-drive's parallel dispatch (crash, abort
  in the allocator, or a hang re-reading `/proc/<pid>/maps`). `--flag "-check noarg_temp_created"`
  does not override the profile.
- **Never pass a FUNCTION RESULT, an ARRAY CONSTRUCTOR or an ARRAY EXPRESSION to an EXPLICIT-SHAPE
  array dummy**; assign to a named local (or a `parameter` when constant) and pass that, or
  `warning (406)` fires on every call. A contiguous array SECTION needs no local. Per call is per
  DRAW in a sampler: one such actual inside a `pf_random_*`/`sph_*` chain put two million lines
  through a `--profile debug` run. `check_no_array_temporary_at_an_explicit_shape_dummy` reports
  the three shapes it can prove. **A genuinely non-contiguous actual reaching an assumed-size or explicit-shape dummy is
  copied onto the STACK**, which dies with SIGSEGV once the copy passes the stack limit: library
  code forwarding a caller's assumed-shape array to such a dummy tests `is_contiguous` and copies
  into an allocatable itself (`parquet_write_int32_column`, `test_write_strided_arguments`; an
  `intent(out)` one is read into the copy and assigned back, `test_read_strided_values`). **A
  `contiguous` assumed-shape dummy makes the same stack copy with no `warning (406)`**, so a zero
  count clears nothing: no dummy a caller's array reaches is declared `contiguous`. The
  warning names the CALLEE, so check the actual at the outermost call.
  An I/O list section of an allocatable component warns; an implied-do over it is silent
  (`col_print`). Triage: `fpm test --profile debug 2>&1 | grep -c 'warning (406)'`.
- **`pack` and an EXPLICIT-SHAPE FUNCTION RESULT are stack temporaries under ifx**, the result's in
  the caller, with no `warning (406)`, since neither is an argument temporary: a large masked table
  or query array dies with SIGSEGV once it passes the stack limit. `ulimit -s` does not govern an
  OpenMP worker's stack (`OMP_STACKSIZE`), so a region meets it first, with the process limit
  unlimited. Count and copy by hand, and declare an array result `allocatable`
  (`interp_1d_build`, `interp_1d_oneshot_array`); a regression test runs the large case inside a
  region, where the ordinary `fpm test` reaches a worker's stack
  (`test_large_tables_on_worker_threads`, `test/test_interpolate_omp.f90`).
- **Two threads reaching `ERROR STOP` at once leave the exit status nondeterministic, including 0**
  (`exit()` from two threads is undefined); gfortran is deterministic. One abort inside or after a
  region is safe, so the fix is a `critical` around the whole fatal body (`api-conventions.md`).
- **A `critical` hammered by a team as wide as the machine slows by orders of magnitude under ifx
  once anything else shares the processors**: the Intel runtime's default lock for `critical`
  (`KMP_LOCK_KIND=queuing`) is FIFO with spinning waiters, so it is handed to a thread that is not
  running while the rest spin; gfortran's unfair mutex does not. A test hammering one lock requests
  a bounded team with `num_threads` and raises its rounds per thread, never
  `omp_get_max_threads()` (`hammer_team`, `test/test_index_omp.f90`). Confirm the diagnosis by
  rerunning under `KMP_LOCK_KIND=futex`.
- **An automatic-length `character` result whose length is a specification expression over
  host-associated variables is an ICE** at `-O1`+ (clean at `-O0`), when called from a sibling
  contained procedure inside a submodule's `module procedure`. Use the subroutine shape the
  character-function rule already forces (`tok_text`, `src/parquet_read_filter.f90`).
- **A default structure constructor `type_name()` is rejected (`error #6053`) when a component's
  own type has private components in another module.** Reset the type's own components explicitly
  and clear the private-dependent one separately (`table_drop_column`).
- **A non-polymorphic `type(T)` actual passed to a `class(T)` dummy in another compilation unit
  makes ifx build the class descriptor in the caller's prologue on every call**, one record per
  allocatable component, into `.bss` (shared by every thread). The typed accessor tiers in
  `columns-tables.md` are the fix; gfortran gains from the same shape.
- **Vectorised transcendentals are not quiet on a NaN** (`__svml_sin2` raises `IEEE_INVALID` in its
  argument reduction). A procedure meant to propagate a NaN quietly returns every NaN argument
  before any transcendental (`pf_angdist_deg`). Reproduce against the built library, not a copy of
  the formula, which vectorises differently.
- **The same transcendental expression can differ by 1–2 ulp between a bulk loop and a scalar
  evaluation at `-O0`** (identical at `-O2`); assert a re-derived value at a tolerance above the
  round-trip error, never at zero (`test_count_within_sky`, 1e-9 degrees).
- **ifx's default `-fp-model=fast` rewrites a division whose denominator it can see is CONSTANT as
  a multiply by the reciprocal**, which is not the correctly rounded quotient — so a test that
  re-derives a reference that way tests the rewrite instead of the library, against a bit-equality
  assertion. Fold such a reference as a `parameter`: a constant expression goes through the front
  end and is correctly rounded under every compiler and `-fp-model`
  (`test_safe_div_matches_division`, `test_normal_scores_every_method_token`). A flagless
  `fpm test` selects this; `-prec-div` or `-fp-model=precise` turns it off.
  **Do not apply the same fix to an FMA-contractable shape.** `rm + 1 - 2*a` contracts at run time,
  so the library and a matching run-time expression agree while an unfused folded constant does
  not; that reference stays run-time arithmetic.
- **The same reciprocal substitution also fires on a RUN-TIME divisor that divides more than once
  in a procedure**, so the constant-denominator entry above is the narrow case rather than the
  rule. `mux = px(1) / w_sum` (four divisions by `w_sum` in `stats_pair_moments`) and
  `mu = acc%vsum / acc%w_sum` (two in `stats_engine`) are the same division of the same
  bit-identical operands and came out **one ulp apart**, because the substitution is worth it in
  one procedure and not the other. Nothing in either source says so.
- **Writing `a / b` instead of `a * (1/b)` does not survive the rewrite -- the SOURCE FORM is
  not the fix, `volatile` on the denominator is.** `spatial_scan_axis` divides the axis dot
  product by the squared axis length so that a point ON the axis recovers its parameter exactly
  and `q - tp*v` cancels to zero; ifx's default `-fp-model=fast` hoists `1/dd` out of the
  candidate loop anyway and puts back the very formulation the kernel was changed away from
  (`test_axis_zero_radius_finds_the_axis` reports 19 of 21 on-axis points, missing `x = 7` and
  `x = 14`, whose parameters `0.35` and `0.7` are not representable). Declaring the divisor
  `volatile` removes the rewrite's premise rather than arguing with it: the value may change
  between references, so no reciprocal can be hoisted. Cost is an L1 load per candidate, below
  this harness's noise end to end.
- **The profile matters more than the optimisation level, and the DEFAULT profile is the
  dangerous one.** fpm passes ifx NO flags at all without `--profile` (`-fpp -fPIC -qopenmp
  -free`), so ifx's own defaults -- `-O2 -fp-model=fast` -- apply; `--profile debug` passes
  `-O0` and `--profile release` passes `-fp-model=precise`, and BOTH of those are value-safe.
  So a flagless `fpm test` is the arm that catches this class, and reproducing a failure under
  `--profile debug` will quietly fail to reproduce it. Check `build/compile_commands.json`, or
  `fpm build --verbose | grep 'ifx -c'`, before concluding a flag is present.
- **FMA contraction is a SECOND, independent way to lose the same exactness, and no source form
  closes it.** Under `-xHost` (FMA available) `wx = qx - tp * vx` contracts to a fused
  multiply-add, which does NOT round the product first, so the residue is nonzero for every
  non-representable parameter and the same test reports 5 of 21. Parenthesising the product
  helps only with `-assume protect_parens`; ifx does not honour parentheses against contraction
  by default. No profile this project builds passes `-xHost`, so this is a limit on what a
  consumer may add, not a defect here -- and the general lesson is that an exact-cancellation
  guarantee is a property of the FP MODEL, not of the arithmetic as written.
- **`-fp-model=fast` also folds an algebraic identity out of an expression, which can turn a test's
  own PRECONDITION into a lie.** `total = a + b + c` followed by `total - a == 0.0` — the check
  that two tiny weights vanished into a huge one — is rewritten to `b + c`, so the fixture reports
  itself unusable when it is exactly what the test needs
  (`test_absorbing_weight_keeps_positions_ordered`). It is a Heisenbug: printing `total` first
  hides it. Declare the variable `volatile` so the stored value is read back rather than the
  expression that produced it, which is the question such a precondition is asking. It rearranges
  a weighted sum the same way: `(1 - s)*a + s*b` at `s = 1` answered `0.09999999999999998` for
  `a = 0.7`, `b = 0.1`. A weight of exactly zero stays exact under any rearrangement and a weight of
  exactly one does not, so an interpolant that must return an end value exactly answers that end
  directly (`interp_2d_eval`, `src/parquet_interpolate_2d.f90`).
- **A `pure` procedure can be INLINED at one call site and left out-of-line at another — at `-O0` —
  so "both routes call the same kernel" does not mean "both routes round alike."** ifx compiled the
  out-of-line `stats_block_moments` vectorised (`addpd`/`mulpd`, two lanes combined at the end) and
  an inlined copy of it scalar, which grouped one block's additions differently and moved the last
  bit. Confirm with `objdump -dr <object>` and the relocation list for the caller: a missing
  `R_X86_64_PLT32` to the callee means it was inlined. The lesson is general — **a documented
  bit-for-bit identity between two public procedures has to be delivered by CALLING one from the
  other, never by writing two bodies to match** (`cov_f64` hands a diagonal pair to
  `variance_f64`).
- **ifx turns flush-to-zero AND denormals-are-zero on at `-O1` and above**; gfortran and nagfor
  leave gradual underflow in force. It is a process-wide MXCSR setting made by the main program, so
  a library procedure receives a subnormal argument already collapsed to zero and cannot recover
  it — `pf_probit` of the smallest subnormal answers `-Infinity` rather than about `-38.47`. A test
  over subnormal inputs therefore reports the BUILD, not the kernel: detect it with
  `ieee_get_underflow_mode` and skip, naming the flag (`subnormals_are_flushed`,
  `test/test_utils.f90`). `-no-ftz` or `-fp-model=precise` restores it.
- **WHICH IEEE exception a site raises is not portable, so assert THAT one is raised, never which.**
  The same division by a subnormal raises overflow under gfortran and divide-by-zero under ifx,
  whose flush-to-zero turned the divisor into zero first (`BOBYQA runs the configuration whose
  geometry step overflows`, `test/test_prima.f90`). A test that must show a site is still reached
  reads the flags with halting held off and asserts `any(raised)`.
- **An ABSENT optional allocatable dummy passed into an OpenMP region and on to an optional dummy
  segfaults at `-O0 -check all`** (`SIGSEGV` at the call; clean on gfortran and ifx release). Never
  pass an optional array dummy into a parallel region: fill a local that always exists and
  `move_alloc` it into the optional afterwards (`mm_probe_hit_buffer`).
- **A `pure` guard-only subroutine's CALL is deleted at `-O0`** (debug profile only; `-O2` and
  gfortran abort). Write guard-only subroutines impure (`api-conventions.md`). **Where the caller
  is `pure` too, impure is not an option and the guard has to produce something**: the two-sided
  `int32` key check was a `pure` guard called from `pure module subroutine map_keys_r1_i32`, so
  `fpm test --profile debug run_tester_errors -- errors` answered -3000000000 as 1294967296 with
  exit status 0 while the same source aborted correctly without `-check all`. Merged into
  `ix_narrow_keys_1`, which writes the narrowed list, it cannot be elided. A silent-truncation
  symptom with no diagnostic anywhere is what this looks like from the outside, so treat a
  guard-only `pure` procedure as a defect on sight rather than waiting for the scenario to fail.

## flang-specific gotchas

flang builds here are serial only and `--profile release` does not link (`build.md`).

- **A rejected format is reported through `iostat` AND leaves partial text in the buffer**
  (`ios = 1005`; gfortran/ifx/nagfor leave it empty), so never decide "was this rendered?" from
  emptiness (`rendered_ok`, `src/parquet_utils.f90`, keys on the overflow asterisk;
  `test_to_str_bad_fmt`). A literal bad format is a compile error under flang; a reproducer
  must pass it through a `character(len=*)` variable.
- **A `character` temporary built inside a loop is not reclaimed until the procedure returns**, so
  a long loop of `call sub("%" // what // ": ...")` exhausts the stack (SIGSEGV in the callee's
  prologue, `EXC_BAD_ACCESS (code=2)` at a guard-page address, unwindable backtrace). Diagnose with
  `ulimit -s 65520`, scale the fixture, print the counter. **Hoist**: build each message once above
  the loop into a `character(len=:), allocatable`; never raise the limit (`-fno-stack-arrays` does
  not help).
- **The runtime `SUM` of a `real` array compensates its rounding, and `-O0` calls it**, with or
  without `dim=`/`mask=`: `1 + 4*1e-16` sums to `1.0000000000000004` at `-O0` and to `1.0` at
  `-O1` and above, where the intrinsic is inlined as a loop in index order, as gfortran and nagfor
  do at every level. The default profile passes flang no `-O`, so a plain `fpm test` is the arm
  that differs, and an algorithm whose path turns on the last bit follows another path there. The
  vendored PRIMA engines sum through `parquet_prima_linalg`'s ordered `sum` (its header, deviation
  9; `check_prima_sums_are_the_ordered_sum`), and a test objective feeding a path-sensitive
  reproducer loops (`brown_almost_linear`, `test/test_optimize_support.f90`).
- **An INTERNAL procedure passed as an actual argument to a `procedure(...)` dummy SIGSEGVs
  before the callee runs** (flang 22.1.8 on arm64 macOS, with or without a host reference; a
  module procedure passed the same way runs, and gfortran, ifx and nagfor run both forms). Every
  callback in `src/`, `test/`, `app/` and `bench/`, and every guide example, is a module procedure
  with its context in module variables, or a type-bound procedure of an object passed as a
  `class(...)` dummy with its context in components. Nothing enforces it beyond the serial flang
  `fpm test` on a macOS machine (`build.md`).

## nagfor-specific gotchas

`--profile release` is the only nagfor configuration with optimisation on (`nag`, `nagdeb`,
`nagundef` are all `-O0`); it is the only one that can see a codegen defect, so run it as its own
check and read a hang there as a possible miscompilation (`sample <pid>` names the procedure).
Running and triaging NAG builds: the `/nag-build` skill (`.claude/skills/nag-build.md`).

- **nagfor unmasks the IEEE traps by default (`-ieee=stop`) for the whole process.** `anint(NaN)`
  and `int(NaN)` trap (test `ieee_is_nan` first, as its own statement); `arrow::compute::MinMax`
  raises `FE_INVALID` benignly on every non-empty float array. Mask the traps around a foreign call
  known to raise with `feholdexcept` + `feclearexcept` + `fesetenv` (never `feupdateenv`), scoped
  to the one call. `-ieee=full` is a diagnosis, not the fix (`print_stat_all_types` and
  `write_float_nan_to_int32` scenarios).
- **A test FIXTURE's own arithmetic trips those traps too.** Build a NaN or an Infinity with
  `ieee_value`, never as `0/0` or an overflowing quotient, and keep a reference expression a test
  computes beside the procedure under test (`a(i)/b(i)`) inside the finite range — it raises exactly
  what the procedure does. The abort takes the whole runner, so one fixture hides every later suite
  in it, and test-drive's concurrent output leaves the message beside whichever test was printing:
  re-run the suite at `OMP_NUM_THREADS=1`, then
  `lldb -b -o "process handle SIGFPE --stop true --pass false" -o run -o bt -- <binary> <suite>`
  names the frame.
- **An array-valued ordered comparison against a NaN raises invalid** (`count(a > 0.0)`,
  `sum(a, mask=a > 0.0)`, vectorised path only — a five-element reproducer does not show it); the
  scalar loop, `count(a /= a)` and `ieee_is_nan` never do. Reported only as a line at program exit.
  Find it with `-ieee=stop -gline` appended to `FPM_FFLAGS` in its own `FPM_BUILD_DIR`, running
  the application rather than the test binary. `NaN > 0` and `NaN <= 0` are both false, so a
  `<= 0` guard does not skip a NaN.
- **`-nan` (in the `nagfor` feature) poisons every undefined `real`, including the unwritten tail
  of an `intent(out)` array**, as a signalling NaN. A test may not read past what the callee wrote:
  use a canary outside the section passed in, or assert that returned entries agree. A second pass
  over a multi-buffer fill uses the SAME cap as the filling pass (`grep` for a loop bound of
  `min(m, size(<one buffer>))`). A raised flag is a line at `STOP` nobody correlates; turn it into
  an assertion: save the flag, clear, call, read, restore `saved .or. raised`
  (`test_sky_distances_are_degrees`, guarded by `ieee_support_flag`). A test building an extreme
  fixture (subnormals by design) saves the flag on entry and restores on exit. `-nan` reaches no
  `INTEGER`/`LOGICAL` and no `allocate` payload.
- **`LEADZ` on `integer(int64)` is wrong by two at `-O1`+** (correct at `-O0`; `int32`, `trailz`,
  `popcnt` correct): a descending `63 - leadz(w)` walk never terminates, a watermark scan is
  silently high. Banned outright across `src/`, `test/`, `app/`, `bench/`, `tools/`
  (`check_no_leadz`); use `trailz` and keep the last index reached, walking ascending and filling
  from the far end when order mattered.
- **`len(s(d+1:))` is wrong when the lower bound is an EXPRESSION** (correct for a variable or
  literal), and at `-O2` the wrong length folds an unrelated `dot == len(s)` comparison. Never form
  a possibly-empty substring with an expression lower bound in a procedure that also compares
  against `len()`: test each span only when non-empty and give it an explicit upper bound
  (`significand_ok`, `src/parquet_utils.f90`; keep its shape). Audit:
  `grep -rnE "\([a-zA-Z_][a-zA-Z0-9_]* *[+-] *[0-9a-zA-Z_]+ *:\)" src/*.f90`.
- **The most-negative int64 CONSTANT combined with a runtime value is mis-evaluated** (guards wrong
  in both directions, `INT64_MIN/scale` with the wrong sign, a common `ieor(., 2**63)` cancelled
  from both sides of a relational). Form bounds from `huge`; a local copy is not a fix (the
  optimiser propagates the constant back at `-O2`+); write the unsigned comparison out as
  `(a < b) .neqv. ((a < 0) .neqv. (b < 0))`. A most-negative constant whose `ieor` result is stored
  rather than compared (`SORT_SIGN_BIT`) is safe. Printing the expression shows the right value
  (`temporal_ts_to_unix_overflow_negative` scenario).
- **An abbreviated `module procedure` body is miscompiled when the FUNCTION RESULT is shaped by an
  assumed-shape dummy that is not the first argument** (`yq(size(xq))` with `xq` third, or
  `character(len=size(xq))`): the dummies arrive with wrong sizes and garbage addresses, so the
  body aborts in a guard on valid input, segfaults, or dies with
  `Cannot allocate array temporary - out of memory`. A result sized from the first dummy or a
  scalar one, or an allocatable result, compiles correctly -- except under `-C=undefined`, which
  cannot compile the abbreviated body of ANY function with an allocatable result (array or scalar)
  and an array dummy other than an assumed-size one, referenced or not (`use of undeclared
  identifier 'xq_'`). So restate the full interface in the body (`interp_1d_oneshot_array` and the
  `%eval` array specifics, `src/parquet_interpolate_1d.f90`). Nothing but a nagfor `fpm test` sees
  the miscompilation; `check_no_shape_nagfor_undefined_cannot_compile` enforces the
  `-C=undefined` half.
- **Keep finalizers deallocate-only; never assign a scalar component in one.** Under
  `-C=undefined` an implicitly invoked finalizer indexes a null definedness map and segfaults on
  the first scalar store, with no diagnostic. Do not re-add the finalizers removed from
  `parquet_string` and `parquet_string_column`; the remaining ones release C++ handles and locks.
- **`-C=undefined` cannot compile two more shapes, and one of either anywhere in `src/`, `test/`,
  `app/` or `bench/` stops `tools/check_nag_undefined.sh`** (`fpm test` compiles every file). A
  FUNCTION whose result is a procedure pointer panics the compiler (`No mapinfo.sym?`): hand the
  pointer back through a subroutine's `pointer, intent(out)` dummy (`pick`,
  `bench/benchmark_optimize.f90`). An unsaved LOCAL array -- explicit-shape, automatic, allocatable
  or `block`-local -- of a type with a non-allocatable, non-pointer component that is finalizable,
  at any depth (`parquet_table_writer`), generates C that does not compile at `end`: hold the array
  in an allocatable component of a local scalar (`sink_set`, `test/test_openmp.f90`). A dummy, a
  `save` or module array, a scalar, and an array of a type finalizable only by its own `final` or
  its parent's compile. `check_no_shape_nagfor_undefined_cannot_compile` enforces both.
- **A procedure-local array PARAMETER passed as an actual argument inside an OpenMP parallel region
  does not compile** (`-openmp`, every profile): the generated C names an undeclared
  `<module>_MP_<procedure>Param_<name>_`. An intrinsic reading it there (`sum(LIST)`) is fine.
  Declare a variable and assign it before the region (`test_region_fill_schedule`,
  `test/test_sphere_omp.f90`).
- **A written `ASSOCIATE` name whose selector is a pointer-valued function reference panics the
  compiler at `-O1`+** (`find_node_sym -- invalid tree`); bind the result to a local pointer and
  associate on that, for every such construct.
- **A parent-type component read through a `class(...)` pointer in a `select type`'s
  `class default` arm generates invalid C** (`no member named 'addr'`); write the concrete
  `type is (...)` arms out (`key_origin`, `src/parquet_toml.f90`; do not fold it back).
- **NAG's I/O runtime keeps ONE global unit table, not thread safe**: an `INQUIRE(FILE=)` or `OPEN`
  on one thread races a `CLOSE` on another (bare SIGSEGV in `strlen`, no traceback; passes under
  the checked profiles, fails under `nag`/`nagdeb`). Serialise every Fortran file operation in a
  concurrent region on ONE named `critical`, bare `INQUIRE`/`OPEN`/`CLOSE` included; put the file
  work in the region and the assertions after it.
- **nagfor's fpp cannot turn a `-D` macro into a string by any route** (`#X` is invalid, the
  gfortran `"&`/`&X"` splice is unrecognised, and fpm substitutes `{version}` only when it is the
  entire macro value). A build-time string needed on every compiler is generated into a source
  file, not preprocessed.
- **A compile-time fork needs a check that RUNS under every compiler selecting an arm** (NAG spells
  the preprocessor flag `-fpp`, has no `-U`, and `-u` means IMPLICIT NONE).
- **`-C=dangling`/`-C=calls` both crash or hang on a `target` attribute on an `optional`,
  assumed-size dummy receiving an ABSENT actual.** The twelve `write_<type>[_chunk]_flat` workers
  therefore declare `valid(*)` without `target` (one `logical` copy on the unmasked path); never
  give it back. A minimal reproducer for a codegen bug says what is sufficient to trigger it, never
  what is necessary — settle a flag question by building the real library.
- **`ERF` and `ERFC` do not propagate a quiet NaN: `erfc(NaN)` is 0, `erf(NaN)` is 1 and
  `erfc_scaled(NaN)` is 0** (7.2/arm64; gfortran and flang all return NaN, and C99 requires it).
  Both infinities are handled correctly by all three, so only the NaN case bites — and it bites
  silently, as a value in range: a survival function written straight onto `erfc` answers
  "probability 0" for an unknown quantile. **Screen the NaN with `x /= x` before the call** in any
  procedure over these three, as `pf_norm_cdf`/`pf_norm_sf` (`src/parquet_utils.f90`) do; `exp`,
  `log` and ordinary arithmetic propagate correctly and need no screen.
- **`SPACING` returns exactly 0 for some values near the bottom of the exponent range**, which
  F2018 16.9.180 forbids — it may never return less than `TINY`. Measured: `spacing(1e-292)` is
  `0` while `spacing(1e-291)` and `spacing(1e-293)` are ordinary subnormals; gfortran and flang
  both clamp to `TINY` below about `1e-292`. A test expressing a tolerance in ulp
  (`abs(got-want)/spacing(want)`) therefore divides by zero at one arbitrary point in the deep
  tail: every gap becomes `Infinity`, the budget becomes unmeetable, and `IEEE_DIVIDE_BY_ZERO`
  surfaces as one line at program exit attached to nothing. Floor it —
  `sp = spacing(x); if (sp <= 0) sp = tiny(x)` (`ulp_gap`, `test/test_utils.f90`).
- **A whole-array narrowing in a constant expression warns once per element**:
  `real(real32), parameter :: C(6) = real(C64, real32)` produces six "Loss of accuracy in
  double-real conversion" warnings, while the same conversion written per element inside an array
  constructor, or on a scalar, produces none. Write the element-wise form; the values are
  identical and the build stays warning-free (`ERFINV_C_R32`, `src/parquet_utils.f90`).
