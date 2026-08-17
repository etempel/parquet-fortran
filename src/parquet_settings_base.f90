!> The settings state that a module with **no C++ dependency** is allowed to read.
!!
!! **This module exists for exactly one reason: to keep `parquet_strings` linkable on its own.**
!! `parquet_strings` is documented as usable without the Arrow/Parquet C++ stack -- a project that
!! wants compact string storage and nothing else can depend on it alone. It needs two answers from
!! the library's settings, though: whether solicited output is suppressed (`verbosity`), and the
!! per-column thread cap. Taking those from `parquet_settings` directly cost it that independence,
!! because `parquet_settings` imports `parquet_bindings` in order to mirror the C++-side knobs -- so
!! linking a program whose only import was `use parquet_strings` pulled in the whole of
!! `parquet_wrapper.cpp` and, with it, Arrow. Nothing failed at compile time; the link failed with
!! the entire C++ surface undefined, which reads as a build misconfiguration rather than a
!! dependency defect.
!!
!! So the STATE lives here, in a leaf module that imports nothing but `iso_fortran_env`, and the
!! public API over it stays in `parquet_settings`, which re-exports the two readers below.
!!
!! **This is a split, not a mirror, and the difference is the whole point.** There is one copy of
!! each value: `parquet_settings`' setters write these variables directly. A pushed second copy in
!! `parquet_strings` would have been the other way to break the dependency and was rejected -- two
!! writers for one value is exactly what `CLAUDE.md`'s "Do not add a second way to set the same
!! thing" forbids, and a missed push site would leave the two disagreeing with nothing to report it.
!!
!! **It also holds the library's one copy of the automatic THREAD-COUNT rule**, for the same
!! reason and by the same argument. `parquet_sorting` and `parquet_random` both have to answer "how
!! many threads should this use when the caller said nothing", CLAUDE.md's auto-threading note names
!! a further copy of that rule as the mistake to avoid, and `parquet_random` is a pure-Fortran
!! counter-based generator that must not acquire a C++ dependency to ask it. `parquet_sorting`
!! reaches `parquet_bindings`, so it cannot be the home either. This module can be, and the
!! behaviour is unchanged: `pf_sort_threads` still reads `cfg_sort_threads`, still in one place, and
!! now delegates the OpenMP half here.
!!
!! **Rules for anything added here.** A variable belongs in this module only if a
!! no-C++-dependency module has to read it; everything else stays in `parquet_settings`, which is
!! where a reader looks for the settings API. The same test governs a procedure: it belongs here
!! only if it is a rule two such modules must share. Whatever is added must keep this module a
!! leaf -- it may import intrinsic modules and `omp_lib` (under `#ifdef _OPENMP`, as
!! `parquet_strings` already does) and nothing else, ever.
!! `check_parquet_strings_stays_leaf` (`tools/check_source_conventions.py`) enforces this by walking
!! the `use` graph, because the failure it guards is invisible until someone tries the standalone
!! build.
module parquet_settings_base
    use iso_fortran_env, only: int64
    implicit none
    private
    !
    public :: parquet_output_is_suppressed
    public :: parquet_get_string_threads
    public :: parquet_get_random_threads
    public :: parquet_get_random_parallel_min_elements
    public :: parquet_auto_thread_count
    public :: parquet_nested_team_unsafe
    public :: verb_normal, verb_silent, verb_errors_only
    public :: cfg_verbosity, cfg_string_threads
    public :: cfg_random_threads, cfg_random_parallel_min_elements
    !
    !> Verbosity levels, ordered so that a `>=` test answers "is this class of output off?".
    integer, parameter :: verb_normal = 0      !! everything prints (the factory default).
    integer, parameter :: verb_silent = 1      !! informational and solicited output goes quiet.
    integer, parameter :: verb_errors_only = 2 !! warnings go quiet too; only errors survive.
    !
    !> How much the library prints. Written by `parquet_set_verbosity`/`parquet_reset_settings`
    !! (parquet_settings.f90); read by the three emit channels there and by
    !! parquet_output_is_suppressed below, which is what the solicited printers ask.
    integer, save :: cfg_verbosity = verb_normal
    !> Cap on the threads one `parquet_string_column` bulk operation may use internally. `0` means
    !! "auto" (as many as OpenMP offers). Written by `parquet_set_string_threads`
    !! (parquet_settings.f90); read only by `parquet_string_threads` (src/parquet_strings.f90),
    !! deliberately, for the same reason cfg_sort_threads has a single reader: one question asked in
    !! one place cannot give two answers (feature_risks.md Risk-40).
    integer, save :: cfg_string_threads = 0
    !> Cap on the threads one bulk `pf_random_permutation`/`pf_random_subset` call may use. `0`
    !! means "auto" (as many as OpenMP offers). Written by `parquet_set_random_threads`
    !! (parquet_settings.f90); read only by `random_threads` (src/parquet_random.f90), for the same
    !! single-reader reason as cfg_sort_threads and cfg_string_threads (feature_risks.md Risk-40).
    integer, save :: cfg_random_threads = 0
    !> Fewest elements a thread must be given before a bulk permutation opens a team at all.
    !!
    !! **A work floor, not a chunk size**, and it exists because threading a small permutation is
    !! monotonically harmful rather than merely useless: machine B measured `m = 10` going from
    !! 0.0021 ms on one core to 0.0050 on sixteen, and parallel efficiency at 1->16 threads of 99 %
    !! at `10**6`, 95 % at `10**4`, **69 % at 1000 and 18 % at 100**. The default sits where that
    !! curve turns. Read only by `random_threads` (src/parquet_random.f90).
    integer(int64), save :: cfg_random_parallel_min_elements = 1000_int64
    !
contains
    !
    !> Whether SOLICITED output -- something the caller explicitly asked to be printed, such as
    !! `%print_stat` or `parquet_string_column%print` -- should stay quiet. Distinct from the emit
    !! channels, which govern the library's own unsolicited messages.
    logical function parquet_output_is_suppressed() result(quiet)
        quiet = cfg_verbosity >= verb_silent
    end function parquet_output_is_suppressed
    !
    !> The configured cap on threads inside one string-column bulk operation; `0` means automatic.
    integer function parquet_get_string_threads() result(n)
        n = cfg_string_threads
    end function parquet_get_string_threads
    !
    !> The configured cap on threads inside one bulk permutation/subset; `0` means automatic.
    integer function parquet_get_random_threads() result(n)
        n = cfg_random_threads
    end function parquet_get_random_threads
    !
    !> The configured work floor, in elements per thread, for a bulk permutation/subset.
    integer(int64) function parquet_get_random_parallel_min_elements() result(n)
        n = cfg_random_parallel_min_elements
    end function parquet_get_random_parallel_min_elements
    !
    !> **The library's one copy of the automatic thread rule**: how many threads an operation that
    !! was given no explicit `threads=` should use right now, under a caller-supplied cap.
    !!
    !! `omp_get_max_threads()` when the caller is not inside an OpenMP parallel region, and 1 when
    !! they are, because a nested region is the caller's business. This picks a DEFAULT and refuses
    !! nothing: an explicit `threads=` is still honoured inside a region, which is the distinction
    !! CLAUDE.md's "Auto-threading: `omp_in_parallel()` picks a DEFAULT" note draws. Without it, T
    !! OpenMP threads would each ask for T more, and T*T oversubscription is slower than not
    !! threading at all.
    !!
    !! **The predicate is `omp_get_level`, NOT `omp_in_parallel`, and the difference is not
    !! pedantic.** `omp_in_parallel` answers "is the enclosing region ACTIVE", i.e. does its team
    !! have more than one thread. It is therefore `.false.` inside a region that exists but runs on
    !! one thread -- `!$omp parallel if(cond)` with `cond` false, or any region at all under
    !! `OMP_NUM_THREADS=1`. That is still a nested region, and the rule above still applies to it,
    !! so the old spelling let every such caller open a full team one level down. It also deadlocks
    !! libgomp; see `parquet_nested_team_unsafe` below and feature_risks.md Risk-104.
    !!
    !! **The cap only ever LOWERS the answer, and never overrides the parallel-region rule** -- a
    !! caller who capped sorting at 8 said nothing about what should happen inside someone else's
    !! parallel region, and lifting the serial answer back to 8 there is exactly the T*T
    !! oversubscription the rule exists to prevent. `cap <= 0` means "no cap", which is how every
    !! `cfg_*_threads` knob spells its automatic default.
    !!
    !! **Never report more threads than can actually run.** `omp_get_max_threads` answers an ICV,
    !! which is what the environment ASKED for; `omp_get_num_procs` answers what this thread's
    !! affinity mask allows. They differ whenever the initial thread was bound before `main`.
    integer function parquet_auto_thread_count(cap) result(n)
#ifdef _OPENMP
        use omp_lib, only: omp_get_max_threads, omp_get_level, omp_get_num_procs
#endif
        integer, intent(in) :: cap                  !! caller's domain cap; `<= 0` means no cap
        n = 1
#ifdef _OPENMP
        if (omp_get_level() == 0) n = omp_get_max_threads()
#endif
        if (cap > 0 .and. cap < n) n = cap
#ifdef _OPENMP
        if (n > omp_get_num_procs()) n = max(1, omp_get_num_procs())
#endif
    end function parquet_auto_thread_count
    !
    !> Whether opening a thread team here would build the shape libgomp deadlocks on.
    !!
    !! `.true.` when a region encloses this call (`omp_get_level() > 0`) but its team has one thread
    !! (`omp_get_active_level() == 0`). libgomp (gfortran 15.2, macOS arm64) intermittently hangs
    !! when a team is opened from inside such a region: main thread and workers all park on one
    !! libgomp mutex that nobody holds. Reduced to twenty lines with no library code --
    !! `!$omp parallel num_threads(1)` / `!$omp single` / `!$omp parallel do num_threads(3)` -- it
    !! hangs 7 runs in 8. Bisected, every clause is load-bearing: `master` instead of `single` never
    !! hangs, an enclosing team of 2 never hangs, and no environment setting fixes it
    !! (`GOMP_SPINCOUNT=0` only moves 7/8 to 2/8). The library's own code is standard-conforming;
    !! this predicate is what stops it building the shape. See feature_risks.md Risk-104.
    !!
    !! **This is what clamps an EXPLICIT `threads=`**, which `parquet_auto_thread_count`'s rule
    !! deliberately does not touch. The two are different questions and the narrowness is the point:
    !! an enclosing team of two or more never reproduced the hang in any configuration tried, so a
    !! genuinely parallel caller's explicit request is still honoured in full.
    logical function parquet_nested_team_unsafe() result(unsafe)
#ifdef _OPENMP
        use omp_lib, only: omp_get_level, omp_get_active_level
#endif
        unsafe = .false.
#ifdef _OPENMP
        unsafe = omp_get_level() > 0 .and. omp_get_active_level() == 0
#endif
    end function parquet_nested_team_unsafe
    !
end module parquet_settings_base ! GCOVR_EXCL_LINE
