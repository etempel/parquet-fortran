!> The settings an Arrow-free module owns: state, getter **and** setter.
!!
!! **The rule, in one sentence: a knob lives here when the Arrow-free module that reads it must be
!! able to re-export it.** `parquet_settings` keeps everything else, keeps the validation vocabulary
!! it alone uses, and keeps the C++ mirror. A future Arrow-free module applies that sentence to its
!! own knobs without revisiting anyone else's, which is what makes the arrangement extensible rather
!! than a list someone has to maintain.
!!
!! **Why the SETTER lives here too, which is the part that is easy to get wrong.** A user who
!! imports one module to get one capability must be able to configure that capability from the same
!! import -- `use parquet_sorting` has to offer the sorting knobs, or the only way to change them is
!! `use parquet_settings`, which imports `parquet_bindings` and drags the whole Arrow stack back in.
!! Re-exporting the getter alone would leave a user able to read a setting and not change it. Four
!! of these setters used to mirror their value to C++ immediately, which is exactly what pinned them
!! to `parquet_settings`; that push now happens at the point of USE instead
!! (`parquet_push_settings_to_cpp`, called when a reader or writer is opened, and by the C++ sort
!! engine's own dispatch), so no setter anywhere reaches C++ and every one was free to move.
!!
!! **This module began for one reason: to keep `parquet_strings` linkable on its own.**
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
!! **Rules for anything added here.** A knob belongs in this module only if an Arrow-free module
!! reads it and therefore has to re-export it; everything else stays in `parquet_settings`, which
!! remains where a reader looks for the settings API and where `parquet_print_settings`,
!! `parquet_reset_settings` and `parquet_settings_from_env` live. The same test governs a procedure:
!! it belongs here only if it is a rule two such modules must share. **The scope is narrow on
!! purpose** -- moving a knob no Arrow-free module reads buys nothing and costs a re-export to keep
!! in step. Whatever is added must keep this module a leaf -- it may import intrinsic modules and
!! `omp_lib` (under `#ifdef _OPENMP`, as
!! `parquet_strings` already does) and nothing else, ever.
!! `check_parquet_strings_stays_leaf` (`tools/check_source_conventions.py`) enforces this by walking
!! the `use` graph, because the failure it guards is invisible until someone tries the standalone
!! build.
module parquet_settings_base
    use iso_fortran_env, only: int32, int64, output_unit, error_unit
    implicit none
    private
    !
    public :: parquet_output_is_suppressed
    public :: parquet_get_string_threads
    public :: parquet_get_random_threads
    public :: parquet_get_random_parallel_min_elements
    public :: parquet_auto_thread_count
    public :: parquet_clamp_to_affinity
    public :: parquet_debug_set_affinity_procs, parquet_debug_reset_affinity_warning
    public :: parquet_nested_team_unsafe
    public :: verb_normal, verb_silent, verb_errors_only
    public :: verbosity_tokens, stream_tokens, stream_stdout, stream_stderr
    public :: cfg_verbosity, cfg_string_threads
    public :: cfg_random_threads, cfg_random_parallel_min_elements
    public :: cfg_sort_threads, cfg_sort_counting_path, cfg_sort_radix_path
    public :: cfg_sort_counting_bucket_limit, cfg_message_stream
    public :: cfg_spatial_threads, cfg_spatial_rebuild_warning, cfg_healpix_threads
    !
    ! ---- The settings API for the Arrow-free modules: state, getter AND setter ----
    public :: parquet_set_sort_threads, parquet_get_sort_threads
    public :: parquet_set_sort_radix_path, parquet_get_sort_radix_path
    public :: parquet_set_sort_counting_path, parquet_get_sort_counting_path
    public :: parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit
    public :: parquet_set_string_threads
    public :: parquet_set_random_threads, parquet_set_random_parallel_min_elements
    public :: parquet_set_spatial_threads, parquet_get_spatial_threads
    public :: parquet_set_healpix_threads, parquet_get_healpix_threads
    public :: parquet_set_spatial_rebuild_warning, parquet_get_spatial_rebuild_warning
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    public :: parquet_emit_info, parquet_emit_warning, parquet_emit_error_context
    !
    ! ---- Shared token-vocabulary helpers, used by the setters above and by
    ! ---- parquet_settings_from_env, which must accept exactly what the setters accept.
    public :: fold_ascii_lower, token_list
    !
    !> Verbosity levels, ordered so that a `>=` test answers "is this class of output off?".
    integer, parameter :: verb_normal = 0      !! everything prints (the factory default).
    integer, parameter :: verb_silent = 1      !! informational and solicited output goes quiet.
    integer, parameter :: verb_errors_only = 2 !! warnings go quiet too; only errors survive.
    !
    !> The accepted vocabulary of the two output knobs, in one place each.
    !!
    !! `parquet_settings_from_env` has to reject a bad token itself -- it cannot let the setter do
    !! it, because the setter's `error stop` cannot name the environment variable the value came
    !! from. Two validators means two chances to disagree about what is accepted, so both read these
    !! arrays and both build their "expected one of: ..." text with `token_list`.
    character(len=11), parameter :: verbosity_tokens(3) = [character(len=11) :: &
        "normal", "silent", "errors_only"]
    character(len=6), parameter :: stream_tokens(2) = [character(len=6) :: "stdout", "stderr"]
    !
    !> Where the library's own messages go. `message_stream` accepts exactly these two, because a
    !! Fortran unit number means nothing on the C++ side of the bind(C) boundary, where three of the
    !! library's warnings and one of its reports are printed -- see doc/pages/operating/settings.md.
    integer, parameter :: stream_stdout = 0
    integer, parameter :: stream_stderr = 1
    !
    !> What `0` resolves to for the counting path's bucket ceiling, and what the getter reports
    !! after a reset. MUST equal `kSortCountingBucketLimit`'s initialiser in
    !! src/parquet_wrapper.cpp, which is what applies before anything has been pushed
    !! (feature_risks.md Risk-42).
    integer(int64), parameter :: sort_counting_bucket_limit_builtin = 4194304_int64 !! 2**22 buckets.
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
    !> Default thread count for every sort that does not name one. `0` means "auto", which is what
    !! pf_sort_threads resolves against the OpenMP environment -- and that is the ONLY place this is
    !! read, deliberately, so a read-time `sort_by=` and a raw-array sort can never disagree about
    !! it (feature_risks.md Risk-40).
    integer, save :: cfg_sort_threads = 0
    !> Cap on the threads one bulk `pf_spatial_index` query may use internally. `0` means "auto" (as
    !! many as OpenMP offers). Written by `parquet_set_spatial_threads` below; read only by
    !! `spatial_threads` (src/parquet_spatial_bulk.f90), for the same single-reader reason as
    !! cfg_sort_threads and cfg_string_threads (feature_risks.md Risk-40).
    integer, save :: cfg_spatial_threads = 0
    !> Cap on the threads one bulk HEALPix conversion may use internally. `0` means "auto", which
    !! for this tier is NOT simply "as many as OpenMP offers": the automatic answer is additionally
    !! bounded by `hpx_max_auto_threads`, a measured ceiling of its own
    !! (src/parquet_healpix_bulk.f90). Written by `parquet_set_healpix_threads` below; read only by
    !! `hpx_threads` (same file), for the same single-reader reason as cfg_sort_threads and
    !! cfg_string_threads (feature_risks.md Risk-40).
    !!
    !! **This knob exists so that `parquet_set_threads` means what it says.** Before it, the bulk
    !! HEALPix forms were the one threading area that read no setting at all, so a program that
    !! asked the library for four threads still got up to `min(omp_get_max_threads(), 64)` of them
    !! here -- silently, which is the failure that convenience exists to prevent.
    integer, save :: cfg_healpix_threads = 0
    !> Whether a `pf_spatial_index` says so when a query radius disagrees badly enough with the one
    !! it was built for that it rebuilds itself.
    !!
    !! **On by default, because a silent rebuild is a silent performance cliff.** The whole point of
    !! the message is to tell the caller their `radius=` hint was wrong -- an index that quietly
    !! rebuilds inside a bulk sweep looks like a library that is slow rather than like a hint that
    !! needs fixing. This is the narrower control for a program that wants everything else the
    !! library prints; `verbosity="silent"` silences it too, as it does all solicited output.
    logical, save :: cfg_spatial_rebuild_warning = .true.
    !> Which stream the library's own messages go to. Read only by the emit channels below.
    integer, save :: cfg_message_stream = stream_stdout
    !> Whether the sort's integer counting fast path may be taken at all. Mirrored to C++ by
    !! `push_performance_settings` (parquet_settings.f90), which is where the boundary lives.
    logical, save :: cfg_sort_counting_path = .true.
    !> Whether the sort's single-key radix fast path may be taken at all.
    !!
    !! Deliberately NOT mirrored to C++, unlike its counting-path neighbour: the radix path exists
    !! only in the Fortran engine, so there is nothing on the other side of the `bind(C)` boundary
    !! for a mirror to govern. Adding one would be a global that no code reads, which is the shape
    !! `feature_risks.md` Risk-42 warns about from the other end.
    logical, save :: cfg_sort_radix_path = .true.
    !> Largest key value RANGE (not cardinality) the counting path will accept. `0` = built-in.
    integer(int64), save :: cfg_sort_counting_bucket_limit = 0
    !> Nonzero once the affinity-clamp warning has been claimed, so it is said once per PROCESS
    !! rather than once per operation or once per subsystem. Claimed by an `!$omp atomic capture`
    !! in `parquet_clamp_to_affinity`, which is reached only by the call that is about to print.
    !!
    !! **One claim across every area, deliberately.** The message names the subsystem that noticed,
    !! but what it asks the reader to fix is the process's `OMP_PLACES`/`OMP_PROC_BIND` setting --
    !! one line of advice, not one per subsystem. A run whose sort, prefetch and string rebuilds are
    !! all clamped has one environment problem, not three.
    !!
    !! **An integer rather than the obvious `logical`, and that is a portability constraint, not a
    !! preference.** The natural test-and-set is `seen = flag; flag = .true.` inside an
    !! `atomic capture`, and nagfor 7.2 rejects it -- *"Invalid form of expression in OpenMP ATOMIC
    !! assignment"* -- while gfortran accepts it, so the shape compiles on the machine you wrote it
    !! on and fails on the next one. A fetch-and-add over an integer is accepted by both, and the
    !! claim is then "the caller that displaced a zero". Do not simplify it back to a logical.
    !!
    !! **A genuine runtime counter, so it is one of the very few module variables here that is not
    !! a `cfg_*` knob.** `CLAUDE.md`'s NAG `-thread_safe` note enumerates them; keep that list
    !! current if another appears.
    integer(int64), save :: affinity_clamp_claims = 0_int64
    !> Overrides what `parquet_clamp_to_affinity` treats as this process's processor count.
    !! **Test-only**; `<= 0` restores the real `omp_get_num_procs()`.
    !!
    !! **Without it the clamp is untestable, and its tests are VACUOUS rather than absent** --
    !! which is worse, because they pass. Reproducing the clamp needs a process bound to fewer
    !! processors than `OMP_NUM_THREADS` asks for, and a process cannot bind itself after it has
    !! started. On an ordinary machine `omp_get_max_threads()` and `omp_get_num_procs()` agree,
    !! so every assertion about the clamp holds just as well with the clamp deleted: verified by
    !! mutation, where removing the clamp entirely left the invariant test passing.
    integer, save :: dbg_affinity_procs = 0
    !
    ! ---- Generic setters over both integer kinds ----
    !
    !> Sets the largest key value range the sort's counting fast path will accept. See
    !> parquet_set_sort_counting_bucket_limit_int64 for the full description.
    interface parquet_set_sort_counting_bucket_limit
        module procedure parquet_set_sort_counting_bucket_limit_int32
        module procedure parquet_set_sort_counting_bucket_limit_int64
    end interface parquet_set_sort_counting_bucket_limit
    !> Sets the work floor, in elements per thread, below which a bulk permutation stays
    !> serial. See parquet_set_random_parallel_min_elements_int64 for the full description.
    interface parquet_set_random_parallel_min_elements
        module procedure parquet_set_random_parallel_min_elements_int32
        module procedure parquet_set_random_parallel_min_elements_int64
    end interface parquet_set_random_parallel_min_elements
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
    integer function parquet_auto_thread_count(cap, area) result(n)
#ifdef _OPENMP
        use omp_lib, only: omp_get_max_threads, omp_get_level
#endif
        integer, intent(in) :: cap             !! caller's domain cap; `<= 0` means no cap
        character(len=*), intent(in) :: area   !! subsystem name, for the affinity-clamp warning
        n = 1
#ifdef _OPENMP
        if (omp_get_level() == 0) n = omp_get_max_threads()
#endif
        if (cap > 0 .and. cap < n) n = cap
        n = parquet_clamp_to_affinity(n, area)
    end function parquet_auto_thread_count
    !
    !> Lowers `n` to the number of processors this process's CPU affinity actually allows, and says
    !> so **once per process** when that clamp bites.
    !!
    !! **This is the ONE place the affinity clamp lives.** Every subsystem that resolves a thread
    !! count reaches it, directly or through `parquet_auto_thread_count`; the `area` strings passed
    !! at those call sites are the authoritative list, so re-derive it with
    !! `grep -rn parquet_clamp_to_affinity src/` rather than trusting an enumeration written here
    !! (one went stale, naming five sites as four while three more had appeared). It is here rather
    !! than in any of them because most live in tiers that cannot see each other, and a second copy
    !! of a rule like this is how two subsystems come to disagree about the same machine.
    !!
    !! **`omp_get_max_threads` is what the environment ASKED for; `omp_get_num_procs` is what the
    !! affinity mask allows.** They differ whenever the initial thread was bound before `main` --
    !! `OMP_PROC_BIND` with `OMP_PLACES=cores` binds it to one core, after which a team asked for 64
    !! lands on however few processors the mask holds and time-shares them, which is slower than not
    !! threading at all. Clamping is therefore a performance decision, never a correctness one: the
    !! answer is identical at every thread count.
    !!
    !! **The warning is the whole point, because nothing else reveals this.** No call fails, no
    !! result changes, and the only symptom is wall-clock. `area` names the subsystem that noticed
    !! so the reader knows which work was affected; the fix it recommends is the same either way.
    !!
    !! **Every resolver passes an `area`; there is no silent variant, deliberately.** The obvious
    !! refinement -- let a QUERY such as `pf_sort_threads()` or `parquet_string_threads()` clamp
    !! without printing, and leave the warning to the operation that follows -- was tried and is
    !! wrong here, because `pf_sort_threads()` is exactly how the sort's own AUTOMATIC path resolves
    !! its count. A silent query therefore hands `resolve_thread_count` an already-clamped number,
    !! whose own clamp is then a no-op, and the warning becomes unreachable for the one job it
    !! exists to catch: `OMP_NUM_THREADS=64` under `OMP_PLACES=cores`, no explicit `threads=`, and
    !! nothing said. Warning from the shared clamp covers both paths and every subsystem at once.
    integer function parquet_clamp_to_affinity(n, area) result(m)
#ifdef _OPENMP
        use omp_lib, only: omp_get_num_procs
#endif
        integer, intent(in) :: n             !! threads resolved before the clamp.
        character(len=*), intent(in) :: area !! subsystem this count belongs to, for the message.
        character(len=32) :: got, asked
        integer(int64) :: seen !! the claim count this call observed; 0 means this call won it.
        integer :: procs       !! processors the mask allows; 0 when this build has no OpenMP.
        !
        m = max(1, n)
        procs = 0
#ifdef _OPENMP
        procs = omp_get_num_procs()
#endif
        if (dbg_affinity_procs > 0) procs = dbg_affinity_procs
        if (procs < 1) return
        if (m <= procs) return
        m = max(1, procs)
        ! Fast path first: in a process whose affinity really is clamped every resolver reaches here
        ! on every operation, so this must cost one load once the line has been said. `atomic read`
        ! is a plain load on every real target.
        !$omp atomic read
        seen = affinity_clamp_claims
        if (seen /= 0_int64) return
        ! Suppression is checked BEFORE the claim, deliberately: a run that silenced its output must
        ! not consume the one warning, so a later call with output enabled still receives it.
        !
        ! **`parquet_output_is_suppressed` rather than `parquet_emit_warning`'s own threshold**, so
        ! this one message goes quiet at verbosity `"silent"` while an ordinary warning survives to
        ! `"errors_only"`. That is deliberate and is what `doc/pages/operating/performance.md`
        ! documents: this is advice about the caller's ENVIRONMENT, not a report of anything the
        ! library found wrong, so it belongs with the output a `"silent"` run is asking to be spared.
        if (parquet_output_is_suppressed()) return
        !$omp atomic capture
        seen = affinity_clamp_claims
        affinity_clamp_claims = affinity_clamp_claims + 1_int64
        !$omp end atomic
        if (seen /= 0_int64) return
        write (got, '(i0)') m
        ! **The count REQUESTED, not `omp_get_max_threads()`.** The clamp fires for an explicit
        ! `threads=` too, and reporting the environment's ICV there names a number the caller never
        ! asked for -- `threads=100` on a machine offering 8 used to print "although 8 were
        ! requested". `n` is what the resolver actually settled on before this clamp touched it, so
        ! it is right on both paths: the caller's own number when they named one, and the
        ! environment's (already narrowed by any cap) when they did not.
        write (asked, '(i0)') n
        call parquet_emit_warning(trim(area) // " is limited to " // trim(got) // &
            " thread(s) because this process's CPU affinity allows no more, although " // &
            trim(asked) // " were requested. This usually means OMP_PROC_BIND is set with " // &
            "OMP_PLACES=cores; OMP_PLACES=sockets avoids it.")
    end function parquet_clamp_to_affinity
    !
    !> Overrides the processor count `parquet_clamp_to_affinity` clamps to. **Test-only**;
    !! `<= 0` restores the real `omp_get_num_procs()`.
    !!
    !! **Public only because Fortran has no narrower visibility, and deliberately accepted** -- the
    !! same trade `parquet_debug_set_string_min_bytes` makes next door. `CLAUDE.md` asks for a C++
    !! hook in preference to a Fortran one, and this module is the Arrow-free leaf: it has no
    !! `bind(C)` surface at all, so there is no C++ side to put it on. It is called by no library
    !! code, and it is process-global saved state, so a suite that uses it must be excluded from
    !! test-drive's per-test parallelism.
    subroutine parquet_debug_set_affinity_procs(n)
        integer, intent(in) :: n !! processors to pretend the affinity mask allows; `<= 0` restores.
        dbg_affinity_procs = n
    end subroutine parquet_debug_set_affinity_procs
    !
    !> Clears the once-per-process claim on the affinity-clamp warning. **Test-only.**
    !!
    !! The warning is said once per process by design, which makes it a single-shot observable: a
    !! test that provokes it consumes it for every test after it in the same process. Resetting is
    !! what lets a negative control ("this configuration says nothing") run after a positive one and
    !! still mean something.
    subroutine parquet_debug_reset_affinity_warning()
        affinity_clamp_claims = 0_int64
    end subroutine parquet_debug_reset_affinity_warning
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
    !
    !> Sets the default thread count for every sort that does not pass `threads=` explicitly --
    !> `pf_sort`/`pf_argsort` and friends, a read-time `parquet_open_reader(..., sort_by=)`, and
    !> `parquet_table%sort_by`, which all share one engine and must share one default.
    !>
    !> Read per sort call, so it takes effect immediately. `0` restores automatic behaviour (as many
    !> threads as OpenMP offers). **A setting caps the automatic answer; it never overrides the rule
    !> that an unqualified sort inside an OpenMP parallel region runs serially** -- eight threads
    !> each asking for eight more is slower than not threading at all, and a caller who set this said
    !> nothing about nesting. An explicit `threads=` is still honoured everywhere, including there.
    subroutine parquet_set_sort_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_sort_threads: n must be >= 0 (0 means automatic)"
        cfg_sort_threads = n
    end subroutine parquet_set_sort_threads
    !
    !> Reports the sort thread cap, or 0 if sorting is left automatic. This is the raw setting, not
    !> the resolved count -- ask `pf_sort_threads()` for the number a sort would actually use here,
    !> which additionally accounts for the OpenMP environment and for being inside a parallel region.
    integer function parquet_get_sort_threads() result(n)

        n = cfg_sort_threads
    end function parquet_get_sort_threads
    !
    !> Sets the cap on how many threads one `parquet_string_column` bulk operation may use --
    !> a reindex, gather, compaction or materialization of a single column's packed payload.
    !>
    !> This is the **within-one-column** axis, and it is the only one of the thread caps that is:
    !> `parquet_set_table_threads` splits a table's work by COLUMN, this splits one column's work by
    !> ROW RANGE. The two are mutually exclusive in practice, because a string operation reached from
    !> inside the table's own parallel region stands down (see below), so setting both does not
    !> multiply.
    !>
    !> Read per operation, so it takes effect immediately. `0` restores automatic behaviour. The
    !> value is a **cap**: never more threads than OpenMP offers, so setting it above
    !> `OMP_NUM_THREADS` changes nothing, and `1` forces every string operation serial.
    !>
    !> **A cap never overrides the rule that a string operation inside an OpenMP parallel region runs
    !> serially.** A caller who capped string work at 8 said nothing about what should happen inside
    !> someone else's region, and lifting the serial answer back to 8 there is exactly the T*T
    !> oversubscription that rule exists to prevent.
    subroutine parquet_set_string_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_string_threads: n must be >= 0 (0 means automatic)"
        cfg_string_threads = n
    end subroutine parquet_set_string_threads
    !
    !> Sets the cap on how many threads one bulk `pf_spatial_index` query may use internally.
    !>
    !> Read per call, so it takes effect immediately. `0` restores automatic behaviour (as many
    !> threads as OpenMP offers, clamped to the affinity mask). The value is a **cap** and never a
    !> request: it can only lower the automatic answer, it never overrides the rule that an
    !> unqualified bulk call inside an OpenMP parallel region runs serially, and an explicit
    !> `threads=` on `%build`/`%all_within` is still honoured everywhere.
    !>
    !> **Threading a spatial query changes how fast it answers and never what it answers.** The
    !> index is read-only during a query and each thread writes its own disjoint slice of the
    !> result, so the serial and threaded answers are identical row for row -- which is what makes
    !> this admissible as a setting at all.
    subroutine parquet_set_spatial_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_spatial_threads: n must be >= 0 (0 means automatic)"
        cfg_spatial_threads = n
    end subroutine parquet_set_spatial_threads
    !
    !> The configured spatial thread cap, as set. `0` means automatic and is NOT the resolved
    !> count -- ask the index what it used, or read `parquet_get_spatial_threads` only to find out
    !> what was asked for.
    integer function parquet_get_spatial_threads() result(n)

        n = cfg_spatial_threads
    end function parquet_get_spatial_threads
    !
    !> Caps the threads one bulk HEALPix conversion (`pf_ang2pix_ring_bulk` and its siblings) may
    !> use when the call itself does not name a `threads=`. `0` restores automatic behaviour.
    !>
    !> **It is a cap, not a request, and it does not lift this tier's own ceiling.** The automatic
    !> answer is the smaller of what OpenMP offers and a measured internal ceiling, and this lowers
    !> that further; it never raises it, never overrides the rule that an unqualified bulk call
    !> inside an OpenMP parallel region runs serially, and never overrides an explicit `threads=`.
    !> To ask for more than the internal ceiling, pass `threads=` on the call.
    !>
    !> **Threading a HEALPix conversion changes how fast it answers and never what it answers.**
    !> Every element is a pure function of its own inputs and each thread writes its own disjoint
    !> slice of the output, so the serial and threaded results are bit-identical -- which is what
    !> makes this admissible as a setting at all.
    subroutine parquet_set_healpix_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_healpix_threads: n must be >= 0 (0 means automatic)"
        cfg_healpix_threads = n
    end subroutine parquet_set_healpix_threads
    !
    !> The configured HEALPix thread cap, as set. `0` means automatic and is NOT the resolved
    !> count -- the automatic answer is bounded by this tier's own ceiling and by the work
    !> available, so read this only to find out what was asked for.
    integer function parquet_get_healpix_threads() result(n)

        n = cfg_healpix_threads
    end function parquet_get_healpix_threads
    !
    !> Turns the spatial index's automatic-rebuild warning on or off.
    !>
    !> The warning fires at most once per index object, when a query radius disagrees badly enough
    !> with the radius the index was built for that the index rebuilds itself. Turning it off does
    !> not stop the rebuild -- it stops the library saying so.
    subroutine parquet_set_spatial_rebuild_warning(on)
        logical, intent(in) :: on !! .true. warns (the default), .false. rebuilds silently.

        cfg_spatial_rebuild_warning = on
    end subroutine parquet_set_spatial_rebuild_warning
    !
    !> Whether the spatial index's automatic-rebuild warning is enabled.
    logical function parquet_get_spatial_rebuild_warning() result(on)

        on = cfg_spatial_rebuild_warning
    end function parquet_get_spatial_rebuild_warning
    !
    !> Sets the cap on how many threads one bulk `pf_random_permutation`/`pf_random_subset` call
    !> may use internally.
    !>
    !> Read per call, so it takes effect immediately. `0` restores automatic behaviour (as many
    !> threads as OpenMP offers). The value is a **cap** and never a request: it can only lower the
    !> automatic answer, it never overrides the rule that an unqualified bulk call inside an OpenMP
    !> parallel region runs serially, and an explicit `threads=` is still honoured everywhere.
    !>
    !> **Threading a permutation changes how fast it is built and never what it contains.**
    !> `pf_random_perm_at(seed, m, k)` is a pure function of its coordinates, so every element is
    !> computed independently of every other -- the 1-thread and 64-thread outputs were verified
    !> bit-identical. That is a stronger guarantee than most threading knobs can offer, and it is
    !> why this one is admissible as a setting at all.
    subroutine parquet_set_random_threads(n)
        integer, intent(in) :: n !! thread cap, or 0 for automatic; must be >= 0.

        if (n < 0) error stop "parquet_set_random_threads: n must be >= 0 (0 means automatic)"
        cfg_random_threads = n
    end subroutine parquet_set_random_threads
    !
    !> Sets the fewest elements a thread must be given before a bulk permutation opens a team.
    !>
    !> **A work floor, not a chunk size.** Threading a small permutation is not merely useless but
    !> harmful -- the team costs more than the whole job -- so below `threads * this` elements the
    !> bulk forms run serially however many threads are available. Machine B measured `m = 10` going
    !> from 0.0021 ms on one core to 0.0050 on sixteen, and 1->16 thread efficiency of 99 % at
    !> `10**6`, 95 % at `10**4`, 69 % at 1000 and 18 % at 100; the default of 1000 sits where that
    !> curve turns.
    !>
    !> Read per call, so it takes effect immediately. `n` takes `integer(int32)` or
    !> `integer(int64)`. `0` disables the floor entirely, which is how a test asks for a team on a
    !> small array; it is not a useful production setting. An explicit `threads=` does not bypass
    !> the floor -- the floor is about whether the work is worth splitting at all, which is a
    !> property of the array rather than of the caller's intent.
    subroutine parquet_set_random_parallel_min_elements_int64(n)
        integer(int64), intent(in) :: n !! elements per thread, or 0 to disable; must be >= 0.

        if (n < 0) error stop "parquet_set_random_parallel_min_elements: n must be >= 0 " // &
            "(0 disables the work floor)"
        cfg_random_parallel_min_elements = n
    end subroutine parquet_set_random_parallel_min_elements_int64
    !
    !> int32 form of parquet_set_random_parallel_min_elements_int64 -- see it for what it means.
    subroutine parquet_set_random_parallel_min_elements_int32(n)
        integer(int32), intent(in) :: n !! elements per thread, or 0 to disable; must be >= 0.

        call parquet_set_random_parallel_min_elements_int64(int(n, kind=int64))
    end subroutine parquet_set_random_parallel_min_elements_int32
    !
    !> Sets the row count below which a sort refuses to use threads at all, however many `threads=`
    !> asks for. Pass `0` to restore the built-in 8192.
    !>
    !> Enables or disables the sort's integer counting fast path.
    !>
    !> The counting path is a second implementation that must produce exactly the same permutation as
    !> the comparator path, and it is the one place in the sort engine where a wrong answer would be
    !> fast rather than slow. Turning it off is how a test compares the two on one fixture; there is
    !> no performance reason for a program to do so.
    !>
    !> The full rule, in this order: the counting path is used when this flag is on **and** the key's
    !> value range fits `parquet_set_sort_counting_bucket_limit` **and** the key is a single,
    !> null-free integer key. Turning the flag off overrides the limit; raising the limit does
    !> nothing while the flag is off.
    subroutine parquet_set_sort_counting_path(enabled)
        logical, intent(in) :: enabled !! .true. (the default) allows the fast path.

        cfg_sort_counting_path = enabled
    end subroutine parquet_set_sort_counting_path
    !
    !> Reports whether the sort's integer counting fast path is allowed.
    logical function parquet_get_sort_counting_path() result(enabled)

        enabled = cfg_sort_counting_path
    end function parquet_get_sort_counting_path
    !
    !> Enables or disables the sort's single-key radix fast path.
    !>
    !> The radix path is a stable LSD radix sort that performs **no comparisons at all**, so it is a
    !> third independent statement of the ordering beside the two comparators
    !> (`feature_risks.md` Risk-89). Turning it off is how a test compares it against the comparison
    !> sort on one fixture, which is the only way to grade a path that cannot fail slowly.
    !>
    !> **The one reason a program might turn it off is MEMORY.** The radix path allocates up to four
    !> `n`-element `int64` buffers -- about 32 bytes per row -- where the comparison sort allocates
    !> nothing beyond the permutation itself. At 50 million rows that is roughly 1.6 GB of scratch,
    !> which a caller sorting near the edge of available memory may not want to spend. It buys
    !> several times the throughput on a single key, so leave it on unless that trade is real for
    !> you.
    !>
    !> The full rule, in this order: the radix path is used when this flag is on **and** there is
    !> exactly one sort key **and** the row count clears an internal floor (the measured crossover
    !> below which the comparison sort is cheaper). It applies to every key family -- integer, real
    !> and string alike -- so unlike the counting path, the key's type is no reason it would decline.
    subroutine parquet_set_sort_radix_path(enabled)
        logical, intent(in) :: enabled !! .true. (the default) allows the fast path.

        cfg_sort_radix_path = enabled
    end subroutine parquet_set_sort_radix_path
    !
    !> Reports whether the sort's single-key radix fast path is allowed.
    logical function parquet_get_sort_radix_path() result(enabled)

        enabled = cfg_sort_radix_path
    end function parquet_get_sort_radix_path
    !
    !> Sets the largest key value RANGE for which the sort's counting fast path is taken. Pass `0`
    !> to restore the built-in 4194304 (2**22).
    !>
    !> **Range, not cardinality** -- the bound is `max(key) - min(key)`, so a thousand values spread
    !> over a billion is far outside a limit that a million densely-packed values sit inside. This
    !> distinction has already misled one test author here (feature_risks.md Risk-39).
    !>
    !> The number IS the memory control: `n` buckets costs `8n` bytes of counters, so the built-in
    !> value caps the counting path at 32 MB. Raising it trades memory for speed on wide-ranged
    !> integer keys; it does nothing at all while parquet_set_sort_counting_path is `.false.`.
    !>
    !> Available in both integer kinds; an int64 key's range can exceed int32.
    subroutine parquet_set_sort_counting_bucket_limit_int64(n)
        integer(int64), intent(in) :: n !! bucket ceiling, or 0 for the built-in default; must be >= 0.

        if (n < 0) error stop "parquet_set_sort_counting_bucket_limit: n must be >= 0 " // &
            "(0 restores the built-in default)"
        cfg_sort_counting_bucket_limit = n
    end subroutine parquet_set_sort_counting_bucket_limit_int64
    !
    !> int32 form of parquet_set_sort_counting_bucket_limit_int64 -- see it for what the value means.
    subroutine parquet_set_sort_counting_bucket_limit_int32(n)
        integer(int32), intent(in) :: n !! bucket ceiling, or 0 for the built-in default; must be >= 0.

        call parquet_set_sort_counting_bucket_limit_int64(int(n, kind=int64))
    end subroutine parquet_set_sort_counting_bucket_limit_int32
    !
    !> Reports the counting path's bucket ceiling -- the EFFECTIVE value, so a program that never set
    !> it is told 4194304 rather than the `0` that is stored.
    integer(int64) function parquet_get_sort_counting_bucket_limit() result(n)

        n = cfg_sort_counting_bucket_limit
        if (n <= 0) n = sort_counting_bucket_limit_builtin
    end function parquet_get_sort_counting_bucket_limit
    !
    !> Sets how much the library prints. One of "normal" (everything, the factory default),
    !> "silent" (the library's own remarks and its explicitly-called print procedures go quiet;
    !> warnings and errors still appear) or "errors_only" (warnings go quiet too). Case-insensitive;
    !> anything else aborts.
    !>
    !> Read per message, so it takes effect immediately.
    !>
    !> **Errors are never suppressed, at any level.** An `error stop`, the C++ side's fatal-error
    !> report, and the context lines a failing close prints before aborting all appear whatever this
    !> is set to -- a program's control flow depends on that output being findable.
    !>
    !> **"silent" turns the explicitly-called print procedures into no-ops** -- `%print_stat`,
    !> `%print_schema_info` and `parquet_string_column`'s printers included. That is deliberate (it
    !> is what a global output control means) and it is a debugging trap worth knowing about: add a
    !> print, see nothing, and the table is not at fault. `parquet_print_settings` is the one
    !> exemption, so a silenced program can always be asked why it is silent.
    subroutine parquet_set_verbosity(level)
        character(len=*), intent(in) :: level !! "normal" | "silent" | "errors_only".
        character(len=:), allocatable :: tok, expected

        call fold_ascii_lower(trim(level), tok)
        select case (tok)
        case ("normal")
            cfg_verbosity = verb_normal
        case ("silent")
            cfg_verbosity = verb_silent
        case ("errors_only")
            cfg_verbosity = verb_errors_only
        case default
            call token_list(verbosity_tokens, expected)
            error stop "parquet_set_verbosity: unknown level '" // tok // &
                "' (expected one of: " // expected // ")"
        end select
    end subroutine parquet_set_verbosity
    !
    !> Reports the current verbosity as the same token parquet_set_verbosity accepts.
    subroutine parquet_get_verbosity(level)
        character(len=:), allocatable, intent(out) :: level !! "normal" | "silent" | "errors_only".

        select case (cfg_verbosity)
        case (verb_silent)
            level = "silent"
        case (verb_errors_only)
            level = "errors_only"
        case default
            level = "normal"
        end select
    end subroutine parquet_get_verbosity
    !
    !> Sets which stream the library's own messages go to: "stdout" (the factory default) or
    !> "stderr". Case-insensitive; anything else aborts.
    !>
    !> Read per message, so it takes effect immediately. The usual reason to change it is a program
    !> that pipes its own stdout to a data consumer and does not want the library's warnings mixed
    !> into that stream.
    !>
    !> **Only these two values are accepted, and that is a constraint rather than a preference.** A
    !> Fortran unit number means nothing to the C++ half of this library, which prints three of the
    !> warnings and one of the reports itself -- so a knob holding an arbitrary unit could be
    !> honoured by the Fortran sites and silently ignored by the C++ ones. Sending messages to a log
    !> file is therefore not supported; a shell redirect or the program's own logging covers it.
    !>
    !> **The error path does not follow this knob at all.** An `error stop` and the C++ side's
    !> fatal-error report always go to stderr, and the context lines `parquet_emit_error_context`
    !> prints just before an abort always go to stdout -- see that procedure for why. The
    !> explicitly-called print procedures are unaffected too: they keep their own `unit=` argument
    !> and its `output_unit` default.
    subroutine parquet_set_message_stream(stream)
        character(len=*), intent(in) :: stream !! "stdout" | "stderr".
        character(len=:), allocatable :: tok, expected

        call fold_ascii_lower(trim(stream), tok)
        select case (tok)
        case ("stdout")
            cfg_message_stream = stream_stdout
        case ("stderr")
            cfg_message_stream = stream_stderr
        case default
            call token_list(stream_tokens, expected)
            error stop "parquet_set_message_stream: unknown stream '" // tok // &
                "' (expected one of: " // expected // ")"
        end select
    end subroutine parquet_set_message_stream
    !
    !> Reports the current message stream as the same token parquet_set_message_stream accepts.
    subroutine parquet_get_message_stream(stream)
        character(len=:), allocatable, intent(out) :: stream !! "stdout" | "stderr".

        if (cfg_message_stream == stream_stderr) then
            stream = "stderr"
        else
            stream = "stdout"
        end if
    end subroutine parquet_get_message_stream
    !
    !> Emits one informational remark -- something worth mentioning that is not a warning about the
    !> data. Suppressed from "silent" downward.
    !>
    !> The library has exactly one of these today (the development-build notice in
    !> parquet_get_version). It has its own channel rather than a special case inside
    !> parquet_emit_warning because it is the one message whose suppression level differs, and a
    !> hard-coded exception there would have to be re-explained every time someone read the
    !> suppression logic.
    subroutine parquet_emit_info(text)
        character(len=*), intent(in) :: text !! the message, with no prefix.

        if (cfg_verbosity >= verb_silent) return
        write (message_unit(), '(a)') text
    end subroutine parquet_emit_info
    !
    !> Emits one warning about the data or the schema. Suppressed only at "errors_only".
    !>
    !> **This is the single place a Fortran-side warning is printed**, which is what makes both
    !> output settings apply everywhere without each call site testing them -- see
    !> tools/check_source_conventions.py's `no direct printing` check, which is what keeps that true.
    !> It supplies the "WARNING: " prefix, so twelve call sites no longer repeat it and it cannot
    !> drift between them.
    subroutine parquet_emit_warning(text)
        character(len=*), intent(in) :: text !! the message, without the "WARNING: " prefix.

        if (cfg_verbosity >= verb_errors_only) return
        write (message_unit(), '(a)') "WARNING: " // text
    end subroutine parquet_emit_warning
    !
    !> Emits one line of context belonging to an error that is about to abort.
    !>
    !> **Never suppressed and never redirected.** These lines carry what the abort message
    !> deliberately leaves out -- the output filename, the schema name -- so silencing them would
    !> turn a diagnosable failure into one that names nothing. They stay on standard output, where
    !> they are today, rather than following `message_stream`: they belong to the error path, and
    !> moving them would change what an existing program sees for no gain.
    subroutine parquet_emit_error_context(text)
        character(len=*), intent(in) :: text !! the context line, printed verbatim.

        write (output_unit, '(a)') text
    end subroutine parquet_emit_error_context
    !
    !> The unit the emit channels write to. One function so the three cannot disagree.
    integer function message_unit() result(u)

        if (cfg_message_stream == stream_stderr) then
            u = error_unit
        else
            u = output_unit
        end if
    end function message_unit
    !
    !> Renders a token vocabulary as "a, b, c", for an error message.
    subroutine token_list(tokens, out)
        character(len=*), intent(in) :: tokens(:) !! the accepted vocabulary.
        character(len=:), allocatable, intent(out) :: out !! comma-separated, in array order.
        integer :: i

        out = ""
        do i = 1, size(tokens)
            if (i > 1) out = out // ", "
            out = out // trim(tokens(i))
        end do
    end subroutine token_list
    !
    !> Lowercases ASCII letters. A local copy rather than parquet_to_lower, because that one lives
    !> in parquet_core, which uses THIS module -- importing it back would be a circular dependency.
    subroutine fold_ascii_lower(text, out)
        character(len=*), intent(in) :: text !! input text.
        character(len=:), allocatable, intent(out) :: out !! text with every ASCII A-Z lowercased.
        integer :: k, ic

        out = text
        do k = 1, len(out)
            ic = iachar(out(k:k))
            if (ic >= iachar("A") .and. ic <= iachar("Z")) out(k:k) = achar(ic + 32)
        end do
    end subroutine fold_ascii_lower
    !
    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits in the
    ! thirty other files carrying this marker, but gfortran attributes a count to this one -- so
    ! the excluded line is expected to show a positive hit count here. The marker stays: which way
    ! a given gcov attributes it is not something to depend on.
end module parquet_settings_base ! GCOVR_EXCL_LINE
