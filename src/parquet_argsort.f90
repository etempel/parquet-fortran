!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_argsort` over plain Fortran arrays, and the sort engine underneath it.
!!
!! **This module is a TIER, not a convenience facade, and what it does NOT import is the point.**
!! Its Fortran `use` graph reaches `parquet_settings_base` and the intrinsic modules and nothing
!! else -- no `parquet_bindings`, and so no Arrow anywhere in the graph. A project that wants an
!! argsort, or that wants `parquet_sampling`'s weighted draws, compiles this tier and stops there
!! instead of compiling the whole reader/writer stack. `parquet_sorting` sits on top and extends
!! `pf_argsort` with the five element types that need a parquet column, a packed string store or a
!! temporal element.
!!
!! **The C++ engine is reached through a procedure POINTER, and that indirection is what keeps this
!! module Arrow-free.** The second, independent engine in `src/parquet_wrapper.cpp` exists so the
!! tests can check this one against it. Binding it is `parquet_sorting_oracle`'s job; a program that
!! never imports that module never compiles it, and fpm prunes it away. See
!! `parquet_argsort_bind_oracle` below.
!!
!! **Naming.** Everything public carries the `pf_` prefix (parquet-fortran) rather than `parquet_`,
!! because the subject is not a parquet file -- see CLAUDE.md's "Naming conventions". The module is
!! `parquet_argsort` rather than `pf_argsort` because a module cannot share its name with a
!! procedure it declares.
!!
!! User guide: `doc/pages/utilities/sorting.md`.
module parquet_argsort
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    ! Every sorting knob this tier reads, plus the output pair, because `warn_thread_clamp` emits.
    ! Taking them from the leaf rather than from `parquet_settings` is what keeps the graph clear of
    ! `parquet_bindings`; see that module's header for the rule.
    use parquet_settings_base
    !
    implicit none
    private
    !
    public :: pf_argsort
    public :: pf_sort_threads
    !
    ! ---- Internal to the sorting tiers; `src/parquet.f90` privatises all of it ----
    !
    ! `parquet_sorting` needs the key type because `pf_sort_keys` holds an array of it, and
    ! `parquet_sorting_oracle` needs to read its components to fill the C++ builder. Its components
    ! are therefore accessible rather than `private` -- the type itself never reaches a user, since
    ! `parquet_sorting` does not re-export it and `pf_sort_keys` holds it privately.
    public :: sort_key_buf, SK_INT, SK_REAL, SK_STR
    ! The two comparators, so parquet_sorting can implement the four debug hooks that take a
    ! `pf_sort_keys` -- a type this tier cannot see. No library code outside those hooks calls them.
    public :: sort_row_less, sort_keys_compare
    !
    ! The extraction, dispatch and narrowing this tier owns, because `parquet_sorting`'s own
    ! submodules reach all of it: its five down-tier extractors produce the same `sort_key_buf`,
    ! its `pf_sort`/`pf_partial_sort`/`pf_unique`/`pf_minmax` families run through the same
    ! dispatchers, and every int32 form narrows with the same helpers. Sharing one copy is the whole
    ! point -- two extraction paths would be two chances to disagree about what a null is.
    public :: extract_i32, extract_i64, extract_f32, extract_f64, extract_bool, extract_chr
    public :: valid_from_mask, fill_identity, tail_team, resolve_thread_count
    public :: drive_engine, drive_engine_grouped, engine_build_runs, runs_to_offsets
    public :: narrow_perm, narrow_offsets
    !
    ! The engine-selection flag, and the RELAYS the selectors dispatch through. `parquet_sorting`
    ! keeps five of the seven selectors (the ones its own operations use), so it reads the same flag
    ! and reaches the same pointers rather than keeping a second copy that could disagree.
    !
    ! The `p_*` pointers themselves stay PRIVATE, and the relays exist, because gfortran 15.2 ICEs
    ! under `-flto` when a submodule calls a module-level procedure pointer -- see `oracle_argsort`.
    ! All seven relays are public, including the two only `parquet_argsort_kernel` calls: gfortran
    ! does not emit a PRIVATE module-contained procedure whose only callers are that module's own
    ! submodules, so those two link-failed as undefined symbols under `--profile release`. The usual
    ! fix for that shape -- declare the interface here and implement it in a submodule -- is exactly
    ! what reintroduces the ICE these relays exist to avoid, so public is the remaining option.
    public :: dbg_fortran_engine, check_oracle
    public :: oracle_argsort, oracle_runs
    public :: oracle_partial, oracle_nth, oracle_is_sorted, oracle_search, oracle_merge
    !
    ! The Fortran engine itself. `parquet_sorting`'s five selectors call the same entry points this
    ! tier's two do -- one engine for all eleven element types is the property the whole sorting
    ! design rests on, so there is exactly one copy and both tiers reach it here.
    public :: sort_build_permutation, sort_build_permutation_threaded
    public :: sort_comparison_permutation, sort_partial_permutation, sort_nth_index
    public :: sort_is_sorted, sort_build_runs_permutation, sort_search_position
    public :: sort_merge_permutation, sort_counting_candidate, sort_counting_permutation
    public :: sort_tier_of, sort_compare_key
    public :: parquet_argsort_bind_oracle, parquet_argsort_select_engine
    !
    ! ---- Re-exported from parquet_settings_base ----
    !
    ! A module re-exports, get and set, every knob its own code reads -- so a program importing this
    ! tier alone can configure the sort it is about to run without importing parquet_settings.
    public :: parquet_set_sort_threads, parquet_get_sort_threads
    public :: parquet_set_sort_radix_path, parquet_get_sort_radix_path
    public :: parquet_set_sort_counting_path, parquet_get_sort_counting_path
    public :: parquet_set_sort_counting_bucket_limit, parquet_get_sort_counting_bucket_limit
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream
    !
    ! Test-only, and PUBLIC because there is no other route: they expose the comparator core, whose
    ! state (`sort_key_buf`) is private to this module. CLAUDE.md's "A Fortran-side debug hook has
    ! to be PUBLIC, so prefer a C++ one" states the rule and the accepted precedents; the C++ route
    ! is unavailable here precisely because Stage 1 exists to move this decision OUT of C++.
    ! No library code calls either, neither appears in README.md's API overview, and neither is
    ! mentioned in any doc/pages/ guide -- see feature_sort.md section 7.4.
    ! NOTE `parquet_debug_use_fortran_sort_engine` -- the SETTER -- is NOT here: it lives in
    ! parquet_sorting_oracle, which is the only module that can honour it, and registering the
    ! oracle's entry points is a side effect of calling it. That is what makes registration
    ! impossible to forget: selecting the C++ engine IS binding it. The getter stays beside the
    ! flag it reads.
    public :: parquet_debug_using_fortran_sort_engine
    public :: parquet_debug_set_sort_depth_limit
    public :: parquet_debug_sort_heapsort_calls
    public :: parquet_debug_set_sort_track_shift
    public :: parquet_debug_sort_max_insertion_shift
    public :: parquet_debug_set_sort_radix_min_rows
    public :: parquet_debug_set_sort_task_floor
    public :: parquet_debug_set_sort_tail_min_rows
    public :: parquet_debug_set_sort_engine_min_rows
    public :: parquet_debug_set_sort_counting_max_threads
    public :: parquet_debug_sort_refine_runs
    public :: parquet_debug_set_sort_split_min_card
    public :: parquet_debug_set_sort_radix_fail_alloc
    public :: parquet_debug_reset_sort_radix_passes
    public :: parquet_debug_sort_radix_passes
    public :: parquet_debug_sort_threads_used
    public :: parquet_debug_sort_split_buckets
    public :: parquet_debug_sort_design
    !
    !
    !> Error-message prefix for every `error stop` raised by this module.
    !!
    !! **The string says `parquet_sorting` in BOTH tiers, deliberately.** `pf_argsort` is
    !! documented as part of the sorting API however it is imported, so a caller must not see
    !! a different prefix according to which internal module happened to raise the error --
    !! and every existing error-scenario test asserts the message it has always produced.
    character(len=*), parameter :: EP = "parquet_sorting: "
    !
    ! ---- Engine selection: the Fortran engine is the DEFAULT; the selector is TEST-ONLY ---------
    !
    ! Stage 6 flipped this to `.true.`, so `pf_sort`/`pf_argsort` and every operation reached
    ! through `drive_engine` run the Fortran engine. The selector itself STAYS: the conformance
    ! tests A/B the two engines over the same data through the same public entry point, and the
    ! C++ engine is the oracle for that comparison. It is deliberately NOT a `parquet_settings`
    ! knob: that module admits a setting only when it changes how fast, how large or how loud the
    ! library runs and never what it ANSWERS, and an engine selector is exactly a second way to get
    ! a different answer should the two ever disagree. It is also why these are `parquet_debug_*`
    ! and absent from README.md's API overview.
    !
    ! **The C++ engine is NOT dead after this flip, and the published `sort_parallel_min_rows`
    ! setting must NOT be retired.** `parquet_reader_set_sort` and `parquet_open_reader(...,
    ! sort_by=)` reach `sort_build_permutation_threaded` (src/parquet_wrapper.cpp) directly, with
    ! no selector anywhere in that path, and it reads `g_sort_parallel_min_rows` -- mirrored from
    ! that setting -- to decide whether a read-time sort threads. feature_sort_report.md section
    ! 14.6's "retire it at the cutover" note assumed the flip removed the C++ engine from the
    ! library; it removes it only from `pf_sort`/`pf_argsort`.
    !
    ! Both are process-global saved state, which is why the `sorting` and `sort` suites must stay
    ! excluded from test-drive's per-test parallelism (test/run_tester.f90) -- they already are.
    logical, save :: dbg_fortran_engine = .true. !! .true. routes `drive_engine` to the Fortran sort.
    !> Nonzero once the affinity-clamp warning has been claimed, so it is said once per process
    !! rather than once per sort. Claimed by an `!$omp atomic capture` in `warn_thread_clamp`, which
    !! is where the reasoning lives.
    !!
    !! **An integer rather than the obvious `logical`, and that is a portability constraint, not a
    !! preference.** The natural test-and-set is `seen = flag; flag = .true.` inside an
    !! `atomic capture`, and nagfor 7.2 rejects it -- *"Invalid form of expression in OpenMP ATOMIC
    !! assignment"* -- while gfortran accepts it, so the shape compiles on the machine you wrote it
    !! on and fails on the next one. A fetch-and-add over an integer is accepted by both, and the
    !! claim is then "the caller that displaced a zero". Do not simplify it back to a logical.
    integer(int64), save :: thread_clamp_claims = 0_int64
    !> Overrides the introsort's depth limit; NEGATIVE restores the computed `2*floor(log2(n))`.
    !!
    !! Zero forces the heapsort fallback on the first partition, which is otherwise unreachable from
    !! any fixture a test can build: median-of-three pivoting means ordinary data never approaches a
    !! depth of `2*log2(n)`, so without this hook a whole algorithm arm would ship untested and every
    !! mutation to it would survive. CLAUDE.md's "A SIZE THRESHOLD is the same trap wearing different
    !! clothes" is the general form of this.
    integer, save :: dbg_sort_depth_limit = -1
    !> Heapsort fallbacks entered since the depth limit was last set, for the test that forces one.
    !!
    !! A hook that FORCES a state needs a way to prove the state took effect, or the test it enables
    !! passes just as happily against a hook that does nothing -- both paths answer identically here,
    !! so no assertion on the permutation can tell them apart. This is the `had_index` shape from
    !! `feature_risks.md` Risk-75. It costs one increment per heapsort call, i.e. at most O(log n)
    !! per sort and never anything per comparison.
    integer(int64), save :: dbg_sort_heapsort_calls = 0_int64
    !> .true. makes the introsort's final insertion pass record how far it moved anything.
    !!
    !! **This is what stops the final insertion pass from masking a broken heapsort.** That pass is a
    !! complete sort, so a heapsort that orders nothing still yields a correctly sorted answer --
    !! confirmed by mutation testing, where a sift-down with its comparison inverted survived the
    !! whole suite. What it cannot fake is the invariant the quicksort is supposed to establish: that
    !! no element is more than `SORT_INSERTION_CUTOFF` positions LEFT of where it belongs. Measuring
    !! the largest shift is how a test sees that, at one comparison per ELEMENT (not per shift) and
    !! only when armed.
    !!
    !! Meaningful single-threaded only, exactly like the C++ comparison counter it parallels.
    logical, save :: dbg_sort_track_shift = .false.
    !> Largest distance the final insertion pass moved any element since the tracker was armed.
    integer(int64), save :: dbg_sort_max_shift = 0_int64
    !> Overrides the radix path's row floor; NEGATIVE restores the built-in `SORT_RADIX_MIN_ROWS`.
    !!
    !! Needed in BOTH directions, which is unusual for a threshold hook. Raising it (to `huge`)
    !! declines the radix path, which is how the introsort's own negative controls stay non-vacuous
    !! once the floor drops below their fixture sizes; lowering it (to 2) drives every engine fixture
    !! in the suite through the radix path, which is the sweep `feature_sort_radix.md` section 7.4
    !! describes. Both are the `feature_risks.md` Risk-49 shape -- a size threshold hiding a code
    !! path from the tests written for everything else.
    integer(int64), save :: dbg_sort_radix_min_rows = -1_int64
    !> Overrides the balanced split's smallest task size; NEGATIVE restores `SORT_TASK_FLOOR`.
    !!
    !! Risk-49 again, and the sharpest instance of it in this module: the floor binds only when
    !! `nv / team` falls below it, i.e. small `n` with a large team, which is precisely the regime
    !! no fixture in the suite reaches -- so the constant ships unexercised rather than merely
    !! untuned. It is also the reason a value for it cannot be measured by rebuilding: a crossover
    !! sits inside this project's 11-16% cross-build noise floor, so the sweep has to happen in one
    !! binary, which is what this hook is for.
    integer(int64), save :: dbg_sort_task_floor = -1_int64
    !> Overrides the TAIL passes' row floor; NEGATIVE restores `SORT_TAIL_ELEMS_PER_THREAD * nt`.
    !!
    !! The tail (key extraction, the identity fill, the int32 narrowing) is memcpy-shaped, so its
    !! threading crossover has no reason to equal the SORT's -- and until this existed the two shared
    !! one number, the since-retired `sort_parallel_min_rows`, which could not be right for both.
    !! This hook
    !! is what lets the tail's own crossover be measured without disturbing the sort's.
    integer(int64), save :: dbg_sort_tail_min_rows = -1_int64
    !> Overrides the Fortran ENGINE's own threading floor; NEGATIVE restores the built-in rule.
    !!
    !! The Fortran engine's floor is internal and automatic -- a measured function of the team --
    !! so this hook is the only way to move it; the C++ engine has its own separate bind(C)
    !! override, since neither is a setting any more. This hook is what
    !! `force_parallel_threshold` in the tests drives.
    integer(int64), save :: dbg_sort_engine_min_rows = -1_int64
    !> Overrides how large a team may be and still take the counting path; NEGATIVE restores 2.
    !!
    !! The counting sort is SERIAL, so whether it beats the radix is a question about the TEAM as
    !! well as the value range -- and the grid that first set this rule stepped 1, 4, 16, 64 threads
    !! and so never measured the one team size where the answer had changed. This hook exists so the
    !! ceiling can be A/B'd inside one binary rather than across two builds, which for a crossover is
    !! the only resolution that works (see `feature_sort_report.md` section 12.1).
    integer(int64), save :: dbg_sort_counting_max_threads = -1_int64
    !> Parallel refine dispatches the last string sort made; 0 means the refine ran entirely serially.
    !!
    !! **The refine is the one phase whose threading NOTHING else can observe.** Its serial and
    !! threaded arms produce byte-identical permutations -- that is what makes the serial fallback
    !! safe -- so every correctness test passes either way, and the phase sat unthreaded through the
    !! whole parallel-sort campaign while a shared-prefix column scaled 1.06x from 1 to 64 threads.
    !! Counting dispatches rather than setting a flag is what lets a test tell the two threaded levels
    !! apart: refining over many runs reports the run count, while one giant run reports the number of
    !! sub-bucket loops that opened a team.
    integer(int64), save :: dbg_sort_refine_runs = 0_int64
    !> Overrides the distinct-value count the split digit must reach; NEGATIVE restores
    !! `SORT_SPLIT_MIN_CARD`.
    !!
    !! Selects between the two designs at a fixed cardinality, so the sweep that locates the real
    !! crossover -- where refined Design B stops beating Design A -- can run without a rebuild per
    !! point. Set it to 0 to force Design B onto every key, or to `huge` to force Design A.
    integer(int64), save :: dbg_sort_split_min_card = -1_int64
    !> Which of the radix path's scratch allocations should report failure: 0 none, 1 the main
    !! buffers, 2 the deep string refine's.
    !!
    !! Those fallbacks -- decline and let the comparison sort finish the job -- are otherwise
    !! unreachable from any fixture a test can build: provoking a real `allocate` failure needs a
    !! machine-sized array, and on Linux's default overcommit policy it would not report one anyway.
    !! Without this they would ship untested and every mutation to them would survive, which is the
    !! same argument `dbg_sort_depth_limit` carries for the heapsort arm.
    !!
    !! **It selects rather than switches, and it has to.** The two allocations are in series: with a
    !! single flag, failing the main one returns before the refine's is ever reached, so the refine's
    !! fallback would stay untested however the flag was set.
    integer, save :: dbg_sort_radix_fail_alloc = 0
    !> Radix scatter passes actually EXECUTED since the counter was last reset.
    !!
    !! The `had_index` shape from `feature_risks.md` Risk-75, and the only observable an optimisation
    !! that changes the PASS COUNT has. Several of them exist -- the constant-digit skip, and the
    !! narrow-integer bias that exists to make that skip fire -- and every one of them leaves the
    !! permutation bit-identical by construction. So no assertion on an answer can distinguish a
    !! build where the optimisation fires from one where it never does, and without this counter a
    !! test for any of them is vacuous rather than merely weak.
    !!
    !! Counts a pass that scatters, never one the skip declined, and never the string refine's own
    !! recursion -- it is a measure of the LSD loop's work, which is what those optimisations move.
    !! One increment per pass, i.e. at most eight per key and never anything per element.
    integer(int64), save :: dbg_sort_radix_passes = 0_int64
    !> Threads the engine's permutation build actually opened on its last call; 1 means serial.
    !!
    !! Stage 4, and the same `had_index` shape as `dbg_sort_radix_passes` above (`feature_risks.md`
    !! Risk-75). It is the ONLY thing that can tell a threaded build from a serial one, because the
    !! permutation is bit-identical either way: `sort_row_less` is a total order, so exactly one
    !! correct answer exists and no assertion on `perm` can see the team size. Without this counter
    !! every threading test is vacuous, and a policy bug that silently never threads -- the easiest
    !! one to write -- passes the whole suite.
    !!
    !! Records what the policy RESOLVED, not `omp_get_num_threads()` from inside the region. The two
    !! differ when the runtime gives a smaller team than asked for, and it is the decision under test
    !! here, not the runtime's response to it.
    !!
    !! **It has a C++ TWIN, and the two are not interchangeable.**
    !! `parquet_debug_get_sort_threads_used` (`src/parquet_wrapper.cpp`, reached by a local `bind(C)`
    !! interface in `test/test_settings.f90`, `test/test_sorting.f90` and `test/test_diagnostics.f90`)
    !! answers for the **C++** engine; this one answers for the **Fortran** engine. Neither can see
    !! the other, which is deliberate -- a single shared counter would have to be written across the
    !! `bind(C)` boundary by whichever engine ran, and Stage 6 exists to remove that boundary.
    !!
    !! So the tests reading the C++ twin are exactly the ones the Stage 6 cutover has to repoint at
    !! this one, and that repointing is the whole of Group 2 in `feature_sort.md` §6 Stage 6, 6a --
    !! the three failures that reversed the stage ordering. Repoint them; do not delete them.
    integer(int64), save :: dbg_sort_threads_used = 1_int64
    !> Buckets Design B's split produced on the last permutation build; 0 means B did not run.
    !!
    !! Stage 4, and the same reasoning as `dbg_sort_threads_used`: Design B and the serial LSD loop
    !! produce the SAME permutation by construction, so no assertion on `perm` can say which ran.
    !! Every test of the split -- that it happens at all, that a hostile key declines it, that the
    !! balance test and the bucket cap fire -- needs this, and would otherwise pass against an engine
    !! that quietly never took the parallel path.
    !!
    !! Zero is the informative value, not a missing one: it is what a decline looks like, and a
    !! decline is a normal outcome on a low-cardinality key.
    integer(int64), save :: dbg_sort_split_buckets = 0_int64
    !> Which parallel radix design ran on the last build: 0 serial, 1 Design A, 2 Design B.
    !!
    !! Stage 4. All three produce the SAME permutation — the comparator is a total order, so exactly
    !! one answer is correct — which means no assertion on `perm` can tell them apart. Design A is
    !! reached only when Design B declines, so without this a test of the fallback is testing nothing:
    !! it would pass identically against an engine that ran B, ran A, or ran neither.
    integer(int64), save :: dbg_sort_design = 0_int64
    !
    ! ---- Internal key families ----
    integer, parameter :: SK_INT = 1  !! key values live in `ints`.
    integer, parameter :: SK_REAL = 2 !! key values live in `reals`.
    integer, parameter :: SK_STR = 3  !! key values live in `offsets`/`data`.
    !
    !> One extracted sort key, in the canonical form the C++ engine takes.
    !!
    !! Exactly one of `ints`/`reals`/(`offsets`,`data`) is allocated, matching `family`. `valid`
    !! is left UNALLOCATED when the key has no nulls at all, which is the engine's own fast path
    !! -- the same convention `parquet_column%row_validity` already uses.
    type :: sort_key_buf
        ! Components are deliberately NOT `private`: `parquet_sorting_oracle` is a separate module
        ! and has to read every one of them to fill the C++ builder. The type is still invisible to
        ! a user -- `parquet_sorting` does not re-export it, `pf_sort_keys` holds it as a private
        ! component, and `src/parquet.f90` privatises what little is left.
        integer :: family = SK_INT                          !! SK_INT / SK_REAL / SK_STR.
        logical :: descending = .false.                     !! .true. sorts high to low.
        logical :: nulls_first = .false.                    !! .true. places nulls before values.
        integer(int64), allocatable :: ints(:)              !! SK_INT values.
        real(real64), allocatable :: reals(:)               !! SK_REAL values.
        integer(int64), allocatable :: offsets(:)           !! SK_STR: n+1 byte offsets, 0-based.
        character(kind=c_char), allocatable :: data(:)      !! SK_STR: the packed bytes.
        integer(c_int8_t), allocatable :: valid(:)          !! 1 = valid; UNALLOCATED means no nulls.
    end type sort_key_buf
    !> The permutation that would sort `values`: `perm(k)` is the index of the element that
    !> belongs at position k. `values` is never modified.
    !>
    !> The permutation's integer kind is chosen by how the caller declares `perm`. The
    !> `integer(int32)` form aborts when the array is longer than `huge(1_int32)` rather than
    !> truncating; declare `perm` as `integer(int64)` for arrays that large.
    !>
    !> This tier covers the six intrinsic element types.
    !>
    !> `parquet_sorting` imports this generic and adds its own specifics to it, so a program
    !> with a single `use parquet_sorting` sees one `pf_argsort` covering all eleven types.
    interface pf_argsort
        module procedure argsort_i32_i32
        module procedure argsort_i32_i64
        module procedure argsort_i64_i32
        module procedure argsort_i64_i64
        module procedure argsort_f32_i32
        module procedure argsort_f32_i64
        module procedure argsort_f64_i32
        module procedure argsort_f64_i64
        module procedure argsort_bool_i32
        module procedure argsort_bool_i64
        module procedure argsort_chr_i32
        module procedure argsort_chr_i64
    end interface pf_argsort
    ! ---- Key extraction, engine dispatch and the shared helpers ----
    interface
        !> Extracts a 32-bit integer key into the canonical form the engine takes.
        module subroutine extract_i32(values, buf, descending, nulls_first, proc, is_valid, threads)
        integer(int32), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_i32
        !> Extracts a 64-bit integer key into the canonical form the engine takes.
        module subroutine extract_i64(values, buf, descending, nulls_first, proc, is_valid, threads)
        integer(int64), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_i64
        !> Extracts a 32-bit real key into the canonical form the engine takes.
        module subroutine extract_f32(values, buf, descending, nulls_first, proc, is_valid, threads)
        real(real32), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_f32
        !> Extracts a 64-bit real key into the canonical form the engine takes.
        module subroutine extract_f64(values, buf, descending, nulls_first, proc, is_valid, threads)
        real(real64), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_f64
        !> Extracts a logical key into the canonical form the engine takes.
        module subroutine extract_bool(values, buf, descending, nulls_first, proc, is_valid, threads)
        logical, intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_bool
        !> Extracts a string key into the canonical form the engine takes.
        module subroutine extract_chr(values, buf, descending, nulls_first, proc, is_valid, threads)
        character(len=*), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_chr
        !> Runs the C++ engine over `keys`, returning a 1-based permutation.
        module subroutine drive_engine(keys, nrows, proc, perm, threads)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            integer, intent(in), optional :: threads            !! thread request; absent = auto.
        end subroutine drive_engine
        !> `drive_engine`, plus the group boundaries when `group_offsets` is asked for.
        !!
        !! Absent `group_offsets` is exactly `drive_engine`, one-shot fast path and all. Present,
        !! it routes through `engine_build_runs` instead, which always uses the builder -- so
        !! asking for boundaries costs one extra copy of a single key. That is the documented
        !! price of one entry point serving three operations rather than three of them.
        module subroutine drive_engine_grouped(keys, nrows, proc, perm, threads, group_offsets, &
                group_ekeys)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            integer, intent(in), optional :: threads            !! thread request; absent = auto.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:) !! group bounds.
            integer, intent(in), optional :: group_ekeys !! ENGINE keys defining a group; absent = all.
        end subroutine drive_engine_grouped
        !> Turns `engine_build_runs`' tie flags into the offsets `group_offsets` promises:
        !! length `ngroups + 1`, last entry `nrows + 1`, so group g is `perm(o(g):o(g+1)-1)`
        !! for every g with no last-iteration special case.
        module subroutine runs_to_offsets(tie, nrows, offsets)
            integer(c_int8_t), intent(in) :: tie(:) !! 1 where a row ties the previous one.
            integer(int64), intent(in) :: nrows     !! rows sorted; `tie` may be longer.
            integer(int64), allocatable, intent(out) :: offsets(:) !! the group offsets.
        end subroutine runs_to_offsets
        !> Resolves how many threads a sort should use. **This is the only place the auto rule
        !! lives**, and the only place in this module carrying OpenMP plumbing at all -- the
        !! same arrangement parquet_tables_parallel.f90 keeps for the table layer, and worth
        !! more here, since the alternative is that plumbing repeated in 65 generated bodies.
        !> How many threads an AUTOMATIC sort -- one where `threads=` is absent -- would use
        !! right now: `omp_get_max_threads()` when the caller is not inside an OpenMP parallel
        !! region, and 1 when they are, because a nested region is the caller's business.
        !!
        !! Public because the read-time `parquet_open_reader(..., sort_by=)` has to ask the
        !! same question from a different module, and one implementation of this rule is worth
        !! more than a private copy in each -- two copies drifting would mean a raw-array sort
        !! and a read-time sort silently disagreeing about when to thread. Useful in its own
        !! right for reporting or logging what an automatic sort is about to do.
        module function pf_sort_threads() result(n)
            integer :: n !! threads an automatic sort would use; 1 means serial.
        end function pf_sort_threads
        module subroutine resolve_thread_count(threads, nrows, count)
            integer, intent(in), optional :: threads !! caller's request; absent means auto.
            integer(int64), intent(in) :: nrows      !! rows to be sorted.
            integer(int64), intent(out) :: count     !! resolved count; 1 sorts serially.
        end subroutine resolve_thread_count
        !> Team size for a trivially parallel whole-column loop, given an already-resolved
        !! sort thread count.
        !!
        !! Separate from `resolve_thread_count` because the question is different: that one
        !! answers "how many threads may this sort use", this one answers "is this particular
        !! O(n) loop big enough to be worth a team". Both are needed -- a 64-thread sort still
        !! should not open a team to copy 500 elements.
        module function tail_team(nthreads, n) result(team)
            integer(int64), intent(in) :: nthreads !! the sort's resolved thread count.
            integer(int64), intent(in) :: n        !! elements the loop will walk.
            integer :: team                        !! team size; 1 means run it serially.
        end function tail_team
        !> Fills `perm(1:n)` with `1..n`, threaded when `nthreads` and `n` justify it.
        !!
        !! Its own procedure rather than an inline loop because it is one of the three
        !! whole-column serial loops that bound a threaded sort's end-to-end speedup, and
        !! measuring it separately is how that was found. See `app/benchmark_sort_tail.f90`.
        module subroutine fill_identity(perm, n, nthreads)
            integer(int64), intent(out) :: perm(:)  !! receives `1..n`.
            integer(int64), intent(in) :: n         !! elements to fill.
            integer(int64), intent(in) :: nthreads  !! the sort's resolved thread count.
        end subroutine fill_identity
        !> Builds the engine's int8 validity array from a logical mask, leaving `valid`
        !! UNALLOCATED when the mask marks nothing null (the engine's no-nulls fast path).
        module subroutine valid_from_mask(mask, n, proc, valid)
            logical, intent(in) :: mask(:)                          !! .false. marks a null.
            integer(int64), intent(in) :: n                         !! expected length.
            character(len=*), intent(in) :: proc                    !! calling procedure, for messages.
            integer(c_int8_t), allocatable, intent(out) :: valid(:) !! 1 per valid element.
        end subroutine valid_from_mask
        !> Sorts, and reports where the runs of EQUAL rows are: `tie(k)` is 1 when output
        !! position k holds a row comparing equal to the one before it. One call, because
        !! `pf_unique`/`pf_rank` need both and would otherwise build the permutation twice.
        module subroutine engine_build_runs(keys, nrows, proc, perm, tie, threads, group_ekeys)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.
            integer, intent(in), optional :: threads            !! thread request; absent = auto.
            !> How many LEADING keys decide whether two rows tie; absent means all of them.
            !! Counted in ENGINE keys, already resolved from the caller's key count -- the two
            !! differ because one `%add` of a `parquet_timestamp` contributes two engine keys.
            !! The sort itself always uses every key; only the tie test is narrowed.
            integer, intent(in), optional :: group_ekeys
        end subroutine engine_build_runs
        !> Narrows a 1-based int64 permutation to int32, aborting rather than truncating.
        module subroutine narrow_perm(perm64, proc, perm32, threads)
            integer(int64), intent(in) :: perm64(:)                !! the permutation.
            character(len=*), intent(in) :: proc                   !! calling procedure, for messages.
            integer(int32), allocatable, intent(out) :: perm32(:)  !! the narrowed copy.
            integer, intent(in), optional :: threads               !! caller's team request.
        end subroutine narrow_perm
        !> Narrows a group-offsets array to int32, aborting rather than truncating.
        !!
        !! NOT the same test as `narrow_perm`'s, and the difference is exactly one row: a
        !! permutation's largest entry is `n`, but this array's is the sentinel `n + 1`. At
        !! `n == huge(int32)` the permutation narrows cleanly while the sentinel wraps negative,
        !! and a negative sentinel turns the last group's `o(g+1) - 1` into a huge negative
        !! bound -- a silently empty or wildly wrong slice instead of an abort. So this checks
        !! the sentinel itself rather than the length.
        module subroutine narrow_offsets(offsets64, proc, offsets32)
            integer(int64), intent(in) :: offsets64(:)                !! the group offsets.
            character(len=*), intent(in) :: proc                      !! calling procedure.
            integer(int32), allocatable, intent(out) :: offsets32(:)  !! the narrowed copy.
        end subroutine narrow_offsets
    end interface
    !
    ! ---- pf_argsort and pf_sort ----
    interface
        !> pf_argsort over a 32-bit integer array, returning an int32 permutation.
        module subroutine argsort_i32_i32(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_i32_i32
        !> pf_argsort over a 32-bit integer array, returning an int64 permutation.
        module subroutine argsort_i32_i64(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        integer(int32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_i32_i64
        !> pf_argsort over a 64-bit integer array, returning an int32 permutation.
        module subroutine argsort_i64_i32(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        integer(int64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_i64_i32
        !> pf_argsort over a 64-bit integer array, returning an int64 permutation.
        module subroutine argsort_i64_i64(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_i64_i64
        !> pf_argsort over a 32-bit real array, returning an int32 permutation.
        module subroutine argsort_f32_i32(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        real(real32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_f32_i32
        !> pf_argsort over a 32-bit real array, returning an int64 permutation.
        module subroutine argsort_f32_i64(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        real(real32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_f32_i64
        !> pf_argsort over a 64-bit real array, returning an int32 permutation.
        module subroutine argsort_f64_i32(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        real(real64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_f64_i32
        !> pf_argsort over a 64-bit real array, returning an int64 permutation.
        module subroutine argsort_f64_i64(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        real(real64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_f64_i64
        !> pf_argsort over a logical array, returning an int32 permutation.
        module subroutine argsort_bool_i32(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        logical, intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_bool_i32
        !> pf_argsort over a logical array, returning an int64 permutation.
        module subroutine argsort_bool_i64(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        logical, intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_bool_i64
        !> pf_argsort over a string array, returning an int32 permutation.
        module subroutine argsort_chr_i32(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        character(len=*), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int32), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_chr_i32
        !> pf_argsort over a string array, returning an int64 permutation.
        module subroutine argsort_chr_i64(values, perm, descending, nulls_first, is_valid, &
                threads, group_offsets)
        character(len=*), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
            !> where each run of rows comparing EQUAL under the grouping keys begins, as
            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel
            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and
            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an
            !! off-by-one usually gets written.
            !!
            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one
            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,
            !! because rows in the same non-value tier compare equal -- deliberately unlike
            !! `pf_unique`, which drops nulls entirely, since a group list must account for
            !! every row.
            !!
            !! Costs one extra copy of a single key: boundaries come from the builder path,
            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.
            integer(int64), allocatable, intent(out), optional :: group_offsets(:)
        end subroutine argsort_chr_i64
    end interface
    ! ---- The comparator core (parquet_argsort_engine -- HAND-WRITTEN, not generated) ----
    interface
        !> RAW output tier of row `i` under one key: values(0), NaNs(1), nulls(2).
        !!
        !! Absolute, and **neither `descending` nor `nulls_first` reaches this**. That a
        !! descending sort still puts nulls last is Arrow's own rule. `nulls_first` is left
        !! out for a different reason: it only ever REVERSES the tier order, so
        !! `sort_compare_key` applies it once by negating the tier comparison rather than
        !! having this relabel on every call — which is what keeps the whole comparator chain
        !! inside GCC's default inlining budget. Only a real-family key can be tier 1.
        module function sort_tier_of(key, i) result(tier)
            type(sort_key_buf), intent(in) :: key !! the bound key.
            integer(int64), intent(in) :: i       !! row, 1-based.
            integer :: tier                       !! 0, 1 or 2.
        end function sort_tier_of
        !> -1/0/+1 for rows `a` and `b` under ONE key, with its order and null placement applied.
        !!
        !! Two rows in the same non-value tier (both null, or both NaN) compare EQUAL, so the
        !! caller's index tiebreaker keeps them in file order. `descending` negates the answer
        !! within the value tier only.
        module function sort_compare_key(key, a, b) result(c)
            type(sort_key_buf), intent(in) :: key !! the bound key.
            integer(int64), intent(in) :: a       !! first row, 1-based.
            integer(int64), intent(in) :: b       !! second row, 1-based.
            integer :: c                          !! -1, 0 or +1.
        end function sort_compare_key
        !> THE sort comparator: every key in precedence order, then the row index as tiebreaker.
        !!
        !! The index tiebreaker makes this a TOTAL ORDER in which no two distinct rows compare
        !! equal, which is what makes an unstable sort produce the stable answer, makes
        !! nth_element deterministic, and makes a parallel result bit-identical to a serial one
        !! by construction. Keep it beside `sort_keys_compare` -- feature_risks.md Risk-34.
        module function sort_row_less(keys, a, b) result(less)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: a           !! first row, 1-based.
            integer(int64), intent(in) :: b           !! second row, 1-based.
            logical :: less                           !! .true. when `a` sorts before `b`.
        end function sort_row_less
        !> The same ordering as `sort_row_less`, three-way and WITHOUT the index tiebreaker.
        !!
        !! Everything that must recognise "these two rows are equal" -- binary search, run
        !! detection for pf_unique/pf_rank, merging, is_sorted -- needs this one, since under
        !! the tiebreaker no two rows ever are equal. Sorting is the only caller that must NOT
        !! use it. `nkeys` is how many LEADING keys take part, clamped to `size(keys)`: run
        !! detection passes a prefix because "sort by field then magnitude, but group by field
        !! alone" is one pass.
        module function sort_keys_compare(keys, a, b, nkeys) result(c)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: a           !! first row, 1-based.
            integer(int64), intent(in) :: b           !! second row, 1-based.
            integer, intent(in) :: nkeys              !! leading keys taking part.
            integer :: c                              !! -1, 0 or +1.
        end function sort_keys_compare
        !> Fills `perm` with the 1-based permutation that puts rows `1..n` in key order.
        !!
        !! The serial half of the pure-Fortran engine (feature_sort.md Stage 2): an INTROSORT
        !! -- quicksort with median-of-three pivoting, a depth-limited heapsort fallback and a
        !! final insertion pass -- ordering by `sort_row_less` and nothing else.
        !!
        !! **It is unstable, and that is why it is correct.** `sort_row_less` ends with a row
        !! index tiebreaker, so no two distinct rows compare equal and every correct sorting
        !! algorithm produces the SAME permutation -- the stable one. Switching this to a merge
        !! sort to "make it stable" would buy a temporary buffer and change no answer.
        module subroutine sort_comparison_permutation(keys, n, perm)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! rows to order.
            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        end subroutine sort_comparison_permutation
        !> THE engine entry point: the counting fast path where it applies, the introsort otherwise.
        !!
        !! Mirrors the C++ `sort_build_permutation` exactly, including that the range scan's
        !! `lo`/`hi` are carried from the candidate test into the placement pass rather than
        !! rescanned. The two paths answer identically — the counting one is stable by
        !! construction, which is the same answer the comparator's index tiebreaker gives.
        module subroutine sort_build_permutation(keys, n, perm)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! rows to order.
            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        end subroutine sort_build_permutation
        !> THE engine entry point when a thread count is available -- Stage 4.
        !!
        !! Answers **bit-identically to `sort_build_permutation` at every thread count**, and
        !! that is a property of the ordering rather than of the implementation: `sort_row_less`
        !! ends in a row-index tiebreaker, so no two distinct rows compare equal, exactly one
        !! permutation is correct, and every correct algorithm must produce it. A threading bug
        !! therefore shows up as a WRONG permutation, never as a differently-ordered valid one.
        !!
        !! `nthreads` is a resolved count, never a sentinel -- `resolve_thread_count` has already
        !! applied the caller's `threads=`, the automatic policy and the in-parallel rule. This
        !! procedure applies only the two clauses that need the DATA to decide: the row floor
        !! (an internal team-scaled rule), below which a team costs more than it saves,
        !! and one thread meaning the plain serial path. Both are observable through
        !! `parquet_debug_sort_threads_used`, which is the only way a test can see either.
        module subroutine sort_build_permutation_threaded(keys, n, nthreads, perm)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! rows to order.
            integer(int64), intent(in) :: nthreads    !! resolved thread count; 1 sorts serially.
            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        end subroutine sort_build_permutation_threaded
        !> Whether the single-key integer counting sort applies, and over what value range.
        !!
        !! `lo`/`hi` are the key's range over its VALID rows only — a null row's value slot
        !! holds whatever the buffer contained, so including it could widen the range past the
        !! bucket limit and decline the fast path for no reason. An all-null key answers
        !! `.true.` with `lo == hi == 0`, which yields the identity permutation.
        module function sort_counting_candidate(keys, n, lo, hi) result(ok)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only a lone integer key qualifies.
            integer(int64), intent(in) :: n           !! rows.
            integer(int64), intent(out) :: lo         !! smallest valid key value, or 0.
            integer(int64), intent(out) :: hi         !! largest valid key value, or 0.
            logical :: ok                             !! .true. when the counting path applies.
        end function sort_counting_candidate
        !> Fills `perm` by counting sort over `lo..hi`, with the nulls placed as one block.
        !!
        !! Two O(n) passes and no comparisons at all. Stable by construction: the placement
        !! pass walks the input in index order, so equal values are emitted in file order —
        !! the same answer `sort_row_less`'s index tiebreaker produces.
        module subroutine sort_counting_permutation(key, n, lo, hi, perm)
            type(sort_key_buf), intent(in) :: key    !! the lone integer key.
            integer(int64), intent(in) :: n          !! rows.
            integer(int64), intent(in) :: lo         !! smallest valid key value.
            integer(int64), intent(in) :: hi         !! largest valid key value.
            integer(int64), intent(inout) :: perm(:) !! receives `n` 1-based row indices.
        end subroutine sort_counting_permutation
        !> The first `count` entries of the sorted permutation, by heap selection.
        !!
        !! `std::partial_sort`'s algorithm, not a full sort truncated -- a test counts
        !! comparisons to hold that apart. Everything past `count` in `perm` is untouched.
        module subroutine sort_partial_permutation(keys, n, count, perm)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! rows available.
            integer(int64), intent(in) :: count       !! leading entries to order.
            integer(int64), intent(inout) :: perm(:)  !! receives `count` 1-based row indices.
        end subroutine sort_partial_permutation
        !> The row a full sort would place at 1-based rank `nth`, by quickselect.
        !!
        !! Deterministic because the comparator is a total order: there is exactly one row at
        !! that rank, so this and a full sort cannot disagree. `idx` is 0 for an out-of-range
        !! rank, which every caller has already rejected.
        module subroutine sort_nth_index(keys, n, nth, idx)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! rows.
            integer(int64), intent(in) :: nth         !! 1-based rank wanted.
            integer(int64), intent(out) :: idx        !! 1-based row index at that rank.
        end subroutine sort_nth_index
        !> Are rows `1..n` already in order under every key?
        !!
        !! Over `sort_keys_compare`, so adjacent EQUAL rows are in order — the tiebreaker would
        !! turn this into "is the row index ascending".
        module function sort_is_sorted(keys, n) result(answer)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! rows.
            logical :: answer                         !! .true. when already ordered.
        end function sort_is_sorted
        !> Sorts, then flags where the runs of EQUAL rows begin. `tie(1)` is always 0.
        !!
        !! `group_keys` is how many LEADING keys decide a tie; the sort itself always uses every
        !! key. That asymmetry is what produces "grouped by field, ordered within group".
        module subroutine sort_build_runs_permutation(keys, n, group_keys, perm, tie)
            type(sort_key_buf), intent(in) :: keys(:)  !! the keys, in precedence order.
            integer(int64), intent(in) :: n            !! rows.
            integer(int64), intent(in) :: group_keys   !! leading keys that decide a tie.
            integer(int64), intent(inout) :: perm(:)   !! receives `n` 1-based row indices.
            integer(c_int8_t), intent(inout) :: tie(:) !! 1 where a row ties with its predecessor.
        end subroutine sort_build_runs_permutation
        !> Binary search for the target row, which the caller APPENDED as row `n_search + 1`.
        !!
        !! **Preserve the appending.** It is what removes any compare-a-row-against-a-value arm
        !! and so makes drift from the sort comparator structurally impossible — Risk-34.
        module function sort_search_position(keys, n_search, upper) result(pos)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys; row n_search+1 is the target.
            integer(int64), intent(in) :: n_search    !! rows being searched.
            logical, intent(in) :: upper              !! .true. for upper_bound.
            integer(int64) :: pos                     !! 1-based insertion point in 1..n_search+1.
        end function sort_search_position
        !> Merges the already-ordered ranges `1..na` and `na+1..n` into one permutation.
        !!
        !! Ties take from the FIRST range, which is `std::merge`'s stability guarantee and what
        !! makes `pf_merge` agree with `pf_sort` of the concatenation element for element.
        module subroutine sort_merge_permutation(keys, n, na, perm)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.
            integer(int64), intent(in) :: n           !! total rows across both ranges.
            integer(int64), intent(in) :: na          !! rows in the first range.
            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.
        end subroutine sort_merge_permutation
    end interface
    !
    ! ---- Test-only access to the comparator core (parquet_argsort_engine) ----
    interface
        !> Test-only reader for which engine `drive_engine` would use right now.
        module function parquet_debug_using_fortran_sort_engine() result(on)
            logical :: on !! .true. when the Fortran engine is selected.
        end function parquet_debug_using_fortran_sort_engine
        !> Test-only override for the introsort's depth limit; NEGATIVE restores the computed one.
        !!
        !! Zero makes the very first oversized range fall back to heapsort, which is the only
        !! way to reach that arm from a test-sized fixture. Has no effect on the C++ engine.
        !! Also ZEROES the heapsort counter, so a test arms and reads in the obvious order.
        module subroutine parquet_debug_set_sort_depth_limit(n)
            integer, intent(in) :: n !! forced depth limit, or a negative value to restore.
        end subroutine parquet_debug_set_sort_depth_limit
        !> Test-only count of heapsort fallbacks since `parquet_debug_set_sort_depth_limit`.
        !!
        !! What makes the forced-fallback test non-vacuous: the quicksort and heapsort paths
        !! answer identically, so only this counter can say which one ran.
        module function parquet_debug_sort_heapsort_calls() result(n)
            integer(int64) :: n !! heapsort fallbacks entered.
        end function parquet_debug_sort_heapsort_calls
        !> Test-only arming of the final insertion pass's largest-shift tracker, zeroing it too.
        !!
        !! Off by default, because armed it writes process-global state from an ordinary sort.
        module subroutine parquet_debug_set_sort_track_shift(on)
            logical, intent(in) :: on !! .true. arms the tracker.
        end subroutine parquet_debug_set_sort_track_shift
        !> Test-only reader for how far the insertion pass moved anything since it was armed.
        !!
        !! Must not exceed `SORT_INSERTION_CUTOFF` after a correct sort — that is the whole
        !! invariant the quicksort exists to establish, and the only observable that a defect
        !! in the partition or the heapsort has not simply been repaired by the insertion pass.
        module function parquet_debug_sort_max_insertion_shift() result(n)
            integer(int64) :: n !! largest shift, in positions.
        end function parquet_debug_sort_max_insertion_shift
        !> Test-only override for the radix path's row floor; NEGATIVE restores the built-in.
        !!
        !! Used in both directions. A huge value DECLINES the radix path, which is what keeps
        !! the introsort's and the counting path's own negative controls non-vacuous now that
        !! the floor sits below their fixture sizes. A small one drives ordinary fixtures
        !! through the radix path. Has no effect on the C++ engine.
        module subroutine parquet_debug_set_sort_radix_min_rows(n)
            integer(int64), intent(in) :: n !! forced floor, or a negative value to restore.
        end subroutine parquet_debug_set_sort_radix_min_rows
        !> Test-only override for the balanced split's task floor; NEGATIVE restores the
        !! built-in `SORT_TASK_FLOOR`.
        !!
        !! The floor binds only when `nv / team` falls below it -- small `n` with a large
        !! team -- which no fixture in the suite reaches, so without this the constant is
        !! unexercised rather than merely untuned. Also the sweep instrument: a crossover
        !! cannot be located by rebuilding, because it sits inside this project's cross-build
        !! noise floor. Has no effect on the C++ engine.
        module subroutine parquet_debug_set_sort_task_floor(n)
            integer(int64), intent(in) :: n !! forced floor, or a negative value to restore.
        end subroutine parquet_debug_set_sort_task_floor
        !> Test-only override for the TAIL passes' row floor; NEGATIVE restores the built-in.
        !!
        !! Separate from the sort's own floor because the tail is memcpy-shaped and crosses
        !! over an order of magnitude lower; the two shared one setting until this existed,
        !! which meant one number governing two different questions. Has no effect on the
        !! C++ engine.
        module subroutine parquet_debug_set_sort_tail_min_rows(n)
            integer(int64), intent(in) :: n !! forced floor, or a negative value to restore.
        end subroutine parquet_debug_set_sort_tail_min_rows
        !> Test-only override for the Fortran ENGINE's threading floor; NEGATIVE restores it.
        !!
        !! The engine's floor is internal and automatic, and there is no published setting
        !! for it -- `sort_parallel_min_rows` was retired once this rule replaced it.
        module subroutine parquet_debug_set_sort_engine_min_rows(n)
            integer(int64), intent(in) :: n !! forced floor, or a negative value to restore.
        end subroutine parquet_debug_set_sort_engine_min_rows
        !> Test-only override for the counting path's team ceiling; NEGATIVE restores it.
        !!
        !! Setting it to 1 restores the pre-fix behaviour (counting serial-only), which is
        !! how the fix is A/B'd in one binary; setting it high forces counting onto teams
        !! that should decline it. Has no effect on the C++ engine, which has never gated
        !! the counting path on the team at all.
        module subroutine parquet_debug_set_sort_counting_max_threads(n)
            integer(int64), intent(in) :: n !! forced ceiling, or a negative value to restore.
        end subroutine parquet_debug_set_sort_counting_max_threads
        !> Test-only count of parallel refine dispatches in the last string sort; 0 = serial.
        module function parquet_debug_sort_refine_runs() result(n)
            integer(int64) :: n !! runs refined by a team, or sub-bucket loops that opened one.
        end function parquet_debug_sort_refine_runs
        !> Test-only override for the split's minimum distinct-value count; NEGATIVE restores
        !! the built-in `SORT_SPLIT_MIN_CARD`.
        !!
        !! Selects the design at a fixed cardinality: 0 forces refined Design B onto every
        !! key, a huge value forces Design A. Both directions are needed -- one keeps Design
        !! A's own coverage non-vacuous on keys that would otherwise take the split, the
        !! other reaches the split from low-cardinality fixtures. Has no effect on the C++
        !! engine.
        module subroutine parquet_debug_set_sort_split_min_card(n)
            integer(int64), intent(in) :: n !! forced cardinality floor, or negative to restore.
        end subroutine parquet_debug_set_sort_split_min_card
        !> Test-only forcing of an allocation failure in the radix path, to reach its fallbacks.
        !!
        !! Selects WHICH allocation fails, because the two are in series and a single flag
        !! would make the first mask the second: 0 none, 1 the main scratch, 2 the deep
        !! string refine's. The fallback answers identically -- it is the comparison sort --
        !! so no assertion on a permutation can tell it apart from the radix path. Pair this
        !! with the insertion-shift tracker, which can.
        module subroutine parquet_debug_set_sort_radix_fail_alloc(which)
            integer, intent(in) :: which !! 0 none, 1 the main scratch, 2 the refine's.
        end subroutine parquet_debug_set_sort_radix_fail_alloc
        !> Test-only zeroing of the executed-radix-pass counter, before the sort under test.
        module subroutine parquet_debug_reset_sort_radix_passes()
        end subroutine parquet_debug_reset_sort_radix_passes
        !> Test-only count of radix scatter passes executed since that reset.
        !!
        !! What makes a test of any pass-count optimisation non-vacuous: the constant-digit
        !! skip and the narrow-integer bias both leave the permutation bit-identical, so this
        !! is the only thing that can say whether either fired. Zero means the radix path did
        !! not run at all, which is itself worth asserting -- a floor or a decline is easy to
        !! trip by accident and looks exactly like an optimisation working perfectly.
        module function parquet_debug_sort_radix_passes() result(n)
            integer(int64) :: n !! passes executed.
        end function parquet_debug_sort_radix_passes
        !> Test-only count of threads the engine's last permutation build opened; 1 = serial.
        !!
        !! What makes any Stage 4 threading test non-vacuous. The permutation is bit-identical
        !! at every thread count -- the comparator is a total order, so there is exactly one
        !! correct answer -- which means no assertion on `perm` can distinguish a threaded run
        !! from a serial one. A policy that silently refuses to thread is therefore invisible
        !! to every other test in the suite, and is the easiest Stage 4 bug to write.
        !!
        !! Reports the RESOLVED count, not the team the runtime actually granted. Has no
        !! effect on, and says nothing about, the C++ engine.
        module function parquet_debug_sort_threads_used() result(n)
            integer(int64) :: n !! threads resolved for the last build; 1 means serial.
        end function parquet_debug_sort_threads_used
        !> Test-only count of buckets Design B's split produced; 0 means it did not run.
        !!
        !! Design B and the serial LSD loop answer identically by construction, so this is the
        !! only way to tell which one ran -- and therefore the only way any test of the split,
        !! the bucket cap or the balance test can be non-vacuous. Zero is informative rather
        !! than missing: it is exactly what a declined split looks like, which is the normal
        !! outcome on a low-cardinality key.
        module function parquet_debug_sort_split_buckets() result(n)
            integer(int64) :: n !! buckets in the last split; 0 if Design B declined.
        end function parquet_debug_sort_split_buckets
        !> Test-only report of which parallel radix design ran: 0 serial, 1 A, 2 B.
        !!
        !! The three answer identically by construction, so this is the only way a test of the
        !! Design A fallback can be non-vacuous -- A is reached only when B declines, and an
        !! assertion on the permutation cannot distinguish A, B and the serial loop.
        module function parquet_debug_sort_design() result(n)
            integer(int64) :: n !! 0 serial, 1 Design A, 2 Design B.
        end function parquet_debug_sort_design
    end interface
    !
    !
    ! ---- The test-only C++ engine, bound at run time by parquet_sorting_oracle ----
    !
    abstract interface
        !> One C++-engine entry point. `keys` and the pre-allocated outputs are exactly what
        !! the corresponding selector has already prepared, so the oracle does the builder
        !! work and nothing else.
        subroutine oracle_argsort_i(keys, nrows, nthreads, proc, perm)
            import :: sort_key_buf, int64
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key describes.
            integer(int64), intent(in) :: nthreads             !! resolved thread count.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer(int64), intent(inout) :: perm(:)           !! identity-filled by the caller.
        end subroutine oracle_argsort_i
        !> The C++ engine's partial sort; `perm` is allocated and identity-filled already.
        subroutine oracle_partial_i(keys, nrows, count, proc, perm)
            import :: sort_key_buf, int64
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key describes.
            integer(int64), intent(in) :: count                !! leading entries to order.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer(int64), intent(inout) :: perm(:)           !! the first `count` indices.
        end subroutine oracle_partial_i
        !> The C++ engine's nth-element selection.
        subroutine oracle_nth_i(keys, nrows, nth, proc, idx)
            import :: sort_key_buf, int64
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key describes.
            integer(int64), intent(in) :: nth                  !! 1-based rank wanted.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer(int64), intent(out) :: idx                 !! 1-based row index at that rank.
        end subroutine oracle_nth_i
        !> The C++ engine's already-sorted test.
        subroutine oracle_is_sorted_i(keys, nrows, proc, answer)
            import :: sort_key_buf, int64
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key describes.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            logical, intent(out) :: answer                     !! .true. when already in order.
        end subroutine oracle_is_sorted_i
        !> The C++ engine's grouped sort: a permutation plus the tie flags runs are built from.
        subroutine oracle_runs_i(keys, nrows, nthreads, gek, proc, perm, tie)
            import :: sort_key_buf, int64, c_int8_t
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key describes.
            integer(int64), intent(in) :: nthreads             !! resolved thread count.
            integer(int64), intent(in) :: gek                  !! engine keys defining a group.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer(int64), intent(inout) :: perm(:)           !! identity-filled by the caller.
            integer(c_int8_t), intent(inout) :: tie(:)         !! 1 where a row ties the previous.
        end subroutine oracle_runs_i
        !> The C++ engine's binary search over a sorted key.
        subroutine oracle_search_i(keys, nrows, n_search, upper, proc, pos)
            import :: sort_key_buf, int64
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key has, target included.
            integer(int64), intent(in) :: n_search             !! rows to search, target excluded.
            logical, intent(in) :: upper                       !! .true. for upper_bound.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer(int64), intent(out) :: pos                 !! 1-based insertion point.
        end subroutine oracle_search_i
        !> The C++ engine's merge of two sorted runs.
        subroutine oracle_merge_i(keys, nrows, na, proc, perm)
            import :: sort_key_buf, int64
            type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
            integer(int64), intent(in) :: nrows                !! rows each key describes.
            integer(int64), intent(in) :: na                   !! rows belonging to the first input.
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer(int64), intent(inout) :: perm(:)           !! identity-filled by the caller.
        end subroutine oracle_merge_i
    end interface
    !
    !> The bound C++ entry points. All null until parquet_sorting_oracle binds them, which it
    !! does as a side effect of `parquet_debug_use_fortran_sort_engine`, so they cannot be
    !! unbound while `dbg_fortran_engine` is `.false.`. `check_oracle` says what happens if
    !! some future caller finds a way.
    procedure(oracle_argsort_i), pointer, save :: p_argsort => null()
    procedure(oracle_partial_i), pointer, save :: p_partial => null()
    procedure(oracle_nth_i), pointer, save :: p_nth => null()
    procedure(oracle_is_sorted_i), pointer, save :: p_is_sorted => null()
    procedure(oracle_runs_i), pointer, save :: p_runs => null()
    procedure(oracle_search_i), pointer, save :: p_search => null()
    procedure(oracle_merge_i), pointer, save :: p_merge => null()
    !
contains
    !
    !> Binds the seven C++-engine entry points. Called by parquet_sorting_oracle, and by
    !! nothing else; idempotent, so calling it on every engine selection costs nothing.
    subroutine parquet_argsort_bind_oracle(argsort_p, partial_p, nth_p, is_sorted_p, runs_p, &
            search_p, merge_p)
        procedure(oracle_argsort_i) :: argsort_p       !! the whole-permutation entry point.
        procedure(oracle_partial_i) :: partial_p       !! the partial sort.
        procedure(oracle_nth_i) :: nth_p               !! nth-element selection.
        procedure(oracle_is_sorted_i) :: is_sorted_p   !! the already-sorted test.
        procedure(oracle_runs_i) :: runs_p             !! the grouped sort.
        procedure(oracle_search_i) :: search_p         !! the binary search.
        procedure(oracle_merge_i) :: merge_p           !! the merge.
        !
        p_argsort => argsort_p
        p_partial => partial_p
        p_nth => nth_p
        p_is_sorted => is_sorted_p
        p_runs => runs_p
        p_search => search_p
        p_merge => merge_p
    end subroutine parquet_argsort_bind_oracle
    !
    !> Selects which engine the dispatchers run. TEST-ONLY, and reached only through
    !! parquet_sorting_oracle's `parquet_debug_use_fortran_sort_engine`.
    subroutine parquet_argsort_select_engine(use_fortran)
        logical, intent(in) :: use_fortran !! .true. selects the Fortran engine.
        !
        dbg_fortran_engine = use_fortran
    end subroutine parquet_argsort_select_engine
    !
    !> **Relays onto the oracle's procedure pointers, and they exist for a COMPILER
    !! reason rather than a design one -- do not inline them back into the callers.**
    !!
    !! gfortran 15.2 ICEs (`in write_symbol, at lto-streamer-out.cc:3086`, during
    !! `IPA pass: modref`) when a SUBMODULE calls a module-level procedure pointer under
    !! `-flto`, which is what `--profile release` builds with. Every ingredient was
    !! bisected: the optimisation level is irrelevant, so are `save`, `=> null()` and
    !! accessibility, copying the pointer to a local first does NOT help, and a submodule
    !! of a DIFFERENT module that use-associates the pointer fails identically. Calling
    !! from the owning module's own `contains` -- which is what these do -- is clean.
    !!
    !! So no submodule may name a `p_*` pointer at all: even passing
    !! `associated(p_argsort)` as an actual argument reproduces it, though the bare test
    !! alone does not. `check_oracle` is folded in here for that reason, not for brevity.
    !! `check_source_conventions.py`'s `check_no_submodule_oracle_pointer_call` enforces
    !! it, because nothing in CI or a plain `fpm test` builds with `-flto` -- a
    !! reintroduced call would sit in the tree until someone next asked for a release
    !! build. See CLAUDE.md, "Compiler & language gotchas".
    subroutine oracle_argsort(keys, nrows, nthreads, proc, perm)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        integer(int64), intent(in) :: nthreads             !! resolved thread count.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)           !! the permutation to fill.
        !
        call check_oracle(associated(p_argsort), proc)
        call p_argsort(keys, nrows, nthreads, proc, perm)
    end subroutine oracle_argsort
    !
    !> Relay onto `p_partial`; see `oracle_argsort` for why these exist.
    subroutine oracle_partial(keys, nrows, count, proc, perm)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        integer(int64), intent(in) :: count                !! leading rows to order.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)           !! the permutation to fill.
        !
        call check_oracle(associated(p_partial), proc)
        call p_partial(keys, nrows, count, proc, perm)
    end subroutine oracle_partial
    !
    !> Relay onto `p_nth`; see `oracle_argsort` for why these exist.
    subroutine oracle_nth(keys, nrows, nth, proc, idx)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        integer(int64), intent(in) :: nth                  !! the rank wanted, 1-based.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        integer(int64), intent(out) :: idx                 !! the row holding that rank.
        !
        call check_oracle(associated(p_nth), proc)
        call p_nth(keys, nrows, nth, proc, idx)
    end subroutine oracle_nth
    !
    !> Relay onto `p_is_sorted`; see `oracle_argsort` for why these exist.
    subroutine oracle_is_sorted(keys, nrows, proc, answer)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        logical, intent(out) :: answer                     !! whether the rows are ordered.
        !
        call check_oracle(associated(p_is_sorted), proc)
        call p_is_sorted(keys, nrows, proc, answer)
    end subroutine oracle_is_sorted
    !
    !> Relay onto `p_runs`; see `oracle_argsort` for why these exist.
    subroutine oracle_runs(keys, nrows, nthreads, gek, proc, perm, tie)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        integer(int64), intent(in) :: nthreads             !! resolved thread count.
        integer(int64), intent(in) :: gek                  !! engine keys defining a group.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)           !! the permutation to fill.
        integer(c_int8_t), intent(inout) :: tie(:)         !! 1 where a row ties the previous.
        !
        call check_oracle(associated(p_runs), proc)
        call p_runs(keys, nrows, nthreads, gek, proc, perm, tie)
    end subroutine oracle_runs
    !
    !> Relay onto `p_search`; see `oracle_argsort` for why these exist.
    subroutine oracle_search(keys, nrows, n_search, upper, proc, pos)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        integer(int64), intent(in) :: n_search             !! rows belonging to the haystack.
        logical, intent(in) :: upper                       !! upper rather than lower bound.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        integer(int64), intent(out) :: pos                 !! the insertion position found.
        !
        call check_oracle(associated(p_search), proc)
        call p_search(keys, nrows, n_search, upper, proc, pos)
    end subroutine oracle_search
    !
    !> Relay onto `p_merge`; see `oracle_argsort` for why these exist.
    subroutine oracle_merge(keys, nrows, na, proc, perm)
        type(sort_key_buf), intent(in), target :: keys(:)  !! the keys, primary first.
        integer(int64), intent(in) :: nrows                !! rows each key describes.
        integer(int64), intent(in) :: na                   !! rows belonging to the first input.
        character(len=*), intent(in) :: proc               !! calling procedure, for messages.
        integer(int64), intent(inout) :: perm(:)           !! the permutation to fill.
        !
        call check_oracle(associated(p_merge), proc)
        call p_merge(keys, nrows, na, proc, perm)
    end subroutine oracle_merge
    !
    !> Aborts if the C++ engine was selected without being bound.
    !!
    !! **Unreachable by construction, and kept anyway.** The only way to clear
    !! `dbg_fortran_engine` is `parquet_debug_use_fortran_sort_engine`, which binds the
    !! pointers before it clears the flag -- so a build that can select the C++ engine has
    !! already imported the oracle. It must NEVER be softened into a silent fall back to the
    !! Fortran engine: the A/B conformance tests would then compare that engine against
    !! itself and pass, which is exactly the vacuous agreement they exist to rule out.
    subroutine check_oracle(bound, proc)
        logical, intent(in) :: bound          !! whether the entry point is associated.
        character(len=*), intent(in) :: proc  !! calling procedure, for the message.
        !
        if (.not. bound) then ! GCOVR_EXCL_START -- unreachable; see the note above.
            error stop EP // proc // ": the C++ sort engine was selected but is not bound; " // &
                "add `use parquet_sorting_oracle` to the program that selects it"
        end if ! GCOVR_EXCL_STOP
    end subroutine check_oracle
end module parquet_argsort ! GCOVR_EXCL_LINE
