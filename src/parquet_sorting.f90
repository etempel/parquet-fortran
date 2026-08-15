!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> Sorting for plain Fortran arrays and for this library's own column types.
!!
!! This module is the public face of the same C++ `std::sort` engine that orders a read-time
!! `parquet_open_reader(..., sort_by=)` and `parquet_table%sort_by`. Sharing one engine is the
!! point: a read-time sort, a table sort and a raw-array sort can never disagree about where
!! nulls go, where NaNs go, or how ties are broken.
!!
!! **Naming.** Everything public here carries the `pf_` prefix (parquet-fortran) rather than
!! `parquet_`, because the subject is not a parquet file -- see CLAUDE.md's "Naming
!! conventions". The module is `parquet_sorting` rather than `parquet_sort` because a module
!! cannot share its name with a procedure it declares.
!!
!! Four operations, over eleven element types:
!!
!! * `pf_argsort(values, perm)` -- the permutation that would sort `values`. Never modifies it.
!! * `pf_sort(values, sorted)` -- an independent sorted copy. Never modifies its input.
!! * `pf_permute(values, perm)` -- applies a permutation to `values` IN PLACE.
!! * `pf_is_sorted(values, answer)` -- whether `values` is already in the stated order.
!!
!! **Ordering reproduces `arrow::compute::SortIndices` exactly.** Null and NaN placement is
!! absolute: `descending` reverses the values, never the tiers. Ascending gives values, then
!! NaNs, then nulls; `nulls_first=.true.` gives nulls, then NaNs, then values. Ties always keep
!! their original order -- **every sort here is stable, unconditionally**, so there is no
!! `stable=` argument to pass.
!!
!! **Where nullness comes from depends on the type.** The six types with no null state of their
!! own (`integer`, `real`, `logical`, `character`) take an optional `is_valid(:)` mask; the
!! temporal types and the two column types carry their own and take no such argument.
!!
!! **Sorting one column of a table desynchronises it.** `%col` hands back a writable pointer
!! into a table's live storage, so `call pf_permute(p, perm)` on it reorders that column and
!! leaves every other column where it was, silently breaking row correspondence. Use
!! `parquet_table%sort_by`, which reorders every column together.
module parquet_sorting
    use, intrinsic :: iso_fortran_env, only : int8, int32, int64, real32, real64
    use iso_c_binding, only : c_ptr, c_loc, c_null_ptr, c_int8_t, c_char, c_long_long
    use parquet_bindings, only : parquet_sort_builder_new, parquet_sort_builder_add_key_int64, &
        parquet_sort_builder_add_key_double, parquet_sort_builder_add_key_string, &
        parquet_sort_builder_build, parquet_sort_builder_is_sorted, parquet_sort_builder_free, &
        parquet_sort_argsort_int64, parquet_sort_argsort_double, parquet_sort_argsort_string, &
        parquet_sort_is_sorted_int64, parquet_sort_is_sorted_double, parquet_sort_is_sorted_string, &
        parquet_sort_builder_build_partial, parquet_sort_builder_nth_element, &
        parquet_sort_partial_argsort_int64, parquet_sort_partial_argsort_double, &
        parquet_sort_partial_argsort_string, parquet_sort_nth_index_int64, &
        parquet_sort_nth_index_double, parquet_sort_nth_index_string, &
        parquet_sort_builder_build_runs, parquet_sort_builder_search, parquet_sort_builder_merge
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    use parquet_strings, only : parquet_string_column
    use parquet_temporal, only : parquet_date, parquet_time, parquet_timestamp
    use parquet_columns, only : parquet_column, parquet_kind_name, PK_INT32, PK_INT64, PK_FLOAT32, &
        PK_FLOAT64, PK_LOGICAL, PK_STRING, PK_DATE, PK_TIME, PK_TIMESTAMP
    !
    implicit none
    private
    !
    public :: pf_sort_keys
    public :: pf_sort
    public :: pf_argsort
    public :: pf_permute
    public :: pf_is_sorted
    public :: pf_partial_sort
    public :: pf_partial_argsort
    public :: pf_nth_element
    public :: pf_nth_quantile
    public :: pf_lower_bound
    public :: pf_upper_bound
    public :: pf_equal_range
    public :: pf_unique_count
    public :: pf_unique
    public :: pf_rank
    public :: pf_minmax
    public :: pf_argminmax
    public :: pf_merge
    public :: pf_sort_threads
    !
    ! Test-only, and PUBLIC because there is no other route: they expose the comparator core, whose
    ! state (`sort_key_buf`) is private to this module. CLAUDE.md's "A Fortran-side debug hook has
    ! to be PUBLIC, so prefer a C++ one" states the rule and the accepted precedents; the C++ route
    ! is unavailable here precisely because Stage 1 exists to move this decision OUT of C++.
    ! No library code calls either, neither appears in README.md's API overview, and neither is
    ! mentioned in any doc/pages/ guide -- see feature_sort.md section 7.4.
    public :: parquet_debug_sort_row_less
    public :: parquet_debug_sort_keys_compare
    public :: parquet_debug_sort_sweep_less
    public :: parquet_debug_sort_sweep_compare
    public :: parquet_debug_use_fortran_sort_engine
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
    public :: parquet_debug_set_sort_split_min_card
    public :: parquet_debug_set_sort_radix_fail_alloc
    public :: parquet_debug_reset_sort_radix_passes
    public :: parquet_debug_sort_radix_passes
    public :: parquet_debug_sort_threads_used
    public :: parquet_debug_sort_split_buckets
    public :: parquet_debug_sort_design
    !
    !> Error-message prefix for every `error stop` raised by this module.
    character(len=*), parameter :: EP = "parquet_sorting: "
    !
    ! ---- Stage 2 engine selection: TEST-ONLY SCAFFOLDING, deleted at the Stage 6 cutover --------
    !
    ! feature_sort.md's Stage 2 requires BOTH engines to stay reachable, so that the conformance
    ! tests and the A/B benchmark can run them over the same data through the same public entry
    ! point. It is deliberately NOT a `parquet_settings` knob: that module admits a setting only
    ! when it changes how fast, how large or how loud the library runs and never what it ANSWERS,
    ! and an engine selector is exactly a second way to get a different answer should the two ever
    ! disagree. It is also why these are `parquet_debug_*` and absent from README.md's API overview.
    !
    ! Both are process-global saved state, which is why the `sorting` and `sort` suites must stay
    ! excluded from test-drive's per-test parallelism (test/run_tester.f90) -- they already are.
    logical, save :: dbg_fortran_engine = .false. !! .true. routes `drive_engine` to the Fortran sort.
    !> .true. once the affinity-clamp warning has been emitted, so it is said once per process
    !! rather than once per sort. Written without synchronisation -- see `warn_thread_clamp`, which
    !! explains why a duplicated diagnostic is preferable to a lock on every sort's resolution path.
    logical, save :: warned_thread_clamp = .false.
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
    !! one number, `sort_parallel_min_rows`, which therefore could not be right for both. This hook
    !! is what lets the tail's own crossover be measured without disturbing the sort's.
    integer(int64), save :: dbg_sort_tail_min_rows = -1_int64
    !> Overrides the Fortran ENGINE's own threading floor; NEGATIVE restores the built-in rule.
    !!
    !! Distinct from `parquet_set_sort_parallel_min_rows`, which remains the published knob and
    !! still governs the C++ engine. The Fortran engine's floor is internal and automatic -- a
    !! measured function of the team -- so this hook is the only way to move it, and is what
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
    ! ---- Fractional-position rounding for pf_nth_quantile ----
    integer, parameter :: RND_NEAREST = 1 !! round a fractional rank to the nearest whole one.
    integer, parameter :: RND_DOWN = 2    !! round a fractional rank down.
    integer, parameter :: RND_UP = 3      !! round a fractional rank up.
    !
    ! ---- Tie handling for pf_rank ----
    integer, parameter :: RANK_COMPETITION = 1 !! ties share the lower rank; the next gap is skipped.
    integer, parameter :: RANK_DENSE = 2       !! ties share a rank and no rank is skipped.
    integer, parameter :: RANK_ORDINAL = 3     !! every element gets its own rank, ties in file order.
    !
    ! ---- Which bound pf_lower_bound/pf_upper_bound/pf_equal_range want ----
    integer, parameter :: SRCH_LOWER = 1 !! the first position not ordered before the target.
    integer, parameter :: SRCH_UPPER = 2 !! the first position the target is ordered before.
    integer, parameter :: SRCH_BOTH = 3  !! both, from one extraction.
    !
    !> One extracted sort key, in the canonical form the C++ engine takes.
    !!
    !! Exactly one of `ints`/`reals`/(`offsets`,`data`) is allocated, matching `family`. `valid`
    !! is left UNALLOCATED when the key has no nulls at all, which is the engine's own fast path
    !! -- the same convention `parquet_column%row_validity` already uses.
    type :: sort_key_buf
        private
        integer :: family = SK_INT                          !! SK_INT / SK_REAL / SK_STR.
        logical :: descending = .false.                     !! .true. sorts high to low.
        logical :: nulls_first = .false.                    !! .true. places nulls before values.
        integer(int64), allocatable :: ints(:)              !! SK_INT values.
        real(real64), allocatable :: reals(:)               !! SK_REAL values.
        integer(int64), allocatable :: offsets(:)           !! SK_STR: n+1 byte offsets, 0-based.
        character(kind=c_char), allocatable :: data(:)      !! SK_STR: the packed bytes.
        integer(c_int8_t), allocatable :: valid(:)          !! 1 = valid; UNALLOCATED means no nulls.
    end type sort_key_buf
    !
    !> A list of sort keys, applied in the order added -- the first key added is the primary one.
    !!
    !! This is how a multi-key sort is expressed, because Fortran cannot offer "an optional
    !! second and third array, each of any type" as a generic: with eleven element types that
    !! would need over a thousand specific procedures. Add as many keys as needed, of any mix of
    !! types, then hand the object to `pf_argsort`:
    !!
    !! ```fortran
    !! type(pf_sort_keys) :: k
    !! call k%add(ra)                          ! primary
    !! call k%add(dec, descending=.true.)      ! breaks ties on ra
    !! call k%add(name)                        ! breaks ties on both
    !! call pf_argsort(k, perm)
    !! ```
    !!
    !! **It holds no C handle**, deliberately: its keys are ordinary allocatable Fortran arrays,
    !! and the C++ builder is created, used and freed entirely inside `pf_argsort`. That keeps
    !! this type free of a `FINAL`, free of an assignment guard, and -- because a finalizable
    !! type must never be given to OpenMP's `private()` -- usable per-thread in the obvious way.
    type :: pf_sort_keys
        private
        integer :: nkeys = 0                                !! ENGINE keys held; see `add_ekeys`.
        integer(int64) :: nrows = -1                        !! rows every key must have; -1 until the first add.
        type(sort_key_buf), allocatable :: keys(:)          !! the keys, in precedence order.
        !> engine keys contributed by each `%add` call, one entry per call. Almost always 1, but a
        !! `parquet_timestamp` key binds as TWO engine keys (a seconds/nanoseconds split), so
        !! `nkeys` above is not the number of keys the caller added and must never be reported as
        !! such. This array is what translates between the two, for `%nkeys_added` and for
        !! `group_nkeys`, and its SIZE -- not `nkeys` -- is the caller's key count.
        integer, allocatable :: add_ekeys(:)
    contains
        procedure, private :: add_i32 !! %add specific for a 32-bit integer key.
        procedure, private :: add_i64 !! %add specific for a 64-bit integer key.
        procedure, private :: add_f32 !! %add specific for a 32-bit real key.
        procedure, private :: add_f64 !! %add specific for a 64-bit real key.
        procedure, private :: add_bool !! %add specific for a logical key.
        procedure, private :: add_chr !! %add specific for a string key.
        procedure, private :: add_date !! %add specific for a date key.
        procedure, private :: add_time !! %add specific for a time key.
        procedure, private :: add_ts !! %add specific for a timestamp key.
        procedure, private :: add_strcol !! %add specific for a packed string column key.
        procedure, private :: add_col !! %add specific for a type-erased column key.
        !> Appends one sort key. Keys apply in the order added, the first being primary.
        generic :: add => add_i32, add_i64, add_f32, add_f64, add_bool, add_chr, add_date, add_time, add_ts, add_strcol, &
            add_col
        procedure :: nkeys_added => keys_count !! Keys added so far, one per %add call.
        procedure :: clear => keys_clear       !! Drops every key, leaving the object reusable.
    end type pf_sort_keys
    !
    !> The permutation that would sort `values`: `perm(k)` is the index of the element that
    !> belongs at position k. `values` is never modified.
    !>
    !> The permutation's integer kind is chosen by how the caller declares `perm`. The
    !> `integer(int32)` form aborts when the array is longer than `huge(1_int32)` rather than
    !> truncating; declare `perm` as `integer(int64)` for arrays that large.
    !>
    !> Also takes a `pf_sort_keys` object in place of `values`, for a multi-key sort.
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
        module procedure argsort_date_i32
        module procedure argsort_date_i64
        module procedure argsort_time_i32
        module procedure argsort_time_i64
        module procedure argsort_ts_i32
        module procedure argsort_ts_i64
        module procedure argsort_strcol_i32
        module procedure argsort_strcol_i64
        module procedure argsort_col_i32
        module procedure argsort_col_i64
        module procedure argsort_keys_i32
        module procedure argsort_keys_i64
    end interface pf_argsort
    !
    !> An independent sorted copy of `values`, leaving `values` untouched.
    !>
    !> Deliberately not defined for `parquet_string_column` or `parquet_column`: copying a
    !> whole column to sort it serves no purpose, and reordering one in place is
    !> `pf_argsort` followed by `pf_permute`, which says what it does at the call site.
    interface pf_sort
        module procedure sort_i32
        module procedure sort_i64
        module procedure sort_f32
        module procedure sort_f64
        module procedure sort_bool
        module procedure sort_chr
        module procedure sort_date
        module procedure sort_time
        module procedure sort_ts
    end interface pf_sort
    !
    !> Applies `perm` to `values` IN PLACE: afterwards element k is what was at `perm(k)`.
    !> `perm` itself is not modified.
    !>
    !> `perm` is validated as a true permutation of 1..n before anything is written, since an
    !> invalid one would silently duplicate some elements and drop others. Pass
    !> `assume_valid=.true.` to skip that check when the permutation came from `pf_argsort`
    !> and is known good -- it means the same thing for all eleven types, the two column ones
    !> included.
    !>
    !> **`assume_valid` skips the O(n) contents check only.** `perm`'s LENGTH is checked either
    !> way, because a short permutation would make the gather read past the end of `values` and
    !> no promise from the caller can make that defined.
    interface pf_permute
        module procedure permute_i32_i32
        module procedure permute_i32_i64
        module procedure permute_i64_i32
        module procedure permute_i64_i64
        module procedure permute_f32_i32
        module procedure permute_f32_i64
        module procedure permute_f64_i32
        module procedure permute_f64_i64
        module procedure permute_bool_i32
        module procedure permute_bool_i64
        module procedure permute_chr_i32
        module procedure permute_chr_i64
        module procedure permute_date_i32
        module procedure permute_date_i64
        module procedure permute_time_i32
        module procedure permute_time_i64
        module procedure permute_ts_i32
        module procedure permute_ts_i64
        module procedure permute_strcol_i32
        module procedure permute_strcol_i64
        module procedure permute_col_i32
        module procedure permute_col_i64
    end interface pf_permute
    !
    !> Whether `values` is already in the stated order. O(n) with an early exit, and no copy.
    !>
    !> Uses the same comparison `pf_sort` does, so the two can never disagree about nulls,
    !> NaNs or direction on one array. A run of equal values is sorted.
    interface pf_is_sorted
        module procedure is_sorted_i32
        module procedure is_sorted_i64
        module procedure is_sorted_f32
        module procedure is_sorted_f64
        module procedure is_sorted_bool
        module procedure is_sorted_chr
        module procedure is_sorted_date
        module procedure is_sorted_time
        module procedure is_sorted_ts
        module procedure is_sorted_strcol
        module procedure is_sorted_col
        module procedure is_sorted_keys
    end interface pf_is_sorted
    !
    !> The permutation that would sort the FIRST `n` elements of `values`, without ordering
    !> the rest. `perm` comes back with exactly `n` entries (fewer if the array is shorter).
    !>
    !> `n` is CLAMPED to the array size rather than being an error, so a caller whose `n` is
    !> derived -- a fraction of a row count, a config value, a post-filter survivor count --
    !> needs no `min(n, size(v))` of their own. A negative `n` is still an error.
    !>
    !> "The last n" is `descending=.true.`, not a separate procedure.
    interface pf_partial_argsort
        module procedure partial_argsort_i32_i32
        module procedure partial_argsort_i32_i64
        module procedure partial_argsort_i64_i32
        module procedure partial_argsort_i64_i64
        module procedure partial_argsort_f32_i32
        module procedure partial_argsort_f32_i64
        module procedure partial_argsort_f64_i32
        module procedure partial_argsort_f64_i64
        module procedure partial_argsort_bool_i32
        module procedure partial_argsort_bool_i64
        module procedure partial_argsort_chr_i32
        module procedure partial_argsort_chr_i64
        module procedure partial_argsort_date_i32
        module procedure partial_argsort_date_i64
        module procedure partial_argsort_time_i32
        module procedure partial_argsort_time_i64
        module procedure partial_argsort_ts_i32
        module procedure partial_argsort_ts_i64
        module procedure partial_argsort_strcol_i32
        module procedure partial_argsort_strcol_i64
        module procedure partial_argsort_col_i32
        module procedure partial_argsort_col_i64
        module procedure partial_argsort_keys_i32
        module procedure partial_argsort_keys_i64
    end interface pf_partial_argsort
    !
    !> The first `n` elements of `values` in order, as an independent copy of length `n`.
    !> Same clamping rule as `pf_partial_argsort`. Never modifies its input.
    !>
    !> Cheaper than `pf_sort` only while `n` stays well below the array size -- the underlying
    !> `std::partial_sort` degrades past a full sort as `n` approaches it. At `n = size` this
    !> is strictly worse than calling `pf_sort`.
    interface pf_partial_sort
        module procedure partial_sort_i32
        module procedure partial_sort_i64
        module procedure partial_sort_f32
        module procedure partial_sort_f64
        module procedure partial_sort_bool
        module procedure partial_sort_chr
        module procedure partial_sort_date
        module procedure partial_sort_time
        module procedure partial_sort_ts
    end interface pf_partial_sort
    !
    !> The element a full sort would place at 1-based rank `nth`, without sorting -- O(n)
    !> rather than O(n log n). `index` optionally reports which element of `values` that was.
    !>
    !> **The reported index is the one a full STABLE sort would give.** `std::nth_element`
    !> normally leaves an arbitrary member of an equal-comparing run at that position; here
    !> the comparator ends with a tiebreaker on the original index, making it a total order
    !> under which no two elements compare equal, so the answer is deterministic and agrees
    !> with `pf_sort` element for element.
    !>
    !> `nth` counts NULLS too, placed by the same tier rules as the sort (last by default).
    !> Takes `descending`/`nulls_first`/`is_valid` exactly as `pf_argsort` does.
    interface pf_nth_element
        module procedure nth_i32_i32
        module procedure nth_i32_i32_i32
        module procedure nth_i32_i32_i64
        module procedure nth_i32_i64
        module procedure nth_i32_i64_i32
        module procedure nth_i32_i64_i64
        module procedure nth_i64_i32
        module procedure nth_i64_i32_i32
        module procedure nth_i64_i32_i64
        module procedure nth_i64_i64
        module procedure nth_i64_i64_i32
        module procedure nth_i64_i64_i64
        module procedure nth_f32_i32
        module procedure nth_f32_i32_i32
        module procedure nth_f32_i32_i64
        module procedure nth_f32_i64
        module procedure nth_f32_i64_i32
        module procedure nth_f32_i64_i64
        module procedure nth_f64_i32
        module procedure nth_f64_i32_i32
        module procedure nth_f64_i32_i64
        module procedure nth_f64_i64
        module procedure nth_f64_i64_i32
        module procedure nth_f64_i64_i64
        module procedure nth_bool_i32
        module procedure nth_bool_i32_i32
        module procedure nth_bool_i32_i64
        module procedure nth_bool_i64
        module procedure nth_bool_i64_i32
        module procedure nth_bool_i64_i64
        module procedure nth_chr_i32
        module procedure nth_chr_i32_i32
        module procedure nth_chr_i32_i64
        module procedure nth_chr_i64
        module procedure nth_chr_i64_i32
        module procedure nth_chr_i64_i64
        module procedure nth_date_i32
        module procedure nth_date_i32_i32
        module procedure nth_date_i32_i64
        module procedure nth_date_i64
        module procedure nth_date_i64_i32
        module procedure nth_date_i64_i64
        module procedure nth_time_i32
        module procedure nth_time_i32_i32
        module procedure nth_time_i32_i64
        module procedure nth_time_i64
        module procedure nth_time_i64_i32
        module procedure nth_time_i64_i64
        module procedure nth_ts_i32
        module procedure nth_ts_i32_i32
        module procedure nth_ts_i32_i64
        module procedure nth_ts_i64
        module procedure nth_ts_i64_i32
        module procedure nth_ts_i64_i64
        module procedure nth_strcol_i32
        module procedure nth_strcol_i32_i32
        module procedure nth_strcol_i32_i64
        module procedure nth_strcol_i64
        module procedure nth_strcol_i64_i32
        module procedure nth_strcol_i64_i64
    end interface pf_nth_element
    !
    !> The value at `quantile` (on a **0-1 scale**, not 0-100) of the NON-NULL values.
    !> `index` optionally reports which element that was; `n_null` how many were excluded.
    !>
    !> **Nulls are excluded from the population, not placed in it** -- unlike every other
    !> operation in this module, which is why this one takes neither `descending` nor
    !> `nulls_first`: there is no null tier to position, and a descending quantile is just
    !> `1 - quantile`.
    !>
    !> `rounding=` selects how a fractional position is resolved: `"nearest"` (the default),
    !> `"down"` or `"up"`, matched case-insensitively. An unrecognized token aborts.
    !>
    !> Aborts when EVERY value is null: there is no value to return, and no sentinel exists
    !> across all ten types. `n_null` is for PARTIAL nullness; the all-null case never
    !> reaches it. Guard with `count(mask)` (or a column's own null count) if that matters.
    interface pf_nth_quantile
        module procedure quantile_i32
        module procedure quantile_i32_i32
        module procedure quantile_i32_i64
        module procedure quantile_i64
        module procedure quantile_i64_i32
        module procedure quantile_i64_i64
        module procedure quantile_f32
        module procedure quantile_f32_i32
        module procedure quantile_f32_i64
        module procedure quantile_f64
        module procedure quantile_f64_i32
        module procedure quantile_f64_i64
        module procedure quantile_bool
        module procedure quantile_bool_i32
        module procedure quantile_bool_i64
        module procedure quantile_chr
        module procedure quantile_chr_i32
        module procedure quantile_chr_i64
        module procedure quantile_date
        module procedure quantile_date_i32
        module procedure quantile_date_i64
        module procedure quantile_time
        module procedure quantile_time_i32
        module procedure quantile_time_i64
        module procedure quantile_ts
        module procedure quantile_ts_i32
        module procedure quantile_ts_i64
        module procedure quantile_strcol
        module procedure quantile_strcol_i32
        module procedure quantile_strcol_i64
    end interface pf_nth_quantile
    !
    !> The first position at which `target` could be inserted into an already-sorted
    !> `values` without breaking its order -- i.e. the first element not ordered BEFORE it.
    !>
    !> `pos` lands in `1 .. size(values)+1`; it is `size(values)+1` when every element is
    !> ordered before the target. Together with `pf_upper_bound` it brackets every element
    !> equal to the target, which is what `pf_equal_range` returns in one call.
    !>
    !> **`values` is checked for sortedness first, and that check is O(n).** Searching an
    !> unsorted array returns a plausible index with no symptom at all, so the check is on by
    !> default. Check once with `pf_is_sorted` and pass `assume_sorted=.true.` in a loop:
    !>
    !> ```fortran
    !> call pf_is_sorted(v, ok)                       ! O(N), once
    !> do k = 1, m
    !>     call pf_lower_bound(v, targets(k), pos, assume_sorted=.true.)   ! O(log N) each
    !> end do
    !> ```
    !>
    !> `descending`/`nulls_first` must describe the order `values` is ACTUALLY in -- they
    !> select the comparison, they do not reorder anything.
    interface pf_lower_bound
        module procedure lower_bound_i32_i32
        module procedure lower_bound_i32_i64
        module procedure lower_bound_i64_i32
        module procedure lower_bound_i64_i64
        module procedure lower_bound_f32_i32
        module procedure lower_bound_f32_i64
        module procedure lower_bound_f64_i32
        module procedure lower_bound_f64_i64
        module procedure lower_bound_bool_i32
        module procedure lower_bound_bool_i64
        module procedure lower_bound_chr_i32
        module procedure lower_bound_chr_i64
        module procedure lower_bound_date_i32
        module procedure lower_bound_date_i64
        module procedure lower_bound_time_i32
        module procedure lower_bound_time_i64
        module procedure lower_bound_ts_i32
        module procedure lower_bound_ts_i64
        module procedure lower_bound_strcol_i32
        module procedure lower_bound_strcol_i64
    end interface pf_lower_bound
    !
    !> The first position at which `target` is ordered BEFORE the element there -- i.e. one
    !> past the last element equal to the target.
    !>
    !> Same arguments, same sortedness rule and same `1 .. size(values)+1` range as
    !> `pf_lower_bound`; `pf_upper_bound - pf_lower_bound` is how many elements equal the
    !> target.
    interface pf_upper_bound
        module procedure upper_bound_i32_i32
        module procedure upper_bound_i32_i64
        module procedure upper_bound_i64_i32
        module procedure upper_bound_i64_i64
        module procedure upper_bound_f32_i32
        module procedure upper_bound_f32_i64
        module procedure upper_bound_f64_i32
        module procedure upper_bound_f64_i64
        module procedure upper_bound_bool_i32
        module procedure upper_bound_bool_i64
        module procedure upper_bound_chr_i32
        module procedure upper_bound_chr_i64
        module procedure upper_bound_date_i32
        module procedure upper_bound_date_i64
        module procedure upper_bound_time_i32
        module procedure upper_bound_time_i64
        module procedure upper_bound_ts_i32
        module procedure upper_bound_ts_i64
        module procedure upper_bound_strcol_i32
        module procedure upper_bound_strcol_i64
    end interface pf_upper_bound
    !
    !> The INCLUSIVE range `first .. last` of elements equal to `target`, from one pass.
    !>
    !> `first` is `pf_lower_bound`'s answer and `last` is `pf_upper_bound`'s minus one, so a
    !> target that is absent comes back with `last == first - 1` and `last - first + 1 == 0`.
    !> Do not read `values(first)` without checking that count first.
    !>
    !> Cheaper than calling the two bounds separately: the values are extracted once.
    interface pf_equal_range
        module procedure equal_range_i32_i32
        module procedure equal_range_i32_i64
        module procedure equal_range_i64_i32
        module procedure equal_range_i64_i64
        module procedure equal_range_f32_i32
        module procedure equal_range_f32_i64
        module procedure equal_range_f64_i32
        module procedure equal_range_f64_i64
        module procedure equal_range_bool_i32
        module procedure equal_range_bool_i64
        module procedure equal_range_chr_i32
        module procedure equal_range_chr_i64
        module procedure equal_range_date_i32
        module procedure equal_range_date_i64
        module procedure equal_range_time_i32
        module procedure equal_range_time_i64
        module procedure equal_range_ts_i32
        module procedure equal_range_ts_i64
        module procedure equal_range_strcol_i32
        module procedure equal_range_strcol_i64
    end interface pf_equal_range
    !
    !> How many DISTINCT non-null values `values` holds. `n_null` optionally reports how many
    !> were null.
    !>
    !> **Nulls are excluded from the population, not counted as one value** -- the same rule
    !> `pf_nth_quantile` follows, and the reason this takes neither `descending` (a count does
    !> not depend on direction) nor `nulls_first` (there is no null tier to place).
    !>
    !> Distinctness is the sort comparator's own equality, so on a floating-point array it is
    !> EXACT: `0.1 + 0.2` and `0.3` are two distinct values. Every NaN counts as one value,
    !> collectively, since NaNs compare equal to each other here (they do not under `==`).
    interface pf_unique_count
        module procedure unique_count_i32_i32
        module procedure unique_count_i32_i64
        module procedure unique_count_i64_i32
        module procedure unique_count_i64_i64
        module procedure unique_count_f32_i32
        module procedure unique_count_f32_i64
        module procedure unique_count_f64_i32
        module procedure unique_count_f64_i64
        module procedure unique_count_bool_i32
        module procedure unique_count_bool_i64
        module procedure unique_count_chr_i32
        module procedure unique_count_chr_i64
        module procedure unique_count_date_i32
        module procedure unique_count_date_i64
        module procedure unique_count_time_i32
        module procedure unique_count_time_i64
        module procedure unique_count_ts_i32
        module procedure unique_count_ts_i64
        module procedure unique_count_strcol_i32
        module procedure unique_count_strcol_i64
        module procedure unique_count_col_i32
        module procedure unique_count_col_i64
    end interface pf_unique_count
    !
    !> The distinct non-null values of `values`, in order, as an independent copy.
    !>
    !> Same distinctness rule as `pf_unique_count` -- exact for reals, all NaNs collapsing to
    !> one. `descending` chooses the order the distinct values come back in; there is no
    !> `nulls_first`, because nulls are excluded rather than placed.
    !>
    !> Each distinct value is taken from its FIRST occurrence in the sorted order, which for
    !> equal-comparing-but-not-identical values (a `character` array's trailing blanks, a
    !> `parquet_string_column`'s empty strings) is the earliest such element of `values`.
    interface pf_unique
        module procedure unique_i32
        module procedure unique_i64
        module procedure unique_f32
        module procedure unique_f64
        module procedure unique_bool
        module procedure unique_chr
        module procedure unique_date
        module procedure unique_time
        module procedure unique_ts
        module procedure unique_strcol
    end interface pf_unique
    !
    !> The rank of every element of `values`, without reordering it. `ranks(i)` is the rank of
    !> `values(i)`, so this is a per-element answer rather than a permutation.
    !>
    !> `method=` chooses how ties are handled, matched case-insensitively:
    !>
    !> | token | ranks of `10, 20, 20, 30` |
    !> |---|---|
    !> | `"competition"` (the default) | 1, 2, 2, 4 |
    !> | `"dense"` | 1, 2, 2, 3 |
    !> | `"ordinal"` | 1, 2, 3, 4 |
    !>
    !> **A null gets rank 0**, which is why this takes `descending` but NOT `nulls_first`: a
    !> null has no rank at all, so there is no position for `nulls_first` to choose. NaNs are
    !> ranked as ordinary values (all tying with each other), unlike nulls.
    !>
    !> `"ordinal"` ranks are exactly the inverse of `pf_argsort`'s permutation.
    interface pf_rank
        module procedure rank_i32_i32
        module procedure rank_i32_i64
        module procedure rank_i64_i32
        module procedure rank_i64_i64
        module procedure rank_f32_i32
        module procedure rank_f32_i64
        module procedure rank_f64_i32
        module procedure rank_f64_i64
        module procedure rank_bool_i32
        module procedure rank_bool_i64
        module procedure rank_chr_i32
        module procedure rank_chr_i64
        module procedure rank_date_i32
        module procedure rank_date_i64
        module procedure rank_time_i32
        module procedure rank_time_i64
        module procedure rank_ts_i32
        module procedure rank_ts_i64
        module procedure rank_strcol_i32
        module procedure rank_strcol_i64
        module procedure rank_col_i32
        module procedure rank_col_i64
    end interface pf_rank
    !
    !> The smallest and largest value in `values`, skipping nulls and NaNs.
    !>
    !> Takes no `descending`/`nulls_first`: a minimum and a maximum are absolute, and reversing
    !> the order would only exchange the two answers.
    !>
    !> **Aborts when every value is null or NaN** -- there is nothing to return, and no
    !> sentinel exists across all nine types. This matches `pf_nth_quantile`'s decision for the
    !> same degenerate case; guard with `count(is_valid)` where that can happen.
    !>
    !> Use `pf_argminmax` when the positions matter rather than the values.
    interface pf_minmax
        module procedure minmax_i32
        module procedure minmax_i64
        module procedure minmax_f32
        module procedure minmax_f64
        module procedure minmax_chr
        module procedure minmax_date
        module procedure minmax_time
        module procedure minmax_ts
        module procedure minmax_strcol
    end interface pf_minmax
    !
    !> WHERE the smallest and largest value of `values` are: `imin`/`imax` are 1-based indices
    !> into `values`, skipping nulls and NaNs.
    !>
    !> The index-returning twin of `pf_minmax`, split off because Fortran cannot offer both
    !> answers from one generic -- optional `imin`/`imax` varying only by integer kind would
    !> make a positional call ambiguous. A caller wanting both pays one extra call.
    !>
    !> Ties report the FIRST occurrence, which is the element a full stable sort would place at
    !> either end. Aborts on an all-null-or-NaN input, exactly as `pf_minmax` does. Defined for
    !> `parquet_column` as well, since an index needs no compile-time element type.
    interface pf_argminmax
        module procedure argminmax_i32_i32
        module procedure argminmax_i32_i64
        module procedure argminmax_i64_i32
        module procedure argminmax_i64_i64
        module procedure argminmax_f32_i32
        module procedure argminmax_f32_i64
        module procedure argminmax_f64_i32
        module procedure argminmax_f64_i64
        module procedure argminmax_chr_i32
        module procedure argminmax_chr_i64
        module procedure argminmax_date_i32
        module procedure argminmax_date_i64
        module procedure argminmax_time_i32
        module procedure argminmax_time_i64
        module procedure argminmax_ts_i32
        module procedure argminmax_ts_i64
        module procedure argminmax_strcol_i32
        module procedure argminmax_strcol_i64
        module procedure argminmax_col_i32
        module procedure argminmax_col_i64
    end interface pf_argminmax
    !
    !> Merges two ALREADY-SORTED arrays into one sorted array, in O(size(a) + size(b)) rather
    !> than the O(n log n) of sorting their concatenation.
    !>
    !> `descending`/`nulls_first` must match the order `a` and `b` are actually in -- they
    !> select the comparison, exactly as in the searches. Both inputs are checked for
    !> sortedness unless `assume_sorted=.true.`.
    !>
    !> **Supply `is_valid_a`/`is_valid_b` whenever either input has nulls.** A sorted array
    !> containing nulls is what `pf_sort(..., is_valid=)` produces, and a merge that is not told
    !> which elements are null compares them as ordinary values and interleaves them into the
    !> middle of the result. The precondition cannot be checked, either: a null's stored value
    !> is indistinguishable from a real one without the mask.
    !>
    !> `merged_valid` reports the result's validity and is ALWAYS allocated when asked for, all
    !> `.true.` when neither input mask was supplied. Ties take from `a` first, so the result
    !> matches `pf_sort` of the concatenation element for element.
    interface pf_merge
        module procedure merge_i32
        module procedure merge_i64
        module procedure merge_f32
        module procedure merge_f64
        module procedure merge_bool
        module procedure merge_chr
        module procedure merge_date
        module procedure merge_time
        module procedure merge_ts
    end interface pf_merge
    !
    ! ---- Key extraction and pf_sort_keys%add (parquet_sorting_keys) ----
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
        !> Extracts a date key into the canonical form the engine takes.
        module subroutine extract_date(values, buf, descending, nulls_first, proc, threads)
        type(parquet_date), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_date
        !> Extracts a time key into the canonical form the engine takes.
        module subroutine extract_time(values, buf, descending, nulls_first, proc, threads)
        type(parquet_time), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_time
        !> Extracts a timestamp key into the canonical form the engine takes.
        module subroutine extract_ts(values, buf, descending, nulls_first, proc, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_ts
        !> Extracts a packed string column key into the canonical form the engine takes.
        module subroutine extract_strcol(values, buf, descending, nulls_first, proc, threads)
        type(parquet_string_column), intent(in) :: values
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_strcol
        !> Extracts a type-erased column key into the canonical form the engine takes.
        module subroutine extract_col(values, buf, descending, nulls_first, proc, threads)
        type(parquet_column), intent(in) :: values
            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.
            logical, intent(in) :: descending !! .true. sorts high to low.
            logical, intent(in) :: nulls_first !! .true. places nulls before values.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        end subroutine extract_col
        !> Appends a 32-bit integer sort key.
        module subroutine add_i32(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        integer(int32), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_i32
        !> Appends a 64-bit integer sort key.
        module subroutine add_i64(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        integer(int64), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_i64
        !> Appends a 32-bit real sort key.
        module subroutine add_f32(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        real(real32), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_f32
        !> Appends a 64-bit real sort key.
        module subroutine add_f64(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        real(real64), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_f64
        !> Appends a logical sort key.
        module subroutine add_bool(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        logical, intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_bool
        !> Appends a string sort key.
        module subroutine add_chr(self, values, descending, nulls_first, is_valid)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        character(len=*), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine add_chr
        !> Appends a date sort key.
        module subroutine add_date(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_date), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_date
        !> Appends a time sort key.
        module subroutine add_time(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_time), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_time
        !> Appends a timestamp sort key.
        module subroutine add_ts(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_timestamp), intent(in) :: values(:)
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_ts
        !> Appends a packed string column sort key.
        module subroutine add_strcol(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_string_column), intent(in) :: values
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_strcol
        !> Appends a type-erased column sort key.
        module subroutine add_col(self, values, descending, nulls_first)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        type(parquet_column), intent(in) :: values
            logical, intent(in), optional :: descending !! .true. sorts this key high to low.
            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.
        end subroutine add_col
        !> Number of keys added so far: one per `%add` call, whatever their types.
        !!
        !! Counts the keys YOU added, which is not always what the engine holds -- one
        !! `parquet_timestamp` key becomes two engine keys internally. This reports 1 for it,
        !! and `group_nkeys` counts in the same units.
        module function keys_count(self) result(n)
            class(pf_sort_keys), intent(in) :: self !! the key list.
            integer :: n                            !! keys added.
        end function keys_count
        !> Drops every key, leaving the object reusable.
        module subroutine keys_clear(self)
            class(pf_sort_keys), intent(inout) :: self !! the key list.
        end subroutine keys_clear
        !> Appends `buf` to `self`, checking every key describes the same number of rows.
        module subroutine keys_append(self, buf, proc)
            class(pf_sort_keys), intent(inout) :: self          !! the key list.
            type(sort_key_buf), allocatable, intent(inout) :: buf(:) !! keys to append; moved from.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
        end subroutine keys_append
        !> Validates a `group_nkeys` request and translates it from CALLER keys to ENGINE keys.
        !!
        !! Always sets `group_ekeys`, so a caller can pass it on unconditionally: with
        !! `group_nkeys` absent it comes back as every engine key, which is what grouping on
        !! the full key list means.
        module subroutine resolve_group_nkeys(keys, group_nkeys, want_offsets, proc, group_ekeys)
            class(pf_sort_keys), intent(in) :: keys       !! the key list.
            integer, intent(in), optional :: group_nkeys  !! caller keys per group; absent = all.
            logical, intent(in) :: want_offsets           !! whether group_offsets was asked for.
            character(len=*), intent(in) :: proc          !! calling procedure, for messages.
            integer, intent(out) :: group_ekeys           !! the engine-key prefix length.
        end subroutine resolve_group_nkeys
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
        !> Runs the engine over `keys` but orders only the first `count` entries -- `perm`
        !! comes back with exactly `count` elements.
        module subroutine drive_engine_partial(keys, nrows, count, proc, perm)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            integer(int64), intent(in) :: count                 !! leading entries to order.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `count` 1-based indices.
        end subroutine drive_engine_partial
        !> The 1-based index a full stable sort would place at rank `nth`, without sorting.
        module subroutine engine_nth_index(keys, nrows, nth, proc, idx)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            integer(int64), intent(in) :: nth                   !! 1-based rank wanted.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), intent(out) :: idx                  !! 1-based row index at that rank.
        end subroutine engine_nth_index
        !> Clamps a requested count to the array size, aborting only on a negative one.
        module subroutine resolve_count(n, nrows, proc, count)
            integer, intent(in) :: n                !! requested count, as the caller gave it.
            integer(int64), intent(in) :: nrows     !! the array size.
            character(len=*), intent(in) :: proc    !! calling procedure, for messages.
            integer(int64), intent(out) :: count    !! min(n, nrows).
        end subroutine resolve_count
        !> Aborts unless `nth` names a rank that exists.
        module subroutine check_rank(nth, nrows, proc)
            integer(int64), intent(in) :: nth      !! 1-based rank wanted.
            integer(int64), intent(in) :: nrows    !! the array size.
            character(len=*), intent(in) :: proc   !! calling procedure, for messages.
        end subroutine check_rank
        !> How many of a key's rows are non-null.
        module subroutine key_valid_count(keys, nrows, n_valid)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only the first is consulted.
            integer(int64), intent(in) :: nrows       !! the array size.
            integer(int64), intent(out) :: n_valid    !! rows that are not null.
        end subroutine key_valid_count
        !> Turns a `rounding=` token into an RND_* mode, aborting on an unrecognized one.
        module subroutine resolve_rounding(rounding, proc, mode)
            character(len=*), intent(in), optional :: rounding !! token; default "nearest".
            character(len=*), intent(in) :: proc               !! calling procedure, for messages.
            integer, intent(out) :: mode                       !! RND_NEAREST / RND_DOWN / RND_UP.
        end subroutine resolve_rounding
        !> The 1-based rank a quantile names within `n_valid` non-null values.
        module subroutine quantile_rank(quantile, n_valid, mode, proc, rank)
            real(real64), intent(in) :: quantile   !! position on a 0-1 scale.
            integer(int64), intent(in) :: n_valid  !! non-null population size.
            integer, intent(in) :: mode            !! RND_* rounding of a fractional position.
            character(len=*), intent(in) :: proc   !! calling procedure, for messages.
            integer(int64), intent(out) :: rank    !! 1-based rank within the non-null values.
        end subroutine quantile_rank
        !> Whether every row is already in order under `keys`, using the same comparator
        !! `drive_engine` sorts with, so the two can never disagree.
        module subroutine engine_is_sorted(keys, nrows, proc, answer)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            logical, intent(out) :: answer                      !! .true. when already in order.
        end subroutine engine_is_sorted
        !> Builds the engine's int8 validity array from a logical mask, leaving `valid`
        !! UNALLOCATED when the mask marks nothing null (the engine's no-nulls fast path).
        module subroutine valid_from_mask(mask, n, proc, valid)
            logical, intent(in) :: mask(:)                          !! .false. marks a null.
            integer(int64), intent(in) :: n                         !! expected length.
            character(len=*), intent(in) :: proc                    !! calling procedure, for messages.
            integer(c_int8_t), allocatable, intent(out) :: valid(:) !! 1 per valid element.
        end subroutine valid_from_mask
        !> Aborts unless `perm` is a true permutation of 1..n. Uses a bit-packed seen-set, so
        !! the scratch is n/8 bytes rather than the 4n a default LOGICAL array would cost.
        !!
        !! The LENGTH check always runs; `scan=.false.` skips only the O(n) range/duplicate
        !! walk. That split is what `assume_valid=` selects: a caller may promise the contents
        !! are a permutation, but a wrong-LENGTH perm would make the gather that follows read
        !! past the end of the array, and no promise can make that defined.
        module subroutine check_permutation(perm, n, proc, scan)
            integer(int64), intent(in) :: perm(:) !! the permutation to validate.
            integer(int64), intent(in) :: n       !! expected length.
            character(len=*), intent(in) :: proc  !! calling procedure, for messages.
            logical, intent(in), optional :: scan !! .false. checks the length only; default .true.
        end subroutine check_permutation
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
        !> Binary-searches `keys`, whose LAST row is the target the caller appended.
        module subroutine engine_search(keys, nrows, n_search, upper, proc, pos)
            type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.
            integer(int64), intent(in) :: nrows               !! rows each key has, target included.
            integer(int64), intent(in) :: n_search            !! rows to search, target excluded.
            logical, intent(in) :: upper                      !! .true. for upper_bound.
            character(len=*), intent(in) :: proc              !! calling procedure, for messages.
            integer(int64), intent(out) :: pos                !! 1-based insertion point.
        end subroutine engine_search
        !> Merges rows 1..`na` of `keys` with the rest, both already in order.
        module subroutine engine_merge(keys, nrows, na, proc, perm)
            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.
            integer(int64), intent(in) :: nrows                 !! rows each key describes.
            integer(int64), intent(in) :: na                    !! rows belonging to the first input.
            character(len=*), intent(in) :: proc                !! calling procedure, for messages.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
        end subroutine engine_merge
        !> Appends `src`'s rows to `dst`'s, key for key -- how a search target joins the array
        !! it is searched for in, and how `pf_merge` concatenates its two inputs. Both must
        !! describe the same number of keys, of the same families.
        module subroutine buf_append(dst, nd, src, ns, proc)
            type(sort_key_buf), allocatable, intent(inout) :: dst(:) !! grown in place.
            integer(int64), intent(in) :: nd                     !! rows currently in `dst`.
            type(sort_key_buf), allocatable, intent(in) :: src(:) !! keys to append.
            integer(int64), intent(in) :: ns                     !! rows in `src`.
            character(len=*), intent(in) :: proc                 !! calling procedure, for messages.
        end subroutine buf_append
        !> How many of a key's rows hold an actual VALUE -- neither null nor NaN, i.e. the
        !! population `pf_minmax` reduces over.
        module subroutine key_value_count(keys, nrows, n_value)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only the first is consulted.
            integer(int64), intent(in) :: nrows       !! the array size.
            integer(int64), intent(out) :: n_value    !! rows that are neither null nor NaN.
        end subroutine key_value_count
        !> Which rows of a key are null, as a plain mask. Every element is `.false.` when the
        !! key has no nulls at all (the module's unallocated-`valid` convention).
        module subroutine key_null_mask(keys, nrows, isnull)
            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only the first is consulted.
            integer(int64), intent(in) :: nrows       !! the array size.
            logical, allocatable, intent(out) :: isnull(:) !! .true. where the row is null.
        end subroutine key_null_mask
        !> Lower-cases a trimmed token, the one case-folding site the module has.
        module subroutine fold_token(text, tok)
            character(len=*), intent(in) :: text              !! the raw token.
            character(len=:), allocatable, intent(out) :: tok !! trimmed and lower-cased.
        end subroutine fold_token
        !> Turns a `method=` token into a RANK_* mode, aborting on an unrecognized one.
        module subroutine resolve_rank_method(method, proc, mode)
            character(len=*), intent(in), optional :: method !! token; default "competition".
            character(len=*), intent(in) :: proc             !! calling procedure, for messages.
            integer, intent(out) :: mode                     !! RANK_COMPETITION/_DENSE/_ORDINAL.
        end subroutine resolve_rank_method
        !> Aborts unless the extracted key is in the order the caller says it is. `what` names
        !! the argument, since `pf_merge` has two arrays to tell apart.
        module subroutine check_sorted_input(keys, nrows, proc, what)
            type(sort_key_buf), intent(in), target :: keys(:) !! the extracted key.
            integer(int64), intent(in) :: nrows               !! its row count.
            character(len=*), intent(in) :: proc              !! calling procedure, for messages.
            character(len=*), intent(in) :: what              !! the argument's name.
        end subroutine check_sorted_input
        !> Narrows one int64 answer to int32, aborting rather than truncating. `noun` names
        !! what the number is, so the message says which argument to widen.
        module subroutine narrow_i64(value, proc, noun, dst)
            integer(int64), intent(in) :: value  !! the answer.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            character(len=*), intent(in) :: noun !! what the number is, for the message.
            integer(int32), intent(out) :: dst   !! the narrowed copy.
        end subroutine narrow_i64
        !> The array counterpart of `narrow_i64`.
        module subroutine narrow_i64_array(src, proc, noun, dst)
            integer(int64), intent(in) :: src(:) !! the answers.
            character(len=*), intent(in) :: proc !! calling procedure, for messages.
            character(len=*), intent(in) :: noun !! what the numbers are, for the message.
            integer(int32), allocatable, intent(out) :: dst(:) !! the narrowed copy.
        end subroutine narrow_i64_array
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
    ! ---- pf_argsort and pf_sort (parquet_sorting_argsort) ----
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
        !> pf_argsort over a date array, returning an int32 permutation.
        module subroutine argsort_date_i32(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_date_i32
        !> pf_argsort over a date array, returning an int64 permutation.
        module subroutine argsort_date_i64(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_date_i64
        !> pf_argsort over a time array, returning an int32 permutation.
        module subroutine argsort_time_i32(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_time_i32
        !> pf_argsort over a time array, returning an int64 permutation.
        module subroutine argsort_time_i64(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_time_i64
        !> pf_argsort over a timestamp array, returning an int32 permutation.
        module subroutine argsort_ts_i32(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_ts_i32
        !> pf_argsort over a timestamp array, returning an int64 permutation.
        module subroutine argsort_ts_i64(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_ts_i64
        !> pf_argsort over a packed string column array, returning an int32 permutation.
        module subroutine argsort_strcol_i32(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_string_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_strcol_i32
        !> pf_argsort over a packed string column array, returning an int64 permutation.
        module subroutine argsort_strcol_i64(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_string_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_strcol_i64
        !> pf_argsort over a type-erased column array, returning an int32 permutation.
        module subroutine argsort_col_i32(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_col_i32
        !> pf_argsort over a type-erased column array, returning an int64 permutation.
        module subroutine argsort_col_i64(values, perm, descending, nulls_first, &
                threads, group_offsets)
        type(parquet_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
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
        end subroutine argsort_col_i64
        !> pf_argsort over a multi-key `pf_sort_keys`, returning an int32 permutation.
        module subroutine argsort_keys_i32(keys, perm, threads, group_offsets, group_nkeys)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            integer(int32), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
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
            !> how many LEADING keys have to be equal for two rows to share a group. ABSENT
            !! means all of them. Counts the keys YOU added, one per `%add` call, which is not
            !! always the engine's own count -- one `parquet_timestamp` key becomes two engine
            !! keys internally, and this argument never exposes that.
            !!
            !! **It does not change the sort.** Every key still orders the rows; only the
            !! equality test that closes a group is narrowed. That is what gives "group by
            !! field, ordered by magnitude within each group": sort by both, group on the first.
            !!
            !! Must be between 1 and the number of keys, and requires `group_offsets` -- on its
            !! own it would change nothing, so passing it alone is an error rather than a no-op.
            !! A single default-kind `integer` with no int64 form: a key count cannot approach
            !! `huge(int32)`.
            integer, intent(in), optional :: group_nkeys
        end subroutine argsort_keys_i32
        !> pf_argsort over a multi-key `pf_sort_keys`, returning an int64 permutation.
        module subroutine argsort_keys_i64(keys, perm, threads, group_offsets, group_nkeys)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.
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
            !> how many LEADING keys have to be equal for two rows to share a group. ABSENT
            !! means all of them. Counts the keys YOU added, one per `%add` call, which is not
            !! always the engine's own count -- one `parquet_timestamp` key becomes two engine
            !! keys internally, and this argument never exposes that.
            !!
            !! **It does not change the sort.** Every key still orders the rows; only the
            !! equality test that closes a group is narrowed. That is what gives "group by
            !! field, ordered by magnitude within each group": sort by both, group on the first.
            !!
            !! Must be between 1 and the number of keys, and requires `group_offsets` -- on its
            !! own it would change nothing, so passing it alone is an error rather than a no-op.
            !! A single default-kind `integer` with no int64 form: a key count cannot approach
            !! `huge(int32)`.
            integer, intent(in), optional :: group_nkeys
        end subroutine argsort_keys_i64
        !> pf_sort over a 32-bit integer array: an independent sorted copy.
        module subroutine sort_i32(values, sorted, descending, nulls_first, is_valid, sorted_valid, threads)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.
            !! when `is_valid` was absent, since the caller asked a direct question.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_i32
        !> pf_sort over a 64-bit integer array: an independent sorted copy.
        module subroutine sort_i64(values, sorted, descending, nulls_first, is_valid, sorted_valid, threads)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.
            !! when `is_valid` was absent, since the caller asked a direct question.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_i64
        !> pf_sort over a 32-bit real array: an independent sorted copy.
        module subroutine sort_f32(values, sorted, descending, nulls_first, is_valid, sorted_valid, threads)
        real(real32), intent(in) :: values(:)
            real(real32), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.
            !! when `is_valid` was absent, since the caller asked a direct question.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_f32
        !> pf_sort over a 64-bit real array: an independent sorted copy.
        module subroutine sort_f64(values, sorted, descending, nulls_first, is_valid, sorted_valid, threads)
        real(real64), intent(in) :: values(:)
            real(real64), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.
            !! when `is_valid` was absent, since the caller asked a direct question.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_f64
        !> pf_sort over a logical array: an independent sorted copy.
        module subroutine sort_bool(values, sorted, descending, nulls_first, is_valid, sorted_valid, threads)
        logical, intent(in) :: values(:)
            logical, allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.
            !! when `is_valid` was absent, since the caller asked a direct question.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_bool
        !> pf_sort over a string array: an independent sorted copy.
        module subroutine sort_chr(values, sorted, descending, nulls_first, is_valid, sorted_valid, threads)
        character(len=*), intent(in) :: values(:)
            character(len=len(values)), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.
            !! when `is_valid` was absent, since the caller asked a direct question.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_chr
        !> pf_sort over a date array: an independent sorted copy.
        module subroutine sort_date(values, sorted, descending, nulls_first, threads)
        type(parquet_date), intent(in) :: values(:)
            type(parquet_date), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_date
        !> pf_sort over a time array: an independent sorted copy.
        module subroutine sort_time(values, sorted, descending, nulls_first, threads)
        type(parquet_time), intent(in) :: values(:)
            type(parquet_time), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_time
        !> pf_sort over a timestamp array: an independent sorted copy.
        module subroutine sort_ts(values, sorted, descending, nulls_first, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            type(parquet_timestamp), allocatable, intent(out) :: sorted(:) !! the sorted copy.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine sort_ts
    end interface
    !
    ! ---- pf_partial_sort and pf_partial_argsort (parquet_sorting_select) ----
    interface
        !> pf_partial_argsort over a 32-bit integer array, returning an int32 permutation.
        module subroutine partial_argsort_i32_i32(values, perm, n, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_i32_i32
        !> pf_partial_argsort over a 32-bit integer array, returning an int64 permutation.
        module subroutine partial_argsort_i32_i64(values, perm, n, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_i32_i64
        !> pf_partial_argsort over a 64-bit integer array, returning an int32 permutation.
        module subroutine partial_argsort_i64_i32(values, perm, n, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_i64_i32
        !> pf_partial_argsort over a 64-bit integer array, returning an int64 permutation.
        module subroutine partial_argsort_i64_i64(values, perm, n, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_i64_i64
        !> pf_partial_argsort over a 32-bit real array, returning an int32 permutation.
        module subroutine partial_argsort_f32_i32(values, perm, n, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_f32_i32
        !> pf_partial_argsort over a 32-bit real array, returning an int64 permutation.
        module subroutine partial_argsort_f32_i64(values, perm, n, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_f32_i64
        !> pf_partial_argsort over a 64-bit real array, returning an int32 permutation.
        module subroutine partial_argsort_f64_i32(values, perm, n, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_f64_i32
        !> pf_partial_argsort over a 64-bit real array, returning an int64 permutation.
        module subroutine partial_argsort_f64_i64(values, perm, n, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_f64_i64
        !> pf_partial_argsort over a logical array, returning an int32 permutation.
        module subroutine partial_argsort_bool_i32(values, perm, n, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_bool_i32
        !> pf_partial_argsort over a logical array, returning an int64 permutation.
        module subroutine partial_argsort_bool_i64(values, perm, n, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_bool_i64
        !> pf_partial_argsort over a string array, returning an int32 permutation.
        module subroutine partial_argsort_chr_i32(values, perm, n, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_chr_i32
        !> pf_partial_argsort over a string array, returning an int64 permutation.
        module subroutine partial_argsort_chr_i64(values, perm, n, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine partial_argsort_chr_i64
        !> pf_partial_argsort over a date array, returning an int32 permutation.
        module subroutine partial_argsort_date_i32(values, perm, n, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_date_i32
        !> pf_partial_argsort over a date array, returning an int64 permutation.
        module subroutine partial_argsort_date_i64(values, perm, n, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_date_i64
        !> pf_partial_argsort over a time array, returning an int32 permutation.
        module subroutine partial_argsort_time_i32(values, perm, n, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_time_i32
        !> pf_partial_argsort over a time array, returning an int64 permutation.
        module subroutine partial_argsort_time_i64(values, perm, n, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_time_i64
        !> pf_partial_argsort over a timestamp array, returning an int32 permutation.
        module subroutine partial_argsort_ts_i32(values, perm, n, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_ts_i32
        !> pf_partial_argsort over a timestamp array, returning an int64 permutation.
        module subroutine partial_argsort_ts_i64(values, perm, n, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_ts_i64
        !> pf_partial_argsort over a packed string column array, returning an int32 permutation.
        module subroutine partial_argsort_strcol_i32(values, perm, n, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_strcol_i32
        !> pf_partial_argsort over a packed string column array, returning an int64 permutation.
        module subroutine partial_argsort_strcol_i64(values, perm, n, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_strcol_i64
        !> pf_partial_argsort over a type-erased column array, returning an int32 permutation.
        module subroutine partial_argsort_col_i32(values, perm, n, descending, nulls_first)
        type(parquet_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_col_i32
        !> pf_partial_argsort over a type-erased column array, returning an int64 permutation.
        module subroutine partial_argsort_col_i64(values, perm, n, descending, nulls_first)
        type(parquet_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_argsort_col_i64
        !> pf_partial_argsort over a multi-key `pf_sort_keys`, returning an int32 permutation.
        !!
        !! Each key carries its own `descending`/`nulls_first` from `%add`. No `threads`:
        !! the partial sort is not threaded, as its per-type specifics already reflect.
        module subroutine partial_argsort_keys_i32(keys, perm, n)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            integer(int32), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading rows to order; clamped to the row count.
        end subroutine partial_argsort_keys_i32
        !> pf_partial_argsort over a multi-key `pf_sort_keys`, returning an int64 permutation.
        !!
        !! Each key carries its own `descending`/`nulls_first` from `%add`. No `threads`:
        !! the partial sort is not threaded, as its per-type specifics already reflect.
        module subroutine partial_argsort_keys_i64(keys, perm, n)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            integer(int64), allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.
            integer, intent(in) :: n !! leading rows to order; clamped to the row count.
        end subroutine partial_argsort_keys_i64
        !> pf_partial_sort over a 32-bit integer array: the first `n` in order, as a copy.
        module subroutine partial_sort_i32(values, sorted, n, descending, nulls_first, is_valid, sorted_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order; always allocated when asked for.
        end subroutine partial_sort_i32
        !> pf_partial_sort over a 64-bit integer array: the first `n` in order, as a copy.
        module subroutine partial_sort_i64(values, sorted, n, descending, nulls_first, is_valid, sorted_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order; always allocated when asked for.
        end subroutine partial_sort_i64
        !> pf_partial_sort over a 32-bit real array: the first `n` in order, as a copy.
        module subroutine partial_sort_f32(values, sorted, n, descending, nulls_first, is_valid, sorted_valid)
        real(real32), intent(in) :: values(:)
            real(real32), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order; always allocated when asked for.
        end subroutine partial_sort_f32
        !> pf_partial_sort over a 64-bit real array: the first `n` in order, as a copy.
        module subroutine partial_sort_f64(values, sorted, n, descending, nulls_first, is_valid, sorted_valid)
        real(real64), intent(in) :: values(:)
            real(real64), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order; always allocated when asked for.
        end subroutine partial_sort_f64
        !> pf_partial_sort over a logical array: the first `n` in order, as a copy.
        module subroutine partial_sort_bool(values, sorted, n, descending, nulls_first, is_valid, sorted_valid)
        logical, intent(in) :: values(:)
            logical, allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order; always allocated when asked for.
        end subroutine partial_sort_bool
        !> pf_partial_sort over a string array: the first `n` in order, as a copy.
        module subroutine partial_sort_chr(values, sorted, n, descending, nulls_first, is_valid, sorted_valid)
        character(len=*), intent(in) :: values(:)
            character(len=len(values)), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, allocatable, intent(out), optional :: sorted_valid(:)
            !! validity of `sorted`, in its order; always allocated when asked for.
        end subroutine partial_sort_chr
        !> pf_partial_sort over a date array: the first `n` in order, as a copy.
        module subroutine partial_sort_date(values, sorted, n, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            type(parquet_date), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_sort_date
        !> pf_partial_sort over a time array: the first `n` in order, as a copy.
        module subroutine partial_sort_time(values, sorted, n, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            type(parquet_time), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_sort_time
        !> pf_partial_sort over a timestamp array: the first `n` in order, as a copy.
        module subroutine partial_sort_ts(values, sorted, n, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            type(parquet_timestamp), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.
            integer, intent(in) :: n !! leading elements to order; clamped to the size.
            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.
            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.
        end subroutine partial_sort_ts
        !> pf_nth_element over a 32-bit integer array, with an int32 rank and no index out-argument.
        module subroutine nth_i32_i32(values, nth, p_value, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            integer(int32), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i32_i32
        !> pf_nth_element over a 32-bit integer array, with an int32 rank and an int32 index.
        module subroutine nth_i32_i32_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            integer(int32), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i32_i32_i32
        !> pf_nth_element over a 32-bit integer array, with an int32 rank and an int64 index.
        module subroutine nth_i32_i32_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            integer(int32), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i32_i32_i64
        !> pf_nth_element over a 32-bit integer array, with an int64 rank and no index out-argument.
        module subroutine nth_i32_i64(values, nth, p_value, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            integer(int32), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i32_i64
        !> pf_nth_element over a 32-bit integer array, with an int64 rank and an int32 index.
        module subroutine nth_i32_i64_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            integer(int32), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i32_i64_i32
        !> pf_nth_element over a 32-bit integer array, with an int64 rank and an int64 index.
        module subroutine nth_i32_i64_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            integer(int32), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i32_i64_i64
        !> pf_nth_element over a 64-bit integer array, with an int32 rank and no index out-argument.
        module subroutine nth_i64_i32(values, nth, p_value, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            integer(int64), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i64_i32
        !> pf_nth_element over a 64-bit integer array, with an int32 rank and an int32 index.
        module subroutine nth_i64_i32_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            integer(int64), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i64_i32_i32
        !> pf_nth_element over a 64-bit integer array, with an int32 rank and an int64 index.
        module subroutine nth_i64_i32_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            integer(int64), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i64_i32_i64
        !> pf_nth_element over a 64-bit integer array, with an int64 rank and no index out-argument.
        module subroutine nth_i64_i64(values, nth, p_value, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            integer(int64), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i64_i64
        !> pf_nth_element over a 64-bit integer array, with an int64 rank and an int32 index.
        module subroutine nth_i64_i64_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            integer(int64), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i64_i64_i32
        !> pf_nth_element over a 64-bit integer array, with an int64 rank and an int64 index.
        module subroutine nth_i64_i64_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            integer(int64), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_i64_i64_i64
        !> pf_nth_element over a 32-bit real array, with an int32 rank and no index out-argument.
        module subroutine nth_f32_i32(values, nth, p_value, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            real(real32), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f32_i32
        !> pf_nth_element over a 32-bit real array, with an int32 rank and an int32 index.
        module subroutine nth_f32_i32_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            real(real32), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f32_i32_i32
        !> pf_nth_element over a 32-bit real array, with an int32 rank and an int64 index.
        module subroutine nth_f32_i32_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            real(real32), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f32_i32_i64
        !> pf_nth_element over a 32-bit real array, with an int64 rank and no index out-argument.
        module subroutine nth_f32_i64(values, nth, p_value, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            real(real32), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f32_i64
        !> pf_nth_element over a 32-bit real array, with an int64 rank and an int32 index.
        module subroutine nth_f32_i64_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            real(real32), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f32_i64_i32
        !> pf_nth_element over a 32-bit real array, with an int64 rank and an int64 index.
        module subroutine nth_f32_i64_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            real(real32), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f32_i64_i64
        !> pf_nth_element over a 64-bit real array, with an int32 rank and no index out-argument.
        module subroutine nth_f64_i32(values, nth, p_value, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            real(real64), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f64_i32
        !> pf_nth_element over a 64-bit real array, with an int32 rank and an int32 index.
        module subroutine nth_f64_i32_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            real(real64), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f64_i32_i32
        !> pf_nth_element over a 64-bit real array, with an int32 rank and an int64 index.
        module subroutine nth_f64_i32_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            real(real64), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f64_i32_i64
        !> pf_nth_element over a 64-bit real array, with an int64 rank and no index out-argument.
        module subroutine nth_f64_i64(values, nth, p_value, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            real(real64), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f64_i64
        !> pf_nth_element over a 64-bit real array, with an int64 rank and an int32 index.
        module subroutine nth_f64_i64_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            real(real64), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f64_i64_i32
        !> pf_nth_element over a 64-bit real array, with an int64 rank and an int64 index.
        module subroutine nth_f64_i64_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            real(real64), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_f64_i64_i64
        !> pf_nth_element over a logical array, with an int32 rank and no index out-argument.
        module subroutine nth_bool_i32(values, nth, p_value, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            logical, intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_bool_i32
        !> pf_nth_element over a logical array, with an int32 rank and an int32 index.
        module subroutine nth_bool_i32_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            logical, intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_bool_i32_i32
        !> pf_nth_element over a logical array, with an int32 rank and an int64 index.
        module subroutine nth_bool_i32_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            logical, intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_bool_i32_i64
        !> pf_nth_element over a logical array, with an int64 rank and no index out-argument.
        module subroutine nth_bool_i64(values, nth, p_value, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            logical, intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_bool_i64
        !> pf_nth_element over a logical array, with an int64 rank and an int32 index.
        module subroutine nth_bool_i64_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            logical, intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_bool_i64_i32
        !> pf_nth_element over a logical array, with an int64 rank and an int64 index.
        module subroutine nth_bool_i64_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            logical, intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_bool_i64_i64
        !> pf_nth_element over a string array, with an int32 rank and no index out-argument.
        module subroutine nth_chr_i32(values, nth, p_value, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_chr_i32
        !> pf_nth_element over a string array, with an int32 rank and an int32 index.
        module subroutine nth_chr_i32_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_chr_i32_i32
        !> pf_nth_element over a string array, with an int32 rank and an int64 index.
        module subroutine nth_chr_i32_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_chr_i32_i64
        !> pf_nth_element over a string array, with an int64 rank and no index out-argument.
        module subroutine nth_chr_i64(values, nth, p_value, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_chr_i64
        !> pf_nth_element over a string array, with an int64 rank and an int32 index.
        module subroutine nth_chr_i64_i32(values, nth, p_value, index, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_chr_i64_i32
        !> pf_nth_element over a string array, with an int64 rank and an int64 index.
        module subroutine nth_chr_i64_i64(values, nth, p_value, index, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine nth_chr_i64_i64
        !> pf_nth_element over a date array, with an int32 rank and no index out-argument.
        module subroutine nth_date_i32(values, nth, p_value, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_date), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_date_i32
        !> pf_nth_element over a date array, with an int32 rank and an int32 index.
        module subroutine nth_date_i32_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_date), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_date_i32_i32
        !> pf_nth_element over a date array, with an int32 rank and an int64 index.
        module subroutine nth_date_i32_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_date), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_date_i32_i64
        !> pf_nth_element over a date array, with an int64 rank and no index out-argument.
        module subroutine nth_date_i64(values, nth, p_value, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_date), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_date_i64
        !> pf_nth_element over a date array, with an int64 rank and an int32 index.
        module subroutine nth_date_i64_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_date), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_date_i64_i32
        !> pf_nth_element over a date array, with an int64 rank and an int64 index.
        module subroutine nth_date_i64_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_date), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_date_i64_i64
        !> pf_nth_element over a time array, with an int32 rank and no index out-argument.
        module subroutine nth_time_i32(values, nth, p_value, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_time), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_time_i32
        !> pf_nth_element over a time array, with an int32 rank and an int32 index.
        module subroutine nth_time_i32_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_time), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_time_i32_i32
        !> pf_nth_element over a time array, with an int32 rank and an int64 index.
        module subroutine nth_time_i32_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_time), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_time_i32_i64
        !> pf_nth_element over a time array, with an int64 rank and no index out-argument.
        module subroutine nth_time_i64(values, nth, p_value, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_time), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_time_i64
        !> pf_nth_element over a time array, with an int64 rank and an int32 index.
        module subroutine nth_time_i64_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_time), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_time_i64_i32
        !> pf_nth_element over a time array, with an int64 rank and an int64 index.
        module subroutine nth_time_i64_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_time), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_time_i64_i64
        !> pf_nth_element over a timestamp array, with an int32 rank and no index out-argument.
        module subroutine nth_ts_i32(values, nth, p_value, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_ts_i32
        !> pf_nth_element over a timestamp array, with an int32 rank and an int32 index.
        module subroutine nth_ts_i32_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_ts_i32_i32
        !> pf_nth_element over a timestamp array, with an int32 rank and an int64 index.
        module subroutine nth_ts_i32_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_ts_i32_i64
        !> pf_nth_element over a timestamp array, with an int64 rank and no index out-argument.
        module subroutine nth_ts_i64(values, nth, p_value, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_ts_i64
        !> pf_nth_element over a timestamp array, with an int64 rank and an int32 index.
        module subroutine nth_ts_i64_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_ts_i64_i32
        !> pf_nth_element over a timestamp array, with an int64 rank and an int64 index.
        module subroutine nth_ts_i64_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_ts_i64_i64
        !> pf_nth_element over a packed string column array, with an int32 rank and no index out-argument.
        module subroutine nth_strcol_i32(values, nth, p_value, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_strcol_i32
        !> pf_nth_element over a packed string column array, with an int32 rank and an int32 index.
        module subroutine nth_strcol_i32_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_strcol_i32_i32
        !> pf_nth_element over a packed string column array, with an int32 rank and an int64 index.
        module subroutine nth_strcol_i32_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int32), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_strcol_i32_i64
        !> pf_nth_element over a packed string column array, with an int64 rank and no index out-argument.
        module subroutine nth_strcol_i64(values, nth, p_value, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_strcol_i64
        !> pf_nth_element over a packed string column array, with an int64 rank and an int32 index.
        module subroutine nth_strcol_i64_i32(values, nth, p_value, index, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_strcol_i64_i32
        !> pf_nth_element over a packed string column array, with an int64 rank and an int64 index.
        module subroutine nth_strcol_i64_i64(values, nth, p_value, index, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            integer(int64), intent(in) :: nth !! 1-based rank wanted.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            logical, intent(in), optional :: descending !! .true. ranks high to low.
            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.
        end subroutine nth_strcol_i64_i64
        !> pf_nth_quantile over a 32-bit integer array, with no index out-argument.
        module subroutine quantile_i32(values, quantile, p_value, rounding, is_valid, n_null)
        integer(int32), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            integer(int32), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_i32
        !> pf_nth_quantile over a 32-bit integer array, with an int32 index.
        module subroutine quantile_i32_i32(values, quantile, p_value, index, rounding, is_valid, n_null)
        integer(int32), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            integer(int32), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_i32_i32
        !> pf_nth_quantile over a 32-bit integer array, with an int64 index.
        module subroutine quantile_i32_i64(values, quantile, p_value, index, rounding, is_valid, n_null)
        integer(int32), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            integer(int32), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_i32_i64
        !> pf_nth_quantile over a 64-bit integer array, with no index out-argument.
        module subroutine quantile_i64(values, quantile, p_value, rounding, is_valid, n_null)
        integer(int64), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            integer(int64), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_i64
        !> pf_nth_quantile over a 64-bit integer array, with an int32 index.
        module subroutine quantile_i64_i32(values, quantile, p_value, index, rounding, is_valid, n_null)
        integer(int64), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            integer(int64), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_i64_i32
        !> pf_nth_quantile over a 64-bit integer array, with an int64 index.
        module subroutine quantile_i64_i64(values, quantile, p_value, index, rounding, is_valid, n_null)
        integer(int64), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            integer(int64), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_i64_i64
        !> pf_nth_quantile over a 32-bit real array, with no index out-argument.
        module subroutine quantile_f32(values, quantile, p_value, rounding, is_valid, n_null)
        real(real32), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            real(real32), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_f32
        !> pf_nth_quantile over a 32-bit real array, with an int32 index.
        module subroutine quantile_f32_i32(values, quantile, p_value, index, rounding, is_valid, n_null)
        real(real32), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            real(real32), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_f32_i32
        !> pf_nth_quantile over a 32-bit real array, with an int64 index.
        module subroutine quantile_f32_i64(values, quantile, p_value, index, rounding, is_valid, n_null)
        real(real32), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            real(real32), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_f32_i64
        !> pf_nth_quantile over a 64-bit real array, with no index out-argument.
        module subroutine quantile_f64(values, quantile, p_value, rounding, is_valid, n_null)
        real(real64), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            real(real64), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_f64
        !> pf_nth_quantile over a 64-bit real array, with an int32 index.
        module subroutine quantile_f64_i32(values, quantile, p_value, index, rounding, is_valid, n_null)
        real(real64), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            real(real64), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_f64_i32
        !> pf_nth_quantile over a 64-bit real array, with an int64 index.
        module subroutine quantile_f64_i64(values, quantile, p_value, index, rounding, is_valid, n_null)
        real(real64), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            real(real64), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_f64_i64
        !> pf_nth_quantile over a logical array, with no index out-argument.
        module subroutine quantile_bool(values, quantile, p_value, rounding, is_valid, n_null)
        logical, intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            logical, intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_bool
        !> pf_nth_quantile over a logical array, with an int32 index.
        module subroutine quantile_bool_i32(values, quantile, p_value, index, rounding, is_valid, n_null)
        logical, intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            logical, intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_bool_i32
        !> pf_nth_quantile over a logical array, with an int64 index.
        module subroutine quantile_bool_i64(values, quantile, p_value, index, rounding, is_valid, n_null)
        logical, intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            logical, intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_bool_i64
        !> pf_nth_quantile over a string array, with no index out-argument.
        module subroutine quantile_chr(values, quantile, p_value, rounding, is_valid, n_null)
        character(len=*), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_chr
        !> pf_nth_quantile over a string array, with an int32 index.
        module subroutine quantile_chr_i32(values, quantile, p_value, index, rounding, is_valid, n_null)
        character(len=*), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_chr_i32
        !> pf_nth_quantile over a string array, with an int64 index.
        module subroutine quantile_chr_i64(values, quantile, p_value, index, rounding, is_valid, n_null)
        character(len=*), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_chr_i64
        !> pf_nth_quantile over a date array, with no index out-argument.
        module subroutine quantile_date(values, quantile, p_value, rounding, n_null)
        type(parquet_date), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_date), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_date
        !> pf_nth_quantile over a date array, with an int32 index.
        module subroutine quantile_date_i32(values, quantile, p_value, index, rounding, n_null)
        type(parquet_date), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_date), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_date_i32
        !> pf_nth_quantile over a date array, with an int64 index.
        module subroutine quantile_date_i64(values, quantile, p_value, index, rounding, n_null)
        type(parquet_date), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_date), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_date_i64
        !> pf_nth_quantile over a time array, with no index out-argument.
        module subroutine quantile_time(values, quantile, p_value, rounding, n_null)
        type(parquet_time), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_time), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_time
        !> pf_nth_quantile over a time array, with an int32 index.
        module subroutine quantile_time_i32(values, quantile, p_value, index, rounding, n_null)
        type(parquet_time), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_time), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_time_i32
        !> pf_nth_quantile over a time array, with an int64 index.
        module subroutine quantile_time_i64(values, quantile, p_value, index, rounding, n_null)
        type(parquet_time), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_time), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_time_i64
        !> pf_nth_quantile over a timestamp array, with no index out-argument.
        module subroutine quantile_ts(values, quantile, p_value, rounding, n_null)
        type(parquet_timestamp), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_ts
        !> pf_nth_quantile over a timestamp array, with an int32 index.
        module subroutine quantile_ts_i32(values, quantile, p_value, index, rounding, n_null)
        type(parquet_timestamp), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_ts_i32
        !> pf_nth_quantile over a timestamp array, with an int64 index.
        module subroutine quantile_ts_i64(values, quantile, p_value, index, rounding, n_null)
        type(parquet_timestamp), intent(in) :: values(:)
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            type(parquet_timestamp), intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_ts_i64
        !> pf_nth_quantile over a packed string column array, with no index out-argument.
        module subroutine quantile_strcol(values, quantile, p_value, rounding, n_null)
        type(parquet_string_column), intent(in) :: values
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_strcol
        !> pf_nth_quantile over a packed string column array, with an int32 index.
        module subroutine quantile_strcol_i32(values, quantile, p_value, index, rounding, n_null)
        type(parquet_string_column), intent(in) :: values
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
            integer(int32), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_strcol_i32
        !> pf_nth_quantile over a packed string column array, with an int64 index.
        module subroutine quantile_strcol_i64(values, quantile, p_value, index, rounding, n_null)
        type(parquet_string_column), intent(in) :: values
            real(real64), intent(in) :: quantile !! position on a 0-1 scale.
            character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
            integer(int64), intent(out) :: index !! which element of `values` that was.
            character(len=*), intent(in), optional :: rounding !! "nearest"/"down"/"up".
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            !! LAST rather than next to `index`, because both are optional
            !! int64 out-arguments, so with `n_null` at position 4 a positional call could
            !! not be told apart from the `index` form. Nothing else here is a character,
            !! so `rounding` at position 4 disambiguates them.
        end subroutine quantile_strcol_i64
    end interface
    !
    ! ---- pf_permute and pf_is_sorted (parquet_sorting_permute) ----
    interface
        !> pf_permute over a 32-bit integer array, with an int32 permutation.
        module subroutine permute_i32_i32(values, perm, assume_valid)
        integer(int32), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_i32_i32
        !> pf_permute over a 32-bit integer array, with an int64 permutation.
        module subroutine permute_i32_i64(values, perm, assume_valid)
        integer(int32), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_i32_i64
        !> pf_permute over a 64-bit integer array, with an int32 permutation.
        module subroutine permute_i64_i32(values, perm, assume_valid)
        integer(int64), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_i64_i32
        !> pf_permute over a 64-bit integer array, with an int64 permutation.
        module subroutine permute_i64_i64(values, perm, assume_valid)
        integer(int64), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_i64_i64
        !> pf_permute over a 32-bit real array, with an int32 permutation.
        module subroutine permute_f32_i32(values, perm, assume_valid)
        real(real32), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_f32_i32
        !> pf_permute over a 32-bit real array, with an int64 permutation.
        module subroutine permute_f32_i64(values, perm, assume_valid)
        real(real32), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_f32_i64
        !> pf_permute over a 64-bit real array, with an int32 permutation.
        module subroutine permute_f64_i32(values, perm, assume_valid)
        real(real64), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_f64_i32
        !> pf_permute over a 64-bit real array, with an int64 permutation.
        module subroutine permute_f64_i64(values, perm, assume_valid)
        real(real64), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_f64_i64
        !> pf_permute over a logical array, with an int32 permutation.
        module subroutine permute_bool_i32(values, perm, assume_valid)
        logical, intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_bool_i32
        !> pf_permute over a logical array, with an int64 permutation.
        module subroutine permute_bool_i64(values, perm, assume_valid)
        logical, intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_bool_i64
        !> pf_permute over a string array, with an int32 permutation.
        module subroutine permute_chr_i32(values, perm, assume_valid)
        character(len=*), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_chr_i32
        !> pf_permute over a string array, with an int64 permutation.
        module subroutine permute_chr_i64(values, perm, assume_valid)
        character(len=*), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_chr_i64
        !> pf_permute over a date array, with an int32 permutation.
        module subroutine permute_date_i32(values, perm, assume_valid)
        type(parquet_date), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_date_i32
        !> pf_permute over a date array, with an int64 permutation.
        module subroutine permute_date_i64(values, perm, assume_valid)
        type(parquet_date), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_date_i64
        !> pf_permute over a time array, with an int32 permutation.
        module subroutine permute_time_i32(values, perm, assume_valid)
        type(parquet_time), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_time_i32
        !> pf_permute over a time array, with an int64 permutation.
        module subroutine permute_time_i64(values, perm, assume_valid)
        type(parquet_time), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_time_i64
        !> pf_permute over a timestamp array, with an int32 permutation.
        module subroutine permute_ts_i32(values, perm, assume_valid)
        type(parquet_timestamp), intent(inout) :: values(:)
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_ts_i32
        !> pf_permute over a timestamp array, with an int64 permutation.
        module subroutine permute_ts_i64(values, perm, assume_valid)
        type(parquet_timestamp), intent(inout) :: values(:)
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_ts_i64
        !> pf_permute over a packed string column array, with an int32 permutation.
        module subroutine permute_strcol_i32(values, perm, assume_valid)
        type(parquet_string_column), intent(inout) :: values
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_strcol_i32
        !> pf_permute over a packed string column array, with an int64 permutation.
        module subroutine permute_strcol_i64(values, perm, assume_valid)
        type(parquet_string_column), intent(inout) :: values
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_strcol_i64
        !> pf_permute over a type-erased column array, with an int32 permutation.
        module subroutine permute_col_i32(values, perm, assume_valid)
        type(parquet_column), intent(inout) :: values
            integer(int32), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_col_i32
        !> pf_permute over a type-erased column array, with an int64 permutation.
        module subroutine permute_col_i64(values, perm, assume_valid)
        type(parquet_column), intent(inout) :: values
            integer(int64), intent(in) :: perm(:) !! 1-based permutation; not modified.
            logical, intent(in), optional :: assume_valid
            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.
            !! Its LENGTH is checked either way. A false promise silently duplicates and
            !! drops elements.
        end subroutine permute_col_i64
        !> pf_is_sorted over a 32-bit integer array.
        module subroutine is_sorted_i32(values, answer, descending, nulls_first, is_valid)
        integer(int32), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_i32
        !> pf_is_sorted over a 64-bit integer array.
        module subroutine is_sorted_i64(values, answer, descending, nulls_first, is_valid)
        integer(int64), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_i64
        !> pf_is_sorted over a 32-bit real array.
        module subroutine is_sorted_f32(values, answer, descending, nulls_first, is_valid)
        real(real32), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_f32
        !> pf_is_sorted over a 64-bit real array.
        module subroutine is_sorted_f64(values, answer, descending, nulls_first, is_valid)
        real(real64), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_f64
        !> pf_is_sorted over a logical array.
        module subroutine is_sorted_bool(values, answer, descending, nulls_first, is_valid)
        logical, intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_bool
        !> pf_is_sorted over a string array.
        module subroutine is_sorted_chr(values, answer, descending, nulls_first, is_valid)
        character(len=*), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine is_sorted_chr
        !> pf_is_sorted over a date array.
        module subroutine is_sorted_date(values, answer, descending, nulls_first)
        type(parquet_date), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_date
        !> pf_is_sorted over a time array.
        module subroutine is_sorted_time(values, answer, descending, nulls_first)
        type(parquet_time), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_time
        !> pf_is_sorted over a timestamp array.
        module subroutine is_sorted_ts(values, answer, descending, nulls_first)
        type(parquet_timestamp), intent(in) :: values(:)
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_ts
        !> pf_is_sorted over a packed string column array.
        module subroutine is_sorted_strcol(values, answer, descending, nulls_first)
        type(parquet_string_column), intent(in) :: values
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_strcol
        !> pf_is_sorted over a type-erased column array.
        module subroutine is_sorted_col(values, answer, descending, nulls_first)
        type(parquet_column), intent(in) :: values
            logical, intent(out) :: answer !! .true. when already in the stated order.
            logical, intent(in), optional :: descending !! .true. tests high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.
        end subroutine is_sorted_col
        !> pf_is_sorted over a multi-key `pf_sort_keys`.
        !!
        !! Takes no `descending`/`nulls_first`/`is_valid`: each key carries its own, given to
        !! `%add` when it was appended. No `threads` either -- this is an O(n) scan with an
        !! early exit, which threading would cost more than it saves.
        module subroutine is_sorted_keys(keys, answer)
            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.
            logical, intent(out) :: answer !! .true. when already in the stated order.
        end subroutine is_sorted_keys
    end interface
    !
    ! ---- Searching a sorted array (parquet_sorting_search) ----
    interface
        !> pf_lower_bound over a sorted 32-bit integer array, with int32 result(s).
        module subroutine lower_bound_i32_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_i32_i32
        !> pf_lower_bound over a sorted 32-bit integer array, with int64 result(s).
        module subroutine lower_bound_i32_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_i32_i64
        !> pf_lower_bound over a sorted 64-bit integer array, with int32 result(s).
        module subroutine lower_bound_i64_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_i64_i32
        !> pf_lower_bound over a sorted 64-bit integer array, with int64 result(s).
        module subroutine lower_bound_i64_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_i64_i64
        !> pf_lower_bound over a sorted 32-bit real array, with int32 result(s).
        module subroutine lower_bound_f32_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_f32_i32
        !> pf_lower_bound over a sorted 32-bit real array, with int64 result(s).
        module subroutine lower_bound_f32_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_f32_i64
        !> pf_lower_bound over a sorted 64-bit real array, with int32 result(s).
        module subroutine lower_bound_f64_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_f64_i32
        !> pf_lower_bound over a sorted 64-bit real array, with int64 result(s).
        module subroutine lower_bound_f64_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_f64_i64
        !> pf_lower_bound over a sorted logical array, with int32 result(s).
        module subroutine lower_bound_bool_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_bool_i32
        !> pf_lower_bound over a sorted logical array, with int64 result(s).
        module subroutine lower_bound_bool_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_bool_i64
        !> pf_lower_bound over a sorted string array, with int32 result(s).
        module subroutine lower_bound_chr_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_chr_i32
        !> pf_lower_bound over a sorted string array, with int64 result(s).
        module subroutine lower_bound_chr_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_chr_i64
        !> pf_lower_bound over a sorted date array, with int32 result(s).
        module subroutine lower_bound_date_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_date_i32
        !> pf_lower_bound over a sorted date array, with int64 result(s).
        module subroutine lower_bound_date_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_date_i64
        !> pf_lower_bound over a sorted time array, with int32 result(s).
        module subroutine lower_bound_time_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_time_i32
        !> pf_lower_bound over a sorted time array, with int64 result(s).
        module subroutine lower_bound_time_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_time_i64
        !> pf_lower_bound over a sorted timestamp array, with int32 result(s).
        module subroutine lower_bound_ts_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_ts_i32
        !> pf_lower_bound over a sorted timestamp array, with int64 result(s).
        module subroutine lower_bound_ts_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_ts_i64
        !> pf_lower_bound over a sorted packed string column array, with int32 result(s).
        module subroutine lower_bound_strcol_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_strcol_i32
        !> pf_lower_bound over a sorted packed string column array, with int64 result(s).
        module subroutine lower_bound_strcol_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine lower_bound_strcol_i64
        !> pf_upper_bound over a sorted 32-bit integer array, with int32 result(s).
        module subroutine upper_bound_i32_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_i32_i32
        !> pf_upper_bound over a sorted 32-bit integer array, with int64 result(s).
        module subroutine upper_bound_i32_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_i32_i64
        !> pf_upper_bound over a sorted 64-bit integer array, with int32 result(s).
        module subroutine upper_bound_i64_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_i64_i32
        !> pf_upper_bound over a sorted 64-bit integer array, with int64 result(s).
        module subroutine upper_bound_i64_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_i64_i64
        !> pf_upper_bound over a sorted 32-bit real array, with int32 result(s).
        module subroutine upper_bound_f32_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_f32_i32
        !> pf_upper_bound over a sorted 32-bit real array, with int64 result(s).
        module subroutine upper_bound_f32_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_f32_i64
        !> pf_upper_bound over a sorted 64-bit real array, with int32 result(s).
        module subroutine upper_bound_f64_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_f64_i32
        !> pf_upper_bound over a sorted 64-bit real array, with int64 result(s).
        module subroutine upper_bound_f64_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_f64_i64
        !> pf_upper_bound over a sorted logical array, with int32 result(s).
        module subroutine upper_bound_bool_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_bool_i32
        !> pf_upper_bound over a sorted logical array, with int64 result(s).
        module subroutine upper_bound_bool_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_bool_i64
        !> pf_upper_bound over a sorted string array, with int32 result(s).
        module subroutine upper_bound_chr_i32(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_chr_i32
        !> pf_upper_bound over a sorted string array, with int64 result(s).
        module subroutine upper_bound_chr_i64(values, target, pos, descending, nulls_first, is_valid, assume_sorted)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_chr_i64
        !> pf_upper_bound over a sorted date array, with int32 result(s).
        module subroutine upper_bound_date_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_date_i32
        !> pf_upper_bound over a sorted date array, with int64 result(s).
        module subroutine upper_bound_date_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_date_i64
        !> pf_upper_bound over a sorted time array, with int32 result(s).
        module subroutine upper_bound_time_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_time_i32
        !> pf_upper_bound over a sorted time array, with int64 result(s).
        module subroutine upper_bound_time_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_time_i64
        !> pf_upper_bound over a sorted timestamp array, with int32 result(s).
        module subroutine upper_bound_ts_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_ts_i32
        !> pf_upper_bound over a sorted timestamp array, with int64 result(s).
        module subroutine upper_bound_ts_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_ts_i64
        !> pf_upper_bound over a sorted packed string column array, with int32 result(s).
        module subroutine upper_bound_strcol_i32(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_strcol_i32
        !> pf_upper_bound over a sorted packed string column array, with int64 result(s).
        module subroutine upper_bound_strcol_i64(values, target, pos, descending, nulls_first, assume_sorted)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: pos !! 1-based insertion point.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine upper_bound_strcol_i64
        !> pf_equal_range over a sorted 32-bit integer array, with int32 result(s).
        module subroutine equal_range_i32_i32(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_i32_i32
        !> pf_equal_range over a sorted 32-bit integer array, with int64 result(s).
        module subroutine equal_range_i32_i64(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_i32_i64
        !> pf_equal_range over a sorted 64-bit integer array, with int32 result(s).
        module subroutine equal_range_i64_i32(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_i64_i32
        !> pf_equal_range over a sorted 64-bit integer array, with int64 result(s).
        module subroutine equal_range_i64_i64(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_i64_i64
        !> pf_equal_range over a sorted 32-bit real array, with int32 result(s).
        module subroutine equal_range_f32_i32(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_f32_i32
        !> pf_equal_range over a sorted 32-bit real array, with int64 result(s).
        module subroutine equal_range_f32_i64(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_f32_i64
        !> pf_equal_range over a sorted 64-bit real array, with int32 result(s).
        module subroutine equal_range_f64_i32(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_f64_i32
        !> pf_equal_range over a sorted 64-bit real array, with int64 result(s).
        module subroutine equal_range_f64_i64(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_f64_i64
        !> pf_equal_range over a sorted logical array, with int32 result(s).
        module subroutine equal_range_bool_i32(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_bool_i32
        !> pf_equal_range over a sorted logical array, with int64 result(s).
        module subroutine equal_range_bool_i64(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_bool_i64
        !> pf_equal_range over a sorted string array, with int32 result(s).
        module subroutine equal_range_chr_i32(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_chr_i32
        !> pf_equal_range over a sorted string array, with int64 result(s).
        module subroutine equal_range_chr_i64(values, target, first, last, descending, nulls_first, is_valid, assume_sorted)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_chr_i64
        !> pf_equal_range over a sorted date array, with int32 result(s).
        module subroutine equal_range_date_i32(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_date_i32
        !> pf_equal_range over a sorted date array, with int64 result(s).
        module subroutine equal_range_date_i64(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_date_i64
        !> pf_equal_range over a sorted time array, with int32 result(s).
        module subroutine equal_range_time_i32(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_time_i32
        !> pf_equal_range over a sorted time array, with int64 result(s).
        module subroutine equal_range_time_i64(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_time_i64
        !> pf_equal_range over a sorted timestamp array, with int32 result(s).
        module subroutine equal_range_ts_i32(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_ts_i32
        !> pf_equal_range over a sorted timestamp array, with int64 result(s).
        module subroutine equal_range_ts_i64(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_ts_i64
        !> pf_equal_range over a sorted packed string column array, with int32 result(s).
        module subroutine equal_range_strcol_i32(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int32), intent(out) :: first !! first element equal to `target`.
            integer(int32), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_strcol_i32
        !> pf_equal_range over a sorted packed string column array, with int64 result(s).
        module subroutine equal_range_strcol_i64(values, target, first, last, descending, nulls_first, assume_sorted)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
            integer(int64), intent(out) :: first !! first element equal to `target`.
            integer(int64), intent(out) :: last  !! last one; `first - 1` when absent.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check. Only pass it for an order you have
            !! already established -- searching unsorted input answers with a plausible
            !! index and no symptom at all.
        end subroutine equal_range_strcol_i64
    end interface
    !
    ! ---- Distinct values and ranks (parquet_sorting_unique) ----
    interface
        !> pf_unique_count over a 32-bit integer array, with an int32 count.
        module subroutine unique_count_i32_i32(values, count, is_valid, n_null, threads)
        integer(int32), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_i32_i32
        !> pf_unique_count over a 32-bit integer array, with an int64 count.
        module subroutine unique_count_i32_i64(values, count, is_valid, n_null, threads)
        integer(int32), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_i32_i64
        !> pf_unique_count over a 64-bit integer array, with an int32 count.
        module subroutine unique_count_i64_i32(values, count, is_valid, n_null, threads)
        integer(int64), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_i64_i32
        !> pf_unique_count over a 64-bit integer array, with an int64 count.
        module subroutine unique_count_i64_i64(values, count, is_valid, n_null, threads)
        integer(int64), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_i64_i64
        !> pf_unique_count over a 32-bit real array, with an int32 count.
        module subroutine unique_count_f32_i32(values, count, is_valid, n_null, threads)
        real(real32), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_f32_i32
        !> pf_unique_count over a 32-bit real array, with an int64 count.
        module subroutine unique_count_f32_i64(values, count, is_valid, n_null, threads)
        real(real32), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_f32_i64
        !> pf_unique_count over a 64-bit real array, with an int32 count.
        module subroutine unique_count_f64_i32(values, count, is_valid, n_null, threads)
        real(real64), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_f64_i32
        !> pf_unique_count over a 64-bit real array, with an int64 count.
        module subroutine unique_count_f64_i64(values, count, is_valid, n_null, threads)
        real(real64), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_f64_i64
        !> pf_unique_count over a logical array, with an int32 count.
        module subroutine unique_count_bool_i32(values, count, is_valid, n_null, threads)
        logical, intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_bool_i32
        !> pf_unique_count over a logical array, with an int64 count.
        module subroutine unique_count_bool_i64(values, count, is_valid, n_null, threads)
        logical, intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_bool_i64
        !> pf_unique_count over a string array, with an int32 count.
        module subroutine unique_count_chr_i32(values, count, is_valid, n_null, threads)
        character(len=*), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_chr_i32
        !> pf_unique_count over a string array, with an int64 count.
        module subroutine unique_count_chr_i64(values, count, is_valid, n_null, threads)
        character(len=*), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_chr_i64
        !> pf_unique_count over a date array, with an int32 count.
        module subroutine unique_count_date_i32(values, count, n_null, threads)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_date_i32
        !> pf_unique_count over a date array, with an int64 count.
        module subroutine unique_count_date_i64(values, count, n_null, threads)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_date_i64
        !> pf_unique_count over a time array, with an int32 count.
        module subroutine unique_count_time_i32(values, count, n_null, threads)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_time_i32
        !> pf_unique_count over a time array, with an int64 count.
        module subroutine unique_count_time_i64(values, count, n_null, threads)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_time_i64
        !> pf_unique_count over a timestamp array, with an int32 count.
        module subroutine unique_count_ts_i32(values, count, n_null, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_ts_i32
        !> pf_unique_count over a timestamp array, with an int64 count.
        module subroutine unique_count_ts_i64(values, count, n_null, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_ts_i64
        !> pf_unique_count over a packed string column array, with an int32 count.
        module subroutine unique_count_strcol_i32(values, count, n_null, threads)
        type(parquet_string_column), intent(in) :: values
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_strcol_i32
        !> pf_unique_count over a packed string column array, with an int64 count.
        module subroutine unique_count_strcol_i64(values, count, n_null, threads)
        type(parquet_string_column), intent(in) :: values
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_strcol_i64
        !> pf_unique_count over a type-erased column array, with an int32 count.
        module subroutine unique_count_col_i32(values, count, n_null, threads)
        type(parquet_column), intent(in) :: values
            integer(int32), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_col_i32
        !> pf_unique_count over a type-erased column array, with an int64 count.
        module subroutine unique_count_col_i64(values, count, n_null, threads)
        type(parquet_column), intent(in) :: values
            integer(int64), intent(out) :: count !! how many distinct non-null values.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_count_col_i64
        !> pf_unique over a 32-bit integer array: its distinct non-null values, in order.
        module subroutine unique_i32(values, distinct, descending, is_valid, n_null, threads)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_i32
        !> pf_unique over a 64-bit integer array: its distinct non-null values, in order.
        module subroutine unique_i64(values, distinct, descending, is_valid, n_null, threads)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_i64
        !> pf_unique over a 32-bit real array: its distinct non-null values, in order.
        module subroutine unique_f32(values, distinct, descending, is_valid, n_null, threads)
        real(real32), intent(in) :: values(:)
            real(real32), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_f32
        !> pf_unique over a 64-bit real array: its distinct non-null values, in order.
        module subroutine unique_f64(values, distinct, descending, is_valid, n_null, threads)
        real(real64), intent(in) :: values(:)
            real(real64), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_f64
        !> pf_unique over a logical array: its distinct non-null values, in order.
        module subroutine unique_bool(values, distinct, descending, is_valid, n_null, threads)
        logical, intent(in) :: values(:)
            logical, allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_bool
        !> pf_unique over a string array: its distinct non-null values, in order.
        module subroutine unique_chr(values, distinct, descending, is_valid, n_null, threads)
        character(len=*), intent(in) :: values(:)
            character(len=len(values)), allocatable, intent(out) :: distinct(:) !! the distinct values.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_chr
        !> pf_unique over a date array: its distinct non-null values, in order.
        module subroutine unique_date(values, distinct, descending, n_null, threads)
        type(parquet_date), intent(in) :: values(:)
            type(parquet_date), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_date
        !> pf_unique over a time array: its distinct non-null values, in order.
        module subroutine unique_time(values, distinct, descending, n_null, threads)
        type(parquet_time), intent(in) :: values(:)
            type(parquet_time), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_time
        !> pf_unique over a timestamp array: its distinct non-null values, in order.
        module subroutine unique_ts(values, distinct, descending, n_null, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            type(parquet_timestamp), allocatable, intent(out) :: distinct(:) !! the distinct values, in order.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_ts
        !> pf_unique over a packed string column array: its distinct non-null values, in order.
        module subroutine unique_strcol(values, distinct, descending, n_null, threads)
        type(parquet_string_column), intent(in) :: values
            type(parquet_string_column), intent(out) :: distinct !! the distinct values.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer(int64), intent(out), optional :: n_null !! how many values were null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine unique_strcol
        !> pf_rank over a 32-bit integer array, with int32 ranks.
        module subroutine rank_i32_i32(values, ranks, method, descending, is_valid, threads)
        integer(int32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_i32_i32
        !> pf_rank over a 32-bit integer array, with int64 ranks.
        module subroutine rank_i32_i64(values, ranks, method, descending, is_valid, threads)
        integer(int32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_i32_i64
        !> pf_rank over a 64-bit integer array, with int32 ranks.
        module subroutine rank_i64_i32(values, ranks, method, descending, is_valid, threads)
        integer(int64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_i64_i32
        !> pf_rank over a 64-bit integer array, with int64 ranks.
        module subroutine rank_i64_i64(values, ranks, method, descending, is_valid, threads)
        integer(int64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_i64_i64
        !> pf_rank over a 32-bit real array, with int32 ranks.
        module subroutine rank_f32_i32(values, ranks, method, descending, is_valid, threads)
        real(real32), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_f32_i32
        !> pf_rank over a 32-bit real array, with int64 ranks.
        module subroutine rank_f32_i64(values, ranks, method, descending, is_valid, threads)
        real(real32), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_f32_i64
        !> pf_rank over a 64-bit real array, with int32 ranks.
        module subroutine rank_f64_i32(values, ranks, method, descending, is_valid, threads)
        real(real64), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_f64_i32
        !> pf_rank over a 64-bit real array, with int64 ranks.
        module subroutine rank_f64_i64(values, ranks, method, descending, is_valid, threads)
        real(real64), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_f64_i64
        !> pf_rank over a logical array, with int32 ranks.
        module subroutine rank_bool_i32(values, ranks, method, descending, is_valid, threads)
        logical, intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_bool_i32
        !> pf_rank over a logical array, with int64 ranks.
        module subroutine rank_bool_i64(values, ranks, method, descending, is_valid, threads)
        logical, intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_bool_i64
        !> pf_rank over a string array, with int32 ranks.
        module subroutine rank_chr_i32(values, ranks, method, descending, is_valid, threads)
        character(len=*), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_chr_i32
        !> pf_rank over a string array, with int64 ranks.
        module subroutine rank_chr_i64(values, ranks, method, descending, is_valid, threads)
        character(len=*), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_chr_i64
        !> pf_rank over a date array, with int32 ranks.
        module subroutine rank_date_i32(values, ranks, method, descending, threads)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_date_i32
        !> pf_rank over a date array, with int64 ranks.
        module subroutine rank_date_i64(values, ranks, method, descending, threads)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_date_i64
        !> pf_rank over a time array, with int32 ranks.
        module subroutine rank_time_i32(values, ranks, method, descending, threads)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_time_i32
        !> pf_rank over a time array, with int64 ranks.
        module subroutine rank_time_i64(values, ranks, method, descending, threads)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_time_i64
        !> pf_rank over a timestamp array, with int32 ranks.
        module subroutine rank_ts_i32(values, ranks, method, descending, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_ts_i32
        !> pf_rank over a timestamp array, with int64 ranks.
        module subroutine rank_ts_i64(values, ranks, method, descending, threads)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_ts_i64
        !> pf_rank over a packed string column array, with int32 ranks.
        module subroutine rank_strcol_i32(values, ranks, method, descending, threads)
        type(parquet_string_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_strcol_i32
        !> pf_rank over a packed string column array, with int64 ranks.
        module subroutine rank_strcol_i64(values, ranks, method, descending, threads)
        type(parquet_string_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_strcol_i64
        !> pf_rank over a type-erased column array, with int32 ranks.
        module subroutine rank_col_i32(values, ranks, method, descending, threads)
        type(parquet_column), intent(in) :: values
            integer(int32), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_col_i32
        !> pf_rank over a type-erased column array, with int64 ranks.
        module subroutine rank_col_i64(values, ranks, method, descending, threads)
        type(parquet_column), intent(in) :: values
            integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
            character(len=*), intent(in), optional :: method
            !! "competition" (the default), "dense" or "ordinal", case-insensitive.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            integer, intent(in), optional :: threads
            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the
            !! caller is not already inside an OpenMP parallel region, and serial when they are.
            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no
            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that
            !! governs row counts and indices here does not apply.
        end subroutine rank_col_i64
    end interface
    !
    ! ---- Extremes and merging (parquet_sorting_reduce) ----
    interface
        !> pf_minmax over a 32-bit integer array: its smallest and largest value.
        module subroutine minmax_i32(values, vmin, vmax, is_valid)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(out) :: vmin !! the smallest value.
        integer(int32), intent(out) :: vmax !! the largest value.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine minmax_i32
        !> pf_minmax over a 64-bit integer array: its smallest and largest value.
        module subroutine minmax_i64(values, vmin, vmax, is_valid)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(out) :: vmin !! the smallest value.
        integer(int64), intent(out) :: vmax !! the largest value.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine minmax_i64
        !> pf_minmax over a 32-bit real array: its smallest and largest value.
        module subroutine minmax_f32(values, vmin, vmax, is_valid)
        real(real32), intent(in) :: values(:)
        real(real32), intent(out) :: vmin !! the smallest value.
        real(real32), intent(out) :: vmax !! the largest value.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine minmax_f32
        !> pf_minmax over a 64-bit real array: its smallest and largest value.
        module subroutine minmax_f64(values, vmin, vmax, is_valid)
        real(real64), intent(in) :: values(:)
        real(real64), intent(out) :: vmin !! the smallest value.
        real(real64), intent(out) :: vmax !! the largest value.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine minmax_f64
        !> pf_minmax over a string array: its smallest and largest value.
        module subroutine minmax_chr(values, vmin, vmax, is_valid)
        character(len=*), intent(in) :: values(:)
        character(len=:), allocatable, intent(out) :: vmin !! the smallest value.
        character(len=:), allocatable, intent(out) :: vmax !! the largest value.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine minmax_chr
        !> pf_minmax over a date array: its smallest and largest value.
        module subroutine minmax_date(values, vmin, vmax)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(out) :: vmin !! the smallest value.
        type(parquet_date), intent(out) :: vmax !! the largest value.
        end subroutine minmax_date
        !> pf_minmax over a time array: its smallest and largest value.
        module subroutine minmax_time(values, vmin, vmax)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(out) :: vmin !! the smallest value.
        type(parquet_time), intent(out) :: vmax !! the largest value.
        end subroutine minmax_time
        !> pf_minmax over a timestamp array: its smallest and largest value.
        module subroutine minmax_ts(values, vmin, vmax)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(out) :: vmin !! the smallest value.
        type(parquet_timestamp), intent(out) :: vmax !! the largest value.
        end subroutine minmax_ts
        !> pf_minmax over a packed string column array: its smallest and largest value.
        module subroutine minmax_strcol(values, vmin, vmax)
        type(parquet_string_column), intent(in) :: values
        character(len=:), allocatable, intent(out) :: vmin !! the smallest value.
        character(len=:), allocatable, intent(out) :: vmax !! the largest value.
        end subroutine minmax_strcol
        !> pf_argminmax over a 32-bit integer array, with int32 indices.
        module subroutine argminmax_i32_i32(values, imin, imax, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_i32_i32
        !> pf_argminmax over a 32-bit integer array, with int64 indices.
        module subroutine argminmax_i32_i64(values, imin, imax, is_valid)
        integer(int32), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_i32_i64
        !> pf_argminmax over a 64-bit integer array, with int32 indices.
        module subroutine argminmax_i64_i32(values, imin, imax, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_i64_i32
        !> pf_argminmax over a 64-bit integer array, with int64 indices.
        module subroutine argminmax_i64_i64(values, imin, imax, is_valid)
        integer(int64), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_i64_i64
        !> pf_argminmax over a 32-bit real array, with int32 indices.
        module subroutine argminmax_f32_i32(values, imin, imax, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_f32_i32
        !> pf_argminmax over a 32-bit real array, with int64 indices.
        module subroutine argminmax_f32_i64(values, imin, imax, is_valid)
        real(real32), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_f32_i64
        !> pf_argminmax over a 64-bit real array, with int32 indices.
        module subroutine argminmax_f64_i32(values, imin, imax, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_f64_i32
        !> pf_argminmax over a 64-bit real array, with int64 indices.
        module subroutine argminmax_f64_i64(values, imin, imax, is_valid)
        real(real64), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_f64_i64
        !> pf_argminmax over a string array, with int32 indices.
        module subroutine argminmax_chr_i32(values, imin, imax, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_chr_i32
        !> pf_argminmax over a string array, with int64 indices.
        module subroutine argminmax_chr_i64(values, imin, imax, is_valid)
        character(len=*), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        end subroutine argminmax_chr_i64
        !> pf_argminmax over a date array, with int32 indices.
        module subroutine argminmax_date_i32(values, imin, imax)
        type(parquet_date), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_date_i32
        !> pf_argminmax over a date array, with int64 indices.
        module subroutine argminmax_date_i64(values, imin, imax)
        type(parquet_date), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_date_i64
        !> pf_argminmax over a time array, with int32 indices.
        module subroutine argminmax_time_i32(values, imin, imax)
        type(parquet_time), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_time_i32
        !> pf_argminmax over a time array, with int64 indices.
        module subroutine argminmax_time_i64(values, imin, imax)
        type(parquet_time), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_time_i64
        !> pf_argminmax over a timestamp array, with int32 indices.
        module subroutine argminmax_ts_i32(values, imin, imax)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_ts_i32
        !> pf_argminmax over a timestamp array, with int64 indices.
        module subroutine argminmax_ts_i64(values, imin, imax)
        type(parquet_timestamp), intent(in) :: values(:)
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_ts_i64
        !> pf_argminmax over a packed string column array, with int32 indices.
        module subroutine argminmax_strcol_i32(values, imin, imax)
        type(parquet_string_column), intent(in) :: values
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_strcol_i32
        !> pf_argminmax over a packed string column array, with int64 indices.
        module subroutine argminmax_strcol_i64(values, imin, imax)
        type(parquet_string_column), intent(in) :: values
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_strcol_i64
        !> pf_argminmax over a type-erased column array, with int32 indices.
        module subroutine argminmax_col_i32(values, imin, imax)
        type(parquet_column), intent(in) :: values
            integer(int32), intent(out) :: imin !! where the smallest value is.
            integer(int32), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_col_i32
        !> pf_argminmax over a type-erased column array, with int64 indices.
        module subroutine argminmax_col_i64(values, imin, imax)
        type(parquet_column), intent(in) :: values
            integer(int64), intent(out) :: imin !! where the smallest value is.
            integer(int64), intent(out) :: imax !! where the largest value is.
        end subroutine argminmax_col_i64
        !> pf_merge over two sorted 32-bit integer arrays.
        module subroutine merge_i32(a, b, merged, is_valid_a, is_valid_b, merged_valid, descending, nulls_first, assume_sorted)
        integer(int32), intent(in) :: a(:)
        integer(int32), intent(in) :: b(:)
            integer(int32), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.
            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.
            logical, allocatable, intent(out), optional :: merged_valid(:)
            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when
            !! neither input mask was supplied, since the caller asked a direct question.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_i32
        !> pf_merge over two sorted 64-bit integer arrays.
        module subroutine merge_i64(a, b, merged, is_valid_a, is_valid_b, merged_valid, descending, nulls_first, assume_sorted)
        integer(int64), intent(in) :: a(:)
        integer(int64), intent(in) :: b(:)
            integer(int64), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.
            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.
            logical, allocatable, intent(out), optional :: merged_valid(:)
            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when
            !! neither input mask was supplied, since the caller asked a direct question.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_i64
        !> pf_merge over two sorted 32-bit real arrays.
        module subroutine merge_f32(a, b, merged, is_valid_a, is_valid_b, merged_valid, descending, nulls_first, assume_sorted)
        real(real32), intent(in) :: a(:)
        real(real32), intent(in) :: b(:)
            real(real32), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.
            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.
            logical, allocatable, intent(out), optional :: merged_valid(:)
            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when
            !! neither input mask was supplied, since the caller asked a direct question.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_f32
        !> pf_merge over two sorted 64-bit real arrays.
        module subroutine merge_f64(a, b, merged, is_valid_a, is_valid_b, merged_valid, descending, nulls_first, assume_sorted)
        real(real64), intent(in) :: a(:)
        real(real64), intent(in) :: b(:)
            real(real64), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.
            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.
            logical, allocatable, intent(out), optional :: merged_valid(:)
            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when
            !! neither input mask was supplied, since the caller asked a direct question.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_f64
        !> pf_merge over two sorted logical arrays.
        module subroutine merge_bool(a, b, merged, is_valid_a, is_valid_b, merged_valid, descending, nulls_first, assume_sorted)
        logical, intent(in) :: a(:)
        logical, intent(in) :: b(:)
            logical, allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.
            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.
            logical, allocatable, intent(out), optional :: merged_valid(:)
            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when
            !! neither input mask was supplied, since the caller asked a direct question.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_bool
        !> pf_merge over two sorted string arrays.
        module subroutine merge_chr(a, b, merged, is_valid_a, is_valid_b, merged_valid, descending, nulls_first, assume_sorted)
        character(len=*), intent(in) :: a(:)
        character(len=*), intent(in) :: b(:)
            character(len=:), allocatable, intent(out) :: merged(:)
            !! the merged copy, widened to `max(len(a), len(b))`. DEFERRED-length,
            !! unlike `pf_sort`'s output, because the width comes from two inputs
            !! rather than one -- so declare it `character(len=:), allocatable`.
            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.
            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.
            logical, allocatable, intent(out), optional :: merged_valid(:)
            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when
            !! neither input mask was supplied, since the caller asked a direct question.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_chr
        !> pf_merge over two sorted date arrays.
        module subroutine merge_date(a, b, merged, descending, nulls_first, assume_sorted)
        type(parquet_date), intent(in) :: a(:)
        type(parquet_date), intent(in) :: b(:)
            type(parquet_date), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_date
        !> pf_merge over two sorted time arrays.
        module subroutine merge_time(a, b, merged, descending, nulls_first, assume_sorted)
        type(parquet_time), intent(in) :: a(:)
        type(parquet_time), intent(in) :: b(:)
            type(parquet_time), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_time
        !> pf_merge over two sorted timestamp arrays.
        module subroutine merge_ts(a, b, merged, descending, nulls_first, assume_sorted)
        type(parquet_timestamp), intent(in) :: a(:)
        type(parquet_timestamp), intent(in) :: b(:)
            type(parquet_timestamp), allocatable, intent(out) :: merged(:) !! the merged copy.
            logical, intent(in), optional :: descending !! .true. for high-to-low order.
            logical, intent(in), optional :: nulls_first !! .true. when nulls come first.
            logical, intent(in), optional :: assume_sorted
            !! .true. skips the O(n) sortedness check on BOTH inputs.
        end subroutine merge_ts
    end interface
    !
    ! ---- The comparator core (parquet_sorting_engine -- HAND-WRITTEN, not generated) ----
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
        !! (`parquet_get_sort_parallel_min_rows`), below which a team costs more than it saves,
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
    ! ---- Test-only access to the comparator core (parquet_sorting_engine) ----
    interface
        !> Test-only view of what the Fortran SORT comparator says about one pair of rows.
        !!
        !! Public only because it has to be: `sort_key_buf` is private to this module, so a
        !! test cannot reach `sort_row_less` any other way, and the C++-side hook convention
        !! is unavailable for a decision that Stage 1 exists to move out of C++. Not called by
        !! library code. Rows are 1-based, as everywhere else in this module's public API.
        module function parquet_debug_sort_row_less(keys, a, b) result(less)
            type(pf_sort_keys), intent(in) :: keys !! the built key set.
            integer(int64), intent(in) :: a        !! first row, 1-based.
            integer(int64), intent(in) :: b        !! second row, 1-based.
            logical :: less                        !! .true. when `a` sorts before `b`.
        end function parquet_debug_sort_row_less
        !> Test-only view of what the Fortran TIE-FREE comparator says about one pair of rows.
        !!
        !! Same reasoning as `parquet_debug_sort_row_less`. `nkeys` counts ENGINE keys and is
        !! clamped to how many the set holds; note a `parquet_timestamp` key binds as two.
        module function parquet_debug_sort_keys_compare(keys, a, b, nkeys) result(c)
            type(pf_sort_keys), intent(in) :: keys !! the built key set.
            integer(int64), intent(in) :: a        !! first row, 1-based.
            integer(int64), intent(in) :: b        !! second row, 1-based.
            integer, intent(in) :: nkeys           !! leading engine keys taking part.
            integer :: c                           !! -1, 0 or +1.
        end function parquet_debug_sort_keys_compare
        !> Test-only sweep of `nreps` passes of `nrows` comparisons, returning a checksum.
        !!
        !! For app/benchmark_sort_comparator.f90, which needs the comparator's own cost rather
        !! than the cost of reaching it: at ~5 ns per comparison a per-call harness measures
        !! its own overhead. The C++ twin is `parquet_debug_sort_sweep_less_cpp` in
        !! src/parquet_wrapper.cpp and the two loops are deliberately identical, down to the
        !! stride walk — their checksums must agree, which is what proves they did the same
        !! work. Neither uses `mod` on a runtime divisor: that is an integer division, and it
        !! would cost more than the comparison being timed.
        module function parquet_debug_sort_sweep_less(keys, nrows, nreps) result(count)
            type(pf_sort_keys), intent(in) :: keys  !! the built key set.
            integer(int64), intent(in) :: nrows     !! rows to walk per pass.
            integer(int64), intent(in) :: nreps     !! passes.
            integer(int64) :: count                 !! how many pairs compared less; -1 if unusable.
        end function parquet_debug_sort_sweep_less
        !> Test-only twin of that sweep for the tie-free comparator, summing its answers.
        module function parquet_debug_sort_sweep_compare(keys, nrows, nreps, nkeys) result(total)
            type(pf_sort_keys), intent(in) :: keys  !! the built key set.
            integer(int64), intent(in) :: nrows     !! rows to walk per pass.
            integer(int64), intent(in) :: nreps     !! passes.
            integer, intent(in) :: nkeys            !! leading engine keys taking part.
            integer(int64) :: total                 !! sum of the answers; -1 if unusable.
        end function parquet_debug_sort_sweep_compare
        !> Test-only switch routing `pf_argsort` and friends to the Fortran engine, or back to C++.
        !!
        !! Stage 2 scaffolding, deleted at the Stage 6 cutover. Both engines answer identically
        !! -- that is what the conformance tests assert -- so this changes timing and nothing
        !! else, which is exactly why it is a debug hook rather than a setting.
        module subroutine parquet_debug_use_fortran_sort_engine(on)
            logical, intent(in) :: on !! .true. selects the Fortran engine.
        end subroutine parquet_debug_use_fortran_sort_engine
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
        !! The engine's floor is internal and automatic, so unlike the tail's it has no
        !! published setting to move it. `parquet_set_sort_parallel_min_rows` still governs
        !! the C++ engine and is unaffected by this.
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
end module parquet_sorting ! GCOVR_EXCL_LINE
