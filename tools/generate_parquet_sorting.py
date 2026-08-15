#!/usr/bin/env python3
"""Generate the per-type blocks of the `parquet_sorting` module.

`parquet_sorting` is this library's public sorting API: `pf_sort`, `pf_argsort`, `pf_permute`
and `pf_is_sorted` over eleven element types, plus the `pf_sort_keys` multi-key builder. That
is the same handful of statements repeated per type, which is what this script emits -- by
hand it is ~75 near-identical procedures for M1 alone, each needing its own `!>`/`!!`
doc-comments.

Emitted (all COMMITTED to the repository, exactly like the other generators' output -- nothing
is generated at build time, so the fpm build stays dependency-free):

  src/parquet_sorting.f90           module spec: pf_sort_keys, the public generics, and the
                                    interface bodies for every submodule-implemented procedure.
  src/parquet_sorting_keys.f90      key extraction (one per type) + pf_sort_keys%add.
  src/parquet_sorting_argsort.f90   pf_argsort and pf_sort.
  src/parquet_sorting_permute.f90   pf_permute and pf_is_sorted.
  src/parquet_sorting_select.f90    pf_partial_sort, pf_partial_argsort, pf_nth_element,
                                    pf_nth_quantile.
  src/parquet_sorting_search.f90    pf_lower_bound, pf_upper_bound, pf_equal_range.
  src/parquet_sorting_unique.f90    pf_unique_count, pf_unique, pf_rank.
  src/parquet_sorting_reduce.f90    pf_minmax, pf_argminmax, pf_merge.

THE KIND TABLE: the nine scalar rows are imported from tools/generate_parquet_columns.py --
the single place a supported column kind is declared -- and the three extras this module adds
(character(len=*), parquet_string_column, parquet_column) are declared locally, because they
are not `parquet_column` storage kinds at all. `--self-test` cross-checks the imported set, so
a change over there fails here rather than silently changing what this module supports.

Usage:  tools/generate_parquet_sorting.py [--check] [--self-test]

  --check      regenerate into memory and compare with the committed files; exit 1 on any
               difference (so CI, or a reviewer, can prove the committed output is current).
  --self-test  verify the imported kind table still has the shape this script assumes.
"""

import argparse
import importlib.util
import pathlib
import sys

REPO_ROOT = pathlib.Path(__file__).resolve().parent.parent


def _load_scalar_kinds():
    """Import the 9 SCALAR rows of the columns generator's kind table -- one source of truth."""
    path = REPO_ROOT / "tools" / "generate_parquet_columns.py"
    spec = importlib.util.spec_from_file_location("_pq_columns_gen", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return [k for k in mod.KINDS if k[4] == 1]


#: Tags of the imported scalar kinds, in order. --self-test asserts this still holds; the
#: emitted module's type coverage is defined by it.
EXPECTED_SCALAR_TAGS = ["i32", "i64", "f32", "f64", "bool", "str", "date", "time", "ts"]

# --------------------------------------------------------------------------------------
# The type table. One row per element type this module sorts.
#
#   tag       short name used in every specific's name (argsort_<tag>_i32, ...)
#   decl      the Fortran declaration of ONE element
#   what      human-readable name, used in doc-comments
#   family    how the values reach the engine:
#               'int'    integer-valued, widened into an int64 buffer
#               'int64'  integer-valued and already int64 -- BORROWED, never copied
#               'real'   real-valued, widened into a real64 buffer
#               'real64' already real64 -- BORROWED, never copied
#               'chr'    character(len=*) array, packed into (offsets, data)
#               'strcol' parquet_string_column, packed into (offsets, data)
#               'ts'     parquet_timestamp -- TWO integer keys (seconds, nanoseconds)
#               'col'    parquet_column -- kind resolved at runtime, may be two keys
#   nulls     where nullness comes from:
#               'arg'  an optional is_valid(:) logical argument
#               'elem' the elements themselves (temporal kinds)
#               'own'  the container's own validity (strcol, col)
#   sort      .true. when pf_sort (out-of-place) applies -- false for the container types
#   permute   how pf_permute reorders it: 'gather' (array) or 'reindex' (container)
# --------------------------------------------------------------------------------------
TYPES = [
    ("i32",    "integer(int32)",             "32-bit integer",   "int",    "arg",  True,  "gather"),
    ("i64",    "integer(int64)",             "64-bit integer",   "int64",  "arg",  True,  "gather"),
    ("f32",    "real(real32)",               "32-bit real",      "real",   "arg",  True,  "gather"),
    ("f64",    "real(real64)",               "64-bit real",      "real64", "arg",  True,  "gather"),
    ("bool",   "logical",                    "logical",          "int",    "arg",  True,  "gather"),
    ("chr",    "character(len=*)",           "string",           "chr",    "arg",  True,  "gather"),
    ("date",   "type(parquet_date)",         "date",             "int",    "elem", True,  "gather"),
    ("time",   "type(parquet_time)",         "time",             "int",    "elem", True,  "gather"),
    ("ts",     "type(parquet_timestamp)",    "timestamp",        "ts",     "elem", True,  "gather"),
    ("strcol", "type(parquet_string_column)", "packed string column", "strcol", "own", False, "reindex"),
    ("col",    "type(parquet_column)",       "type-erased column", "col",  "own",  False, "reindex"),
]

#: Families whose values are handed to the engine without a copy, rather than through the builder.
BORROWED = ("int64", "real64")

#: Families that need the multi-key builder rather than a one-shot call: a timestamp is two
#: integer keys (its (seconds, nanoseconds) pair cannot be folded into one int64 without
#: overflowing outside roughly 1678-2262), and a parquet_column may be a timestamp.
MULTIKEY = ("ts", "col")

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!"""

IDX_KINDS = [("i32", "integer(int32)", "int32"), ("i64", "integer(int64)", "int64")]


def wrap_list(names, indent, width=110):
    """Join `names` with ', ', wrapping with Fortran '&' continuations."""
    pad = " " * indent
    out, line = [], ""
    for i, name in enumerate(names):
        piece = name + (", " if i < len(names) - 1 else "")
        if line and len(pad) + len(line) + len(piece) > width:
            out.append(line + "&")
            line = piece
        else:
            line += piece
    out.append(line)
    return ("\n" + pad).join(out)


#: Types `pf_nth_element`/`pf_nth_quantile` apply to: every one except `parquet_column`, whose
#: element type is a runtime discriminator, so there is no compile-time type for `p_value`
#: (a whole vector row has no defined order, so no *_VEC kind can be a key).
def has_nth(t):
    return t[0] != "col"


#: Types the M3 families apply to. Each exclusion comes from the same two
#: rules -- a type is out wherever the answer would need a compile-time element type it does not
#: have, and it stays in wherever the answer is a permutation, a boolean or an integer -- so these
#: are derived, not chosen. `pf_argminmax` keeping `parquet_column` while `pf_minmax` drops it is
#: that second rule; `logical` is out of both as vacuous (the index of the first .false. is not a
#: question worth an API).
def has_search(t):
    return t[0] != "col"       # a search needs a target value of the element's own type


def has_unique(t):
    return t[0] != "col"       # `distinct` needs a compile-time element type


def has_minmax(t):
    return t[0] not in ("col", "bool")


def has_argminmax(t):
    return t[0] != "bool"


def has_merge(t):
    return t[3] not in ("strcol", "col")   # merging is defined on plain arrays only


def tgt_decl(t, name="target"):
    """Declaration of ONE search target of type `t`, as an intent(in) scalar."""
    tag, decl, what, family, nulls, _, _ = t
    if family in ("chr", "strcol"):
        return f"        character(len=*), intent(in) :: {name}"
    return f"        {decl}, intent(in) :: {name}"


def pval_decl(t, name="p_value"):
    """Declaration of ONE element of type `t`, as an intent(out) result."""
    tag, decl, what, family, nulls, _, _ = t
    if family in ("chr", "strcol"):
        return f"        character(len=:), allocatable, intent(out) :: {name}"
    return f"        {decl}, intent(out) :: {name}"


def val_decl(t, intent, name="values"):
    """Declaration of the values array for type `t`."""
    tag, decl, what, family, nulls, _, _ = t
    if family in ("strcol", "col"):
        return f"        {decl}, intent({intent}) :: {name}"
    return f"        {decl}, intent({intent}) :: {name}(:)"


def opt_valid(t):
    """The optional is_valid argument, for the six types that take one."""
    if t[4] != "arg":
        return []
    return ["        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null."]


#: The public `threads=` argument, on Open-1's six generics only. A thread count cannot exceed
#: int32, so CLAUDE.md's dual-kind rule does not apply and the absence of an int64 form is stated
#: in the doc-comment rather than left to be "fixed" later.
THREADS_DOC = [
    "            integer, intent(in), optional :: threads",
    "            !! how many threads to sort with. ABSENT means auto: `omp_get_max_threads()` when the",
    "            !! caller is not already inside an OpenMP parallel region, and serial when they are.",
    "            !! `threads=1` forces serial. Deliberately a single default-kind `integer` with no",
    "            !! int64 form -- a thread count cannot exceed int32, so the dual-kind rule that",
    "            !! governs row counts and indices here does not apply.",
]

#: `group_nkeys`, on the two `pf_sort_keys` specifics only -- the per-type ones hold a single key,
#: so a prefix of it could only ever be the whole thing.
GROUP_NKEYS_DOC = [
    "            !> how many LEADING keys have to be equal for two rows to share a group. ABSENT",
    "            !! means all of them. Counts the keys YOU added, one per `%add` call, which is not",
    "            !! always the engine's own count -- one `parquet_timestamp` key becomes two engine",
    "            !! keys internally, and this argument never exposes that.",
    "            !!",
    "            !! **It does not change the sort.** Every key still orders the rows; only the",
    "            !! equality test that closes a group is narrowed. That is what gives \"group by",
    "            !! field, ordered by magnitude within each group\": sort by both, group on the first.",
    "            !!",
    "            !! Must be between 1 and the number of keys, and requires `group_offsets` -- on its",
    "            !! own it would change nothing, so passing it alone is an error rather than a no-op.",
    "            !! A single default-kind `integer` with no int64 form: a key count cannot approach",
    "            !! `huge(int32)`.",
    "            integer, intent(in), optional :: group_nkeys",
]


def group_offsets_doc(idecl):
    """The `group_offsets` doc block, whose kind always follows `perm`'s (they index the same array)."""
    return [
        "            !> where each run of rows comparing EQUAL under the grouping keys begins, as",
        "            !! offsets INTO `perm`: length `ngroups + 1`, with the last entry the sentinel",
        "            !! `n + 1`, so group g is `perm(o(g) : o(g+1) - 1)` for every g and",
        "            !! `ngroups = size(o) - 1`. No last-iteration special case, which is where an",
        "            !! off-by-one usually gets written.",
        "            !!",
        "            !! Always allocated when asked for: an empty array gives `[1]` (no groups), one",
        "            !! element gives `[1, 2]`. All nulls form ONE group and all NaNs form ONE group,",
        "            !! because rows in the same non-value tier compare equal -- deliberately unlike",
        "            !! `pf_unique`, which drops nulls entirely, since a group list must account for",
        "            !! every row.",
        "            !!",
        "            !! Costs one extra copy of a single key: boundaries come from the builder path,",
        "            !! so asking for them gives up the one-shot borrow a lone key would otherwise use.",
        f"            {idecl}, allocatable, intent(out), optional :: group_offsets(:)",
    ]


COMMON_OPTS = [
    "        logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.",
    "        logical, intent(in), optional :: nulls_first !! .true. places nulls before values; default .false.",
]


# --------------------------------------------------------------------------------------
# src/parquet_sorting.f90 -- the module spec
# --------------------------------------------------------------------------------------
def gen_spec():
    o = []
    w = o.append
    w(BANNER)
    w('''!> Sorting for plain Fortran arrays and for this library's own column types.
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
    public :: parquet_debug_set_sort_radix_fail_alloc
    public :: parquet_debug_reset_sort_radix_passes
    public :: parquet_debug_sort_radix_passes
    public :: parquet_debug_sort_threads_used
    public :: parquet_debug_sort_split_buckets
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
    contains''')
    for t in TYPES:
        w(f"        procedure, private :: add_{t[0]} !! %add specific for a {t[2]} key.")
    w("        !> Appends one sort key. Keys apply in the order added, the first being primary.")
    w("        generic :: add => " + wrap_list([f"add_{t[0]}" for t in TYPES], 12))
    w("        procedure :: nkeys_added => keys_count !! Keys added so far, one per %add call.")
    w("        procedure :: clear => keys_clear       !! Drops every key, leaving the object reusable.")
    w("    end type pf_sort_keys")
    w("    !")

    # ---- generic interfaces ----
    w("    !> The permutation that would sort `values`: `perm(k)` is the index of the element that")
    w("    !> belongs at position k. `values` is never modified.")
    w("    !>")
    w("    !> The permutation's integer kind is chosen by how the caller declares `perm`. The")
    w("    !> `integer(int32)` form aborts when the array is longer than `huge(1_int32)` rather than")
    w("    !> truncating; declare `perm` as `integer(int64)` for arrays that large.")
    w("    !>")
    w("    !> Also takes a `pf_sort_keys` object in place of `values`, for a multi-key sort.")
    w("    interface pf_argsort")
    for t in TYPES:
        for ik, _, _ in IDX_KINDS:
            w(f"        module procedure argsort_{t[0]}_{ik}")
    for ik, _, _ in IDX_KINDS:
        w(f"        module procedure argsort_keys_{ik}")
    w("    end interface pf_argsort")
    w("    !")
    w("    !> An independent sorted copy of `values`, leaving `values` untouched.")
    w("    !>")
    w("    !> Deliberately not defined for `parquet_string_column` or `parquet_column`: copying a")
    w("    !> whole column to sort it serves no purpose, and reordering one in place is")
    w("    !> `pf_argsort` followed by `pf_permute`, which says what it does at the call site.")
    w("    interface pf_sort")
    for t in TYPES:
        if t[5]:
            w(f"        module procedure sort_{t[0]}")
    w("    end interface pf_sort")
    w("    !")
    w("    !> Applies `perm` to `values` IN PLACE: afterwards element k is what was at `perm(k)`.")
    w("    !> `perm` itself is not modified.")
    w("    !>")
    w("    !> `perm` is validated as a true permutation of 1..n before anything is written, since an")
    w("    !> invalid one would silently duplicate some elements and drop others. Pass")
    w("    !> `assume_valid=.true.` to skip that check when the permutation came from `pf_argsort`")
    w("    !> and is known good -- it means the same thing for all eleven types, the two column ones")
    w("    !> included.")
    w("    !>")
    w("    !> **`assume_valid` skips the O(n) contents check only.** `perm`'s LENGTH is checked either")
    w("    !> way, because a short permutation would make the gather read past the end of `values` and")
    w("    !> no promise from the caller can make that defined.")
    w("    interface pf_permute")
    for t in TYPES:
        for ik, _, _ in IDX_KINDS:
            w(f"        module procedure permute_{t[0]}_{ik}")
    w("    end interface pf_permute")
    w("    !")
    w("    !> Whether `values` is already in the stated order. O(n) with an early exit, and no copy.")
    w("    !>")
    w("    !> Uses the same comparison `pf_sort` does, so the two can never disagree about nulls,")
    w("    !> NaNs or direction on one array. A run of equal values is sorted.")
    w("    interface pf_is_sorted")
    for t in TYPES:
        w(f"        module procedure is_sorted_{t[0]}")
    w("        module procedure is_sorted_keys")
    w("    end interface pf_is_sorted")
    w("    !")
    w("    !> The permutation that would sort the FIRST `n` elements of `values`, without ordering")
    w("    !> the rest. `perm` comes back with exactly `n` entries (fewer if the array is shorter).")
    w("    !>")
    w("    !> `n` is CLAMPED to the array size rather than being an error, so a caller whose `n` is")
    w("    !> derived -- a fraction of a row count, a config value, a post-filter survivor count --")
    w("    !> needs no `min(n, size(v))` of their own. A negative `n` is still an error.")
    w("    !>")
    w("    !> \"The last n\" is `descending=.true.`, not a separate procedure.")
    w("    interface pf_partial_argsort")
    for t in TYPES:
        for ik, _, _ in IDX_KINDS:
            w(f"        module procedure partial_argsort_{t[0]}_{ik}")
    for ik, _, _ in IDX_KINDS:
        w(f"        module procedure partial_argsort_keys_{ik}")
    w("    end interface pf_partial_argsort")
    w("    !")
    w("    !> The first `n` elements of `values` in order, as an independent copy of length `n`.")
    w("    !> Same clamping rule as `pf_partial_argsort`. Never modifies its input.")
    w("    !>")
    w("    !> Cheaper than `pf_sort` only while `n` stays well below the array size -- the underlying")
    w("    !> `std::partial_sort` degrades past a full sort as `n` approaches it. At `n = size` this")
    w("    !> is strictly worse than calling `pf_sort`.")
    w("    interface pf_partial_sort")
    for t in TYPES:
        if t[5]:
            w(f"        module procedure partial_sort_{t[0]}")
    w("    end interface pf_partial_sort")
    w("    !")
    w("    !> The element a full sort would place at 1-based rank `nth`, without sorting -- O(n)")
    w("    !> rather than O(n log n). `index` optionally reports which element of `values` that was.")
    w("    !>")
    w("    !> **The reported index is the one a full STABLE sort would give.** `std::nth_element`")
    w("    !> normally leaves an arbitrary member of an equal-comparing run at that position; here")
    w("    !> the comparator ends with a tiebreaker on the original index, making it a total order")
    w("    !> under which no two elements compare equal, so the answer is deterministic and agrees")
    w("    !> with `pf_sort` element for element.")
    w("    !>")
    w("    !> `nth` counts NULLS too, placed by the same tier rules as the sort (last by default).")
    w("    !> Takes `descending`/`nulls_first`/`is_valid` exactly as `pf_argsort` does.")
    w("    interface pf_nth_element")
    for t in TYPES:
        if not has_nth(t):
            continue
        for nk, _, _ in IDX_KINDS:
            w(f"        module procedure nth_{t[0]}_{nk}")
            for ik, _, _ in IDX_KINDS:
                w(f"        module procedure nth_{t[0]}_{nk}_{ik}")
    w("    end interface pf_nth_element")
    w("    !")
    w("    !> The value at `quantile` (on a **0-1 scale**, not 0-100) of the NON-NULL values.")
    w("    !> `index` optionally reports which element that was; `n_null` how many were excluded.")
    w("    !>")
    w("    !> **Nulls are excluded from the population, not placed in it** -- unlike every other")
    w("    !> operation in this module, which is why this one takes neither `descending` nor")
    w("    !> `nulls_first`: there is no null tier to position, and a descending quantile is just")
    w("    !> `1 - quantile`.")
    w("    !>")
    w("    !> `rounding=` selects how a fractional position is resolved: `\"nearest\"` (the default),")
    w("    !> `\"down\"` or `\"up\"`, matched case-insensitively. An unrecognized token aborts.")
    w("    !>")
    w("    !> Aborts when EVERY value is null: there is no value to return, and no sentinel exists")
    w("    !> across all ten types. `n_null` is for PARTIAL nullness; the all-null case never")
    w("    !> reaches it. Guard with `count(mask)` (or a column's own null count) if that matters.")
    w("    interface pf_nth_quantile")
    for t in TYPES:
        if not has_nth(t):
            continue
        w(f"        module procedure quantile_{t[0]}")
        for ik, _, _ in IDX_KINDS:
            w(f"        module procedure quantile_{t[0]}_{ik}")
    w("    end interface pf_nth_quantile")
    w("    !")

    # ---- M3 generics ----
    for gname, blurb in (
        ("pf_lower_bound",
         ["The first position at which `target` could be inserted into an already-sorted",
          "`values` without breaking its order -- i.e. the first element not ordered BEFORE it.",
          "",
          "`pos` lands in `1 .. size(values)+1`; it is `size(values)+1` when every element is",
          "ordered before the target. Together with `pf_upper_bound` it brackets every element",
          "equal to the target, which is what `pf_equal_range` returns in one call.",
          "",
          "**`values` is checked for sortedness first, and that check is O(n).** Searching an",
          "unsorted array returns a plausible index with no symptom at all, so the check is on by",
          "default. Check once with `pf_is_sorted` and pass `assume_sorted=.true.` in a loop:",
          "",
          "```fortran",
          "call pf_is_sorted(v, ok)                       ! O(N), once",
          "do k = 1, m",
          "    call pf_lower_bound(v, targets(k), pos, assume_sorted=.true.)   ! O(log N) each",
          "end do",
          "```",
          "",
          "`descending`/`nulls_first` must describe the order `values` is ACTUALLY in -- they",
          "select the comparison, they do not reorder anything."]),
        ("pf_upper_bound",
         ["The first position at which `target` is ordered BEFORE the element there -- i.e. one",
          "past the last element equal to the target.",
          "",
          "Same arguments, same sortedness rule and same `1 .. size(values)+1` range as",
          "`pf_lower_bound`; `pf_upper_bound - pf_lower_bound` is how many elements equal the",
          "target."]),
        ("pf_equal_range",
         ["The INCLUSIVE range `first .. last` of elements equal to `target`, from one pass.",
          "",
          "`first` is `pf_lower_bound`'s answer and `last` is `pf_upper_bound`'s minus one, so a",
          "target that is absent comes back with `last == first - 1` and `last - first + 1 == 0`.",
          "Do not read `values(first)` without checking that count first.",
          "",
          "Cheaper than calling the two bounds separately: the values are extracted once."]),
        ("pf_unique_count",
         ["How many DISTINCT non-null values `values` holds. `n_null` optionally reports how many",
          "were null.",
          "",
          "**Nulls are excluded from the population, not counted as one value** -- the same rule",
          "`pf_nth_quantile` follows, and the reason this takes neither `descending` (a count does",
          "not depend on direction) nor `nulls_first` (there is no null tier to place).",
          "",
          "Distinctness is the sort comparator's own equality, so on a floating-point array it is",
          "EXACT: `0.1 + 0.2` and `0.3` are two distinct values. Every NaN counts as one value,",
          "collectively, since NaNs compare equal to each other here (they do not under `==`)."]),
        ("pf_unique",
         ["The distinct non-null values of `values`, in order, as an independent copy.",
          "",
          "Same distinctness rule as `pf_unique_count` -- exact for reals, all NaNs collapsing to",
          "one. `descending` chooses the order the distinct values come back in; there is no",
          "`nulls_first`, because nulls are excluded rather than placed.",
          "",
          "Each distinct value is taken from its FIRST occurrence in the sorted order, which for",
          "equal-comparing-but-not-identical values (a `character` array's trailing blanks, a",
          "`parquet_string_column`'s empty strings) is the earliest such element of `values`."]),
        ("pf_rank",
         ["The rank of every element of `values`, without reordering it. `ranks(i)` is the rank of",
          "`values(i)`, so this is a per-element answer rather than a permutation.",
          "",
          "`method=` chooses how ties are handled, matched case-insensitively:",
          "",
          "| token | ranks of `10, 20, 20, 30` |",
          "|---|---|",
          "| `\"competition\"` (the default) | 1, 2, 2, 4 |",
          "| `\"dense\"` | 1, 2, 2, 3 |",
          "| `\"ordinal\"` | 1, 2, 3, 4 |",
          "",
          "**A null gets rank 0**, which is why this takes `descending` but NOT `nulls_first`: a",
          "null has no rank at all, so there is no position for `nulls_first` to choose. NaNs are",
          "ranked as ordinary values (all tying with each other), unlike nulls.",
          "",
          "`\"ordinal\"` ranks are exactly the inverse of `pf_argsort`'s permutation."]),
        ("pf_minmax",
         ["The smallest and largest value in `values`, skipping nulls and NaNs.",
          "",
          "Takes no `descending`/`nulls_first`: a minimum and a maximum are absolute, and reversing",
          "the order would only exchange the two answers.",
          "",
          "**Aborts when every value is null or NaN** -- there is nothing to return, and no",
          "sentinel exists across all nine types. This matches `pf_nth_quantile`'s decision for the",
          "same degenerate case; guard with `count(is_valid)` where that can happen.",
          "",
          "Use `pf_argminmax` when the positions matter rather than the values."]),
        ("pf_argminmax",
         ["WHERE the smallest and largest value of `values` are: `imin`/`imax` are 1-based indices",
          "into `values`, skipping nulls and NaNs.",
          "",
          "The index-returning twin of `pf_minmax`, split off because Fortran cannot offer both",
          "answers from one generic -- optional `imin`/`imax` varying only by integer kind would",
          "make a positional call ambiguous. A caller wanting both pays one extra call.",
          "",
          "Ties report the FIRST occurrence, which is the element a full stable sort would place at",
          "either end. Aborts on an all-null-or-NaN input, exactly as `pf_minmax` does. Defined for",
          "`parquet_column` as well, since an index needs no compile-time element type."]),
        ("pf_merge",
         ["Merges two ALREADY-SORTED arrays into one sorted array, in O(size(a) + size(b)) rather",
          "than the O(n log n) of sorting their concatenation.",
          "",
          "`descending`/`nulls_first` must match the order `a` and `b` are actually in -- they",
          "select the comparison, exactly as in the searches. Both inputs are checked for",
          "sortedness unless `assume_sorted=.true.`.",
          "",
          "**Supply `is_valid_a`/`is_valid_b` whenever either input has nulls.** A sorted array",
          "containing nulls is what `pf_sort(..., is_valid=)` produces, and a merge that is not told",
          "which elements are null compares them as ordinary values and interleaves them into the",
          "middle of the result. The precondition cannot be checked, either: a null's stored value",
          "is indistinguishable from a real one without the mask.",
          "",
          "`merged_valid` reports the result's validity and is ALWAYS allocated when asked for, all",
          "`.true.` when neither input mask was supplied. Ties take from `a` first, so the result",
          "matches `pf_sort` of the concatenation element for element."]),
    ):
        for line in blurb:
            w(("    !> " + line).rstrip())
        w(f"    interface {gname}")
        if gname in ("pf_lower_bound", "pf_upper_bound", "pf_equal_range"):
            base = {"pf_lower_bound": "lower_bound", "pf_upper_bound": "upper_bound",
                    "pf_equal_range": "equal_range"}[gname]
            for t in TYPES:
                if not has_search(t):
                    continue
                for ik, _, _ in IDX_KINDS:
                    w(f"        module procedure {base}_{t[0]}_{ik}")
        elif gname == "pf_unique_count":
            for t in TYPES:
                for ik, _, _ in IDX_KINDS:
                    w(f"        module procedure unique_count_{t[0]}_{ik}")
        elif gname == "pf_unique":
            for t in TYPES:
                if has_unique(t):
                    w(f"        module procedure unique_{t[0]}")
        elif gname == "pf_rank":
            for t in TYPES:
                for ik, _, _ in IDX_KINDS:
                    w(f"        module procedure rank_{t[0]}_{ik}")
        elif gname == "pf_minmax":
            for t in TYPES:
                if has_minmax(t):
                    w(f"        module procedure minmax_{t[0]}")
        elif gname == "pf_argminmax":
            for t in TYPES:
                if not has_argminmax(t):
                    continue
                for ik, _, _ in IDX_KINDS:
                    w(f"        module procedure argminmax_{t[0]}_{ik}")
        else:
            for t in TYPES:
                if has_merge(t):
                    w(f"        module procedure merge_{t[0]}")
        w(f"    end interface {gname}")
        w("    !")

    # ---- interface bodies ----
    w("    ! ---- Key extraction and pf_sort_keys%add (parquet_sorting_keys) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"        !> Extracts a {what} key into the canonical form the engine takes.")
        w(f"        module subroutine extract_{tag}(values, buf, descending, nulls_first, proc{', is_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        w("            type(sort_key_buf), allocatable, intent(out) :: buf(:) !! one entry, or two for a timestamp.")
        w("            logical, intent(in) :: descending !! .true. sorts high to low.")
        w("            logical, intent(in) :: nulls_first !! .true. places nulls before values.")
        w("            character(len=*), intent(in) :: proc !! calling procedure, for messages.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine extract_{tag}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"        !> Appends a {what} sort key.")
        w(f"        module subroutine add_{tag}(self, values, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
        w("            class(pf_sort_keys), intent(inout) :: self !! the key list.")
        w(val_decl(t, "in"))
        w("            logical, intent(in), optional :: descending !! .true. sorts this key high to low.")
        w("            logical, intent(in), optional :: nulls_first !! .true. places this key's nulls first.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine add_{tag}")
    w("        !> Number of keys added so far: one per `%add` call, whatever their types.")
    w("        !!")
    w("        !! Counts the keys YOU added, which is not always what the engine holds -- one")
    w("        !! `parquet_timestamp` key becomes two engine keys internally. This reports 1 for it,")
    w("        !! and `group_nkeys` counts in the same units.")
    w("        module function keys_count(self) result(n)")
    w("            class(pf_sort_keys), intent(in) :: self !! the key list.")
    w("            integer :: n                            !! keys added.")
    w("        end function keys_count")
    w("        !> Drops every key, leaving the object reusable.")
    w("        module subroutine keys_clear(self)")
    w("            class(pf_sort_keys), intent(inout) :: self !! the key list.")
    w("        end subroutine keys_clear")
    w("        !> Appends `buf` to `self`, checking every key describes the same number of rows.")
    w("        module subroutine keys_append(self, buf, proc)")
    w("            class(pf_sort_keys), intent(inout) :: self          !! the key list.")
    w("            type(sort_key_buf), allocatable, intent(inout) :: buf(:) !! keys to append; moved from.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("        end subroutine keys_append")
    w("        !> Validates a `group_nkeys` request and translates it from CALLER keys to ENGINE keys.")
    w("        !!")
    w("        !! Always sets `group_ekeys`, so a caller can pass it on unconditionally: with")
    w("        !! `group_nkeys` absent it comes back as every engine key, which is what grouping on")
    w("        !! the full key list means.")
    w("        module subroutine resolve_group_nkeys(keys, group_nkeys, want_offsets, proc, group_ekeys)")
    w("            class(pf_sort_keys), intent(in) :: keys       !! the key list.")
    w("            integer, intent(in), optional :: group_nkeys  !! caller keys per group; absent = all.")
    w("            logical, intent(in) :: want_offsets           !! whether group_offsets was asked for.")
    w("            character(len=*), intent(in) :: proc          !! calling procedure, for messages.")
    w("            integer, intent(out) :: group_ekeys           !! the engine-key prefix length.")
    w("        end subroutine resolve_group_nkeys")
    w("        !> Runs the C++ engine over `keys`, returning a 1-based permutation.")
    w("        module subroutine drive_engine(keys, nrows, proc, perm, threads)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
    w("            integer, intent(in), optional :: threads            !! thread request; absent = auto.")
    w("        end subroutine drive_engine")
    w("        !> `drive_engine`, plus the group boundaries when `group_offsets` is asked for.")
    w("        !!")
    w("        !! Absent `group_offsets` is exactly `drive_engine`, one-shot fast path and all. Present,")
    w("        !! it routes through `engine_build_runs` instead, which always uses the builder -- so")
    w("        !! asking for boundaries costs one extra copy of a single key. That is the documented")
    w("        !! price of one entry point serving three operations rather than three of them.")
    w("        module subroutine drive_engine_grouped(keys, nrows, proc, perm, threads, group_offsets, &")
    w("                group_ekeys)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
    w("            integer, intent(in), optional :: threads            !! thread request; absent = auto.")
    w("            integer(int64), allocatable, intent(out), optional :: group_offsets(:) !! group bounds.")
    w("            integer, intent(in), optional :: group_ekeys !! ENGINE keys defining a group; absent = all.")
    w("        end subroutine drive_engine_grouped")
    w("        !> Turns `engine_build_runs`' tie flags into the offsets `group_offsets` promises:")
    w("        !! length `ngroups + 1`, last entry `nrows + 1`, so group g is `perm(o(g):o(g+1)-1)`")
    w("        !! for every g with no last-iteration special case.")
    w("        module subroutine runs_to_offsets(tie, nrows, offsets)")
    w("            integer(c_int8_t), intent(in) :: tie(:) !! 1 where a row ties the previous one.")
    w("            integer(int64), intent(in) :: nrows     !! rows sorted; `tie` may be longer.")
    w("            integer(int64), allocatable, intent(out) :: offsets(:) !! the group offsets.")
    w("        end subroutine runs_to_offsets")
    w("        !> Resolves how many threads a sort should use. **This is the only place the auto rule")
    w("        !! lives**, and the only place in this module carrying OpenMP plumbing at all -- the")
    w("        !! same arrangement parquet_tables_parallel.f90 keeps for the table layer, and worth")
    w("        !! more here, since the alternative is that plumbing repeated in 65 generated bodies.")
    w("        !> How many threads an AUTOMATIC sort -- one where `threads=` is absent -- would use")
    w("        !! right now: `omp_get_max_threads()` when the caller is not inside an OpenMP parallel")
    w("        !! region, and 1 when they are, because a nested region is the caller's business.")
    w("        !!")
    w("        !! Public because the read-time `parquet_open_reader(..., sort_by=)` has to ask the")
    w("        !! same question from a different module, and one implementation of this rule is worth")
    w("        !! more than a private copy in each -- two copies drifting would mean a raw-array sort")
    w("        !! and a read-time sort silently disagreeing about when to thread. Useful in its own")
    w("        !! right for reporting or logging what an automatic sort is about to do.")
    w("        module function pf_sort_threads() result(n)")
    w("            integer :: n !! threads an automatic sort would use; 1 means serial.")
    w("        end function pf_sort_threads")
    w("        module subroutine resolve_thread_count(threads, nrows, count)")
    w("            integer, intent(in), optional :: threads !! caller's request; absent means auto.")
    w("            integer(int64), intent(in) :: nrows      !! rows to be sorted.")
    w("            integer(int64), intent(out) :: count     !! resolved count; 1 sorts serially.")
    w("        end subroutine resolve_thread_count")
    w("        !> Runs the engine over `keys` but orders only the first `count` entries -- `perm`")
    w("        !! comes back with exactly `count` elements.")
    w("        module subroutine drive_engine_partial(keys, nrows, count, proc, perm)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            integer(int64), intent(in) :: count                 !! leading entries to order.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), allocatable, intent(out) :: perm(:) !! the first `count` 1-based indices.")
    w("        end subroutine drive_engine_partial")
    w("        !> The 1-based index a full stable sort would place at rank `nth`, without sorting.")
    w("        module subroutine engine_nth_index(keys, nrows, nth, proc, idx)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            integer(int64), intent(in) :: nth                   !! 1-based rank wanted.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), intent(out) :: idx                  !! 1-based row index at that rank.")
    w("        end subroutine engine_nth_index")
    w("        !> Clamps a requested count to the array size, aborting only on a negative one.")
    w("        module subroutine resolve_count(n, nrows, proc, count)")
    w("            integer, intent(in) :: n                !! requested count, as the caller gave it.")
    w("            integer(int64), intent(in) :: nrows     !! the array size.")
    w("            character(len=*), intent(in) :: proc    !! calling procedure, for messages.")
    w("            integer(int64), intent(out) :: count    !! min(n, nrows).")
    w("        end subroutine resolve_count")
    w("        !> Aborts unless `nth` names a rank that exists.")
    w("        module subroutine check_rank(nth, nrows, proc)")
    w("            integer(int64), intent(in) :: nth      !! 1-based rank wanted.")
    w("            integer(int64), intent(in) :: nrows    !! the array size.")
    w("            character(len=*), intent(in) :: proc   !! calling procedure, for messages.")
    w("        end subroutine check_rank")
    w("        !> How many of a key's rows are non-null.")
    w("        module subroutine key_valid_count(keys, nrows, n_valid)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only the first is consulted.")
    w("            integer(int64), intent(in) :: nrows       !! the array size.")
    w("            integer(int64), intent(out) :: n_valid    !! rows that are not null.")
    w("        end subroutine key_valid_count")
    w("        !> Turns a `rounding=` token into an RND_* mode, aborting on an unrecognized one.")
    w("        module subroutine resolve_rounding(rounding, proc, mode)")
    w("            character(len=*), intent(in), optional :: rounding !! token; default \"nearest\".")
    w("            character(len=*), intent(in) :: proc               !! calling procedure, for messages.")
    w("            integer, intent(out) :: mode                       !! RND_NEAREST / RND_DOWN / RND_UP.")
    w("        end subroutine resolve_rounding")
    w("        !> The 1-based rank a quantile names within `n_valid` non-null values.")
    w("        module subroutine quantile_rank(quantile, n_valid, mode, proc, rank)")
    w("            real(real64), intent(in) :: quantile   !! position on a 0-1 scale.")
    w("            integer(int64), intent(in) :: n_valid  !! non-null population size.")
    w("            integer, intent(in) :: mode            !! RND_* rounding of a fractional position.")
    w("            character(len=*), intent(in) :: proc   !! calling procedure, for messages.")
    w("            integer(int64), intent(out) :: rank    !! 1-based rank within the non-null values.")
    w("        end subroutine quantile_rank")
    w("        !> Whether every row is already in order under `keys`, using the same comparator")
    w("        !! `drive_engine` sorts with, so the two can never disagree.")
    w("        module subroutine engine_is_sorted(keys, nrows, proc, answer)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            logical, intent(out) :: answer                      !! .true. when already in order.")
    w("        end subroutine engine_is_sorted")
    w("        !> Builds the engine's int8 validity array from a logical mask, leaving `valid`")
    w("        !! UNALLOCATED when the mask marks nothing null (the engine's no-nulls fast path).")
    w("        module subroutine valid_from_mask(mask, n, proc, valid)")
    w("            logical, intent(in) :: mask(:)                          !! .false. marks a null.")
    w("            integer(int64), intent(in) :: n                         !! expected length.")
    w("            character(len=*), intent(in) :: proc                    !! calling procedure, for messages.")
    w("            integer(c_int8_t), allocatable, intent(out) :: valid(:) !! 1 per valid element.")
    w("        end subroutine valid_from_mask")
    w("        !> Aborts unless `perm` is a true permutation of 1..n. Uses a bit-packed seen-set, so")
    w("        !! the scratch is n/8 bytes rather than the 4n a default LOGICAL array would cost.")
    w("        !!")
    w("        !! The LENGTH check always runs; `scan=.false.` skips only the O(n) range/duplicate")
    w("        !! walk. That split is what `assume_valid=` selects: a caller may promise the contents")
    w("        !! are a permutation, but a wrong-LENGTH perm would make the gather that follows read")
    w("        !! past the end of the array, and no promise can make that defined.")
    w("        module subroutine check_permutation(perm, n, proc, scan)")
    w("            integer(int64), intent(in) :: perm(:) !! the permutation to validate.")
    w("            integer(int64), intent(in) :: n       !! expected length.")
    w("            character(len=*), intent(in) :: proc  !! calling procedure, for messages.")
    w("            logical, intent(in), optional :: scan !! .false. checks the length only; default .true.")
    w("        end subroutine check_permutation")
    w("        !> Sorts, and reports where the runs of EQUAL rows are: `tie(k)` is 1 when output")
    w("        !! position k holds a row comparing equal to the one before it. One call, because")
    w("        !! `pf_unique`/`pf_rank` need both and would otherwise build the permutation twice.")
    w("        module subroutine engine_build_runs(keys, nrows, proc, perm, tie, threads, group_ekeys)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
    w("            integer(c_int8_t), allocatable, intent(out) :: tie(:) !! 1 where a row ties the previous.")
    w("            integer, intent(in), optional :: threads            !! thread request; absent = auto.")
    w("            !> How many LEADING keys decide whether two rows tie; absent means all of them.")
    w("            !! Counted in ENGINE keys, already resolved from the caller's key count -- the two")
    w("            !! differ because one `%add` of a `parquet_timestamp` contributes two engine keys.")
    w("            !! The sort itself always uses every key; only the tie test is narrowed.")
    w("            integer, intent(in), optional :: group_ekeys")
    w("        end subroutine engine_build_runs")
    w("        !> Binary-searches `keys`, whose LAST row is the target the caller appended.")
    w("        module subroutine engine_search(keys, nrows, n_search, upper, proc, pos)")
    w("            type(sort_key_buf), intent(in), target :: keys(:) !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows               !! rows each key has, target included.")
    w("            integer(int64), intent(in) :: n_search            !! rows to search, target excluded.")
    w("            logical, intent(in) :: upper                      !! .true. for upper_bound.")
    w("            character(len=*), intent(in) :: proc              !! calling procedure, for messages.")
    w("            integer(int64), intent(out) :: pos                !! 1-based insertion point.")
    w("        end subroutine engine_search")
    w("        !> Merges rows 1..`na` of `keys` with the rest, both already in order.")
    w("        module subroutine engine_merge(keys, nrows, na, proc, perm)")
    w("            type(sort_key_buf), intent(in), target :: keys(:)   !! the keys, primary first.")
    w("            integer(int64), intent(in) :: nrows                 !! rows each key describes.")
    w("            integer(int64), intent(in) :: na                    !! rows belonging to the first input.")
    w("            character(len=*), intent(in) :: proc                !! calling procedure, for messages.")
    w("            integer(int64), allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
    w("        end subroutine engine_merge")
    w("        !> Appends `src`'s rows to `dst`'s, key for key -- how a search target joins the array")
    w("        !! it is searched for in, and how `pf_merge` concatenates its two inputs. Both must")
    w("        !! describe the same number of keys, of the same families.")
    w("        module subroutine buf_append(dst, nd, src, ns, proc)")
    w("            type(sort_key_buf), allocatable, intent(inout) :: dst(:) !! grown in place.")
    w("            integer(int64), intent(in) :: nd                     !! rows currently in `dst`.")
    w("            type(sort_key_buf), allocatable, intent(in) :: src(:) !! keys to append.")
    w("            integer(int64), intent(in) :: ns                     !! rows in `src`.")
    w("            character(len=*), intent(in) :: proc                 !! calling procedure, for messages.")
    w("        end subroutine buf_append")
    w("        !> How many of a key's rows hold an actual VALUE -- neither null nor NaN, i.e. the")
    w("        !! population `pf_minmax` reduces over.")
    w("        module subroutine key_value_count(keys, nrows, n_value)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only the first is consulted.")
    w("            integer(int64), intent(in) :: nrows       !! the array size.")
    w("            integer(int64), intent(out) :: n_value    !! rows that are neither null nor NaN.")
    w("        end subroutine key_value_count")
    w("        !> Which rows of a key are null, as a plain mask. Every element is `.false.` when the")
    w("        !! key has no nulls at all (the module's unallocated-`valid` convention).")
    w("        module subroutine key_null_mask(keys, nrows, isnull)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only the first is consulted.")
    w("            integer(int64), intent(in) :: nrows       !! the array size.")
    w("            logical, allocatable, intent(out) :: isnull(:) !! .true. where the row is null.")
    w("        end subroutine key_null_mask")
    w("        !> Lower-cases a trimmed token, the one case-folding site the module has.")
    w("        module subroutine fold_token(text, tok)")
    w("            character(len=*), intent(in) :: text              !! the raw token.")
    w("            character(len=:), allocatable, intent(out) :: tok !! trimmed and lower-cased.")
    w("        end subroutine fold_token")
    w("        !> Turns a `method=` token into a RANK_* mode, aborting on an unrecognized one.")
    w("        module subroutine resolve_rank_method(method, proc, mode)")
    w("            character(len=*), intent(in), optional :: method !! token; default \"competition\".")
    w("            character(len=*), intent(in) :: proc             !! calling procedure, for messages.")
    w("            integer, intent(out) :: mode                     !! RANK_COMPETITION/_DENSE/_ORDINAL.")
    w("        end subroutine resolve_rank_method")
    w("        !> Aborts unless the extracted key is in the order the caller says it is. `what` names")
    w("        !! the argument, since `pf_merge` has two arrays to tell apart.")
    w("        module subroutine check_sorted_input(keys, nrows, proc, what)")
    w("            type(sort_key_buf), intent(in), target :: keys(:) !! the extracted key.")
    w("            integer(int64), intent(in) :: nrows               !! its row count.")
    w("            character(len=*), intent(in) :: proc              !! calling procedure, for messages.")
    w("            character(len=*), intent(in) :: what              !! the argument's name.")
    w("        end subroutine check_sorted_input")
    w("        !> Narrows one int64 answer to int32, aborting rather than truncating. `noun` names")
    w("        !! what the number is, so the message says which argument to widen.")
    w("        module subroutine narrow_i64(value, proc, noun, dst)")
    w("            integer(int64), intent(in) :: value  !! the answer.")
    w("            character(len=*), intent(in) :: proc !! calling procedure, for messages.")
    w("            character(len=*), intent(in) :: noun !! what the number is, for the message.")
    w("            integer(int32), intent(out) :: dst   !! the narrowed copy.")
    w("        end subroutine narrow_i64")
    w("        !> The array counterpart of `narrow_i64`.")
    w("        module subroutine narrow_i64_array(src, proc, noun, dst)")
    w("            integer(int64), intent(in) :: src(:) !! the answers.")
    w("            character(len=*), intent(in) :: proc !! calling procedure, for messages.")
    w("            character(len=*), intent(in) :: noun !! what the numbers are, for the message.")
    w("            integer(int32), allocatable, intent(out) :: dst(:) !! the narrowed copy.")
    w("        end subroutine narrow_i64_array")
    w("        !> Narrows a 1-based int64 permutation to int32, aborting rather than truncating.")
    w("        module subroutine narrow_perm(perm64, proc, perm32)")
    w("            integer(int64), intent(in) :: perm64(:)                !! the permutation.")
    w("            character(len=*), intent(in) :: proc                   !! calling procedure, for messages.")
    w("            integer(int32), allocatable, intent(out) :: perm32(:)  !! the narrowed copy.")
    w("        end subroutine narrow_perm")
    w("        !> Narrows a group-offsets array to int32, aborting rather than truncating.")
    w("        !!")
    w("        !! NOT the same test as `narrow_perm`'s, and the difference is exactly one row: a")
    w("        !! permutation's largest entry is `n`, but this array's is the sentinel `n + 1`. At")
    w("        !! `n == huge(int32)` the permutation narrows cleanly while the sentinel wraps negative,")
    w("        !! and a negative sentinel turns the last group's `o(g+1) - 1` into a huge negative")
    w("        !! bound -- a silently empty or wildly wrong slice instead of an abort. So this checks")
    w("        !! the sentinel itself rather than the length.")
    w("        module subroutine narrow_offsets(offsets64, proc, offsets32)")
    w("            integer(int64), intent(in) :: offsets64(:)                !! the group offsets.")
    w("            character(len=*), intent(in) :: proc                      !! calling procedure.")
    w("            integer(int32), allocatable, intent(out) :: offsets32(:)  !! the narrowed copy.")
    w("        end subroutine narrow_offsets")
    w("    end interface")
    w("    !")
    w("    ! ---- pf_argsort and pf_sort (parquet_sorting_argsort) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_argsort over a {what} array, returning an {iname} permutation.")
            w(f"        module subroutine argsort_{tag}_{ik}(values, perm, descending, nulls_first{', is_valid' if nulls == 'arg' else ''}, &")
            w("                threads, group_offsets)")
            w(val_decl(t, "in"))
            w(f"            {idecl}, allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
            w("            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.")
            w("            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.")
            if nulls == "arg":
                w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
            for line in THREADS_DOC:
                w(line)
            for line in group_offsets_doc(idecl):
                w(line)
            w(f"        end subroutine argsort_{tag}_{ik}")
    for ik, idecl, iname in IDX_KINDS:
        w(f"        !> pf_argsort over a multi-key `pf_sort_keys`, returning an {iname} permutation.")
        w(f"        module subroutine argsort_keys_{ik}(keys, perm, threads, group_offsets, group_nkeys)")
        w("            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.")
        w(f"            {idecl}, allocatable, intent(out) :: perm(:) !! the 1-based permutation.")
        for line in THREADS_DOC:
            w(line)
        for line in group_offsets_doc(idecl):
            w(line)
        for line in GROUP_NKEYS_DOC:
            w(line)
        w(f"        end subroutine argsort_keys_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        if not has_sort:
            continue
        w(f"        !> pf_sort over a {what} array: an independent sorted copy.")
        w(f"        module subroutine sort_{tag}(values, sorted, descending, nulls_first{', is_valid, sorted_valid' if nulls == 'arg' else ''}, threads)")
        w(val_decl(t, "in"))
        if family == "chr":
            w("            character(len=len(values)), allocatable, intent(out) :: sorted(:) !! the sorted copy.")
        else:
            w(f"            {decl}, allocatable, intent(out) :: sorted(:) !! the sorted copy.")
        w("            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.")
        w("            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
            w("            logical, allocatable, intent(out), optional :: sorted_valid(:)")
            w("            !! validity of `sorted`, in its order. ALWAYS ALLOCATED when asked for -- all .true.")
            w("            !! when `is_valid` was absent, since the caller asked a direct question.")
        for line in THREADS_DOC:
            w(line)
        w(f"        end subroutine sort_{tag}")
    w("    end interface")
    w("    !")
    w("    ! ---- pf_partial_sort and pf_partial_argsort (parquet_sorting_select) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_partial_argsort over a {what} array, returning an {iname} permutation.")
            w(f"        module subroutine partial_argsort_{tag}_{ik}(values, perm, n, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
            w(val_decl(t, "in"))
            w(f"            {idecl}, allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.")
            w("            integer, intent(in) :: n !! leading elements to order; clamped to the size.")
            w("            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.")
            w("            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.")
            if nulls == "arg":
                w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
            w(f"        end subroutine partial_argsort_{tag}_{ik}")
    for ik, idecl, iname in IDX_KINDS:
        w(f"        !> pf_partial_argsort over a multi-key `pf_sort_keys`, returning an {iname} permutation.")
        w("        !!")
        w("        !! Each key carries its own `descending`/`nulls_first` from `%add`. No `threads`:")
        w("        !! the partial sort is not threaded, as its per-type specifics already reflect.")
        w(f"        module subroutine partial_argsort_keys_{ik}(keys, perm, n)")
        w("            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.")
        w(f"            {idecl}, allocatable, intent(out) :: perm(:) !! the first `n` 1-based indices.")
        w("            integer, intent(in) :: n !! leading rows to order; clamped to the row count.")
        w(f"        end subroutine partial_argsort_keys_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        if not has_sort:
            continue
        w(f"        !> pf_partial_sort over a {what} array: the first `n` in order, as a copy.")
        w(f"        module subroutine partial_sort_{tag}(values, sorted, n, descending, nulls_first{', is_valid, sorted_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        if family == "chr":
            w("            character(len=len(values)), allocatable, intent(out) :: sorted(:) !! the first `n`, in order.")
        else:
            w(f"            {decl}, allocatable, intent(out) :: sorted(:) !! the first `n`, in order.")
        w("            integer, intent(in) :: n !! leading elements to order; clamped to the size.")
        w("            logical, intent(in), optional :: descending !! .true. sorts high to low; default .false.")
        w("            logical, intent(in), optional :: nulls_first !! .true. places nulls first; default .false.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
            w("            logical, allocatable, intent(out), optional :: sorted_valid(:)")
            w("            !! validity of `sorted`, in its order; always allocated when asked for.")
        w(f"        end subroutine partial_sort_{tag}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_nth(t):
            continue
        for nk, nkdecl, nkname in IDX_KINDS:
            for ik, idecl, iname in [(None, None, None)] + IDX_KINDS:
                sfx = f"_{ik}" if ik else ""
                iarg = ", index" if ik else ""
                w(f"        !> pf_nth_element over a {what} array, with an {nkname} rank" +
                  (f" and an {iname} index." if ik else " and no index out-argument."))
                w(f"        module subroutine nth_{tag}_{nk}{sfx}(values, nth, p_value{iarg}, descending, " +
                  f"nulls_first{', is_valid' if nulls == 'arg' else ''})")
                w(val_decl(t, "in"))
                w(f"            {nkdecl}, intent(in) :: nth !! 1-based rank wanted.")
                w("    " + pval_decl(t) + " !! the value at that rank.")
                if ik:
                    w(f"            {idecl}, intent(out) :: index !! which element of `values` that was.")
                w("            logical, intent(in), optional :: descending !! .true. ranks high to low.")
                w("            logical, intent(in), optional :: nulls_first !! .true. ranks nulls first.")
                if nulls == "arg":
                    w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
                w(f"        end subroutine nth_{tag}_{nk}{sfx}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_nth(t):
            continue
        for ik, idecl, iname in [(None, None, None)] + IDX_KINDS:
            sfx = f"_{ik}" if ik else ""
            iarg = ", index" if ik else ""
            w(f"        !> pf_nth_quantile over a {what} array" +
              (f", with an {iname} index." if ik else ", with no index out-argument."))
            w(f"        module subroutine quantile_{tag}{sfx}(values, quantile, p_value{iarg}, rounding" +
              f"{', is_valid' if nulls == 'arg' else ''}, n_null)")
            w(val_decl(t, "in"))
            w("            real(real64), intent(in) :: quantile !! position on a 0-1 scale.")
            w("    " + pval_decl(t) + " !! the value at that quantile.")
            if ik:
                w(f"            {idecl}, intent(out) :: index !! which element of `values` that was.")
            w("            character(len=*), intent(in), optional :: rounding !! \"nearest\"/\"down\"/\"up\".")
            if nulls == "arg":
                w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
            w("            integer(int64), intent(out), optional :: n_null !! how many values were null.")
            w("            !! LAST rather than next to `index`, because both are optional")
            w("            !! int64 out-arguments, so with `n_null` at position 4 a positional call could")
            w("            !! not be told apart from the `index` form. Nothing else here is a character,")
            w("            !! so `rounding` at position 4 disambiguates them.")
            w(f"        end subroutine quantile_{tag}{sfx}")
    w("    end interface")
    w("    !")
    w("    ! ---- pf_permute and pf_is_sorted (parquet_sorting_permute) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, _, how = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_permute over a {what} array, with an {iname} permutation.")
            w(f"        module subroutine permute_{tag}_{ik}(values, perm, assume_valid)")
            w(val_decl(t, "inout"))
            w(f"            {idecl}, intent(in) :: perm(:) !! 1-based permutation; not modified.")
            w("            logical, intent(in), optional :: assume_valid")
            w("            !! .true. promises `perm` is a permutation of 1..n, skipping the O(n) check.")
            w("            !! Its LENGTH is checked either way. A false promise silently duplicates and")
            w("            !! drops elements.")
            w(f"        end subroutine permute_{tag}_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"        !> pf_is_sorted over a {what} array.")
        w(f"        module subroutine is_sorted_{tag}(values, answer, descending, nulls_first{', is_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        w("            logical, intent(out) :: answer !! .true. when already in the stated order.")
        w("            logical, intent(in), optional :: descending !! .true. tests high-to-low order.")
        w("            logical, intent(in), optional :: nulls_first !! .true. expects nulls before values.")
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w(f"        end subroutine is_sorted_{tag}")
    w("        !> pf_is_sorted over a multi-key `pf_sort_keys`.")
    w("        !!")
    w("        !! Takes no `descending`/`nulls_first`/`is_valid`: each key carries its own, given to")
    w("        !! `%add` when it was appended. No `threads` either -- this is an O(n) scan with an")
    w("        !! early exit, which threading would cost more than it saves.")
    w("        module subroutine is_sorted_keys(keys, answer)")
    w("            class(pf_sort_keys), intent(in) :: keys !! the keys, primary first.")
    w("            logical, intent(out) :: answer !! .true. when already in the stated order.")
    w("        end subroutine is_sorted_keys")
    w("    end interface")
    w("    !")
    emit_m3_interfaces(w)
    w("end module parquet_sorting ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


DESC_DOC = "            logical, intent(in), optional :: descending !! .true. for high-to-low order."
NLO_DOC = "            logical, intent(in), optional :: nulls_first !! .true. when nulls come first."
VALID_DOC = "            logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null."
SORTED_DOC = ("            logical, intent(in), optional :: assume_sorted\n"
              "            !! .true. skips the O(n) sortedness check. Only pass it for an order you have\n"
              "            !! already established -- searching unsorted input answers with a plausible\n"
              "            !! index and no symptom at all.")


def merged_decl(t):
    """Declaration of pf_merge's result array."""
    tag, decl, what, family, nulls, _, _ = t
    if family == "chr":
        return ("            character(len=:), allocatable, intent(out) :: merged(:)\n"
                "            !! the merged copy, widened to `max(len(a), len(b))`. DEFERRED-length,\n"
                "            !! unlike `pf_sort`'s output, because the width comes from two inputs\n"
                "            !! rather than one -- so declare it `character(len=:), allocatable`.")
    return f"            {decl}, allocatable, intent(out) :: merged(:) !! the merged copy."


def distinct_decl(t):
    """Declaration of pf_unique's result."""
    tag, decl, what, family, nulls, _, _ = t
    if family == "chr":
        return "            character(len=len(values)), allocatable, intent(out) :: distinct(:) !! the distinct values."
    if family == "strcol":
        return "            type(parquet_string_column), intent(out) :: distinct !! the distinct values."
    return f"            {decl}, allocatable, intent(out) :: distinct(:) !! the distinct values, in order."


def emit_m3_interfaces(w):
    """Interface bodies for the M3 families (search, unique/rank, minmax/merge)."""
    w("    ! ---- Searching a sorted array (parquet_sorting_search) ----")
    w("    interface")
    for base, out in (("lower_bound", "lower"), ("upper_bound", "upper"), ("equal_range", "range")):
        for t in TYPES:
            if not has_search(t):
                continue
            tag, decl, what, family, nulls, _, _ = t
            for ik, idecl, iname in IDX_KINDS:
                res = "first, last" if out == "range" else "pos"
                w(f"        !> pf_{base} over a sorted {what} array, with {iname} result(s).")
                w(f"        module subroutine {base}_{tag}_{ik}(values, target, {res}, descending, " +
                  f"nulls_first{', is_valid' if nulls == 'arg' else ''}, assume_sorted)")
                w(val_decl(t, "in"))
                w(tgt_decl(t) + " !! the value to look for.")
                if out == "range":
                    w(f"            {idecl}, intent(out) :: first !! first element equal to `target`.")
                    w(f"            {idecl}, intent(out) :: last  !! last one; `first - 1` when absent.")
                else:
                    w(f"            {idecl}, intent(out) :: pos !! 1-based insertion point.")
                w(DESC_DOC)
                w(NLO_DOC)
                if nulls == "arg":
                    w(VALID_DOC)
                w(SORTED_DOC)
                w(f"        end subroutine {base}_{tag}_{ik}")
    w("    end interface")
    w("    !")
    w("    ! ---- Distinct values and ranks (parquet_sorting_unique) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_unique_count over a {what} array, with an {iname} count.")
            w(f"        module subroutine unique_count_{tag}_{ik}(values, count" +
              f"{', is_valid' if nulls == 'arg' else ''}, n_null, threads)")
            w(val_decl(t, "in"))
            w(f"            {idecl}, intent(out) :: count !! how many distinct non-null values.")
            if nulls == "arg":
                w(VALID_DOC)
            w("            integer(int64), intent(out), optional :: n_null !! how many values were null.")
            for line in THREADS_DOC:
                w(line)
            w(f"        end subroutine unique_count_{tag}_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_unique(t):
            continue
        w(f"        !> pf_unique over a {what} array: its distinct non-null values, in order.")
        w(f"        module subroutine unique_{tag}(values, distinct, descending" +
          f"{', is_valid' if nulls == 'arg' else ''}, n_null, threads)")
        w(val_decl(t, "in"))
        w(distinct_decl(t))
        w(DESC_DOC)
        if nulls == "arg":
            w(VALID_DOC)
        w("            integer(int64), intent(out), optional :: n_null !! how many values were null.")
        for line in THREADS_DOC:
            w(line)
        w(f"        end subroutine unique_{tag}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_rank over a {what} array, with {iname} ranks.")
            w(f"        module subroutine rank_{tag}_{ik}(values, ranks, method, descending" +
              f"{', is_valid' if nulls == 'arg' else ''}, threads)")
            w(val_decl(t, "in"))
            w(f"            {idecl}, allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.")
            w("            character(len=*), intent(in), optional :: method")
            w("            !! \"competition\" (the default), \"dense\" or \"ordinal\", case-insensitive.")
            w(DESC_DOC)
            if nulls == "arg":
                w(VALID_DOC)
            for line in THREADS_DOC:
                w(line)
            w(f"        end subroutine rank_{tag}_{ik}")
    w("    end interface")
    w("    !")
    w("    ! ---- Extremes and merging (parquet_sorting_reduce) ----")
    w("    interface")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_minmax(t):
            continue
        w(f"        !> pf_minmax over a {what} array: its smallest and largest value.")
        w(f"        module subroutine minmax_{tag}(values, vmin, vmax{', is_valid' if nulls == 'arg' else ''})")
        w(val_decl(t, "in"))
        w(pval_decl(t, "vmin") + " !! the smallest value.")
        w(pval_decl(t, "vmax") + " !! the largest value.")
        if nulls == "arg":
            w(VALID_DOC)
        w(f"        end subroutine minmax_{tag}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_argminmax(t):
            continue
        for ik, idecl, iname in IDX_KINDS:
            w(f"        !> pf_argminmax over a {what} array, with {iname} indices.")
            w(f"        module subroutine argminmax_{tag}_{ik}(values, imin, imax" +
              f"{', is_valid' if nulls == 'arg' else ''})")
            w(val_decl(t, "in"))
            w(f"            {idecl}, intent(out) :: imin !! where the smallest value is.")
            w(f"            {idecl}, intent(out) :: imax !! where the largest value is.")
            if nulls == "arg":
                w(VALID_DOC)
            w(f"        end subroutine argminmax_{tag}_{ik}")
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_merge(t):
            continue
        masks = ", is_valid_a, is_valid_b, merged_valid" if nulls == "arg" else ""
        w(f"        !> pf_merge over two sorted {what} arrays.")
        w(f"        module subroutine merge_{tag}(a, b, merged{masks}, descending, nulls_first, assume_sorted)")
        w(val_decl(t, "in", "a"))
        w(val_decl(t, "in", "b"))
        w(merged_decl(t))
        if nulls == "arg":
            w("            logical, intent(in), optional :: is_valid_a(:) !! `a`'s validity; absent means none.")
            w("            logical, intent(in), optional :: is_valid_b(:) !! `b`'s validity; absent means none.")
            w("            logical, allocatable, intent(out), optional :: merged_valid(:)")
            w("            !! validity of `merged`. ALWAYS ALLOCATED when asked for -- all .true. when")
            w("            !! neither input mask was supplied, since the caller asked a direct question.")
        w(DESC_DOC)
        w(NLO_DOC)
        w("            logical, intent(in), optional :: assume_sorted")
        w("            !! .true. skips the O(n) sortedness check on BOTH inputs.")
        w(f"        end subroutine merge_{tag}")
    w("    end interface")
    w("    !")
    emit_engine_interfaces(w)


def emit_engine_interfaces(w):
    """The comparator core's interfaces (feature_sort.md Stage 1).

    These four are implemented in src/parquet_sorting_engine.f90, which -- ALONE among the
    src/parquet_sorting*.f90 files -- is HAND-WRITTEN and not emitted by this script. Only the
    interfaces live here, and they have to: a `module procedure` body must be declared by the
    module it is a submodule of, and sibling submodules cannot reach each other's contained
    procedures (a plain contained procedure would compile and then fail at LINK time the moment
    parquet_sorting_keys called it, which is exactly what Stage 2 and Stage 6 need it to do).

    So: change a SIGNATURE here, change the BODY in src/parquet_sorting_engine.f90.
    """
    w("    ! ---- The comparator core (parquet_sorting_engine -- HAND-WRITTEN, not generated) ----")
    w("    interface")
    w("        !> RAW output tier of row `i` under one key: values(0), NaNs(1), nulls(2).")
    w("        !!")
    w("        !! Absolute, and **neither `descending` nor `nulls_first` reaches this**. That a")
    w("        !! descending sort still puts nulls last is Arrow's own rule. `nulls_first` is left")
    w("        !! out for a different reason: it only ever REVERSES the tier order, so")
    w("        !! `sort_compare_key` applies it once by negating the tier comparison rather than")
    w("        !! having this relabel on every call — which is what keeps the whole comparator chain")
    w("        !! inside GCC's default inlining budget. Only a real-family key can be tier 1.")
    w("        module function sort_tier_of(key, i) result(tier)")
    w("            type(sort_key_buf), intent(in) :: key !! the bound key.")
    w("            integer(int64), intent(in) :: i       !! row, 1-based.")
    w("            integer :: tier                       !! 0, 1 or 2.")
    w("        end function sort_tier_of")
    w("        !> -1/0/+1 for rows `a` and `b` under ONE key, with its order and null placement applied.")
    w("        !!")
    w("        !! Two rows in the same non-value tier (both null, or both NaN) compare EQUAL, so the")
    w("        !! caller's index tiebreaker keeps them in file order. `descending` negates the answer")
    w("        !! within the value tier only.")
    w("        module function sort_compare_key(key, a, b) result(c)")
    w("            type(sort_key_buf), intent(in) :: key !! the bound key.")
    w("            integer(int64), intent(in) :: a       !! first row, 1-based.")
    w("            integer(int64), intent(in) :: b       !! second row, 1-based.")
    w("            integer :: c                          !! -1, 0 or +1.")
    w("        end function sort_compare_key")
    w("        !> THE sort comparator: every key in precedence order, then the row index as tiebreaker.")
    w("        !!")
    w("        !! The index tiebreaker makes this a TOTAL ORDER in which no two distinct rows compare")
    w("        !! equal, which is what makes an unstable sort produce the stable answer, makes")
    w("        !! nth_element deterministic, and makes a parallel result bit-identical to a serial one")
    w("        !! by construction. Keep it beside `sort_keys_compare` -- feature_risks.md Risk-34.")
    w("        module function sort_row_less(keys, a, b) result(less)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: a           !! first row, 1-based.")
    w("            integer(int64), intent(in) :: b           !! second row, 1-based.")
    w("            logical :: less                           !! .true. when `a` sorts before `b`.")
    w("        end function sort_row_less")
    w("        !> The same ordering as `sort_row_less`, three-way and WITHOUT the index tiebreaker.")
    w("        !!")
    w("        !! Everything that must recognise \"these two rows are equal\" -- binary search, run")
    w("        !! detection for pf_unique/pf_rank, merging, is_sorted -- needs this one, since under")
    w("        !! the tiebreaker no two rows ever are equal. Sorting is the only caller that must NOT")
    w("        !! use it. `nkeys` is how many LEADING keys take part, clamped to `size(keys)`: run")
    w("        !! detection passes a prefix because \"sort by field then magnitude, but group by field")
    w("        !! alone\" is one pass.")
    w("        module function sort_keys_compare(keys, a, b, nkeys) result(c)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: a           !! first row, 1-based.")
    w("            integer(int64), intent(in) :: b           !! second row, 1-based.")
    w("            integer, intent(in) :: nkeys              !! leading keys taking part.")
    w("            integer :: c                              !! -1, 0 or +1.")
    w("        end function sort_keys_compare")
    w("        !> Fills `perm` with the 1-based permutation that puts rows `1..n` in key order.")
    w("        !!")
    w("        !! The serial half of the pure-Fortran engine (feature_sort.md Stage 2): an INTROSORT")
    w("        !! -- quicksort with median-of-three pivoting, a depth-limited heapsort fallback and a")
    w("        !! final insertion pass -- ordering by `sort_row_less` and nothing else.")
    w("        !!")
    w("        !! **It is unstable, and that is why it is correct.** `sort_row_less` ends with a row")
    w("        !! index tiebreaker, so no two distinct rows compare equal and every correct sorting")
    w("        !! algorithm produces the SAME permutation -- the stable one. Switching this to a merge")
    w("        !! sort to \"make it stable\" would buy a temporary buffer and change no answer.")
    w("        module subroutine sort_comparison_permutation(keys, n, perm)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! rows to order.")
    w("            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.")
    w("        end subroutine sort_comparison_permutation")
    w("        !> THE engine entry point: the counting fast path where it applies, the introsort otherwise.")
    w("        !!")
    w("        !! Mirrors the C++ `sort_build_permutation` exactly, including that the range scan's")
    w("        !! `lo`/`hi` are carried from the candidate test into the placement pass rather than")
    w("        !! rescanned. The two paths answer identically — the counting one is stable by")
    w("        !! construction, which is the same answer the comparator's index tiebreaker gives.")
    w("        module subroutine sort_build_permutation(keys, n, perm)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! rows to order.")
    w("            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.")
    w("        end subroutine sort_build_permutation")
    w("        !> THE engine entry point when a thread count is available -- Stage 4.")
    w("        !!")
    w("        !! Answers **bit-identically to `sort_build_permutation` at every thread count**, and")
    w("        !! that is a property of the ordering rather than of the implementation: `sort_row_less`")
    w("        !! ends in a row-index tiebreaker, so no two distinct rows compare equal, exactly one")
    w("        !! permutation is correct, and every correct algorithm must produce it. A threading bug")
    w("        !! therefore shows up as a WRONG permutation, never as a differently-ordered valid one.")
    w("        !!")
    w("        !! `nthreads` is a resolved count, never a sentinel -- `resolve_thread_count` has already")
    w("        !! applied the caller's `threads=`, the automatic policy and the in-parallel rule. This")
    w("        !! procedure applies only the two clauses that need the DATA to decide: the row floor")
    w("        !! (`parquet_get_sort_parallel_min_rows`), below which a team costs more than it saves,")
    w("        !! and one thread meaning the plain serial path. Both are observable through")
    w("        !! `parquet_debug_sort_threads_used`, which is the only way a test can see either.")
    w("        module subroutine sort_build_permutation_threaded(keys, n, nthreads, perm)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! rows to order.")
    w("            integer(int64), intent(in) :: nthreads    !! resolved thread count; 1 sorts serially.")
    w("            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.")
    w("        end subroutine sort_build_permutation_threaded")
    w("        !> Whether the single-key integer counting sort applies, and over what value range.")
    w("        !!")
    w("        !! `lo`/`hi` are the key's range over its VALID rows only — a null row's value slot")
    w("        !! holds whatever the buffer contained, so including it could widen the range past the")
    w("        !! bucket limit and decline the fast path for no reason. An all-null key answers")
    w("        !! `.true.` with `lo == hi == 0`, which yields the identity permutation.")
    w("        module function sort_counting_candidate(keys, n, lo, hi) result(ok)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys; only a lone integer key qualifies.")
    w("            integer(int64), intent(in) :: n           !! rows.")
    w("            integer(int64), intent(out) :: lo         !! smallest valid key value, or 0.")
    w("            integer(int64), intent(out) :: hi         !! largest valid key value, or 0.")
    w("            logical :: ok                             !! .true. when the counting path applies.")
    w("        end function sort_counting_candidate")
    w("        !> Fills `perm` by counting sort over `lo..hi`, with the nulls placed as one block.")
    w("        !!")
    w("        !! Two O(n) passes and no comparisons at all. Stable by construction: the placement")
    w("        !! pass walks the input in index order, so equal values are emitted in file order —")
    w("        !! the same answer `sort_row_less`'s index tiebreaker produces.")
    w("        module subroutine sort_counting_permutation(key, n, lo, hi, perm)")
    w("            type(sort_key_buf), intent(in) :: key    !! the lone integer key.")
    w("            integer(int64), intent(in) :: n          !! rows.")
    w("            integer(int64), intent(in) :: lo         !! smallest valid key value.")
    w("            integer(int64), intent(in) :: hi         !! largest valid key value.")
    w("            integer(int64), intent(inout) :: perm(:) !! receives `n` 1-based row indices.")
    w("        end subroutine sort_counting_permutation")
    w("        !> The first `count` entries of the sorted permutation, by heap selection.")
    w("        !!")
    w("        !! `std::partial_sort`'s algorithm, not a full sort truncated -- a test counts")
    w("        !! comparisons to hold that apart. Everything past `count` in `perm` is untouched.")
    w("        module subroutine sort_partial_permutation(keys, n, count, perm)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! rows available.")
    w("            integer(int64), intent(in) :: count       !! leading entries to order.")
    w("            integer(int64), intent(inout) :: perm(:)  !! receives `count` 1-based row indices.")
    w("        end subroutine sort_partial_permutation")
    w("        !> The row a full sort would place at 1-based rank `nth`, by quickselect.")
    w("        !!")
    w("        !! Deterministic because the comparator is a total order: there is exactly one row at")
    w("        !! that rank, so this and a full sort cannot disagree. `idx` is 0 for an out-of-range")
    w("        !! rank, which every caller has already rejected.")
    w("        module subroutine sort_nth_index(keys, n, nth, idx)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! rows.")
    w("            integer(int64), intent(in) :: nth         !! 1-based rank wanted.")
    w("            integer(int64), intent(out) :: idx        !! 1-based row index at that rank.")
    w("        end subroutine sort_nth_index")
    w("        !> Are rows `1..n` already in order under every key?")
    w("        !!")
    w("        !! Over `sort_keys_compare`, so adjacent EQUAL rows are in order — the tiebreaker would")
    w("        !! turn this into \"is the row index ascending\".")
    w("        module function sort_is_sorted(keys, n) result(answer)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! rows.")
    w("            logical :: answer                         !! .true. when already ordered.")
    w("        end function sort_is_sorted")
    w("        !> Sorts, then flags where the runs of EQUAL rows begin. `tie(1)` is always 0.")
    w("        !!")
    w("        !! `group_keys` is how many LEADING keys decide a tie; the sort itself always uses every")
    w("        !! key. That asymmetry is what produces \"grouped by field, ordered within group\".")
    w("        module subroutine sort_build_runs_permutation(keys, n, group_keys, perm, tie)")
    w("            type(sort_key_buf), intent(in) :: keys(:)  !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n            !! rows.")
    w("            integer(int64), intent(in) :: group_keys   !! leading keys that decide a tie.")
    w("            integer(int64), intent(inout) :: perm(:)   !! receives `n` 1-based row indices.")
    w("            integer(c_int8_t), intent(inout) :: tie(:) !! 1 where a row ties with its predecessor.")
    w("        end subroutine sort_build_runs_permutation")
    w("        !> Binary search for the target row, which the caller APPENDED as row `n_search + 1`.")
    w("        !!")
    w("        !! **Preserve the appending.** It is what removes any compare-a-row-against-a-value arm")
    w("        !! and so makes drift from the sort comparator structurally impossible — Risk-34.")
    w("        module function sort_search_position(keys, n_search, upper) result(pos)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys; row n_search+1 is the target.")
    w("            integer(int64), intent(in) :: n_search    !! rows being searched.")
    w("            logical, intent(in) :: upper              !! .true. for upper_bound.")
    w("            integer(int64) :: pos                     !! 1-based insertion point in 1..n_search+1.")
    w("        end function sort_search_position")
    w("        !> Merges the already-ordered ranges `1..na` and `na+1..n` into one permutation.")
    w("        !!")
    w("        !! Ties take from the FIRST range, which is `std::merge`'s stability guarantee and what")
    w("        !! makes `pf_merge` agree with `pf_sort` of the concatenation element for element.")
    w("        module subroutine sort_merge_permutation(keys, n, na, perm)")
    w("            type(sort_key_buf), intent(in) :: keys(:) !! the keys, in precedence order.")
    w("            integer(int64), intent(in) :: n           !! total rows across both ranges.")
    w("            integer(int64), intent(in) :: na          !! rows in the first range.")
    w("            integer(int64), intent(inout) :: perm(:)  !! receives `n` 1-based row indices.")
    w("        end subroutine sort_merge_permutation")
    w("    end interface")
    w("    !")
    w("    ! ---- Test-only access to the comparator core (parquet_sorting_engine) ----")
    w("    interface")
    w("        !> Test-only view of what the Fortran SORT comparator says about one pair of rows.")
    w("        !!")
    w("        !! Public only because it has to be: `sort_key_buf` is private to this module, so a")
    w("        !! test cannot reach `sort_row_less` any other way, and the C++-side hook convention")
    w("        !! is unavailable for a decision that Stage 1 exists to move out of C++. Not called by")
    w("        !! library code. Rows are 1-based, as everywhere else in this module's public API.")
    w("        module function parquet_debug_sort_row_less(keys, a, b) result(less)")
    w("            type(pf_sort_keys), intent(in) :: keys !! the built key set.")
    w("            integer(int64), intent(in) :: a        !! first row, 1-based.")
    w("            integer(int64), intent(in) :: b        !! second row, 1-based.")
    w("            logical :: less                        !! .true. when `a` sorts before `b`.")
    w("        end function parquet_debug_sort_row_less")
    w("        !> Test-only view of what the Fortran TIE-FREE comparator says about one pair of rows.")
    w("        !!")
    w("        !! Same reasoning as `parquet_debug_sort_row_less`. `nkeys` counts ENGINE keys and is")
    w("        !! clamped to how many the set holds; note a `parquet_timestamp` key binds as two.")
    w("        module function parquet_debug_sort_keys_compare(keys, a, b, nkeys) result(c)")
    w("            type(pf_sort_keys), intent(in) :: keys !! the built key set.")
    w("            integer(int64), intent(in) :: a        !! first row, 1-based.")
    w("            integer(int64), intent(in) :: b        !! second row, 1-based.")
    w("            integer, intent(in) :: nkeys           !! leading engine keys taking part.")
    w("            integer :: c                           !! -1, 0 or +1.")
    w("        end function parquet_debug_sort_keys_compare")
    w("        !> Test-only sweep of `nreps` passes of `nrows` comparisons, returning a checksum.")
    w("        !!")
    w("        !! For app/benchmark_sort_comparator.f90, which needs the comparator's own cost rather")
    w("        !! than the cost of reaching it: at ~5 ns per comparison a per-call harness measures")
    w("        !! its own overhead. The C++ twin is `parquet_debug_sort_sweep_less_cpp` in")
    w("        !! src/parquet_wrapper.cpp and the two loops are deliberately identical, down to the")
    w("        !! stride walk — their checksums must agree, which is what proves they did the same")
    w("        !! work. Neither uses `mod` on a runtime divisor: that is an integer division, and it")
    w("        !! would cost more than the comparison being timed.")
    w("        module function parquet_debug_sort_sweep_less(keys, nrows, nreps) result(count)")
    w("            type(pf_sort_keys), intent(in) :: keys  !! the built key set.")
    w("            integer(int64), intent(in) :: nrows     !! rows to walk per pass.")
    w("            integer(int64), intent(in) :: nreps     !! passes.")
    w("            integer(int64) :: count                 !! how many pairs compared less; -1 if unusable.")
    w("        end function parquet_debug_sort_sweep_less")
    w("        !> Test-only twin of that sweep for the tie-free comparator, summing its answers.")
    w("        module function parquet_debug_sort_sweep_compare(keys, nrows, nreps, nkeys) result(total)")
    w("            type(pf_sort_keys), intent(in) :: keys  !! the built key set.")
    w("            integer(int64), intent(in) :: nrows     !! rows to walk per pass.")
    w("            integer(int64), intent(in) :: nreps     !! passes.")
    w("            integer, intent(in) :: nkeys            !! leading engine keys taking part.")
    w("            integer(int64) :: total                 !! sum of the answers; -1 if unusable.")
    w("        end function parquet_debug_sort_sweep_compare")
    w("        !> Test-only switch routing `pf_argsort` and friends to the Fortran engine, or back to C++.")
    w("        !!")
    w("        !! Stage 2 scaffolding, deleted at the Stage 6 cutover. Both engines answer identically")
    w("        !! -- that is what the conformance tests assert -- so this changes timing and nothing")
    w("        !! else, which is exactly why it is a debug hook rather than a setting.")
    w("        module subroutine parquet_debug_use_fortran_sort_engine(on)")
    w("            logical, intent(in) :: on !! .true. selects the Fortran engine.")
    w("        end subroutine parquet_debug_use_fortran_sort_engine")
    w("        !> Test-only reader for which engine `drive_engine` would use right now.")
    w("        module function parquet_debug_using_fortran_sort_engine() result(on)")
    w("            logical :: on !! .true. when the Fortran engine is selected.")
    w("        end function parquet_debug_using_fortran_sort_engine")
    w("        !> Test-only override for the introsort's depth limit; NEGATIVE restores the computed one.")
    w("        !!")
    w("        !! Zero makes the very first oversized range fall back to heapsort, which is the only")
    w("        !! way to reach that arm from a test-sized fixture. Has no effect on the C++ engine.")
    w("        !! Also ZEROES the heapsort counter, so a test arms and reads in the obvious order.")
    w("        module subroutine parquet_debug_set_sort_depth_limit(n)")
    w("            integer, intent(in) :: n !! forced depth limit, or a negative value to restore.")
    w("        end subroutine parquet_debug_set_sort_depth_limit")
    w("        !> Test-only count of heapsort fallbacks since `parquet_debug_set_sort_depth_limit`.")
    w("        !!")
    w("        !! What makes the forced-fallback test non-vacuous: the quicksort and heapsort paths")
    w("        !! answer identically, so only this counter can say which one ran.")
    w("        module function parquet_debug_sort_heapsort_calls() result(n)")
    w("            integer(int64) :: n !! heapsort fallbacks entered.")
    w("        end function parquet_debug_sort_heapsort_calls")
    w("        !> Test-only arming of the final insertion pass's largest-shift tracker, zeroing it too.")
    w("        !!")
    w("        !! Off by default, because armed it writes process-global state from an ordinary sort.")
    w("        module subroutine parquet_debug_set_sort_track_shift(on)")
    w("            logical, intent(in) :: on !! .true. arms the tracker.")
    w("        end subroutine parquet_debug_set_sort_track_shift")
    w("        !> Test-only reader for how far the insertion pass moved anything since it was armed.")
    w("        !!")
    w("        !! Must not exceed `SORT_INSERTION_CUTOFF` after a correct sort — that is the whole")
    w("        !! invariant the quicksort exists to establish, and the only observable that a defect")
    w("        !! in the partition or the heapsort has not simply been repaired by the insertion pass.")
    w("        module function parquet_debug_sort_max_insertion_shift() result(n)")
    w("            integer(int64) :: n !! largest shift, in positions.")
    w("        end function parquet_debug_sort_max_insertion_shift")
    w("        !> Test-only override for the radix path's row floor; NEGATIVE restores the built-in.")
    w("        !!")
    w("        !! Used in both directions. A huge value DECLINES the radix path, which is what keeps")
    w("        !! the introsort's and the counting path's own negative controls non-vacuous now that")
    w("        !! the floor sits below their fixture sizes. A small one drives ordinary fixtures")
    w("        !! through the radix path. Has no effect on the C++ engine.")
    w("        module subroutine parquet_debug_set_sort_radix_min_rows(n)")
    w("            integer(int64), intent(in) :: n !! forced floor, or a negative value to restore.")
    w("        end subroutine parquet_debug_set_sort_radix_min_rows")
    w("        !> Test-only forcing of an allocation failure in the radix path, to reach its fallbacks.")
    w("        !!")
    w("        !! Selects WHICH allocation fails, because the two are in series and a single flag")
    w("        !! would make the first mask the second: 0 none, 1 the main scratch, 2 the deep")
    w("        !! string refine's. The fallback answers identically -- it is the comparison sort --")
    w("        !! so no assertion on a permutation can tell it apart from the radix path. Pair this")
    w("        !! with the insertion-shift tracker, which can.")
    w("        module subroutine parquet_debug_set_sort_radix_fail_alloc(which)")
    w("            integer, intent(in) :: which !! 0 none, 1 the main scratch, 2 the refine's.")
    w("        end subroutine parquet_debug_set_sort_radix_fail_alloc")
    w("        !> Test-only zeroing of the executed-radix-pass counter, before the sort under test.")
    w("        module subroutine parquet_debug_reset_sort_radix_passes()")
    w("        end subroutine parquet_debug_reset_sort_radix_passes")
    w("        !> Test-only count of radix scatter passes executed since that reset.")
    w("        !!")
    w("        !! What makes a test of any pass-count optimisation non-vacuous: the constant-digit")
    w("        !! skip and the narrow-integer bias both leave the permutation bit-identical, so this")
    w("        !! is the only thing that can say whether either fired. Zero means the radix path did")
    w("        !! not run at all, which is itself worth asserting -- a floor or a decline is easy to")
    w("        !! trip by accident and looks exactly like an optimisation working perfectly.")
    w("        module function parquet_debug_sort_radix_passes() result(n)")
    w("            integer(int64) :: n !! passes executed.")
    w("        end function parquet_debug_sort_radix_passes")
    w("        !> Test-only count of threads the engine's last permutation build opened; 1 = serial.")
    w("        !!")
    w("        !! What makes any Stage 4 threading test non-vacuous. The permutation is bit-identical")
    w("        !! at every thread count -- the comparator is a total order, so there is exactly one")
    w("        !! correct answer -- which means no assertion on `perm` can distinguish a threaded run")
    w("        !! from a serial one. A policy that silently refuses to thread is therefore invisible")
    w("        !! to every other test in the suite, and is the easiest Stage 4 bug to write.")
    w("        !!")
    w("        !! Reports the RESOLVED count, not the team the runtime actually granted. Has no")
    w("        !! effect on, and says nothing about, the C++ engine.")
    w("        module function parquet_debug_sort_threads_used() result(n)")
    w("            integer(int64) :: n !! threads resolved for the last build; 1 means serial.")
    w("        end function parquet_debug_sort_threads_used")
    w("        !> Test-only count of buckets Design B's split produced; 0 means it did not run.")
    w("        !!")
    w("        !! Design B and the serial LSD loop answer identically by construction, so this is the")
    w("        !! only way to tell which one ran -- and therefore the only way any test of the split,")
    w("        !! the bucket cap or the balance test can be non-vacuous. Zero is informative rather")
    w("        !! than missing: it is exactly what a declined split looks like, which is the normal")
    w("        !! outcome on a low-cardinality key.")
    w("        module function parquet_debug_sort_split_buckets() result(n)")
    w("            integer(int64) :: n !! buckets in the last split; 0 if Design B declined.")
    w("        end function parquet_debug_sort_split_buckets")
    w("    end interface")
    w("    !")


# --------------------------------------------------------------------------------------
# src/parquet_sorting_keys.f90 -- key extraction, pf_sort_keys%add, and the engine drivers
# --------------------------------------------------------------------------------------

#: How each single-buffer type reduces one element to its key value. Emitted inside
#: `do k = 1_int64, n`. Keyed by TAG, not family: `logical`, `parquet_date` and `parquet_time`
#: all bind as integer keys but reach their integer three different ways.
EXTRACT_LOOP = {
    "i32":  "            buf(1)%ints(k) = int(values(k), int64)",
    "i64":  "            buf(1)%ints(k) = values(k)",
    "f32":  "            buf(1)%reals(k) = real(values(k), real64)",
    "f64":  "            buf(1)%reals(k) = values(k)",
    "bool": "            buf(1)%ints(k) = merge(1_int64, 0_int64, values(k))",
    "date": "            buf(1)%ints(k) = int(values(k)%raw(), int64)",
    "time": "            buf(1)%ints(k) = values(k)%raw()",
}


def emit_extract(w, t):
    """The body of one `extract_<tag>` module procedure."""
    tag, decl, what, family, nulls, _, _ = t
    w(f"    module procedure extract_{tag}")
    # -- declarations
    if family in ("int", "int64", "real", "real64"):
        w("        integer(int64) :: k, n")
    elif family == "chr":
        w("        integer(int64) :: k, n, total, pos, j, ln")
    elif family == "ts":
        w("        integer(int64) :: k, n, s")
        w("        integer(int32) :: ns")
        w("        logical, allocatable :: mask(:)")
    elif family == "strcol":
        # Only the row count: the packing itself lives in pack_string_store, which owns its locals.
        w("        integer(int64) :: n")
    elif family == "col":
        w("        integer(int64) :: n")
        w("        integer :: kind")
    if family == "bool_unused":
        pass
    if nulls == "elem" and family != "ts":
        w("        logical, allocatable :: mask(:)")
    w("        !")
    # -- row count
    if family in ("strcol",):
        w("        n = values%size()")
    elif family == "col":
        w("        n = values%length()")
    else:
        w("        n = size(values, kind=int64)")

    if family in ("int", "int64", "real", "real64"):
        store = "ints" if family in ("int", "int64") else "reals"
        skind = "SK_INT" if store == "ints" else "SK_REAL"
        zero = "0_int64" if store == "ints" else "0.0_real64"
        w("        allocate(buf(1))")
        w(f"        buf(1)%family = {skind}")
        w("        buf(1)%descending = descending")
        w("        buf(1)%nulls_first = nulls_first")
        w(f"        allocate(buf(1)%{store}(max(n, 1_int64)))")
        w(f"        buf(1)%{store} = {zero}")
        w("        do k = 1_int64, n")
        w(EXTRACT_LOOP[tag])
        w("        end do")
        if nulls == "elem":
            w("        allocate(mask(max(n, 1_int64)))")
            w("        mask = .true.")
            w("        do k = 1_int64, n")
            w("            mask(k) = .not. values(k)%is_null()")
            w("        end do")
            w("        call valid_from_mask(mask, n, proc, buf(1)%valid)")
    elif family == "chr":
        w("        allocate(buf(1))")
        w("        buf(1)%family = SK_STR")
        w("        buf(1)%descending = descending")
        w("        buf(1)%nulls_first = nulls_first")
        w("        ! Sorted on the FULL declared length, trailing blanks included, which is exactly")
        w("        ! Fortran's own `<` for equal-length strings -- so pf_is_sorted agrees with a")
        w("        ! hand-written a(k) <= a(k+1) loop rather than quietly trimming behind it.")
        w("        ln = int(len(values), int64)")
        w("        allocate(buf(1)%offsets(n + 1_int64))")
        w("        total = 0_int64")
        w("        do k = 1_int64, n + 1_int64")
        w("            buf(1)%offsets(k) = total")
        w("            total = total + ln")
        w("        end do")
        w("        buf(1)%offsets(n + 1_int64) = n * ln")
        w("        allocate(buf(1)%data(max(n * ln, 1_int64)))")
        w("        pos = 0_int64")
        w("        do k = 1_int64, n")
        w("            do j = 1_int64, ln")
        w("                buf(1)%data(pos + j) = values(k)(j:j)")
        w("            end do")
        w("            pos = pos + ln")
        w("        end do")
    elif family == "ts":
        w("        ! A timestamp is TWO integer keys: folding (seconds, nanoseconds) into one int64")
        w("        ! as s*10**9 + ns overflows outside roughly 1678-2262, well inside the range this")
        w("        ! library handles. Seconds lead, nanoseconds break their ties, and both carry the")
        w("        ! same validity so the pair can never disagree about which rows are null.")
        w("        allocate(buf(2))")
        w("        buf(:)%family = SK_INT")
        w("        buf(:)%descending = descending")
        w("        buf(:)%nulls_first = nulls_first")
        w("        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))")
        w("        buf(1)%ints = 0_int64")
        w("        buf(2)%ints = 0_int64")
        w("        allocate(mask(max(n, 1_int64)))")
        w("        mask = .true.")
        w("        do k = 1_int64, n")
        w("            call values(k)%get_raw(s, ns)")
        w("            buf(1)%ints(k) = s")
        w("            buf(2)%ints(k) = int(ns, int64)")
        w("            mask(k) = .not. values(k)%is_null()")
        w("        end do")
        w("        call valid_from_mask(mask, n, proc, buf(1)%valid)")
        w("        if (allocated(buf(1)%valid)) buf(2)%valid = buf(1)%valid")
    elif family == "strcol":
        # Delegates to the same worker the parquet_column entry point uses -- see
        # pack_string_store for why this is two memcpys rather than a per-element loop.
        w("        call pack_string_store(values, n, descending, nulls_first, buf)")
        w("        call string_store_valid_flags(values, n, buf)")
    elif family == "col":
        w("        ! One column, whose element type is only known at runtime -- so this reduces to")
        w("        ! whichever of the four extractors above matches, and a PK_TIMESTAMP column")
        w("        ! produces two keys exactly as the bare type does.")
        w("        kind = values%kindof()")
        w("        if (values%colwidth() /= 1) then")
        w("            error stop EP // proc // \": a vector column cannot be a sort key; there is no \" // &")
        w("                \"defined order on a whole vector row\"")
        w("        end if")
        w("        select case (kind)")
        w("        case (PK_INT32, PK_INT64, PK_LOGICAL, PK_DATE, PK_TIME)")
        w("            call extract_col_integer(values, n, descending, nulls_first, buf)")
        w("        case (PK_FLOAT32, PK_FLOAT64)")
        w("            call extract_col_real(values, n, descending, nulls_first, buf)")
        w("        case (PK_STRING)")
        w("            call extract_col_string(values, n, descending, nulls_first, buf)")
        w("        case (PK_TIMESTAMP)")
        w("            call extract_col_timestamp(values, n, descending, nulls_first, buf)")
        w("        case default")
        w("            block")
        w("                character(len=:), allocatable :: kname")
        w("                call parquet_kind_name(kind, kname)")
        w("                error stop EP // proc // \": a \" // kname // \" column cannot be a sort key\"")
        w("            end block")
        w("        end select")
        w("        call col_valid_flags(values, n, buf)")

    if nulls == "arg":
        w("        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)")
    w(f"    end procedure extract_{tag}")
    w("    !")


def gen_keys():
    o = []
    w = o.append
    w(BANNER)
    w('''!> Turns each supported element type into the canonical key form the C++ engine takes, and
!! implements `pf_sort_keys`.
!!
!! **This file decides nothing about order.** It extracts values, says which rows are null, and
!! passes the caller's `descending`/`nulls_first` flags through -- every ordering decision is made
!! in one place (`sort_compare_key`, `src/parquet_wrapper.cpp`), which is what stops a raw-array
!! sort, a table sort and a read-time `sort_by=` from ever disagreeing.
!!
!! The canonical form is deliberately narrow: an integer key, a real key, or a packed
!! (offsets, data) string key, each with an optional per-row validity array. Everything else
!! reduces to one of those three -- a `logical` and every temporal kind order exactly as their
!! stored integers do, and a `parquet_timestamp` becomes two integer keys rather than one.
submodule (parquet_sorting) parquet_sorting_keys
    implicit none
    !
contains
    !''')
    for t in TYPES:
        emit_extract(w, t)

    # ---- parquet_column sub-extractors (plain contained procedures) ----
    w('''    !> Reads every integer-valued scalar kind of a column as int64 -- including logical and the
    !! two date/time kinds, whose stored values order exactly as the values they represent.
    !!
    !! **The kind switch is above the loop, and each arm is one whole-array assignment through
    !! `%data_ptr`** rather than a per-row `%get_at`. The obvious shape -- switch inside the loop --
    !! costs a `%kindof()` call, a `select case`, and an un-inlinable `%get_at` (which itself calls
    !! `check_kind` and `check_index`) on every row, for a decision that cannot change between rows.
    !! `col` needs the `target` attribute for `%data_ptr`'s own `target` dummy to yield a pointer
    !! that stays associated after it returns; the pointer is used only within this procedure, which
    !! is what the standard guarantees when the ultimate actual argument is not itself a target.
    subroutine extract_col_integer(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in), target :: col            !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int32), pointer :: p32(:)
        integer(int64), pointer :: p64(:)
        logical, pointer :: pb(:)
        type(parquet_date), pointer :: pd(:)
        type(parquet_time), pointer :: pt(:)
        !
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! No pre-zero pass: every one of the n elements is written below, and the single padding
        ! element that `max(n, 1)` adds when n == 0 is the only one that needs initialising.
        if (n <= 0_int64) then
            buf(1)%ints = 0_int64
            return
        end if
        select case (col%kindof())
        case (PK_INT32)
            call col%data_ptr(p32)
            buf(1)%ints(1:n) = int(p32(1:n), int64)
        case (PK_INT64)
            call col%data_ptr(p64)
            buf(1)%ints(1:n) = p64(1:n)
        case (PK_LOGICAL)
            call col%data_ptr(pb)
            buf(1)%ints(1:n) = merge(1_int64, 0_int64, pb(1:n))
        case (PK_DATE)
            call col%data_ptr(pd)
            buf(1)%ints(1:n) = int(pd(1:n)%raw(), int64)   ! %raw is elemental
        case default
            call col%data_ptr(pt)
            buf(1)%ints(1:n) = pt(1:n)%raw()               ! %raw is elemental
        end select
    end subroutine extract_col_integer
    !
    !> Reads a float32 or float64 column as real64. NaNs pass straight through: the engine tiers
    !! them itself, exactly as it does for a read-time sort.
    !!
    !! Kind switch above the loop and one whole-array assignment per arm -- see
    !! `extract_col_integer` for why, and for why `col` carries `target`.
    subroutine extract_col_real(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in), target :: col            !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        real(real32), pointer :: p32(:)
        real(real64), pointer :: p64(:)
        !
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        if (n <= 0_int64) then
            buf(1)%reals = 0.0_real64
            return
        end if
        if (col%kindof() == PK_FLOAT32) then
            call col%data_ptr(p32)
            buf(1)%reals(1:n) = real(p32(1:n), real64)
        else
            call col%data_ptr(p64)
            buf(1)%reals(1:n) = p64(1:n)
        end if
    end subroutine extract_col_real
    !
    !> Packs a string column into the (offsets, data) pair the engine takes: row k occupies
    !! `data(offsets(k)+1 : offsets(k+1))`, with `offsets` 0-based because the C++ side indexes
    !! with it directly.
    !!
    !! Only ever reached for `PK_STRING` (the dispatch above checks the kind), and that kind stores a
    !! `parquet_string_column` -- so this hands the whole job to `pack_string_store`, which is the
    !! same worker the `parquet_string_column` entry point uses. Two entry points, one body: a sort
    !! key must not depend on which of the two types the caller happened to hold.
    subroutine extract_col_string(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        type(parquet_string_column), pointer :: store
        !
        call col%string_column(store)
        call pack_string_store(store, n, descending, nulls_first, buf)
    end subroutine extract_col_string
    !
    !> Shared body behind both string-key entry points: packs `store` into `buf(1)`.
    !!
    !! **Two bulk copies, not a loop.** A `parquet_string_column` already holds exactly the layout a
    !! sort key wants -- int64 offsets with `offsets(1) = 0`, and one packed payload -- so
    !! `%copy_buffers` moves both in two `memcpy`s. The obvious implementation instead walks the
    !! column calling `%get` per element, which allocates a deferred-length string, fills it, copies
    !! it out and frees it, **once per row**; that measured at roughly 0.11 s per allocation per 4 M
    !! elements, i.e. 19 % of a `parquet_string_column` sort and 33 % of a `parquet_column` one,
    !! because the latter paid it twice. Do not reintroduce a per-element `%get` here; see
    !! `feature_risks.md` Risk-60.
    !!
    !! **A null is zero-width in both layouts** (`set_null` compacts the payload), so the copy needs
    !! no null special-casing -- a null row simply occupies an empty range, which is what the old
    !! `%get(..., allow_null=.true.)` produced for it. Validity is carried separately, below.
    subroutine pack_string_store(store, n, descending, nulls_first, buf)
        type(parquet_string_column), intent(in) :: store           !! the packed string storage.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int64) :: total
        !
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        total = store%character_size()
        allocate(buf(1)%offsets(n + 1_int64))
        allocate(buf(1)%data(max(total, 1_int64)))
        call store%copy_buffers(buf(1)%offsets, buf(1)%data)
    end subroutine pack_string_store
    !
    !> Fills `buf(1)%valid` from a string store's nulls, if it has any.
    !!
    !! **Separate from `pack_string_store` on purpose.** The `parquet_column` entry point already
    !! runs `col_valid_flags` after its extractor, so a packer that also filled `valid` would
    !! allocate it twice and abort -- which is exactly what happened when the two string entry points
    !! were first merged. Only the `parquet_string_column` entry point, which has no such follow-up,
    !! calls this.
    !!
    !! A bit walk rather than a copy, because the engine takes one int8 per row where the column
    !! packs eight rows per byte. `%null_count()` answers in O(1), so a null-free column skips the
    !! array entirely -- and an unallocated `valid` is what tells the engine there are none.
    subroutine string_store_valid_flags(store, n, buf)
        type(parquet_string_column), intent(in) :: store !! the packed string storage.
        integer(int64), intent(in) :: n                  !! row count.
        type(sort_key_buf), intent(inout) :: buf(:)      !! the key to attach validity to.
        integer(int64) :: k
        !
        if (store%null_count() <= 0_int64) return
        allocate(buf(1)%valid(max(n, 1_int64)))
        buf(1)%valid = 1_c_int8_t
        do k = 1_int64, n
            if (store%is_null(k)) buf(1)%valid(k) = 0_c_int8_t
        end do
    end subroutine string_store_valid_flags
    !
    !> Splits a timestamp column into its (seconds, nanoseconds) pair of integer keys -- see
    !! `extract_ts` for why a timestamp becomes two keys rather than one.
    !!
    !! Reaches the elements through `%data_ptr` rather than a per-row `%get_at` -- see
    !! `extract_col_integer` for why, and for why `col` carries `target`. This one keeps a row loop
    !! (rather than one whole-array assignment per key) because `%get_raw` yields the two halves
    !! together: splitting it into two elemental array calls would either walk the column twice or
    !! need an int32 temporary the whole length of the column, to save an elemental call the
    !! compiler can already inline.
    subroutine extract_col_timestamp(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in), target :: col            !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives two keys.
        integer(int64) :: k, s
        integer(int32) :: ns
        type(parquet_timestamp), pointer :: pts(:)
        !
        allocate(buf(2))
        buf(:)%family = SK_INT
        buf(:)%descending = descending
        buf(:)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))
        if (n <= 0_int64) then
            buf(1)%ints = 0_int64
            buf(2)%ints = 0_int64
            return
        end if
        call col%data_ptr(pts)
        do k = 1_int64, n
            call pts(k)%get_raw(s, ns)
            buf(1)%ints(k) = s
            buf(2)%ints(k) = int(ns, int64)
        end do
    end subroutine extract_col_timestamp
    !
    !> Copies a column's own per-row validity onto every key extracted from it, leaving it
    !! UNALLOCATED for a null-free column -- which is the engine's no-nulls fast path, and the same
    !! convention `%row_validity` itself uses by leaving its own result unallocated.
    !!
    !! **Deliberately built from per-row `%is_null` rather than `%row_validity`**, which would be
    !! the obvious choice: `%row_validity` is `intent(inout)` (a temporal column rescans and
    !! caches its null count there), so reaching it from a public `pf_argsort(column)` would mean
    !! either making that argument `intent(inout)` -- wrong for a query, and it would stop a caller
    !! passing their own `intent(in)` dummy -- or taking a copy of the column, which is a full deep
    !! copy of every value on the way to sorting it. `%is_null` is `intent(in)` and is always
    !! correct without consulting any cache, so it costs one extra O(n) pass in front of an
    !! O(n log n) sort and nothing else.
    subroutine col_valid_flags(col, n, buf)
        type(parquet_column), intent(in) :: col                 !! the key column.
        integer(int64), intent(in) :: n                         !! row count.
        type(sort_key_buf), intent(inout) :: buf(:)             !! the keys extracted from it.
        integer :: ik
        logical, allocatable :: rowmask(:)
        !
        ! O(1), and it answers the question the scan below was asking. A bitmap kind that has
        ! never had a null set allocates no bitmap at all, so there is nothing to look at -- which
        ! turns the null-free case, the common one, from a full pass of n un-inlinable %is_null
        ! calls into a single test. The temporal kinds answer .true. unconditionally (their null
        ! state lives inside the element, not in a bitmap), so they fall through to the scan on
        ! their own without needing a kind test here; that is the right answer for them, since a
        ! scan is genuinely the only way to know.
        if (.not. col%has_validity_storage()) return
        ! ONE bulk call, not two per-row scans. This used to walk the column twice with
        ! `col%is_null(k)` -- once to find out whether any null existed, once to record which --
        ! i.e. up to 2n un-inlinable cross-module calls, each re-checking the index and
        ! re-dispatching the kind. `%row_validity` does the same work by walking the validity
        ! bitmap a 64-bit WORD at a time, skipping 64 valid rows whenever a word is zero, and it
        ! specialises the string and temporal kinds above their loops rather than inside them.
        !
        ! It was unreachable from here until it became `intent(in)`: `pf_argsort(column)` holds its
        ! column by `intent(in)`, and `%row_validity` was `intent(inout)` purely so a temporal
        ! column could refresh a null cache in passing. Measured on a 4M-row int32 sort key, this
        ! phase went from ~20 ms to ~2 ms, which at 0.1% null density is most of what separated a
        ! null-bearing sort from a null-free one. THIS CALL IS ALSO THE ENFORCEMENT: narrowing
        ! %row_validity back to intent(inout) does not fail a test, it fails the build, here.
        !
        ! An unallocated result means "no nulls" (F2018 15.5.2.12 is why that convention exists
        ! throughout this library), which is exactly the early return the old `any_null` scan gave.
        call col%row_validity(rowmask)
        if (.not. allocated(rowmask)) return
        allocate(buf(1)%valid(max(n, 1_int64)))
        ! `merge` rather than a loop: one vectorisable pass over a LOGICAL array, against n
        ! branches. The two arrays are distinct, so no aliasing temporary is involved.
        buf(1)%valid(1:n) = merge(1_c_int8_t, 0_c_int8_t, rowmask(1:n))
        do ik = 2, size(buf)
            buf(ik)%valid = buf(1)%valid
        end do
    end subroutine col_valid_flags
    !''')

    # ---- pf_sort_keys %add specifics ----
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        w(f"    module procedure add_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        extra = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_sort_keys%add\"{extra})")
        w("        call keys_append(self, buf, \"pf_sort_keys%add\")")
        w(f"    end procedure add_{tag}")
        w("    !")

    w('''    module procedure keys_count
        ! The CALLER's count -- one per %add call -- not self%nkeys, which counts engine keys and
        ! answers 2 for a lone parquet_timestamp. See the add_ekeys component.
        n = 0
        if (allocated(self%add_ekeys)) n = size(self%add_ekeys)
    end procedure keys_count
    !
    module procedure keys_clear
        self%nkeys = 0
        self%nrows = -1_int64
        if (allocated(self%keys)) deallocate(self%keys)
        if (allocated(self%add_ekeys)) deallocate(self%add_ekeys)
    end procedure keys_clear
    !
    module procedure resolve_group_nkeys
        integer :: ncaller, k
        character(len=32) :: got_str, have_str
        !
        ncaller = 0
        if (allocated(keys%add_ekeys)) ncaller = size(keys%add_ekeys)
        if (.not. present(group_nkeys)) then
            group_ekeys = keys%nkeys
            return
        end if
        if (.not. want_offsets) then
            error stop EP // proc // ": group_nkeys was given without group_offsets; on its own " // &
                "it changes nothing, so ask for the boundaries too or drop it"
        end if
        if (group_nkeys < 1 .or. group_nkeys > ncaller) then
            write (got_str, "(i0)") group_nkeys
            write (have_str, "(i0)") ncaller
            error stop EP // proc // ": group_nkeys is " // trim(got_str) // ", which is not " // &
                "between 1 and the " // trim(have_str) // " keys given"
        end if
        ! Caller keys to engine keys. A prefix of the caller's keys is a prefix of the engine's,
        ! because %add appends its engine keys contiguously and in order.
        group_ekeys = 0
        do k = 1, group_nkeys
            group_ekeys = group_ekeys + keys%add_ekeys(k)
        end do
    end procedure resolve_group_nkeys
    !
    module procedure keys_append
        type(sort_key_buf), allocatable :: bigger(:)
        integer, allocatable :: more_ekeys(:)
        integer(int64) :: n
        integer :: ik
        character(len=32) :: got_str, want_str
        !
        n = key_rows(buf(1))
        if (self%nkeys == 0) then
            self%nrows = n
        else if (n /= self%nrows) then
            write (got_str, "(i0)") n
            write (want_str, "(i0)") self%nrows
            error stop EP // proc // ": every key must describe the same number of rows; this " // &
                "one has " // trim(got_str) // " where the first key has " // trim(want_str)
        end if
        ! Grown exactly, not geometrically: a key list is a handful of entries, and each entry
        ! holds only allocatable descriptors (the value buffers themselves are moved, not copied).
        if (.not. allocated(self%keys)) allocate(self%keys(0))
        allocate(bigger(self%nkeys + size(buf)))
        do ik = 1, self%nkeys
            call move_key(self%keys(ik), bigger(ik))
        end do
        do ik = 1, size(buf)
            call move_key(buf(ik), bigger(self%nkeys + ik))
        end do
        ! One entry per %add call, holding how many engine keys THIS call contributed -- the only
        ! record of the caller-versus-engine key distinction, and what %nkeys_added and group_nkeys
        ! both read. Recorded here rather than in each %add specific so a new key type cannot
        ! forget it.
        if (.not. allocated(self%add_ekeys)) allocate(self%add_ekeys(0))
        allocate(more_ekeys(size(self%add_ekeys) + 1))
        more_ekeys(1:size(self%add_ekeys)) = self%add_ekeys
        more_ekeys(size(more_ekeys)) = size(buf)
        call move_alloc(more_ekeys, self%add_ekeys)
        self%nkeys = self%nkeys + size(buf)
        call move_alloc(bigger, self%keys)
        deallocate(buf)
    end procedure keys_append
    !
    !> Moves one key's buffers from `src` to `dst` without copying them.
    subroutine move_key(src, dst)
        type(sort_key_buf), intent(inout) :: src !! the key to move from; left empty.
        type(sort_key_buf), intent(inout) :: dst !! the key to move into.
        dst%family = src%family
        dst%descending = src%descending
        dst%nulls_first = src%nulls_first
        if (allocated(src%ints)) call move_alloc(src%ints, dst%ints)
        if (allocated(src%reals)) call move_alloc(src%reals, dst%reals)
        if (allocated(src%offsets)) call move_alloc(src%offsets, dst%offsets)
        if (allocated(src%data)) call move_alloc(src%data, dst%data)
        if (allocated(src%valid)) call move_alloc(src%valid, dst%valid)
    end subroutine move_key
    !
    !> How many rows one extracted key describes.
    pure function key_rows(buf) result(n)
        type(sort_key_buf), intent(in) :: buf !! the key.
        integer(int64) :: n                   !! its row count.
        select case (buf%family)
        case (SK_REAL)
            n = size(buf%reals, kind=int64)
        case (SK_STR)
            n = size(buf%offsets, kind=int64) - 1_int64
        case default
            n = size(buf%ints, kind=int64)
        end select
    end function key_rows
    !
    module procedure valid_from_mask
        integer(int64) :: k, m
        character(len=32) :: got_str, want_str
        !
        m = size(mask, kind=int64)
        if (m /= n) then
            write (got_str, "(i0)") m
            write (want_str, "(i0)") n
            error stop EP // proc // ": is_valid has " // trim(got_str) // " elements but the " // &
                "values have " // trim(want_str)
        end if
        ! Left UNALLOCATED when nothing is null: the caller turns that into a null pointer, which
        ! the engine reads as "no nulls" and takes its own fast path for. Answering `.true.` for
        ! every row instead would be correct and measurably slower.
        if (all(mask(1:n))) return
        allocate(valid(max(n, 1_int64)))
        valid = 1_c_int8_t
        do k = 1_int64, n
            if (.not. mask(k)) valid(k) = 0_c_int8_t
        end do
    end procedure valid_from_mask
    !
    module procedure drive_engine
        type(c_ptr) :: builder
        integer(int64) :: status, nthreads
        integer :: ik
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once" ! GCOVR_EXCL_LINE
        end if
        ! Sized EXACTLY, never max(nrows, 1): a zero-row sort must hand back a zero-length
        ! permutation, or `size(perm)` lies and a caller's `do k = 1, size(perm)` reads element 1
        ! of an empty array. The engine is not called at all below two rows, so nothing downstream
        ! needs the one-element floor the extraction buffers use.
        allocate(perm(nrows))
        do ik = 1_int64, nrows
            perm(ik) = ik
        end do
        if (nrows < 2_int64) return
        call resolve_thread_count(threads, nrows, nthreads)
        if (dbg_fortran_engine) then
            ! Stage 2 scaffolding -- see `dbg_fortran_engine`'s declaration. **Stage 4 made this
            ! branch honour `threads`**, and the resolution above is deliberately SHARED with the
            ! C++ path below rather than repeated here, so the two engines are handed the same
            ! number by the same procedure and an A/B compares engines rather than policies.
            !
            ! The A/B stays valid at every thread count for a reason that is about the ordering and
            ! not about either implementation: both comparators end in a row-index tiebreaker, so no
            ! two distinct rows compare equal, exactly one permutation is correct, and a threaded
            ! answer that differs from a serial one is WRONG rather than merely different.
            call sort_build_permutation_threaded(keys, nrows, nthreads, perm)
            return
        end if
        if (size(keys) == 1) then
            ! One key needs no builder at all: the one-shot entry points BORROW the buffer that
            ! was just extracted, so this saves a handle allocation and a second copy of every
            ! value. Multi-key has to go through the builder, which owns its keys.
            call engine_one_shot(keys(1), nrows, nthreads, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build(builder, nthreads, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects -- kept
            ! because silently ignoring a nonzero status is how a real failure goes unnoticed.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine
    !
    module procedure drive_engine_partial
        type(c_ptr) :: builder
        integer(int64) :: status, ik
        integer :: jk
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(count))
        do ik = 1_int64, count
            perm(ik) = ik
        end do
        if (count < 1_int64 .or. nrows < 2_int64) return
        if (dbg_fortran_engine) then
            call sort_partial_permutation(keys, nrows, count, perm)
            return
        end if
        if (size(keys) == 1) then
            call engine_one_shot_partial(keys(1), nrows, count, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do jk = 1, size(keys)
            call engine_add_key(builder, keys(jk), nrows)
        end do
        status = parquet_sort_builder_build_partial(builder, count, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine_partial
    !
    module procedure engine_nth_index
        type(c_ptr) :: builder
        integer :: ik
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        if (dbg_fortran_engine) then
            call sort_nth_index(keys, nrows, nth, idx)
            return
        end if
        if (size(keys) == 1) then
            call engine_one_shot_nth(keys(1), nrows, nth, idx)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        idx = parquet_sort_builder_nth_element(builder, nth)
        call parquet_sort_builder_free(builder)
        if (idx < 1_int64) then
            ! The C side answers 0 for an empty key list or an out-of-range rank; both are already
            ! rejected above and by the caller's own bounds check.
            error stop EP // proc // ": the sort engine could not resolve that rank" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_nth_index
    !
    module procedure resolve_count
        character(len=32) :: n_str
        !
        ! Clamped, not refused: `n` is very often derived (a fraction of a row count, a config
        ! value, a post-filter survivor count), and aborting would put min(n, size(v)) at every
        ! call site. A NEGATIVE n is a different thing -- a caller error, not a boundary.
        if (n < 0) then
            write (n_str, "(i0)") n
            error stop EP // proc // ": n is " // trim(n_str) // ", which is negative"
        end if
        count = min(int(n, int64), nrows)
    end procedure resolve_count
    !
    !> Partially argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_partial(key, nrows, count, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: count           !! leading entries to order.
        integer(int64), intent(inout) :: perm(:)      !! receives `count` 1-based indices.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_partial_argsort_double(nrows, key%reals, vp, df, nf, count, perm)
        case (SK_STR)
            call parquet_sort_partial_argsort_string(nrows, key%offsets, key%data, vp, df, nf, count, perm)
        case default
            call parquet_sort_partial_argsort_int64(nrows, key%ints, vp, df, nf, count, perm)
        end select
    end subroutine engine_one_shot_partial
    !
    !> Resolves one already-extracted key's nth index through the matching one-shot entry point.
    subroutine engine_one_shot_nth(key, nrows, nth, idx)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nth             !! 1-based rank wanted.
        integer(int64), intent(out) :: idx            !! 1-based row index at that rank.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            idx = parquet_sort_nth_index_double(nrows, key%reals, vp, df, nf, nth)
        case (SK_STR)
            idx = parquet_sort_nth_index_string(nrows, key%offsets, key%data, vp, df, nf, nth)
        case default
            idx = parquet_sort_nth_index_int64(nrows, key%ints, vp, df, nf, nth)
        end select
    end subroutine engine_one_shot_nth
    !
    module procedure engine_is_sorted
        type(c_ptr) :: builder
        integer(int64) :: res
        integer :: ik
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once" ! GCOVR_EXCL_LINE
        end if
        answer = .true.
        if (nrows < 2_int64) return
        if (dbg_fortran_engine) then
            answer = sort_is_sorted(keys, nrows)
            return
        end if
        if (size(keys) == 1) then
            call engine_one_shot_is_sorted(keys(1), nrows, answer)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        res = parquet_sort_builder_is_sorted(builder)
        call parquet_sort_builder_free(builder)
        if (res < 0_int64) then
            error stop EP // proc // ": the sort engine had no key to test" ! GCOVR_EXCL_LINE
        end if
        answer = res == 1_int64
    end procedure engine_is_sorted
    !
    !> Argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot(key, nrows, nthreads, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nthreads        !! resolved thread count; 1 sorts serially.
        integer(int64), intent(inout) :: perm(:)      !! receives the 1-based permutation.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_argsort_double(nrows, key%reals, vp, df, nf, nthreads, perm)
        case (SK_STR)
            call parquet_sort_argsort_string(nrows, key%offsets, key%data, vp, df, nf, nthreads, perm)
        case default
            call parquet_sort_argsort_int64(nrows, key%ints, vp, df, nf, nthreads, perm)
        end select
    end subroutine engine_one_shot
    !
    !> Tests one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_is_sorted(key, nrows, answer)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        logical, intent(out) :: answer                !! .true. when already in order.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        integer(int64) :: res
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            res = parquet_sort_is_sorted_double(nrows, key%reals, vp, df, nf)
        case (SK_STR)
            res = parquet_sort_is_sorted_string(nrows, key%offsets, key%data, vp, df, nf)
        case default
            res = parquet_sort_is_sorted_int64(nrows, key%ints, vp, df, nf)
        end select
        answer = res == 1_int64
    end subroutine engine_one_shot_is_sorted
    !
    !> Adds one already-extracted key to a C++ builder.
    subroutine engine_add_key(builder, key, nrows)
        type(c_ptr), intent(in) :: builder            !! the builder handle.
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_builder_add_key_double(builder, key%reals, vp, df, nf)
        case (SK_STR)
            call parquet_sort_builder_add_key_string(builder, key%offsets, key%data, vp, df, nf)
        case default
            call parquet_sort_builder_add_key_int64(builder, key%ints, vp, df, nf)
        end select
    end subroutine engine_add_key
    !
    !> The three scalars every engine call needs: a pointer to the validity array (or a null
    !! pointer when the key has no nulls) and the two order flags as int8.
    subroutine key_flags(key, valid_ptr, desc_flag, nulls_flag)
        type(sort_key_buf), intent(in), target :: key   !! the key.
        type(c_ptr), intent(out) :: valid_ptr           !! its validity array, or C_NULL_PTR.
        integer(c_int8_t), intent(out) :: desc_flag     !! nonzero for descending.
        integer(c_int8_t), intent(out) :: nulls_flag    !! nonzero to place nulls first.
        !
        valid_ptr = c_null_ptr
        if (allocated(key%valid)) valid_ptr = c_loc(key%valid)
        desc_flag = merge(1_c_int8_t, 0_c_int8_t, key%descending)
        nulls_flag = merge(1_c_int8_t, 0_c_int8_t, key%nulls_first)
    end subroutine key_flags
    !
    module procedure check_rank
        character(len=32) :: a_str, b_str
        !
        if (nth < 1_int64 .or. nth > nrows) then
            write (a_str, "(i0)") nth
            write (b_str, "(i0)") nrows
            error stop EP // proc // ": nth is " // trim(a_str) // ", which is outside 1.." // &
                trim(b_str)
        end if
    end procedure check_rank
    !
    module procedure key_valid_count
        integer(int64) :: k
        !
        ! An unallocated `valid` is the module's "no nulls at all" convention, so the whole array
        ! counts -- the same fast path the engine itself takes.
        if (.not. allocated(keys(1)%valid)) then
            n_valid = nrows
            return
        end if
        n_valid = 0_int64
        do k = 1_int64, nrows
            if (keys(1)%valid(k) /= 0_c_int8_t) n_valid = n_valid + 1_int64
        end do
    end procedure key_valid_count
    !
    module procedure fold_token
        integer :: k, ic
        !
        tok = trim(adjustl(text))
        do k = 1, len(tok)
            ic = iachar(tok(k:k))
            if (ic >= iachar("A") .and. ic <= iachar("Z")) tok(k:k) = achar(ic + 32)
        end do
    end procedure fold_token
    !
    module procedure resolve_rounding
        character(len=:), allocatable :: tok, shown
        !
        mode = RND_NEAREST
        if (.not. present(rounding)) return
        call fold_token(rounding, tok)
        select case (tok)
        case ("nearest")
            mode = RND_NEAREST
        case ("down")
            mode = RND_DOWN
        case ("up")
            mode = RND_UP
        case default
            ! Capped to a short preview: the caller controls this string's length, and ifx's
            ! ERROR STOP runtime corrupts the heap once the composed message reaches 8192 bytes
            ! (CLAUDE.md). Same shape as parquet_filter_add's own rule preview.
            shown = trim(adjustl(rounding))
            if (len(shown) > 100) shown = shown(1:100) // "..."
            error stop EP // proc // ": rounding='" // shown // "' is not recognized; use " // &
                "'nearest' (the default), 'down' or 'up'"
        end select
    end procedure resolve_rounding
    !
    module procedure quantile_rank
        real(real64) :: pos
        !
        if (quantile < 0.0_real64 .or. quantile > 1.0_real64 .or. quantile /= quantile) then
            ! The NaN arm is what the self-comparison catches; ieee_is_nan would need another
            ! import here for one test, and this expression is exact with no arithmetic drift.
            error stop EP // proc // ": quantile must lie on a 0-1 scale (note: NOT 0-100)"
        end if
        if (n_valid < 1_int64) then
            error stop EP // proc // ": every value is null, so no quantile exists; guard with " // &
                "count(is_valid) (or the column's own null count) if that can happen"
        end if
        ! Position on the 0-based index scale of the non-null values, so quantile=0 gives the
        ! smallest and quantile=1 the largest exactly, with no rounding involved at either end.
        pos = quantile * real(n_valid - 1_int64, real64)
        select case (mode)
        case (RND_DOWN)
            rank = int(floor(pos), int64) + 1_int64
        case (RND_UP)
            rank = int(ceiling(pos), int64) + 1_int64
        case default
            rank = int(nint(pos, int64), int64) + 1_int64
        end select
        if (rank < 1_int64) rank = 1_int64
        if (rank > n_valid) rank = n_valid
    end procedure quantile_rank
    !
    module procedure check_permutation
        integer(int8), allocatable :: seen(:)
        integer(int64) :: k, v, word
        logical :: do_scan
        character(len=32) :: a_str, b_str
        !
        do_scan = .true.
        if (present(scan)) do_scan = scan
        if (size(perm, kind=int64) /= n) then
            write (a_str, "(i0)") size(perm, kind=int64)
            write (b_str, "(i0)") n
            error stop EP // proc // ": perm has " // trim(a_str) // " elements but the values " // &
                "have " // trim(b_str)
        end if
        if (n < 1_int64) return
        ! The length check above is unconditional; only the walk below is skippable. See the
        ! interface's own note for why a caller's promise cannot cover a wrong length.
        if (.not. do_scan) return
        ! A BIT-PACKED seen-set, not a LOGICAL array: gfortran's default LOGICAL is 32 bits, so a
        ! plain seen(n) would cost 4n bytes of scratch to validate a permutation whose own payload
        ! is 8n -- a 50% overhead on an operation whose whole point is to be cheap. This is n/8.
        allocate(seen((n + 7_int64) / 8_int64))
        seen = 0_int8
        do k = 1_int64, n
            v = perm(k)
            if (v < 1_int64 .or. v > n) then
                write (a_str, "(i0)") k
                write (b_str, "(i0)") v
                error stop EP // proc // ": perm(" // trim(a_str) // ") is " // trim(b_str) // &
                    ", which is outside the valid index range"
            end if
            word = (v - 1_int64) / 8_int64 + 1_int64
            if (btest(seen(word), int(mod(v - 1_int64, 8_int64)))) then
                write (a_str, "(i0)") v
                error stop EP // proc // ": perm is not a permutation -- the index " // &
                    trim(a_str) // " appears more than once"
            end if
            seen(word) = ibset(seen(word), int(mod(v - 1_int64, 8_int64)))
        end do
    end procedure check_permutation
    !
    module procedure pf_sort_threads
        use parquet_settings, only : parquet_get_sort_threads
#ifdef _OPENMP
        use omp_lib, only : omp_get_max_threads, omp_in_parallel
#endif
        integer :: cap
        !
        n = 1
#ifdef _OPENMP
        ! Serial inside a parallel region, deliberately. This is not a refusal and not a
        ! correctness guard -- it picks a DEFAULT, exactly as parallel_prefetch_ok
        ! (parquet_tables_read.f90) does for the table's own internally-parallel read, whose
        ! comment states the reason: nested regions are the caller's business. Without it, T
        ! OpenMP threads would each ask for T more, and T*T oversubscription is slower than not
        ! threading at all. An EXPLICIT threads= is still honoured there -- see
        ! resolve_thread_count, which only consults this when the caller said nothing.
        if (.not. omp_in_parallel()) n = omp_get_max_threads()
#endif
        ! parquet_set_sort_threads CAPS the automatic answer; it never raises it, and it never
        ! overrides the parallel-region rule above -- a caller who capped sorting at 8 said nothing
        ! about what should happen inside someone else's parallel region, and lifting the serial
        ! answer back to 8 there is exactly the T*T oversubscription that rule exists to prevent.
        ! This is the ONE place the setting is read: Risk-40 records that pf_sort_threads is public
        ! precisely so a read-time sort_by= and a raw-array sort ask the same question, and a second
        ! reader is how the two would come to disagree.
        cap = parquet_get_sort_threads()
        if (cap > 0 .and. cap < n) n = cap
    end procedure pf_sort_threads
    !
    module procedure resolve_thread_count
        !
        if (present(threads)) then
            ! An explicit request is honoured wherever it is made, including inside a parallel
            ! region: the caller has said what they want, and refusing it there would leave no way
            ! to thread a sort at all from code that is itself parallel.
            count = max(1_int64, int(threads, int64))
        else
            count = int(pf_sort_threads(), int64)
        end if
        ! Never more threads than rows; the C++ side clamps again by its own minimum chunk size.
        if (count > nrows) count = max(nrows, 1_int64)
    end procedure resolve_thread_count
    !
    module procedure narrow_perm
        integer(int64) :: n
        character(len=32) :: n_str
        !
        n = size(perm64, kind=int64)
        if (n > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array that
            ! would trip it cannot be built by any fixture this repository can run. Kept because
            ! the alternative is a silent truncation into a plausible wrong index.
            write (n_str, "(i0)") n
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, which " // &
                "does not fit an int32 permutation; declare perm as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        allocate(perm32(n))
        perm32 = int(perm64, int32)
    end procedure narrow_perm
    !
    module procedure narrow_offsets
        integer(int64) :: sentinel
        character(len=32) :: n_str
        !
        ! The SENTINEL, not the length -- and the difference is exactly one row. A permutation's
        ! largest entry is n, but this array's is n + 1, so at n == huge(int32) narrow_perm's own
        ! test passes while this one must not: a wrapped sentinel makes the last group's
        ! o(g+1) - 1 a huge negative bound, i.e. a silently wrong slice instead of an abort.
        sentinel = offsets64(size(offsets64))
        if (sentinel > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-row sort; the same reason
            ! narrow_perm's own guard has no test either. Kept because the failure it prevents is
            ! silent, which is precisely when an untestable guard earns its place. The marker sits
            ! INSIDE the test: the test itself runs on every call, and excluding it too would file
            ! it forever as a stale-exclusion candidate in tools/coverage.sh's own report.
            write (n_str, "(i0)") sentinel - 1_int64
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, so the " // &
                "group offsets do not fit int32; declare group_offsets as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        allocate(offsets32(size(offsets64)))
        offsets32 = int(offsets64, int32)
    end procedure narrow_offsets
    !
    module procedure narrow_i64
        character(len=32) :: v_str
        !
        if (value > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array that
            ! would trip it cannot be built by any fixture this repository can run. Kept because
            ! the alternative is a silent truncation into a plausible wrong index.
            write (v_str, "(i0)") value
            error stop EP // proc // ": the " // noun // " is " // trim(v_str) // ", which does " // &
                "not fit an int32; declare that argument as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        dst = int(value, int32)
    end procedure narrow_i64
    !
    module procedure narrow_i64_array
        integer(int64) :: n, biggest
        character(len=32) :: v_str
        !
        n = size(src, kind=int64)
        if (n > 0_int64) then
            biggest = maxval(src)
            if (biggest > int(huge(1_int32), int64)) then
                ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; see narrow_perm.
                write (v_str, "(i0)") biggest
                error stop EP // proc // ": the largest " // noun // " is " // trim(v_str) // &
                    ", which does not fit an int32; declare that argument as integer(int64)"
                ! GCOVR_EXCL_STOP
            end if
        end if
        allocate(dst(n))
        dst = int(src, int32)
    end procedure narrow_i64_array
    !
    module procedure resolve_rank_method
        character(len=:), allocatable :: tok, shown
        !
        mode = RANK_COMPETITION
        if (.not. present(method)) return
        call fold_token(method, tok)
        select case (tok)
        case ("competition")
            mode = RANK_COMPETITION
        case ("dense")
            mode = RANK_DENSE
        case ("ordinal")
            mode = RANK_ORDINAL
        case default
            ! Capped to a short preview, exactly as resolve_rounding is: the caller controls this
            ! string's length and ifx's ERROR STOP runtime corrupts the heap at 8192 bytes.
            shown = trim(adjustl(method))
            if (len(shown) > 100) shown = shown(1:100) // "..."
            error stop EP // proc // ": method='" // shown // "' is not recognized; use " // &
                "'competition' (the default), 'dense' or 'ordinal'"
        end select
    end procedure resolve_rank_method
    !
    module procedure key_null_mask
        integer(int64) :: k
        !
        allocate(isnull(max(nrows, 1_int64)))
        isnull = .false.
        if (.not. allocated(keys(1)%valid)) return
        do k = 1_int64, nrows
            isnull(k) = keys(1)%valid(k) == 0_c_int8_t
        end do
    end procedure key_null_mask
    !
    module procedure key_value_count
        integer(int64) :: k
        logical :: has_valid, is_real
        !
        ! Tier 0 of sort_tier_of, counted on the Fortran side rather than asked of the engine: it
        ! is the same two questions (is this row null, and -- for a real key only -- is it a NaN)
        ! and neither needs a comparison. A NaN is skipped because it is not a minimum or a maximum
        ! of anything, while remaining an ordinary value everywhere else in this module.
        has_valid = allocated(keys(1)%valid)
        is_real = keys(1)%family == SK_REAL
        n_value = 0_int64
        do k = 1_int64, nrows
            if (has_valid) then
                if (keys(1)%valid(k) == 0_c_int8_t) cycle
            end if
            if (is_real) then
                if (ieee_is_nan(keys(1)%reals(k))) cycle
            end if
            n_value = n_value + 1_int64
        end do
    end procedure key_value_count
    !
    module procedure check_sorted_input
        logical :: ok
        !
        call engine_is_sorted(keys, nrows, proc, ok)
        if (.not. ok) then
            error stop EP // proc // ": " // what // " is not sorted in the order given by " // &
                "descending/nulls_first; sort it first, or pass assume_sorted=.true. only for " // &
                "an order you have already established"
        end if
    end procedure check_sorted_input
    !
    module procedure buf_append
        integer :: ik
        integer(int64) :: total_d, total_s, k, n
        integer(int64), allocatable :: newoff(:), newints(:)
        real(real64), allocatable :: newreals(:)
        character(kind=c_char), allocatable :: newdata(:)
        integer(c_int8_t), allocatable :: newvalid(:)
        !
        if (.not. allocated(dst) .or. .not. allocated(src)) then
            ! Both come straight from an extract_* call, which always allocates.
            error stop EP // proc // ": internal error: a sort key was not extracted" ! GCOVR_EXCL_LINE
        end if
        if (size(dst) /= size(src)) then
            ! Only reachable if two different types were extracted into one pair, which no
            ! generated caller does -- every one extracts both sides with the same extractor.
            error stop EP // proc // ": internal error: mismatched key counts" ! GCOVR_EXCL_LINE
        end if
        n = nd + ns
        do ik = 1, size(dst)
            select case (dst(ik)%family)
            case (SK_REAL)
                allocate(newreals(max(n, 1_int64)))
                newreals = 0.0_real64
                if (nd > 0_int64) newreals(1:nd) = dst(ik)%reals(1:nd)
                if (ns > 0_int64) newreals(nd + 1_int64:n) = src(ik)%reals(1:ns)
                call move_alloc(newreals, dst(ik)%reals)
            case (SK_STR)
                ! The offsets are byte positions into `data`, so the appended half's have to be
                ! rebased by however many bytes the first half occupies -- this is the one family
                ! where concatenating two keys is not just concatenating two arrays.
                total_d = dst(ik)%offsets(nd + 1_int64)
                total_s = src(ik)%offsets(ns + 1_int64)
                allocate(newoff(n + 1_int64))
                newoff(1:nd + 1_int64) = dst(ik)%offsets(1:nd + 1_int64)
                do k = 1_int64, ns
                    newoff(nd + 1_int64 + k) = total_d + src(ik)%offsets(k + 1_int64)
                end do
                allocate(newdata(max(total_d + total_s, 1_int64)))
                if (total_d > 0_int64) newdata(1:total_d) = dst(ik)%data(1:total_d)
                if (total_s > 0_int64) newdata(total_d + 1_int64:total_d + total_s) = src(ik)%data(1:total_s)
                call move_alloc(newoff, dst(ik)%offsets)
                call move_alloc(newdata, dst(ik)%data)
            case default
                allocate(newints(max(n, 1_int64)))
                newints = 0_int64
                if (nd > 0_int64) newints(1:nd) = dst(ik)%ints(1:nd)
                if (ns > 0_int64) newints(nd + 1_int64:n) = src(ik)%ints(1:ns)
                call move_alloc(newints, dst(ik)%ints)
            end select
            ! Materialized only when at least one side has nulls, so a null-free append stays on
            ! the engine's no-nulls fast path. An absent half is all-valid, which is exactly what
            ! the unallocated convention means.
            if (allocated(dst(ik)%valid) .or. allocated(src(ik)%valid)) then
                allocate(newvalid(max(n, 1_int64)))
                newvalid = 1_c_int8_t
                if (allocated(dst(ik)%valid) .and. nd > 0_int64) newvalid(1:nd) = dst(ik)%valid(1:nd)
                if (allocated(src(ik)%valid) .and. ns > 0_int64) newvalid(nd + 1_int64:n) = src(ik)%valid(1:ns)
                call move_alloc(newvalid, dst(ik)%valid)
            end if
        end do
    end procedure buf_append
    !
    ! ---- The M3 engine drivers ----
    !
    ! Unlike drive_engine above, these three always go through the builder, even for a single key.
    ! The one-shot entry points exist to skip a copy on the hottest path in the library, and none
    ! of these is it -- run detection, binary search and merging each cost one extra copy of an
    ! already-extracted buffer in exchange for one entry point per operation instead of three.
    !
    module procedure engine_build_runs
        type(c_ptr) :: builder
        integer(int64) :: status, k, nthreads, gek
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(nrows))
        allocate(tie(max(nrows, 1_int64)))
        tie = 0_c_int8_t
        do k = 1_int64, nrows
            perm(k) = k
        end do
        if (nrows < 2_int64) return
        call resolve_thread_count(threads, nrows, nthreads)
        ! Resolved HERE, never in C++: the boundary carries a real count, never a "0 means all"
        ! sentinel, so the C++ side obeys rather than interprets what a prefix of zero would mean.
        gek = int(size(keys), int64)
        if (present(group_ekeys)) gek = int(group_ekeys, int64)
        if (dbg_fortran_engine) then
            call sort_build_runs_permutation(keys, nrows, gek, perm, tie)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build_runs(builder, nthreads, gek, perm, tie)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_build_runs
    !
    module procedure drive_engine_grouped
        integer(c_int8_t), allocatable :: tie(:)
        !
        if (.not. present(group_offsets)) then
            ! Nothing to report, so nothing is given up: this is drive_engine exactly, one-shot
            ! borrow and all. The branch is what keeps asking for boundaries the only thing that
            ! costs anything.
            call drive_engine(keys, nrows, proc, perm, threads=threads)
            return
        end if
        call engine_build_runs(keys, nrows, proc, perm, tie, threads=threads, group_ekeys=group_ekeys)
        call runs_to_offsets(tie, nrows, group_offsets)
    end procedure drive_engine_grouped
    !
    module procedure runs_to_offsets
        integer(int64) :: k, ngroups, pos
        !
        ! Bounded by nrows, NEVER by size(tie): engine_build_runs allocates tie with a
        ! max(nrows, 1) floor, so a zero-row sort leaves one element in it that describes no row
        ! and would otherwise be counted as a group.
        ngroups = 0_int64
        do k = 1_int64, nrows
            if (tie(k) == 0_c_int8_t) ngroups = ngroups + 1_int64
        end do
        allocate(offsets(ngroups + 1_int64))
        pos = 0_int64
        do k = 1_int64, nrows
            if (tie(k) == 0_c_int8_t) then
                pos = pos + 1_int64
                offsets(pos) = k
            end if
        end do
        ! The sentinel. It is what lets group g be perm(o(g) : o(g+1) - 1) for EVERY g including
        ! the last -- and for a zero-row sort it is the array's only entry, so `[1]` means "no
        ! groups" rather than an unallocated result every caller would have to test for.
        offsets(ngroups + 1_int64) = nrows + 1_int64
    end procedure runs_to_offsets
    !
    module procedure engine_search
        type(c_ptr) :: builder
        integer(c_int8_t) :: wflag
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        if (dbg_fortran_engine) then
            pos = sort_search_position(keys, n_search, upper)
            return
        end if
        wflag = merge(1_c_int8_t, 0_c_int8_t, upper)
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        pos = parquet_sort_builder_search(builder, n_search, wflag)
        call parquet_sort_builder_free(builder)
        if (pos < 1_int64) then
            ! The C side answers -1 only for an empty key list, rejected above.
            error stop EP // proc // ": the sort engine had no key to search" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_search
    !
    module procedure engine_merge
        type(c_ptr) :: builder
        integer(int64) :: status, k
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(nrows))
        do k = 1_int64, nrows
            perm(k) = k
        end do
        if (nrows < 2_int64) return
        if (dbg_fortran_engine) then
            call sort_merge_permutation(keys, nrows, na, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_merge(builder, na, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not merge" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_merge
    !
end submodule parquet_sorting_keys ! GCOVR_EXCL_LINE''')
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_argsort.f90 -- pf_argsort and pf_sort
# --------------------------------------------------------------------------------------
def rows_expr(t):
    """Expression giving the row count of `values` for type `t`."""
    family = t[3]
    if family == "strcol":
        return "values%size()"
    if family == "col":
        return "values%length()"
    return "size(values, kind=int64)"


def gen_argsort():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_argsort` (the permutation that would sort an array) and `pf_sort` (an independent sorted
!! copy), over every supported element type.
!!
!! Both are thin: they extract the values into the engine's canonical key form
!! (`parquet_sorting_keys`), run the engine, and -- for `pf_sort` -- gather the result. No ordering
!! decision is made here.
!!
!! **The int32 permutation forms exist because a caller with a default-kind `INTEGER` should not be
!! forced to widen one** (CLAUDE.md's "Public numeric arguments"). They compute in int64 and narrow
!! at the end, aborting rather than truncating when the array is longer than `huge(1_int32)`.
submodule (parquet_sorting) parquet_sorting_argsort
    implicit none
    !
contains
    !''')

    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure argsort_{tag}_{ik}")
            w("        type(sort_key_buf), allocatable :: buf(:)")
            w("        logical :: desc, nlo")
            if ik == "i32":
                w("        integer(int64), allocatable :: perm64(:), go64(:)")
            w("        !")
            w("        desc = .false.")
            w("        if (present(descending)) desc = descending")
            w("        nlo = .false.")
            w("        if (present(nulls_first)) nlo = nulls_first")
            w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_argsort\"{iv})")
            if ik == "i32":
                # A local cannot be conditionally absent, so the branch has to be here rather
                # than one level down -- forwarding an unconditional go64 would give up the
                # one-shot path for every caller who never asked for boundaries.
                w("        if (present(group_offsets)) then")
                w(f"            call drive_engine_grouped(buf, {rows_expr(t)}, \"pf_argsort\", perm64, &")
                w("                threads=threads, group_offsets=go64)")
                w("            call narrow_offsets(go64, \"pf_argsort\", group_offsets)")
                w("        else")
                w(f"            call drive_engine_grouped(buf, {rows_expr(t)}, \"pf_argsort\", perm64, threads=threads)")
                w("        end if")
                w("        call narrow_perm(perm64, \"pf_argsort\", perm)")
            else:
                # An absent optional dummy passed on as an optional actual stays absent
                # (F2018 15.5.2.13), so the i64 form needs no branch at all.
                w(f"        call drive_engine_grouped(buf, {rows_expr(t)}, \"pf_argsort\", perm, threads=threads, &")
                w("            group_offsets=group_offsets)")
            w(f"    end procedure argsort_{tag}_{ik}")
            w("    !")

    for ik, idecl, iname in IDX_KINDS:
        w(f"    module procedure argsort_keys_{ik}")
        w("        integer :: gek")
        if ik == "i32":
            w("        integer(int64), allocatable :: perm64(:), go64(:)")
        w("        !")
        w("        if (keys%nkeys < 1) then")
        w("            error stop EP // \"pf_argsort: this pf_sort_keys has no key; \" // &")
        w("                \"call keys%add(...) at least once before sorting\"")
        w("        end if")
        w("        call resolve_group_nkeys(keys, group_nkeys, present(group_offsets), \"pf_argsort\", gek)")
        if ik == "i32":
            w("        if (present(group_offsets)) then")
            w("            call drive_engine_grouped(keys%keys(1:keys%nkeys), keys%nrows, \"pf_argsort\", &")
            w("                perm64, threads=threads, group_offsets=go64, group_ekeys=gek)")
            w("            call narrow_offsets(go64, \"pf_argsort\", group_offsets)")
            w("        else")
            w("            call drive_engine_grouped(keys%keys(1:keys%nkeys), keys%nrows, \"pf_argsort\", &")
            w("                perm64, threads=threads)")
            w("        end if")
            w("        call narrow_perm(perm64, \"pf_argsort\", perm)")
        else:
            w("        call drive_engine_grouped(keys%keys(1:keys%nkeys), keys%nrows, \"pf_argsort\", perm, &")
            w("            threads=threads, group_offsets=group_offsets, group_ekeys=gek)")
        w(f"    end procedure argsort_keys_{ik}")
        w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        if not has_sort:
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure sort_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64), allocatable :: perm(:)")
        w("        integer(int64) :: k, n")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w(f"        n = {rows_expr(t)}")
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_sort\"{iv})")
        w("        call drive_engine(buf, n, \"pf_sort\", perm, threads=threads)")
        if family == "chr":
            w("        allocate(character(len=len(values)) :: sorted(n))")
        else:
            w("        allocate(sorted(n))")
        w("        do k = 1_int64, n")
        w("            sorted(k) = values(perm(k))")
        w("        end do")
        if nulls == "arg":
            w("        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's")
            w("        ! \"unallocated means no nulls\" convention governs an INPUT, where unallocated is")
            w("        ! the caller declining to supply information; an output they explicitly asked")
            w("        ! for is a direct question, and answering it with an unallocated array would")
            w("        ! force `if (allocated(...))` around every use.")
            w("        if (present(sorted_valid)) then")
            w("            allocate(sorted_valid(n))")
            w("            sorted_valid = .true.")
            w("            if (present(is_valid)) then")
            w("                do k = 1_int64, n")
            w("                    sorted_valid(k) = is_valid(perm(k))")
            w("                end do")
            w("            end if")
            w("        end if")
        w(f"    end procedure sort_{tag}")
        w("    !")

    w("end submodule parquet_sorting_argsort ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_permute.f90 -- pf_permute and pf_is_sorted
# --------------------------------------------------------------------------------------
def gen_permute():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_permute` (apply a permutation in place) and `pf_is_sorted` (test an existing order).
!!
!! **`pf_permute` gathers into a fresh array and moves it back**, rather than following cycles in
!! place. Cycle-following would save the temporary, but it needs a "visited" bit per element and
!! would have to be written once per type anyway; the gather is one obvious pass, and for the two
!! container types the work is delegated to their own `%reindex` regardless.
!!
!! **`perm` is validated before anything is written.** An invalid permutation does not fail -- it
!! silently duplicates some elements and drops others, which is precisely the wrong-answer-with-no-
!! symptom class this project guards hardest against. The check is O(n) in front of an O(n)
!! operation, so it is a constant factor rather than a change of complexity, which is why it is on
!! by default; `assume_valid=.true.` skips it for a permutation that came straight from
!! `pf_argsort`. It skips the CONTENTS check only -- `perm`'s length is checked either way, since a
!! short permutation makes the gather read past the end of `values`.
submodule (parquet_sorting) parquet_sorting_permute
    implicit none
    !
contains
    !''')

    for t in TYPES:
        tag, decl, what, family, nulls, _, how = t
        for ik, idecl, iname in IDX_KINDS:
            # Everything downstream of here -- check_permutation, the gather, %reindex -- wants an
            # integer(int64) permutation. For the _i32 specifics that means a widened copy; for the
            # _i64 specifics `perm` ALREADY IS one, and building `p64` from it was an allocation
            # plus a full copy of n int64s per call, to produce a value equal to an argument that
            # was already in hand. `pidx` is whichever of the two is correct for this specific, so
            # the body below is written once and neither kind can drift from the other.
            widen = ik == "i32"
            pidx = "p64" if widen else "perm"
            w(f"    module procedure permute_{tag}_{ik}")
            if widen:
                w("        integer(int64), allocatable :: p64(:)")
            if how == "gather":
                if family == "chr":
                    w("        character(len=len(values)), allocatable :: tmp(:)")
                else:
                    w(f"        {decl}, allocatable :: tmp(:)")
                w("        integer(int64) :: k, n")
            w("        logical :: skip")
            w("        !")
            w("        skip = .false.")
            w("        if (present(assume_valid)) skip = assume_valid")
            if widen:
                w("        allocate(p64(size(perm, kind=int64)))")
                w("        p64 = int(perm, int64)")
            if how == "gather":
                w(f"        n = {rows_expr(t)}")
                w("        ! The LENGTH is checked even under assume_valid=.true.: the gather below")
                w(f"        ! indexes values({pidx}(k)) for k = 1..size(values), so a short perm would")
                w("        ! read past its end. Only the O(n) contents walk is what the caller may skip.")
                w(f"        call check_permutation({pidx}, n, \"pf_permute\", scan=.not. skip)")
                if family == "chr":
                    w("        allocate(character(len=len(values)) :: tmp(n))")
                else:
                    w("        allocate(tmp(n))")
                w("        do k = 1_int64, n")
                w(f"            tmp(k) = values({pidx}(k))")
                w("        end do")
                # `tmp` is a fresh local and `values` is the dummy, so the two can never alias and
                # the section assignment is legal here -- CLAUDE.md's rule against array sections is
                # about a SELF-overlapping assignment, which needs a temporary per element, and this
                # is the other side of that same rule. It is written this way because one statement
                # beats three, NOT for speed: measured against the explicit `do k` loop it replaced,
                # 11.91 ms vs 11.89 ms on a 4M-element f64 permute, i.e. exactly 1.00x. gfortran was
                # already emitting the same thing. Do not cite this line as an optimisation.
                w("        values(1:n) = tmp(1:n)")
            else:
                w("        ! `assume_valid` means the same thing here as for the nine array types:")
                w("        ! %reindex_trusted skips the O(n) contents walk and keeps the O(1) length")
                w("        ! check. Both column types validate unconditionally without it.")
                w("        if (skip) then")
                w(f"            call values%reindex_trusted({pidx})")
                w("        else")
                w(f"            call values%reindex({pidx})")
                w("        end if")
            w(f"    end procedure permute_{tag}_{ik}")
            w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure is_sorted_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_is_sorted\"{iv})")
        w(f"        call engine_is_sorted(buf, {rows_expr(t)}, \"pf_is_sorted\", answer)")
        w(f"    end procedure is_sorted_{tag}")
        w("    !")

    w("    module procedure is_sorted_keys")
    w("        !")
    w("        if (keys%nkeys < 1) then")
    w("            error stop EP // \"pf_is_sorted: this pf_sort_keys has no key; \" // &")
    w("                \"call keys%add(...) at least once before asking\"")
    w("        end if")
    w("        call engine_is_sorted(keys%keys(1:keys%nkeys), keys%nrows, \"pf_is_sorted\", answer)")
    w("    end procedure is_sorted_keys")
    w("    !")

    w("end submodule parquet_sorting_permute ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_select.f90 -- pf_partial_sort and pf_partial_argsort
# --------------------------------------------------------------------------------------
def gen_select():
    o = []
    w = o.append
    w(BANNER)
    w("""!> `pf_partial_sort` and `pf_partial_argsort` -- ordering only the first `n` elements.
!!
!! Same engine, same comparator, same tiers as a full sort (`feature_risks.md` Risk-34): these
!! reach `std::partial_sort` through the very object `std::sort` is given, so a partial result can
!! never disagree with the corresponding prefix of a full one.
!!
!! **`n` is clamped, not checked.** Asking for more elements than the array holds returns all of
!! them, in order. That is deliberate -- `n` is very often derived, and refusing it would put
!! `min(n, size(v))` at every call site. A negative `n` is a caller error and aborts.
!!
!! **A partial sort that is not actually partial is invisible**: returning the first `n` of a FULL
!! sort is correct and merely slower, so no correctness test can tell the two apart. That is what
!! `parquet_debug_get_sort_comparisons` exists for, and why the guide states the complexity claim
!! with its own caveat rather than as a free win.
submodule (parquet_sorting) parquet_sorting_select
    implicit none
    !
contains
    !""")

    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure partial_argsort_{tag}_{ik}")
            w("        type(sort_key_buf), allocatable :: buf(:)")
            w("        integer(int64), allocatable :: perm64(:)")
            w("        integer(int64) :: nrows, count")
            w("        logical :: desc, nlo")
            w("        !")
            w("        desc = .false.")
            w("        if (present(descending)) desc = descending")
            w("        nlo = .false.")
            w("        if (present(nulls_first)) nlo = nulls_first")
            w(f"        nrows = {rows_expr(t)}")
            w("        call resolve_count(n, nrows, \"pf_partial_argsort\", count)")
            w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_partial_argsort\"{iv})")
            w("        call drive_engine_partial(buf, nrows, count, \"pf_partial_argsort\", perm64)")
            if ik == "i32":
                w("        call narrow_perm(perm64, \"pf_partial_argsort\", perm)")
            else:
                w("        call move_alloc(perm64, perm)")
            w(f"    end procedure partial_argsort_{tag}_{ik}")
            w("    !")

    for ik, idecl, iname in IDX_KINDS:
        w(f"    module procedure partial_argsort_keys_{ik}")
        w("        integer(int64), allocatable :: perm64(:)")
        w("        integer(int64) :: count")
        w("        !")
        w("        if (keys%nkeys < 1) then")
        w("            error stop EP // \"pf_partial_argsort: this pf_sort_keys has no key; \" // &")
        w("                \"call keys%add(...) at least once before sorting\"")
        w("        end if")
        w("        call resolve_count(n, keys%nrows, \"pf_partial_argsort\", count)")
        w("        call drive_engine_partial(keys%keys(1:keys%nkeys), keys%nrows, count, &")
        w("            \"pf_partial_argsort\", perm64)")
        if ik == "i32":
            w("        call narrow_perm(perm64, \"pf_partial_argsort\", perm)")
        else:
            w("        call move_alloc(perm64, perm)")
        w(f"    end procedure partial_argsort_keys_{ik}")
        w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, has_sort, _ = t
        if not has_sort:
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure partial_sort_{tag}")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64), allocatable :: perm(:)")
        w("        integer(int64) :: k, nrows, count")
        w("        logical :: desc, nlo")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w(f"        nrows = {rows_expr(t)}")
        w("        call resolve_count(n, nrows, \"pf_partial_sort\", count)")
        w(f"        call extract_{tag}(values, buf, desc, nlo, \"pf_partial_sort\"{iv})")
        w("        call drive_engine_partial(buf, nrows, count, \"pf_partial_sort\", perm)")
        if family == "chr":
            w("        allocate(character(len=len(values)) :: sorted(count))")
        else:
            w("        allocate(sorted(count))")
        w("        do k = 1_int64, count")
        w("            sorted(k) = values(perm(k))")
        w("        end do")
        if nulls == "arg":
            w("        if (present(sorted_valid)) then")
            w("            allocate(sorted_valid(count))")
            w("            sorted_valid = .true.")
            w("            if (present(is_valid)) then")
            w("                do k = 1_int64, count")
            w("                    sorted_valid(k) = is_valid(perm(k))")
            w("                end do")
            w("            end if")
            w("        end if")
        w(f"    end procedure partial_sort_{tag}")
        w("    !")

    # ---- pf_nth_element: one shared impl per type, six thin specifics over it ----
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_nth(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for nk, nkdecl, _ in IDX_KINDS:
            for ik, idecl, _ in [(None, None, None)] + IDX_KINDS:
                sfx = "_" + ik if ik else ""
                w("    module procedure nth_" + tag + "_" + nk + sfx)
                w("        integer(int64) :: idx")
                w("        logical :: desc, nlo")
                w("        !")
                w("        desc = .false.")
                w("        if (present(descending)) desc = descending")
                w("        nlo = .false.")
                w("        if (present(nulls_first)) nlo = nulls_first")
                w("        call nth_impl_" + tag + "(values, int(nth, int64), p_value, idx, desc, nlo" + iv + ")")
                if ik == "i32":
                    w("        call narrow_index(idx, \"pf_nth_element\", index)")
                elif ik == "i64":
                    w("        index = idx")
                w("    end procedure nth_" + tag + "_" + nk + sfx)
                w("    !")

    # ---- pf_nth_quantile: same shape, over its own shared impl ----
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_nth(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, _ in [(None, None, None)] + IDX_KINDS:
            sfx = "_" + ik if ik else ""
            w("    module procedure quantile_" + tag + sfx)
            w("        integer(int64) :: idx, nn")
            w("        !")
            w("        call quantile_impl_" + tag + "(values, quantile, p_value, idx, nn, rounding" + iv + ")")
            w("        if (present(n_null)) n_null = nn")
            if ik == "i32":
                w("        call narrow_index(idx, \"pf_nth_quantile\", index)")
            elif ik == "i64":
                w("        index = idx")
            w("    end procedure quantile_" + tag + sfx)
            w("    !")

    # ---- the two shared workers, one pair per type ----
    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_nth(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        iarg = ", is_valid" if nulls == "arg" else ""
        getval = ("        call values%get(idx, p_value, allow_null=.true.)" if family == "strcol"
                  else "        p_value = values(idx)")
        w("    !> Shared worker behind every pf_nth_element specific for a " + what + " array.")
        w("    subroutine nth_impl_" + tag + "(values, nth, p_value, idx, descending, nulls_first" + iarg + ")")
        w(val_decl(t, "in"))
        w("        integer(int64), intent(in) :: nth   !! 1-based rank wanted.")
        w(pval_decl(t) + " !! the value at that rank.")
        w("        integer(int64), intent(out) :: idx  !! which element of `values` that was.")
        w("        logical, intent(in) :: descending   !! .true. ranks high to low.")
        w("        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.")
        if nulls == "arg":
            w("        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64) :: nrows")
        w("        !")
        w("        nrows = " + rows_expr(t))
        w("        call check_rank(nth, nrows, \"pf_nth_element\")")
        w("        call extract_" + tag + "(values, buf, descending, nulls_first, \"pf_nth_element\"" + iv + ")")
        w("        call engine_nth_index(buf, nrows, nth, \"pf_nth_element\", idx)")
        w(getval)
        w("    end subroutine nth_impl_" + tag)
        w("    !")
        w("    !> Shared worker behind every pf_nth_quantile specific for a " + what + " array.")
        w("    subroutine quantile_impl_" + tag + "(values, quantile, p_value, idx, n_null, rounding" + iarg + ")")
        w(val_decl(t, "in"))
        w("        real(real64), intent(in) :: quantile !! position on a 0-1 scale.")
        w(pval_decl(t) + " !! the value at that quantile.")
        w("        integer(int64), intent(out) :: idx     !! which element of `values` that was.")
        w("        integer(int64), intent(out) :: n_null  !! how many values were null.")
        w("        character(len=*), intent(in), optional :: rounding !! rounding token.")
        if nulls == "arg":
            w("        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64) :: nrows, n_valid, rank")
        w("        integer :: mode")
        w("        !")
        w("        call resolve_rounding(rounding, \"pf_nth_quantile\", mode)")
        w("        nrows = " + rows_expr(t))
        w("        ! Ascending with nulls LAST, unconditionally: the population is the non-null")
        w("        ! values, so a rank in 1..n_valid can never address a null.")
        w("        call extract_" + tag + "(values, buf, .false., .false., \"pf_nth_quantile\"" + iv + ")")
        w("        call key_valid_count(buf, nrows, n_valid)")
        w("        n_null = nrows - n_valid")
        w("        call quantile_rank(quantile, n_valid, mode, \"pf_nth_quantile\", rank)")
        w("        call engine_nth_index(buf, nrows, rank, \"pf_nth_quantile\", idx)")
        w(getval)
        w("    end subroutine quantile_impl_" + tag)
        w("    !")

    w("    !> Narrows a 1-based int64 index to int32, aborting rather than truncating.")
    w("    subroutine narrow_index(idx64, proc, idx32)")
    w("        integer(int64), intent(in) :: idx64  !! the index.")
    w("        character(len=*), intent(in) :: proc !! calling procedure, for messages.")
    w("        integer(int32), intent(out) :: idx32 !! the narrowed copy.")
    w("        character(len=32) :: n_str")
    w("        !")
    w("        if (idx64 > int(huge(1_int32), int64)) then")
    w("            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array")
    w("            ! that would trip it cannot be built by any fixture this repository can run.")
    w("            ! Kept because the alternative is a silent truncation into a wrong index.")
    w("            write (n_str, \"(i0)\") idx64")
    w("            error stop EP // proc // \": the answer is at element \" // trim(n_str) // &")
    w("                \", which does not fit an int32 index; declare index as integer(int64)\"")
    w("            ! GCOVR_EXCL_STOP")
    w("        end if")
    w("        idx32 = int(idx64, int32)")
    w("    end subroutine narrow_index")
    w("    !")
    w("end submodule parquet_sorting_select ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_search.f90 -- pf_lower_bound, pf_upper_bound, pf_equal_range
# --------------------------------------------------------------------------------------
def emit_target_extract(w, t, indent="        "):
    """Extracts ONE search target into `tbuf`, as a one-row key matching `buf`."""
    tag, decl, what, family, nulls, _, _ = t
    if family == "chr":
        w(indent + "! Compared at the ARRAY's element length, which is what makes this agree with")
        w(indent + "! Fortran's own `<` on the same two operands: a shorter target is blank-padded,")
        w(indent + "! exactly as a comparison would pad it. A target with non-blank characters past")
        w(indent + "! that length has no exact answer here at all, so it is refused rather than")
        w(indent + "! silently truncated into a different value.")
        w(indent + "if (len_trim(target) > len(values)) then")
        w(indent + "    write (a_str, \"(i0)\") len_trim(target)")
        w(indent + "    write (b_str, \"(i0)\") len(values)")
        w(indent + "    error stop EP // proc // \": target has \" // trim(a_str) // \" non-blank \" // &")
        w(indent + "        \"characters but values holds \" // trim(b_str) // \" per element, so no \" // &")
        w(indent + "        \"exact comparison exists; widen values or trim target\"")
        w(indent + "end if")
        w(indent + "padded = target")
        w(indent + "call extract_chr([padded], tbuf, desc, nlo, proc)")
    elif family == "strcol":
        w(indent + "! A parquet_string_column stores bytes verbatim, so the target is used verbatim")
        w(indent + "! too -- trailing blanks included. There is no declared width to pad to.")
        w(indent + "call extract_chr([target], tbuf, desc, nlo, proc)")
    else:
        w(indent + f"call extract_{tag}([target], tbuf, desc, nlo, proc)")


def gen_search():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_lower_bound`, `pf_upper_bound` and `pf_equal_range` -- locating a value in an array that
!! is ALREADY sorted.
!!
!! **The target is appended to the array's own key and compared as row n+1.** That is the whole
!! design: there is no compare-a-row-against-a-value arm anywhere, so a search cannot drift from
!! the order `pf_sort` produces (`feature_risks.md` Risk-34). It costs one element of copy.
!!
!! **Searching unsorted input is the worst failure this module can have** -- a plausible index, no
!! abort, no symptom. So sortedness is checked by default, at O(n) in front of an O(log n) search,
!! and `assume_sorted=.true.` is the caller's explicit statement that they have established the
!! order themselves. Do not flip that default.
submodule (parquet_sorting) parquet_sorting_search
    implicit none
    !
contains
    !''')

    for base, want, res in (("lower_bound", "SRCH_LOWER", "pos"),
                            ("upper_bound", "SRCH_UPPER", "pos"),
                            ("equal_range", "SRCH_BOTH", "range")):
        for t in TYPES:
            if not has_search(t):
                continue
            tag, decl, what, family, nulls, _, _ = t
            iv = ", is_valid=is_valid" if nulls == "arg" else ""
            for ik, idecl, iname in IDX_KINDS:
                w(f"    module procedure {base}_{tag}_{ik}")
                w("        integer(int64) :: lo, hi")
                w("        !")
                w(f"        call search_impl_{tag}(values, target, {want}, descending, nulls_first, &")
                w(f"            assume_sorted, \"pf_{base}\", lo, hi{iv})")
                if res == "range":
                    if ik == "i32":
                        w(f"        call narrow_i64(lo, \"pf_{base}\", \"first matching index\", first)")
                        w(f"        call narrow_i64(hi - 1_int64, \"pf_{base}\", \"last matching index\", last)")
                    else:
                        w("        first = lo")
                        w("        last = hi - 1_int64")
                else:
                    src = "lo" if want == "SRCH_LOWER" else "hi"
                    if ik == "i32":
                        w(f"        call narrow_i64({src}, \"pf_{base}\", \"insertion point\", pos)")
                    else:
                        w(f"        pos = {src}")
                w(f"    end procedure {base}_{tag}_{ik}")
                w("    !")

    for t in TYPES:
        if not has_search(t):
            continue
        tag, decl, what, family, nulls, _, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        iarg = ", is_valid" if nulls == "arg" else ""
        w(f"    !> Shared worker behind every search specific for a {what} array. Extracts once,")
        w("    !! checks the order once, and runs one or both binary searches over the result.")
        w(f"    subroutine search_impl_{tag}(values, target, want, descending, nulls_first, &")
        w(f"            assume_sorted, proc, lo, hi{iarg})")
        w(val_decl(t, "in"))
        w(tgt_decl(t) + " !! the value to look for.")
        w("        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.")
        w("        logical, intent(in), optional :: descending       !! .true. for high-to-low order.")
        w("        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.")
        w("        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.")
        w("        character(len=*), intent(in) :: proc              !! calling procedure, for messages.")
        w("        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.")
        w("        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.")
        if nulls == "arg":
            w("        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w("        type(sort_key_buf), allocatable :: buf(:), tbuf(:)")
        w("        integer(int64) :: n")
        w("        logical :: desc, nlo, check")
        if family == "chr":
            w("        character(len=len(values)) :: padded")
            w("        character(len=32) :: a_str, b_str")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w("        check = .true.")
        w("        if (present(assume_sorted)) check = .not. assume_sorted")
        w(f"        n = {rows_expr(t)}")
        w(f"        call extract_{tag}(values, buf, desc, nlo, proc{iv})")
        w("        if (check) call check_sorted_input(buf, n, proc, \"values\")")
        emit_target_extract(w, t)
        w("        call buf_append(buf, n, tbuf, 1_int64, proc)")
        w("        lo = 0_int64")
        w("        hi = 0_int64")
        w("        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)")
        w("        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)")
        w(f"    end subroutine search_impl_{tag}")
        w("    !")

    w("end submodule parquet_sorting_search ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_unique.f90 -- pf_unique_count, pf_unique, pf_rank
# --------------------------------------------------------------------------------------
def gen_unique():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_unique_count`, `pf_unique` and `pf_rank` -- questions about repeated values.
!!
!! All three rest on one engine call (`engine_build_runs`), which sorts and reports where the runs
!! of EQUAL rows are in the same pass. Equality is the sort comparator's own, minus the index
!! tiebreaker that makes it a total order -- so "distinct" here means exactly "the sort would not
!! have to choose between them", and two NaNs are one value even though `==` says otherwise.
!!
!! **Nulls are outside the population, in all three.** They are excluded from a count, excluded
!! from the distinct values, and given rank 0 rather than a place in the ranking. That is why none
!! of the three takes `nulls_first`: there is no null tier to position. Extraction is therefore
!! always `nulls_first=.false.`, which puts every null last and contiguous -- the property the
!! walks below rely on to stop counting.
submodule (parquet_sorting) parquet_sorting_unique
    implicit none
    !
contains
    !''')

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure unique_count_{tag}_{ik}")
            w("        integer(int64), allocatable :: idxs(:)")
            w("        integer(int64) :: nd, nn")
            w("        !")
            w(f"        call unique_impl_{tag}(values, .false., \"pf_unique_count\", idxs, nd, nn{iv}, &")
            w("            threads=threads)")
            w("        if (present(n_null)) n_null = nn")
            if ik == "i32":
                w("        call narrow_i64(nd, \"pf_unique_count\", \"distinct-value count\", count)")
            else:
                w("        count = nd")
            w(f"    end procedure unique_count_{tag}_{ik}")
            w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_unique(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure unique_{tag}")
        w("        integer(int64), allocatable :: idxs(:)")
        w("        integer(int64) :: k, nd, nn")
        w("        logical :: desc")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w(f"        call unique_impl_{tag}(values, desc, \"pf_unique\", idxs, nd, nn{iv}, threads=threads)")
        w("        if (present(n_null)) n_null = nn")
        if family == "chr":
            w("        allocate(character(len=len(values)) :: distinct(nd))")
        elif family == "strcol":
            w("        call distinct%clear()")
        else:
            w("        allocate(distinct(nd))")
        if family == "strcol":
            w("        ! `%append_from` carries both the bytes and the null state, so nothing is")
            w("        ! materialized per element. `idxs` only ever names non-null elements -- the walk")
            w("        ! in unique_impl_strcol stops at the first null -- so carrying the null state")
            w("        ! cannot change what lands here. See feature_risks.md Risk-60.")
        w("        do k = 1_int64, nd")
        if family == "strcol":
            w("            call distinct%append_from(values, idxs(k))")
        else:
            w("            distinct(k) = values(idxs(k))")
        w("        end do")
        w(f"    end procedure unique_{tag}")
        w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure rank_{tag}_{ik}")
            w("        integer(int64), allocatable :: r64(:)")
            w("        logical :: desc")
            w("        !")
            w("        desc = .false.")
            w("        if (present(descending)) desc = descending")
            w(f"        call rank_impl_{tag}(values, method, desc, \"pf_rank\", r64{iv}, threads=threads)")
            if ik == "i32":
                w("        call narrow_i64_array(r64, \"pf_rank\", \"rank\", ranks)")
            else:
                w("        call move_alloc(r64, ranks)")
            w(f"    end procedure rank_{tag}_{ik}")
            w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        iarg = ", is_valid" if nulls == "arg" else ""
        w(f"    !> Shared worker behind pf_unique_count and pf_unique for a {what} array: the")
        w("    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.")
        w(f"    subroutine unique_impl_{tag}(values, descending, proc, first_idx, ndist, nnull{iarg}, threads)")
        w(val_decl(t, "in"))
        w("        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.")
        w("        character(len=*), intent(in) :: proc !! calling procedure, for messages.")
        w("        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.")
        w("        integer(int64), intent(out) :: ndist !! how many distinct non-null values.")
        w("        integer(int64), intent(out) :: nnull !! how many values were null.")
        if nulls == "arg":
            w("        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w("        integer, intent(in), optional :: threads !! thread request; absent = auto.")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64), allocatable :: perm(:)")
        w("        integer(c_int8_t), allocatable :: tie(:)")
        w("        logical, allocatable :: isnull(:)")
        w("        integer(int64) :: k, n")
        w("        !")
        w(f"        n = {rows_expr(t)}")
        w(f"        call extract_{tag}(values, buf, descending, .false., proc{iv})")
        w("        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)")
        w("        call key_null_mask(buf, n, isnull)")
        w("        allocate(first_idx(max(n, 1_int64)))")
        w("        first_idx = 0_int64")
        w("        ndist = 0_int64")
        w("        nnull = 0_int64")
        w("        do k = 1_int64, n")
        w("            ! nulls_first=.false. puts every null in the last tier, so the first one ends")
        w("            ! the walk and the rest of the array is exactly the null count.")
        w("            if (isnull(perm(k))) then")
        w("                nnull = n - k + 1_int64")
        w("                exit")
        w("            end if")
        w("            if (tie(k) == 0_c_int8_t) then")
        w("                ndist = ndist + 1_int64")
        w("                first_idx(ndist) = perm(k)")
        w("            end if")
        w("        end do")
        w(f"    end subroutine unique_impl_{tag}")
        w("    !")
        w(f"    !> Shared worker behind every pf_rank specific for a {what} array.")
        w(f"    subroutine rank_impl_{tag}(values, method, descending, proc, ranks{iarg}, threads)")
        w(val_decl(t, "in"))
        w("        character(len=*), intent(in), optional :: method !! tie-handling token.")
        w("        logical, intent(in) :: descending    !! .true. ranks high to low.")
        w("        character(len=*), intent(in) :: proc !! calling procedure, for messages.")
        w("        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.")
        if nulls == "arg":
            w("        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w("        integer, intent(in), optional :: threads !! thread request; absent = auto.")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64), allocatable :: perm(:)")
        w("        integer(c_int8_t), allocatable :: tie(:)")
        w("        logical, allocatable :: isnull(:)")
        w("        integer(int64) :: k, i, n, seen, dense, run_start")
        w("        integer :: mode")
        w("        !")
        w("        call resolve_rank_method(method, proc, mode)")
        w(f"        n = {rows_expr(t)}")
        w(f"        call extract_{tag}(values, buf, descending, .false., proc{iv})")
        w("        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)")
        w("        call key_null_mask(buf, n, isnull)")
        w("        allocate(ranks(n))")
        w("        if (n < 1_int64) return")
        w("        ranks = 0_int64")
        w("        seen = 0_int64")
        w("        dense = 0_int64")
        w("        run_start = 0_int64")
        w("        do k = 1_int64, n")
        w("            i = perm(k)")
        w("            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are")
        w("            ! contiguous at the end: the walk should not depend on that placement twice.")
        w("            if (isnull(i)) cycle")
        w("            if (tie(k) == 0_c_int8_t) then")
        w("                run_start = seen + 1_int64")
        w("                dense = dense + 1_int64")
        w("            end if")
        w("            seen = seen + 1_int64")
        w("            select case (mode)")
        w("            case (RANK_DENSE)")
        w("                ranks(i) = dense")
        w("            case (RANK_ORDINAL)")
        w("                ranks(i) = seen")
        w("            case default")
        w("                ranks(i) = run_start")
        w("            end select")
        w("        end do")
        w(f"    end subroutine rank_impl_{tag}")
        w("    !")

    w("end submodule parquet_sorting_unique ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


# --------------------------------------------------------------------------------------
# src/parquet_sorting_reduce.f90 -- pf_minmax, pf_argminmax, pf_merge
# --------------------------------------------------------------------------------------
def gen_reduce():
    o = []
    w = o.append
    w(BANNER)
    w('''!> `pf_minmax`, `pf_argminmax` and `pf_merge`.
!!
!! **The extremes are two `nth_element` calls, not a hand-written scan.** Rank 1 ascending is the
!! minimum and rank 1 DESCENDING is the maximum, so reaching them through the engine means the
!! answers cannot disagree with `pf_sort`'s own ends (`feature_risks.md` Risk-34). Both calls are
!! O(n), the same as the scan would be, and neither needs a per-type comparison written here.
!!
!! Rank `n_value` of the ascending order would name the same maximum VALUE, but the last of a tied
!! run rather than the first -- see `minmax_impl_*`'s own comment for why that asymmetry is not
!! acceptable in a pair of answers a caller reads together.
!!
!! `n_value` counts rows that are neither null nor NaN. A NaN is skipped because it is not the
!! minimum or maximum of anything -- while staying an ordinary value everywhere else in this
!! module, which is exactly the asymmetry `sort_tier_of` already encodes.
!!
!! **`pf_merge` concatenates the two inputs into one key and merges the halves.** The alternative,
!! comparing an element of `a` against an element of `b` through a second comparison path, is the
!! drift this module has spent three milestones avoiding.
submodule (parquet_sorting) parquet_sorting_reduce
    implicit none
    !
contains
    !''')

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_minmax(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        w(f"    module procedure minmax_{tag}")
        w("        integer(int64) :: i1, i2")
        w("        !")
        w(f"        call minmax_impl_{tag}(values, \"pf_minmax\", i1, i2{iv})")
        if family == "strcol":
            w("        call values%get(i1, vmin, allow_null=.true.)")
            w("        call values%get(i2, vmax, allow_null=.true.)")
        else:
            w("        vmin = values(i1)")
            w("        vmax = values(i2)")
        w(f"    end procedure minmax_{tag}")
        w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_argminmax(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        for ik, idecl, iname in IDX_KINDS:
            w(f"    module procedure argminmax_{tag}_{ik}")
            w("        integer(int64) :: i1, i2")
            w("        !")
            w(f"        call minmax_impl_{tag}(values, \"pf_argminmax\", i1, i2{iv})")
            if ik == "i32":
                w("        call narrow_i64(i1, \"pf_argminmax\", \"index of the smallest value\", imin)")
                w("        call narrow_i64(i2, \"pf_argminmax\", \"index of the largest value\", imax)")
            else:
                w("        imin = i1")
                w("        imax = i2")
            w(f"    end procedure argminmax_{tag}_{ik}")
            w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_merge(t):
            continue
        iva = ", is_valid=is_valid_a" if nulls == "arg" else ""
        ivb = ", is_valid=is_valid_b" if nulls == "arg" else ""
        w(f"    module procedure merge_{tag}")
        w("        type(sort_key_buf), allocatable :: bufa(:), bufb(:)")
        w("        integer(int64), allocatable :: perm(:)")
        w("        integer(int64) :: k, j, na, nb, n")
        w("        logical :: desc, nlo, check")
        if family == "chr":
            w("        character(len=max(len(a), len(b))), allocatable :: pa(:), pb(:)")
        w("        !")
        w("        desc = .false.")
        w("        if (present(descending)) desc = descending")
        w("        nlo = .false.")
        w("        if (present(nulls_first)) nlo = nulls_first")
        w("        check = .true.")
        w("        if (present(assume_sorted)) check = .not. assume_sorted")
        w("        na = size(a, kind=int64)")
        w("        nb = size(b, kind=int64)")
        w("        n = na + nb")
        if family == "chr":
            w("        ! Both halves are widened to one common element length before extraction, or")
            w("        ! the packed keys would compare strings of two different widths against each")
            w("        ! other. Element by element, because a whole-array assignment into an")
            w("        ! allocatable is the reallocation hazard CLAUDE.md documents.")
            w("        allocate(pa(na), pb(nb))")
            w("        do k = 1_int64, na")
            w("            pa(k) = a(k)")
            w("        end do")
            w("        do k = 1_int64, nb")
            w("            pb(k) = b(k)")
            w("        end do")
            w(f"        call extract_chr(pa, bufa, desc, nlo, \"pf_merge\"{iva})")
            w(f"        call extract_chr(pb, bufb, desc, nlo, \"pf_merge\"{ivb})")
        else:
            w(f"        call extract_{tag}(a, bufa, desc, nlo, \"pf_merge\"{iva})")
            w(f"        call extract_{tag}(b, bufb, desc, nlo, \"pf_merge\"{ivb})")
        w("        if (check) then")
        w("            call check_sorted_input(bufa, na, \"pf_merge\", \"a\")")
        w("            call check_sorted_input(bufb, nb, \"pf_merge\", \"b\")")
        w("        end if")
        w("        call buf_append(bufa, na, bufb, nb, \"pf_merge\")")
        w("        call engine_merge(bufa, n, na, \"pf_merge\", perm)")
        if family == "chr":
            w("        allocate(character(len=max(len(a), len(b))) :: merged(n))")
        else:
            w("        allocate(merged(n))")
        w("        do k = 1_int64, n")
        w("            if (perm(k) <= na) then")
        w("                merged(k) = a(perm(k))")
        w("            else")
        w("                merged(k) = b(perm(k) - na)")
        w("            end if")
        w("        end do")
        if nulls == "arg":
            w("        ! Always allocated when asked for, all .true. when neither input mask was")
            w("        ! supplied -- the same rule pf_sort's own sorted_valid follows.")
            w("        if (present(merged_valid)) then")
            w("            allocate(merged_valid(n))")
            w("            merged_valid = .true.")
            w("            do k = 1_int64, n")
            w("                j = perm(k)")
            w("                if (j <= na) then")
            w("                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)")
            w("                else")
            w("                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)")
            w("                end if")
            w("            end do")
            w("        end if")
        w(f"    end procedure merge_{tag}")
        w("    !")

    for t in TYPES:
        tag, decl, what, family, nulls, _, _ = t
        if not has_argminmax(t):
            continue
        iv = ", is_valid=is_valid" if nulls == "arg" else ""
        iarg = ", is_valid" if nulls == "arg" else ""
        w(f"    !> Shared worker behind pf_minmax and pf_argminmax for a {what} array.")
        w(f"    subroutine minmax_impl_{tag}(values, proc, imin, imax{iarg})")
        w(val_decl(t, "in"))
        w("        character(len=*), intent(in) :: proc !! calling procedure, for messages.")
        w("        integer(int64), intent(out) :: imin  !! where the smallest value is.")
        w("        integer(int64), intent(out) :: imax  !! where the largest value is.")
        if nulls == "arg":
            w("        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.")
        w("        type(sort_key_buf), allocatable :: buf(:)")
        w("        integer(int64) :: n, n_value")
        w("        !")
        w(f"        n = {rows_expr(t)}")
        w("        ! Ascending with nulls LAST, unconditionally: the population is the values, so")
        w("        ! ranks 1 and n_value address them and nothing else.")
        w(f"        call extract_{tag}(values, buf, .false., .false., proc{iv})")
        w("        call key_value_count(buf, n, n_value)")
        w("        if (n_value < 1_int64) then")
        w("            error stop EP // proc // \": every value is null or NaN, so there is no \" // &")
        w("                \"minimum or maximum; guard with count(is_valid) (or the column's own \" // &")
        w("                \"null count) if that can happen\"")
        w("        end if")
        w("        call engine_nth_index(buf, n, 1_int64, proc, imin)")
        w("        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the")
        w("        ! ascending one. Both name the same value, but a stable ascending sort puts the")
        w("        ! LAST of a tied run at the end, so rank n_value would report the last equal")
        w("        ! maximum while imin reported the first equal minimum -- the same call answering")
        w("        ! two different questions at the two ends. Flipping the key's own direction keeps")
        w("        ! both as \"rank 1\", so both report the first occurrence. Tiers are absolute, so")
        w("        ! this moves no null and no NaN out of the way of rank 1.")
        w("        buf(:)%descending = .true.")
        w("        call engine_nth_index(buf, n, 1_int64, proc, imax)")
        w(f"    end subroutine minmax_impl_{tag}")
        w("    !")

    w("end submodule parquet_sorting_reduce ! GCOVR_EXCL_LINE")
    return "\n".join(o) + "\n"


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="compare the committed files with a fresh generation; exit 1 on drift")
    ap.add_argument("--self-test", action="store_true",
                    help="verify the imported kind table still has the shape this script assumes")
    args = ap.parse_args()

    if args.self_test:
        scalar = _load_scalar_kinds()
        tags = [k[0] for k in scalar]
        if tags != EXPECTED_SCALAR_TAGS:
            print("generate_parquet_sorting.py: the columns generator's scalar kind table has "
                  "changed.\n  expected: %s\n  found:    %s\n"
                  "Update TYPES (and EXPECTED_SCALAR_TAGS) here to match, then re-run --check."
                  % (EXPECTED_SCALAR_TAGS, tags), file=sys.stderr)
            return 1
        our_tags = [t[0] for t in TYPES]
        missing = [t for t in tags if t not in our_tags and t != "str"]
        if missing:
            print("generate_parquet_sorting.py: scalar kind(s) %s are not in TYPES." % missing,
                  file=sys.stderr)
            return 1
        print("generate_parquet_sorting.py: self-test OK (%d scalar kinds imported, %d types emitted)."
              % (len(scalar), len(TYPES)))
        return 0

    outputs = {
        REPO_ROOT / "src" / "parquet_sorting.f90": gen_spec(),
        REPO_ROOT / "src" / "parquet_sorting_keys.f90": gen_keys(),
        REPO_ROOT / "src" / "parquet_sorting_argsort.f90": gen_argsort(),
        REPO_ROOT / "src" / "parquet_sorting_permute.f90": gen_permute(),
        REPO_ROOT / "src" / "parquet_sorting_select.f90": gen_select(),
        REPO_ROOT / "src" / "parquet_sorting_search.f90": gen_search(),
        REPO_ROOT / "src" / "parquet_sorting_unique.f90": gen_unique(),
        REPO_ROOT / "src" / "parquet_sorting_reduce.f90": gen_reduce(),
    }

    if args.check:
        drift = []
        for path, text in outputs.items():
            if not path.exists() or path.read_text() != text:
                drift.append(path.relative_to(REPO_ROOT))
        if drift:
            print("generate_parquet_sorting.py --check: %d file(s) differ from a fresh generation:"
                  % len(drift), file=sys.stderr)
            for d in drift:
                print("  %s" % d, file=sys.stderr)
            print("Re-run tools/generate_parquet_sorting.py to regenerate.", file=sys.stderr)
            return 1
        print("generate_parquet_sorting.py --check: %d file(s) are current." % len(outputs))
        return 0

    for path, text in outputs.items():
        path.write_text(text)
        print("wrote %s (%d lines)" % (path.relative_to(REPO_ROOT), text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
