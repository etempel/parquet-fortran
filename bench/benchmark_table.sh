#!/usr/bin/env bash
# Benchmarks the parquet_table layer against reading/writing columns directly, on one synthetic
# float64 file. It drives bench/benchmark_table.f90 through nine runs plus two sweeps -- a raw
# reader baseline, a table open+materialize_all, a lazy open reading only TOUCH of the columns, a
# slice-regime open covering one of SLICES equal row ranges, an access comparison, a write
# comparison on a null-free table, the same on one where NULLFRAC of the rows are null, a streamed
# write handing BATCHES tables to a sink, a sort run, a peak-memory pair, an argsort thread sweep
# and a grouping sweep over GROUP_GROUPS group counts. An eleventh mode, read_one, is not part of
# the sequence and is run on its own (see below).
#
# ALWAYS pass --profile release for anything measured here; the wrapper already does. See CLAUDE.md's
# "Manual (never-`fpm test`) large-scale/benchmark tools" for why, and for the FPM_FFLAGS rules.
#
# ---------------------------------------------------------------------------------------------
# What each run measures, and how to read it
# ---------------------------------------------------------------------------------------------
#
# access -- needs a fixture with at least two columns (NCOLS=2 or more) and reports two separate
#   things. First, on already-materialized columns, what %get and %col themselves cost; the columns
#   are prefetched before anything is timed, because opening is lazy and a %get on a cold column
#   would otherwise decode the whole column from the file, measuring the decode rather than the
#   copy. Second, and the reason the mode exists, the same z = x + y evaluated three ways: over
#   plain allocatable arrays, over %col pointers, and over %col pointers the caller declared
#   contiguous. Computing through a pointer is measurably slower than over arrays you own (roughly
#   1.1x on bandwidth-bound columns, ~1.7x once they fit in cache), and the contiguous row is there
#   to test the obvious explanation and refute it -- promising contiguity at the call site recovers
#   nothing, so the gap is not %col's missing stride guarantee. The actionable figure is "passes
#   before %get wins": %col skips a copy, so it is ahead until you have iterated over the same
#   columns enough times for the slower arithmetic to give that saving back.
#
# write -- materializes the table before timing anything, for the same reason the access run
#   prefetches: a parquet_write_table on a freshly opened table would decode every column as it
#   wrote it, charging the whole read to the write path, while the hand-written loop that runs
#   afterwards finds every column resident. The comparison still leans slightly towards the
#   hand-written loop, which copies each column out with %get first where parquet_write_table
#   writes from the store with no copy -- so parity in that output means the table path is
#   genuinely no more expensive.
#
# read_filtered -- what a FILTERED table costs, on both filter engines, and it needs its own
#   fixture. The default engine installs a filter by decoding every filter column over the whole
#   file in one batched pass, then reads each payload column whole and filters it; both are
#   proportional to the file however few rows survive. bounded=.true. evaluates the filter one row
#   group at a time and assembles each column from per-row-group chunks instead. The two arms are
#   run as SEPARATE PROCESSES, like read_raw and read_table and for the same reason.
#
#   IT NEEDS A --scatter FIXTURE, and a run without one measures the opposite of what it looks
#   like. Every c<n> column this program writes is monotone in the row number, so a threshold on
#   one of them is exactly the case the row-group statistics screen prunes almost entirely -- both
#   engines are then cheap because most of the file is never read, which says nothing about either.
#   write_fixture --scatter=<n> adds an int32 `key` column CYCLING 0..n-1, so every row group holds
#   the whole key range, the screen can rule nothing out, and the survivors are spread over every
#   row group. That is the case a bounded read exists for. The run prints `pruned row groups`, and
#   a figure other than 0 there means the fixture or the --scatter value was wrong.
#
#   It also needs enough ROW GROUPS to be meaningful: the writer's auto-sizing gives a few very
#   large ones, so FILTER_CHUNK forces a row-group size. With two row groups "one row group at a
#   time" is half the file.
#
#   Read the TIME as the regression guard for the default engine -- it is the only run here that
#   covers a filtered table at all. Read RSS as the memory result: the Arrow pool figures printed
#   beside it are what is RESIDENT when they are read, not the peak, and parquet_open_table
#   releases every column it cached before it returns, so the two engines' pool figures are much
#   closer than their peaks are. For the peak, wrap the run in /usr/bin/time (-l on macOS, -v on
#   Linux), one arm per process.
#
# write_nulls -- the counterpart to write. write measures the null-free path, where a column with
#   no nulls is handed to the writer with no validity mask at all; write_nulls measures the case
#   that shortcut cannot help, and splits it three ways: parquet_write_table, a hand-written loop
#   building its mask a row at a time through %is_null, and the same loop handed a finished mask.
#   The third is the floor -- writing with nulls and nothing else -- so the gap above it is mask
#   construction, and parquet_write_table should sit at that floor because it walks the validity
#   bitmap a word at a time. The per-row line is legitimately much slower: %is_null takes a column
#   name, so every call repeats a lookup the library does once, and the public API offers no way to
#   hoist it. It builds its null-carrying input itself (an untimed extra write plus read) rather
#   than asking the fixture writer for one, because parquet_table exposes no way to mark a row null
#   in memory.
#
# write_stream -- the streamed counterpart to write, and the only run that measures the
#   parquet_table_writer sink. Three arms over the same BATCHES tables: the sink; the hand-written
#   buffer loop it replaces (%clone_structure, %append into it, parquet_write_table_chunk at the
#   threshold, empty and re-reserve); and that same loop with every column declared
#   protected_cols:. The first two should be at PARITY -- the sink is the second loop with its
#   bookkeeping inside -- and a slower sink means the re-reserve after a flush regressed, so every
#   append after the first row group is growing the buffer again. The third arm is the same write
#   with no validity mask, so the gap above it is what the always-present mask costs: a streamed
#   table write passes a mask for every column it can, because a streamed column's nullability is
#   fixed by its first row group. That is the figure doc/pages/tables/table-write.md sends the
#   reader here for.
#
#   READ ITS CONTROL FIRST, for the reason read_one's control exists: the second and third arms run
#   identical Fortran over identical rows, so "the mask is free" and "%set_protected never reached
#   the writer" produce the same 1.00 ratio. The run prints all three outputs' stored nullability --
#   the sink's and the masked arm's must be T and the protected arm's F -- and says outright that a
#   run where they do not means nothing.
#
# sort -- the only run that touches no file: it builds a table of SORT_SIZE_GB worth of float64
#   columns plus one character column IN MEMORY, because what it measures is the cost of reordering
#   an already-resident table, and reading a fixture first would only add a decode to both sides. It
#   splits %sort_by into its two halves -- the permutation build (pf_argsort, in C++) and the
#   per-column reindex loop -- which is the split worth watching, because on a many-core machine the
#   second dominates: when the loop was still serial it measured 13.1 s against the permutation's
#   1.9 s on a 100+ core server. Both halves are threaded now (the loop runs one column per thread,
#   gated), so that ratio is the historical motivation rather than what a run today reports.
#
#   It then measures the reindex phase TWO ways, all columns validating the permutation against only
#   the first one doing so. That comparison is also the only thing that would notice if
#   %reindex_trusted silently stopped differing from %reindex -- the two figures would simply
#   coincide, with every test still passing. The trailing "reference" block prices the two seen-set
#   representations against each other; logical is what reindex used before the bit-packed set
#   replaced it, so those lines say what that change was worth rather than what is still available.
#
# argsort -- also file-free, and answers a different question from sort: there the permutation build
#   is a minority of %sort_by, but for a caller of raw-array pf_sort/pf_argsort it IS the whole
#   operation. It runs pf_argsort at each of ARGSORT_THREADS over each of ARGSORT_NROWS, splitting
#   the result into the per-chunk std::sorts and the merge that follows, and reporting the merge's
#   last round separately -- that round is where a pairwise merge collapsed to a single thread, so
#   it is the one the co-ranked merge exists to fix.
#
#   Each run measures BOTH merges, back to back in one process: once with the minimum segment size
#   forced above the whole array, which leaves every pair unsegmented and so reproduces the pairwise
#   merge exactly, and once with co-ranking in force. That is deliberate rather than convenient -- an
#   earlier version compared a co-ranked build against a pairwise one measured on a different day,
#   and the serial baseline alone had drifted 15% in between, which is larger than some of the
#   effects being reported. Any before/after claim about this phase should come from one process,
#   not two runs. Keep 1 first in ARGSORT_THREADS: it is the serial baseline, and it reports no
#   phases at all, because the engine takes the plain std::sort path rather than chunking.
#
# peakmem -- answers a question none of the runs above can, and the reason it needs its own mode is
#   worth knowing before anyone tries to fold it back into sort. A parallel row-structural mutation
#   holds one transient column copy per thread instead of one in total; the library's answer to that
#   is documentation plus the parquet_set_table_threads cap rather than a memory-derived limit, so
#   the "at most doubles the table's peak" claim has to be measured rather than asserted. sort
#   CANNOT measure it -- it builds a SECOND, standalone set of columns in order to time the reindex
#   phase in isolation, and that second set, not the mutation, is what sets its process peak. Three
#   different machines reported an RSS figure from that mode and all three had to discard it.
#   --mode=peakmem builds one table, sorts it exactly once, and allocates nothing else.
#
#   The answer is the DIFFERENCE between the run's two points, PEAKMEM_THREADS="1 0" (serial, then
#   automatic). Both build the identical table, so whatever separates their peaks is the mutation's
#   transient, and the mode prints the predicted value -- (T-1) copies of one column -- beside it.
#   The peak is read with /usr/bin/time wrapped around the benchmark binary via `fpm run --runner`,
#   not around fpm, whose own compile and link peaks would dominate; the mode's two in-process RSS
#   lines are context, not the answer, because the transient is gone before the program regains
#   control. On an 8-core machine the transient measures about 86% of (T-1) copies, the shortfall
#   being that the copies are not simultaneous -- each lives only between its column's allocation
#   and its move_alloc, and schedule(dynamic) staggers when columns finish.
#
# group -- the grouping verbs, each against the composition it replaces, and file-free for the
#   reason sort is: what it measures is the cost of partitioning and walking an already-resident
#   table. It runs once per GROUP_GROUPS entry over a GROUP_NROWS-row table, and each run reports
#   four things.
#
#   First, what the OBJECT costs above the partition: %group_by against a bare
#   %argsort_by(keys, perm, group_offsets=), which is the same sort without the object, so the
#   difference is the dropna pass and two array copies and nothing else.
#
#   Second, %agg("mean") against the two compositions that give the same answer -- %gather into
#   one caller-owned buffer plus pf_mean, and %rows + %get_slice + pf_mean, which allocates twice
#   per group. All three run SERIAL (%agg is called with threads=1) so that the comparison is
#   like for like. This is the row that needs the sweep, and the ratio is not monotone in the
#   group count: both sides carry a per-group cost -- the composition's two allocations, and the
#   statistics module's own per-call entry work, which %agg pays once per group too -- so the
#   sweep is how you see which of the two you are paying at your own group size.
#
#   Third, the %apply thread ladder: a trivial module-procedure callback with threads= absent
#   (SERIAL by contract) and then at 1, 2, 4 and 8. READ THE RECORDED TEAM BESIDE EACH RUNG, for
#   read_one's reason -- "the ladder does not scale" and "no team ever opened" print the same
#   times, and only the recorded team separates them. A rung whose team is below its request was
#   clamped to this process's CPU affinity, so on a 4-core machine the 8 rung is a 4-thread run.
#   The callback is trivial deliberately: a heavier one would flatter every rung.
#
#   Fourth, %broadcast per row against the %group_ids lookup a caller writes instead.
#
#   The run ends with CHECKSUMS, and they are not decoration: the three mean arms and the two
#   broadcast arms compute the same numbers, so a run whose sums differ measured different work
#   and its ratios mean nothing. The mode says so itself when they disagree.
#
# lazy / slice -- read these against read_table: the open figure shows what an open costs when it
#   reads nothing, and the two partial modes show that a program pays only for the columns and rows
#   it asks for. A slice cannot be cheaper than one row group, so a fixture written with a single
#   row group will show no slice saving -- the mode says so in its own output.
#
# raw baseline -- reads ONE ARRAY PER COLUMN and holds them all at once, matching what a table
#   holds. Reusing a single buffer for every column instead would measure a different job and
#   flatter the raw path -- the same pages get overwritten and stay warm, where the table touches
#   the whole file's worth of distinct memory (worth ~15% of the raw-vs-table gap on a 0.4 GB
#   8-column file).
#
# ---------------------------------------------------------------------------------------------
# The number worth watching: "Arrow pool still holding", NOT RSS
# ---------------------------------------------------------------------------------------------
#
# In the two read sections, watch "Arrow pool still holding". parquet_table keeps its own Fortran
# copy of every column and releases the reader's decoded Arrow buffers as it goes, so a fully
# materialized table should report ~0 MiB there while the raw baseline reports the whole file (on
# top of its own Fortran copies -- roughly two copies resident). Roughly two copies for the table
# means the release stopped happening.
#
# RESIDENT SET SIZE CANNOT ANSWER THIS QUESTION. Arrow's memory pool keeps freed pages rather than
# returning them to the OS, so RSS stays high in both cases; the pool's own bytes_allocated() is
# what distinguishes "released" from "still alive". The two read modes also run as separate
# processes for the same reason. See CLAUDE.md's "Measuring whether Arrow memory was actually
# freed: RSS cannot answer, the pool counter can".
#
# ---------------------------------------------------------------------------------------------
# Usage
# ---------------------------------------------------------------------------------------------
#   bench/benchmark_table.sh
#   TARGET_FILE_SIZE_GB=2.0 NCOLS=16 bench/benchmark_table.sh   # bigger file, more columns
#   NCOLS=32 TOUCH=2 SLICES=8 bench/benchmark_table.sh          # 2 of 32 columns, an eighth of rows
#   TEST_FILE=/tmp/bench.parquet bench/benchmark_table.sh       # keep the synthetic file
#   SORT_SIZE_GB=3 NCOLS=24 bench/benchmark_table.sh            # sort run is sized on its own
#   ARGSORT_NROWS="1000000 20000000" ARGSORT_THREADS="1 2 4 8 16 32 64" bench/benchmark_table.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   TARGET_FILE_SIZE_GB=0.25  Approximate uncompressed (in-memory) size of the test file.
#   NCOLS=8                   Number of float64 columns in the test file. Minimum 2 -- the access
#                              run compares `z = x + y` across access paths and needs two columns.
#   TOUCH=2                   Columns the lazy-read mode actually reads, out of NCOLS.
#   SLICES=4                  Equal row slices to divide the file into for the slice mode.
#   NULLFRAC=0.1              Fraction of rows the write_nulls run marks null (0 < f < 1).
#   BATCHES=32                Tables the write_stream run hands to the sink, one %append each.
#                             The fixture is cut into that many equal row ranges.
#   STREAM_CHUNK=0            Rows per row group every write_stream arm uses. 0, the default,
#                             takes a quarter of the fixture's rows, so every arm crosses three
#                             flush boundaries; the sink's own estimate is larger than any fixture
#                             this script writes, and taking it would measure a flush that never
#                             ran.
#   FILTER_SELECT=0.01        Fraction of rows the read_filtered run's filter keeps.
#   FILTER_SCATTER=1000       Period of that run's fixture `key` column. The filter threshold is
#                             sized from it, and its whole job is that every row group holds the
#                             full 0..n-1 range so the statistics screen prunes nothing.
#   FILTER_CHUNK=50000        Rows per row group in that run's fixture. Auto-sizing gives a
#                             handful of huge row groups, which makes "one row group at a time"
#                             mean almost nothing.
#   SORT_SIZE_GB=1            Size of the IN-MEMORY table the sort run builds. That run needs no
#                              file fixture, so it is sized independently of TARGET_FILE_SIZE_GB --
#                              the cost it measures grows with rows AND with columns, and the
#                              default file size is too small to separate the two.
#   ARGSORT_NROWS="1000000 20000000"
#                             Row counts the argsort thread sweep runs at. Also file-free.
#   ARGSORT_THREADS="1 2 4 8" Thread counts the argsort sweep runs at. 1 is the serial baseline
#                              every speedup below is measured against, so keep it first.
#   GROUP_NROWS=20000000      Rows in the in-memory table the group sweep builds. File-free, so
#                              it is sized independently of TARGET_FILE_SIZE_GB.
#   GROUP_GROUPS="10 1000 100000"
#                             Distinct key values the group sweep runs at, one process each. The
#                              spread is the point: the per-group overhead of the composition
#                              %agg replaces is invisible at the first and dominant at the last.
#   PEAKMEM_THREADS="1 0"     Table-mutation thread caps the peak-memory runs use (1 = serial,
#                              0 = automatic). The answer is the DIFFERENCE between the two peaks,
#                              so both are needed and 1 must come first.
#   TEST_FILE                 Path for the synthetic test file. Default: a fresh mktemp -d
#                              directory, deleted automatically when the script exits. Set this
#                              to keep the file around afterward -- it is NOT deleted when
#                              explicitly set, and its parent directory is created if needed.
#
# ---------------------------------------------------------------------------------------------
# read_one: run on its own, and read its CONTROL before its timings
# ---------------------------------------------------------------------------------------------
#
# read_one times reading a single whole column with Arrow's own use_threads on and off, which
# answers a question no other mode does: whether Arrow already parallelises a single-column decode
# internally. On an 8-core machine it does not -- 0.96x-1.02x across two column sizes -- which is
# why splitting one column's read across row groups is still worth doing.
#
# ITS MOST IMPORTANT LINE IS THE CONTROL, NOT THE TIMINGS. "The two arms take the same time" and
# "the flag never reached the reader" produce identical output, so the mode prints the use_threads
# value each arm's reader actually resolved to (1 and 0) and says outright that the timings mean
# nothing if those match. Copy that shape for any future A/B benchmark whose expected result is NO
# DIFFERENCE: without a control, such a benchmark passes just as happily when it is measuring one
# configuration against itself. The mode also uses a fresh reader per timed read (a reader caches
# its decoded column, so a second read on one reader times a cache hit), one untimed warm-up read so
# both arms see the same page-cache state, and alternating arms within each round.
#
#   fpm run benchmark_table --profile release -- \
#       --mode=write_fixture --file=/tmp/pf_bench.parquet --size=4.0 --ncols=24
#   fpm run benchmark_table --profile release -- --mode=read_one --file=/tmp/pf_bench.parquet
#
# The peak-memory pair can also be run on its own, which is usually what you want -- it is the only
# part of the script whose answer is a difference between two processes, and the rest of the script
# does not have to run for that difference to mean anything. Use a narrow table to make it bite:
# the bound is tightest when the thread count approaches the column count.
#
#   for t in 1 0; do
#       fpm run benchmark_table --profile release --runner "/usr/bin/time -l" -- \
#           --mode=peakmem --size=4.0 --ncols=24 --threads="$t"
#   done
#   # GNU time (Linux) reports the same figure under -v rather than -l.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

TARGET_FILE_SIZE_GB="${TARGET_FILE_SIZE_GB:-0.25}"
NCOLS="${NCOLS:-8}"
TOUCH="${TOUCH:-2}"
SLICES="${SLICES:-4}"
NULLFRAC="${NULLFRAC:-0.1}"
BATCHES="${BATCHES:-32}"
STREAM_CHUNK="${STREAM_CHUNK:-0}"
FILTER_SELECT="${FILTER_SELECT:-0.01}"
FILTER_SCATTER="${FILTER_SCATTER:-1000}"
FILTER_CHUNK="${FILTER_CHUNK:-50000}"
SORT_SIZE_GB="${SORT_SIZE_GB:-1}"
ARGSORT_NROWS="${ARGSORT_NROWS:-1000000 20000000}"
ARGSORT_THREADS="${ARGSORT_THREADS:-1 2 4 8}"
GROUP_NROWS="${GROUP_NROWS:-20000000}"
GROUP_GROUPS="${GROUP_GROUPS:-10 1000 100000}"
PEAKMEM_THREADS="${PEAKMEM_THREADS:-1 0}"
TEST_FILE="${TEST_FILE:-}"

cleanup_dir=""
if [[ -z "$TEST_FILE" ]]; then
    cleanup_dir="$(mktemp -d)"
    TEST_FILE="$cleanup_dir/benchmark_table.parquet"
    trap 'rm -rf "$cleanup_dir" benchmark_table_out1.parquet benchmark_table_out2.parquet benchmark_table_out3.parquet benchmark_table_nulls.parquet' EXIT
else
    mkdir -p "$(dirname "$TEST_FILE")"
    trap 'rm -f benchmark_table_out1.parquet benchmark_table_out2.parquet benchmark_table_out3.parquet benchmark_table_nulls.parquet' EXIT
fi

echo "=== parquet_table benchmark ==="
echo "file        : $TEST_FILE"
echo "target size : ${TARGET_FILE_SIZE_GB} GB uncompressed"
echo "columns     : ${NCOLS} x float64"
echo

fpm run benchmark_table --profile release -- \
    --mode=write_fixture --file="$TEST_FILE" --size="$TARGET_FILE_SIZE_GB" --ncols="$NCOLS"
echo

# Each read mode runs in its OWN process: malloc does not reliably return freed pages to the
# OS, so measuring both in one process would report the pair's high-water mark and understate
# the table's release.
fpm run benchmark_table --profile release -- --mode=read_raw --file="$TEST_FILE"
echo

fpm run benchmark_table --profile release -- --mode=read_table --file="$TEST_FILE"
echo

# Opening is lazy, so these two are the point of the whole layer: what an open costs when it
# reads nothing, and what a program that wants only part of the file actually pays.
fpm run benchmark_table --profile release -- --mode=read_lazy --file="$TEST_FILE" --touch="$TOUCH"
echo

fpm run benchmark_table --profile release -- --mode=read_slice --file="$TEST_FILE" --slices="$SLICES"
echo

fpm run benchmark_table --profile release -- --mode=access --file="$TEST_FILE"
echo

# The filtered read, on both engines. Its own fixture, because it needs a column the row-group
# statistics screen cannot prune on (see this script's header) and enough row groups for "one row
# group at a time" to differ from "the whole column"; and its own process per arm, because peak
# memory is what separates them.
FILTER_FILE="${TEST_FILE%.parquet}_filtered.parquet"
fpm run benchmark_table --profile release -- \
    --mode=write_fixture --file="$FILTER_FILE" --size="$TARGET_FILE_SIZE_GB" --ncols="$NCOLS" \
    --scatter="$FILTER_SCATTER" --chunk="$FILTER_CHUNK"
echo

for b in 0 1; do
    fpm run benchmark_table --profile release -- --mode=read_filtered --file="$FILTER_FILE" \
        --scatter="$FILTER_SCATTER" --select="$FILTER_SELECT" --bounded="$b"
    echo
done

fpm run benchmark_table --profile release -- --mode=write --file="$TEST_FILE"
echo

# The write run above measures the null-free path, where the writer is handed no validity mask at
# all. This one measures what a mask actually costs, which is the case the shortcut cannot help.
fpm run benchmark_table --profile release -- --mode=write_nulls --file="$TEST_FILE" --nullfrac="$NULLFRAC"
echo

# Both runs above write the whole table in one call. This one writes it a row group at a time, and
# compares the sink against the buffer loop it replaces (parity expected) and against the same loop
# with no validity mask (which is what protected_cols: buys). Read its CONTROL line first.
fpm run benchmark_table --profile release -- --mode=write_stream --file="$TEST_FILE" \
    --batches="$BATCHES" --chunk="$STREAM_CHUNK"
echo

# The one run that uses no file at all: it builds its table in memory, because what it measures is
# the cost of REORDERING a resident table (%sort_by's permutation build against its per-column
# reindex), and reading a fixture first would only add a decode to both sides.
fpm run benchmark_table --profile release -- --mode=sort --size="$SORT_SIZE_GB" --ncols="$NCOLS"
echo

# The run above measures %sort_by, where the permutation build is a minority of the cost. This one
# measures the permutation build ALONE -- which is the whole operation for a caller of raw-array
# pf_sort/pf_argsort -- and splits it into the per-chunk sorts and the pairwise merge that follows
# them. The merge is done in log2(T) rounds with T/2, T/4, ..., 1 threads, so its LAST round is a
# single-threaded pass over the whole array whose share does not shrink as threads are added. That
# share is what a co-ranked parallel merge would remove, and reading it off is why this sweep
# exists (feature_sort_merge.md step 0). Also file-free: it sorts an array it generates itself.
# A parallel row-structural mutation holds one transient column copy per thread instead of one in
# total, and the library's answer to that is documentation plus the parquet_set_table_threads cap
# rather than a memory-derived limit -- so the "at most doubles the table's peak" claim needs
# measuring rather than asserting. The --mode=sort run above CANNOT measure it: it builds a second,
# standalone set of columns to time the reindex phase in isolation, and that second set, not the
# mutation, is what sets its process peak. --mode=peakmem builds nothing the sort does not need.
#
# The answer is the DIFFERENCE between the two runs below. Both build the identical table, so
# whatever separates their peaks is the mutation's transient, and the mode prints the predicted
# value -- (T-1) copies of one column -- next to it. /usr/bin/time is the measurement: the
# transient is gone by the time the program regains control, so no in-process reading can see it.
# --runner puts the timer around the benchmark binary rather than around fpm, whose own compile and
# link peaks would otherwise dominate.
if [[ "$(uname -s)" == "Darwin" ]]; then
    TIME_FLAG="-l"        # macOS: reports "maximum resident set size" in BYTES
else
    TIME_FLAG="-v"        # GNU time: reports "Maximum resident set size (kbytes)"
fi

echo "=== %sort_by peak memory: serial vs automatic threads ==="
for pt in $PEAKMEM_THREADS; do
    fpm run benchmark_table --profile release --runner "/usr/bin/time $TIME_FLAG" -- \
        --mode=peakmem --size="$SORT_SIZE_GB" --ncols="$NCOLS" --threads="$pt" 2>&1 |
        grep -Ei "in-memory table|--threads=|threads the mutation|sort_by \(one run\)|one column|predicted transient|current RSS|maximum resident set size"
    echo
done

echo "=== pf_argsort thread sweep (permutation build, by phase) ==="
for nr in $ARGSORT_NROWS; do
    for t in $ARGSORT_THREADS; do
        fpm run benchmark_table --profile release -- --mode=argsort --nrows="$nr" --threads="$t"
        echo
    done
done

# The grouping verbs, each against the composition it replaces. Also file-free, and swept over
# group counts rather than row counts: what changes with the number of groups is how much
# per-group overhead the composition carries, which is the whole question %agg answers. Read each
# run's recorded teams and its checksums before its ratios (see this script's header).
echo "=== grouping: the object, %agg, the %apply ladder and %broadcast ==="
for ng in $GROUP_GROUPS; do
    fpm run benchmark_table --profile release -- --mode=group --nrows="$GROUP_NROWS" --groups="$ng"
    echo
done
