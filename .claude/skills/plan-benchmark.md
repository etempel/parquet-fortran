---
name: plan-benchmark
description: "Write the run sheet for a cross-machine benchmarking campaign — feature_benchmark_<name>.md in the repository root plus one copy per machine — from the template this skill carries. Runs no benchmark."
argument-hint: "<campaign name and the question it answers>"
allowed-tools: Bash(git:*), Bash(grep:*), Bash(ls:*), Bash(cat:*), Bash(sed:*), Bash(find:*)
disable-model-invocation: true
---

# /plan-benchmark — write a cross-machine run sheet

**This skill writes the instructions for a measurement campaign and stops. It runs no benchmark.**
Measurement rules are in `.claude/rules/benchmarking.md`; machine facts and activation commands in
`.claude/rules/developer-environments.md` — read it first. The request: $ARGUMENTS

Injected context:

- Working tree: !`git status --short | head -10`
- Latest commits: !`git log --oneline -3`
- Existing run sheets: !`ls feature_benchmark*.md 2>/dev/null || echo none`

## 1 What the writer does

- Name the file `feature_benchmark_<campaign>.md` (for the campaign, not a machine), in the
  repository root; git-ignored by the `feature_*.md` rule. If it exists, stop and report. **A new
  campaign gets new filenames, and a RETURNED copy is never overwritten**: it records which commit
  that machine ran, under which environment, with which deviations.
- Fill §1 of the sheet: the question, the commit(s), the command, the duration, what decides the
  outcome.
- **Commit and push every code change the run needs**; the receiving machine has the repository,
  not your working tree. For a before/after comparison push both commits and name both hashes.
  Never ask a runner to patch or `git stash`.
- Fill §2 Step 2 with each machine's activation from `developer-environments.md`; say so when it is
  unknown.
- **Say what size of effect the campaign is looking for**, so the runner can tell from the noise
  floor whether the machine can answer it at all.
- **An anchor figure the run must reproduce names its exact configuration** (size, width, column,
  mode); an under-specified anchor stops the wrong runs.
- **State each pass criterion in the shape the mechanism produces** (a bisection is logarithmic,
  not flat).
- **Name every axis the build tree must be keyed on**: configuration, compiler, and every flag the
  campaign toggles.
- **Number every measurement step before any teardown step** (restoring scaffolded source, deleting
  a branch).
- **Say what a runner does if the commit or branch does not exist**: stop, or proceed on `main`
  and record what.
- **Show how each prediction was derived, not only its value**; a prediction chained from
  removal-based estimates is fragile.
- **A step that confirms the MECHANISM is REQUIRED, not optional**; after a fix, re-run the
  diagnostic that found the problem, not only the end-to-end measurement.
- **More than one toolchain: ask for provenance per toolchain, from inside each activated shell.**
- Delete the report-skeleton fields that do not apply.
- Make one copy per machine, suffixed with the machine's name (`feature_benchmark_<campaign>_A.md`,
  `_B`, `_C`), and carry each to its machine by hand (email, scp, paste); it is not in git.
  Machines run independently and may run concurrently; each writes only into its own copy, and the
  analysis happens once, when every copy is back.

## 2 The run sheet to emit

Write the following into the file, filled in, keeping the runner's sections verbatim apart from
the fields.

### §1 The campaign

| | |
|---|---|
| **Question being answered** | |
| **Commit(s) to measure** | both pushed |
| **Command to run** | |
| **Roughly how long** | |
| **What decides the outcome** | |
| **Effect size sought** | |

### §2 Steps for the runner

**Step 1 — get the code.** `git pull` in the repository; `git log --oneline -1` must match §1, else
stop. If this run is compared against an earlier run at a different commit, record
`git diff --stat <earlier>..<this> -- app src tools` and its reading: nothing measured changed (the
runs are comparable) or something in `app/`, `src/` or the wrapper changed (the earlier figures are
not a baseline). Clear any tree an earlier campaign left: `rm -rf test_run/<campaign-prefix>-*`.

**Step 2 — activate this machine's environment, and verify it took.** Record the exact activation
commands in the report. Then run `tools/machine_report.sh` **inside the activated shell, once per
toolchain**; it describes the environment it is invoked from, and a run before activation reports a
default environment nothing is measured in (if its `fc`/`cc`/Arrow lines disagree with the `FPM_*`
flags printed beside them, the flags are the ones that built the binary). Check: the Fortran
compiler and version intended (minimum gfortran 13; `-dumpversion` truncates to `14`); `fpm` found
and its `fc`/`cc` lines naming the expected compilers (fpm derives C/C++ from the Fortran family);
Arrow and Parquet found by `pkg-config`. An activation script may not survive non-interactive
sourcing, and a sourced script inherits the caller's positional parameters (`set --` first); assert
`FPM_FC` is the toolchain asked for and `FPM_CXXFLAGS` is non-empty. Then verify optimisation:
`fpm build --profile release --show-model | grep -o 'fortran_compile_flags="[^"]*"'` must show
`-O` (fpm has no release profile for flang); if absent, stop, or append
`FPM_FFLAGS="${FPM_FFLAGS:-} -O3"` and record the deviation. Optional for LTO:
`tools/machine_report.sh --lto-probe`, recording passes and failures; a pass proves the compilers
can consume LTO objects, never that the library's static archive links that way.

**Step 3 — run it.** Exactly the §1 command, from the repository root, on an otherwise idle
machine: `<command> 2>&1 | tee test_run/<campaign>-run.log`. One run per toolchain, each in a
fresh shell. `ls -l` the logs after the first rung: a driver that dies before its first line looks
like a run that found nothing. Let the machine settle between runs and say so; the threshold is a
fraction of the core count and the wait is bounded, never an absolute load and never unbounded:

```bash
settle() {
    local ncpu deadline load
    ncpu=$( (nproc 2>/dev/null || sysctl -n hw.logicalcpu) )
    deadline=$(( SECONDS + 120 ))
    while [ "$SECONDS" -lt "$deadline" ]; do
        load=$( (awk '{print $1}' /proc/loadavg 2>/dev/null || sysctl -n vm.loadavg | awk '{print $2}') )
        awk -v l="$load" -v n="$ncpu" 'BEGIN{exit !(l < n/4)}' && break
        sleep 20
    done
    sleep 10
    echo "load1 at run start: $load" >> test_run/<campaign>/load.log
}
```

Record `load.log`. "Loaded" is not "noisy": measure the floor and let it decide, but say which arms
ran under which load, because an A/B whose arms saw different external load cannot be compared
across toolchains. A workstation whose idle load never drops may substitute a fixed sleep; record it
as a deviation.

**Step 3b — measure this machine's noise floor (required).** Re-run the identical configuration a
second time, back to back (`<command, reduced> 2>&1 | tee test_run/<campaign>-noise.log`), and
report the largest relative difference between the two runs of the identical build. Any campaign
difference smaller than it is "indistinguishable from zero", never a small win or regression. If the
whole question sits below the floor, that is the finding.

**Step 3c — if any two arms come from different BUILDS, measure a cross-build floor too.** A
rebuild (cpp variant, flag, LTO, mutation) moves untouched arms by code layout, an order of
magnitude more than re-running one binary. Rules: include an untouched control arm in every build;
do not take the control's absolute spread as the floor for a larger arm (layout noise scales with
the arm) — express it as a percentage or take it from the largest untouched arm; a rung that
removes work and comes out slower has measured layout alone and is a direct floor on the arm that
matters; build each variant at least twice where the answer is close, quoting both floors; measure
it for this campaign and never carry one forward (it is a property of how far apart the two builds
are, so pose the question as a two-commit A/B rather than a flag ladder wherever possible). A flat
control still proves the flag did what it claimed; when the controls are very quiet, the governing
floor is the run-to-run spread of the arm under study, and the report says which governs.

**Step 4 — write the report into this file**, under the name it was given, without removing the
instructions or renaming it, then send it back.

### §3 Rules that apply to every run

- All output goes under `test_run/<campaign>/` (build trees, logs, fixtures); a benchmark tool never
  needs a new `.gitignore` entry. Clean up with `rm -rf test_run/<campaign>-*`.
- A fresh build tree re-fetches dependencies (`test-drive`), so the first run needs network; report
  a failure there rather than working around it.
- Never assign `FPM_FFLAGS`, `FPM_CXXFLAGS` or `FPM_LDFLAGS`; append
  (`FPM_FFLAGS="${FPM_FFLAGS:-} -newflag"`).
- Always build through the campaign's wrapper, never a bare `fpm run`: without `--profile release`
  every number is meaningless.
- A failure is a result: record it with the error text and stop; never retry with the flag removed
  and report that number.
- A deviation is allowed but must be labelled; an unreported deviation is a fabricated result.
- The machine must be otherwise idle, and the report says whether it was; a large NUMA machine is a
  poor instrument for small differences and best for scaling questions.
- Do not disqualify a loaded machine without measuring its floor.
- If the harness has two differently shaped modes, run both: a real change moves both in the same
  direction; a sign disagreement is unresolvable and may be measuring the fixture.
- Say whether a variant changed the ANSWER as well as the timing: identical checksum means pure
  overhead; a moved checksum means timings-only.
- A noise floor measured on a proxy is evidence about the proxy; measure it with the campaign's own
  harness, and never quote an assumed one ("best of N" removes spikes, not drift or ordered bias).
- A tooling warning that would qualify the numbers is a claim; spend one short run checking it.
- Do not compare figures across machines unless the campaign says they are comparable.
- Keep the provenance with the numbers: compiler, version, architecture, commit.

### §4 Running on several machines

Do not rename your copy or strip its instructions; do not add another machine's numbers to it.

### §5 Report skeleton

A run is recorded whatever it concluded; an empty field is written as "none".

```
## Report
### <machine name> on <YYYY-MM-DD> (commit <hash>)
Command run: <exact>. Machine otherwise idle: yes / no. Best of N rounds per figure.
**Environment activation used**: <commands, or "none needed">
**Provenance** (tools/machine_report.sh inside each activated shell): host/OS; CPU physical+logical;
  architecture/SIMD; RAM; Fortran compiler+version; C++ compiler+version (the one fpm derived);
  Arrow/Parquet version; fpm version; commit, clean or dirty
**Did it build and pass?** build / fpm test / error scenarios — before any timing. A scaffolded
  campaign runs the suite on the NO-MACRO build.
**What changed since the commit an earlier run used**: the diff --stat command, its output, the reading
  (or "n/a")
**Deviations from the run sheet**: <or "none">
**Measured noise floor** (Step 3b): the figure, what it was taken on, the resulting reading rule
**Cross-build noise floor** (Step 3c, or "n/a, single binary"): how far the untouched control moved
**Results**: the tool's own output verbatim in a fenced block, then a short paragraph
**Anything surprising**: toolchain mismatches, out-of-proportion figures, steps that did not work
**What this run does not settle**: machines, compilers or questions still outstanding
```
