# Benchmarking / testing run — instruction template

**This is a TEMPLATE, not a run.** Copy it to a file named `feature_<something>.md` in the
repository root, fill in the marked places, and hand that copy to whoever will run it. Do not edit
this template with the details of one campaign.

Why that filename: `feature_*.md` is git-ignored in this repository, which is what a per-campaign
instruction file should be — it is working material, it is carried between machines by hand, and it
must never end up in the published package. **Everything a run needs to EXECUTE goes in the
repository instead** (a program under `app/`, a wrapper under `tools/`), so it reaches every machine
through `git pull` and no one has to copy code around or keep versions in step by hand.

**How a campaign flows:**

1. **One instructions file is prepared** for the campaign — `feature_<campaign>.md`, named for the
   question being answered rather than for any machine.
2. **Each machine gets its own copy**, suffixed with the machine's name:
   `feature_<campaign>_A.md`, `feature_<campaign>_B.md`, … Hand that copy to the machine.
3. **The machine writes its report into its own copy** — into the file it was given, not a new one —
   and sends that file back.
4. **When every copy is back, they are analysed together** and the decision is made once, with all
   the figures in view.

So each returned file contains the same instructions plus **one** report. That is why the report
skeleton in §5 insists on full provenance even when it feels redundant: at analysis time the reports
are read side by side, and a figure whose compiler, architecture and commit are not attached to it
cannot be placed against the others.

A machine is not expected to compare itself against any other — it reports its own numbers cleanly
and flags anything surprising, and the attribution happens at analysis time. (If cross-machine
attribution *is* wanted from a runner, hand them a previously completed copy alongside their own;
say so explicitly in §1 when you do.)

---

## 0. For the person WRITING the instructions — fill this in, then delete this section

- [ ] Name the file `feature_<campaign>.md` (for the campaign, not a machine) in the repository root.
- [ ] Fill in **§1 The campaign**: what question is being answered, which commit, which command.
- [ ] **Commit and push every code change the run needs** — the receiving machine has the
      repository, not your working tree. For a before/after comparison push *both* commits and give
      both hashes below; never ask anyone to patch or `git stash` on the far machine.
- [ ] Fill in **§2** with any environment activation this machine needs, if you know it. If you do
      not, say so — §2 tells the runner how to find out and record it.
- [ ] Delete this section, and delete any §5 report skeleton fields that do not apply.
- [ ] Make one copy per machine, suffixed with the machine's name
      (`feature_<campaign>_A.md`, `feature_<campaign>_B.md`, …), and carry each to its machine
      (email, scp, paste — it is small and it is not in git).

---

## 1. The campaign

*(fill in)*

| | |
|---|---|
| **Question being answered** | *e.g. "does splitting the range check out of the conversion loop pay on this target?"* |
| **Commit(s) to measure** | *e.g. `abc1234` (baseline) and `def5678` (change) — both pushed* |
| **Command to run** | *e.g. `tools/benchmark_stage7.sh --both --test`* |
| **Roughly how long** | *e.g. a few minutes per build configuration; `--test` adds the suite twice* |
| **What decides the outcome** | *e.g. "if the fused column wins at every size, the item is dropped"* |

---

## 2. For the person RUNNING it — steps

### Step 1 — get the code

```bash
cd <path-to>/parquet-fortran
git pull
git log --oneline -1        # must match the commit named in §1
```

If it does not match, stop — a number measured at the wrong commit cannot be compared with anything.

**If this campaign will be compared against an earlier run taken at a DIFFERENT commit** (a re-run
after a fix, or a machine measured later than the others), check what actually changed in between,
and record it:

```bash
git diff --stat <earlier-commit>..<this-commit> -- app src tools
```

Two outcomes, and they are not the same:

- **Nothing in the measured code changed** — only documentation, comments, or printed text. Say so,
  naming the command you ran, and the two runs are directly comparable despite the different hashes.
- **Something in `app/`, `src/` or the campaign's wrapper changed.** Then the earlier figures are
  *not* a baseline for these ones; say what changed and leave the comparison to analysis time.

This costs one command and it is the difference between a comparison that means something and one
that quietly does not. A machine that skipped it once would have had its numbers silently filed
against a different build.

### Step 2 — activate this machine's environment

Some machines need nothing. Others need a `module load`, an activation script, or a conda
environment before the compiler and Arrow are on `PATH`.

**Whatever you do here, record the exact commands in your report** — the next campaign will start
from what you write, and an environment that "was already set up" is unreproducible.

**Then verify it actually took effect.** Do not assume:

```bash
tools/machine_report.sh
```

Read the output before continuing, and check three things:

1. **The Fortran compiler is the one you intended, at the version you intended.** This project's
   minimum is **gfortran 13** — an older one miscompiles it in a way that surfaces much later as an
   unrelated-looking runtime abort, so a wrong version here silently invalidates everything.
2. **`fpm` is found**, and the `fc`/`cc` lines under "what FPM will actually use" name the compilers
   you expect. Those lines are authoritative: fpm derives the C and C++ compilers from the Fortran
   compiler's *family* when `FPM_CXX`/`FPM_CC` are unset, so the first `g++` on `PATH` is often not
   what will build the C++ half.
3. **Arrow and Parquet are found** by `pkg-config`.

> **Activation scripts do not always survive being sourced non-interactively.** One machine's script
> ends by spawning an interactive subshell; run from a script or an agent with no tty, that subshell
> exits immediately, and everything after it silently runs under the *system* toolchain — while the
> environment variables it exported beforehand are set, so the shell looks correctly configured. The
> version check above is what catches this. It is not a formality; it has already caught it once.

**Optional but recommended if the campaign involves link-time optimisation:**

```bash
tools/machine_report.sh --lto-probe
```

That builds and links a tiny mixed Fortran/C++ program with LTO, per available toolchain, in about
fifteen seconds — so "LTO cannot link here" is discovered now rather than several minutes into a
run. Record whatever it prints, including failures.

### Step 3 — run it

Run exactly the command in §1, from the repository root, on an otherwise idle machine, capturing
everything:

```bash
<command from §1> 2>&1 | tee test_run/<campaign>-run.log
```

If the campaign names more than one toolchain or environment, do one run per toolchain, **each in a
fresh shell** — activation scripts leak into one another otherwise — and keep the outputs separate.

### Step 4 — write the report into this file

Fill in the skeleton in §5 and **write it into this same file** — the copy you were given, under the
name you were given it. Then send the file back. §5 says what has to be in it.

Do not create a separate results document, do not remove the instructions above, and do not rename
the file: the instructions travelling back with the report are what makes the report interpretable
later, and the filename is how the analysis step knows which machine produced it.

---

## 3. Rules that apply to every run

These are not style preferences; each one exists because breaking it produced a wrong number that
was believed for a while.

- **All output goes under `test_run/`, in a subdirectory named for the campaign** — build trees,
  logs, generated fixtures, everything. That directory is already git-ignored and is already where
  this project's tests write, so **a benchmark tool must never need a new `.gitignore` entry**; if
  you find yourself adding one, put the output under `test_run/<campaign>/` instead. Two things make
  it the right place rather than merely a convenient one: `tools/run_error_scenarios.sh` resolves
  its executable with `find "${FPM_BUILD_DIR:-build}" -name error_scenarios | head -n 1`, so a build
  tree nested inside `build/` would leave the *standalone* runner ambiguous long afterwards; and the
  test suite writes its fixtures to `test_run/` at the top level, so a subdirectory cannot collide
  with them. Clean up with `rm -rf test_run/<campaign>-*`.
- **A tool that builds into a fresh tree re-fetches its dependencies.** `fpm` clones `test-drive`
  into each new `FPM_BUILD_DIR`, so the first run in a fresh tree **needs network access**. On a
  machine without it, the run fails at the clone rather than at anything meaningful — if that
  happens, say so in the report rather than working around it silently.
- **Never override `FPM_FFLAGS`, `FPM_CXXFLAGS` or `FPM_LDFLAGS`.** On most machines they already
  carry Arrow's include and link paths, and assigning them *replaces* rather than appends —
  producing `fatal error: 'arrow/api.h' file not found`, which reads like a missing dependency
  rather than a flag mistake. Append: `FPM_FFLAGS="${FPM_FFLAGS:-} -newflag"`.
- **Always build through the campaign's `tools/*.sh` wrapper, never a bare `fpm run`.** The wrappers
  pass `--profile release`. Without a profile, fpm applies *no* optimisation flags at all and every
  number is meaningless.
- **A failure is a result.** A build that does not compile, a link that does not resolve, a test
  suite that fails — record it, with the error text, and stop. Do not retry with the flag removed
  and report the number that produced; that answers a different question and looks identical.
- **A deviation is allowed, but it must be labelled.** If the run sheet cannot be followed as
  written and you find a way around it, say exactly what you changed and why, in the report. A
  deviation reported is evidence; a deviation not reported is a fabricated result.
- **The machine must be otherwise idle**, and the report must say whether it was. A large
  many-core/NUMA machine is a *poor* instrument for small differences — this project has recorded
  5.6x run-to-run variation at one size on such a machine — so it is best used for scaling
  questions, with small deltas taken on a quiet workstation.
- **Do not compare figures across machines unless the campaign says they are comparable.** Different
  Arrow versions, thread counts and architectures all change results independently. Report the
  numbers; let whoever set the campaign do the attribution.
- **Keep the provenance with the numbers.** A figure without its compiler, version, architecture and
  commit cannot be compared with anything later, and comparison is the entire purpose.

---

## 4. Running on several machines

Machines run **independently and may run at the same time** — each has its own copy of this file and
writes only into that copy, so there is nothing to merge and no ordering to respect. Return the file
you were given, with your report in it, under the name you were given it.

Two things follow that are easy to get wrong:

- **Do not rename your copy**, and do not strip the instructions out of it before returning it. The
  instructions travelling with the report are what makes the report interpretable months later —
  they record what was asked, not just what was answered.
- **Do not add another machine's numbers to your copy**, even if you have them. Each file holds one
  machine's results; the comparison is made once, at analysis time, across all of them.

---

## 5. Report skeleton — fill this in

*(Fill in the fields below, in place. This copy of the file carries one machine's report; the other
machines' copies carry theirs, and all of them are read together at analysis time.)*

## Report

A run is recorded **whatever it concluded**. A measured "not worth it", a build that would not link,
a test suite that failed — each is a result, and recording it is what stops the next person
re-deriving it. An empty or missing field is not the same as "nothing to report": write "none".

### `<machine name>` on `<YYYY-MM-DD>` (commit `<hash>`)

Command run: `<exactly what was executed>`. Machine otherwise idle: **yes / no**. Best of N rounds
per figure, as the program reports.

**Environment activation used** — the exact commands, so the next campaign can reproduce it:

```bash
<commands, or "none needed">
```

**Provenance** *(from `tools/machine_report.sh`; abbreviate but keep every line that identifies the
toolchain)*:

| | |
|---|---|
| host / OS | |
| CPU, physical + logical cores | |
| architecture / SIMD | |
| RAM | |
| Fortran compiler + version | |
| C++ compiler + version *(the one fpm derived, not the first on PATH)* | |
| Arrow / Parquet version | |
| fpm version | |
| commit, clean or dirty | |

**Did it build and pass?** State this before any timing — a fast build that fails its tests is not a
result.

| | outcome |
|---|---|
| build | |
| `fpm test` | |
| error scenarios | |

**What changed since the commit an earlier run used** *(only if this is being compared against one;
otherwise "n/a")* — the `git diff --stat` command you ran, its output, and your reading of it: are
the two runs directly comparable, or did the measured code change in between?

**Deviations from the run sheet** — anything not done exactly as written, and why. Write "none" if
there were none.

**Results** — paste the tool's own output verbatim in a fenced block, then add a short paragraph
saying what it shows. Verbatim first: a summary can be re-derived from raw output, but raw output
cannot be recovered from a summary.

**Anything surprising** — an error that looks like a defect in this project but turns out to be a
toolchain mismatch, a figure that is wildly out of proportion to its neighbours, a step in these
instructions that did not work as written, a warning that should not have been there. This section is often the most valuable part of a run, and it is the one most easily left
out because it is not what the campaign asked for.

**What this run does not settle** — which machines, compilers or questions remain outstanding.
