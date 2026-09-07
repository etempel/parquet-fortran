---
name: mutation-test
description: "Verify that a test guards a documented behaviour by breaking the code deliberately and confirming the named test fails — snapshot, mutate, clean build, run, restore — never restoring with git checkout."
argument-hint: "<file or procedure, the behaviour, and the test expected to fail>"
allowed-tools: Bash(fpm:*), Bash(git:*), Bash(grep:*), Bash(find:*), Bash(ls:*), Bash(cat:*), Bash(sed:*), Bash(awk:*), Bash(cp:*), Bash(diff:*), Bash(mkdir:*), Bash(date:*), Bash(python3:*), Bash(build/:*), Bash(test_run/:*)
disable-model-invocation: true
---

# /mutation-test — prove a test catches a broken behaviour

What counts as evidence (fast paths, thresholds, masked guards, no-op mutations, canaries, optional
arguments nobody passes) is in `.claude/rules/testing.md`, "Mutation testing"; this is the
procedure. Request: $ARGUMENTS

Injected context:

- Working tree: !`git status --short | head -10`
- Test binaries present: !`find build -type f -name 'run_tester*' | wc -l`

## 0 Never restore with `git checkout`

The tree is normally uncommitted, so `git checkout -- <file>` discards the feature, not the
mutation. Every restore in this procedure comes from the snapshot it takes.

## 1 Plan the round

- Per mutation: the file, the exact edit (invert a guard, delete a branch, return a constant, flip
  a tie-break, drop a forwarded argument), the documented behaviour it breaks, and the NAMED test
  expected to fail. A generated file is mutated in its emitted output, never in the generator.
- Check the mutated path is reached: a counting fast path, a size threshold or a cache can skip it.
  Lower the threshold with its `parquet_debug_set_*` override or choose a fixture the fast path
  declines. Zero invocations is a passing test.

## 2 Snapshot, and verify the restore before the first mutation

```bash
SNAP=<session scratchpad>/mut-$(date +%s); mkdir -p "$SNAP"
for f in <files>; do mkdir -p "$SNAP/$(dirname "$f")"; cp "$f" "$SNAP/$f"; done
```

The scratchpad is the session's own temporary directory named in the system prompt, never a path
inside the repository. Run step 4's restore once on the unmutated files and confirm `diff -q`
reports nothing.

## 3 One mutation per round

- Apply the edit; confirm it with `diff "$SNAP/$f" "$f"`.
- `fpm clean --skip` — a stale binary makes a mutation look caught or survived wrongly, and
  "Project is up to date" is not trustworthy across a restore. Then the narrowest run containing
  the expected test: `fpm test <runner> -- <suite> "<test>"`, capturing the exit status.
- Verdict: CAUGHT if the test fails OR the process aborts (`error stop` nonzero, SIGABRT 134,
  SIGSEGV 139 or 11); SURVIVED only if the run is green. Read the exit status; `grep -c FAILED`
  alone reports an abort as survived. For an error scenario, "caught" is the scenario exiting 0 and
  printing its "accepted" line.

## 4 Restore

```bash
for f in <files>; do cp "$SNAP/$f" "$f"; diff -q "$SNAP/$f" "$f" || echo "RESTORE FAILED: $f"; done
fpm clean --skip
```

Re-run the suite green before the next mutation. A failure persisting across a verified-correct
source is a stale-cache symptom, not a new defect.

## 5 Read a survivor

Before calling it a coverage gap: masked by a redundant sibling guard or a later check? A semantic
no-op (a total order has no ties to break)? The path never reached? An IEEE-flag mutation the
optimiser removed (use the two-step underflow through a live named variable, proved with a
standalone program)? Only then propose the missing assertion, with its negative control — propose,
unless the invocation asked for the test to be written.

## 6 Report

A table per mutation: file, edit, behaviour, expected test, run, exit status, verdict; survivors
with the proposed test; confirmation that every file was restored (`diff -q` against the snapshot)
and the final run is green.
