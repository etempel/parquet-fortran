#!/usr/bin/env bash
# PreToolUse guard for the `feature_*.md` rule in .claude/rules/workflow.md.
#
# The planning documents in the repository root are git-ignored, so deleting, moving or
# overwriting one is unrecoverable. This hook turns any Bash command that could do so into a
# permission prompt ("ask"), rather than blocking it outright: the maintainer stays free to
# remove a document by confirming.
#
# Triggers on:
#   * `git clean` in any form -- with -x or -X it removes ignored files without naming them, and
#     the flags are too easy to add to a command that was safe when it was written;
#   * `git stash` UNLESS every occurrence in the command is provably unable to reach an ignored
#     file (see below);
#   * a delete/move verb (rm, mv, unlink, shred, truncate, git rm) in the SAME simple command as a
#     feature_*.md name -- the command is split on newlines, `;`, `&&`, `||`, `|` and `&` first,
#     so copying a document in one statement beside a script that mentions `rm` in another does
#     not prompt, while `rm feature_x.md` on any line, a heredoc body included, still does;
#   * a truncating `>` redirect onto a feature_*.md path (`>>` appends and is left alone).
#
# WHICH `git stash` FORMS STAY QUIET, and why the test is shaped this way. The baseline recipe in
# .claude/rules/benchmarking.md is `git stash push -- src tools`, rebuild, measure, `git stash
# pop` -- a scoped stash that cannot touch the repository root at all. Asking on it every time
# taught nothing, so two shapes are let through:
#
#   * `git stash pop|apply|list|show|branch` -- these restore or report. None can delete an
#     ignored file; `pop` in particular is the second half of the recipe and the recovery path
#     after an -a stash, so prompting on it would be backwards.
#   * `git stash [push|save] -- <pathspec>...` with NO untracked-including flag and with every
#     pathspec a plain relative path that is not `.`, does not climb with `..`, and does not name
#     a feature document. A pathspec confines the stash, so the root stays untouched whatever the
#     flags say -- but the flags are still checked, because a rule with two independent reasons to
#     be safe survives one of them being mis-read.
#
# Everything else about stash still asks: a bare `git stash`, a `push` with no pathspec, anything
# carrying -u/-a/--include-untracked/--all, and `drop`/`clear`/`store`, which destroy a stash that
# may be the only copy of a document an earlier `git stash --all` swept up.
#
# **Only `--all` actually reaches these documents** (`-u` stashes untracked files but not ignored
# ones, and `feature_*.md` is ignored). The unflagged forms are refused anyway: the distinction is
# one flag wide, invisible in a diff, and wrong in the direction that costs a document.
#
# Reads the PreToolUse payload on stdin and prints a hookSpecificOutput decision, or nothing at
# all when the command is harmless. Test it by hand with, for example:
#   echo '{"tool_input":{"command":"rm feature_doc.md"}}' | .claude/hooks/guard-feature-docs.sh
# and run the whole case table, which is the check that a change here still refuses what it must:
#   .claude/hooks/guard-feature-docs.sh --self-test

if [ "${1:-}" = "--self-test" ]; then
    python3 - "$0" <<"SELFTEST"
import json, subprocess, sys

hook = sys.argv[1]

# (expect_ask, command). Every "quiet" row must be a command that genuinely cannot reach an
# ignored file, and every "ask" row one that could -- or that is one flag away from being able to.
CASES = [
    # --- git clean: always ask, whatever the flags say.
    (True,  "git clean -fd"),
    (True,  "git clean -fdx"),
    (True,  "git clean -n"),
    # --- git stash without a pathspec: could take the whole tree.
    (True,  "git stash"),
    (True,  "git stash push"),
    (True,  "git stash save wip"),
    (True,  "git stash -u"),
    (True,  "git stash --all"),
    # --- git stash with a pathspec but an untracked-including flag.
    (True,  "git stash push -u -- src"),
    (True,  "git stash push --include-untracked -- src"),
    (True,  "git stash push -a -- src"),
    (True,  "git stash push --all -- src"),
    (True,  "git stash push -ku -- src"),
    # --- git stash with a pathspec that can reach the root or a document.
    (True,  "git stash push -- ."),
    (True,  "git stash push -- src ../elsewhere"),
    (True,  "git stash push -- feature_fof.md"),
    (True,  "git stash push -- :/"),
    (True,  "git stash push -- *"),
    (True,  "git stash push --"),
    # --- git stash subcommands that destroy a stash.
    (True,  "git stash drop"),
    (True,  "git stash clear"),
    (True,  "git stash store abc123"),
    # --- one unsafe occurrence anywhere is enough.
    (True,  "git stash push -- src && git clean -fdx"),
    (True,  "git stash push -- src; git stash"),
    # --- the delete-verb and redirect rules, unchanged.
    (True,  "rm feature_doc.md"),
    (True,  "mv feature_x.md /tmp/"),
    (True,  "git rm feature_risks.md"),
    (True,  "echo hi > feature_x.md"),
    (True,  "cp a.md b.md; rm feature_x.md"),
    # --- scoped stash: the benchmarking recipe, in the forms it is actually written.
    (False, "git stash push -- src tools"),
    (False, "git stash push -- src test bench"),
    (False, "git stash push --quiet -- src tools"),
    (False, "git stash push -k -- src"),
    (False, "git stash push -m baseline -- src tools"),
    (False, "git stash push -- src/parquet_spatial.f90"),
    (False, "git stash push -- src test bench >/dev/null 2>&1 && echo stashed"),
    # --- restoring and reporting.
    (False, "git stash pop"),
    (False, "git stash pop >/dev/null 2>&1 && echo popped"),
    (False, "git stash apply"),
    (False, "git stash list"),
    (False, "git stash show -p"),
    # --- the full baseline cycle as one command, which is what prompted this narrowing.
    (False, "git stash push -- src test bench >/dev/null 2>&1 && echo stashed; "
            "fpm build; git stash pop >/dev/null 2>&1 && echo popped; git status --short"),
    # --- ordinary commands, including ones that merely NAME a document.
    (False, "fpm test"),
    (False, "cat feature_fof.md"),
    (False, "grep -n combine feature_fof.md"),
    (False, "cp feature_fof.md /tmp/backup.md"),
    (False, "echo hi >> feature_x.md"),
]

bad = 0
for expect_ask, cmd in CASES:
    payload = json.dumps({"tool_input": {"command": cmd}})
    out = subprocess.run([hook], input=payload, capture_output=True, text=True).stdout.strip()
    got_ask = "permissionDecision" in out
    if got_ask != expect_ask:
        bad += 1
        want = "ask" if expect_ask else "quiet"
        print("FAIL: expected %-5s got %-5s for: %s" % (want, "ask" if got_ask else "quiet", cmd))

if bad:
    print("guard-feature-docs.sh --self-test: %d of %d case(s) FAILED" % (bad, len(CASES)))
    sys.exit(1)
print("guard-feature-docs.sh --self-test: all %d cases behave as documented." % len(CASES))
SELFTEST
    exit $?
fi

python3 -c '
import json, re, sys
c = json.load(sys.stdin).get("tool_input", {}).get("command", "")

def path_is_safe(p):
    # A plain relative path, inside the tree, not a feature document. Anything with a glob, a
    # magic pathspec prefix or a leading dot is refused rather than reasoned about.
    if not re.match(r"^[A-Za-z0-9_./-]+$", p):
        return False
    if p.startswith("-") or p.startswith("/") or p.startswith("."):
        return False
    if ".." in p or "feature_" in p:
        return False
    return True

def stash_is_scoped(rest):
    # `rest` is what follows `git stash` up to the next shell separator. Redirections are cut
    # first so that a target such as /dev/null is never read as a pathspec.
    toks = re.split(r"[<>]", rest)[0].split()
    sub = toks[0] if toks and not toks[0].startswith("-") else ""
    if sub in ("pop", "apply", "list", "show", "branch"):
        return True
    if sub not in ("", "push", "save"):
        return False
    if "--" not in toks:
        return False
    head = toks[:toks.index("--")]
    for t in head:
        if t in ("--include-untracked", "--all"):
            return False
        if t.startswith("--"):
            continue
        if t.startswith("-") and ("u" in t[1:] or "a" in t[1:]):
            return False
    paths = toks[toks.index("--") + 1:]
    if not paths:
        return False
    return all(path_is_safe(p) for p in paths)

sweep = bool(re.search(r"(^|[;&|(\s])git\s+clean\b", c))
for m in re.finditer(r"(^|[;&|(\s])git\s+stash\b(?P<rest>[^;&|\n]*)", c):
    if not stash_is_scoped(m.group("rest")):
        sweep = True
        break
redir = re.search(r"(?<!>)>\s*[A-Za-z0-9_./-]*feature_[A-Za-z0-9_.-]*\.md", c)
hit = bool(sweep or redir)
if not hit:
    for seg in re.split(r"\n|;|&&|\|\||\||&", c):
        if re.search(r"(^|[(\s])(rm|mv|unlink|shred|truncate|git\s+rm)\b", seg) and \
           re.search(r"feature_[A-Za-z0-9_.*-]*\.md", seg):
            hit = True
            break
if hit:
    print(json.dumps({"hookSpecificOutput": {"hookEventName": "PreToolUse",
        "permissionDecision": "ask",
        "permissionDecisionReason": "This command can delete, move, overwrite or stash a feature_*.md "
        "planning document; they are git-ignored and unrecoverable (.claude/rules/workflow.md)."}}))
'
