#!/usr/bin/env bash
# PreToolUse guard for the `feature_*.md` rule in .claude/rules/workflow.md.
#
# The planning documents in the repository root are git-ignored, so deleting, moving or
# overwriting one is unrecoverable. This hook turns any Bash command that could do so into a
# permission prompt ("ask"), rather than blocking it outright: the maintainer stays free to
# remove a document by confirming.
#
# Triggers on:
#   * `git clean` and `git stash` in any form -- both remove untracked files without naming them;
#   * a delete/move verb (rm, mv, unlink, shred, truncate, git rm) in a command naming feature_*.md;
#   * a truncating `>` redirect onto a feature_*.md path (`>>` appends and is left alone).
#
# Reads the PreToolUse payload on stdin and prints a hookSpecificOutput decision, or nothing at
# all when the command is harmless. Test it by hand with, for example:
#   echo '{"tool_input":{"command":"rm feature_doc.md"}}' | .claude/hooks/guard-feature-docs.sh
python3 -c '
import json, re, sys
c = json.load(sys.stdin).get("tool_input", {}).get("command", "")
sweep = re.search(r"(^|[;&|(\s])git\s+(clean|stash)\b", c)
verb  = re.search(r"(^|[;&|(\s])(rm|mv|unlink|shred|truncate|git\s+rm)\b", c)
name  = re.search(r"feature_[A-Za-z0-9_.*-]*\.md", c)
redir = re.search(r"(?<!>)>\s*[A-Za-z0-9_./-]*feature_[A-Za-z0-9_.-]*\.md", c)
if sweep or (verb and name) or redir:
    print(json.dumps({"hookSpecificOutput": {"hookEventName": "PreToolUse",
        "permissionDecision": "ask",
        "permissionDecisionReason": "This command can delete, move, overwrite or stash a feature_*.md "
        "planning document; they are git-ignored and unrecoverable (.claude/rules/workflow.md)."}}))
'
