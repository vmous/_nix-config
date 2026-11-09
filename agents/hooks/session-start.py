#!/usr/bin/env python3
# SessionStart hook for Claude Code and Codex: adds the always-on guidelines,
# the body of skills/jazzy-dev/SKILL.md, to the session's context, as the
# `additionalContext` JSON that Ponytail's hook also uses for both clients.
import json
import pathlib
import re

skill = pathlib.Path(__file__).resolve().parent.parent / "skills" / "jazzy-dev" / "SKILL.md"
text = skill.read_text(encoding="utf-8")
body = re.sub(r"\A---\n.*?\n---\n", "", text, count=1, flags=re.S).strip()
print(json.dumps({"hookSpecificOutput": {"hookEventName": "SessionStart",
                                         "additionalContext": body}}))
