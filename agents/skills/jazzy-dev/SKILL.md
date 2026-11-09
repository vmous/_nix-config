---
name: jazzy-dev
description: Personal development guidelines for every coding task in every repository, next to Ponytail and Caveman, and which workflow to follow for commits and pull requests. The jazzy-dev plugin's hook adds them at session start; load this skill only when that hook didn't run.
---

# jazzy-dev guidelines

These add to Ponytail and Caveman; where they disagree, these win. A
repository's own instructions (`AGENTS.md`, `CLAUDE.md`) win over these
where they are more specific.

## Workflows

- Before creating any commit, including one needed in the middle of a
  larger task, follow the `commit-workflow` skill.
- Before creating or revising a pull request (also called a merge request
  or a code review), follow the `pull-request-workflow` skill.

## Changes

- When removing or replacing anything (a tool, package, function,
  variable, file or setting), also remove what refers to it:
  configuration, ignore rules, comments, documentation and leftover files.
- Comments and documentation explain why, not what. At a non-obvious
  workaround, quote the error it fixes and link the source, even when that
  takes more than the one line Ponytail allows a comment.
- Don't enumerate things that go stale (function names, lists of
  settings); point to the file that defines them. This applies to agent
  instruction files too.

## Verification

- Verify every change before reporting it done, with the checks the
  repository documents for its area.
- Test against copies, temporary directories and throwaway servers, never
  against live state.

## Boundaries

- Don't edit files that other tools or an employer manage. Leave blocks
  that tools added alone, and out of commits unless asked.
- Real secrets (credentials, tokens, host names, ids) never go into a
  repository or into output.
