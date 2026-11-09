---
name: pull-request-workflow
description: Create a pull request (also called a merge request or code review, e.g. a CRUX CR) or publish a new revision of one the user's way - build first, commit with the commit-workflow skill, publish exactly one commit per package, set the summary and description from the commit messages on every revision, and check that publishing left the commit messages and dates intact. Use whenever asked to create, update or publish a pull request, merge request or code review.
---

# Pull request workflow

A pull request can span several packages, each its own git repository (as
in a CRUX code review). A pull request in a single repository (as on
GitHub) has one package.

1. **Build.** Make sure all packages in the workspace build successfully.
   Don't publish while one fails.
2. **Commit.** Ask whether the changes are already committed; if yes, go
   to step 3. Otherwise commit each package with uncommitted changes
   through the `commit-workflow` skill, letting the user pick each
   package's files (untracked files especially), with one message shared
   by all packages.
3. **Pick each package's commit** (`references/commit-selection.md`). A
   package publishes exactly one commit. If it has more, stop and ask the
   user to squash them; never squash on your own.
4. **Summary and description.** Take them from the commit messages as
   written, not from text a publish tool appended to them:
   - All packages have the same message: the summary is its subject, the
     description its body.
   - The messages differ: the summary sums up all the packages' subjects
     in one line; the description holds each package's subject and body,
     packages in alphabetical order, separated by a line of `----------`
     with an empty line on both sides.

   Show text you wrote yourself, such as that combined summary, and wait
   for the user's OK. Text copied verbatim from the commit messages needs
   no OK.
5. **Publish** with the platform's tool. Record each package's commit
   first (`references/rewritten-commit.md`). Pass the summary and the
   description explicitly on every call, for every revision, even where
   the platform's guidance says to leave them alone (e.g. `cr -r` keeps the
   previous text). Publish each package's commit on top of the base found
   in step 3, and when the base belongs to another open review, declare
   the dependency (CRUX: `--parent <base>` and `--stack <CR>`).
6. **Check the result.** If publishing rewrote a commit, check it
   (`references/rewritten-commit.md`): the original message is intact,
   with new text only before or after it, separated by exactly one empty
   line, and the author and committer dates are unchanged. If not, show
   the difference and ask before repairing, because repairing needs a new
   revision.
