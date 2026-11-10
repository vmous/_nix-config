---
name: commit-workflow
description: Create a git commit the user's way - the user picks the files, the message follows the repository's commit template and history, and nothing is committed until the user agrees to the message. Use whenever asked to commit or to write a commit message, when a commit is needed, or when another workflow such as a pull request needs one.
---

# Commit workflow

1. **Pick the files.** List the modified, staged and untracked files
   (`git status --porcelain`). Ask the user which to include: offer a
   multi-select if the client has one, otherwise a list they can edit.
   Stage exactly those paths; never `git add -A` or `git add .`.
2. **Gather context.**
   - The template: `git config --type=path --get commit.template` (if
     unset, follow the history).
   - Recent `git log` messages, for tone, subject scopes and sections.
     Where the history disagrees with the repository's `AGENTS.md` or
     `CLAUDE.md`, those files win.
   - The staged changes, including changes the agent didn't make.
3. **Upkeep check** (`references/upkeep-check.md`): propose instruction
   updates for conventions the work settled. Stage the ones the user
   includes.
4. **Write the message** (details: `references/message-guide.md`).
   - The subject states the end impact of the change, not the means.
   - Be crisp; don't repeat yourself. Leave out optional template
     sections you don't use, headers included.
   - Valid markdown; code objects in backticks. Keep the template's and
     the history's line limits: when rewrapping, move line breaks only,
     filling each line as far as the limit allows, and never add or remove
     content.
5. **Agree before committing.** Show the proposed message in a fenced
   code block. Commit only after the user agrees, with exactly the agreed
   text (`git commit -F <file>`). Don't amend, rewrite or push unless
   asked.
