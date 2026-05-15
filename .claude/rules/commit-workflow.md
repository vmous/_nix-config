# Commit Workflow

When asked to create a commit (or when it's clear a commit is needed):

1. Use `~/.git-commit-template` as the structural template
2. Inspect recent commit messages in the current repo for style/tone
3. Write the message content:
   - Describe the change and its effects concisely
   - Avoid duplication across sections
   - If you decide to not include an optional section as described in the template then do not include the respective section at all
   - When describing the change in more detail include things like:
     - background (e.g., does it fix a bug, is it a new feature etc.)
     - (optional) decisions and tradeoffs made explicitly or derived by the conversation history of this agent session
     - itemization of the core changes (summarize when possible)
   - When describing impact of the change include things like:
     - How does the affect the experience (e.g., a new files are created, new data are stored or printed in the output; anything that could help someone associate their new experience with the changes)
     - How is this change expected to chage production? (e.g. what it may break / regression, monitors...)
   - When describing rollback implications think about impact in a distributed architecture when deployments are not instanteneous and systems rely on eventual consistency.
4. Present the proposed commit message in a fenced code block, then prompt the user with a selection widget offering three options:
   - **Accept** — commit immediately with the message as-is
   - **Modify** — the user will copy the proposed message, edit it, and paste it back in their reply. Show the modified message back to the user and repeat this same 3-option prompt (loop until the user selects Accept or Reject)
   - **Reject** — do not commit; allow the user to provide follow-up feedback
