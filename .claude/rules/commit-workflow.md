# Commit Workflow

When asked to create a commit (or when it's clear a commit is needed) follow these steps

## Selecting the files to be commited

1. Check repository status
   - Gather a complete list of all modified, staged, and untracked files in the current working directory.
2. Ask the user which files should be included in the commit
   - Allow the user to select or deselect specific files to be included in the upcoming commit using an interactive checklist interface (or ask them to paste the list of files if an interactive widget is not natively supported by your current UI).

## Before starting writing the commit message

1. Read `~/.git-commit-template` as template for structure and basic formatting rules
2. Read the repository's past commit messages to understand expected tone and style
3. Read and understand the changes introduced in files selected to be commited in the previous section "Selecting the files to be commited"

## Writing the commit message

Using the context you build already write the commit message
1. Focus your one-liner to the end-impact of the change.
   - Leave the details for the rest of the body (e.g., if in order to achieve the change's end-impact an incidental but extensive refactoring was required, then the end-impact is still more important to mention in the one-liner and the details of the refactoring can be mentioned in the body details).
2. Be crisp, don't be verbose, don't repeat yourself
3. If you decide to not include an optional section as described in the template then do not include the respective section at all
4. When describing impact of the change include things like:
   - How does the affect the experience (e.g., a new files are created, new data are stored or printed in the output; anything that could help someone associate their new experience with the changes)
   - How is this change expected to chage production? (e.g. what it may break / regression, monitors...)
5. When describing the change in more detail include things like:
   - background (e.g., does it fix a bug, is it a new feature etc.)
   - (optional) decisions and tradeoffs made explicitly or derived by the conversation history of this agent session
   - itemization of the core changes (summarize when possible)
6. When describing rollback implications think about impact in a distributed architecture when deployments are not instantaneous and systems rely on eventual consistency.

At all stages of the process keep the message properly formatted
- message must be valid markdown
   - references to code objects or variables must be enclosed in '`'
- follow the agreed upon line character length limits structure and formatting (based on template and past commit messages)
   - do not remove or add content
   - move the line breaks so that the resulted lines have the maximum possible characters but within the length limits

## Agreeing on the commit message

1. Present the proposed commit message you wrote above in a fenced code block.
