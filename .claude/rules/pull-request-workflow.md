# Pull Request Workflow

Terminology clarification: The term "pull request" is based on original Git workflow terminology. Depending on the platform used then "merge request" or "code review" are terms that can be used interchangeably.

When asked to create a pull request:

1. Make sure all packages in the workspace build successfully
2. Ask the user whether they have already committed the changes to be included to the pull request
   - If "yes" jump to step 4
3. Indentify packages that have uncommited changes and commit them
   - Ask the user to verify the files to be included in the commit for each file. User must have the ability exclude files from the commit (especially untracked files)
   - Use standard commit worflow to write the commit message
   - Use the same commit message for all packages you are committing changes to
4. Use the same commit message to create the summary and description of the pull request
5. Run the tool that publishes the pull request.
   - If running the publish tool results in a new commit (commit hash is changed) ensure that:
     - The new commit message text includes the original commit message text intact and only new text is added to before the beginning or after the end the original commit message text.
     - The original commit message text and the new commit message text are separated by exactly one empty line
     - The new commit has the same commiter (`GIT_COMMITTER_DATE ` in Git) and author (`GIT_AUTHOR_DATE` in Git) dates as the original commit
