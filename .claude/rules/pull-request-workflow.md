# Pull Request Workflow

The term "pull request" is based on original Git workflow terminology. Depending on the platform used then "merge request" or "code review" are terms that can be used interchangeably. When asked to create a pull request:

1. Make sure all packages in the workspace build successfuly
2. Ask the user whether they have already committed the changes to be included to the pull request
   - If "yes" jump to step 4
3. Indentify packages that have uncommited changes and commit them
   - Ask the user to verify the files to be included in the commit for each file. User must have the ability exclude files from the commit (especially untracked files)
   - Use standard commit worflow to write the commit message
   - Use the same commit message for all packages you are committing changes to
4. Use the same commit message to create the summary and description of the pull request
