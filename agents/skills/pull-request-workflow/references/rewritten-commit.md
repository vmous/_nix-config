# Checking a commit the publish tool rewrote

Before publishing, record each package's commit:

```sh
git log -1 --format='%H %aI %cI'
git log -1 --format=%B > <tmp>/original-message.txt
```

After publishing, compare: the new message contains the original
unchanged, as a prefix or a suffix, separated from the added text by
exactly one empty line, and `%aI` and `%cI` match the recorded dates.

Repair only after the user agrees, then publish a new revision:

```sh
GIT_COMMITTER_DATE='<recorded %cI>' git commit --amend \
  --date='<recorded %aI>' -F <corrected message file>
```
