# Picking a package's commit

For each package in the pull request:

1. Fetch the upstream, so that `@{u}` is current, then list the candidate
   commits newest first: `git rev-list @{u}..HEAD`.
2. Walk down the list. Stop at the first commit whose message has a
   `CR: https://code.amazon.com/reviews/CR-…` line (match it ignoring
   case) naming a review other than the one being published. That commit
   is the base: it belongs to an earlier review and reaches upstream when
   that review merges. A commit naming the review being revised belongs to
   this one.
3. The commits above the base (all of them, if nothing stops the walk)
   belong to this pull request:
   - exactly one: publish it on top of the base;
   - more than one: stop and ask the user to squash them;
   - none: the package has nothing to publish.

Examples, oldest to newest:

| Stack | Publishing | Result |
|---|---|---|
| A (`CR-A`), B | a new review | B, on base A |
| A (`CR-A`), B, C | a new review | ask to squash B and C |
| A (`CR-A`), B (`CR-B`) | a revision of CR-B | B, on base A |
| A merged upstream, B | a new review | B |

Platforms that add no such line (e.g. GitHub) never stop the walk: every
commit after `@{u}` belongs to the pull request.
