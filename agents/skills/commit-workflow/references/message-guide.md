# Commit message guide

Caveman's `caveman-commit` writes the subject and the body. The first two
items are the fallback for where it isn't installed; the last two describe
the sections added after the body.

- **Subject:** Conventional Commits, `type(scope): summary` (types such as
  `feat`, `fix`, `docs`, `refactor`, `chore`). Imperative mood, no trailing
  period, 50 characters where possible and 72 at most. The summary states
  the end impact, even when an incidental refactoring was most of the work.
- **Body:** only for a why the subject can't carry, and always for breaking
  changes, security fixes, data migrations and reverts. Wrap at 72. No
  "This commit does X", "I", "we", "now" or "currently"; no AI attribution.
- **`REFERENCES:`** optional; itemized links to related tickets, issues,
  reviews or documents.
- **`QA/TEST:`** required; itemized: the checks that were run, and the ones
  that weren't (e.g., the other machine).
