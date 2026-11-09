# Commit message guide

- **Subject:** the end impact first, even when an incidental but extensive
  refactoring was most of the work; the refactoring goes in the body.
- **Details:** the background (a bug fix, a new feature, ...); decisions
  and trade-offs made in the session (optional); the core changes,
  itemized and summarized where possible.
- **Impact:** what users will notice (files created, data stored or
  printed, anything that links their new experience to the change) and
  how production changes (what may break or regress, monitors).
- **Rollback implications:** deployments aren't instantaneous and systems
  rely on eventual consistency; say what a partial rollout or a rollback
  leaves behind.
