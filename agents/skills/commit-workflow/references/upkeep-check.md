# Upkeep check

Run before writing the commit message, so an update can go into the same
commit.

1. Look at the work behind the change: the session and the staged diff. Did
   it settle a design decision or convention, or correct how the agent
   approached the task?
2. Keep only durable guidance. Skip task-specific details, what the code or
   `git log` already shows, and what Ponytail, Caveman or the existing
   instructions already say.
3. Route each item:
   - Specific to this repository: its `AGENTS.md`.
   - Generic, for every repository:
     `~/Workspace/personal/_nix-config/agents/skills/jazzy-dev/SKILL.md`.
     That is another repository: propose the change as a separate commit
     there.
4. Propose the smallest update: refine an existing bullet rather than add
   one. Show the diff and ask whether to include it. If nothing qualifies,
   say so in one line.
