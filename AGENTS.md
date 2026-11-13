# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## What This Repo Is

Personal dotfiles and *nix configuration, symlinked into `$HOME` (see Configuration Management). The repo is expected to live at `~/Workspace/personal/_nix-config`.

Configuration targets three machine types detected at shell startup via `$JMACHINE`: `mac`, `worklinux`, `homelinux`.

## Safety and Boundaries

- **Secrets:** `.emacs.d/secrets.el` in the repo is a template; the real one is a private, non-symlinked `~/.emacs.d/secrets.el`. Real values (hostnames, credentials, tokens) never go into the repo or into output, and config refers to them only through `jazzy/secrets/...` constants.
- **Files other tools own:** don't edit employer-managed files such as `~/.ssh/config`. Leave blocks added by tools (e.g. "managed by AIM CLI") alone and out of commits unless asked. The worktree holds many untracked local files, so commit only the files the user selects.
- **Live sync:** Unison keeps this repo, `.git` included, in sync with the cloud desktop continuously, the newer copy winning (see `.unison/_nix-config-sync.prf.amzn-mac`). Scratch files therefore reach the other machine, and git must never run in this repo on both machines at once.
- **Both machines:** changes must work on the Apple Silicon Mac (`mac`) and the x86_64 Linux cloud desktop (`worklinux`). Don't assume macOS-only tools such as `/usr/libexec/java_home`, or a single CPU architecture for downloaded binaries.

## Configuration Management

Files are symlinked from this repo into `$HOME`, and today most of them are linked by hand.

Ansible (`ansible/`) is being worked on and is in an alpha stage: it is not yet how the machines are set up, and it covers only the files listed in `repo_conf_files` in `ansible/main.yml`. To try it:

```sh
ansible-playbook ansible/main.yml
```

Useful flags: `--check` (dry run), `--tags "shared,dotfiles"`, `--skip-tags "linux"`, `-vvv` (debug).

Ansible runs locally (`inventory.ini` points to `localhost`). The playbook symlinks dotfiles from this repo into `$HOME`, backing up any existing non-symlink files first, and asserts the repo location above.

## Repository Structure

Verification lists the checks for each area.

- `ansible/`, `ansible.cfg` — Playbooks (alpha, see Configuration Management). `main.yml` is the entry point; it imports `macos.yml` and `linux.yml` conditionally.
- `.zshrc` / `.zshenv` / `.zsh.d/` — Zsh configuration. Platform-specific blocks are gated on `$JMACHINE`. The `.zsh.d/amzn/` subdirectory holds work-specific shell extensions.
- `.alias` — Shell aliases and utility functions sourced by `.zshrc`.
- `.bashrc` / `.bash_profile` — Bash configuration.
- `.emacs` / `.emacs.d/` — Emacs configuration split by language/domain (e.g., `core.el`, `python.el`, `scala.el`).
- `.tmux.conf` — tmux configuration.
- `.config/herdr/config.toml` — herdr configuration. Apply with `herdr server reload-config`.
- `.unison/` / `Library/LaunchAgents/` — Unison sync profiles between the Mac and the cloud desktop, and the launchd agents that run them.
- `.gitconfig` / `.gitconfig-aliases.sh` / `.gitconfig.amzn` — Git aliases and settings. Work-specific overrides are conditionally included from `.gitconfig.amzn` for paths under `~/Workspace/work/`.
- `.git-commit-template` — Structured commit message template (used by git via `commit.template`).
- `agents/` — `jazzy-dev`, a local Claude Code and Codex plugin marketplace that `j-tools-ai-setup` installs: the always-on guidelines for every repository (`skills/jazzy-dev/SKILL.md`, which `hooks/` adds at session start) and the commit and pull request workflow skills. Guidance specific to this repo stays in this file. Commits use the `ai` scope. The clients run a cached copy: on every change bump `version` in both `plugin.json` files (Claude Code ignores a change without it), then on each machine run `claude plugin marketplace update jazzy-dev`, `claude plugin update jazzy-dev@jazzy-dev` and `codex plugin add jazzy-dev@jazzy-dev`.
- `.rtorrent.rc`, `.i3/`, `.nice-dcv/`, `openvpn`, `sources.list`, `Library/Spelling/` — Configuration for tools that aren't installed on every machine.

## Verification

Check every change with the checks for its area below.

- Live state, never to test against: running tmux or herdr sessions, real Unison roots or backups, real data for destructive commands, and the real `HOME`, which shell and Emacs startup write to.
- When a change affects both `mac` and `worklinux`, check it on both, or say which one was not checked.

### Checks by area

- **Ansible:** `ansible-playbook --syntax-check ansible/main.yml`, then a `--check` dry run.
- **zsh** (`.zshrc`, `.zshenv`, `.zsh.d/`, `.alias`): syntax with `zsh -n <file>`, and functions against temporary fixtures with stubbed prompts. Check the resulting environment in a login shell with a temporary `HOME`, because with the real one startup re-points `~/.ssh/ssh_auth_sock` and writes caches: copy `.zshrc`, `.zshenv`, `.alias`, `.zsh.d/`, `~/.oh-my-zsh` and `~/.local/share/mise/completions.zsh` into `<tmp>`, create an empty `<tmp>/.ssh/`, and run `HOME=<tmp> zsh -lic '...' </dev/null` (two `can't change option: zle` lines are expected). On the cloud desktop, also link `<tmp>/.local/bin` to `~/.local/bin` (mise lives there) and run `SHELL=/apollo/env/EnvImprovement/var/bin/zsh HOME=<tmp> /apollo/env/EnvImprovement/var/bin/zsh -lic '...' </dev/null`: otherwise `/apollo/env/EnvImprovement/dotfiles/zshrc`, which `.zsh.d/amzn/devdsk.zsh` loads, re-runs the shell (`exec $ZSH -$- "$@"`) and drops the command. A `mise WARN … not trusted` line is expected there.
- **bash** (`.bashrc`, `.bash_profile`, `.gitconfig-aliases.sh`): `bash -n <file>`.
- **Emacs:** load the full configuration in batch mode with a temporary `HOME`, because with the real one the load writes history, `recentf` and the `eln-cache`: copy `.emacs`, the repo's `.emacs.d/*.el` (the empty `secrets.el` template included) and `~/.emacs.d/elpa` into `<tmp>`, create an empty `<tmp>/.emacs.d/backup/` (without it the load stops at "Directory `…/.emacs.d/backup' does not exist! Create it?"), and run `HOME=<tmp> emacs --batch --eval '(load "\~/.emacs")'`. Check for errors and for warnings from repo files; warnings from packages in `elpa/` don't count. On the Mac, simulate a Dock launch with launchd's minimal environment: `env -i HOME=<tmp> USER=$USER SHELL=/bin/zsh PATH=/usr/bin:/bin /Applications/Emacs.app/Contents/MacOS/Emacs --batch ...`. The import of the shell environment runs only on the hosts named in the real secrets: to include it, link the real `~/.emacs.d/secrets.el` into `<tmp>` in place of the template (never open it) and add the zsh copies above.
- **tmux:** `tmux source-file -n <file>` checks syntax only. To also catch invalid options, source a copy without the `run` line (it starts the plugins, and tmux-continuum would save or restore sessions) into a throwaway server: `tmux -L check -f /dev/null new-session -d 'sleep 20'`, then `tmux -L check source-file <copy>` and `tmux -L check kill-server`. Never source it into the running server.
- **herdr:** `herdr config check`; to check a copy, place it at `<dir>/herdr/config.toml` and run `XDG_CONFIG_HOME=<dir> herdr config check`.
- **Unison:** verify a profile on a copy, never against the real roots: copy `.unison/<name>.prf.amzn-mac` to `<tmp>/<name>.prf` without its `root`, `repeat` (neither `watch` nor a number of seconds ever exits) and `logfile` (the real log, by absolute path) lines, add `root` lines for two temporary directories holding fixtures under the profile's `path`, and run `UNISON=<tmp> timeout 60 unison <name> -batch`. Archives, the log and backups then stay in `<tmp>`. Check the launchd plists with `plutil -lint`.
- **git config** (`.gitconfig`, `.gitconfig.amzn`): `git config --file <file> --list`.
- **`agents/`:** `claude plugin validate agents`, and run the hook (`CLAUDE_PLUGIN_ROOT=agents python3 agents/hooks/session-start.py`), which must print valid JSON.
- **Tools not installed on every machine:** there is no automated check: review changes against the tool's documentation, check shell scripts (e.g., `.nice-dcv/launch.sh`) with `zsh -n`, and test on a machine where the tool is installed.

## Key Design Decisions

### General

- Dotfiles are **symlinked** into `$HOME`, not copied. Edits should happen in this repo.
- The shell is the single source of truth for environment variables (e.g., `PATH`, `JAVA_HOME`). On the Mac, Emacs imports them from the login shell (see the imported variables in `.emacs.d/environment.el`), so add a new variable there rather than re-detecting it in Elisp.
- Work-specific configuration lives in `.zsh.d/amzn/`, `.gitconfig.amzn` and `*.amzn-*` files. Personal configuration must work without them.
- AI coding agent support (the setup in `.zsh.d/tools.zsh`, the `agents/` plugin) covers Claude Code and Codex only. Leave other clients, such as Kiro on the cloud desktop, out.

### Zsh

- Oh-My-Zsh is expected at `~/.oh-my-zsh` but is not tracked here (listed in `untracked_conf_files`).
- `compinit` must remain near the end of `.zshrc` — completion configuration before it is honored; configuration after it is ignored.
- Shared zsh logic lives in `.zsh.d/utils.zsh`, sourced early by `.zshrc`. Before writing zsh scripts, functions or aliases, read that file for the current set of helpers and reuse them instead of re-implementing them; logic that other places could use should be added there as a new helper.
- New user-facing commands are named `j-*`, and their internal helpers start with `_`.
- Use a function rather than an alias when other functions call it: aliases expand when a function is defined. Shared code must check that optional or work-only functions exist before calling them, e.g. `(( ${+functions[name]} ))`.
- Destructive commands show what they'll remove as totals and sizes, not full listings, and confirm through the shared yes/no helper. Cleanup commands report the space actually freed.

### Emacs

- For machine- or platform-specific behaviour use the conditionals in `.emacs.d/environment.el` (read it for the current set), never raw `system-type`. OS and CPU conditionals are for platform behaviour; host-name conditionals (matched via secrets) are for settings that belong to one particular machine.
- Generic programming setup (e.g., `lsp-mode`, `company`, `flycheck`, `projectile`) goes in `prog.el`, and JVM-generic setup in `java.el`. Language files such as `scala.el` hold only language-specific parts: the major mode, the language server client, the hook to `lsp`, and that language's requirements.
- Elisp symbols use the `jazzy/<area>/` namespace. Boolean constants are named `is-*`, not `-p`, which Elisp reserves for predicate functions.
- Every Emacs Lisp file starts with `-*- lexical-binding: t -*-`.
- Packages are installed with `use-package` and `:ensure t`. Archive priority is gnu > melpa-stable > melpa; use `:pin melpa` only when the stable release is broken, with a comment saying why. Never edit `~/.emacs.d/elpa`: fix package problems through configuration, or by regenerating autoloads and recompiling.
- Never put `:after` on a package whose `:init` has to run before it loads, such as hooks that trigger loading it.
- The `custom-set-variables` block in `.emacs` is managed by Emacs; don't edit it by hand.

### tmux and herdr

- SSH agent socket is kept stable across tmux reattaches via a symlink at `~/.ssh/ssh_auth_sock`.
- tmux (`.tmux.conf`) and herdr (`.config/herdr/config.toml`) are kept in step. When adjusting either, make the same workflow available in the other as far as it supports it (e.g., the `C-a` prefix, `|`/`-` splits, `h`/`j`/`k`/`l` pane navigation), so a pattern learned in one works in both.

### Unison

- Regenerable artifacts (e.g., virtualenvs, `node_modules`, caches) are not synced. Build outputs are synced but not backed up (`backupnot`).
- Changes to the sync rules leave existing backups alone; deleting backups is left to the cleanup tool in `.zsh.d/tools.zsh`, which asks first.
- Update the ignore rules when adding or retiring a tool that generates files.

### `jazzy-dev` AI agent plugin

- Guidance that holds in every repository goes into the plugin, and guidance specific to one repository into its `AGENTS.md`; neither repeats what Ponytail or Caveman already say. The plugin adds to them and wins where they disagree, and a repository's own `AGENTS.md` wins over the plugin where it is more specific.
- The always-on guidelines are a skill (`skills/jazzy-dev/SKILL.md`) that the SessionStart hook injects, as Ponytail does with its core, not a rules file: `SKILL.md` is a format Claude Code and Codex share while rules files are Claude Code's own, and a skill still loads where the hook doesn't run (e.g., Codex before its hooks are trusted). The workflows are skills loaded on demand, so their full text costs nothing until a commit or pull request happens.
- `agents/` is both the marketplace and the plugin root, installed through each client's own plugin system, the mechanism Ponytail and Caveman use too, so there are no links into the clients' directories to maintain. The price is a cached copy per client (see Repository Structure for releasing a change).
- `agents/` holds no `AGENTS.md`: Codex would load it as instructions for this repository whenever it works in that directory.
- `commit-workflow` runs the commit process and has Caveman's `caveman-commit` write the subject and the body, then adds `REFERENCES` and `QA/TEST`: Caveman keeps the message style, and the workflow keeps the steps `caveman-commit` lacks (picking the files, the upkeep check, agreeing on the message, committing). Neither client can hide a plugin's skill from automatic selection, so the always-on guidelines send every commit to `commit-workflow` first.

## Development Workflow

- It is recommended that commits follow the `.git-commit-template`. Specific to this repository:
    - Subjects are Conventional Commits, even though older ones use bracketed scopes (`[emacs]`): don't follow the history's format. Scopes are the area names those older subjects use (e.g., `emacs`, `zsh`, `ai`).
    - `QA/TEST` lists the checks from Verification that were run, and says which were not (e.g., the other machine).
