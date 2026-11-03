# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## What This Repo Is

Personal dotfiles and *nix configuration, symlinked into `$HOME` (see Configuration Management). The repo is expected to live at `~/Workspace/personal/_nix-config`.

Configuration targets three machine types detected at shell startup via `$JMACHINE`: `mac`, `worklinux`, `homelinux`.

## Safety and Boundaries

- **Secrets:** `.emacs.d/secrets.el` in the repo is a template; the real one is a private, non-symlinked `~/.emacs.d/secrets.el`. Real values (hostnames, credentials, tokens) never go into the repo or into output, and config refers to them only through `jazzy/secrets/...` constants.
- **Files other tools own:** don't edit employer-managed files such as `~/.ssh/config`. Leave blocks added by tools (e.g. "managed by AIM CLI") alone and out of commits unless asked. The worktree holds many untracked local files, so commit only the files the user selects.
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

Each entry says how to verify changes to it (see also Verification).

- `ansible/`, `ansible.cfg` — Playbooks (alpha, see Configuration Management). `main.yml` is the entry point; it imports `macos.yml` and `linux.yml` conditionally. Verify with `ansible-playbook --syntax-check ansible/main.yml`, then a `--check` dry run.
- `.zshrc` / `.zshenv` / `.zsh.d/` — Zsh configuration. Platform-specific blocks are gated on `$JMACHINE`. The `.zsh.d/amzn/` subdirectory holds work-specific shell extensions. Verify syntax with `zsh -n <file>`, the resulting environment in a fresh login shell (`zsh -lic '...'`), and functions against temporary fixtures with stubbed prompts.
- `.alias` — Shell aliases and utility functions sourced by `.zshrc`. Verify with `zsh -n .alias`.
- `.bashrc` / `.bash_profile` — Bash configuration. Verify with `bash -n <file>`.
- `.emacs` / `.emacs.d/` — Emacs configuration split by language/domain (e.g., `core.el`, `python.el`, `scala.el`). Verify by loading the full configuration in batch mode (`emacs --batch --eval '(load "~/.emacs")'`) and checking for errors and warnings. On the Mac, simulate a Dock launch with launchd's minimal environment: `env -i HOME=$HOME USER=$USER SHELL=/bin/zsh PATH=/usr/bin:/bin /Applications/Emacs.app/Contents/MacOS/Emacs --batch ...`.
- `.tmux.conf` — tmux configuration. `tmux source-file -n <file>` checks syntax only. To also catch invalid options, source a copy without the `run` line (it starts the plugins, and tmux-continuum would save or restore sessions) into a throwaway server: `tmux -L check -f /dev/null new-session -d 'sleep 20'`, then `tmux -L check source-file <copy>` and `tmux -L check kill-server`. Never source it into the running server.
- `.config/herdr/config.toml` — herdr configuration. Verify with `herdr config check`; to check a copy, place it at `<dir>/herdr/config.toml` and run `XDG_CONFIG_HOME=<dir> herdr config check`. Apply with `herdr server reload-config`.
- `.unison/` / `Library/LaunchAgents/` — Unison sync profiles between the Mac and the cloud desktop, and the launchd agents that run them. Verify a profile on a copy: drop its `root`, `repeat` (`watch` never exits) and backup lines, point `root` and `backupdir` at temporary directories, and run `UNISON=<dir> unison <profile> -batch` under a timeout; never against the real roots. Verify plists with `plutil -lint`.
- `.gitconfig` / `.gitconfig-aliases.sh` / `.gitconfig.amzn` — Git aliases and settings. Work-specific overrides are conditionally included from `.gitconfig.amzn` for paths under `~/Workspace/work/`. Verify with `git config --file <file> --list`.
- `.git-commit-template` — Structured commit message template (used by git via `commit.template`).
- `.claude/rules/` — Commit and pull request workflows for Claude agents.
- `.rtorrent.rc`, `.i3/`, `.nice-dcv/`, `openvpn`, `sources.list`, `Library/Spelling/` — Configuration for tools that aren't installed on every machine. There is no automated check: review changes against the tool's documentation, check shell scripts (e.g., `.nice-dcv/launch.sh`) with `zsh -n`, and test on a machine where the tool is installed.

## Verification

Verify every change before reporting it done, using the checks for its area in Repository Structure.

- Test against copies, temporary directories and throwaway servers, never against live state: running tmux or herdr sessions, real Unison roots or backups, or real data for destructive commands.
- When a change affects both `mac` and `worklinux`, check it on both, or say which one was not checked.
- Report what was verified and what wasn't.

## Commit Message Convention

Commits use the template in `.git-commit-template`. The subject line is prefixed with a bracketed scope indicating what area changed, e.g. `[zsh]`, `[emacs]`, `[git]`, `[ansible]`. See recent `git log` for examples.

## Key Design Decisions

- Dotfiles are **symlinked** into `$HOME`, not copied. Edits should happen in this repo.
- Oh-My-Zsh is expected at `~/.oh-my-zsh` but is not tracked here (listed in `untracked_conf_files`).
- `compinit` must remain near the end of `.zshrc` — completion configuration before it is honored; configuration after it is ignored.
- Shared zsh logic lives in `.zsh.d/utils.zsh`, sourced early by `.zshrc`. Before writing zsh scripts, functions or aliases, read that file for the current set of helpers and reuse them instead of re-implementing them; logic that other places could use should be added there as a new helper.
- SSH agent socket is kept stable across tmux reattaches via a symlink at `~/.ssh/ssh_auth_sock`.
- tmux (`.tmux.conf`) and herdr (`.config/herdr/config.toml`) are kept in step. When adjusting either, make the same workflow available in the other as far as it supports it (e.g., the `C-a` prefix, `|`/`-` splits, `h`/`j`/`k`/`l` pane navigation), so a pattern learned in one works in both.
