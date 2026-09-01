# AGENTS.md

This file provides guidance to AI coding agents when working with code in this repository.

## What This Repo Is

Personal dotfiles and *nix configuration managed via Ansible. The repo is expected to live at `~/Workspace/personal/_nix-config` — Ansible enforces this location at runtime.

Configuration targets three machine types detected at shell startup via `$JMACHINE`: `mac`, `worklinux`, `homelinux`.

## Applying Configuration

```sh
ansible-playbook ansible/main.yml
```

Useful flags: `--check` (dry run), `--tags "shared,dotfiles"`, `--skip-tags "linux"`, `-vvv` (debug).

Ansible runs locally (`inventory.ini` points to `localhost`). The playbook symlinks dotfiles from this repo into `$HOME`, backing up any existing non-symlink files first.

## Repository Structure

- `ansible/` — Playbooks. `main.yml` is the entry point; it imports `macos.yml` and `linux.yml` conditionally.
- `.zshrc` / `.zshenv` / `.zsh.d/` — Zsh configuration. Platform-specific blocks are gated on `$JMACHINE`. The `.zsh.d/amzn/` subdirectory holds work-specific shell extensions.
- `.emacs` / `.emacs.d/` — Emacs configuration split by language/domain (e.g., `core.el`, `python.el`, `scala.el`).
- `.gitconfig` — Git aliases and settings. Work-specific overrides are conditionally included from `.gitconfig.amzn` for paths under `~/Workspace/work/`.
- `.alias` — Shell aliases and utility functions sourced by `.zshrc`.
- `.git-commit-template` — Structured commit message template (used by git via `commit.template`).

## Commit Message Convention

Commits use the template in `.git-commit-template`. The subject line is prefixed with a bracketed scope indicating what area changed, e.g. `[zsh]`, `[emacs]`, `[git]`, `[ansible]`. See recent `git log` for examples.

## Key Design Decisions

- Dotfiles are **symlinked** into `$HOME`, not copied. Edits should happen in this repo.
- Oh-My-Zsh is expected at `~/.oh-my-zsh` but is not tracked here (listed in `untracked_conf_files`).
- `compinit` must remain near the end of `.zshrc` — completion configuration before it is honored; configuration after it is ignored.
- SSH agent socket is kept stable across tmux reattaches via a symlink at `~/.ssh/ssh_auth_sock`.
