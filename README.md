# _nix-config

Remembering the configuration of my *nix installations.

## Configuring with Ansible

This repository uses Ansible ([Ansible@GitHub](https://github.com/ansible/ansible), [docs](https://docs.ansible.com/)) to apply configuration.

First install Ansible on your system (e.g., `brew install ansible` for MacOS or `sudo dnf install ansible` for Linux distributions - or use your distribution's preferred package manager) and then run the main runbook like so:
```sh
ansible-playbook ansible/main.yml
```

IMPORTANT: Use the following for debugging your Ansible scripts
- `--check`: do a dry run
- `--tags "${TAG1},${TAG2}"`: run tasks tagged with `${TAG1}` and `${TAG2}`.
- `--skip-tags "${TAG3}"`: run all tasks except those tagged with `${TAG3}`
- `-vvv`: add debugging information
