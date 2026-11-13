############################## tools ###########################################

#
# Unison
#

function _unison_local_roots {
  # Print the local root of every Unison profile, one per line and without
  # duplicates: each `root` value that is not a remote URI (e.g. `ssh://...`),
  # with a leading `~` expanded and trailing slashes removed.
  local _unison_dir="${UNISON:-${HOME}/.unison}"
  local -a _profiles=( "${_unison_dir}"/*.prf(N) )
  (( ${#_profiles} )) || return 0
  sed -nE 's/^[[:space:]]*root[[:space:]]*=[[:space:]]*(.*[^[:space:]])[[:space:]]*$/\1/p' \
    "${_profiles[@]}" \
    | grep -v '://' \
    | sed -e "s|^~|${HOME}|" -e 's|/*$||' \
    | sort -u
}

function j-tools-cleanup-orphaned-unison-backup {
  # Delete Unison backups that belong to projects no longer worked on.
  #
  # With `backup = Name *`, Unison keeps copies of overwritten or deleted files
  # in its central backup directory, mirroring each path relative to the root of
  # the profile that synced it. A top-level backup entry is therefore named
  # after a top-level entry of a root, prefixed with `.bak.<N>.` when Unison
  # backed up that whole folder or file. Entries matching no top-level entry of
  # any local root belong to projects that are gone; they are listed and
  # deleted only after confirmation.
  local _unison_dir="${UNISON:-${HOME}/.unison}"
  local _backup_dir="${_unison_dir}/backup"

  if [[ ! -d "${_backup_dir}" ]]; then
    echo_warning "No Unison backup directory at ${_backup_dir}. Nothing to do."
    return 0
  fi

  local -a _roots=( ${(f)"$(_unison_local_roots)"} )
  if (( ${#_roots} == 0 )); then
    echo_error "No local Unison roots found in ${_unison_dir}/*.prf. Aborting."
    return 1
  fi
  echo_info "Projects currently worked on are the top-level entries of:"
  printf '  %s\n' "${_roots[@]}"

  # (DN): include dot-entries (e.g. `.bak.1.<name>`) and expand to nothing when
  # the backup directory is empty.
  local -a _orphans
  local _entry _name _root
  for _entry in "${_backup_dir}"/*(DN); do
    _name="${${_entry:t}#.bak.<->.}"
    for _root in "${_roots[@]}"; do
      [[ -e "${_root}/${_name}" ]] && continue 2
    done
    _orphans+=( "${_entry}" )
  done

  if (( ${#_orphans} == 0 )); then
    echo_info "All backups in ${_backup_dir} belong to current projects. Nothing to do."
    return 0
  fi

  echo_info "Found ${#_orphans} backup entries in ${_backup_dir} whose projects are no longer present (measuring their size, this can take a while):"
  local _kb _total_kb=0
  for _entry in "${_orphans[@]}"; do
    _kb=$(du -sk "${_entry}" 2>/dev/null | cut -f1)
    (( _total_kb += ${_kb:-0} ))
    printf '  %10s  %s\n' "$(kb_to_human ${_kb})" "${_entry:t}"
  done

  if [[ $(yes_or_no "Do you want to delete the above backup directories? It will release $(kb_to_human ${_total_kb}) of storage.") == "y" ]]; then
    run rm -rf -- "${_orphans[@]}"
  else
    echo_info "Nothing deleted."
  fi
}

#
# All cleanups
#

function j-tools-cleanup {
  # Run every disk cleanup tool in one go. Only supported on the Mac for now.
  # A failing step does not stop the others; the overall status is non-zero if
  # any of them failed.
  if [[ "${JMACHINE}" != "mac" ]]; then
    echo_warning "j-tools-cleanup is only supported on the Mac for now. Nothing to do."
    return 0
  fi

  local _status=0

  echo_info "Cleaning up Unison backups of projects no longer present..."
  j-tools-cleanup-orphaned-unison-backup || _status=1

  echo_info "Cleaning up the Brazil package cache..."
  if (( ${+functions[bpcclean]} )); then
    bpcclean || _status=1
  else
    echo_warning "\`bpcclean\` is not defined (is .zsh.d/amzn/brazil.zsh sourced?). Skipping."
  fi

  return ${_status}
}

#
# AI coding agents
#

function _j-tools-ai-plugin {
  # Install the plugin $2 (e.g. `ponytail@ponytail`) from the marketplace $3
  # (a GitHub repository such as `DietrichGebert/ponytail`, or a local path)
  # into the client $1 (`claude` or `codex`), unless it is installed already.
  local _client="${1}" _plugin="${2}" _repo="${3}"
  local _install=install
  [[ "${_client}" == "codex" ]] && _install=add

  if "${_client}" plugin list --json 2>/dev/null | grep -q "\"${_plugin}\""; then
    echo_info "${_client}: ${_plugin} is already installed."
    return 0
  fi
  if ! "${_client}" plugin marketplace list --json 2>/dev/null | grep -q "${_repo}"; then
    run "${_client}" plugin marketplace add "${_repo}" || return 1
  fi
  run "${_client}" plugin "${_install}" "${_plugin}"
}

function j-tools-ai-claude-settings {
  # Apply the Claude Code preferences below to ~/.claude/settings.json and leave
  # every other key alone: AIM, plugins, herdr and Claude Code itself write the
  # rest, so the file is not linked from this repo. Safe to re-run.
  #
  # The output style is Concise, unless Caveman is installed in Claude Code and
  # the user chooses it instead: using both doubles the brevity instructions.
  local _settings="${HOME}/.claude/settings.json" _style=Concise _caveman _changes
  local _wanted='{
    "theme": "auto",
    "includeCoAuthoredBy": false,
    "tui": "fullscreen"
  }'

  if ! cmd_exists claude || ! cmd_exists jq; then
    echo_warning "claude: Claude Code and jq are needed to update ${_settings}. Skipping."
    return 1
  fi

  _caveman=$(claude plugin list --json 2>/dev/null \
    | jq -r '.[] | select(.id == "caveman@caveman") | .enabled')
  if [[ -n "${_caveman}" ]]; then
    if [[ $(yes_or_no "Caveman is installed in Claude Code. Do you want to use it instead of the Concise output style?") == "y" ]]; then
      _style=""
      [[ "${_caveman}" == "true" ]] || run claude plugin enable caveman@caveman --scope user || return 1
    else
      [[ "${_caveman}" == "false" ]] || run claude plugin disable caveman@caveman --scope user || return 1
    fi
  fi

  [[ -f "${_settings}" ]] || print '{}' > "${_settings}"
  _wanted=$(jq -c --arg s "${_style}" \
    'if $s != "" then . + {outputStyle: $s} else . end' <<< "${_wanted}") || return 1
  _changes=$(jq -nr --argjson w "${_wanted}" --arg s "${_style}" --slurpfile cur "${_settings}" \
    '($w | to_entries[] | select(.value != $cur[0][.key])
      | "  \(.key): \($cur[0][.key]) -> \(.value)"),
     (select($s == "" and ($cur[0] | has("outputStyle")))
      | "  outputStyle: \($cur[0].outputStyle) -> (none)")') || return 1
  if [[ -z "${_changes}" ]]; then
    echo_info "claude: the settings are already set."
    return 0
  fi
  echo_info "claude: updating ${_settings}:"
  print -r -- "${_changes}"
  jq --argjson w "${_wanted}" --arg s "${_style}" \
    '. + $w | if $s == "" then del(.outputStyle) else . end' \
    "${_settings}" > "${_settings}.tmp" \
    && mv "${_settings}.tmp" "${_settings}"
}

function j-tools-ai-setup {
  # Install herdr, Ponytail, Caveman and this repo's jazzy-dev plugin (agents/)
  # into the AI coding agents found on this machine (Claude Code, Codex), and
  # set the Claude Code preferences.
  # Safe to re-run: anything installed already is skipped. Clients missing here
  # are skipped too.
  #
  # Caveman is installed into Claude Code disabled; j-tools-ai-claude-settings,
  # run last, asks whether to use it instead of the Concise output style.
  local _status=0 _client
  local _codex_home="${CODEX_HOME:-${HOME}/.codex}"

  if [[ $(yes_or_no "Install herdr, the Ponytail and Caveman plugins (third-party code) and the jazzy-dev plugin into the AI agents on this machine?") != "y" ]]; then
    echo_info "Nothing installed."
    return 0
  fi

  echo_info "herdr..."
  if ! cmd_exists herdr; then
    if [[ "${JMACHINE}" == "mac" ]]; then
      run brew install herdr || _status=1
    else
      # herdr's installer puts it in ~/.local/bin; `herdr update` keeps it
      # current afterwards.
      run sh -c 'curl -fsSL https://herdr.dev/install.sh | sh' && rehash || _status=1
    fi
  fi
  if cmd_exists herdr; then
    for _client in claude codex; do
      cmd_exists "${_client}" || continue
      if herdr integration status 2>/dev/null | grep -q "^${_client}: current"; then
        echo_info "herdr: the ${_client} integration is already installed."
      else
        run herdr integration install "${_client}" || _status=1
      fi
    done
  fi

  echo_info "Ponytail..."
  if cmd_exists claude; then
    _j-tools-ai-plugin claude ponytail@ponytail DietrichGebert/ponytail || _status=1
  fi
  if cmd_exists codex; then
    _j-tools-ai-plugin codex ponytail@ponytail DietrichGebert/ponytail || _status=1
  fi

  echo_info "Caveman..."
  if cmd_exists claude; then
    # Installed disabled: j-tools-ai-claude-settings, run last, asks whether to
    # use it instead of the Concise output style.
    if claude plugin list --json 2>/dev/null | grep -q '"caveman@caveman"'; then
      echo_info "claude: caveman@caveman is already installed."
    else
      _j-tools-ai-plugin claude caveman@caveman JuliusBrussee/caveman \
        && run claude plugin disable caveman@caveman --scope user || _status=1
    fi
  fi
  if cmd_exists codex; then
    if [[ -d "${_codex_home}/caveman" ]]; then
      echo_info "codex: Caveman is already installed."
    else
      run npx -y github:JuliusBrussee/caveman -- --only codex || _status=1
    fi
  fi

  echo_info "jazzy-dev..."
  for _client in claude codex; do
    cmd_exists "${_client}" || continue
    _j-tools-ai-plugin "${_client}" jazzy-dev@jazzy-dev \
      "${HOME}/Workspace/personal/_nix-config/agents" || _status=1
  done

  if cmd_exists claude; then
    echo_info "Claude Code settings..."
    j-tools-ai-claude-settings || _status=1
  fi

  if cmd_exists codex; then
    echo_info yellow "In Codex, open /hooks and trust the Ponytail, Caveman and jazzy-dev hooks."
  fi
  echo_info yellow "Start new sessions in each agent to pick up the changes."
  return ${_status}
}
