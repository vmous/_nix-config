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

  echo_info "Found ${#_orphans} backup entries in ${_backup_dir} whose projects are no longer present:"
  printf '  %s\n' "${_orphans[@]:t}"

  if [[ $(yes_or_no "Delete these ${#_orphans} backup entries from ${_backup_dir}?") == "y" ]]; then
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
