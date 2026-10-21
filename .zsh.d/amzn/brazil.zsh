export BRAZIL_WORKSPACE_DEFAULT_LAYOUT=short

# build
alias bb=brazil-build
alias bbc='bb clean'
alias bbt='bb test'
alias bbr='bb release'
alias bba='brazil-build apollo-pkg'
alias bre='brazil-runtime-exec'

# workspace
alias bwsj='cd ~/Workspace/work/'
alias bws='brazil ws'
alias bwscreate='bws create -n'
alias bwsuse='bws use -p'
alias bwsremove='bws remove -p'

# recursive
alias brc=brazil-recursive-cmd
alias brcb='brc brazil-build'
alias brcall='brc --allPackages'
alias brcball='brcall brazil-build'
alias bbra='brcb apollo-pkg'

alias brcp='brazil-recursive-cmd-parallel -j 8'
alias brcpall='brcp --allPackages'

# purge cached packages older than 30 days, keeping any used in the last 24h
function bpcclean {
  # Ask before cleaning, showing how many package versions are unused for more
  # than 30 days and their size. The package cache daemon decides what it actually
  # purges, so that size is an estimate (based on when each version folder was
  # last touched); the space really released is reported afterwards.
  local _bpc=${HOME}/.toolbox/bin/brazil-package-cache
  local _packages="$(${_bpc} cache_root 2>/dev/null | tail -1)"
  _packages="${_packages%/}/packages"
  if [[ ! -d "${_packages}" ]]; then
    echo_error "Cannot find the Brazil package cache (${_packages}). Aborting."
    return 1
  fi

  # (N/m+30): directories last modified more than 30 days ago, nothing if none.
  local -a _stale=( "${_packages}"/*/*(N/m+30) )
  if (( ${#_stale} == 0 )); then
    echo_info "No cached packages unused for more than 30 days in ${_packages}. Nothing to clean."
    return 0
  fi

  local _estimate_kb=$(du -skc "${_stale[@]}" 2>/dev/null | tail -1 | cut -f1)
  echo_info "${#_stale} cached package version(s) in ${_packages} have not been used for more than 30 days."
  if [[ $(yes_or_no "Do you want to delete the cache of these packages? It will release about $(kb_to_human ${_estimate_kb}) of storage.") != "y" ]]; then
    echo_info "Nothing deleted."
    return 0
  fi

  local _before_kb=$(du -sk "${_packages}" 2>/dev/null | cut -f1)
  run ${_bpc} clean --days 30 --keepCacheHours 24 || return 1
  local _after_kb=$(du -sk "${_packages}" 2>/dev/null | cut -f1)
  echo_info "Released $(kb_to_human $(( _before_kb - _after_kb ))) of storage."
}
alias bpcrestart='${HOME}/.toolbox/bin/brazil-package-cache stop && rm ${HOME}/brazil-pkg-cache/daemon-lck && ${HOME}/.toolbox/bin/brazil-package-cache start'

# brazil-path
alias bvenv='SYMLINK_FARM=$(brazil-path "graphName=testrun;recipe=runtimefarm;excludeRoot=true") && ln -s ${SYMLINK_FARM} .venv'

# misc.
alias bbcbtr='j-bbc 2>&1 | tee clean.log && j-bb 2>&1 | tee build.log && j-bbt 2>&1 | tee test.log && j-bbr 2>&1 | tee release.log'
