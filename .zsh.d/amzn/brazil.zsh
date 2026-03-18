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

# package cache
alias bpcclean='${HOME}/.toolbox/bin/brazil-package-cache clean --days 30 --keepCacheHours 24'
alias bpcrestart='${HOME}/.toolbox/bin/brazil-package-cache stop && rm ${HOME}/brazil-pkg-cache/daemon-lck && ${HOME}/.toolbox/bin/brazil-package-cache start'

# brazil-path
alias bvenv='SYMLINK_FARM=$(brazil-path "graphName=testrun;recipe=runtimefarm;excludeRoot=true") && ln -s ${SYMLINK_FARM} .venv'

# misc.
alias bbcbtr='j-bbc 2>&1 | tee clean.log && j-bb 2>&1 | tee build.log && j-bbt 2>&1 | tee test.log && j-bbr 2>&1 | tee release.log'
