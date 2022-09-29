export BRAZIL_WORKSPACE_DEFAULT_LAYOUT=short

# override brazil platform to use RHEL5 artifacts by default
# according to: https://w.amazon.com/index.php/Search/A9/Infra/CorpDevBoxes#Amazon_Linux
#export BRAZIL_PLATFORM_OVERRIDE=RHEL5_64

# workspace
alias j-bwj='cd /workplace/vmous/'
alias j-bw='brazil ws'
alias j-bwc='j-bw --create'
alias j-bwcl='j-bw --clean'
alias j-bwd='j-bw --delete'
alias j-bwdr='j-bw --dryrun'
alias j-bwdru='j-bwdr --unoptimized'
alias j-bws='j-bw --show'
alias j-bwsm='j-bw --sync --md'
alias j-bwu='j-bw --use'
alias j-bwr='j-bw --remove'
alias j-bwea='j-bw attachenvironment --alias'
alias j-bwed='j-bw detachenvironment --alias'
alias j-bwc-a9='j-bwc -vs A9Search/main'
alias j-bwc-a9-meta='j-bwc -vs A9Search/QueryConfigRelease-certified'
alias j-bwc-bdm-bucketer='j-bwc -vs A9SearchBDMBucketBuilder/release'
alias j-bwc-bdm-predict='j-bwc -vs A9SearchBDMPredictionBuild/release'
alias j-bwc-rsas='j-bwc -vs RemoteSearchAggregator/release'
alias j-bwc-bdm-predict-container-build='j-bwc -vs A9SearchBDMPredictionBuild/container-build'

# versionset
alias j-bvrup='brazil vs --removeunusedpackages --vs'

# package cache
alias j-bpc='brazil-package-cache'
alias j-bpcd='j-bpc disable_edge_cache'
alias j-bpce='j-bpc enable_edge_cache'
alias j-bpcs='j-bpc start'
alias j-bpcr='j-bpcd && j-bpce && j-bpcs'

# build
alias bb='brazil-build'
alias j-bb='bb'
alias j-bbb='j-bb build'
alias j-bbc='j-bb clean'
alias j-bbt='j-bb test'
alias j-bbtv='j-bb test-valgrind'
alias j-bba='j-bb apollo-pkg'
alias j-bbr='j-bb release'
alias j-bb-ensime='j-bb scala-dev-init'
alias j-bb-cbtr='j-bbc 2>&1 | tee clean.log && j-bb 2>&1 | tee build.log && j-bbt 2>&1 | tee test.log && j-bbr 2>&1 | tee release.log'

# virtual environment
alias j-b-venv='SYMLINK_FARM=$(brazil-path "graphName=testrun;recipe=runtimefarm;excludeRoot=true") && ln -s ${SYMLINK_FARM} .venv'

# misc.
alias j-brc='brazil-recursive-cmd'
alias j-brc-all='j-brc --allPackages'

alias j-brcp='brazil-recursive-cmd-parallel -j 8'
alias j-brcp-all='j-brcp --allPackages'

alias bre='brazil-runtime-exec'
alias j-bre='bre'

alias j-bo='/apollo/env/OctaneBrazilTools/bin/brazil-octane'
