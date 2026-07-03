############################## apollo ###########################################
alias j-afresh='cd $PWD' # useful inside the Apollo symlink farms
# watch build deployment
alias j-awbd='watch -t '\''ls -drt /apollo/var/logs/apollo-update* | xargs tail | strings | fold -w $COLUMNS | tail -n $LINES'\'''
function j-awpp {
    # https://w.amazon.com/index.php/Apollo/Docs/TroubleshootingDeployments#Pull_step_progress
    ssh -o ConnectTimeout=1 $1 'tail -f /apollo/var/logs/apollo-update.root.log.*(om[1]) | grep --line-buffered \ "Getting\|exit code" \
        | sed -u "s/pullPackage .*: Getting package \(.*\) by\|from .*/Pull Started: \1/" \
        | sed -u "s/pullPackage .*: .*exit code: \(.*\)/Pull Ended:   Exit Code: \1/"'
}
# get host class
alias j-aghc='/apollo/bin/getmyhostclass'
# host control
alias j-ahc='/apollo/env/ApolloCommandLine/bin/apolloHostControlCLI'
# environment activate
alias j-aea='sudo /apollo/bin/runCommand -a Activate -e'
# environment de-activate
alias j-aed='sudo /apollo/bin/runCommand -a Deactivate -e'
function j-acs {
  # Commands are prefixed with "dry" so they are only printed, not executed.
  # Remove the "dry" prefix from a command to actually run it.
  echo "According to https://w.amazon.com/index.php/Apollo/HowTo/CleanAHost"
  run dry sudo /apollo/sbin/apolloLocalHostControl --status StayDown \
    && run dry sudo /apollo/bin/cleanupOldEnvSymlinks --all \
    && run dry sudo /apollo/sbin/apolloLocalHostControl --status StayDown --rebooted \
    && run dry sudo /apollo/sbin/apolloLocalHostControl --status Active \
    && run dry sudo rm -rf /local/apollo/_env/to_be_removed/*
}
