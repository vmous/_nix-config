############################## a9 ###############################################
#
# Laguna
#
alias j-laguna='/apollo/bin/env -e Laguna-Client laguna_emr'
alias j-laguna-ls='j-laguna list-clusters'
alias j-laguna-create='j-laguna create-cluster -c'
function j-laguna-create-jazzy {
  local USAGE="Usage: $0 <cluster_name> <num_instances>"
  if [[ ${#} -ne 2 ]]; then
    echo "Wrong number of arguments."
    echo ${USAGE}
    return
  fi

  local CLUSTER_NAME=${1}
  local NUM_INSTANCES=${2}; shift; shift;
  local CMD="j-laguna create-cluster -c ${CLUSTER_NAME} -i ${NUM_INSTANCES} -m r3.2xlarge -s c3.2xlarge --emr-version emr-5.12.1 --extend-idle-cluster-period ${*}"

  echo "Executing: ${CMD}"
  eval ${CMD}
}
alias j-laguna-terminate='j-laguna terminate-cluster -c'
alias j-laguna-ssh='j-laguna connect-cluster -c'
alias j-laguna-sftp='j-laguna sftp-cluster -c'
function j-laguna-ssh-build {
  if [ $# -eq 0 ]; then
    echo "Please provide the Master Node hostname or DNS alias."
    echo "Example: $0 <master_node>"
    return
  fi

  local MASTER_NODE=$1; shift
  local CMD="ssh -i ~/.ssh/laguna-ec2key.pem ec2-user@${MASTER_NODE} $*"
  eval ${CMD}
}
function j-laguna-scp {
    local CMD="scp -i ~/.ssh/laguna-ec2key.pem "
}

#
# Fresca
#
alias j-fresca-tunnel='ssh -N -D 2600 -f security-bastions-prod-iad.amazon.com -o ServerAliveInterval=60'
alias j-fresca-tunnel-spark-gamma='ssh -N -D 8157 -f -i ~/emr-gamma hadoop@ec2-54-147-186-124.compute-1.amazonaws.com -o ServerAliveInterval=60'

function j-fresca-spark-gamma-ship() {
  local _source_jar="./build/Scala2.11/*.jar"
  local _target_dir="hadoop@ec2-54-147-186-124.compute-1.amazonaws.com:/mnt/user/vmous/"
  if [ ! $# -eq 0 ]; then
    if [ $1 = "jarjar" ]; then
      _source_jar="./build/assembly/*.uber.jar"
    else
      echo "Cannot understand $1. Aborting"
      return
    fi
  fi
  local _cmd="/usr/bin/scp -i ~/emr-gamma ${_source_jar} ${_target_dir}"
  echo "Executing: ${_cmd}"
  eval ${_cmd}
}

function mdb () {
    GET http://a9-mdb.amazon.com/api/middb >mdb.json
    # Install jq first
    # brew install jq
    jq -C <mdb.json
}
