############################## aws ##############################################
#
# AWS Odin Credentials
#
alias j-aws-o-laguna='export AWS_CREDENTIALS_ODIN=com.amazon.access.a9-search-relevance-laguna-dev-1'
alias j-aws-o-rel='export AWS_CREDENTIALS_ODIN=com.a9.relevance.common.aws'
alias j-aws-o-anal-qnrmuc='export AWS_CREDENTIALS_ODIN=com.amazon.access.search-analytics-search-qnr-muc-1'

#
# AWS Console Access
#
# can add `--with-admin`, `--with-read-only` or `--with-power-user`
alias j-aws-c-rel='open $(=ssh vmous.aka.corp.amazon.com /apollo/bin/env -e envImprovement aws-console-access com.a9.relevance.common.aws --with-admin)'
#alias j-aws-c-infra='open $(=ssh vmous.aka.corp.amazon.com /apollo/bin/env -e envImprovement aws-console-access ec2-a9-search-infra-account --with-admin)'

#
# AWS EC2
#
alias j-aws-p2-start='j-aws-o-rel && /apollo/env/AmazonAwsCli/bin/aws --region us-east-1 ec2 start-instances --instance-ids i-0cec0591dbada1c76'

#
# AWS S3
#

# Workaround for https://github.com/aws/aws-cli/pull/1122
function j-aws-s3-multipart-workaround {
  if [[ ${#} -ne 1 ]]; then
    echo "Please provide the threshold"
    echo "Example: $0 <threshold>"
    echo "where <threshold> is a threshold in bytes (e.g., 521MB or 1GB)"
    return
  fi

  local THRESHOLD=${1}
  local CMD="/apollo/env/AmazonAwsCli/bin/aws configure set s3.multipart_threshold ${THRESHOLD}"

  eval ${CMD}
}

function j-aws-s3-get {
  local USAGE="Usage: $0 <krypt|kms> <link>,\nwhere <enc> can be either \"krypt\" or \"kms\" and <link> is the link provided in the S3 object's overviwe console page (e.g. 'https://s3.amazonaws.com/<bucket>/<key>')"

  if [[ ${#} -ne 2 ]]; then
    echo "Wrong number of arguments."
    echo ${USAGE}
    return
  fi

  local ENC=""
  if [[ "${1}" = "krypt" ]]; then
    # nothing to do
  elif [[ "${1}" = "kms" ]]; then
    ENC="-k us-east-1 -e KMS -m \"dcda7203-d69b-4014-a560-377f9bbd3b5f\""
  else
    echo "Unsupported encoding argument \"${1}\"."
    echo ${USAGE}
    return
  fi

  local URL=${2}
  local ENDPOINT="$(echo ${URL} | sed -e's,^\(https://s3.amazonaws.com/\).*,\1,g')"
  URL=$(echo ${URL} | sed -e s,${ENDPOINT},,g)
  local BUCKET="$(echo ${URL} | sed -e's,^\([^/]*\).*,\1,g')"
  URL=$(echo ${URL} | sed -e s,${BUCKET},,g)
  local KEY=$(echo ${URL} | sed -e s,^/,,g)
  local FILE=$(echo ${KEY} | sed -e s,/,.,g)

  local CMD="s3GetEncrypted -b ${BUCKET} -o ${KEY} ${ENC} -c com.a9.relevance.common.aws --region us-east-1 --endpoint s3.amazonaws.com > ${FILE}"

  #  s3GetEncrypted -b a9-behavior-driven-matching-alpha -o projects/bdm/prediction/input-xdf-features/eu/eu-kitchen-in.csv -k us-east-1 -c com.a9.relevance.common.aws -m com.a9.relevance.common.crypt_keypair > 20170717_eu-kitchen-in.csv

  # 3GetEncrypted --bucket a9-behavior-driven-matching-prod --object=projects/bdm/widget/${REGION}/low-precision-${MID}.tsv --credential=com.a9.relevance.common.aws low-precision-${MID}.tsv

  echo "Executing: ${CMD}"
  eval ${CMD}
}

function j-aws-s3-put-all {
    local USAGE="Usage: $0 <krypt|kms> <local_dir> <s3_root_uri>,\nwhere <enc> can be either \"krypt\" or \"kms\" and <link> is the link provided in the S3 object's overviwe console page (e.g. 'https://s3.amazonaws.com/<bucket>/<key>')"

  if [[ ${#} -ne 3 ]]; then
    echo "Wrong number of arguments."
    echo ${USAGE}
    return
  fi

  local ENC=""
  if [[ "${1}" = "krypt" ]]; then
    # nothing to do
  elif [[ "${1}" = "kms" ]]; then
    ENC="-k us-east-1 -e KMS -m \"dcda7203-d69b-4014-a560-377f9bbd3b5f\""
  else
    echo "Unsupported encoding argument \"${1}\"."
    echo ${USAGE}
    return
  fi

  local LOCAL_DIR=${2}
  if [[ -e ${LOCAL_DIR} ]]; then
      # do stuff
  else
    echo "Directory \"${2}\" not found."
    echo ${USAGE}
    return
  fi

  local URL=${3}
  echo $URL
  local ENDPOINT="$(echo ${URL} | sed -e's,^\(https://s3.amazonaws.com/\).*,\1,g')"
  echo $ENDPOINT
  local BUCKET="$(echo ${URL} | sed -e's,^\([^/]*\).*,\1,g')"
  echo $BUCKET
  URL=$(echo ${URL} | sed -e s,${BUCKET},,g)
  echo $URL
  local KEY=$(echo ${URL} | sed -e s,^/,,g)
  echo $KEY
  local FILE=$(echo ${KEY} | sed -e s,/,.,g)
  echo $FILE
}

function j-aws-s3-rm-cons {
    local USAGE="Usage: ${0} <link>\nwhere <link> is the link provided in the S3 object's overviwe console page (e.g. 'https://s3.amazonaws.com/<bucket>/<key>')"

  if [[ ${#} -ne 1 ]]; then
    echo "Wrong number of arguments."
    echo ${USAGE}
    return
  fi

  local URL=${1}
  local ENDPOINT="$(echo ${URL} | sed -e's,^\(https://s3.amazonaws.com/\).*,\1,g')"
  URL=$(echo ${URL} | sed -e s,${ENDPOINT},,g)
  local BUCKET="$(echo ${URL} | sed -e's,^\([^/]*\).*,\1,g')"
  URL=$(echo ${URL} | sed -e s,${BUCKET},,g)
  local KEY=$(echo ${URL} | sed -e s,^/,,g)
  local FILE=$(echo ${KEY} | sed -e s,/,.,g)
  local S3_URI="s3://${BUCKET}/${KEY}"

  local CMD="/apollo/bin/env -e AmazonAwsCli aws s3 rm"

  IS_FOLDER=`/apollo/bin/env -e AmazonAwsCli aws s3 ls ${B}`
  while true; do
    echo -n "Recursively remove (if applicable)? [y/n]: "
    read YN
    case ${YN} in
      [Yy]* ) echo "Recursively removing..."; CMD="${CMD} --recursive"; break;;
      [Nn]* ) return;;
      * ) echo "Please answer yes or no.";;
    esac
  done

  CMD="${CMD} s3://${BUCKET}/${KEY}"

  echo "Executing: ${CMD}"
  eval ${CMD}
}

#
# AWS EMR
#
function j-aws-emr-tunnel {
  local USAGE="Usage: $0 <pem_file> <masternodedns>"

  if [[ ${#} -ne 2 ]]; then
    echo "Wrong number of arguments."
    echo ${USAGE}
    return
  fi

  local CMD="ssh -o \"StrictHostKeyChecking no\" -i ${1} -ND 8157 hadoop@${2} &"
  echo "Executing: ${CMD}"

  eval ${CMD}
}

function j-aws-emr-webuis {
  local USAGE="Usage: $0 <pem_file> <masternodedns>"

  if [[ ${#} -ne 2 ]]; then
    echo "Wrong number of arguments."
    echo ${USAGE}
    return
  fi

  j-aws-emr-tunnel ${1} ${2}

  sleep 2

  echo "---------------------------------------------"
  echo "| Interface             | URI"
  echo "---------------------------------------------"
  echo "| Yarn Resource Manager | http://${2}:8088";
  open http://${2}:8088
  #echo "| Yarn Node Manager     | http://<slave-public-dns-name>:8042"
  echo "| Hadoop HDFS NameNode  | http://${2}:50070";
  open http://${2}:50070
  #echo "| Hadoop HDFS DataNode  | http://<slave-public-dns-name>:50075"
  echo "| Spark HistoryServer   | http://${2}:18080";
  open http://${2}:18080
  echo "| Zeppellin             | http://${2}:8890";
  open http://${2}:8890
  echo "| Jupiter               | http://${2}:8880";
  open http://${2}:8880
  #echo "| Hue                   | http://${2}:8888";
  #open http://${2}:8888
  echo "| Ganglia               | http://${2}/ganglia/";
  open http://${2}/ganglia/
  #echo "| HBase UI              | http://${2}:16010";
  #open http://${2}:16010
}
