############################## aws ##############################################
#
# Amazon AWS CLI
# https://w.amazon.com/bin/view/AmazonAwsCli/
#
alias aws-amzn='/apollo/bin/env -e AmazonAwsCli aws'

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
alias j-aws-p2-start='j-aws-o-rel && aws-amzn --region us-east-1 ec2 start-instances --instance-ids i-0cec0591dbada1c76'

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
  local CMD="aws-amzn configure set s3.multipart_threshold ${THRESHOLD}"

  eval ${CMD}
}

function j-aws-cred-update {
    local _usage="${0} <profile name> <material set>"
    local _aws_cred_file="${HOME}/.aws/credentials"

    if [[ ${#} -ne 2 ]]; then
        echo "Wrong number of arguments."
        echo ${_usage}
        return
    fi

    local _profile="${1}"
    local _materialset="${2}"

    local _profile_conf_line=`grep -n ${_profile} ${_aws_cred_file}`

    if [ "${_profile_conf_line}" = "" ]; then
        echo "Profile ${_profile} not found in ${_aws_cred_file}"
        return
    fi

    local _line="$_profile_conf_line[(ws.:.)1]"
    local _line_plus_one="$((_line + 1))"
    local _line_plus_two="$((_line + 2))"
    local _creds_from_file=`head -n $((_line + 2)) ${_aws_cred_file} | tail -2`
    local _old_creds=("${(f)_creds_from_file}")
    local _line_one=${_old_creds[1]}
    local _line_two=${_old_creds[2]}
    local _old_aws_access_key_id="$_line_one[(ws.=.)2]"
    local _old_aws_secret_access_key="$_line_two[(ws.=.)2]"

    local _odin_get=`odin-get ${_materialset}`
    local _new_creds=("${(f)_odin_get}")
    local _new_aws_access_key_id=${_new_creds[1]}
    local _new_aws_secret_access_key=${_new_creds[2]}

    if [ "${_old_aws_access_key_id}" != "${_new_aws_access_key_id}" ] || [ "${_old_aws_secret_access_key}" != "${_new_aws_secret_access_key}" ]; then
        echo "AWS credential configuration stale."
        echo "'aws_access_key_id': Replacing ${_old_aws_access_key_id} with ${_new_aws_access_key_id}"
        echo "'aws_secret_access_key': Replacing ${_old_aws_secret_access_key} with ${_new_aws_secret_access_key}"
        _sed_str_1="sed -i '${_line_plus_one}s/.*/aws_access_key_id=${_new_aws_access_key_id}/' ${_aws_cred_file}"
        eval ${_sed_str_1}
        _sed_str_2="sed -i '${_line_plus_two}s/.*/aws_secret_access_key=${_new_aws_secret_access_key}/' ${_aws_cred_file}"
        eval ${_sed_str_2}
    else
        echo "AWS credential configuration is up-to-date. Nothing to do."
    fi
}

function j-aws-s3-amzn {
    local _usage="Usage: $0 <get|put> <options>"

    function _j-aws-s3-amzn-get {
        local _options="Available options: <enc> <obj_url>,\nwhere <enc> can be either \"krypt\" or \"kms\" and <obj_url> is the link provided in the S3 object's overviwe console page (e.g. 'https://s3.amazonaws.com/<bucket>/<key>')"

        if [[ ${#} -ne 3 ]]; then
            echo "Wrong number of arguments."
            echo ${_options}
            return
        fi

        local _enc="${2}"
        local _obj_url="${3}"

        if [[ "${_enc}" = "krypt" ]]; then
            _enc=""
        elif [[ "${_enc}" = "kms" ]]; then
            _enc="-k us-east-1 -e KMS -m \"dcda7203-d69b-4014-a560-377f9bbd3b5f\""
        else
            echo "Unsupported encoding argument \"${_enc}\"."
            echo ${_options}
            return
        fi

        local _endpoint="$(echo ${_obj_url} | sed -e's,^\(https://s3.amazonaws.com/\).*,\1,g')"
        _obj_url=$(echo ${_obj_url} | sed -e s,${_endpoint},,g)
        local _bucket="$(echo ${_obj_url} | sed -e's,^\([^/]*\).*,\1,g')"
        _obj_url=$(echo ${_obj_url} | sed -e s,${_bucket},,g)
        local _key=$(echo ${_obj_url} | sed -e s,^/,,g)
        local _file=$(echo ${_key} | sed -e s,/,.,g)

        local _cmd="s3GetEncrypted -b ${_bucket} -o ${_key} ${_enc} -c com.a9.relevance.common.aws --region us-east-1 --endpoint s3.amazonaws.com > ${_file}"

        echo "Executing: ${_cmd}"
        eval ${_cmd}
    }

    function _j-aws-s3-amzn-put {
        local _options="Available options: <enc> <s3_url> <local_object>\nwhere\n- <enc> can be either \"krypt\" or \"kms\",\n- <s3_url> is an S3 URL where the local object is to be uploaded to (e.g. 's3://<bucket>/<key>'), and\n- <loca_object> path to local object that is to be uploaded."

        if [[ ${#} -ne 4 ]]; then
            echo "Wrong number of arguments."
            echo ${_options}
            return
        fi

        local _enc="${2}"
        local _s3_url="${3}"
        local _local_object="${4}"

        if [[ "${_enc}" = "krypt" ]]; then
            _enc="--credential com.amazon.access.a9-search-relevance-laguna-dev-1 --encryption-material-provider Odin --material com.a9.relevance.common.crypt_keypair"
        elif [[ "${_enc}" = "kms" ]]; then
            _enc="--credential com.amazon.access.a9-search-relevance-laguna-dev-1 --encryption-material-provider KMS --kmsRegion us-east-1 --material \"dcda7203-d69b-4014-a560-377f9bbd3b5f\""
        else
            echo "Unsupported encoding argument \"${_enc}\"."
            echo ${_options}
            return
        fi

        _s3_url=$(echo ${_s3_url} | sed -e s,"s3://",,g)
        local _bucket="$(echo ${_s3_url} | sed -e's,^\([^/]*\).*,\1,g')"
        _s3_url=$(echo ${_s3_url} | sed -e s,${_bucket},,g)
        local _key=$(echo ${_s3_url} | sed -e s,^/,,g)

        local _cmd="s3PutEncrypted --bucket ${_bucket} --object ${_key} --region us-east-1 --endpoint s3.amazonaws.com ${_enc} ${_local_object}"

        echo "Executing: ${_cmd}"
        eval ${_cmd}
    }

    if [[ ${#} -lt 2 ]]; then
        echo "Wrong number of arguments."
        echo ${_usage}
        return
    fi

    echo "Received: ${0} ${*}"

    local _cmd="${1}"
    if [[ "${_cmd}" == "get" ]]; then
        _j-aws-s3-amzn-get ${*}
    elif [[ "${_cmd}" == "put" ]]; then
        _j-aws-s3-amzn-put ${*}
    else
        echo "Unsupported S3 function"
        echo ${_usage}
    fi

}

function j-aws-s3-put-all {
    local USAGE="Usage: $0 <krypt|kms> <local_dir> <s3_root_uri>,\nwhere <enc> can be either \"krypt\" or \"kms\" and <link> is the link provided in the S3 object's overview console page (e.g. 'https://s3.amazonaws.com/<bucket>/<key>')"

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
    local USAGE="Usage: ${0} <link>\nwhere <link> is the link provided in the S3 object's overview console page (e.g. 'https://s3.amazonaws.com/<bucket>/<key>')"

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

  local CMD="aws-amzn s3 rm"

  IS_FOLDER=`aws-amzn s3 ls ${B}`
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

  local CMD="ssh -o \"StrictHostKeyChecking no\" -i ${1} -J ec2-34-205-84-46.compute-1.amazonaws.com -ND 8157 hadoop@${2} &"
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
