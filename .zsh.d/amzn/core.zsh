alias SHUTUP='export MAKE_OUTPUT_LEVEL=QUIET'

############################## authentication ###################################
function j-yubikey {
  # https://w.amazon.com/index.php/NextGenMidway/UserGuide/mwinit/Advanced_Daily_Setup_Process
  local USAGE="Usage: ${0} [force]. The 'force' argument is optional."

  if [[ ${#} -gt  1 ]]; then
    echo "Wrong number of arguments"
    echo ${USAGE}
    return
  fi

  local SSH_CERT=~/.ssh/id_rsa-cert.pub
  local EXPIRED=false
  local FORCE_AUTH=false
  if (! test -f "$SSH_CERT") || (test "`find ~/.ssh/id_rsa-cert.pub -mmin +1220`"); then
    EXPIRED=true
    echo "Midway expired"
  else
    echo "Midway has not expired"
  fi

  if [[ ${#} -eq 1 ]]; then
    if [[ ${1} == "force" ]]; then
      # Force authentication
      echo "Forcing Midway authentication"
      FORCE_AUTH=true
    else
      echo "'${1}': unsupported argument"
      echo ${USAGE}
      return
    fi
  fi

  if [[ "${EXPIRED}" = true || "${FORCE_AUTH}" = true ]]; then
    if mwinit; then
      ssh-add -D ~/.ssh/*_rsa
      ssh-add ~/.ssh/*_rsa
    else
      echo "Failed to authenticate."
      exit 1
    fi
  else
    echo "Nothing to do."
    return
  fi
}

function j-yubikey-upgrade {
  # https://w.amazon.com/index.php/NextGenMidway/UserGuide#Mac
  local _tmp_dir=`mktemp -d`
  pushd . > /dev/null
  cd ${_tmp_dir}
  curl -O https://s3.amazonaws.com/com.amazon.aws.midway.software/mac/mwinit
  chmod u+x mwinit
  sudo mv mwinit /usr/local/amazon/bin/mwinit
  popd > /dev/null
  rm -rf ${_tmp_dir}
}
