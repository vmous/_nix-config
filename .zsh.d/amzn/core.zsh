alias SHUTUP='export MAKE_OUTPUT_LEVEL=QUIET'

############################## authentication ###################################
function j-yubikey {
  # https://w.amazon.com/index.php/NextGenMidway/UserGuide/mwinit/Advanced_Daily_Setup_Process
  SSH_CERT=~/.ssh/id_rsa-cert.pub
  if (! test -f "$SSH_CERT") || (test "`find ~/.ssh/id_rsa-cert.pub -mmin +1220`"); then
    echo "Midway expired"
    if mwinit; then
      ssh-add -D ~/.ssh/*_rsa
      ssh-add ~/.ssh/*_rsa
    else
      echo "Failed to authenticate."
      exit 1
    fi
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
