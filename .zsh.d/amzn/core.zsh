alias SHUTUP='export MAKE_OUTPUT_LEVEL=QUIET'

############################## authentication ###################################
function _midway_cert_is_fresh {
  # Predicate: is the Midway SSH certificate at ${1} still fresh (not expired)?
  # True when the file exists and was modified within the last 1220 minutes.
  # This is the single place that defines how Midway freshness is computed; it
  # is shared by every platform branch in `j-authenticate`.
  local _cert="${1}"
  test -f "${_cert}" && ! test "`find ${_cert} -mmin +1220`"
}

function j-authenticate {
  # https://w.amazon.com/index.php/NextGenMidway/UserGuide/mwinit/Advanced_Daily_Setup_Process
  local USAGE="Usage: ${0} [force|upgrade]. Without an argument authenticate with Midway; 'force' re-authenticates even when not expired; 'upgrade' installs or upgrades the mwinit binary."

  if [[ ${#} -gt  1 ]]; then
    echo "Wrong number of arguments"
    echo ${USAGE}
    return 1
  fi

  case "${1}" in
    upgrade)
      if [[ "${JMACHINE}" == "mac" ]]; then
        # https://w.amazon.com/index.php/NextGenMidway/UserGuide#Mac
        cmd_exists brew || { echo_error "brew is not installed. Cannot upgrade mwinit. Aborting."; return 1; }
        brew list mwinit &>/dev/null && brew upgrade mwinit || brew install mwinit
      elif [[ "${JMACHINE}" == "worklinux" ]]; then
        # https://w.amazon.com/index.php/NextGenMidway/UserGuide#Linux
        local _tmp_dir=`mktemp -d`
        pushd . > /dev/null
        cd ${_tmp_dir}
        curl -O https://s3.amazonaws.com/com.amazon.aws.midway.software/linux/mwinit
        chmod u+x mwinit
        sudo mv mwinit /usr/local/amazon/bin/mwinit
        popd > /dev/null
        rm -rf ${_tmp_dir}
      else
        echo_warning "mwinit is not required on this machine. Nothing to do."
      fi
      ;;

    force|"")
      local FORCE_AUTH=false
      if [[ "${1}" == "force" ]]; then
        echo "Forcing re-authentication."
        FORCE_AUTH=true
      fi

      if [[ "${JMACHINE}" == "mac" ]]; then
        # Kerberos: nothing to do. On Mac managed by the KSSO (key icon on
        # menu bar).
        # Midway: the FIDO2/YubiKey security key is attached locally, so
        # authenticate with U2F ('--fido2'). This signs the local SSH public key
        # and writes a fresh certificate to ~/.ssh/id_ecdsa-cert.pub. The
        # freshness of that certificate gates whether re-authentication is
        # needed.
        local PRIVATE_KEY=${HOME}/.ssh/id_ecdsa
        local SSH_CERT=${PRIVATE_KEY}-cert.pub

        if [[ "${FORCE_AUTH}" != true ]] && _midway_cert_is_fresh "${SSH_CERT}"; then
          echo_warning "Midway token available and not expired. Nothing to do."
        elif ! mwinit --fido2; then
          echo_error "Failed to authenticate with Midway!"
          return 1
        else
          # Reload the refreshed certificate into the SSH agent. The Mac is the
          # origin of SSH agent forwarding and daily SSH here is served by the
          # agent, so the on-disk certificate is not enough: it must live in the
          # agent to be usable locally and forwardable to remote hosts. 'ssh-add
          # -D' clears the stale entry (it deletes all agent identities, ignoring
          # the argument) before re-adding the fresh one.
          ssh-add -D ${PRIVATE_KEY}
          ssh-add ${PRIVATE_KEY}
        fi
      elif [[ "${JMACHINE}" == "worklinux" ]]; then
        local PRIVATE_KEY=${HOME}/.ssh/id_rsa
        local SSH_CERT=${PRIVATE_KEY}-cert.pub

        # Kerberos: renew the ticket when it is missing/expired, or when 'force'
        # is given. 'klist -s' runs silently and exits non-zero when there is no
        # ticket or it has expired; 'kinit -f' then obtains a fresh forwardable
        # one.
        if [[ "${FORCE_AUTH}" != true ]] && klist -s; then
          echo_warning "Kerberos ticket already valid. Nothing to do."
        elif ! kinit -f; then
          echo_error "Failed to renew the Kerberos ticket!"
          return 1
        fi

        # Midway: the FIDO2/YubiKey security key is not attached to this remote
        # host, so authenticate with a One Time Password ('-o') instead of U2F,
        # and sign the local SSH public key ('-s'). mwinit authenticates here and
        # writes a fresh Midway-signed certificate to ~/.ssh/id_rsa-cert.pub on
        # this machine (it does not reuse credentials from the originating Mac
        # session). Its freshness gates whether re-authentication is needed. No
        # ssh-add is needed because SSH auto-loads the certificate sitting next
        # to the default identity ~/.ssh/id_rsa.
        if [[ "${FORCE_AUTH}" != true ]] && _midway_cert_is_fresh "${SSH_CERT}"; then
          echo_warning "Midway token available and not expired. Nothing to do."
        elif ! mwinit -s -o; then
          echo_error "Failed to authenticate with Midway!"
          return 1
        fi
      else
        echo_warning "mwinit is not required on this machine. Nothing to do."
        return
      fi
      ;;

    *)
      echo_error "Unrecognized argument ${1}. Exiting..."
      echo ${USAGE}
      return 1
      ;;
  esac
}
