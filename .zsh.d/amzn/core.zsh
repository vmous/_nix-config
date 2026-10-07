alias SHUTUP='export MAKE_OUTPUT_LEVEL=QUIET'

############################## authentication ###################################
function _midway_local_is_valid {
  # True when the local Midway credentials are valid: the SSH certificate at
  # ${1} has not expired and, when `mcscli` is installed, the local MCS
  # session is valid too, ignoring any forwarded session. The expiry is read
  # from the certificate itself (`ssh-keygen -L` prints it as local-time ISO
  # 8601, which compares correctly as a string); a missing or unreadable
  # certificate yields no expiry and counts as invalid.
  local _cert="${1}"
  local _expiry=$(ssh-keygen -L -f "${_cert}" 2>/dev/null | sed -n 's/.*Valid: from .* to //p')
  [[ -n "${_expiry}" && "$(date +%Y-%m-%dT%H:%M:%S)" < "${_expiry}" ]] || return 1
  ! cmd_exists mcscli || mcscli is-valid session --local &>/dev/null
}

function _midway_forwarded_session_exists {
  # True when `mcscli` is installed and the agent behind `${SSH_AUTH_SOCK}`
  # holds the forwarded MCS key; false for NICE DCV, plain `ssh`, or a tmux
  # pane whose agent symlink is stale.
  cmd_exists mcscli && ssh-add -l 2>/dev/null | grep -q "MidwayClientSuite"
}

function _midway_forwarded_session_is_valid {
  # True when the forwarded MCS session is valid (not expired). `mcscli`
  # silently falls back to local credentials when it cannot use the forwarded
  # session, so this also requires the reported source to be the forwarded one.
  local _status=$(mcscli is-valid session --output json 2>/dev/null)
  [[ "${_status}" == *'"session_source":"session_forwarding"'* \
     && "${_status}" == *'"is_valid":true'* ]]
}

function j-authenticate {
  # https://w.amazon.com/index.php/NextGenMidway/UserGuide/mwinit/Advanced_Daily_Setup_Process
  local USAGE="Usage: ${0} [force|upgrade]. Without an argument refresh the local Midway credentials if expired; 'force' refreshes them even when not expired; 'upgrade' installs or upgrades the mwinit binary."

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
        # authenticate with U2F ('--fido2'). This writes a fresh certificate to
        # ~/.ssh/id_ecdsa-cert.pub and refreshes ~/.midway/cookie and the MCS
        # session that `wssh fwd` forwards to remote hosts. No ssh-add is
        # needed: SSH here loads the certificate next to ~/.ssh/id_ecdsa from
        # disk, and `wssh fwd` forwards the MCS agent, not the default one.
        local PRIVATE_KEY=${HOME}/.ssh/id_ecdsa
        local SSH_CERT=${PRIVATE_KEY}-cert.pub

        if [[ "${FORCE_AUTH}" != true ]] && _midway_local_is_valid "${SSH_CERT}"; then
          echo_warning "Local Midway credentials available and not expired. Nothing to do."
        elif ! mwinit --fido2; then
          echo_error "Failed to refresh local Midway credentials!"
          return 1
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
        # host, so authenticate with a One Time Password ('-o') and sign the
        # local SSH public key ('-s'), writing ~/.ssh/id_rsa-cert.pub (auto-
        # loaded by SSH next to ~/.ssh/id_rsa) and ~/.midway/cookie. A session
        # forwarded from the Mac by `wssh fwd` covers MCS-aware tools in shells
        # opened through it, but not NICE DCV, sessions without the forwarded
        # agent, or tools that read ~/.midway/cookie directly, so the local
        # credentials are always kept valid; the forwarded session is only
        # reported.
        if ! _midway_forwarded_session_exists; then
          echo_warning "No forwarded Midway session (not connected through \`wssh fwd\`)."
        elif _midway_forwarded_session_is_valid; then
          echo_info "Valid forwarded Midway session identified."
        else
          echo_warning "Invalid forwarded Midway session identified."
          echo_warning "Run \`j-authenticate\` on the Mac to refresh the forwarded Midway session."
        fi

        if [[ "${FORCE_AUTH}" != true ]] && _midway_local_is_valid "${SSH_CERT}"; then
          echo_warning "Local Midway credentials available and not expired. Nothing to do."
        else
          echo_info "Refreshing local Midway credentials."
          if ! mwinit -s -o; then
            echo_error "Failed to refresh local Midway credentials!"
            return 1
          fi
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
