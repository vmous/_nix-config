############################## utility functions ################################
function cmd_exists() {
  # Can also be done with the following:
  # which "${1}" > /dev/null 2>&1;
  command -v "${1}" >/dev/null 2>&1
}

function is_dir_a_git_repo() {
  [[ -d "${1}/.git" ]]
}

function is_integer() {
  re='^-?[0-9]+$' # integer (positive or negative)
  [[ $1 =~ "$re" ]]
}
function is_unsigned_integer() {
  re='^[0-9]+$' # integer (positive only)
  [[ $1 =~ "$re" ]]
}
function is_real() {
  re='^-?[0-9]+([.][0-9]+)?$' # real (positive or negative)
  [[ $1 =~ "$re" ]]
}
function is_unsigned_real() {
  re='^[0-9]+([.][0-9]+)?$' # real (positive only)
  [[ $1 =~ "$re" ]]
}

function is_text_file() {
  [[ -f "$1" ]] && file -bL --mime "$1" | grep -q "^text"
}

echo_warning() {
  # Print a yellow warning message to stderr. Colour is emitted only when
  # stderr is a terminal, so redirected or piped output stays free of escape
  # codes. Centralises warning styling so callers just pass the message text.
  # Kept POSIX sh-compliant (only `[ -t 2 ]` and `printf`, no tput/zsh builtins).
  if [ -t 2 ]; then
    printf '\033[33m[WARN] %s\033[0m\n' "$*" >&2
  else
    printf '[WARN] %s\n' "$*" >&2
  fi
}

function run() {
  # Echo a command, then run it (without re-parsing it via eval).
  # Prefix with "dry" to print the command without executing it:
  #   run <command>       # echo, then run
  #   run dry <command>   # echo only
  if [[ "${1}" == "dry" ]]; then
    shift
    echo "[dry-run] $*"
    return 0
  fi
  echo "$*"
  "$@"
}

function yes_or_no() {
  local _yn
  while true; do
    read _yn\?"$1 [y/n] "
    if [[ ${_yn} == "y" ]] || [[ ${_yn} == "n" ]]; then
      break
    else
      echo "Please answer 'y' or 'n." >&2
    fi
  done
  echo ${_yn}
}
