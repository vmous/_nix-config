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

echo_error() {
  # Print a red error message to stderr. Colour is emitted only when stderr is
  # a terminal, so redirected or piped output stays free of escape codes.
  # Centralises error styling so callers just pass the message text.
  # Kept POSIX sh-compliant (only `[ -t 2 ]` and `printf`, no tput/zsh builtins).
  if [ -t 2 ]; then
    printf '\033[31m[ERROR] %s\033[0m\n' "$*" >&2
  else
    printf '[ERROR] %s\n' "$*" >&2
  fi
}

echo_info() {
  # Print an informational message ([INFO] prefix) to stdout. An optional first
  # argument names a colour (red|green|blue|yellow); without it the terminal's
  # default colour is used. Colour is emitted only when stdout is a terminal, so
  # redirected or piped output stays free of escape codes.
  # Kept POSIX sh-compliant (only `[ -t 1 ]` and `printf`, no tput/zsh builtins).
  local _open='' _close=''
  case "$1" in
    red)    _open='\033[31m'; _close='\033[0m'; shift ;;
    green)  _open='\033[32m'; _close='\033[0m'; shift ;;
    blue)   _open='\033[34m'; _close='\033[0m'; shift ;;
    yellow) _open='\033[33m'; _close='\033[0m'; shift ;;
  esac
  if [ -t 1 ] && [ -n "${_open}" ]; then
    printf "${_open}[INFO] %s${_close}\n" "$*"
  else
    printf '[INFO] %s\n' "$*"
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
