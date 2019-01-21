############################## utility functions ################################
function cmd_exists() {
  # Can also be done with the following:
  # which "${1}" > /dev/null 2>&1;
  command -v "${1}" >/dev/null 2>&1
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
