local ret_status="%(?:%{$fg_bold[green]%}$ :%{$fg_bold[red]%}$ )%{$reset_color%}"
PROMPT='$(_user_host)%{$fg_bold[blue]%}%~%{$reset_color%} $(git_prompt_info) $(git_prompt_status)
$(_virtualenv_info)${ret_status}'
RPROMPT='$(check_last_exit_code)'

function _user_host() {
  if [[ -n $SSH_CONNECTION ]]; then
    me="%n@%m"
  elif [[ $LOGNAME != $USER ]]; then
    me="%n"
  fi
  if [[ -n $me ]]; then
    echo "%{$fg[cyan]%}$me%{$reset_color%}:"
  fi
}

function check_last_exit_code() {
  local LAST_EXIT_CODE=$?
  local COLOR="green"
  if [[ $LAST_EXIT_CODE -ne 0 ]]; then
    COLOR="red"
  fi
  local EXIT_CODE_PROMPT=' '
  EXIT_CODE_PROMPT+="%{$fg[$COLOR]%}-%{$reset_color%}"
  EXIT_CODE_PROMPT+="%{$fg_bold[$COLOR]%}$LAST_EXIT_CODE%{$reset_color%}"
  EXIT_CODE_PROMPT+="%{$fg[$COLOR]%}-%{$reset_color%}"
  echo "$EXIT_CODE_PROMPT"
}

function _virtualenv_info() {
#  [[ -n "$VIRTUAL_ENV" ]] && echo "\e[3m(`basename $VIRTUAL_ENV`)\e[0m "
  [[ -n "$VIRTUAL_ENV" ]] && echo "(`basename $VIRTUAL_ENV`) "
}

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[red]%}\uE0A0"
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"

ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✔%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[yellow]%}✚%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%}⚑%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[yellow]%}✖%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[yellow]%}⇔%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[yellow]%}§%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[yellow]%}？%{$reset_color%} "

