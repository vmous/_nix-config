local ret_status="%(?:%{$fg_bold[green]%}$ :%{$fg_bold[red]%}$ )%{$reset_color%}"
PROMPT='$(_user_host)%{$fg_bold[blue]%}%~%{$reset_color%} $(git_prompt_info) $(git_prompt_status)
$(aws_profile_prompt_info)$(virtualenv_prompt_info)${ret_status}'
RPROMPT='${return_code}'

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

local return_code=" %(?..%{$fg[red]%}%? ↵%{$reset_color%})"

ZSH_THEME_AWS_PREFIX="[aws:"
ZSH_THEME_AWS_SUFFIX="] "

ZSH_THEME_GIT_PROMPT_PREFIX="%{$fg_bold[red]%}\uE0A0 "
ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[red]%} ✗%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[green]%} ✔%{$reset_color%}"
ZSH_THEME_GIT_PROMPT_ADDED="%{$fg[yellow]%}✚%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_MODIFIED="%{$fg[yellow]%}⚑%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_DELETED="%{$fg[yellow]%}✖%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_RENAMED="%{$fg[yellow]%}⇔%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_UNMERGED="%{$fg[yellow]%}§%{$reset_color%} "
ZSH_THEME_GIT_PROMPT_UNTRACKED="%{$fg[yellow]%}？%{$reset_color%} "

ZSH_THEME_VIRTUALENV_PREFIX="[venv:"
ZSH_THEME_VIRTUALENV_SUFFIX="] "
