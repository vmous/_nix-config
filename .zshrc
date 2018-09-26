#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

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

function date_time_timestamp() {
  echo $(date +%Y%m%d%H%M%S)
}

############################## platform #########################################
JUNAME=`uname`
JMACHINE="unknown"
if [[ "${JUNAME}" == "Linux" ]]; then
    JMACHINEID=`cat /var/lib/dbus/machine-id`
    if [[ "${JMACHINEID}" == "3a5887098fa63dfdbfe53d660000067c" ]]; then
        JMACHINE="worklinux"
    else
        JMACHINE="homelinux"
    fi
elif [[ "${JUNAME}" == "Darwin" ]]; then
    JMACHINE="mac"
fi

if [[ "${JMACHINE}" == "homelinux" ]]; then
    # Home Linux only
elif [[ "${JMACHINE}" == "worklinux" ]]; then
    # Work Linux only
    source ~/.zshrc_amzn
    source ~/.alias_amzn
elif [[ "${JMACHINE}" == "mac" ]]; then
    # Mac only
    export PATH="/usr/local/sbin:${PATH}"
    if cmd_exists cowsay; then
        /usr/local/bin/cowsay -f eyes "Welcome ${USER}"
    fi
    source ~/.alias_amzn
else
    echo "I don't recognize this machine!"
fi

############################## aliases ##########################################
source ~/.alias
if [[ "${JUNAME}" == "Linux" ]]; then
    # "ls -G" for BSD ls (comming from .alias that was just sourced)
    # "ls --colour=auto" for GNU
    alias ls='ls --color=auto'
fi

############################## colours ##########################################
if [[ "${JMACHINE}" != "worklinux" ]]; then
    autoload -U colors
    colors
fi

############################## path #############################################
export PATH=~/wrappers:${PATH}

############################## prompts ##########################################
export PROMPT="
%{$fg[white]%}(%D %*) <%?> [%~] $program %{$fg[default]%}
%{$fg[cyan]%}%m %#%{$fg[default]%} "

export PROMPT="
%{$fg[white]%}(%D %*) <%?> [%~] $program %{$fg[default]%}
%{$fg[cyan]%}%m %#%{$fg[default]%} "

export RPROMPT=

set-title() {
    echo -e "\e]0;${*}\007"
}

ssh() {
    set-title ${*};
    /usr/bin/ssh -2 ${*};
    set-title ${HOST};
}

############################## autocomplete ####################################
# these are some (mostly) sane defaults, if you want your own settings, I
# recommend using compinstall to choose them.  See 'man zshcompsys' for more
# info about this stuff.
setopt COMPLETE_IN_WORD LIST_PACKED

zstyle ':completion:*' menu select
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format %S%d%s
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'

# scp and ssh to generate completions quickly, and I only
# ever ssh or scp as me, so disregard all other users known on
# the system
zstyle -e ':completion:*:scp:*' users 'reply=()'
zstyle -e ':completion:*:ssh:*' users 'reply=()'

# WARNING: This 'if' is needed because there is a conflict
# in awscli completion with
# source ~/.zshrc_amzn
# if you do a below commands again it will break
if [[ "${JMACHINE}" != "worklinux" ]]; then
    autoload -U compinit
    compinit
fi

############################## zsh ##############################################
setopt AUTO_PUSHD # push directories on every cd
setopt NO_BEEP    # never ever beep ever

############################## history options ##################################
setopt EXTENDED_HISTORY        # store time in history
setopt HIST_EXPIRE_DUPS_FIRST  # unique events are more usefull to me
setopt HIST_VERIFY             # Make those history commands nice
setopt INC_APPEND_HISTORY      # immediatly insert history into history file
HISTSIZE=16000                 # spots for duplicates/uniques
SAVEHIST=15000                 # unique events guarenteed
HISTFILE=~/.zsh_history
setopt histignoredups          # ignore duplicates of the previous event

############################## key bindings #####################################
#
# Pushes current command on command stack and gives blank line, after that line
# uns command stack is popped
#
bindkey "^t" push-line-or-edit

#
# Time to time you just need to quickly execute some command line from history
# searching with ctrl-r and you realize you need to modify it and would like to
# do it more comfortable than in shell’s command line.
# A solution: You can specify and editor for the command line and invoke it when needed.
#
# https://blogs.amazon.com/sde_tip_of_the_day/archive/2013/09/editing-a-long-command-line.html
# http://stackoverflow.com/questions/890620/unable-to-have-bash-like-c-x-e-in-zsh
#
# Now Ctrl-x followed by e opens my emacs with the current command line.
#
autoload edit-command-line
zle -N edit-command-line
bindkey -e '^x^e' edit-command-line

############################## misc. ############################################
# Make Emacs the default editor
export EDITOR=emacs
export VISUAL=emacs

# Enabling "smarter" word selection (bash-style)
# Particularly helpful for `backward-kill-word`
autoload -U select-word-style
select-word-style bash

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

## history
#setopt APPEND_HISTORY
## for sharing history between zsh processes
#setopt INC_APPEND_HISTORY
#setopt SHARE_HISTORY

## automatically decide when to page a list of completions
#LISTMAX=0

## disable mail checking
#MAILCHECK=0

# RVM (Ruby Version Manager)
# http://jazzman.webhop.net/doku.php?id=laboratory:programming:ruby:rvm#installation_configuration
# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
# Note: Ignore the warning that might occur that looks like
#       `RVM PATH line not found for Zsh, rerun this command with '--auto-dotfiles' flag to fix it.`
#       as the checker seems to be using a stupid regex that does not match the
#       curly brackets around the variables PATH and HOME below.
export PATH="${PATH}:${HOME}/.rvm/bin"
# Load RVM into a shell session *as a function*
[[ -s "${HOME}/.rvm/scripts/rvm" ]] && source "${HOME}/.rvm/scripts/rvm"

# Tex
if [[ "${JMACHINE}" == "mac" ]]; then
    # Install MacTex via Howebrew first
    # brew cask install mactex
    export PATH="${PATH}:/Library/TeX/texbin"
fi

# pyenv + pyenv-virtualenv
if [[ "${JMACHINE}" == "mac" ]]; then
    # Install pyenv via Homebrew first
    # brew install pyenv
    if cmd_exists pyenv; then
        eval "$(pyenv init -)"
    fi

    # Install pyenv-virtualenv via Homebrew first
    # brew install pyenv-virtualenv
    if cmd_exists pyenv-virtualenv-init; then
        eval "$(pyenv virtualenv-init -)"
    fi
fi
