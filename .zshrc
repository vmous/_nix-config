#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

source ${HOME}/.zsh.d/utils.zsh

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
else
    echo "I don't recognize this machine!"
fi

if [[ "${JMACHINE}" == "homelinux" ]]; then
    # Home Linux only
elif [[ "${JMACHINE}" == "worklinux" ]]; then
    # Work Linux only
    source ${HOME}/.zsh.d/amzn/devdsk.zsh
elif [[ "${JMACHINE}" == "mac" ]]; then
    # Mac only
    export PATH="/usr/local/sbin:${PATH}"
fi

############################## aliases ##########################################
source ~/.alias

############################## colours ##########################################
if [[ "${JMACHINE}" != "worklinux" ]]; then
    autoload -U colors
    colors
fi

############################## prompts ##########################################
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
# source ${HOME}.zsh.d/amzn/core.zsh
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

############################## oh-my-zsh #######################################
# https://github.com/robbyrussell/oh-my-zsh

# After enabling OMZ to my remote machine, Emacs Tramp hangs every time there is
# an attempt to connect to that machine from my local machine. To fix this I
# have used the suggestions in https://www.emacswiki.org/emacs/TrampMode
#
# Note: Terminal type has been changed by setting `tramp-terminal-type` to
# "tramp" in Tramp's Emacs configuration
if [[ ${TERM} == "tramp" ]]; then
    PS1='%(?..[%?])%!:%~%# '
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    unfunction precmd
    unfunction preexec
else
    source ${HOME}/.zsh.d/.zshrc.oh-my-zsh
fi

if [[ "${JMACHINE}" == "homelinux" ]]; then
    # Home Linux only
fi

if [[ "${JMACHINE}" == "worklinux" ]]; then
    # Work Linux only
fi

if [[ "${JMACHINE}" == "mac" ]]; then
    # Mac only

    # cowsay
    if cmd_exists cowsay; then
        /usr/local/bin/cowsay -f eyes "Welcome ${USER}"
    fi

    # pyenv + pyenv-virtualenv
    # Install pyenv via Homebrew first
    # brew install pyenv
    if cmd_exists pyenv; then
        eval "$(pyenv init -)"
    fi

    # pyenv-virtualenv
    # Install pyenv-virtualenv via Homebrew first
    # brew install pyenv-virtualenv
    if cmd_exists pyenv-virtualenv-init; then
        eval "$(pyenv virtualenv-init -)"
    fi

    # Git
    export PATH=/usr/local/Cellar/git/2.21.0/bin:${PATH}

    # Tex
    # Install MacTex via Howebrew first
    # brew cask install mactex
    export PATH="${PATH}:/Library/TeX/texbin"
fi

if [[ "${JMACHINE}" == "worklinux" ]] || [[ "${JMACHINE}" == "mac" ]]; then
    source ${HOME}/.zsh.d/amzn/core.zsh
    source ${HOME}/.zsh.d/amzn/brazil.zsh
    source ${HOME}/.zsh.d/amzn/apollo.zsh
    source ${HOME}/.zsh.d/amzn/a9.zsh
    source ${HOME}/.zsh.d/aws.zsh

    export PATH=${HOME}/.toolbox/bin:${PATH}
fi
