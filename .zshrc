#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

source ${HOME}/.zsh.d/utils.zsh

############################## platform #########################################
JUNAME=`uname`
JMACHINE="unknown"
WORKLINUXMACHINEID="ec2149dcab7635ac20054547005dd667"
if [[ "${JUNAME}" == "Linux" ]]; then
    JMACHINEID=`cat /etc/machine-id`
    if [[ "${JMACHINEID}" == "${WORKLINUXMACHINEID}" ]]; then
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

# SSH uses a Unix socket to communicate with other processes. The socket's path
# can be found by looking at the environment variable `${SSH_AUTH_SOCK}`. When
# you re-connect to a multiplexer session (e.g., tmux or scree) that was started
# during a previous SSH session, this variable will contain the path of the
# previous SSH authentication socket, and this will cause processes that try to
# connect to your authentication agent to fail.
#
# To fix this, we can have a symlink to always point to the active SSH
# authentication socket and have the multiplexer sessions point to that symlink.
#
# For GNU Screen add the following in your `${HOME}/.screenrc`:
# ```
# unsetenv SSH_AUTH_SOCK
# setenv SSH_AUTH_SOCK ${HOME}/.ssh/ssh-auth-sock.${HOSTNAME}
# ```
#
# For Tmux add the follwing in your `${HOME}/.tmux.conf`:
# ```
# setenv -g SSH_AUTH_SOCK ${HOME}/.ssh/ssh_auth_sock
# set -g update-environment -r
# ```
#
# We can keep the symlink up to date by updating it every time a new SSH session
# is created (and .zshrc is run) as is done below
if test "${SSH_AUTH_SOCK}" && ! test "${TMUX}"; then
    ln -sf ${SSH_AUTH_SOCK} ${HOME}/.ssh/ssh_auth_sock
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

    # Tex
    # Install MacTex via Howebrew first
    # brew cask install mactex
    export PATH="${PATH}:/Library/TeX/texbin"

    # rbenv
    # https://github.com/rbenv/rbenv
    if cmd_exists rbenv; then
        eval "$(rbenv init -)"
    fi
    # To link Rubies to Homebrew's OpenSSL 1.1 (which is upgraded)
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=$(brew --prefix openssl@1.1)"
fi

if [[ "${JMACHINE}" == "worklinux" ]] || [[ "${JMACHINE}" == "mac" ]]; then
    source ${HOME}/.zsh.d/amzn/core.zsh
    source ${HOME}/.zsh.d/amzn/brazil.zsh
    source ${HOME}/.zsh.d/amzn/apollo.zsh
    source ${HOME}/.zsh.d/amzn/a9.zsh
    source ${HOME}/.zsh.d/aws.zsh

    export PATH=${HOME}/.toolbox/bin:${PATH}

    # pyenv + pyenv-virtualenv
    #
    # pyenv
    #
    # Mac: Install Homebrew
    # brew install pyenv
    # AL:
    # sudo yum install zlib-devel bzip2 bzip2-devel readline-devel sqlite sqlite-devel openssl-devel
    # curl -L https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer | bash
    if [[ "${JMACHINE}" == "worklinux" ]] || [[ "${JMACHINE}" == "homelinux" ]]; then
        # Directory the installer clones the pyenv repo to.
        # By defauult it is `${HOME}/.pyenv` but can be changed by exporting PYENV_ROOT before
        # runnng the installer.
        PYENV_GIT_DIR=${HOME}/.pyenv
        if is_dir_a_git_repo ${PYENV_GIT_DIR}; then
            export PATH=${PYENV_GIT_DIR}/bin:${PATH}
        fi
    fi

    if cmd_exists pyenv; then
        eval "$(pyenv init -)"
    fi
    # pyenv-virtualenv
    #
    # Mac: Install via Homebrew
    # brew install pyenv-virtualenv
    #
    # AL: Installer for pyenv installs pyenv-virtualenv
    if { [[ "${JMACHINE}" == "mac" ]] && cmd_exists pyenv-virtualenv-init; } || { { [[ "${JMACHINE}" == "worklinux" ]] || [[ "${JMACHINE}" == "homelinux" ]]; } && cmd_exists pyenv; }; then
        eval "$(pyenv virtualenv-init -)"
    fi
fi

export PATH=$HOME/.toolbox/bin:$PATH
