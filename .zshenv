# This is due to module_path cannot be passed in implicitly when invoked into
# script. zmodload will try to find module from compile time setting.
# Unfortunately our zsh build will generate a different directory everything it
# built (/local/p4clients/pkgbuild-xxxx....).
#
# I have to do following to fix it locally:
#
# - Change `${HOME}/.oh-my-zsh/oh-my-zsh.sh` (remove `-f`)
#   from: `env ZSH=$ZSH DISABLE_UPDATE_PROMPT=$DISABLE_UPDATE_PROMPT zsh -f $ZSH/tools/check_for_upgrade.sh`
#   to: `env ZSH=$ZSH DISABLE_UPDATE_PROMPT=$DISABLE_UPDATE_PROMPT zsh $ZSH/tools/check_for_upgrade.sh`
#
# - Create this file (`${HOME}/.zshenv`) with following content.
#
# Our zsh is not built to source /etc/zshenv so we have to use ~/.zshenv and
# zsh -f ignores `${HOME}/.zshenv` hence those changes.
#
# There has been an attempt to merge this change upstream but hasn't been
# approved: https://github.com/robbyrussell/oh-my-zsh/pull/4547

MY_PATH=/apollo/env/envImprovement/var/lib/zsh/5.0.7/
if [[ -d ${MY_PATH} && ${module_path[(I)${MY_PATH}]} -eq 0 ]]; then
    module_path[$(($#module_path+1))]=${MY_PATH}
fi
