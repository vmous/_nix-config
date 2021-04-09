#
# Personal zshrc configuration for Amzn Dev Desktop
#
# Add the following line on the very top of your .zshrc file
# source /home/$USER/.zshrc_amzn
#

# Interesting configuration at sourced by the following
# /apollo/env/envImprovement/dotfiles/zshrc
# /apollo/env/envImprovement/dotfiles/anysync
source /apollo/env/envImprovement/var/zshrc

# ZSH Autocomplete for bin/aws
# https://w.amazon.com/index.php/AmazonAwsCli/GettingStarted#ZSH_Autocomplete_for_bin.2Faws
source /apollo/env/AmazonAwsCli/bin/aws_zsh_completer.sh

for f in OdinTools AmazonAwsCli envImprovement; do
    if [[ -d /apollo/env/$f ]]; then
        export PATH=/apollo/env/$f/bin:$PATH
    fi
done


export JAVA_HOME_8=/apollo/env/JavaSE8/jdk1.8
export JAVA_HOME_10=/apollo/env/JavaSE10/jdk-10
export JAVA_HOME_11=/apollo/env/JavaSE11/jdk-11

export JAVA_HOME=${JAVA_HOME_11}

export PATH=$PATH:${JAVA_HOME_11}/bin

export M2_HOME=/workplace/sandbox/software/maven/apache-maven-3.5.4
export M2=$M2_HOME/bin
export PATH=$M2:$PATH

alias j-perm-sync='sudo /usr/multipass/bin/auth-sync.pl -a'

# alternative: /apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --url %s -k --negotiate -u :
alias curl_sdetools='/apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --anyauth --location-trusted -u: -c /tmp/cookies.txt -b /tmp/cookies.txt -k'
