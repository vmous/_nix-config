#
# Personal zshrc configuration for Amzn Dev Desktop
#
# Add the following line on the very top of your .zshrc file
# source /home/$USER/.zshrc_amzn
#

# Interesting configuration at sourced by the following
# /apollo/env/EnvImprovement/dotfiles/zshrc
# /apollo/env/EnvImprovement/dotfiles/anyshrc
source /apollo/env/EnvImprovement/var/zshrc

for f in VmousToolkit; do
    if [[ -d /apollo/env/$f ]]; then
        export PATH=/apollo/env/$f/bin:$PATH
    fi
done

# Java
# Check: https://w.amazon.com/bin/view/JDKTeam/OpenJDK/DevDesktops/ to install
export JAVA_HOME=$(dirname $(dirname $(realpath /usr/bin/java)))
export PATH=$JAVA_HOME/bin:$PATH

# Maven
export M2_HOME=/local/workplace/sandbox/software/maven/apache-maven-3.5.4
export M2=$M2_HOME/bin
export PATH=$M2:$PATH

alias j-perm-sync='sudo /usr/multipass/bin/auth-sync.pl -a'

# mise
# It is recommended to use AxE to handle installation of mise/Python and other
# common builder tools.
eval "$(/home/vmous/.local/bin/mise activate zsh)"

# Before the following sourcing make sure you create the completions file by running the following
#
# mkdir /home/vmous/.local/share/mise
# /home/vmous/.local/bin/mise completion zsh > /home/vmous/.local/share/mise/completions.zsh
# mise completion zsh > /home/vmous/.local/share/mise/completions.zsh
#
source /home/vmous/.local/share/mise/completions.zsh

# alternative: /apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --url %s -k --negotiate -u :
alias curl_sdetools='/apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --anyauth --location-trusted -u: -c /tmp/cookies.txt -b /tmp/cookies.txt -k'
