#
# Personal zshrc configuration for Amzn Dev Desktop
#
# Add the following line on the very top of your .zshrc file
# source /home/$USER/.zshrc_amzn
#

# Amazon Q pre block. Keep at the top of this file.
[[ -f "${HOME}/.local/share/amazon-q/shell/zshrc.pre.zsh" ]] && builtin source "${HOME}/.local/share/amazon-q/shell/zshrc.pre.zsh"

# Interesting configuration at sourced by the following
# /apollo/env/EnvImprovement/dotfiles/zshrc
# /apollo/env/EnvImprovement/dotfiles/anyshrc
source /apollo/env/EnvImprovement/var/zshrc

export PATH=${HOME}/.local/bin:${PATH}

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

# alternative: /apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --url %s -k --negotiate -u :
alias curl_sdetools='/apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --anyauth --location-trusted -u: -c /tmp/cookies.txt -b /tmp/cookies.txt -k'

#### Amazon-Internal-MCP-Server
### https://w.amazon.com/bin/view/Amazon-Internal-MCP-Server/
# E.g., Quip configuration setup:
# - Go to https://quip-amazon.com/dev/token and generate your personal token
# - Add the `QUIP_API_TOKEN=<PERSONAL_QUIP_TOKEN>` to the Amazon Internal MCP
#   Server defined below (create it if it does not exist yet)
AMAZON_MCP_SERVER_CONFIG_PATH=${HOME}/.amazon-internal-mcp-server/.env

# Amazon Q post block. Keep at the bottom of this file.
[[ -f "${HOME}/.local/share/amazon-q/shell/zshrc.post.zsh" ]] && builtin source "${HOME}/.local/share/amazon-q/shell/zshrc.po\
st.zsh"
