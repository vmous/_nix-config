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

# Java
# Check: https://w.amazon.com/bin/view/JDKTeam/OpenJDK/DevDesktops/ to install

# Maven
export M2_HOME=/local/workplace/sandbox/software/maven/apache-maven-3.5.4
export M2=$M2_HOME/bin
export PATH=$M2:$PATH

alias j-perm-sync='sudo /usr/multipass/bin/auth-sync.pl -a'

# alternative: /apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --url %s -k --negotiate -u :
alias curl_sdetools='/apollo/bin/env -e SDETools /apollo/env/SDETools/bin/curl --anyauth --location-trusted -u: -c /tmp/cookies.txt -b /tmp/cookies.txt -k'

function j-upgrade-intellij {
  INSTALLATION_ROOT=/opt/JetBrains
  DOWNLOAD_ROOT=${INSTALLATION_ROOT}/downloads
  if [ -d "${INSTALLATION_ROOT}" ]; then
    echo "Installation root \"${INSTALLATION_ROOT}\" already exists"
    INSTALLATIONS=$(find "${INSTALLATION_ROOT}"/idea* -type d -prune -printf "%f ")
    if [ -z "${INSTALLATIONS}" ]; then
      echo "No existing installations found"
    else
      echo "Found existing installations: ${INSTALLATIONS}"
    fi
  else
    echo "Creating installation root ${INSTALLATION_ROOT}"
    mkdir -p ${INSTALLATION_ROOT}
  fi

  DOWNLOAD_URL="https://download.jetbrains.com/product?code=${INTELLIJ_EDITION_CODE:-IIC}&latest&distribution=linux"
  echo "Downloading latest IntelliJ from ${DOWNLOAD_URL} (this may take a minute)..."
  [ -d "${DOWNLOAD_ROOT}" ] || sudo mkdir -p ${DOWNLOAD_ROOT}
  sudo curl -L "${DOWNLOAD_URL}" -o ${DOWNLOAD_ROOT}/intellij-latest.tar.gz

  echo "Extracting to application folder (this may take a few seconds)..."
  sudo tar xfz ${DOWNLOAD_ROOT}/intellij-latest.tar.gz -C ${DOWNLOAD_ROOT}/
  LATEST_VERSION=$(ls ${DOWNLOAD_ROOT}/ | grep idea)

  if [ -d "${INSTALLATION_ROOT}/${LATEST_VERSION}" ]; then
    echo "The latest IntelliJ version \"${LATEST_VERSION}\" is already installed under \"${INSTALLATION_ROOT}/\". Nothing to do..."
  else
    echo "Installing latest IntelliJ version \"${LATEST_VERSION}\" under \"${INSTALLATION_ROOT}/\"..."
    sudo mv ${DOWNLOAD_ROOT}/${LATEST_VERSION} ${INSTALLATION_ROOT}/

    echo "Creating application shortcuts..."
    sudo rm -f /usr/share/applications/IntelliJ.desktop
    sudo rm -f /local/Desktop/IntelliJ.desktop
    echo -e "[Desktop Entry]\nEncoding=UTF-8\nVersion=1.0\nType=Application\nTerminal=false\nExec=/opt/JetBrains/${LATEST_VERSION}/bin/idea.sh\nName=IntelliJ IDEA\nIcon=/opt/JetBrains/${LATEST_VERSION}/bin/idea.svg\nCategories=Development;IDE;" | sudo tee /usr/share/applications/IntelliJ.desktop > /dev/null
    sudo ln -fs /usr/share/applications/IntelliJ.desktop ${HOME}/Desktop/IntelliJ.desktop

    echo "Clean up"
    sudo rm -rf ${DOWNLOAD_ROOT}

    echo "All done. Enjoy your new IntelliJ!"
  fi
}
