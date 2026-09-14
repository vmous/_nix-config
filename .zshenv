# Amazon-CDD-specific fix.
# This is due to module_path cannot be passed in implicitly when invoked into
# script. zmodload will try to find module from compile time setting.
# Unfortunately our zsh build will generate a different directory everything it
# built (/local/p4clients/pkgbuild-xxxx....). The -d guard makes it a no-op on
# non-Amazon-CDD machines (e.g. MacOS).
MY_PATH="/apollo/env/EnvImprovement/var/lib/zsh/${ZSH_VERSION}/"
if [[ -d ${MY_PATH} && ${module_path[(I)${MY_PATH}]} -eq 0 ]]; then
    module_path+=(${MY_PATH})
fi

# Rust/Cargo environment
[[ -f "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# AIM MCP servers (managed by AIM CLI)
export PATH="$HOME/.aim/mcp-servers:$PATH"
