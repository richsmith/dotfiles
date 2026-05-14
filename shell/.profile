# ~/.profile: executed by the command interpreter for login shells.
# Read by: login bash, display managers (gdm/lightdm), and our
# .zprofile (which sources this so login zsh gets the same env).
# Child shells inherit the env via exec, so no re-sourcing needed.
#
# Keep this file sh-compatible — no zsh/bash-isms — so display
# managers and POSIX shells can source it cleanly.

umask u=rwx,g=,o=

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi


# Core paths
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state

# PATH: user bins first, then npm bin
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi
PATH="$PATH:$XDG_DATA_HOME/npm/bin"
export PATH


# Preferences
export LANG=en_GB.UTF-8
export EDITOR='emacs -nw'


# XDG Base Directory Specification hacks — make tools store
# config/cache/data under XDG dirs instead of polluting $HOME.
export ANDROID_USER_HOME="$XDG_DATA_HOME"/android
export AWS_CONFIG_FILE="$XDG_CONFIG_HOME"/aws/config
export AWS_SHARED_CREDENTIALS_FILE="$XDG_CONFIG_HOME"/aws/credentials
export CARGO_HOME="$XDG_DATA_HOME"/cargo
export CLAUDE_CONFIG_DIR="$XDG_CONFIG_HOME"/claude
export DOCKER_CONFIG="$XDG_CONFIG_HOME"/docker
export DOTNET_CLI_HOME="$XDG_DATA_HOME"/dotnet
export GEM_HOME="$XDG_DATA_HOME"/gem
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle
export IPYTHONDIR="$XDG_CONFIG_HOME"/ipython
export KUBECACHEDIR="$XDG_CACHE_HOME"/kube
export KUBECONFIG="$XDG_CONFIG_HOME"/kube
export MINIKUBE_HOME="$XDG_DATA_HOME"/minikube
export NUGET_PACKAGES="$XDG_CACHE_HOME"/nuget
export PYTHON_HISTORY="$XDG_STATE_HOME"/python/history
export PUB_CACHE="$XDG_CACHE_HOME"/pub
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export WGETRC="$XDG_CONFIG_HOME"/wgetrc
export npm_config_cache="$XDG_CACHE_HOME"/npm
export npm_config_prefix="$XDG_DATA_HOME"/npm
export npm_config_userconfig="$XDG_CONFIG_HOME"/npm/config
