# Core stuff
export PATH=$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_STATE_HOME=$HOME/.local/state
# Setting as ZDOTDIR here gets painful...
export ZSH_CONFIG=$XDG_CONFIG_HOME/zsh

# Stop auto-initialising compinit and do it in .zsrhc instead
# (this stops that annoying .zcompdump file being created)
skip_global_compinit=1
