# Env vars (PATH, XDG_*, EDITOR, LANG, tool config) live in
# .profile, sourced by .zprofile for login shells.

# Setting as ZDOTDIR here gets painful...
export ZSH_CONFIG=$XDG_CONFIG_HOME/zsh

# Define here to put zcompdump file in
# (directory creation handled by Stow)
ZSH_CACHE="$XDG_CACHE_HOME"/zsh

# Stop auto-initialising compinit and do it in .zsrhc instead
# (this stops that annoying .zcompdump file being created)
skip_global_compinit=1

# History limits live here so every zsh (incl. non-interactive)
# has sane values — prevents accidental truncation if some shell
# ever writes history with the default SAVEHIST=0.
# Actual write behaviour (INC_APPEND_HISTORY etc.) stays in .zshrc.
export HISTFILE=$ZSH_CONFIG/zsh_history
HISTSIZE=10000000
SAVEHIST=10000000
