# Core stuff
export PATH=$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH
export XDG_CONFIG_HOME=$HOME/.config
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
# Setting as ZDOTDIR here gets painful...
export ZSH_CONFIG=$XDG_CONFIG_HOME/zsh

umask u=rwx,g=,o=
set -o ignoreeof


# Preferences
export LANG=en_GB.UTF-8
export EDITOR='emacs -nw'


# Prompt
if [ -x "$(command -v starship)" ]; then
    export STARSHIP_CONFIG=$XDG_CONFIG_HOME/starship.toml
    eval "$(starship init zsh)"
else
    PS1='%F{yellow}%~%f %F{green}➜%f '
fi


# History config
HISTSIZE=100000
export HISTFILE=$XDG_CONFIG_HOME/.zsh_history
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY


# Hook in other applications if available
if [ -x "$(command -v xclip)" ]; then
    alias clipboard="tee >(xclip -sel clip) | xclip"
    alias clip=clipboard
    alias cb=clipboard
fi

if [ -x "$(command -v exa)" ]; then
    alias ls="exa --group-directories-first -F"
fi

if [ -x "$(command -v direnv)" ]; then
    eval "$(direnv hook zsh)"
fi

if [ -x "$(command -v fzf)" ]; then
    source $ZSH_CONFIG/fzf-key-bindings.zsh
fi

if [ -x "$(command -v ncal)" ]; then
    alias cal="ncal"
fi

if [ -x "$(command -v kubectl)" ]; then
    alias k="kubectl"
    alias kc="kubectl"
fi


# Aliases, commands and shortcuts
go() {
  cd "$1" && ls
}
bindkey "$key[Up]" history-beginning-search-backward
bindkey "$key[Down]" history-beginning-search-forward
alias ll="ls -l"
alias tree="ls --tree"


# Friendly welcome message :)
echo -e "\e[1;32mHi, $USER! Welcome to $HOST.\e[0m"


# Execute any specific local setup
ZSH_LOCAL='zshlocal'
if [ -f $XDG_CONFIG_HOME/$ZSH_LOCAL ]; then
    source $XDG_CONFIG_HOME/$ZSH_LOCAL
elif [ -f $ZSH_CONFIG/$ZSH_LOCAL ]; then
    source $ZSH_CONFIG/$ZSH_LOCAL
fi
