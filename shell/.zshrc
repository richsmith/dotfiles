# Cache for zsh completion
ZSH_CACHE="$XDG_CACHE_HOME"/zsh
mkdir -p $ZSH_CACHE

autoload -U compinit
compinit -d "$ZSH_CACHE/zcompdump-$ZSH_VERSION"


umask u=rwx,g=,o=
set -o ignoreeof


# Preferences
export LANG=en_GB.UTF-8
export EDITOR='emacs -nw'


# History config
HISTSIZE=10000000
SAVEHIST=10000000
export HISTFILE=$ZSH_CONFIG/zsh_history
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_DUPS
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY


# Prompt
if [ -x "$(command -v starship)" ]; then
    export STARSHIP_CONFIG=$XDG_CONFIG_HOME/starship.toml
    eval "$(starship init zsh)"
else
    PS1='%F{yellow}%~%f %F{green}➜%f '
fi


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
    source <(kubectl completion zsh)
fi


# Aliases, commands and shortcuts
go() {
  cd "$1" && ls
}
bindkey "$key[Up]" history-beginning-search-backward
bindkey "$key[Down]" history-beginning-search-forward
alias ll="ls -l"
alias tree="ls --tree"


# Hacks and fixes
alias monerod=monerod --data-dir "$XDG_DATA_HOME"/bitmonero
export _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME"/java
export MINIKUBE_HOME="$XDG_DATA_HOME"/minikube
export GRADLE_USER_HOME="$XDG_DATA_HOME"/gradle
export ANDROID_USER_HOME="$XDG_DATA_HOME"/android
alias adb='HOME="$XDG_DATA_HOME"/android adb'
alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

# Friendly welcome message :)
echo -e "\e[1;32mHi, $USER! Welcome to $HOST.\e[0m"


# Execute any specific local setup
ZSH_LOCAL='zshlocal'
if [ -f $XDG_CONFIG_HOME/$ZSH_LOCAL ]; then
    source $XDG_CONFIG_HOME/$ZSH_LOCAL
elif [ -f $ZSH_CONFIG/$ZSH_LOCAL ]; then
    source $ZSH_CONFIG/$ZSH_LOCAL
fi


