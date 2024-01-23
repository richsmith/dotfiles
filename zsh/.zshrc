export PATH=$HOME/bin:/usr/local/bin:$HOME/.local/bin:$PATH
export ZSH="/home/$USER/.oh-my-zsh"

ZSH_THEME="robbyrussell"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=28

HIST_STAMPS="yyyy-mm-dd"

# define before sourcing oh-my-zsh.sh
plugins=(
    kubectl
    git
    dirhistory
    pip
    web-search
    fzf
)

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder
DISABLE_UPDATE_PROMPT=true

export XDG_CONFIG_HOME=$HOME/.config
export ZSH_COMPDUMP=$ZSH/cache/.zcompdump-$HOST
source $ZSH/oh-my-zsh.sh

# User configuration
export LANG=en_GB.UTF-8

export EDITOR='emacs -nw'

umask u=rwx,g=,o=

# Execute any specific code for this box/user
ZSH_LOCAL='zshlocal'
if [ -f ~/.$ZSH_LOCAL ]; then
    source ~/.$ZSH_LOCAL
elif [ -f $XDG_CONFIG_HOME/$ZSH_LOCAL ]; then
    source $XDG_CONFIG_HOME/$ZSH_LOCAL
fi

eval "$(direnv hook zsh)"

set -o ignoreeof

# Aliases ######################################################################
if [ -x "$(command -v xclip)" ]; then
    alias clipboard="tee >(xclip -sel clip) | xclip"
    alias clip=clipboard
    alias cb=clipboard
fi

if [ -x "$(command -v exa)" ]; then
    alias ls="exa"
fi


# History ######################################################################
HISTSIZE=100000
export HISTFILE=$XDG_CONFIG_HOME/.zsh_history
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.

export NVM_DIR="$HOME/.config/nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
