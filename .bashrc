# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth
export HISTSIZE=2000

VISUAL=emacs; export VISUAL
EDITOR=emacs; export EDITOR


export PYTHONDONTWRITEBYTECODE="1"

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"


# Load in the git branch prompt script.
source ~/util/git-prompt.sh

# Set the prompt
# (Expected: green text on black)
PS1="\u@\h:\[\033[01;33m\]\W/\[\033[00m\]\$(__git_ps1)$ "

# If this is an xterm set the title to pwd
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

######## Path #########
export PATH=$PATH:~/bin

######## Coding #########
export CLASSPATH=/usr/share/java/junit4.jar:~/coding/layout/main/:~/coding/layout/test/:.

########## Aliases ##########
alias ls='ls -GF'
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias cls='clear'
alias up='cd ..'
alias ls.="ls -a | grep '^\\.'"
#alias get='mv ~/clipboard'
alias lock='xscreensaver-command -lock'
alias runphantom='phantomjs --webdriver=9134'


# enable programmable completion features
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

# Welcome message
echo "Hi, $USER, welcome to $HOSTNAME."
echo -n "It is "
date
echo -n "Users: "
users


# Rember to add:
# set bell-style off
# to .inputrc to stop that bloody annoying beep

umask 077 # in case this isn't set in .profile



########## Sorenson-specific stuff ##########

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

alias adgrep='grep -ir --exclude-dir="node_modules" --exclude-dir="coverage" --exclude-dir="webkitbuilds" --exclude-dir="data" --exclude-dir="static/js/vnd" --exclude="javascript.js" --exclude="static/bootstrap.min.css"'

#source `brew --prefix git`/etc/bash_completion.d/git-completion.bash

alias nom='npm'



