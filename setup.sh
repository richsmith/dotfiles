#!/usr/bin/env bash


sudo apt-get -qq install -y zsh emacs terminator



LOCAL_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)
CONFIG_DIR=$HOME/.config

EMACS_DIR=$LOCAL_DIR/emacs
ln -fs $EMACS_DIR $CONFIG_DIR/emacs

TERMINATOR_DIR=$LOCAL_DIR/terminator
ln -fs $TERMINATOR_DIR $CONFIG_DIR/terminator 

