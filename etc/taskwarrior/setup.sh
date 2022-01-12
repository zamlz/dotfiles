#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up taskwarrior"

CONFIG_SOURCE=$HOME/etc/taskwarrior
CONFIG_TARGET=$HOME

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for taskrc"
ln -s $CONFIG_SOURCE/rc $CONFIG_TARGET/.taskrc
