#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger etc.git.setup)"

logger "Setting up Git"

CONFIG_SOURCE=$HOME/etc/git
CONFIG_TARGET=$HOME/.config/git

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir $CONFIG_TARGET
fi

logger "Creating symlink for git config"
ln -s $CONFIG_SOURCE/config $CONFIG_TARGET/config
