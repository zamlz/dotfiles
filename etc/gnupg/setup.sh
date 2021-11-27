#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up GnuPG"

CONFIG_SOURCE=$HOME/etc/gnupg
CONFIG_TARGET=$HOME/.gnupg

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir $CONFIG_TARGET
fi

logger "Creating symlink for gpg.conf"
ln -s $CONFIG_SOURCE/gpg.conf $CONFIG_TARGET/gpg.conf

logger "Creating symlink for gpg-agent.conf"
ln -s $CONFIG_SOURCE/gpg-agent.conf $CONFIG_TARGET/gpg-agent.conf
