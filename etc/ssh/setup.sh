#!/bin/sh

# OpenSSH Setup Script
# --------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up OpenSSH"

CONFIG_SOURCE=$HOME/etc/ssh
CONFIG_TARGET=$HOME/.ssh

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for ssh config file"
ln -s $CONFIG_SOURCE/config $CONFIG_TARGET/config
