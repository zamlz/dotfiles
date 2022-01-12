#!/bin/sh

# Picom Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up Picom"

CONFIG_SOURCE=$HOME/etc/picom
CONFIG_TARGET=$HOME/.config/picom

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for picom.conf"
ln -s $CONFIG_SOURCE/config $CONFIG_TARGET/picom.conf
