#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up Polybar"

CONFIG_SOURCE=$HOME/etc/polybar/systems
CONFIG_TARGET=$HOME/.config/polybar

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir $CONFIG_TARGET
fi

logger "Creating symlink for system.conf"
ln -s $CONFIG_SOURCE/$(hostname).conf $CONFIG_TARGET/system.conf
