#!/bin/sh

# Kitty Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up Kitty"

CONFIG_SOURCE=$HOME/etc/kitty
CONFIG_TARGET=$HOME/.config/kitty

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for kitty.conf"
ln -s $CONFIG_SOURCE/main.conf $CONFIG_TARGET/kitty.conf
