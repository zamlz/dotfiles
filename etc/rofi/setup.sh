#!/bin/sh

# Rofi Setup Script
# -----------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up Rofi"

CONFIG_SOURCE=$HOME/etc/rofi
CONFIG_TARGET=$HOME/.config/rofi

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for config.rasi"
ln -s $CONFIG_SOURCE/config.rasi $CONFIG_TARGET/config.rasi

logger "Creating symlink for default-theme.rasi"
ln -s $CONFIG_SOURCE/default-theme.rasi $CONFIG_TARGET/default-theme.rasi
