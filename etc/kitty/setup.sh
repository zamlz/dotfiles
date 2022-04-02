#!/bin/sh

# Kitty Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Kitty"

CONFIG_SOURCE=$HOME/etc/kitty/main.conf
CONFIG_TARGET=$HOME/.config/kitty/kitty.conf
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "kitty config" $CONFIG_SOURCE $CONFIG_TARGET
