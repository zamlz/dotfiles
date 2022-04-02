#!/bin/sh

# Rofi Setup Script
# -----------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Rofi"

CONFIG_SOURCE_DIR=$HOME/etc/rofi
CONFIG_TARGET_DIR=$HOME/.config/rofi

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "rofi config" $CONFIG_SOURCE_DIR/config.rasi \
    $CONFIG_TARGET_DIR/config.rasi
create_symlink "rofi theme" $CONFIG_SOURCE_DIR/default-theme.rasi \
    $CONFIG_TARGET_DIR/default-theme.rasi
