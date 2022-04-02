#!/bin/sh

# Picom Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Picom"

CONFIG_SOURCE=$HOME/etc/picom/config
CONFIG_TARGET=$HOME/.config/picom/picom.conf
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "picom config" $CONFIG_SOURCE $CONFIG_TARGET
