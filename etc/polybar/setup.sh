#!/bin/sh

# PolyBar Setup Script
# --------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Polybar"

CONFIG_SOURCE=$HOME/etc/polybar/systems/$(hostname).conf
CONFIG_TARGET=$HOME/.config/polybar/system.conf
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "polybar config" $CONFIG_SOURCE $CONFIG_TARGET
