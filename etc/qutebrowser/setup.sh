#!/bin/sh

# Qutebrowser Setup Script
# ------------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Qutebrowser"

CONFIG_SOURCE=$HOME/etc/qutebrowser/config.py
CONFIG_TARGET=$HOME/.config/qutebrowser/config.py
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "qutebrowser config" $CONFIG_SOURCE $CONFIG_TARGET

