#!/bin/sh

# OpenSSH Setup Script
# --------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up OpenSSH"

CONFIG_SOURCE=$HOME/etc/ssh/config
CONFIG_TARGET=$HOME/.ssh/config
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "ssh config" $CONFIG_SOURCE $CONFIG_TARGET
