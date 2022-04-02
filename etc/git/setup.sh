#!/bin/sh

# Git Setup Script
# ----------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Git"

CONFIG_SOURCE=$HOME/etc/git/config
CONFIG_TARGET=$HOME/.config/git/config
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "git config" $CONFIG_SOURCE $CONFIG_TARGET
