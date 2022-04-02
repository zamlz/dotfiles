#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up taskwarrior"

CONFIG_SOURCE=$HOME/etc/taskwarrior/rc
CONFIG_TARGET=$HOME/.taskrc
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "taskwarrior config" $CONFIG_SOURCE $CONFIG_TARGET
