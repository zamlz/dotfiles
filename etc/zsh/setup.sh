#!/bin/sh

# Z Shell Setup Script
# --------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Z Shell"

CONFIG_SOURCE_DIR=$HOME/etc/zsh
CONFIG_TARGET_DIR=$HOME

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "zshrc" $CONFIG_SOURCE_DIR/rc \
    $CONFIG_TARGET_DIR/.zshrc
create_symlink "zlogin" $CONFIG_SOURCE_DIR/login \
    $CONFIG_TARGET_DIR/.zlogin
create_symlink "zlogout" $CONFIG_SOURCE_DIR/logout \
    $CONFIG_TARGET_DIR/.zlogout
