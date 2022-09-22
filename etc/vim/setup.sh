#!/bin/sh

# (Neo)Vim Setup Script
# ---------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up NeoVim"

CONFIG_SOURCE=$HOME/etc/vim/runtime
CONFIG_TARGET=$HOME/.config/nvim
CONFIG_TARGET_DIR=$(dirname $CONFIG_TARGET)

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p $CONFIG_TARGET_DIR
fi

create_symlink "nvim config" $CONFIG_SOURCE $CONFIG_TARGET
