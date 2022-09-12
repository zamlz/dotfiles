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

# Installing Vim-Plug
VIM_PLUG_URL=https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
SAVE_PATH=$HOME/.local/share/nvim/site/autoload/plug.vim

if [ ! -f "$SAVE_PATH" ]; then
    logger "Setting up vim-plug"
    curl -fLo $SAVE_PATH --create-dirs $VIM_PLUG_URL
else
    logger "skipping vim-plug install (already exists)"
fi
