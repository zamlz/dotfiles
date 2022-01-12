#!/bin/sh

# (Neo)Vim Setup Script
# ---------------------

. $HOME/lib/shell/logging && eval "$(get_logger etc.vim.setup)"

logger "Setting up (Neo)Vim"

CONFIG_SOURCE=$HOME/etc/vim
CONFIG_TARGET=$HOME/.config/nvim

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for $CONFIG_TARGET/init.vim"
ln -s $CONFIG_SOURCE/rc $CONFIG_TARGET/init.vim

# Installing Vim-Plug
VIM_PLUG_URL=https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
SAVE_PATH=$HOME/.local/share/nvim/site/autoload/plug.vim

logger "Setting up vim-plug"
curl -fLo $SAVE_PATH --create-dirs $VIM_PLUG_URL

