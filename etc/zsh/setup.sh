#!/bin/sh

# Z Shell Setup Script
# --------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up Z Shell"

CONFIG_SOURCE=$HOME/etc/zsh
CONFIG_TARGET=$HOME

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir $CONFIG_TARGET
fi

logger "Creating symlink for ~/.zshrc"
ln -s $CONFIG_SOURCE/rc $CONFIG_TARGET/.zshrc

logger "Creating symlink for ~/.zlogin"
ln -s $CONFIG_SOURCE/login $CONFIG_TARGET/.zlogin

logger "Creating symlink for ~/.zlogout"
ln -s $CONFIG_SOURCE/logout $CONFIG_TARGET/.zlogout
