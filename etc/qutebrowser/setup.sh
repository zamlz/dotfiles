#!/bin/sh

# Qutebrowser Setup Script
# ------------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting up Qutebrowser"

CONFIG_SOURCE=$HOME/etc/qutebrowser
CONFIG_TARGET=$HOME/.config/qutebrowser

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

logger "Creating symlink for config.py"
ln -s $CONFIG_SOURCE/config.py $CONFIG_TARGET/config.py

