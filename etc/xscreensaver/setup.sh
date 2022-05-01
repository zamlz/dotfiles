#!/bin/sh

# XScreenSaver Setup Script
# -------------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up XScreenSaver"

CONFIG_SOURCE=$HOME/etc/xscreensaver
CONFIG_TARGET=$HOME

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET
fi

create_symlink "xscreensaver config" $CONFIG_SOURCE/settings.conf $CONFIG_TARGET/.xscreensaver
