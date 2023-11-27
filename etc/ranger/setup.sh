#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up Ranger"

CONFIG_SOURCE=$HOME/etc/ranger
CONFIG_TARGET=$HOME/.config/ranger

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET -m 700
fi

create_symlink "gpg config" $CONFIG_SOURCE/rc.conf $CONFIG_TARGET/rc.conf
