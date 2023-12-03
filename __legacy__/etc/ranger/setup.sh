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
    mkdir -p "$CONFIG_TARGET"
fi

create_symlink "gpg config" $CONFIG_SOURCE/rc.conf "$CONFIG_TARGET/rc.conf"

if [ ! -d "$CONFIG_TARGET/plugins" ]; then
    logger "Cloning devicons plugin for ranger"
    git clone https://github.com/alexanderjeurissen/ranger_devicons "$CONFIG_TARGET/plugins/ranger_devicons"
fi
