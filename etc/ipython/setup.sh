#!/bin/sh

# iPython Setup Script
# --------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up iPython"

CONFIG_SOURCE=$HOME/etc/ipython
CONFIG_TARGET=$HOME/.ipython/profile_default

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET -m 700
fi

create_symlink "ipython config" $CONFIG_SOURCE/config.py \
        $CONFIG_TARGET/ipython_config.py
