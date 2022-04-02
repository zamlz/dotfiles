#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up GnuPG"

CONFIG_SOURCE=$HOME/etc/gnupg
CONFIG_TARGET=$HOME/.gnupg

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p $CONFIG_TARGET -m 700
fi

create_symlink "gpg config" $CONFIG_SOURCE/gpg.conf $CONFIG_TARGET/gpg.conf
create_symlink "gpg agent config" $CONFIG_SOURCE/gpg-agent.conf \
    $CONFIG_TARGET/gpg-agent.conf
logger "WARNING: make sure to run 'fetch' for your keycard"
