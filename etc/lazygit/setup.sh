#!/bin/sh

# LazyGit Setup Script
# --------------------

. "$HOME/lib/shell/logging" && eval "$(get_logger "$0")"
. "$HOME/lib/shell/utils"

logger "Setting up LazyGit"

CONFIG_SOURCE=$HOME/etc/lazygit
CONFIG_TARGET=$HOME/.config/lazygit

if [ ! -d "$CONFIG_TARGET" ]; then
    logger "Making directory $CONFIG_TARGET"
    mkdir -p "$CONFIG_TARGET"
fi

create_symlink "lazygit config" "$CONFIG_SOURCE/config.yml" "$CONFIG_TARGET/config.yml"
