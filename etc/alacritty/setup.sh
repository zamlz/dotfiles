#!/bin/sh

# Alacritty Setup Script
# ------------------

. "$HOME/lib/shell/logging" && eval "$(get_logger "$0")"
. "$HOME/lib/shell/utils"

logger "Setting up Alacritty"

CONFIG_SOURCE=$HOME/etc/alacritty/main.yaml
CONFIG_TARGET=$HOME/.config/alacritty/alacritty.yml
CONFIG_TARGET_DIR=$(dirname "$CONFIG_TARGET")

if [ ! -d "$CONFIG_TARGET_DIR" ]; then
    logger "Making directory $CONFIG_TARGET_DIR"
    mkdir -p "$CONFIG_TARGET_DIR"
fi

create_symlink "alacritty config" "$CONFIG_SOURCE" "$CONFIG_TARGET"
