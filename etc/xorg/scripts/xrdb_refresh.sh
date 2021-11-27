#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "refreshing xresources file"
xrdb -I$HOME $HOME/etc/xorg/xresources
