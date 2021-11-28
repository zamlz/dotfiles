#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "killing picom if its running"
pkill -x picom
if [ -f "$HOME/.config/picom/picom.conf" ]; then
    logger "starting new picom process"
    sleep 1 && picom &
fi
