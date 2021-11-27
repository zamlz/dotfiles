#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "disabling the screen saver"
xset s off
