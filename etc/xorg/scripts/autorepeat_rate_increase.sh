#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "increasing the repeat rate of the keyboard"
xset r rate 400 50
