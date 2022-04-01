#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

which xset > /dev/null 2>&1
if [ $? -eq 0 ]; then
    logger "increasing the repeat rate of the keyboard"
    xset r rate 400 50
else
    logger "ERROR: 'xset' is not found!"
fi
