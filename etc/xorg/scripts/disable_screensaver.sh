#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

which xset > /dev/null 2>&1
if [ $? -eq 0 ]; then
    logger "disabling the screen saver"
    xset s off
else
    logger "ERROR: 'xset' is not found!"
fi
