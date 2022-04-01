#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

which xrandr > /dev/null 2>&1
if [ $? -eq 0 ]; then
    logger "refreshing display profile file"
    . $HOME/lib/profiles/$(hostname)/__DEFAULT__
else
    logger "ERROR: 'xrandr' is not found!"
fi
