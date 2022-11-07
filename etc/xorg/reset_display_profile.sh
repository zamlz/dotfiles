#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

which xrandr > /dev/null 2>&1
if [ $? -eq 0 ]; then
    logger "refreshing display profile file"
    # HACK: This is some voodoo magic. For some reason, I need run xrandr
    # atleast once before changing my system resolution
    xrandr > /dev/null
    . $HOME/lib/profiles/$(hostname)/$(cat $HOME/tmp/.$(hostname).xorg.current_profile
    logger "default display profile configured"
else
    logger "ERROR: 'xrandr' is not found!"
fi
