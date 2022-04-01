#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

which xrdb > /dev/null 2>&1
if [ $? -eq 0 ]; then
    logger "refreshing xresources file"
    xrdb -I$HOME $HOME/etc/xorg/xresources
else
    logger "ERROR: 'xrdb' is not found!"
fi
