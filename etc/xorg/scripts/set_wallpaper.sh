#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# FIXME: Make a more intelligent check
# wait a little bit so screen resolution can be set
sleep "0.2"

which feh > /dev/null 2>&1
if [ $? -eq 0 ]; then
    if [ -f "$HOME/.fehbg" ]; then
        logger "Setting wallpaper from ~/.fehbg"
        $HOME/.fehbg
    else
        logger "No wallpaper found! Falling back to xsetroot"

        which xsetroot > /dev/null 2>&1
        if [ $? -eq 0 ]; then
            . $HOME/lib/shell/xrdb_colors
            xsetroot -bitmap ~/lib/bitmaps/tile.xbm -fg $XCOLOR0 -bg $XBACKGROUND
        else
            logger "ERROR: 'xsetroot' is not found!"
        fi
    fi
else
    logger "ERROR: 'feh' is not found!"
fi
