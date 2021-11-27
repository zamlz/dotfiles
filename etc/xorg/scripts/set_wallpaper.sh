#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

if [ -f "$HOME/.fehbg" ]; then
    logger "Setting wallpaper from ~/.fehbg"
    $HOME/.fehbg
else
    logger "No wallpaper found! Falling back to xsetroot"
    . $HOME/lib/shell/xrdb_colors
    xsetroot -bitmap ~/lib/bitmaps/tile.xbm -fg $XCOLOR0 -bg $XBACKGROUND
fi
