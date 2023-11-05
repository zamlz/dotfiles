#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

FONT_DIR=$HOME/.local/share/fonts

which xset > /dev/null 2>&1
if [ $? -eq 0 ]; then
    if [ -d $FONT_DIR ]; then
        logger "rehashing locally installed fonts"
        xset +fp $FONT_DIR
        xset fp rehash
    else
        logger "WARNING: font rehash aborted as no local font directory exists"
    fi
else
    logger "ERROR: 'xset' is not found!"
fi
