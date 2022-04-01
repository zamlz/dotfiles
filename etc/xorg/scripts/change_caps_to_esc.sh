#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

which setxkbmap > /dev/null 2>&1
if [ $? -eq 0 ]; then
    logger "changing the caps locks key to the escape key"
    setxkbmap -option caps:escape
else
    logger "ERROR: 'setxkbmap' is not found!"
fi
