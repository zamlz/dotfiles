#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "changing the caps locks key to the escape key"
setxkbmap -option caps:escape
