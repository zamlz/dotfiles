#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# Makes the caps locks key into the escape key
setxkbmap -option caps:escape
