#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# Increases the typing speed of the keyboard by increasing the repeat rate.
xset r rate 400 50
