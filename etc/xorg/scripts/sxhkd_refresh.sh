#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# Set some important variables for sxhkd
export SXHKD_SHELL=sh
export SXHKD_TERMINAL=urxvt

# Kill an existing sxhkd session and start a new one
pkill -x sxhkd
sxhkd &
