#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# Set some important variables for sxhkd
SXHKD_CONFIG=$HOME/etc/sxhkd/rc
export SXHKD_SHELL=sh
export SXHKD_TERMINAL=kitty

logger "killing any existing sxhkd daemon"
pkill -x sxhkd

logger "starting new sxhkd daemon"
sxhkd -c $SXHKD_CONFIG &
