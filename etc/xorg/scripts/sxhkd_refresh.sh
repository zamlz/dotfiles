#!/bin/sh

# Set some important variables for sxhkd
export SXHKD_SHELL=sh
export SXHKD_TERMINAL=urxvt

# Kill an existing sxhkd session and start a new one
pkill -x sxhkd
sxhkd &

