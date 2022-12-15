#!/bin/sh

# P W D C F W
# Warp the newly spawned terminal to the
# (P)resent (W)orking (D)irectory of the
# (C)urrently (F)ocused (W)indow.
#
# Needs xdotool to be installed

TERMINAL_APP=${1}
TARGET_WINDOWID=$(xdotool getwindowfocus); export TARGET_WINDOWID

${TERMINAL_APP} &
