#!/bin/sh

# export DISPLAY=":0"
# export XAUTHORITY=$HOME/.Xauthority

# Need to only check the first one...
# DP-{1.1,1.2,1.3}
MONITOR="DP-1.1"
LAPTOP="DP-2"

MONITOR_STATE=$(xrandr | grep "$MONITOR connected")

if [ -n "$MONITOR_STATE" ]; then
    $HOME/lib/profiles/display/thinkpad_dock
else
    $HOME/lib/profiles/display/default
fi
