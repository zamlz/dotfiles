#!/bin/sh

export DISPLAY=":0"
export XAUTHORITY=$HOME/.Xauthority

# Need to only check the first one...
# DP-{1.1,1.2,1.3}
MONITOR="DP-1.1"
LAPTOP="DP-2"

MONITOR_STATE=$(xrandr | grep "$MONITOR connected")

if [ -n "$MONITOR_STATE" ]; then
    
    xset -dpms
    xrandr --output DP-1.1 --mode 1920x1080 --pos 3000x656 --rotate normal \
           --output DP-1.2 --primary --mode 1920x1080 --pos 0x544 --rotate normal \
           --output DP-1.3 --mode 1920x1080 --pos 1920x0 --rotate left \
           --output HDMI-0 --off \
           --output DP-2 --off \
           --output DP-1 --off \
           --output DP-0 --off
           #--output DP-2 --mode 1920x1080 --pos 0x544 --rotate normal \
else

    xset +dpms
    xrandr --output DP-1.1 --off \
           --output DP-1.2 --off \
           --output DP-1.3 --off \
           --output HDMI-0 --off \
           --output DP-2 --mode 1920x1080 --pos 0x0 --rotate normal \
           --output DP-1 --off --output DP-0 --off

fi
