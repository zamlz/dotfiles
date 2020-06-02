#!/bin/sh
xrandr \
    --output DVI-D-0 --off \
    --output HDMI-0 --mode 1920x1080 --pos 3000x502 --rotate normal \
    --output DP-0 --mode 1920x1080 --pos 1920x0 --rotate left \
    --output DP-1 --off \
    --output DP-2 --primary --mode 1920x1080 --pos 0x420 --rotate normal \
    --output DP-3 --off \
    --output DP-4 --off \
    --output DP-5 --off
