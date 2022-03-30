#!/bin/sh

xrandr --output eDP1 --primary --mode 2256x1504 --pos 0x0 --rotate normal \
       --output DP1 --off \
       --output DP2 --off \
       --output DP3 --mode 1920x1080 --pos 2256x0 --rotate normal \
       --output DP4 --off \
       --output VIRTUAL1 --off
