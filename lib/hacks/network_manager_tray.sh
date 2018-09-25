#!/bin/sh

# This hack lets me run nm-applet when i don't have a system tray installed

nm-applet       2>&1 > /dev/null &
stalonetray     2>&1 > /dev/null
killall nm-applet
