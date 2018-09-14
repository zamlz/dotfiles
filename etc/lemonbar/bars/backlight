#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Backlight function
#   Needs xbacklight installed

backlight() {
    bl=$(xbacklight | awk -F. '{print $1}')

    if [ -n "${bl}" ]; then
        out="[light %{F${YELLOW}}${bl}%%{F-}]"
    else
        out="[light %{F${YELLOW}}N/A%{F-}]"
    fi
    echo -n $out
}
