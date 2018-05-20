#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Backlight function
#   Needs xbacklight installed

backlight() {
    bl=$(xbacklight | awk -F. '{print $1}')

    out="[light %{F${YELLOW}}${bl}%%{F-}]"

    echo -n $out
}
