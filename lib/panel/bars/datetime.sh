#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Battery function

datetime() {
    out="[date %{F${GREEN}}$(date +'%a %b %d %I:%M')%{F-}]"
    echo -n $out
}
