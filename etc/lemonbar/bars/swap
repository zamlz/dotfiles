#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# System function

swap() {
    swap=$(free -h | grep Swap | awk '{print $3}')
    tot=$(free -h | grep Swap | awk '{print $2}')

    out="[swap %{F${GREEN}}${swap}/${tot}%{F-}]"

    echo -n $out
}
