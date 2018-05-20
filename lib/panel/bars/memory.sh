#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# System function

memory() {
    mem=$(free -h | grep Mem | awk '{print $3}')
    tot=$(free -h | grep Mem | awk '{print $2}')

    out="[memory %{F${BLUE}}${mem}/${tot}%{F-}]"

    echo -n $out
}
