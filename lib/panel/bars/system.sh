#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# System function

system() {
    mem=$(free -h | grep Mem | awk '{print $3}')
    swap=$(free -h | grep Swap | awk '{print $3}')

    out="[system %{F${YELLOW}}mem: $mem swap: $swap%{F-}]"

    echo -n $out
}
