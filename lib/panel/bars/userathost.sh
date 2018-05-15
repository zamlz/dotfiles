#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# userathost function

userathost() {
    out="%{F${RED}}$(whoami)@$(hostname)%{F-}"
    echo -n $out
}
