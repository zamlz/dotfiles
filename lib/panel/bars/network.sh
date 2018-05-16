#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Network function
#   Requires to have nmcli installed.
#   Bad dependency but I don't know any other way yet.

network() {
    net=$(nmcli device | grep ' connected')
    color=${MAGENTA}

    if [ -z "${net}" ]; then
        net=$(nmcli device | grep 'unmanaged')
        color=${RED}
    fi

    DEVICE=$(echo $net | awk '{print $1}')
    TYPE=$(echo $net | awk '{print $2}')
    STATE=$(echo $net | awk '{print $3}')
    CONNECTION=$(echo $net | awk '{print $4}')
    SSID=" : ${CONNECTION}"

    out="[${TYPE} %{F${color}}${DEVICE}${SSID}%{F-}]"
    echo -n $out
}
