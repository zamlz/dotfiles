#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Network function
#   Uses ip for network info,
#   but I still can't figure out how to get SSID

network() {
    net=$(ip -br addr | grep 'UP')
    color=${MAGENTA}

    if [ -z "${net}" ]; then
        net=$(ip -br addr | grep 'UNKNOWN')
        color=${RED}
    fi

    # We can use ip to get the following
    DEVICE=$(echo $net | awk '{print $1}')
    STATUS=$(echo $net | awk '{print $2}')
    IPADDR=$(echo $net | awk '{print $3}')
   
    # TODO: Obtain SSID somehow
    CONNECTION=$(echo $net | awk '{print $9}')
    SSID=": ${CONNECTION}"

    out="[network %{F${color}}${DEVICE}: ${IPADDR}%{F-}]"
    echo -n $out
}
