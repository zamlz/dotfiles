#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# System function

filesystems() {

    root=$(lsblk -rpo MOUNTPOINT,NAME | grep '/ ' | awk '{print $2}')
    rootsize=$(df -h | grep ${root} | awk '{print $2}')
    rootused=$(df -h | grep ${root} | awk '{print $3}')
    
    boot=$(lsblk -rpo MOUNTPOINT,NAME | grep '/boot' | awk '{print $2}')
    bootsize=$(df -h | grep ${boot} | awk '{print $2}')
    bootused=$(df -h | grep ${boot} | awk '{print $3}')

    out="[filesystems %{F${YELLOW}}"
    out="${out} $root: $rootused/$rootsize"
    out="${out} $boot: $bootused/$bootsize"
    out="${out}%{F-}]"

    echo -n $out
}
