#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# System function

filesystems() {

    out="[filesystems %{F${YELLOW}}"
    
    boot=$(lsblk -rpo MOUNTPOINT,NAME | grep '/boot' | awk '{print $2}')
    if [ -n "$boot" ]; then
        bootsize=$(df -h | grep ${boot} | awk '{print $2}')
        bootused=$(df -h | grep ${boot} | awk '{print $3}')
        out="${out} $boot: $bootused/$bootsize"
    fi
    
    root=$(lsblk -rpo MOUNTPOINT,NAME | grep '/ ' | awk '{print $2}')
    if [ -n "$root" ]; then
        rootsize=$(df -h | grep ${root} | awk '{print $2}')
        rootused=$(df -h | grep ${root} | awk '{print $3}')
        out="${out} $root: $rootused/$rootsize"
    fi
    
    home=$(lsblk -rpo MOUNTPOINT,NAME | grep '/home' | awk '{print $2}')
    if [ -n "$home" ]; then
        homesize=$(df -h | grep ${home} | awk '{print $2}')
        homeused=$(df -h | grep ${home} | awk '{print $3}')
        out="${out} $home: $homeused/$homesize"
    fi

    out="${out}%{F-}]"

    echo -n $out
}
