#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# System function

filesystems() {

    out="[filesystems %{F${YELLOW}}"

    bootdir='/boot '
    boot=$(lsblk -rpo MOUNTPOINT,NAME | grep "$bootdir" | awk '{print $2}')
    if [ -n "$boot" ]; then
        bootsize=$(df -h | grep ${boot} | awk '{print $2}')
        bootused=$(df -h | grep ${boot} | awk '{print $3}')
        bootdir=$(echo $bootdir | tr -d ' ')
        out="${out} $boot($bootdir):$bootused/$bootsize"
    fi
   
    rootdir='/ '
    root=$(lsblk -rpo MOUNTPOINT,NAME | grep "$rootdir" | awk '{print $2}')
    if [ -n "$root" ]; then
        rootsize=$(df -h | grep ${root} | awk '{print $2}')
        rootused=$(df -h | grep ${root} | awk '{print $3}')
        rootdir=$(echo $rootdir | tr -d ' ')
        out="${out} $root($rootdir):$rootused/$rootsize"
    fi
   
    homedir='/home '
    home=$(lsblk -rpo MOUNTPOINT,NAME | grep "$homedir" | awk '{print $2}')
    if [ -n "$home" ]; then
        homesize=$(df -h | grep ${home} | awk '{print $2}')
        homeused=$(df -h | grep ${home} | awk '{print $3}')
        homedir=$(echo $homedir | tr -d ' ')
        out="${out} $home($homedir):$homeused/$homesize"
    fi

    out="${out}%{F-}]"

    echo -n $out
}
