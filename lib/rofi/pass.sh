#!/bin/sh

pass_name=$(find ~/.password-store -type f | grep "gpg" | \
    sed -e "s|$HOME\/\.password-store\/||g" -e "s|\.gpg||g" | \
    rofi -dmenu -i -p "pass" )

if [ $pass_name != "" ]; then
    pass -c $pass_name
fi
