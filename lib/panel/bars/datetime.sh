#!/usr/bin/env sh

# Source the xcolors
. $HOME/lib/xorg/xcolor.sh

# Battery function

datetime() {
    out="$(date +'%a %b %d %I:%M')"
    echo -n $out
}
