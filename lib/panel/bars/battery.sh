#!/usr/bin/env sh

# Source the xcolors
. $HOME/lib/xorg/xcolor.sh

# Battery function

battery() {
    batloc="/sys/class/power_supply/"
    batlist=$(ls $batloc | grep "BAT")
    out=""

    for bat in $batlist; do
        cap="$(cat ${batloc}${bat}/capacity)"
        stat="$(cat ${batloc}${bat}/status)"
        out="${out} ${bat}: ${cap}% (${stat})"
    done

    echo -n $out
}
