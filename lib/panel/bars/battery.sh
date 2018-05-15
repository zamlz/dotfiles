#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Battery function

battery() {
    batloc="/sys/class/power_supply/"
    batlist=$(ls $batloc | grep "BAT")
    out="%{F${CYAN}}"

    for bat in $batlist; do
        cap="$(cat ${batloc}${bat}/capacity)"
        stat="$(cat ${batloc}${bat}/status)"
        out="${out} ${bat}: ${cap}% (${stat})"
    done
    out="${out}%{F-}"

    echo -n $out
}
