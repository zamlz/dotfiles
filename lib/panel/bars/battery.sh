#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Battery function

battery() {
    batloc="/sys/class/power_supply/"
    batlist=$(ls $batloc | grep "BAT")
    out="[power"

    for bat in $batlist; do
        cap="$(cat ${batloc}${bat}/capacity)"
        stat="$(cat ${batloc}${bat}/status)"

        if [ "$stat" = "Charging" ]; then
            out="${out} %{F${GREEN}}${bat}: ${cap}%%{F-}"

        elif [ $cap -le 5 ]; then
            out="${out} %{F${RED}}${bat}: ${cap}%%{F-}"

        elif [ $cap -le 25 ]; then
            out="${out} %{F${YELLOW}}${bat}: ${cap}%%{F-}"

        else
            out="${out} %{F${CYAN}}${bat}: ${cap}%%{F-}"
        fi

    done
    out="${out}]"

    echo -n $out
}
