#!/usr/bin/env sh

# Source the xcolors
. $HOME/lib/xorg/xcolor.sh

# Workspace function
# Needs xprop and bc to be installed

workspace() {
    cur=$(xprop -root _NET_CURRENT_DESKTOP | awk '{print $3}')
    all=$(xprop -root _NET_NUMBER_OF_DESKTOPS | awk '{print $3}')
    out=""

    # Construct a list of active workspaces
    winlist=$(xprop -root _NET_CLIENT_LIST | cut -d " " -f5- | tr -d ',')
    activews=""
    for win in ${winlist}; do
        activews="${activews} $(xprop -id ${win} _NET_WM_DESKTOP)"
    done

    for ws in $(seq $all); do
        wid="$(echo "$ws - 1" | bc)"

        # If there are open windows check
        if [ -n "$(echo ${activews} | grep ${wid})" ]; then
            wsc="%{+o}${ws}%{-o}"
        else
            wsc="${ws}"
        fi

        # Current desktop check
        if [ "$wid" = "$cur" ]; then
            out="${out} %{+u}${wsc}%{-u}"
        else
            out="${out}  ${wsc} "
        fi

    done

    echo -n $out
}
