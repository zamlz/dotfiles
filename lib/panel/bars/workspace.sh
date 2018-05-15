#!/usr/bin/env sh

# Sourced the xcolors init.sh
# . $HOME/lib/xorg/xcolor.sh

# Workspace function
# Needs xprop (typically installed with xorg)

workspace() {
    cur=$(xprop -root _NET_CURRENT_DESKTOP | awk '{print $3}')
    all=$(xprop -root _NET_NUMBER_OF_DESKTOPS | awk '{print $3}')
    out="%{F${BLUE}}"

    # Construct a list of active workspaces
    winlist=$(xprop -root _NET_CLIENT_LIST | cut -d " " -f5- | tr -d ',')
    activews=""
    for win in ${winlist}; do
        activews="${activews} $(xprop -id ${win} _NET_WM_DESKTOP)"
    done

    for ws in $(seq $all); do
        wid=$(($ws - 1))

        # If there are open windows check
        if [ -n "$(echo ${activews} | grep ${wid})" ]; then
            wsc="%{+u}${ws}%{-u}"
        else
            wsc="${ws}"
        fi

        # Current desktop check
        if [ "$wid" = "$cur" ]; then
            out="${out} %{+o}${wsc}%{-o}"
        else
            out="${out}  ${wsc} "
        fi

    done
    out="${out}%{F-}"

    echo -n $out
}
