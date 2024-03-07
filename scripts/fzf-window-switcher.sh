#!/usr/bin/env sh

get_window() {
    # this is kinda a hacky way to get rid of my custom prompts if they pop up
    wmctrl -lxp \
        | sed -e "/termprompt.termprompt.*Alacritty/d" \
        | awk '{printf "%s ", $1; for(i=6;i<=NF;++i) printf "%s ", $i; print ""}' \
        | fzf --reverse --prompt "Switch Window: " \
            --preview $HOME'/.config/sxhkd/get-window-info.sh {1}' \
            --preview-window=down,7 \
            --ansi
}

window=$(get_window | awk '{print $1}')
if [ -n "${window}" ]; then
    # This command should set focus of the window but it doesn't work when
    # the windows are in herbstluftwm frames. It does handly workspace
    # switching correctly
    wmctrl -i -a $window
    # this command fails at changing workspace but does work with herbstluftwm
    # titles so I'm running both.
    xdotool windowfocus $window
fi
