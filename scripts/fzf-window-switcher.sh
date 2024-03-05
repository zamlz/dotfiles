#!/usr/bin/env sh

get_window() {
    # this is kinda a hacky way to get rid of my custom prompts if they pop up
    wmctrl -lxp \
        | sed -e "/termprompt.termprompt.*Alacritty/d" \
        | awk '{printf "%s ", $1; for(i=6;i<=NF;++i) printf "%s ", $i; print ""}' \
        | fzf --reverse --prompt "Switch Window: " \
            --preview $HOME'/.config/sxhkd/get-window-info.sh {1}'
}

window=$(get_window)
if [ -n "${window}" ]; then
    wmctrl -i -a $(echo $window | awk '{print $1}')
fi
