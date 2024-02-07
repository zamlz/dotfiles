#!/usr/bin/env sh

get_window() {
    # this is kinda a hacky way to get rid of my custom prompts if they pop up
    wmctrl -lxp \
        | sed -e "s/ $(hostname) //g" \
        | sed -e "/fzf.fzf *Alacritty/d" \
        | fzf --reverse --prompt "Switch Window: "
}

window=$(get_window)
if [ -n "${window}" ]; then
    wmctrl -i -a $(echo $window | awk '{print $1}')
fi
