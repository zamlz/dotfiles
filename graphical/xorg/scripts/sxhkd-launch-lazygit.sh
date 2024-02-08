#!/usr/bin/env sh

TARGET_WINDOWID=$(xdotool getwindowfocus)
source /tmp/.wid/$TARGET_WINDOWID

alacritty \
    --class 'termprompt,termprompt' \
    --option 'window.dimensions.lines=40' \
    --option 'window.dimensions.columns=200' \
    --working-directory $WINDOW_PWD \
    --command lazygit
