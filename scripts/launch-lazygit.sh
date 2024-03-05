#!/usr/bin/env sh

# get the last focused wid by running the save-window-id.sh script
source /tmp/.wid/$(cat /tmp/.last_focused_wid)
if [ -z "${WINDOW_PWD}" ]; then
    cd $HOME
else
    cd $WINDOW_PWD
fi

lazygit
