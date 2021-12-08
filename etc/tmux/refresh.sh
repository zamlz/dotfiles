#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Starting tmux session"
tmux -f ~/etc/tmux/config -T 256 new-session -s "xorg-tmux-session" -d
if [ ! $? -eq 0 ]; then
    logger "tmux session has already started"
fi
