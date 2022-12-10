#!/bin/sh

# Need a logger just to keep track of things
. "$HOME/lib/shell/logging" && eval "$(get_logger "$0")"

# Set some important variables for sxhkd
SXHKD_CONFIG=$HOME/etc/sxhkd/rc
SXHKD_LOG=$HOME/.local/share/sxhkd.log
export SXHKD_SHELL=sh
export SXHKD_TERMINAL=alacritty

if which sxhkd > /dev/null 2>&1; then

    (killall sxhkd \
        && logger "killed existing sxhkd process") \
        || logger "No existing sxhkd process found"

    # Wait until the processes have been shut down
    while pgrep -u $UID -x sxhkd >/dev/null; do sleep 1; done

    logger "attempting to start sxhkd"
    date > "$SXHKD_LOG"
    sxhkd -c "$SXHKD_CONFIG" >> "$SXHKD_LOG" 2>&1 &

    (sleep 1 && [ "$(pgrep -x sxhkd --count)" -eq 1 ] \
        && logger "sxhkd started successfully") \
        || logger "sxhkd has failed to start!"
else
    logger "ERROR: 'sxhkd' is not found!"
fi
