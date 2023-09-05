#!/bin/sh

# Set some important variables for sxhkd
export SXHKD_SHELL=sh

if which sxhkd > /dev/null 2>&1; then

    (killall sxhkd \
        && echo "killed existing sxhkd process") \
        || echo "No existing sxhkd process found"

    # Wait until the processes have been shut down
    while pgrep -u $UID -x sxhkd >/dev/null; do sleep 1; done

    echo "attempting to start sxhkd"
    sxhkd

    (sleep 1 && [ "$(pgrep -x sxhkd --count)" -eq 1 ] \
        && echo "sxhkd started successfully") \
        || echo "sxhkd has failed to start!"
else
    echo "ERROR: 'sxhkd' is not found!"
fi
