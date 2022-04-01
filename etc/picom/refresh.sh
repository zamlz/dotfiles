#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

PICOM_CONFIG=$HOME/.config/picom/picom.conf
PICOM_LOG=$HOME/.local/share/picom.log

which picom > /dev/null 2>&1
if [ $? -eq 0 ]; then

    # Kill any picom process if it exists
    killall picom \
        && logger "killed existing picom process" \
        || logger "No existing picom process found"

    # Wait until the processes have been shut down
    while pgrep -u $UID -x picom >/dev/null; do sleep 1; done

    # Don't start picom too soon in case xorg just came up
    sleep 1

    logger "checking for picom configuration"
    if [ -L $PICOM_CONFIG ]; then

        logger "attempting to start picom"
        date > $PICOM_LOG
        picom >> $PICOM_LOG 2>&1 &

        sleep 1 && [ $(pgrep -x picom --count) -eq 1 ] \
            && logger "picom started succesfully" \
            || logger "picom has failed to start!"
    else
        logger "picom configuration not found: $PICOM_CONFIG"
    fi
else
    logger "ERROR: 'picom' is not found!"
fi
