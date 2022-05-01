#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

XSCREENSAVER_CONFIG=$HOME/.xscreensaver
XSCREENSAVER_LOG=$HOME/.local/share/xscreensaver.log

which xscreensaver > /dev/null 2>&1
if [ $? -eq 0 ]; then

    # Kill any xscreensaver process if it exists
    killall xscreensaver \
        && logger "killed existing xscreensaver process" \
        || logger "No existing xscreensaver process found"

    # Wait until the processes have been shut down
    while pgrep -u $UID -x xscreensaver >/dev/null; do sleep 1; done

    # Don't start xscreensaver too soon in case xorg just came up
    sleep 1

    logger "checking for xscreensaver configuration"
    if [ -L $XSCREENSAVER_CONFIG ]; then

        logger "attempting to start xscreensaver"
        date > $XSCREENSAVER_LOG
        xscreensaver -no-splash >> $XSCREENSAVER_LOG 2>&1 &

        sleep 1 && [ $(pgrep -x xscreensaver --count) -eq 1 ] \
            && logger "xscreensaver started succesfully" \
            || logger "xscreensaver has failed to start!"
    else
        logger "xscreensaver configuration not found: $XSCREENSAVER_CONFIG"
    fi
else
    logger "ERROR: 'xscreensaver' is not found!"
fi
