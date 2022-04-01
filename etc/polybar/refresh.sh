#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

POLYBAR_CONFIG=$HOME/.config/polybar/system.conf
POLYBAR_TOP_LOG=$HOME/.local/share/polybar.top.log
POLYBAR_BOT_LOG=$HOME/.local/share/polybar.bot.log

which polybar > /dev/null 2>&1
if [ $? -eq 0 ]; then

    # Kill any polybar process if it exists
    killall polybar \
        && logger "killed existing polybar process" \
        || logger "No existing polybar process found"

    # Wait until the processes have been shut down
    while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

    logger "checking for polybar system configuration"
    if [ -L "$POLYBAR_CONFIG" ]; then

        logger "attempting to start polybar"
        # Yes, the config passed here is different from what we check
        opts="--config=$HOME/etc/polybar/bar.conf"

        date > $POLYBAR_TOP_LOG
        polybar $opts top >> $POLYBAR_TOP_LOG 2>&1 &

        date > $POLYBAR_BOT_LOG
        polybar $opts bot >> $POLYBAR_BOT_LOG 2>&1 &

        sleep 1 && [ $(pgrep -x polybar --count) -eq 2 ] \
            && logger "polybar started successfully" \
            || logger "polybar has failed to start!"
    else
        logger "polybar system configuration not found: $POLYBAR_CONFIG"
    fi
else
    logger "ERROR: 'polybar' is not found!"
fi

