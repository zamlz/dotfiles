#!/bin/sh

# Monitor setup

# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"
logger "Refreshing herbstluftwm display/monitor settings"

alias hc="herbstclient"

hc unlock
hc detect_monitors
