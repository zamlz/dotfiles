#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "refreshing display profile file"
. $HOME/lib/profiles/$(hostname)/__DEFAULT__
