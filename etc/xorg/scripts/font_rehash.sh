#!/bin/sh

# Need a logger just to keep track of things
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "rehashing locally installed fonts"
xset +fp $HOME/.local/share/fonts
xset fp rehash
