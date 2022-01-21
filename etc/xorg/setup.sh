#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# post setup phase
logger "Setting up color info for xorg apps"
logger "Setting colorscheme to gruvbox-black"
. $HOME/bin/xcolorscheme --set gruvbox-black

logger "Setting up colorscripts"
COLORSCRIPT_DIR=$HOME/lib/colorscripts
if [ ! -d "$COLORSCRIPT_DIR" ]; then
    git clone https://github.com/zamlz/colorscripts.git $COLORSCRIPT_DIR
fi
. $HOME/bin/colors --set hex


