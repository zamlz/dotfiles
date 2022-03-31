#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# post setup phase
logger "Setting up color info for xorg apps"
logger "Setting colorscheme to gruvbox-black"
. $HOME/bin/xcolorscheme --set gruvbox-black

logger "Setting up colorscripts"

# FIXME: This variable needs to be in sync with `bin/colors` but it is being
# defined in two places, get it from some shared location
COLORSCRIPT_DIR=$HOME/lib/colorscripts
COLORSCRIPT_GIT_REPO="https://github.com/zamlz/colorscripts.git"

if [ ! -d "$COLORSCRIPT_DIR" ]; then
    git clone $COLORSCRIPT_GIT_REPO $COLORSCRIPT_DIR
fi
. $HOME/bin/colors --set hex


