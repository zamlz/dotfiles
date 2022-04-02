#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Setting XDG user-dirs.dirs"
xdg-user-dirs-update --set DESKTOP      "$HOME"
xdg-user-dirs-update --set DOWNLOAD     "$HOME/tmp"
xdg-user-dirs-update --set DOCUMENTS    "$HOME/usr"
xdg-user-dirs-update --set MUSIC        "$HOME/usr"
xdg-user-dirs-update --set PICTURES     "$HOME/usr"
xdg-user-dirs-update --set VIDEOS       "$HOME/usr"
xdg-user-dirs-update --set PUBLICSHARE  "$HOME/usr"
xdg-user-dirs-update --set TEMPLATES    "$HOME/lib/templates"

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
