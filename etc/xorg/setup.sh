#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"

# post setup phase
logger "Setting up color info for xorg apps"
logger "Setting colorscheme to gruvbox-black"
. $HOME/bin/xcolorscheme --set gruvbox-black

# FIXME: Remove this comment when I figure out how to setup colorscripts
# . $HOME/bin/colors --set hex


