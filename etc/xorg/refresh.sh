# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

LOCAL_XINITRC_DIR=$HOME/etc/xorg/scripts

# We must run this outside of the scripts above because the window manager
# wants to know which display configuration to configure it's internal
# rules against. However, if we included it as part of the sourced scripts
# above, it would not be set fast enough before the window manager starts
# running commands.
logger "Reset default display profile"
$HOME/etc/xorg/reset_display_profile.sh

logger "Source local user-defined xinitrc.d files if they exist"
if [ -d "$LOCAL_XINITRC_DIR" ] ; then
    find "$LOCAL_XINITRC_DIR" -name "*.sh" | while read -r f; do
        [ -x "$f" ] && $f &
    done
    unset f
fi
