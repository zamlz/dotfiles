# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

LOCAL_XINITRC_DIR=$HOME/etc/xorg/scripts

logger "Source local user-defined xinitrc.d files if they exist"
if [ -d $LOCAL_XINITRC_DIR ] ; then
    find "$LOCAL_XINITRC_DIR" -name "*.sh" | while read -r f; do
        [ -x "$f" ] && $f &
    done
    unset f
fi

