# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"

logger "Source local user-defined xinitrc.d files if they exist"
if [ -d $HOME/etc/xorg/scripts ] ; then
    find "$HOME/etc/xorg/scripts" -name "*.sh" | while read -r f; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

