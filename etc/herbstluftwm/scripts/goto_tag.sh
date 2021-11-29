#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - wmctrl
#   - rofi

# Exit Status:
# 0: Success
# 1: Unkown error code from internal failure

# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"
logger "Initializing herbstluftwm window manager"

hc() {
    herbstclient $@
}

tag_list() {
    wmctrl -d | awk '{print $9}'
}

tag=$(tag_list | rofi -dmenu -i -p "Goto Tag")

if [ -z "$tag" ]; then
    logger "no tag selected, aborting..."
else
    logger "tag selected: $tag"
    hc use "$tag"
    error_code=$?
    if  [ $error_code -eq 0 ]; then
        logger "success!"
    elif  [ $error_code -eq 3 ]; then
        fmt_tag=$(echo "[$tag]" | tr -t ' ' '-')
        logger "creating new workspace $fmt_tag"
        hc add "$fmt_tag"
        hc use "$fmt_tag"
    else
        logger "unknown error code: $error_code"
        exit 1
    fi
fi
