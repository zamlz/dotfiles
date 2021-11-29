#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - wmctrl
#   - rofi

# Exit Status:
# 0: Success
# 1: Unkown error code from internal failure
# 2: Default tag is attempted to be merged
# 3: Merge tag is focused tag

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
elif [ "$tag" = "[λ]" ]; then
    logger "Cannot delete default tag! Aborting..."
    rofi -e "Cannot delete default tag! Aborting..."
    exit 2
else
    logger "tag selected: $tag"
    hc merge_tag "$tag"
    error_code=$?
    if  [ $error_code -eq 0 ]; then
        logger "success!"
    elif  [ $error_code -eq 3 ]; then
        logger "merge_tag: Cannot merge the currently viewed tag"
        rofi -e "merge_tag: Cannot merge the currently viewed tag"
        exit 3
    else
        logger "unknown error code: $error_code"
        exit 1
    fi
fi
