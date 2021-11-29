#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - wmctrl
#   - rofi

# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"
logger "Attempting to delete herbstluftwm tag"

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
    exit 1
else
    logger "tag selected: $tag"
    hc merge_tag "$tag"
    if  [ $? -eq 0 ]; then
        logger "success!"
    else
        logger "merge_tag: Cannot merge the currently viewed tag"
        rofi -e "merge_tag: Cannot merge the currently viewed tag"
        exit 1
    fi
fi
