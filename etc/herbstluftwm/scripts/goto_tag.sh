#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - wmctrl
#   - rofi

# Setup the logger
. $HOME/lib/shell/logging && eval "$(get_logger $0)"
logger "Attempting to goto herbstluftwm tag"

Super=Mod4
hc() {
    herbstclient $@
}

tag_list() {
    wmctrl -d | awk '{print $9}'
}

# Num tags also gives us the index of the next new tag!
num_tags=$(wmctrl -d | wc -l)

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
        fmt_tag=$(echo "[$num_tags:$tag]" | tr -t ' ' '-')
        logger "creating new workspace $fmt_tag"
        hc add "$fmt_tag"
        hc use "$fmt_tag"
    else
        logger "unknown error code: $error_code"
        exit 1
    fi
fi
