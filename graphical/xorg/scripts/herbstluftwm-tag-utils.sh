#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - wmctrl
#   - fzf 

# Num tags also gives us the index of the next new tag!
# NUM_TAGS=$(wmctrl -d | wc -l)

OPERATION_NAME=$1
if [ -z "$OPERATION_NAME" ]; then
    logger "ERROR: operation name must be specified!"
    exit 1
fi

CMD_GOTO="GOTO"
CMD_MOVE="MOVE"
CMD_REMOVE="REMOVE"

TEMP_DIR="/tmp/.htu"

hc() {
    herbstclient $@
}

tag_list() {
    # List current workspace tags (but don't show current workspace)
    wmctrl -d | grep -v "\*" | awk '{print $9}'
}

get_hc_command() {
    operation_name=$1
    case "${operation_name}" in
        "$CMD_GOTO") echo "use";;
        "$CMD_MOVE") echo "move" ;;
        "$CMD_REMOVE") echo "merge_tag";;
        *) logger "unknown operation name: ${operation_name}"; exit 1;;
    esac
    logger "will be performing op: $hc_command"
}

get_tag() {
    operation_name=$1
    
    mkdir -p $TEMP_DIR
    full_options_file=$(mktemp -p $TEMP_DIR)
    selected_option_file=$(mktemp -p $TEMP_DIR)

    # if you ever need to launch this without alacritty, it should just work!
    tag_list > ${full_options_file}
    # FIXME: how do I sync the font option with the one in sxhkd?
    alacritty \
        --class 'termprompt,termprompt' \
        --option "font.size=8" \
        --option "window.dimensions.lines=10" \
        --option "window.dimensions.columns=120" \
        --command $HOME/.config/herbstluftwm/fzf.sh \
        ${operation_name} ${full_options_file} ${selected_option_file}
    tag=$(cat ${selected_option_file})

    if [ -z "$tag" ]; then
        logger "no tag selected, aborting..."
        exit 2
    elif [ "$tag" = "λ" ] && [ "$operation_name" = "$CMD_REMOVE" ]; then
        logger "Cannot delete default tag! Aborting..."
        # FIXME: wait for user to press enter to close prompt
        exit 1
    fi
    logger "tag selected: $tag"
    echo $tag
}


tag=$(get_tag $OPERATION_NAME)
error_code=$?
if  [ $error_code -ne 0 ]; then
    exit $error_code
fi

hc_command=$(get_hc_command ${OPERATION_NAME})

# WHY DO WE ATTEMPT COMMAND BEFORE CHECKING FOR TAG?
# > Well, it may look like bad practice but it's actually quite robust.
# > Basically, I don't trust any sort of grep to check the see if the tag
# > is exists. Not for removing, we can consider that a success if it's
# > missing!
hc ${hc_command} "$tag"
error_code=$?

if  [ $error_code -eq 0 ]; then
    logger "success!"
elif  [ $error_code -eq 3 ]; then
    if [ "$OPERATION_NAME" = "remove" ]; then
        logger "success! ($tag doesn't exist)"
        exit 0
    fi
    logger "retying $hc_command after creating $tag"
    hc add "$tag"
    hc $hc_command "$tag"
elif  [ $error_code -eq 5 ] && [ "$OPERATION_NAME" = "remove" ]; then
    logger "merge_tag: Cannot merge the currently viewed tag"
    exit 1
else
    logger "unknown error code: $error_code"
    exit 1
fi
