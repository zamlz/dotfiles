#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - wmctrl
#   - fzf 

# Num tags also gives us the index of the next new tag!
NUM_TAGS=$(wmctrl -d | wc -l)

hc() {
    herbstclient $@
}

tag_list() {
    # List current workspace tags (but don't show current workspace)
    wmctrl -d | grep -v "\*" | awk '{print $9}'
}

get_fzf_prompt() {
    case "${operation_name}" in
        "goto") echo "JUMP TO";;
        "move") echo "MOVE WINDOW TO" ;;
        "remove") echo "REMOVE";;
        *) logger "unknown operation name: ${operation_name}"; exit 1;;
    esac
}

get_hc_command() {
    case "${operation_name}" in
        "goto") echo "use";;
        "move") echo "move" ;;
        "remove") echo "merge_tag";;
        *) logger "unknown operation name: ${operation_name}"; exit 1;;
    esac
}

get_tag() {
    tag=$(tag_list | fzf --print-query --reverse --prompt="$(get_fzf_prompt) TAG:" | awk 'END {print}')
    echo "${tag}" | tr ' ' '-'
}

main() {
    operation_name=$1
    if [ -z "$operation_name" ]; then
        logger "ERROR: operation name must be specified!"
        exit 1
    fi
    tag=$(get_tag)
    logger "tag selected: $tag"

    if [ -z "$tag" ]; then
        logger "no tag selected, aborting..."
        exit 2
    elif [ "$tag" = "λ" ] && [ "$operation_name" = "remove" ]; then
        logger "Cannot delete default tag! Aborting..."
        # FIXME: wait for user to press enter to close prompt
        exit 1
    fi

    hc_command=$(get_hc_command)
    logger "will be performing op: $hc_command"

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
        if [ "$operation_name" = "remove" ]; then
            logger "success! ($tag doesn't exist)"
            exit 0
        fi
        logger "retying $hc_command after creating $tag"
        hc add "$tag"
        hc $hc_command "$tag"
    elif  [ $error_code -eq 5 ] && [ "$operation_name" = "remove" ]; then
        logger "merge_tag: Cannot merge the currently viewed tag"
        exit 1
    else
        logger "unknown error code: $error_code"
        exit 1
    fi
}
