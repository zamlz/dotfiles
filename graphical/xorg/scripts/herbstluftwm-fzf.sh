#!/bin/sh

# Goto a particular workspace or make it if it doesn't exist

# Dependencies:
#   - fzf 

OPERATION_NAME=$1
FULL_OPTIONS_FILE=$2
SELECTION_OPTION_FILE=$3

if [ -z "${OPERATION_NAME}" ] || [ -z "${FULL_OPTIONS_FILE}" ] || [ -z "${SELECTION_OPTION_FILE}" ]; then
    echo "Error: not all arguments provided!" >&2;
    exit 1;
fi

get_fzf_prompt() {
    operation_name=$1
    case "${operation_name}" in
        "GOTO") echo "Jump to";;
        "MOVE") echo "Move window to" ;;
        "REMOVE") echo "Remove";;
        *) echo "Error: unknown operation name: ${operation_name}" >&2; 
           exit 1;;
    esac
}

FZF_PROMPT=$(get_fzf_prompt ${OPERATION_NAME})

fzf --print-query \
    --reverse \
    --prompt="${FZF_PROMPT} workspace: " \
    --preview="$HOME/.config/herbstluftwm/window-list.sh {}" \
    --preview-label='[Window List]' < ${FULL_OPTIONS_FILE} \
    --preview-window=right,80 \
    | awk 'END {print}' \
    | tr ' ' '-' > ${SELECTION_OPTION_FILE}
