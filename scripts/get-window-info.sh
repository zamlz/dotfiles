#!/bin/sh

WINDOW_HEX_ID=${1}
WINDOW_ID=$(printf "%d" "${WINDOW_HEX_ID}")

WINDOW_DETAILS=$(wmctrl -lxp | grep ${WINDOW_HEX_ID})
WORKSPACE_ID=$(($(echo $WINDOW_DETAILS | awk '{print $2}') + 1))
PROCESS_ID=$(echo $WINDOW_DETAILS | awk '{print $3}')
WINDOW_CLASS=$(echo $WINDOW_DETAILS | awk '{print $4}')
WINDOW_TITLE=$(echo $WINDOW_DETAILS \
    | awk '{for(i=6;i<=NF;++i) printf "%s ", $i; print ""}')

RED="$(tput setaf 1)"
GREEN="$(tput setaf 2)"
YELLOW="$(tput setaf 3)"
BLUE="$(tput setaf 4)"
MAGENTA="$(tput setaf 5)"
CYAN="$(tput setaf 6)"
BOLD="$(tput bold)"
RESET="$(tput sgr0)"

echo "${BOLD}WINDOW_ID${RESET} = ${CYAN}${WINDOW_ID}${RESET}"
echo "${BOLD}WINDOW_ID (hex)${RESET} = ${CYAN}${WINDOW_HEX_ID}${RESET}"
echo "${BOLD}WORKSPACE_ID${RESET} = ${YELLOW}${WORKSPACE_ID}${RESET}"
echo "${BOLD}PROCESS_ID${RESET} = ${RED}${PROCESS_ID}${RESET}"
echo "${BOLD}CLASS${RESET} = ${MAGENTA}'${WINDOW_CLASS}'${RESET}"
echo "${BOLD}TITLE${RESET} = ${GREEN}'${WINDOW_TITLE}'${RESET}"

window_id_file=/tmp/.wid/${WINDOW_ID}
if [ -f $window_id_file ]; then
    source $window_id_file
echo "${BOLD}WINDOW_PWD${RESET} = ${BLUE}${WINDOW_PWD}${RESET}"
fi
