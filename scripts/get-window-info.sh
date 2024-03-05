#!/bin/sh

WINDOW_HEX_ID=${1}
WINDOW_ID=$(printf "%d" "${WINDOW_HEX_ID}")

WINDOW_DETAILS=$(wmctrl -lxp | grep ${WINDOW_HEX_ID})
WORKSPACE_ID=$(($(echo $WINDOW_DETAILS | awk '{print $2}') + 1))
PROCESS_ID=$(echo $WINDOW_DETAILS | awk '{print $3}')
WINDOW_CLASS=$(echo $WINDOW_DETAILS | awk '{print $4}')
WINDOW_TITLE=$(echo $WINDOW_DETAILS \
    | awk '{for(i=6;i<=NF;++i) printf "%s ", $i; print ""}')

echo "WINDOW_ID = ${WINDOW_ID}"
echo "WINDOW_ID (hex) = ${WINDOW_HEX_ID}"
echo "WORKSPACE_ID = ${WORKSPACE_ID}"
echo "PROCESS_ID = ${PROCESS_ID}"
echo "CLASS = '${WINDOW_CLASS}'"
echo "TITLE = '${WINDOW_TITLE}'"

window_id_file=/tmp/.wid/${WINDOW_ID}
if [ -f $window_id_file ]; then
    cat $window_id_file | sed -e 's/=/ = /g'
fi
