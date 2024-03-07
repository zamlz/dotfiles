#!/bin/sh

# for a given workspace name, show list of windows

workspace="$1"

# compare against the last field
workspace_id=$(wmctrl -d \
    | awk -v workspace="$workspace" '{ if ($NF == workspace) print $1 }')

wmctrl -lxp \
    | awk -v workspace_id="$workspace_id" \
      '{ if ($2 == workspace_id) {printf "%s ", $1; for(i=6;i<=NF;++i) printf "%s ", $i; print ""} }'
