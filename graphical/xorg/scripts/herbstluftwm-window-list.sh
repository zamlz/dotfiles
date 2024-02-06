#!/bin/sh

# for a given workspace name, show list of windows

workspace="$1"

# compare against the last field
workspace_id=$(wmctrl -d \
    | awk -v workspace="$workspace" '{ if ($NF == workspace) print $1 }')

wmctrl -lx \
    | awk -v workspace_id="$workspace_id" \
      '{ if ($2 == workspace_id) print $1 " " $3 " " $5 }'
