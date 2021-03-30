#!/bin/sh

# sometimes thunderbolt dock is not recognized yet so this triggers it
boltctl &
export DISPLAY=:0

logger -t DISPLAY_MOD "Attempting to change display mode to '$DISPLAY_MODE'"
su zamlz -c "/home/zamlz/lib/profiles/display/$DISPLAY_MODE" &
