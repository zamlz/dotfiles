#!/bin/sh

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
logger "Initializing profile dmenu"

# Find the profile I want
PROFILE_DIR="$HOME/lib/profiles/$(hostname)"

# There must be a cleaner way to get this choice
profile=$( find  $PROFILE_DIR -not -path '*/.*' -type f,l | \
    sed -e "s|${PROFILE_DIR}/||g" | \
    rofi -dmenu -i -lines 3 -p "Profile" )

if [ -n "$profile" ]; then
    logger "profile selected: $profile"
    $PROFILE_DIR/$profile
    echo $profile > $HOME/tmp/.$(hostname).xorg.current_profile

    # FIXME: Need to generalize polybar refreshing in this script
    logger "refreshing polybar"
    $HOME/etc/polybar/refresh.sh

    logger "attempting to refresh ${WINDOW_MANAGER} monitors"
    REFRESH_SCRIPT=$HOME/etc/${WINDOW_MANAGER}/scripts/refresh_display.sh
    if [ -f $REFRESH_SCRIPT ]; then
        $REFRESH_SCRIPT
    else
        logger "refresh_display.sh script missing for $WINDOW_MANAGER"
    fi
else
    logger "no profile selected, aborting..."
fi
