#!/usr/bin/env zsh

# actions
LOCK="Lock Screen"
QUIT="Quit Herbstluftwm"
SHUTDOWN="Shut Down"
REBOOT="Reboot"

# Either pass what we want to do as an argument to the script
# or get it through rofi
systask="$*"
if [ -z "$systask" ]; then
    systask=$(echo "${LOCK}\n${QUIT}\n${REBOOT}\n${SHUTDOWN}" \
        | fzf --reverse --prompt "System Action: ")
fi

if [ "$systask" = "${LOCK}" ]; then
    echo "Performing action: ${LOCK}"

    # Refresh gpg-agent
    gpg-connect-agent --no-autostart RELOADAGENT /bye > /dev/null

    # Start the locker
    WALLPAPER_FILE=$(grep feh "$HOME/.fehbg" \
        | awk '{print substr($(NF), 2, length($(NF)) - 2)}')
    logger "Found wallapaper file: $WALLPAPER_FILE"

    if [ -f "$WALLPAPER_FILE" ]; then
        # TODO: Fix this code if I'm using the navi wallpaper
        #resolution=$(xrandr | awk '/\*/ {print $1}')
        #magick -size "$resolution" xc:black "$WALLPAPER_FILE" \
        #    -gravity center -composite /tmp/.i3lock.png
        #exec i3lock -tnefi "/tmp/.i3lock.png"
        exec i3lock -tnefi "$WALLPAPER_FILE"
    else
        exec i3lock -nef --color=000000
    fi

elif [ "$systask" = "${QUIT}" ]; then
    echo "Performing action: ${QUIT}"
    sudo herbstclient quit
    if [ "$?" -ne 0 ]; then
        read -t 5 -n 1 key
        exit 1
    fi

elif [ "$systask" = "${SHUTDOWN}" ]; then
    echo "Performing action: ${SHUTDOWN}"
    sudo poweroff
    if [ "$?" -ne 0 ]; then
        read -t 5 -n 1 key
        exit 1
    fi

elif [ "$systask" = "${REBOOT}" ]; then
    echo "Performing action: ${POWEROFF}"
    sudo reboot
    if [ "$?" -ne 0 ]; then
        read -t 5 -n 1 key
        exit 1
    fi
fi
