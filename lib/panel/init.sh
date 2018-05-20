#!/usr/bin/env sh

#   L E M O N B A R
#
#   the bar takes in two arguments
#       1) monitor number
#       2) height of the bar
#       3) border of the bar

# Get the monitor
MONITOR=$1

# Import the colors
. $HOME/lib/xorg/xcolor.sh

# Import the individual bar functions
. $HOME/lib/panel/bars/userathost.sh
. $HOME/lib/panel/bars/workspace.sh
. $HOME/lib/panel/bars/datetime.sh
. $HOME/lib/panel/bars/network.sh
. $HOME/lib/panel/bars/backlight.sh
. $HOME/lib/panel/bars/battery.sh

. $HOME/lib/panel/bars/filesystems.sh
. $HOME/lib/panel/bars/memory.sh
. $HOME/lib/panel/bars/swap.sh
. $HOME/lib/panel/bars/cpu.sh

# Specify various other settings for lemonbar
border=$3
height=$(($2 - $border - $border))
width=$((1600 - $border - $border))
xoff=0
yoff=0
font=$(xrdb -query | grep '*font' | \
        awk '{print $2}' | sed -e 's|xft:||g')

# Construction our configuration for lemonbar
opt_top="-g ${width}x${height}+${xoff}+${yoff}
         -B ${XBACKGROUND} -F ${XFOREGROUND}
         -f ${font} -r ${border} -R ${BLACK}"

yoff=$((${yoff} + ${border} + ${border}))
opt_bot="-g ${width}x${height}+${xoff}+${yoff}
         -B ${XBACKGROUND} -F ${XFOREGROUND}
         -f ${font} -r ${border} -R ${BLACK}"

# Create the top content function
top_content () {
    echo -n "%{S${MONITOR}}"
    echo -n "%{l} "
    userathost
    workspace
    echo -n "%{c}"
    datetime
    echo -n "%{r}"
    network
    backlight
    battery
    echo -n " ";
}

# Create the bottom content function
bot_content() {
    echo -n "%{S${MONITOR}}"
    echo -n "%{l} "
    echo -n "%{c}"
    echo -n "%{r}"
    filesystems
    swap
    memory
    cpu
    echo -n " ";
}


# Serve the content function 
HZ=".5"
if [ -n "$MONITOR" ]; then
    # Serve the content function to lemonbar
    (while true; do echo "$(top_content)"; sleep $HZ; done;) | \
        lemonbar ${opt_top} &
    (while true; do echo "$(bot_content)"; sleep $HZ; done;) | \
        lemonbar ${opt_bot} -b &
else
    # Server the content function to STDOUT
    while true; do echo "$(top_content)"; sleep $HZ; done;
fi
