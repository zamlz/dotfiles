#!/usr/bin/env sh

#   L E M O N B A R
#
#   the bar takes in two arguments
#       1) monitor number
#       2) height of the bar

# Get the monitor
MONITOR=$1

# Import the colors
. $HOME/lib/xorg/xcolor.sh

# Import the individual bar functions
. $HOME/lib/panel/bars/userathost.sh
. $HOME/lib/panel/bars/workspace.sh
. $HOME/lib/panel/bars/datetime.sh
. $HOME/lib/panel/bars/system.sh
. $HOME/lib/panel/bars/network.sh
. $HOME/lib/panel/bars/battery.sh

# Specify various other settings for lemonbar
border=2
height=$(($2 - $border - $border))
width=$((1600 - $border - $border))
xoff=0
yoff=0
font=$(xrdb -query | grep '*font' | \
        awk '{print $2}' | sed -e 's|xft:||g')

# Construction our configuration for lemonbar
options="-g ${width}x${height}+${xoff}+${yoff}
         -B ${XBACKGROUND} -F ${XFOREGROUND}
         -f ${font} -r ${border} -R ${BLACK}"

# Create the content function
content () {
    echo -n "%{S${MONITOR}}"
    echo -n "%{l} "
    userathost
    datetime
    workspace
    echo -n "%{c}"
    echo -n "%{r}"
    system
    network
    battery
    echo -n " ";
}


# Serve the content function 
HZ=".5"
if [ -n "$MONITOR" ]; then
    # Serve the content function to lemonbar
    (while true; do echo "$(content)"; sleep $HZ; done;) | \
        lemonbar ${options} &
else
    # Server the content function to STDOUT
    while true; do echo "$(content)"; sleep $HZ; done;
fi
