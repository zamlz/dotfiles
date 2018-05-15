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
. $HOME/lib/panel/bars/battery.sh
. $HOME/lib/panel/bars/datetime.sh

# Specify various other settings for lemonbar
border=0
height=$(($2 - $border - $border))
width=1600
xoff=0
yoff=0
font=$(xrdb -query | grep '*font' | \
        awk '{print $2}' | sed -e 's|xft:||g')

# Construction our configuration for lemonbar
options="-g ${width}x${height}+${xoff}+${yoff}
         -B ${XBACKGROUND} -F ${XFOREGROUND}
         -f ${font} -r ${border}"

# Create the content function
content () {
    echo -n "%{S${MONITOR}}"
    echo -n "%{l} "
    userathost
    workspace
    echo -n "%{c}"
    datetime
    echo -n "%{r}"
    battery
    echo -n " ";
}

# Serve the content function to lemonbar
(while true; do echo "$(content)"; sleep .5; done;) | \
    lemonbar ${options} &
