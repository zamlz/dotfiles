#!/bin/sh

# Create a list of the path
dmenu_path() {
    cachedir=${XDG_CACHE_HOME:-"$HOME/.cache"}
    if [ -d "$cachedir" ]; then
	    cache=$cachedir/dmenu_run
    else
    	cache=$HOME/.dmenu_cache # if no xdg dir, fall back to dotfile in ~
    fi
    IFS=:
    if stest -dqr -n "$cache" $PATH; then
    	stest -flx $PATH | sort -u | tee "$cache"
    else
    	cat "$cache"
    fi
}

# Specify the input arguments
#       1) Mointor to display on
#       2) height of the launcher
#       3) width of the launcher
#       4) border of the launcer

# source the xcolor
. $HOME/lib/xorg/xcolor.sh

# Specify options here
monitor=$1
border=$4
height=$(($2 - $border - $border))
width=$(($3- $border - $border))
xoff=$border
yoff=$border
font=$(xrdb -query | grep '*font' | \
        awk '{print $2}' | sed -e 's|xft:||g')

# Construct the options
options="-w ${width} -h ${height} -x ${xoff} -y ${yoff}
        -nb ${XBACKGROUND} -nf ${XFOREGROUND}
        -sb ${BLACK} -sf ${XFOREGROUND}
        -fn ${font} -s ${monitor}"

# To allow truly dettached 
echo "$(dmenu_path | dmenu $options) &" | /bin/sh &
