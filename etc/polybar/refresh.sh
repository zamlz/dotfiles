#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the bars ...
opts="--config=$HOME/etc/polybar/bar.conf"
polybar $opts top &
polybar $opts bot &

echo "Polybar has been started."
