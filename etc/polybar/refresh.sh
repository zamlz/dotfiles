#!/bin/sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# Launch the bars ...

polybar="polybar --config=$HOME/etc/polybar/bar.conf"
$polybar top &
$polybar bot &

echo "Polybar has been started."
