#!/usr/bin/env sh

# maim on it's own is a nice minimal screenshot tool that literally prints the
# output back to STDOUT. We take that binary output and pipe it to a file and
# the user's clipboard.

maim --hidecursor "$@" /dev/stdout \
    | tee "/tmp/$(date +'%Y-%m-%dT%H:%M:%S%:z').png" \
    | xclip -selection clipboard -target image/png
