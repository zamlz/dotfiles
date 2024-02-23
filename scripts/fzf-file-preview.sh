#!/usr/bin/env zsh

if [ -d $1 ]; then
    tree -C $1
elif [ -f $1 ]; then
    cat $1
else;
    echo "ERROR: not a file or directory! No preview available"
fi
