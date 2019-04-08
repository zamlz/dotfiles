#!/usr/bin/env sh

# userathost with kernel info
echo "$(whoami)@$(hostname) $(uname -o) $(uname -r)"
