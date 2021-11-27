#!/bin/sh

# Rehash locally installed fonts
xset +fp $HOME/.local/share/fonts
xset fp rehash
