#!/bin/sh

# Git Repository Init
# -------------------

git clone "https://github.com/zamlz/dotfiles.git" dotfiles
for item in $(ls -A dotfiles); do
    mv ./dotfiles/$item ./$item
done
rmdir dotfiles
git remote set-url origin --push "git@github.com:zamlz/dotfiles.git"

# FIXME: Remove once branch is merged
git checkout purge

# Setup Dotfiles
# --------------
./bin/asd setup $(realpath .)
