#!/bin/sh

# Git Repository Init
# -------------------

DOTFILES_HTTPS_URL="https://github.com/zamlz/dotfiles.git" 
DOTFILES_GIT_URL="git@github.com:zamlz/dotfiles.git"

git clone ${DOTFILES_HTTPS_URL} dotfiles
for item in $(ls -A dotfiles); do
    mv "./dotfiles/$item" "./$item"
done
rmdir dotfiles
git remote set-url origin --push ${DOTFILES_GIT_URL}

# Setup Dotfiles
# --------------
./bin/asd setup "$(realpath .)"

# Install Packages
# ----------------
./bin/asd sync
