#!/bin/sh

# GnuPG Setup Script
# ------------------

. $HOME/lib/shell/logging && eval "$(get_logger $0)"
. $HOME/lib/shell/utils

logger "Setting up GNU/Emacs"

CONFIG_SOURCE=$HOME/etc/emacs
CONFIG_FILE=${CONFIG_SOURCE}/config.org

logger "Tangling emacs config file"
emacs -Q --batch \
      --eval "(require 'org)" \
      --eval "(require 'cl-lib)" \
      --eval "(setq python-indent-guess-indent-offset-verbose nil)" \
      --eval "(setq python-indent-offset 4)" \
      --eval "(advice-add 'sh-set-shell :around (lambda (orig-fun &rest args) (cl-letf (((symbol-function 'message) #'ignore)) (apply orig-fun args))))" \
      --eval "(org-babel-tangle-file \"${CONFIG_FILE}\")"
