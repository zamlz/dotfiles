#!/bin/sh

# choose pinentry depending on PINENTRY_USER_DATA
# this *only works* with gpg 2
# see https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=802020

#echo "PINENTRY_USER_DATA=<${PINENTRY_USER_DATA}>, options=<$@>" >> \
#    /tmp/pinentry.log

case $PINENTRY_USER_DATA in
    qt)
        exec /usr/bin/pinentry-qt4 "$@"
        ;;
    tty)
        exec /usr/bin/pinentry-tty "$@"
        ;;
    rofi)
        exec $HOME/lib/rofi/pinentry-rofi "$@"
        ;;
    none)
        echo "Multi-pinentry Error"
        exit 1 # do not ask for passphrase
        ;;
    *)
        exec /usr/bin/pinentry "$@"
        ;;
esac

