#!/bin/sh

# TODO: Need to figure out a way to deal with missing keycards. This causes
#       scdaemon to continously search for it (unclear if there is a timeout).
#       We should instead error out very quickly so the user knows that there
#       is no card inserted. Ideally, GPG should do this, but if we need an
#       workaround, it should belong in here.

# Find the password I want

PROMPT="password"
if [ "$1" = "--qrcode" ]; then
    QRMODE="true"
    PROMPT="${PROMPT} [QrCode]"
fi

pass_name=$(find ~/usr/passwords -type f | grep -v "\.gpg-id" | \
    grep "\.gpg" | sed -e "s|$HOME\/usr\/passwords\/||g" -e "s|\.gpg||g" | \
    rofi -dmenu -i -p "$PROMPT" )

if [ "$pass_name" != "" ]; then
    export PINENTRY_USER_DATA='rofi'
    export PASSWORD_STORE_DIR="$HOME/usr/passwords"
    if [ -z "$QRMODE" ]; then
        pass -c "$pass_name"
        result_status=$?
    else
        pass --qrcode "$pass_name"
        result_status=$?
    fi

    if [ $result_status -ne 0 ]; then
        rofi -e "Password decryption failed: [ERROR CODE: ${result_status}]"
    fi
fi
