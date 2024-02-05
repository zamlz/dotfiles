#!/usr/bin/env zsh

# TODO: Need to figure out a way to deal with missing keycards. This causes
#       scdaemon to continously search for it (unclear if there is a timeout).
#       We should instead error out very quickly so the user knows that there
#       is no card inserted. Ideally, GPG should do this, but if we need an
#       workaround, it should belong in here.

# Reset pinentry?
GPG_TTY=$(tty)

# create prompt
PROMPT="Password Store"
if [ "$1" = "--qrcode" ]; then
    QRMODE="true"
    PROMPT="${PROMPT} [QrCode]"
fi
PROMPT="${PROMPT}: "

# Find the password I want
pass_name=$(find $PASSWORD_STORE_DIR -type f | grep -v "\.gpg-id" | \
    grep "\.gpg" | sed -e "s|$PASSWORD_STORE_DIR\/||g" -e "s|\.gpg||g" | \
    fzf --reverse --prompt="$PROMPT")

# retrieve password
if [ "$pass_name" != "" ]; then
    if [ -z "$QRMODE" ]; then
        pass -c "$pass_name"
        result_status=$?
    else
        pass --qrcode "$pass_name"
        result_status=$?
    fi

    if [ $result_status -ne 0 ]; then
        echo "Password decryption failed: [ERROR CODE: ${result_status}]"
    fi
fi
