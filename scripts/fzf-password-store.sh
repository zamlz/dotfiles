#!/usr/bin/env zsh

# Depends on:
# - feh
# - imagemagick
# - xclip

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
    echo "Retrieving password: [${pass_name}]"
    PASSWORD=$(pass "$pass_name" | head -n1)
    result_status=$?
    if [ $result_status -ne 0 ]; then
        echo "Password decryption failed: [ERROR CODE: ${result_status}]"
    fi

    pwfile=/tmp/.pws
    touch $pwfile
    if [ -z "$QRMODE" ]; then
        echo $PASSWORD > $pwfile
        nohup xclip -selection clipboard $pwfile > /dev/null 2>&1 &
    else
        echo -n "$PASSWORD" | qrencode --size 14 -o $pwfile
        convert $pwfile -negate $pwfile
        nohup feh -x --title "feh:pass: $pass_name" -g +200+200 $pwfile > /dev/null 2>&1 &
    fi
    sleep 0.1
    rm $pwfile
fi
