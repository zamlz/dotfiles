#!/bin/sh

# ENSURE that gpg-agent.conf contains the following line
#   pinentry-program $HOME/lib/rofi/pinentry.sh

echo "OK"
#echo "$PINENTRY_USER_DATA" >> /home/lindenk/test.log
while read cmd rest; do
    cmd=$(printf "%s" "$cmd" | tr 'A-Z' 'a-z')
    if [ -z "$cmd" ]; then
        continue;
    fi
    case "$cmd" in
        \#*)
        ;;
        getpin)
            _PP=$(echo "" | DISPLAY=:0 rofi -dmenu -password -p "Passphrase: " -lines 0)
            echo "D $_PP"
            echo "OK"
            ;;
        bye)
            echo "OK"
            exit 0
            ;;
        *)
            echo "OK"
            ;;
    esac
done
