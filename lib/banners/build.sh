#!/bin/sh

TMP=/tmp/banner

figlet -f $1 -t "$(hostname | tr '[a-z]' '[A-Z]')"
figlet -f $1 -t "$(hostname | tr '[a-z]' '[A-Z]')" | sed -e 's|\\|\\\\|g' > $TMP
echo "" >> $TMP
echo "This is \n:\l (\s \m \r)" >> $TMP
echo "Date: \t \d" >> $TMP
echo "" >> $TMP

cat $TMP
