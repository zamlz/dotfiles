#!/bin/bash

. $HOME/org/config/lib/shell/colorenv

TMP=/tmp/banner

#figlet -f $1 -t "$(hostname | tr '[a-z]' '[A-Z]')"
#figlet -f $1 -t "$(hostname | tr '[a-z]' '[A-Z]')" | sed -e 's|\\|\\\\|g' > $TMP

cat $HOME/org/config/lib/banners/logos/$1 > $TMP

echo "  ${Green}\l${Rst}@${Bold}${Blue}\n.\o ${Black}(\U online)${Rst}" >> $TMP
echo "  ${Cyan}$(uname -o) ${Rst}${Purple}\r ${Bold}${Black}(\v)${Rst}" >> $TMP
echo "  ${Red}$(uname -p) ${Yellow}\m${Rst}" >> $TMP
echo "  \t - \d" >> $TMP
echo "" >> $TMP
