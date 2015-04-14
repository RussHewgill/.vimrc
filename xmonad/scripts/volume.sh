#!/bin/bash

vol=$(amixer get Master -M)

if [[ -e ~/.config/.alert ]]
then
    color1='^bg(#B51414)^fg(#0F0F0F)'
    color2='^bg()^fg'
else
    color1=''
    color1=''
fi


if [[ $(echo $vol | tail -n 1 | perl -pe 's/.*(on|off).*/\1/') == 'on' ]]
then
  echo $color1 $(echo $vol | tail -n 1 | perl -pe 's/.*\[(\d+%).*/\1/') $color2
else
  echo $color2 Mt $color2
fi
