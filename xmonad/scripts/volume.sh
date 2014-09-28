#!/bin/bash

vol=$(amixer get Master -M)

if [[ $(echo $vol | tail -n 1 | perl -pe 's/.*(on|off).*/\1/') == 'on' ]]
then
  echo $(echo $vol | tail -n 1 | perl -pe 's/.*\[(\d+%).*/\1/')
else
  echo Mt
fi
