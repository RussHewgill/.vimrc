#! /bin/bash

if pidof cmus >/dev/null && [[ $(cmus-remote -Q | grep status| cut -d' ' -f2) == 'playing' ]]; then
  cmus-remote -u
fi

WLP=~/Wallpapers/dualmonitor/*
i3lock -i $(shuf -n1 -e $WLP)



