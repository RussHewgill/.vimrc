#!/bin/bash

nosong='^r(3x30)'

if pidof cmus >/dev/null; then
  duration=$(cmus-remote -Q | grep duration | cut -d' ' -f2)
  position=$(cmus-remote -Q | grep position | cut -d' ' -f2)
  out=$(echo 'scale=2; 30 - (30 * ' $position / $duration')' | bc -l)
  echo '^p(-2)^p(_TOP)^r(3x'$out')^p()^fg()'
#else
  #echo $nosong
fi

