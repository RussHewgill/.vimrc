#!/bin/bash

if [[ $(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | perl -lane 'print $F[1] if /native-path/') =~ .*null.* ]]; then
    echo ''
else
    percent=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | perl -lane 'print $F[1] if /percent/')
    charging=$(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | perl -lane 'print $F[1] if /state/')
    if [[ $charging =~ discharging ]]
    then
      echo ' ^fg(#5c5c5c)^r(2x34)^fg() ↓'$percent
    else
      echo ' ^fg(#5c5c5c)^r(2x34)^fg() ⚡'$percent
    fi
fi
