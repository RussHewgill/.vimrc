#!/bin/bash

numlockx on

# no no mouse accel
xset m 0 5

# keyboard repeat rate
xset r rate 250 30

xset s 1200

xmodmap ~/.Xmodmap
xmodmap -e "keycode 49 = F1 asciitilde grave asciitilde"
xmodmap -e "keycode 67 = grave F1 F1 F1 F1 F1 XF86Switch_VT_1"

xsetroot -cursor_name left_ptr
xrdb -merge ~/.Xresources
