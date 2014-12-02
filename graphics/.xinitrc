# vim: set filetype=sh :
#xrandr --setprovideroutputsource modesetting NVIDIA-0
#xrandr --auto

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

xrandr --output HDMI1 --pos 1920x0

xrandr --dpi 96

source ~/initsettings.sh

if [[ -e ~/Documents/.notepad.swp ]]; then
    rm ~/Documents/.notepad.swp
fi

redshift -O 5200

wmname LG3D

exec xmonad
#exec i3

##### not needed ?

#see initsetting.sh

#numlockx on
#xset m 0 5
#xset r rate 250 30
#xmodmap ~/.Xmodmap
#xmodmap -e "keycode 49 = F1 asciitilde grave asciitilde"
#xmodmap -e "keycode 67 = grave F1 F1 F1 F1 F1 XF86Switch_VT_1"
#xsetroot -cursor_name left_ptr
#xrdb -merge ~/.Xresources

