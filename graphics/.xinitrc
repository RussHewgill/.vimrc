xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

setxkbmap -option ctrl:nocaps
numlockx
xset m 0 5
xset r rate 250 30

xsetroot -cursor_name left_ptr

xrdb -merge ~/.Xresources

#stupid nvidia, first time doesn't work
~/monitorsettings.sh
~/monitorsettings.sh
xrandr --output LVDS-1-0 --dpi 110
xrandr --output HDMI-1-0 --dpi 84

redshift -O 5200

exec xmonad


