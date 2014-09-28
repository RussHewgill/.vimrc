xrandr --setprovideroutputsource modesetting NVIDIA-0
xrandr --auto

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

#stupid nvidia, first time doesn't work
/*~/monitorsettings.sh*/
/*~/monitorsettings.sh*/

xrandr --output HDMI-1-0 --pos 1920x0

xrandr --dpi 84


numlockx on
xset m 0 5
xset r rate 250 30

xmodmap ~/.Xmodmap
xsetroot -cursor_name left_ptr
xrdb -merge ~/.Xresources
redshift -O 5200

exec xmonad


##### not needed ?

/*setxkbmap -option ctrl:nocaps*/
/*wmname LG3D*/
