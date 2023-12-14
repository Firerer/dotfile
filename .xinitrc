#!/bin/sh
# init file for window managers
# merge in defaults and keymaps
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
if [ -f "$sysresources" ]; then
    xrdb -merge "$sysresources"
fi

if [ -f "$sysmodmap" ]; then
    xmodmap "$sysmodmap"
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# xsetroot -solid black # simple bg
# [ -d ~/Pictures/Wallpapers ] && feh --bg-fill --randomize ~/Pictures/Wallpapers
# pidof blueman-applet || blueman-applet &
# pidof nm-applet || nm-applet &


# source xprofile
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

# run last to make sure applets are display correctly
# polybar main &

case $2 in
    awesome) awesome;;
    bspwm) bspwm-session ;;
    fluxbox) startfluxbox ;;
    gnome) gnome-session ;;
    i3|i3wm) i3 --shmlog-size 0 ;;
    kde) startplasma-x11 ;;
    leftwm) sxhkd & leftwm ;;
    *) xmonad ;;
esac
