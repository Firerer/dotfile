#!/bin/sh
# merge in defaults and keymaps
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
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

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

#source xprofile
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile
# wallpaper
xsetroot -solid black # simple bg
[ -d ~/Pictures/Wallpapers ] && feh --bg-fill ~/Pictures/Wallpapers

nm-applet &
polybar main &
case $2 in
    awesome) awesome;;
    bspwm) bspwm-session ;;
    budgie) budgie-desktop ;;
    cinnamon) cinnamon-session ;;
    deepin) startdde ;;
    enlightenment) enlightenment_start ;;
    fluxbox) startfluxbox ;;
    gnome) gnome-session ;;
    i3|i3wm) i3 --shmlog-size 0 ;;
    jwm) jwm ;;
    kde) startplasma-x11 ;;
    lxde) startlxde ;;
    lxqt) lxqt-session ;;
    mate) mate-session ;;
    xfce) xfce4-session ;;
    openbox) openbox-session ;;
    xmonad) xmonad ;;
    *) xmonad ;;
esac
