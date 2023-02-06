#!/bin/sh
# merge in defaults and keymaps
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
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


DEFAULT_SESSION=xmonad
dbus_args=(--exit-with-session)
# command `startx kde` first arg is xterm, why?
case $2 in
    awesome) dbus_args+=(awesome) ;;
    bspwm) dbus_args+=(bspwm-session) ;;
    budgie) dbus_args+=(budgie-desktop) ;;
    cinnamon) dbus_args+=(cinnamon-session) ;;
    deepin) dbus_args+=(startdde) ;;
    enlightenment) dbus_args+=(enlightenment_start) ;;
    fluxbox) dbus_args+=(startfluxbox) ;;
    gnome) dbus_args+=(gnome-session) ;;
    i3|i3wm) dbus_args+=(i3 --shmlog-size 0) ;;
    jwm) dbus_args+=(jwm) ;;
    kde) dbus_args+=(startplasma-x11) ;;
    lxde) dbus_args+=(startlxde) ;;
    lxqt) dbus_args+=(lxqt-session) ;;
    mate) dbus_args+=(mate-session) ;;
    xfce) dbus_args+=(xfce4-session) ;;
    openbox) dbus_args+=(openbox-session) ;;
    xmonad) dbus_args+=(xmonad) ;;
    *) dbus_args+=($DEFAULT_SESSION) ;;
esac


pidof nm-applet || nm-applet &
pidof polybar || polybar main &


pidof dunst || dunst &

pidof fcitx5 || fcitx5 &&
  export GTK_IM_MODULE=fcitx5 &&
  export QT_IM_MODULE=fcitx5 &&
  export XMODIFIERS=@im=fcitx5 &

# wallpaper
xsetroot -solid black # simple bg
# feh --bg-fill ~/Pictures/Wallpapers &
# dbus-launch ${dbus_args[*]}
xmonad
