if [[ -z $DISPLAY ]] && [[ $(tty) = "/dev/tty1" ]];
then
  exec startx >> /tmp/xlog-"$(date +%F-%T)" 2>&1
fi
