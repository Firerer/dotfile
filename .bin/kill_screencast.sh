kill $(ps aux | grep gjs | grep Screencast | grep -v 'grep' | awk '{print $2}')
