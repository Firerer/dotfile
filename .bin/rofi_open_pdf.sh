#!/usr/bin/sh
fd --type f -e pdf . ~/Documents ~/Downloads /run/media/di/ \
    | rofi -keep-right -dmenu -i -p FILES -multi-select \
    | xargs -I {} xdg-open {}
