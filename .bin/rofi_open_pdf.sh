#!/usr/bin/sh
fd --type f -e pdf . ~/Downloads ~/Sync/ \
    | rofi -keep-right -dmenu -i -p FILES -multi-select \
    | xargs -I {} xdg-open {}
