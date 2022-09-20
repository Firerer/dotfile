#!/usr/bin/sh
fd --type f -e pdf . ~/Documents ~/Downloads \
    | rofi -keep-right -dmenu -i -p FILES -multi-select \
    | xargs -I {} emacs {}
