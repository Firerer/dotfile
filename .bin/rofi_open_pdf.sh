fd --type f -e pdf . $HOME/Documents | rofi -keep-right -dmenu -i -p FILES -multi-select | xargs -I {} xdg-open {}
