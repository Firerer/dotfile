#!/usr/bin/sh
if ! command -v stow &> /dev/null ;
then
    sudo pacman -Sy stow
fi


cd ~/dotfile
stow .

