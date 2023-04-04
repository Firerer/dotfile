#!/usr/bin/bash
set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
    echo 'Usage: ./download.sh'
    exit
fi

cd ~

# paru
if ! command -v paru &> /dev/null ;
  then
  sudo pacman -S --needed git rustup base-devel
  rustup default stable

  rm -rf paru
  git clone https://aur.archlinux.org/paru.git
  cd paru
  makepkg -si
  cd ..
fi

# clone my dotfile
if [ ! -d "dotfile" ]; then
    git clone git@github.com:Firerrd/dotfile.git
    cd dotfile && stow . && cd ~
    source .bashrc
fi

# font
paru -S --needed ttf-hack-nerd noto-fonts noto-fonts-cjk noto-fonts-extra noto-fonts-emoji
# apps
paru -S --needed firefox stow
# input
paru -S --needed fcitx5 fcitx5-gtk fcitx5-qt fcitx5-chinese-addons fcitx5-configtool
# terminal related
paru -S --needed alacritty fish starship zellij neovim fzf exa zoxide
# helpful tools
paru -S --needed btop htop man neofetch cronie trash-cli
paru -S --needed fd ripgrep ripgrep-all tealdeer difftastic
# ocr
# paru -S --needed tesseract  tesseract-data-chi-sim tesseract-data-chi-tra tesseract-data-eng
#sys tools
paru -S --needed pavucontrol lxappearance xclip xorg-xclipboard playerctl blueman
# optional NTFS fs
# paru -S --needed ntfs-3g
#theme
paru -S --needed catppuccin-gtk-theme-mocha papirus-icon-theme
# xmonad
paru -S --needed xmonad xmonad-contrib xmonad-extras dunst feh polybar maim rofi
# KDE & tiling
# paru -S --needed bismuth
# xorg-server-xephyr # dbug tool
