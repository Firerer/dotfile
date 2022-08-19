#!/usr/bin/env sh
cd ~/dotfile
stow .
ln -s .config/astronvim .config/nvim/lua
mv .config/nvim/lua/astronvim .config/nvim/lua/user
