#!/usr/bin/sh
git config core.hooksPath || git config core.hooksPath ./dev/hooks
pv=$PWD
cd ~/dotfile
stow .
cd $pv
