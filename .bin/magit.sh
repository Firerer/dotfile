#!/bin/sh
#
if git rev-parse --git-dir 2> /dev/null ; then
  emacs --no-window-system --eval "(progn (magit-status) (delete-other-windows))"
else
  echo 'Not in a git repository'
fi
