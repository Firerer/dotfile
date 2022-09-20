#!/bin/sh
function magit() {
  emacsclient -te "(magit-status)"
  if [ $? -nq 0 ]; then
      echo "not at a git repo"
  fi
}
magit
