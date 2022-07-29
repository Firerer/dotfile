#!/usr/bin/env bash

awesome-client "$(cat debug.lua)"
Xephyr :5 & sleep 1 ; DISPLAY=:5 awesome
