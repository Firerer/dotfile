#!/usr/bin/env bash

awesome-client "$(cat debug.lua)"
# Xephyr :5 & sleep 1 ; DISPLAY=:5 awesome
Xephyr :1 -ac -br -noreset -screen 1152x720 & sleep 1
DISPLAY=:1.0 awesome
