#!/bin/env bash
base_dir="/home/di/Documents/logseq/journals"
alacritty --title "take_journal" -e nvim "$base_dir/$(date +%Y_%m_%d).md"
