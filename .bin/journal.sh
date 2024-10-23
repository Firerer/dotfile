#!/bin/env bash
base_dir="$HOME/Documents/logseq/journals"
cd "$base_dir"
alacritty --title "take_journal" -e nvim "$base_dir/$(date +%Y_%m_%d).md"
