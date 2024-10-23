#!/usr/bin/env bash
# from https://sharats.me/posts/shell-script-best-practices/

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then
    set -o xtrace
fi
cd "$(dirname "$0")"
paru -Slq | fzf --multi --preview 'paru -Si {1}' | xargs -ro paru -S
