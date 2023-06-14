#!/usr/bin/env bash
# from https://sharats.me/posts/shell-script-best-practices/

set -o errexit
set -o nounset
set -o pipefail
if [[ "${TRACE-0}" == "1" ]]; then
  set -o xtrace
fi

if [[ "${1-}" =~ ^-*h(elp)?$ ]]; then
  echo 'Usage:  '"$(basename "$0")"'

Convert screenshot to text and copy to clipboard
'
    exit
fi

cd "$(dirname "$0")"

main() {
  maim -s /tmp/tmpimg.png && tesseract /tmp/tmpimg.png stdout | xclip -selection clipboard
}

main "$@"
