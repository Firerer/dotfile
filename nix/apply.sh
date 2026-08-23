set -o errexit
set -o nounset
set -o pipefail

readonly repo_root="$HOME/dotfile"
readonly manifest="$repo_root/nix/dotfiles.conf"
readonly state_home="${XDG_STATE_HOME:-$HOME/.local/state}"
readonly config_home="${XDG_CONFIG_HOME:-$HOME/.config}"
readonly profile_link="$state_home/nix/profiles/dotfiles"
readonly manifest_link="$config_home/user-tmpfiles.d/dotfiles.conf"

check_only=false

usage() {
  printf 'Usage: dotfiles-apply [--check]\n'
}

fail() {
  printf 'dotfiles-apply: %s\n' "$*" >&2
  exit 1
}

expand_home() {
  printf '%s\n' "${1//'%h'/$HOME}"
}

record_conflict() {
  conflicts+=("$1")
}

check_parents() {
  local path=$1
  local parent

  if [[ "$path" != "$HOME" && "$path" != "$HOME/"* ]]; then
    record_conflict "destination is outside the home directory: $path"
    return
  fi

  parent=$(dirname -- "$path")
  while [[ "$parent" != "$HOME" ]]; do
    if [[ -L "$parent" ]]; then
      record_conflict "refusing symlinked parent directory: $parent"
      return
    fi
    if [[ -e "$parent" && ! -d "$parent" ]]; then
      record_conflict "parent is not a directory: $parent"
      return
    fi
    parent=$(dirname -- "$parent")
  done
}

check_link() {
  local destination=$1
  local source=$2
  local current
  local expected

  check_parents "$destination"

  if [[ "$source" != "$repo_root" && "$source" != "$repo_root/"* ]]; then
    record_conflict "source is outside the repository: $source"
    return
  fi

  if [[ ! -e "$source" && ! -L "$source" ]]; then
    record_conflict "missing source: $source"
    return
  fi

  if [[ ! -e "$destination" && ! -L "$destination" ]]; then
    return
  fi

  if [[ ! -L "$destination" ]]; then
    record_conflict "refusing to replace non-symlink: $destination"
    return
  fi

  current=$(realpath --canonicalize-missing -- "$destination")
  expected=$(realpath --canonicalize-missing -- "$source")
  if [[ "$current" == "$expected" ]]; then
    return
  fi

  if [[ "$current" == "$repo_root" || "$current" == "$repo_root/"* ]]; then
    stale_links+=("$destination")
    return
  fi

  record_conflict "refusing foreign symlink: $destination -> $(readlink -- "$destination")"
}

while (($#)); do
  case $1 in
    --check)
      check_only=true
      ;;
    -h | --help)
      usage
      exit 0
      ;;
    *)
      usage >&2
      fail "unknown argument: $1"
      ;;
  esac
  shift
done

[[ $(uname -s) == Linux ]] || fail "only Linux is supported"
command -v nix >/dev/null || fail "nix is required"
command -v systemd-tmpfiles >/dev/null || fail "systemd-tmpfiles is required"
[[ -d "$repo_root/.git" ]] || fail "expected the repository at $repo_root"
[[ -f "$manifest" ]] || fail "missing manifest: $manifest"

declare -a conflicts=()
declare -a stale_links=()

# The middle tmpfiles fields are parsed to keep the manifest shape strict.
# shellcheck disable=SC2034
while read -r type destination mode user group age source remainder; do
  [[ -z "$type" || $type == \#* ]] && continue
  [[ -z "${remainder:-}" ]] || record_conflict "invalid manifest line for $destination"
  case $type in
    d)
      ;;
    L)
      check_link "$(expand_home "$destination")" "$(expand_home "$source")"
      ;;
    *)
      record_conflict "unsupported tmpfiles rule type: $type"
      ;;
  esac
done < "$manifest"

check_link "$manifest_link" "$manifest"
check_parents "$profile_link"

if [[ -e "$profile_link" && ! -L "$profile_link" ]]; then
  record_conflict "refusing to replace non-symlink profile: $profile_link"
fi

if ((${#conflicts[@]})); then
  printf 'dotfiles-apply: preflight failed:\n' >&2
  printf '  - %s\n' "${conflicts[@]}" >&2
  exit 1
fi

systemd-tmpfiles --user --dry-run --create "$manifest" >/dev/null

profile_store=$(nix build \
  --no-link \
  --print-out-paths \
  "path:$repo_root#default")
[[ -n "$profile_store" && $profile_store != *$'\n'* ]] || fail "profile build returned an unexpected result"

if "$check_only"; then
  printf 'Preflight passed. Profile: %s\n' "$profile_store"
  exit 0
fi

mkdir -p -- "$(dirname -- "$profile_link")" "$(dirname -- "$manifest_link")"
ln -sfn -- "$profile_store" "$profile_link"

for destination in "${stale_links[@]}"; do
  [[ -L "$destination" ]] && unlink -- "$destination"
done

if [[ ! -L "$manifest_link" ]]; then
  ln -s -- "$manifest" "$manifest_link"
fi

systemd-tmpfiles --user --create "$manifest"

printf 'Activated packages and dotfiles from %s\n' "$repo_root"
