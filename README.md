# dotfiles

Portable Linux packages managed by Nix, with live configuration files linked
from this checkout by systemd user tmpfiles. Home Manager and Stow are not
required, and editing a file under `~/dotfile/home` takes effect immediately.

## Requirements

- Linux with systemd user tmpfiles
- Nix with flakes enabled
- This repository cloned at exactly `~/dotfile`

## Install

Install the pinned package environment into the default Nix profile, then
apply and register the live configuration links:

```bash
cd ~/dotfile
nix profile install .
systemd-tmpfiles --user --create ~/dotfile/dotfiles.conf
```

The package is installed as the `dotfile` element (from the checkout name)
alongside any unrelated packages already in the default profile. The tmpfiles
declaration links itself into `~/.config/user-tmpfiles.d/`, so missing managed
links are recreated when the user tmpfiles service runs.

The non-forcing tmpfiles link rules preserve conflicting real paths and report
them. Move a reported conflict aside and run the tmpfiles command again.

## Layout

- `home/`: configuration linked into the home directory
- `flake.nix`: packages in the default profile
- `dotfiles.conf`: declarative link and parent-directory rules
- `home/.local/bin/`: personal commands linked individually into `~/.local/bin`
- `others/`: personal material outside package management

## Updating and rollback

Update the pinned package set intentionally, review `flake.lock`, then apply:

```bash
nix flake update
nix profile upgrade dotfile
systemd-tmpfiles --user --create ~/dotfile/dotfiles.conf
```

Git and `flake.lock` define the configuration and package versions. To roll
back, check out the desired revision and run the update commands again.

Nix-installed fonts are exposed through the managed fontconfig fragment.
Fcitx GTK/Qt modules may still need distro-native integration when native
applications cannot discover modules from the Nix profile.
