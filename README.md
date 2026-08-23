# dotfiles

Portable Linux packages managed by Nix, with live configuration files linked
from this checkout by systemd user tmpfiles. Home Manager and Stow are not
required, and editing a file under `~/dotfile/home` takes effect immediately.

## Requirements

- Linux with systemd user tmpfiles
- Nix with flakes enabled
- This repository cloned at exactly `~/dotfile`

## Apply

Inspect the migration without changing home-directory state:

```bash
nix run ~/dotfile#apply -- --check
```

Build the pinned package environment and apply the links:

```bash
nix run ~/dotfile#apply
```

The command manages a dedicated profile at
`~/.local/state/nix/profiles/dotfiles`. It does not alter packages installed in
the default Nix profile. The tmpfiles declaration is linked into
`~/.config/user-tmpfiles.d/`, so missing managed links are recreated when the
user tmpfiles service runs.

Activation refuses to replace real files, real directories, or symlinks that
point outside this repository. Move a reported conflict aside and run the
check again. Existing Stow links into `~/dotfile` are recognized and migrated.

## Layout

- `home/`: configuration linked into the home directory
- `nix/packages.nix`: packages in the dedicated profile
- `nix/dotfiles.conf`: declarative link and parent-directory rules
- `nix/apply.nix` and `nix/apply.sh`: checked activation command
- `.bin/`: legacy personal scripts, intentionally unchanged and unmanaged
- `notes/` and `others/`: personal material outside package management

## Updating and rollback

Update the pinned package set intentionally, review `flake.lock`, then apply:

```bash
nix flake update
nix run ~/dotfile#apply -- --check
nix run ~/dotfile#apply
```

Git and `flake.lock` define the configuration and package versions. To roll
back, check out the desired revision and run the apply command again.

Nix-installed fonts are exposed through the managed fontconfig fragment.
Fcitx GTK/Qt modules may still need distro-native integration when native
applications cannot discover modules from the Nix profile.
