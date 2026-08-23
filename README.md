# dotfiles

Personal Linux packages managed by a pinned Nix flake, with live configuration
linked from this checkout by systemd user tmpfiles. Home Manager and Stow are
not required.

The current package environment supports `x86_64-linux`. Alacritty is wrapped
with `nixGLIntel`, so this profile currently targets an Intel graphics system.

## Requirements

- Linux on x86-64 with systemd user tmpfiles
- Nix with flakes enabled
- This repository cloned at exactly `~/dotfile`
- `paru` and `pacman` when installing the optional Arch package manifest

## Bootstrap

On a new machine, run the repository copy of the management command:

```bash
~/dotfile/home/.local/bin/dotfiles bootstrap
```

Bootstrap performs these operations in order:

1. installs or upgrades the pinned Nix profile;
2. installs packages listed in `packages/arch.txt` when running on Arch;
3. creates the managed configuration links;
4. generates the current COSMIC wallpaper and terminal themes;
5. reloads the user systemd manager and starts managed timers and paths;
6. runs the complete health check.

Tmpfiles uses non-forcing `L` entries. Existing real files and directories are
preserved as conflicts rather than overwritten. Move each reported conflict to
a backup location, rerun bootstrap, and compare it with the managed version
before deleting the backup.

After bootstrap, the installed command provides three operations:

```bash
dotfiles apply       # recreate links and activate generated/user-service state
dotfiles check       # verify links, packages, the flake, and manifest
dotfiles bootstrap   # upgrade packages, apply configuration, then check
```

## Managed state

- `home/`: files and directories exposed through `dotfiles.conf`
- `home/.local/bin/`: personal management and automation commands
- `home/.agents/`: reusable global agent skills
- `packages/arch.txt`: applications unavailable from the Linux Nix package set
- `flake.nix` and `flake.lock`: pinned Nix package environment
- `dotfiles.conf`: links, required parent directories, and systemd enablement
- `others/`: personal material outside package and link management

Only selected portable COSMIC settings are managed. Display identities,
hardware-specific pinned workspaces, application state, and other mutable
COSMIC data remain local to each machine.

Default application associations in `~/.config/mimeapps.list` are intentionally
per-machine and are not linked by this repository.

## Generated files

The following ignored files are generated inside the checkout because their
contents depend on `$HOME` or the active COSMIC light/dark mode:

- `home/.config/alacritty/current-theme.toml`
- `home/.config/superfile/theme/auto.toml`
- `home/.config/cosmic/com.system76.CosmicBackground/v1/all`

`dotfiles apply` regenerates them. The checkout therefore needs to remain
writable.

## User services

The COSMIC theme path and Omni Google Drive timer are enabled declaratively by
`dotfiles.conf`. The synchronization service runs only when both of these
exist:

- `~/.config/rclone/rclone.conf`
- `~/Documents/Omni`

Rclone credentials are deliberately not tracked.

## Updating and rollback

Update the pinned inputs intentionally, inspect the lock-file change, and apply:

```bash
cd ~/dotfile
nix flake update
dotfiles bootstrap
```

Git and `flake.lock` define the managed state. To roll back, check out the
desired revision and rerun `dotfiles bootstrap`.

Nix-installed fonts are exposed through the managed fontconfig fragment.
Fcitx GTK/Qt modules may still need distribution-native integration when native
applications cannot discover modules from the Nix profile.
