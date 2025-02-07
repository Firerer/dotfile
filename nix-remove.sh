#!/bin/bash

# Function to check if script is run as root
check_root() {
    if [ "$EUID" -ne 0 ]; then
        echo "Please run this script as root or with sudo"
        exit 1
    fi
}

# Function to stop and disable Nix daemon
remove_nix_daemon() {
    echo "Stopping and disabling Nix daemon..."

    systemctl stop nix-daemon.service
    systemctl disable nix-daemon.socket nix-daemon.service
    rm /etc/systemd/system/nix-*

    systemctl daemon-reload
    echo "Nix daemon removed successfully"
}

# Function to remove Nix files
remove_nix_files() {
    echo "Removing Nix files..."
    rm -rf /etc/nix /etc/profile.d/nix.sh /etc/tmpfiles.d/nix-daemon.conf /nix ~root/.nix-channels ~root/.nix-defexpr ~root/.nix-profile
    rm /etc/*.backup-before-nix
    echo "Nix files removed successfully"
}

# Function to remove build users and group
remove_nix_users() {
    echo "Removing Nix build users and group..."
    for i in $(seq 1 32); do
        userdel nixbld$i 2>/dev/null || true
    done
    groupdel nixbld 2>/dev/null || true
    echo "Nix users and group removed successfully"
}

# Function to clean up Nix references in shell config files
warn_shell_configs() {
    echo -e "\033[31mWarn\033[0m: Check Nix references in shell configuration files:
        /etc/bash.bashrc
        /etc/bashrc
        /etc/profile
        /etc/zsh/zshrc
        /etc/zshrc
    "
}

main() {
    echo "Starting Nix uninstallation..."
    check_root
    remove_nix_daemon
    remove_nix_files
    remove_nix_users
    warn_shell_configs
    echo "Nix uninstallation completed successfully"
    echo ""
    echo "Install: sh <(curl -L https://nixos.org/nix/install) --daemon"
    echo "Config: experimental-features = nix-command flakes"
}

# Run the script
main
