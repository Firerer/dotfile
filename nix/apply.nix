{ pkgs }:

pkgs.writeShellApplication {
  name = "dotfiles-apply";
  runtimeInputs = with pkgs; [
    coreutils
    systemd
  ];
  text = builtins.readFile ./apply.sh;
}
