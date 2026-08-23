{ pkgs }:

pkgs.writeShellApplication {
  name = "dotfiles-apply";
  runtimeInputs = with pkgs; [
    coreutils
    findutils
    gnugrep
    systemd
  ];
  text = builtins.readFile ./apply.sh;
}
