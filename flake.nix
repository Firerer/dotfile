{
  description = "A flake for my profile";
  outputs = { self, nixpkgs }:
    let
      # https://nixos.org/manual/nixpkgs/stable/
      p = nixpkgs.legacyPackages.x86_64-linux;
    in
    {
      formatter.x86_64-linux = nixpkgs.legacyPackages.x86_64-linux.nixpkgs-fmt;
      packages.x86_64-linux = {
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/buildenv/default.nix
        default = p.buildEnv {
          name = "my-profile_";
          LOCALE_ARCHIVE = "${p.glibcLocales}/lib/locale/locale-archive";
          paths = [
            p.git
            p.emacs
            p.neovim
            p.zellij

            # desktop envs
            p.xmonad
            p.xmonad-contrib
            p.xmonad-extras
            p.xmonad-utils
            p.rofi
            p.rofi-pass
            #p.fcitx5
            p.fcitx5-chinese-addons
            p.fcitx5-configtool
            p.fcitx5-gtk

            # softwares
            p.firefox
            p.chromium
            p.activitywatch
            p.ventoy
            p.logseq
            #p.zoom-us

            # nix
            p.nil
            p.nixpkgs-fmt
          ];
        };
      };
    };
  # https://stackoverflow.com/questions/59323722/how-to-specify-multiple-packages-derivation-for-installation-by-nix-env
  # nix-env -if with import <nixpkgs>{}; [ htop moreutils ]
}
