{
  description = "Portable personal environment and live dotfile links";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/b86751bc4085f48661017fa226dee99fab6c651b";
  inputs.nixgl = {
    url = "github:nix-community/nixGL";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { nixpkgs, nixgl, ... }:
    let
      systems = [ "x86_64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      perSystem =
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ nixgl.overlay ];
            config.allowUnfree = true;
          };
          alacrittyWithNixGL =
            pkgs.runCommand "alacritty-with-nixgl"
              {
                nativeBuildInputs = [ pkgs.makeWrapper ];
              }
              ''
                mkdir -p "$out/bin" "$out/share"
                makeWrapper \
                  ${pkgs.nixgl.nixGLIntel}/bin/nixGLIntel \
                  "$out/bin/alacritty" \
                  --add-flags ${pkgs.alacritty}/bin/alacritty
                cp -rs ${pkgs.alacritty}/share/* "$out/share/"
              '';
          dotfiles = pkgs.buildEnv {
            name = "dotfiles";
            paths = with pkgs; [
              fish
              brave
              starship
              zellij
              neovim
              helix
              nixd
              nixfmt
              shellcheck
              shfmt
              statix
              git
              gcc
              gnumake
              curl
              rclone
              rustc
              cargo
              tree-sitter
              lazygit
              superfile
              qmk
              fzf
              ripgrep
              ripgrep-all
              fd
              eza
              zoxide
              difftastic
              tealdeer
              alacrittyWithNixGL
              fcitx5
              fcitx5-gtk
              qt6Packages.fcitx5-chinese-addons
              qt6Packages.fcitx5-configtool
              xclip
              btop
              htop
              fastfetch
              trash-cli
              util-linux
              man-db
              man-pages
              fontconfig
              nerd-fonts.hack
              noto-fonts
              noto-fonts-cjk-sans
              noto-fonts-color-emoji
            ];
            pathsToLink = [
              "/bin"
              "/share"
            ];
          };
        in
        {
          inherit dotfiles pkgs;
        };
    in
    {
      packages = forAllSystems (system: {
        default = (perSystem system).dotfiles;
      });

      checks = forAllSystems (
        system:
        let
          inherit (perSystem system) dotfiles pkgs;
          manifest =
            pkgs.runCommand "dotfiles-manifest-check"
              {
                nativeBuildInputs = [
                  pkgs.fastfetch
                  pkgs.lazygit
                  pkgs.shellcheck
                  pkgs.shfmt
                  pkgs.starship
                  pkgs.systemd
                ];
              }
              ''
                for script in ${./home/.local/bin}/*; do
                  shellcheck "$script"
                  shfmt -d "$script"
                done
                systemd-tmpfiles --user --dry-run --create ${./dotfiles.conf}
                fastfetch --config ${./home/.config/fastfetch/config.jsonc} --pipe >/dev/null
                lazygit --use-config-file ${./home/.config/lazygit/config.yml} --version >/dev/null
                STARSHIP_CONFIG=${./home/.config/starship/starship.toml} starship explain >/dev/null
                touch "$out"
              '';
        in
        {
          inherit manifest;
          profile = dotfiles;
        }
      );

      formatter = forAllSystems (system: (perSystem system).pkgs.nixpkgs-fmt);
    };
}
