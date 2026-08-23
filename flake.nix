{
  description = "Portable personal environment and live dotfile links";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/b86751bc4085f48661017fa226dee99fab6c651b";
  inputs.nixgl = {
    url = "github:nix-community/nixGL";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, nixgl, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      perSystem = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ nixgl.overlay ];
          };
          alacrittyWithNixGL = pkgs.runCommand "alacritty-with-nixgl"
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
              starship
              zellij
              neovim
              helix
              git
              gcc
              gnumake
              curl
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
            ignoreCollisions = true;
          };
        in
        { inherit dotfiles pkgs; };
    in
    {
      packages = forAllSystems (system: {
        default = (perSystem system).dotfiles;
      });

      checks = forAllSystems (system:
        let
          inherit (perSystem system) dotfiles pkgs;
          manifest = pkgs.runCommand "dotfiles-manifest-check"
            {
              nativeBuildInputs = [ pkgs.shellcheck pkgs.systemd ];
            } ''
            shellcheck ${./home/.local/bin/qmk-flash}
            systemd-tmpfiles --user --dry-run --create ${./dotfiles.conf}
            touch "$out"
          '';
        in
        {
          inherit manifest;
          profile = dotfiles;
        });

      formatter = forAllSystems (system: (perSystem system).pkgs.nixpkgs-fmt);
    };
}
