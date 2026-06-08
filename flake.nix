{
  description = "A flake for my profile";
  nixConfig = {
    # pi agent https://github.com/lukasl-dev/pi.nix
    extra-substituters = [ "https://pi.cachix.org" ];
    extra-trusted-public-keys = [
      "pi.cachix.org-1:lGeoGJaZ5ZDabuRzkcD5EBTNnDM4HJ1vqeOxlWk1Flk="
    ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/b86751bc4085f48661017fa226dee99fab6c651b";
    pi.url = "github:lukasl-dev/pi.nix";
  };
  outputs = { self, nixpkgs, pi }:
    let
      # https://nixos.org/manual/nixpkgs/stable/
      system = "x86_64-linux";
      p = nixpkgs.legacyPackages.${system};
      piPackage = pi.packages.${system}.default;
    in
    {
      formatter.${system} = p.nixpkgs-fmt;
      packages.${system} = {
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/buildenv/default.nix
        default = p.buildEnv {
          name = "my-profile_";
          paths = [
            # lazyvim
            p.neovim # Make sure this is latest version for LazyVim
            p.git # Required for LazyVim plugins
            p.gcc # For treesitter
            p.gnumake # Build system
            p.curl # For nvim-cmp
            p.lazygit # For Git integration
            p.fzf # For fuzzy finding
            p.ripgrep # For live grep
            p.fd # For find files
            p.tree-sitter
            # p.nerdfonts # For icons support (too big)
            # p.jetbrains-mono
            p.xclip # For clipboard support

            # terminal
            p.zellij
            p.starship
            p.fish

            # apps
            # WARN: use flatpak instead
            # p.logseq

            # tools
            p.stow
            piPackage
          ];
        };
      };
    };
}
