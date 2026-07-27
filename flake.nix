{
  description = "A flake for my profile";
  nixConfig = {
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/b86751bc4085f48661017fa226dee99fab6c651b";
  };
  outputs = { self, nixpkgs }:
    let
      # https://nixos.org/manual/nixpkgs/stable/
      system = "x86_64-linux";
      p = nixpkgs.legacyPackages.${system};
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
          ];
        };
      };
    };
}
