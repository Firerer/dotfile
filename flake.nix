{
  description = "Portable personal environment and live dotfile links";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/b86751bc4085f48661017fa226dee99fab6c651b";

  outputs = { nixpkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
      perSystem = system:
        let
          pkgs = import nixpkgs { inherit system; };
          profile = pkgs.buildEnv {
            name = "dotfiles-profile";
            paths = import ./nix/packages.nix { inherit pkgs; };
            pathsToLink = [
              "/bin"
              "/share"
            ];
            ignoreCollisions = true;
          };
          apply = import ./nix/apply.nix { inherit pkgs; };
        in
        { inherit apply pkgs profile; };
    in
    {
      packages = forAllSystems (system: {
        default = (perSystem system).profile;
      });

      apps = forAllSystems (system: {
        apply = {
          type = "app";
          program = "${(perSystem system).apply}/bin/dotfiles-apply";
          meta.description = "Safely build and activate the personal environment";
        };
      });

      checks = forAllSystems (system:
        let
          inherit (perSystem system) apply pkgs profile;
          manifest = pkgs.runCommand "dotfiles-manifest-check"
            {
              nativeBuildInputs = [ pkgs.shellcheck pkgs.systemd ];
            } ''
            shellcheck ${./home/.local/bin/qmk-flash}
            systemd-tmpfiles --user --dry-run --create ${./nix/dotfiles.conf}
            touch "$out"
          '';
        in
        {
          inherit apply manifest profile;
        });

      formatter = forAllSystems (system: (perSystem system).pkgs.nixpkgs-fmt);
    };
}
