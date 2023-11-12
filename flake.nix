{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskell-nix.overlay
          (final: prev: {
            # This overlay adds our project to pkgs
            devcontainersProject =
              import ./project.nix {
                haskell-nix = final.haskell-nix;
                inherit pkgs;
                shellBuildInputs =
                  with pkgs;
                  [
                    nixpkgs-fmt
                    gnumake
                    direnv
                    nix-direnv
                    jq
                    awscli2
                    terraform
                  ];
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskell-nix) config; };
        flake = pkgs.devcontainersProject.flake {
          crossPlatforms = p: [ p.musl64 ];
        };
      in
      flake
    );
}
