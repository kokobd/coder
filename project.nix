{ haskell-nix, pkgs, shellBuildInputs }:

haskell-nix.project' {
  src = ./.;

  compiler-nix-name = "ghc947";
  shell = {
    tools = {
      cabal = { };
      ghcid = { };
      ormolu = { };
      cabal-fmt = { };
    };
    buildInputs = shellBuildInputs;
    exactDeps = false;
  };
}
