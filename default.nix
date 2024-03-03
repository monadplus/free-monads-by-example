{ sources ? import ./nix/sources.nix
, nixpkgs ? import sources.nixpkgs {
    overlays = [ ];
    config = { };
  }
, compiler ? "ghc964"
, doBenchmark ? false
}:

let
  inherit (nixpkgs) pkgs;

  baseHaskellPackages =
    if compiler == "default" then
      pkgs.haskellPackages
    else
      pkgs.haskell.packages.${compiler};

  gitignoreSource = (import (sources."gitignore.nix") { }).gitignoreSource;

  readDirectory = import ./nix/readDirectory.nix;

  haskellPackages =
    let
      manualOverrides = haskellPackagesNew: haskellPackagesOld:
        {
          # Add manual overrides here
        };
    in
    baseHaskellPackages.override {
      overrides =
        pkgs.lib.composeExtensions (readDirectory ./nix/sources) manualOverrides;
    };

  doBench = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  doStatic = pkgs.haskell.lib.justStaticExecutables;
in
doStatic
  (doBench (haskellPackages.callCabal2nix "free-monads-by-example" (gitignoreSource ./.) { }))
