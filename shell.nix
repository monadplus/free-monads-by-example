let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {
    overlays = [ ];
    config = { };
  };
  inherit (nixpkgs) pkgs;
  env = (import ./default.nix { inherit sources nixpkgs; }).env;
in env.overrideAttrs (oldAttrs: {
  buildInputs = with pkgs.haskellPackages;
    oldAttrs.buildInputs ++ [ cabal-install cabal2nix ghcid ];
})
