let
  sources = import ./nix/sources.nix;
  nixpkgs = import sources.nixpkgs {
    overlays = [ ];
    config = { };
  };
  inherit (nixpkgs) pkgs;
  env = (import ./default.nix { inherit sources nixpkgs; }).env;
in
env.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ [
    pkgs.cabal-install
    pkgs.stack
    pkgs.ghcid
  ];
})
