directory:

haskellPackagesNew: haskellPackagesOld:
  let
    pkgs = import <nixpkgs> {};

    isNixFile = path: pkgs.lib.strings.hasSuffix ".nix" path;

    filterDirectory = dir: builtins.filterSource (path: type: type == "regular" && isNixFile (baseNameOf path) ) dir;

    haskellPaths = builtins.attrNames (builtins.readDir (filterDirectory directory));

    toKeyVal = file: {
      name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

      value = haskellPackagesNew.callPackage (directory + "/${file}") { };
    };

  in
    builtins.listToAttrs (map toKeyVal haskellPaths)
