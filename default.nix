let pkgs = import <nixpkgs> { };
in pkgs.haskellPackages.callPackage ./large-hashable.nix { }
