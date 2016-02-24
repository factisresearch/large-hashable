{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16-bytestring, byteable, bytes
      , bytestring, cereal, containers, cryptohash, deepseq, hashable
      , HTF, QuickCheck, safecopy, stdenv, text, transformers
      , unordered-containers
      }:
      mkDerivation {
        pname = "large-hashable";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base16-bytestring bytes bytestring containers text
          transformers unordered-containers
        ];
        executableHaskellDepends = [
          base byteable bytes bytestring cereal cryptohash deepseq safecopy
          text transformers
        ];
        testHaskellDepends = [
          base bytes bytestring containers hashable HTF QuickCheck text
          unordered-containers
        ];
        homepage = "http://github.com/githubuser/large-hashable#readme";
        description = "Initial project template from stack";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
