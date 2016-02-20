{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base16-bytestring, byteable, bytes
      , bytestring, cereal, containers, cryptohash, deepseq, HTF
      , QuickCheck, safecopy, stdenv, tasty-quickcheck, text
      , transformers
      }:
      mkDerivation {
        pname = "large-hashable";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base base16-bytestring bytes bytestring containers text
          transformers
        ];
        executableHaskellDepends = [
          base byteable bytes bytestring cereal cryptohash deepseq safecopy
          text transformers
        ];
        testHaskellDepends = [
          base bytes bytestring HTF QuickCheck tasty-quickcheck text
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
