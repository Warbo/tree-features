{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, atto-lisp, attoparsec, base, bytestring
      , directory, MissingH, parsec, QuickCheck, stdenv, stringable
      , tasty, tasty-quickcheck, xml
      }:
      mkDerivation {
        pname = "TreeFeatures";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          atto-lisp attoparsec base bytestring MissingH parsec stringable xml
        ];
        testHaskellDepends = [
          atto-lisp attoparsec base bytestring directory MissingH parsec
          QuickCheck stringable tasty tasty-quickcheck xml
        ];
        homepage = "http://chriswarbo.net/essays/repos/tree-features.html";
        description = "Feature extraction for tree structured data";
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
