with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "generatinggoals";
  src  = ./.;
  buildInputs = [
    coq_mtac
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.xml
    pkgs.haskellPackages.QuickCheck
    pkgs.haskellPackages.MissingH
  ];
}
