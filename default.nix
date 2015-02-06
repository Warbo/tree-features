with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "generatinggoals";
  src  = ./.;
  buildInputs = [
    coq_mtac
    pkgs.emacs
    pkgs.emacs24Packages.proofgeneral
    pkgs.haskellPackages.ghc
    pkgs.haskellPackages.xml
    pkgs.haskellPackages.QuickCheck
    pkgs.haskellPackages.MissingH
  ];
}
