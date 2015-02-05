with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "generatinggoals";
  src  = ./.;
  buildInputs = [
    coq_mtac
    pkgs.emacs
    pkgs.emacs24Packages.proofgeneral
  ];
}
