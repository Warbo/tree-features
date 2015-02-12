with import <nixpkgs> {};

coq_mtac = pkgs.stdenv.lib.overrideDerivation pkgs.coq (oldAttrs : {
  name = "coq-mtac";
  src  = pkgs.fetchgit {
    url    = git://gitorious.org/mc-aixi-ctw/mc-aixi-ctw.git;
    rev    = "2651fd3";
    sha256 = "1949z7pjb51w89954narwcd1ykb9wxi7prldic1a1slxrr5b6lq7";
  };
});

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
