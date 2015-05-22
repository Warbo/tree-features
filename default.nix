{ mkDerivation, base, directory, MissingH, parsec, QuickCheck
, stdenv, tasty, tasty-quickcheck, xml
}:
mkDerivation {
  pname = "TreeFeatures";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base MissingH parsec xml ];
  testDepends = [
    base directory MissingH parsec QuickCheck tasty tasty-quickcheck
    xml
  ];
  homepage = "http://chriswarbo.net/essays/repos/tree-features.html";
  description = "Feature extraction for tree structured data";
  license = stdenv.lib.licenses.gpl3;
}
