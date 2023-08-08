{ mkDerivation, base, Chart, colour, containers, data-default
, data-default-class, ghc-prim, hgeometry, hgeometry-combinatorial
, lens, lib, log-domain, mwc-random, vector, vector-th-unbox, vinyl
}:
mkDerivation {
  pname = "Chart-extras";
  version = "0.0.0.1";
  src = ./.;
  libraryHaskellDepends = [
    base Chart colour containers data-default data-default-class
    ghc-prim hgeometry hgeometry-combinatorial lens log-domain
    mwc-random vector vector-th-unbox vinyl
  ];
  homepage = "https://github.com/choener/Chart-extras";
  license = lib.licenses.bsd3;
}
