{ mkDerivation, accelerate, accelerate-llvm-native, base
, dimensional, hspec, newtype, QuickCheck, should-not-typecheck
, sigym4-units, stdenv, template-haskell, numtype-dk
}:
mkDerivation {
  pname = "sigym4-units-accelerate";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    accelerate base dimensional newtype sigym4-units template-haskell numtype-dk
  ];
  testHaskellDepends = [
    accelerate accelerate-llvm-native base hspec newtype QuickCheck
    should-not-typecheck sigym4-units
  ];
  homepage = "https:&&github.com/meteogrid/sigym4-units-accelerate";
  description = "Sigym4.Units lifted to Accelerate expressions";
  license = stdenv.lib.licenses.bsd3;
}
