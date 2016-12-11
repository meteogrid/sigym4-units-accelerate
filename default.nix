{ mkDerivation, accelerate, accelerate-llvm-native, base
, dimensional, exact-pi, hspec, newtype, QuickCheck, sigym4-units
, stdenv
}:
mkDerivation {
  pname = "sigym4-units-accelerate";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    accelerate base dimensional exact-pi newtype sigym4-units
  ];
  testHaskellDepends = [
    accelerate accelerate-llvm-native base hspec QuickCheck
    sigym4-units
  ];
  homepage = "https:&&github.com/meteogrid/sigym4-units-accelerate";
  description = "Sigym4.Units lifted to Accelerate expressions";
  license = stdenv.lib.licenses.bsd3;
}
