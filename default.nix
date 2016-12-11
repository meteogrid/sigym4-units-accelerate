{ mkDerivation, accelerate, base, dimensional, sigym4-units, stdenv
}:
mkDerivation {
  pname = "sigym4-units-accelerate";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    accelerate base dimensional sigym4-units
  ];
  homepage = "https:&&github.com/meteogrid/sigym4-units-accelerate";
  description = "Sigym4.Units lifted to Accelerate expressions";
  license = stdenv.lib.licenses.bsd3;
}
