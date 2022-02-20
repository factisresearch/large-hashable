{ mkDerivation, aeson, base, base16-bytestring, byteable, bytes
, bytestring, cereal, containers, criterion, cryptohash, cryptonite
, deepseq, hashable, HTF, inspection-testing, lib, memory
, QuickCheck, safecopy, scientific, strict, template-haskell, text
, time, transformers, unordered-containers, utf8-light, vector
, void
}:
mkDerivation {
  pname = "large-hashable";
  version = "0.1.0.4";
  src = ./.;
  libraryHaskellDepends = [
    aeson base base16-bytestring bytes bytestring containers cryptonite
    memory scientific strict template-haskell text time transformers
    unordered-containers utf8-light vector void
  ];
  testHaskellDepends = [
    aeson base bytes bytestring containers hashable HTF
    inspection-testing QuickCheck scientific strict text time
    unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    base base16-bytestring byteable bytes bytestring cereal criterion
    cryptohash deepseq safecopy text transformers
  ];
  homepage = "https://github.com/factisresearch/large-hashable";
  description = "Efficiently hash (large) Haskell values";
  license = lib.licenses.bsd3;
}
