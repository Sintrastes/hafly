{ mkDerivation, base, hashmap, haskeline, lib, megaparsec, multimap
, parser-combinators, text
}:
mkDerivation {
  pname = "hafly";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base hashmap megaparsec multimap parser-combinators text
  ];
  executableHaskellDepends = [
    base hashmap haskeline megaparsec multimap parser-combinators text
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}