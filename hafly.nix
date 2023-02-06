{ mkDerivation, base, hashmap, haskeline, lib, megaparsec, multimap
, parser-combinators, text, tasty, tasty-hunit
}:
mkDerivation {
  pname = "hafly";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = [
    base hashmap megaparsec multimap parser-combinators text
  ];
  # executableHaskellDepends = [
  #   base hashmap haskeline megaparsec multimap parser-combinators text tasty tasty-hunit
  # ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}