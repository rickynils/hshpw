{ mkDerivation, base, base64-bytestring, bytestring, cmdargs
, containers, cryptohash, directory, haskeline, lib, parsec
}:
mkDerivation {
  pname = "hshpw";
  version = "0.2";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base base64-bytestring bytestring cmdargs containers cryptohash
    directory haskeline parsec
  ];
  description = "hshpw";
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
