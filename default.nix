{ mkDerivation, base, directory, lib, process, random }:
mkDerivation {
  pname = "swaybgchanger";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory process random ];
  executableHaskellDepends = [ base directory process random ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
