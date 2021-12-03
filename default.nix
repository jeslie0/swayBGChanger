let build = { stdenv, coreutils, swaybg, makeWrapper }:
      stdenv.mkDerivation {
        name = "swayBGChanger";
        builder = ./builder.sh;
        src = ./swayBGChanger.sh;
        nativeBuildInputs = [ makeWrapper ];
        inherit swaybg;
      };
in
with (import <nixpkgs> {});
build { inherit stdenv coreutils swaybg makeWrapper; }
