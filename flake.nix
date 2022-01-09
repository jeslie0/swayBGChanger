{
  description = "A background changer for Swaywm";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }: {
    defaultPackage.x86_64-linux =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "swayBGchanger";
        src = ./swayBGChanger.sh;
        nativeBuildInputs = [ makeWrapper ];
        builder = ./builder.sh;
        inherit swaybg;
      };
  };
}
