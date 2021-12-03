export PATH="$coreutils/bin"
source $stdenv/setup

mkdir -p $out/bin
cp $src $out/bin/swayBGChanger
chmod +x $out/bin/swayBGChanger
wrapProgram $out/bin/swayBGChanger --prefix PATH : ${swaybg}/bin ;
