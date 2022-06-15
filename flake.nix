{
  description = "A wallpaper changer for Swaywm.";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = "swaybgchanger";
      in
        {
          packages.${packageName} =
            let unPatchedPkg = haskellPackages.callCabal2nix packageName self {};
            in pkgs.haskell.lib.overrideCabal unPatchedPkg (old: {
              buildDepends = [ pkgs.makeWrapper ];
              postInstall = ''
                            wrapProgram $out/bin/${packageName} \
                            --prefix PATH : ${pkgs.lib.getBin pkgs.swaybg}/bin
                            '';
            }
            );


          defaultPackage = self.packages.${system}.${packageName};

          devShell = haskellPackages.shellFor {
            packages = p: [ self.defaultPackage.${system} ];
            buildInputs = with haskellPackages;
              [ haskell-language-server
                cabal-install
                apply-refact
                hlint
                stylish-haskell
                hasktags
                hindent
              ];
            inputsFrom = builtins.attrValues self.packages.${system};
            withHoogle = true;
          };
          }
    );
}
