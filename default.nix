{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.stdenv.mkDerivation {
  name = "xmonad-phenax-config";
  buildInputs = with nixpkgs; [
    pkg-config
    haskellPackages.implicit-hie
    #stack
    cabal-install
    xorg.libXext
    xorg.libXScrnSaver
    xorg.libXinerama
    xorg.libX11
    xorg.libXrandr
    xorg.libXft
    xorg.libXpm
  ];
}
