{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.stdenv.mkDerivation {
  name = "xmonad-phenax-config";
  buildInputs = with nixpkgs; [
    pkg-config
    stack
    xorg.libXinerama
    xorg.libX11
    xorg.libXrandr
    xorg.libXft
    xorg.libXpm
  ];
}
