{ nixpkgs ? import <nixpkgs> { } }:
with nixpkgs;
nixpkgs.stdenv.mkDerivation {
  name = "xmonad-phenax-config";
  buildInputs = [
    pkg-config
    haskellPackages.implicit-hie
    haskellPackages.haskell-language-server
    cabal-install

    # Libs
    libclang
    alsaLib
    xorg.libXext
    xorg.libXScrnSaver
    xorg.libXinerama
    xorg.libX11
    xorg.libXrandr
    xorg.libXft
    xorg.libXpm
  ];
  nativeBuildInputs = [ clang ];

  LIBCLANG_PATH = "${libclang.lib}/lib";
}
