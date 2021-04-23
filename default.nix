{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.stdenv.mkDerivation {
  name = "ph-xmonad-config";
  buildInputs = with nixpkgs;
    [
      pkg-config
      cabal-install
    ];
}
