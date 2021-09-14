#!/usr/bin/env bash

set -f;

cabalArgs="-O2 --enable-executable-stripping";

cbl() { nix-shell default.nix --run "cabal $*"; }

copy-bin() {
  local bin=$(cbl list-bin $1 $cabalArgs);
  cp $bin ./bin/;
}

rm -rf ./dist-newstyle;

mkdir -p ./bin;

echo "Building...";
cbl build $cabalArgs && \
  echo "Copying binaries..." && \
  copy-bin xmobar && \
  copy-bin xmonad-wm;

