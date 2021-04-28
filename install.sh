#!/usr/bin/env bash

set -f;

cabalArgs="-O2 --enable-executable-stripping";

copy-bin() {
  local bin=$(cabal list-bin $1 $cabalArgs);
  cp $bin ./bin/;
}

rm -f ./bin/*;
mkdir -p ./bin/;

echo "Building...";
cabal build $cabalArgs && \
  echo "Copying binaries..." && \
  copy-bin statusbar && \
  copy-bin window-manager;

