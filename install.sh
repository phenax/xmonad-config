#!/usr/bin/env bash

copy-bin() {
  local bin=$(cabal list-bin $1);
  cp $bin ./bin/;
}

rm ./bin/*;

echo "Building...";
cabal build && \
  echo "Copying binaries..." && \
  copy-bin statusbar && \
  copy-bin window-manager;

