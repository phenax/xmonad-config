#!/usr/bin/env bash

copy-bin() {
  local bin=$(cabal list-bin $1);
  cp $bin ./bin/;
}


cabal build && \
  copy-bin bar;

