default:
  @just --list

@run *FILE:
    ./olox/_build/default/bin/olox.exe {{FILE}}

@build:
    ( cd olox ; dune build )

@dev:
    ( cd olox ; dune build --watch )