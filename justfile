default:
  @just --list

@run *FILE:
    ./olox/_build/default/bin/main.exe {{FILE}}

@compile *FILE:
    ./olox/_build/default/bin/compiler.exe {{FILE}}

@build:
    ( cd olox ; dune build )

@dev:
    ( cd olox ; dune build --watch )

@test: build
    uv run tools/run_tests.py

@vm *FILE:
    ( cd vm ; zig build ) ; ./vm/zig-out/bin/vm {{FILE}}

@vm-run *FILE:
    just compile {{FILE}} && ./vm/zig-out/bin/vm "out.byte"

@old-vm:
    ( cd runtime ; make clean all > /dev/null ; ./build/main )