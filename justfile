default:
  @just --list

@olox *FILE:
    ./olox/_build/default/bin/main.exe {{FILE}}

@compile *FILE:
    ./olox/_build/default/bin/compiler.exe {{FILE}}

@build:
    ( cd olox ; dune build )

@dev:
    ( cd olox ; dune build --watch )

@test: build
    uv run tools/run_tests.py

@vm-build:
    ( cd vm ; zig build )

@vm: vm-build
    ./vm/zig-out/bin/vm "out.byte"

@vm-run:
    ./vm/zig-out/bin/vm "out.byte"

@ozlox *FILE:
    just compile {{FILE}} && ./vm/zig-out/bin/vm "out.byte"

@old-vm:
    ( cd runtime ; make clean all > /dev/null ; ./build/main )