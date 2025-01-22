# olox - Lox implementation in OCaml

## Setup

```sh
opam switch create default ocaml-base-compiler.5.2.0
opam install -y utop ounit2 ocaml-lsp-server ocamlformat ocamlformat-rpc ppx_deriving
```

## Run

```sh
dune build --watch # Watch for changes and build
dune exec bin/olox.exe # Run
dune test # Run tests
```