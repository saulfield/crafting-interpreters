# Crafting Interpreters

## plox (Python)

From the home directory:
```sh
# Setup venv and install the package
. ./setup.sh

# Run the interpreter in REPL mode
lox

# Execute a script
lox [script]
```

# Benchmarking

## Scanner (100MB file of random characters)

```sh
hyperfine \ 
  --runs 5 \ 
  -n Python "lox tests/fuzz.lox" \ 
  -n Zig    "./zlox/main tests/fuzz.lox" \ 
  -n OCaml  "./olox/_build/default/bin/olox.exe tests/fuzz.lox"
```

```sh
Benchmark 1: Python
  Time (mean ± σ):     62.312 s ±  2.315 s    [User: 61.307 s, System: 0.939 s]
  Range (min … max):   60.169 s … 65.255 s    5 runs

Benchmark 2: Zig
  Time (mean ± σ):     860.5 ms ±   7.1 ms    [User: 439.5 ms, System: 417.0 ms]
  Range (min … max):   855.2 ms … 872.0 ms    5 runs

Benchmark 3: OCaml (compiled, immutable)
  Time (mean ± σ):      3.322 s ±  0.020 s    [User: 2.679 s, System: 0.636 s]
  Range (min … max):    3.292 s …  3.343 s    5 runs

Benchmark 4: OCaml (compiled, mutable)
  Time (mean ± σ):      2.796 s ±  0.014 s    [User: 2.234 s, System: 0.556 s]
  Range (min … max):    2.773 s …  2.810 s    5 runs
```