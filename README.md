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
# Python
time lox tests/fuzz.lox

real	1m1.743s
user	1m0.729s
sys	    0m0.920s
```

```sh
# Zig
zig build-exe main.zig -O ReleaseSafe -fstrip -fsingle-threaded
time ./main ../tests/fuzz.lox

real	0m0.875s
user	0m0.442s
sys	    0m0.430s
```