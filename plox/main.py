import sys
import plox
from plox.scanner import Scanner


def run(src: str):
    scanner = Scanner(src)
    tokens = scanner.scan_tokens()
    for token in tokens:
        print(token)


def run_prompt():
    while True:
        try:
            line = input("> ")
            run(line)
            plox.had_error = False
        except EOFError:
            print()
            break


def run_file(src_path: str):
    with open(src_path, "r") as f:
        src = f.read()
    run(src)
    if plox.had_error:
        exit(1)


def main():
    args = sys.argv[1:]  # ignore first arg (script path)
    n_args = len(args)
    if n_args > 1:
        print("Usage: plox [script]")
        exit(1)
    elif n_args == 1:
        run_file(args[0])
    else:
        run_prompt()


if __name__ == "__main__":
    main()
