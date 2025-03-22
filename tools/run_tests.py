from dataclasses import dataclass
from pathlib import Path
import re
import subprocess

BASE_DIR = Path(__file__).parents[1]
INTERPRETER_PATH = str(BASE_DIR / "olox/_build/default/bin/olox.exe")
OUTPUT_PATTERN = re.compile(r"// expect: (.*)")


class bcolors:
    HEADER = "\033[95m"
    OKBLUE = "\033[94m"
    OKCYAN = "\033[96m"
    OKGREEN = "\033[92m"
    WARNING = "\033[93m"
    FAIL = "\033[91m"
    ENDC = "\033[0m"
    BOLD = "\033[1m"
    UNDERLINE = "\033[4m"


@dataclass
class Output:
    text: str
    line: int


def run_test(test_path: Path, print_diff: bool = False) -> bool:
    expected_outputs = []
    with open(test_path) as f:
        for line, text in enumerate(f.readlines(), start=1):
            match = OUTPUT_PATTERN.search(text.strip("\n"))
            if match:
                expected_outputs.append(Output(match.group(1), line))

    result = subprocess.run([INTERPRETER_PATH, str(test_path)], capture_output=True, text=True)
    actual_outputs = []
    for line, text in enumerate(result.stdout.splitlines(), start=1):
        actual_outputs.append(Output(text, line))

    expected = [output.text for output in expected_outputs]
    actual = [output.text for output in actual_outputs]
    if expected == actual:
        return True
    else:
        if print_diff:
            for expected, actual in zip(expected_outputs, actual_outputs):
                if expected != actual:
                    print(f"Line {expected.line}: Expected '{expected.text}', got '{actual.text}'")

            # Handle different lengths
            if len(expected_outputs) > len(actual_outputs):
                print(f"Missing {len(expected_outputs) - len(actual_outputs)} expected outputs")
            elif len(actual_outputs) > len(expected_outputs):
                print(f"Got {len(actual_outputs) - len(expected_outputs)} extra outputs")
        return False


def main():
    ignored_groups = {
        "benchmark",
        "class",
        "constructor",
        "field",
        "inheritance",
        "limit",
        "method",
        "scanning",
        "super",
        "this",
    }
    ignored_names = {
        "mutual_recursion",
        "to_this",
        "prefix_operator",
    }
    all_tests = list(BASE_DIR.rglob("tests/**/*.lox"))
    n_tests = len(all_tests)
    n_ignored = 0
    n_passed = 0
    n_failed = 0
    for path in all_tests:
        group = path.parents[0].stem
        name = path.stem
        if group in ignored_groups or name in ignored_names:
            n_ignored += 1
            continue
        success = run_test(path)
        if success:
            n_passed += 1
            print(f"{bcolors.OKGREEN}[PASS]{bcolors.ENDC}", end=" ")
            print(f"{group}/{name}{path.suffix}")
        else:
            n_failed += 1
            print(f"{bcolors.FAIL}[FAIL]{bcolors.ENDC}", end=" ")
            print(f"{group}/{name}{path.suffix}")
            # break
    print(f"Total tests: {n_tests}")
    print(f"- Passed:  {n_passed}")
    print(f"- Failed:  {n_failed}")
    print(f"- Ignored: {n_ignored}")


if __name__ == "__main__":
    main()
