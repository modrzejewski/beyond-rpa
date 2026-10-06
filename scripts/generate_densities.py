#!/usr/bin/env python3
"""Generate the atomic guess densities of the basis sets in this folder.

fetch_basis_to_library.py copies this script to
guess/electron-densities/<source>/, and it is run from there.

The script runs beyond-rpa on atomic_guess.inp in each basis set folder,
from that folder, because the basis set path in the input is relative to it.
The densities <element>.txt are written next to the input, and the program
output goes to atomic_guess.log. Existing densities are skipped, so the
script can be restarted after an interruption.
"""
import argparse
import os
import subprocess
from pathlib import Path

HERE = Path(__file__).resolve().parent
BIN_RUN = HERE.parents[2] / "bin" / "run"


def number_of_elements(inp: Path) -> int:
    """Return the number of atoms in the xyz block of an input."""
    lines = inp.read_text().splitlines()
    return int(lines[lines.index("xyz") + 1])


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "-nt",
        "--nthreads",
        type=int,
        default=len(os.sched_getaffinity(0)),
        help="number of threads; default: all available cores",
    )
    parser.add_argument(
        "folders",
        nargs="*",
        help="basis set folders to run, e.g. cc-pvtz; default: all",
    )
    args = parser.parse_args()

    if args.folders:
        folders = [HERE / name for name in args.folders]
    else:
        folders = sorted(inp.parent for inp in HERE.glob("*/atomic_guess.inp"))
    for folder in folders:
        print(f"Running {folder.name}... ", end="", flush=True)
        with open(folder / "atomic_guess.log", "w") as log:
            subprocess.run(
                [str(BIN_RUN), "-nt", str(args.nthreads), "atomic_guess.inp"],
                cwd=folder,
                stdout=log,
                stderr=subprocess.STDOUT,
            )
        requested = number_of_elements(folder / "atomic_guess.inp")
        written = len(list(folder.glob("*.txt")))
        status = "done" if written == requested else "INCOMPLETE, see atomic_guess.log"
        print(f"{written} of {requested} densities, {status}")


if __name__ == "__main__":
    main()
