#!/usr/bin/env python3
"""Generate the atomic guess densities of the basis sets.

Usage: python scripts/generate_densities.py [cc-repo | bse | all]

The script runs beyond-rpa on atomic_guess.inp in every basis set folder
guess/electron-densities/<source>/<name>/, from that folder, because the
basis set path in the input is relative to it. The densities <element>.txt
are written next to the input, and the program output goes to
atomic_guess.log. Existing densities are skipped, so the script can be
restarted after an interruption.
"""
import argparse
import os
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BIN_RUN = ROOT / "bin" / "run"
GUESS_ROOT = ROOT / "guess" / "electron-densities"
#
# The same sources as in fetch_basis_to_library.py
#
SOURCES = ("bse", "cc-repo")


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
        "source",
        nargs="?",
        choices=SOURCES + ("all",),
        default="all",
        help="source of the basis set parameters; default: all",
    )
    parser.add_argument(
        "-nt",
        "--nthreads",
        type=int,
        default=len(os.sched_getaffinity(0)),
        help="number of threads; default: all available cores",
    )
    args = parser.parse_args()

    sources = SOURCES if args.source == "all" else (args.source,)
    for source in sources:
        folders = sorted(inp.parent for inp in (GUESS_ROOT / source).glob("*/atomic_guess.inp"))
        if not folders:
            print(f"No atomic_guess.inp for {source}")
        for folder in folders:
            print(f"Running {source}/{folder.name}... ", end="", flush=True)
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
