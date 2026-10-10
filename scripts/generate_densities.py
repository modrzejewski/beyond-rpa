#!/usr/bin/env python3
"""Generate the atomic guess densities of the basis sets.

Usage: python scripts/generate_densities.py [selection ...]

A selection is a source (bse, cc-repo), a basis set <source>/<name>, or
all, the default:

  python scripts/generate_densities.py                  all sources
  python scripts/generate_densities.py bse              every set of bse
  python scripts/generate_densities.py cc-repo bse      both sources
  python scripts/generate_densities.py bse/cc-pvtz-pp   one set
  python scripts/generate_densities.py bse/cc-pvtz-pp bse/aug-cc-pvtz-pp

The script runs beyond-rpa on atomic_guess.inp in each selected basis set
folder guess/electron-densities/<source>/<name>/, from that folder, because
the basis set path in the input is relative to it. The densities
<element>.txt are written next to the input, and the program output is
appended to atomic_guess.log. Existing densities are skipped, so the script
can be restarted after an interruption.
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


def selected_folders(selection: str) -> list[Path]:
    """Return the basis set folders of all, a source, or a set <source>/<name>."""
    selection = selection.lower()
    if selection == "all":
        return [folder for source in SOURCES for folder in selected_folders(source)]
    source, _, name = selection.partition("/")
    if source not in SOURCES:
        raise ValueError(f"unknown source {source}; available: {', '.join(SOURCES)}")
    available = sorted(inp.parent for inp in (GUESS_ROOT / source).glob("*/atomic_guess.inp"))
    if not name:
        if not available:
            print(f"No atomic_guess.inp for {source}")
        return available
    folder = GUESS_ROOT / source / name
    if folder not in available:
        raise ValueError(
            f"no atomic_guess.inp for {selection}; available in {source}: "
            + ", ".join(f.name for f in available)
        )
    return [folder]


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "selection",
        nargs="*",
        default=["all"],
        help=f"all, a source ({', '.join(SOURCES)}) or a basis set <source>/<name>; default: all",
    )
    parser.add_argument(
        "-nt",
        "--nthreads",
        type=int,
        default=len(os.sched_getaffinity(0)),
        help="number of threads; default: all available cores",
    )
    args = parser.parse_args()

    folders = []
    for selection in args.selection:
        try:
            folders += selected_folders(selection)
        except ValueError as error:
            parser.error(str(error))
    #
    # A set selected twice, e.g., by bse and bse/cc-pvtz-pp, runs once
    #
    for folder in dict.fromkeys(folders):
        print(f"Running {folder.parent.name}/{folder.name}... ", end="", flush=True)
        with open(folder / "atomic_guess.log", "a") as log:
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
