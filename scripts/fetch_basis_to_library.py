"""Add a basis set from one source to the beyond-rpa library.

The script sets up everything a new basis set needs:

    basis-sets/<source>/<name>.txt
        the basis set, every element the source provides
    guess/electron-densities/<source>/<name>/
        the folder for its atomic guess densities
    guess/electron-densities/<source>/<name>/atomic_guess.inp
        the input that generates the guess densities
    guess/electron-densities/<source>/generate_densities.py
        runs atomic_guess.inp in every basis set folder of the source

<name> is the basis set name in lowercase. An input file selects the set with
"basis <source>/<name>", for example "basis cc-repo/cc-pwCVTZ". The basis set
file uses GAMESS-US format with optimized general contractions, the form of
all files in basis-sets/. Its header records the source, the download time
(UTC), the software versions, the elements and the contraction options.
"""
import argparse
import importlib.metadata
import re
import shutil
import textwrap
from datetime import datetime, timezone
from pathlib import Path

import basis_set_exchange as bse
from basis_set_exchange import lut, manip, writers

ROOT = Path(__file__).resolve().parent.parent
#
# Copied to guess/electron-densities/<source>/
#
GENERATOR = Path(__file__).resolve().parent / "generate_densities.py"
SOURCES = ("bse", "cc-repo")
ORIGINS = {
    "bse": "Basis Set Exchange, https://www.basissetexchange.org",
    "cc-repo": "ccRepo, https://grant-hill.group.shef.ac.uk/ccrepo",
}
CCREPO_CATALOGUE = (
    "https://raw.githubusercontent.com/Sheffield-Theoretical-Chemistry/"
    "ccrepo-raw/main/cc-basis-catalogue.txt"
)
#
# Download options of the Basis Set Exchange. Only optimize_general is applied.
#
CONTRACTION_OPTIONS = {
    "optimize general contractions": "on",
    "uncontract general": "off",
    "uncontract segmented": "off",
    "uncontract SPDF": "off",
    "make general": "off",
}
ANGULAR_MOMENTA = "spdfghi"
BEYOND_RPA_NAMES = {"ALUMINIUM": "ALUMINUM", "CAESIUM": "CESIUM"}
SPACING_ANGSTROM = 20.0
#
# Input that generates the guess densities, written to the guess folder.
# The basis set path is relative to the guess folder, so the input works
# in any copy of the repository. The program resolves the path against
# its working directory, so the input is run from the guess folder.
#
TEMPLATE = """\
! Atomic guess densities for {label}
! The densities are written to the folder of this file.
! Coordinates are ignored; every element gets an isolated-atom SCF.
! Run from this folder: the basis set path is relative to it.

jobtype atomic_guess

basis file {basis_path}

xyz
{natoms}
{atoms}
end
"""


def number(x: float) -> str:
    """Format a float with a decimal point, as the BSE writers require."""
    s = repr(x)
    if "." not in s:
        mantissa, _, exponent = s.partition("e")
        s = f"{mantissa}.0e{exponent}" if exponent else f"{s}.0"
    return s


def from_bse(basis_name: str) -> dict:
    """Return a BSE basis set."""
    return bse.get_basis(basis_name)


def from_ccrepo(basis_name: str) -> dict:
    """Return all ccRepo elements of a basis set in the BSE layout."""
    #
    # Importing ccrepo downloads its 24 MB catalogue,
    # so the import is done only when ccRepo is used.
    #
    from ccrepo.data import catalogue
    from ccrepo.fetch import fetch_basis

    key = re.compile(rf"^([A-Za-z]{{1,2}}):{re.escape(basis_name.lower())}:")
    symbols = [m.group(1) for m in map(key.match, catalogue.lower().splitlines()) if m]
    if not symbols:
        raise ValueError(f"{basis_name} not found in the ccRepo catalogue")
    fetched = fetch_basis(symbols, basis_name)
    elements = {}
    for symbol in symbols:
        shells = []
        for shell in fetched[symbol].shells:
            shells.append(
                {
                    "function_type": "gto_spherical",
                    "region": "",
                    "angular_momentum": [ANGULAR_MOMENTA.index(shell.l)],
                    "exponents": [number(float(x)) for x in shell.exps],
                    "coefficients": [
                        [number(float(c)) for c in column] for column in shell.coefs
                    ],
                }
            )
        z = str(lut.element_Z_from_sym(symbol))
        elements[z] = {"electron_shells": shells, "references": []}
    return {
        "name": basis_name,
        "description": f"{basis_name} from ccRepo",
        "revision_description": "",
        "version": "",
        "function_types": ["gto_spherical"],
        "elements": elements,
    }


def header(source: str, basis_name: str, basis: dict, downloaded: datetime) -> str:
    """Return the header of a library file; the writer prefixes each line with "!"."""
    if source == "bse":
        software = f"basis_set_exchange {bse.version()}"
    else:
        software = (
            f"ccrepo {importlib.metadata.version('ccrepo')}, "
            f"basis_set_exchange {bse.version()}"
        )
    symbols = [
        lut.element_sym_from_Z(z, normalize=True)
        for z in sorted(int(z) for z in basis["elements"])
    ]
    elements = textwrap.wrap(
        " ".join(symbols),
        width=78,
        initial_indent=f" {f'Elements ({len(symbols)}):':<16}",
        subsequent_indent=" " * 17,
    )
    lines = [
        "-" * 70,
        f" {'Basis set:':<16}{basis_name}",
        f" {'Source:':<16}{ORIGINS[source]}",
    ]
    if source == "cc-repo":
        lines += [f" {'Catalogue:':<16}{CCREPO_CATALOGUE}"]
    else:
        lines += [f" {'BSE version:':<16}{basis['version']}"]
    lines += [
        f" {'Downloaded:':<16}{downloaded:%Y-%m-%d %H:%M:%S} UTC",
        f" {'Software:':<16}{software}",
        *elements,
        f" {'Format:':<16}GAMESS-US, written by basis_set_exchange",
        f" {'Element names:':<16}"
        + ", ".join(f"{a} -> {b}" for a, b in BEYOND_RPA_NAMES.items()),
        " Contractions (basis_set_exchange download options):",
        *(f"   {option:<32}{state}" for option, state in CONTRACTION_OPTIONS.items()),
        f" {'Command:':<16}scripts/fetch_basis_to_library.py {source} {basis_name}",
        "-" * 70,
    ]
    return "\n".join(lines)


def library_text(source: str, basis_name: str) -> str:
    """Return the basis set as GAMESS-US text with beyond-rpa element names."""
    downloaded = datetime.now(timezone.utc)
    if source == "bse":
        basis = from_bse(basis_name)
    else:
        basis = from_ccrepo(basis_name)
    text = writers.write_formatted_basis_str(
        manip.optimize_general(basis),
        "gamess_us",
        header(source, basis_name, basis, downloaded),
    )
    comments, data = text.split("$DATA", 1)
    for bse_name, library_name in BEYOND_RPA_NAMES.items():
        data = data.replace(bse_name, library_name)
    return comments + "$DATA" + data


def elements_in_file(text: str) -> list[str]:
    """Return the symbols of the elements in a library file, in file order."""
    lines = text.splitlines()
    block = lines[lines.index("$DATA") + 1 : lines.index("$END")]
    bse_names = {v: k for k, v in BEYOND_RPA_NAMES.items()}
    symbols = []
    for line in block:
        name = line.strip()
        if re.fullmatch(r"[A-Z]{3,}", name):
            z = lut.element_Z_from_name(bse_names.get(name, name))
            symbols.append(lut.element_sym_from_Z(z, normalize=True))
    return symbols


def guess_input(label: str, basis_path: Path, symbols: list[str]) -> str:
    """Return TEMPLATE filled for the given basis set and elements."""
    atoms = "\n".join(
        f"{symbol:<3}{0.0:12.4f}{0.0:12.4f}{SPACING_ANGSTROM * k:12.4f}"
        for k, symbol in enumerate(symbols)
    )
    return TEMPLATE.format(
        label=label,
        basis_path=basis_path,
        natoms=len(symbols),
        atoms=atoms,
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("source", choices=SOURCES, help="origin of the basis set")
    parser.add_argument("basis", help="basis set name, e.g. cc-pwCVTZ")
    parser.add_argument(
        "--elements",
        nargs="+",
        help="elements in the guess input; default: all elements of the set",
    )
    parser.add_argument(
        "--root",
        type=Path,
        default=ROOT,
        help="beyond-rpa directory; default: the parent of scripts/",
    )
    args = parser.parse_args()

    name = args.basis.lower()
    label = f"{args.source}/{args.basis}"
    basis_file = (args.root / "basis-sets" / args.source / f"{name}.txt").resolve()
    guess_dir = (args.root / "guess" / "electron-densities" / args.source / name).resolve()

    if basis_file.exists():
        print(f"Kept existing {basis_file}")
    else:
        basis_file.parent.mkdir(parents=True, exist_ok=True)
        basis_file.write_text(library_text(args.source, args.basis))
        print(f"Wrote {basis_file}")

    available = elements_in_file(basis_file.read_text())
    symbols = [e.capitalize() for e in args.elements] if args.elements else available
    missing = [e for e in symbols if e not in available]
    if missing:
        raise ValueError(f"Not in {basis_file}: {' '.join(missing)}")

    guess_dir.mkdir(parents=True, exist_ok=True)
    inp = guess_dir / "atomic_guess.inp"
    basis_path = basis_file.relative_to(guess_dir, walk_up=True)
    inp.write_text(guess_input(label, basis_path, symbols))
    print(f"Wrote {inp}")
    generator = guess_dir.parent / GENERATOR.name
    shutil.copy(GENERATOR, generator)
    print(f"Wrote {generator}")
    print(f"Generate the guess densities with: python {generator} {guess_dir.name}")


if __name__ == "__main__":
    main()
