"""Read basis sets of the beyond-rpa library for PySCF reference calculations."""
from pathlib import Path

LIBRARY = Path(__file__).resolve().parents[4] / "basis-sets"
ANGULAR_MOMENTA = "SPDFGHI"


def element(basis_file: str, name: str) -> list:
    """Return one element of a GAMESS-US library file as a PySCF basis."""
    lines = (LIBRARY / basis_file).read_text().splitlines()
    k = [line.strip().upper() for line in lines].index(name.upper()) + 1
    shells = []
    while k < len(lines):
        words = lines[k].split()
        if len(words) == 2 and words[0] in ANGULAR_MOMENTA and words[1].isdigit():
            n = int(words[1])
            primitives = []
            for line in lines[k + 1 : k + 1 + n]:
                _, exponent, coefficient = line.replace("D", "E").split()[:3]
                primitives.append([float(exponent), float(coefficient)])
            shells.append([ANGULAR_MOMENTA.index(words[0])] + primitives)
            k += n + 1
        elif shells and (not words or words[0].upper() == "$END" or words[0].isalpha()):
            break
        else:
            k += 1
    return shells
