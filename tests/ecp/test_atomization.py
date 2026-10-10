"""
Atomization Energy Test Suite for beyond-rpa: ECPs with r^2 terms.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

The cc-pVTZ-PP ECPs of Hf, Ta, and W contain r^2 Exp(-a r^2) terms. The HF atomization energies
of HfO, TaO, and WO are assembled from the single points of the molecules and the atoms, and
compared with PySCF references stored in the input preambles (see pyscf_atomization.py and
inject_reference_preambles.py in the inputs directory). The single points are compared as well.
"""

import re
import sys
import time
import argparse
import subprocess
from pathlib import Path
import pytest

TESTS_DIR = Path(__file__).parent.parent
if str(TESTS_DIR) not in sys.path:
    sys.path.insert(0, str(TESTS_DIR))
import utils

BIN_PATH = Path(__file__).parents[2] / "bin" / "run"
INPUTS_DIR = Path(__file__).parent / "inputs" / "atomization"

#
# Tolerances for each accuracy level: (single point in a.u., atomization energy in kcal/mol).
# Tolerances must be set by the user (see .agents/TESTING.md); a check whose tolerance is
# None is skipped.
#
TOLERANCES = {
    "default": (None, None),
    "ludicrous": (None, None),
}

HARTREE_TO_KCAL = 627.5094688043
FLOAT_REGEX = r"([-+]?\d*\.\d+(?:[Ee][-+]?\d+)?)"
REFERENCE_KEY = "HF single point (a.u.)"
#
# Oxide input name: (oxide, metal, metal input name)
#
OXIDES = {
    "hfo": ("HfO", "Hf", "hf"),
    "tao": ("TaO", "Ta", "ta"),
    "wo": ("WO", "W", "w"),
}
OXYGEN = "o"
_results = {}


def get_reference_inputs() -> list[Path]:
    return sorted(f for name in OXIDES for f in INPUTS_DIR.glob(f"{name}_accuracy_*.inp"))


def accuracy(filepath: Path) -> str:
    return filepath.stem.rsplit("_accuracy_", 1)[1]


def companion(filepath: Path, name: str) -> Path:
    return INPUTS_DIR / f"{name}_accuracy_{accuracy(filepath)}.inp"


def run(filepath: Path, nthreads: int) -> float | None:
    """Return the converged HF energy of one input; each input runs once per session."""
    if filepath not in _results:
        result = subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)
        match = re.search(r"^\s*Converged energy\s+" + FLOAT_REGEX, result.stdout, re.MULTILINE)
        _results[filepath] = float(match.group(1)) if result.returncode == 0 and match else None
    return _results[filepath]


def extract_ref_energy(filepath: Path) -> float | None:
    for line in filepath.read_text().splitlines():
        if not line.startswith("!"):
            break
        if line[1:].strip().startswith(REFERENCE_KEY + ":"):
            return float(line.split(":", 1)[1])
    return None


def terms(filepath: Path, energies: dict) -> list[tuple[str, str, float | None]]:
    """Return (term, unit, value) rows of one oxide from the energies of its three inputs."""
    oxide, metal, metal_name = OXIDES[filepath.stem.rsplit("_accuracy_", 1)[0]]
    e_oxide = energies[filepath]
    e_metal = energies[companion(filepath, metal_name)]
    e_oxygen = energies[companion(filepath, OXYGEN)]
    atomization = None
    if None not in (e_oxide, e_metal, e_oxygen):
        atomization = (e_metal + e_oxygen - e_oxide) * HARTREE_TO_KCAL
    return [
        (f"E({oxide})", "a.u.", e_oxide),
        (f"E({metal})", "a.u.", e_metal),
        ("E(O)", "a.u.", e_oxygen),
        (f"D({metal}-O)", "kcal/mol", atomization),
    ]


def inputs_of(filepath: Path) -> list[Path]:
    metal_name = OXIDES[filepath.stem.rsplit("_accuracy_", 1)[0]][2]
    return [filepath, companion(filepath, metal_name), companion(filepath, OXYGEN)]


def tolerance(filepath: Path, unit: str) -> float | None:
    single_point, atomization = TOLERANCES[accuracy(filepath)]
    return single_point if unit == "a.u." else atomization


@pytest.mark.parametrize("filepath", get_reference_inputs(), ids=lambda p: p.stem)
def test_atomization(filepath: Path, record_property):
    files = inputs_of(filepath)
    ref = {f: extract_ref_energy(f) for f in files}
    assert None not in ref.values(), f"Reference energies missing for {filepath.name}"
    nthreads = utils.get_thread_count()
    calc = {f: run(f, nthreads) for f in files}
    for f, energy in calc.items():
        assert energy is not None, f"beyond-rpa failed or did not converge for {f.name}"
    rows = list(zip(terms(filepath, ref), terms(filepath, calc)))
    for (term, unit, reference), (_, _, calculated) in rows:
        record_property(f"reference {term}", reference)
        record_property(f"calculated {term}", calculated)
        record_property(f"deviation {term}", abs(calculated - reference))
    skipped = []
    for (term, unit, reference), (_, _, calculated) in rows:
        tol = tolerance(filepath, unit)
        if tol is None:
            skipped.append(term)
            continue
        assert calculated == pytest.approx(reference, abs=tol), \
            f"{term} deviation ({abs(calculated - reference):.2e} {unit}) exceeds {tol:.1e}"
    if skipped:
        pytest.skip(f"tolerances not set by the user: {', '.join(skipped)}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run atomization energy tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (including slow tests)")
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print("\nTolerances:")
    for level, (single_point, atomization) in TOLERANCES.items():
        sp = f"{single_point:.1e} a.u." if single_point is not None else "not set"
        at = f"{atomization:.1e} kcal/mol" if atomization is not None else "not set"
        print(f"  {level + ':':<11} single point {sp}, atomization {at}")

    files = get_reference_inputs()
    if not args.full:
        files = [f for f in files if utils.is_fast_test(f)]

    header = f"{'Term':<9} | {'Unit':<8} | {'Ref':>16} | {'Calc':>16} | {'Deviation':>10} | Status"
    width = len(header)
    print("\n" + "." * width)
    print(header)
    print("." * width)
    for filepath in files:
        print(f"Running {filepath.stem}... ", end="", flush=True)
        start_time = time.time()
        group = inputs_of(filepath)
        ref = {f: extract_ref_energy(f) for f in group}
        calc = {f: run(f, nthreads) for f in group}
        elapsed = time.time() - start_time
        if None in calc.values():
            failed = ", ".join(f.stem for f, energy in calc.items() if energy is None)
            print(f"CRASHED ({elapsed:.2f}s): {failed}")
            print("-" * width)
            continue
        print(f"done ({elapsed:.2f}s)")
        for (term, unit, reference), (_, _, calculated) in zip(terms(filepath, ref), terms(filepath, calc)):
            if reference is None:
                print(f"{term:<9} | {unit:<8} | {'N/A':>16} | {calculated:>16.8f} | {'N/A':>10} | ERROR")
                continue
            dev = abs(calculated - reference)
            tol = tolerance(filepath, unit)
            status = "NO TOL" if tol is None else ("PASSED" if dev <= tol else "FAILED")
            print(f"{term:<9} | {unit:<8} | {reference:>16.8f} | {calculated:>16.8f} | {dev:>10.2e} | {status}")
        print("-" * width)
