"""
Semicore THC Grid Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

THC-based post-SCF energies can be sensitive to the quality of the
initial THC grid if semicore orbitals are correlated with cc-pwCVXZ
basis sets. This test validates the numerical accuracy in this
difficult case. The counterpoise-corrected HF and MP2 correlation
interaction energies are compared with PySCF references (exact
integrals) stored in the input preamble (see pyscf_semicore_thc.py
and inject_reference_preambles.py in the inputs directory).

The core-valence correction, i.e., the MP2 correlation interaction
energy of the semicore input minus that of the valence input at the
same accuracy level, is tested as well. The valence inputs use Mg
cc-pVTZ instead of cc-pwCVTZ, so the correction includes the change
of the Mg basis set.

To run the test suite:
- Pytest Mode: `pytest tests/integrals/test_semicore_thc.py [--full]`
- Standalone Mode: `python tests/integrals/test_semicore_thc.py [--full]`
"""

import re
import sys
import time
import argparse
import functools
import subprocess
from pathlib import Path
import pytest

TESTS_DIR = Path(__file__).parent.parent
if str(TESTS_DIR) not in sys.path:
    sys.path.insert(0, str(TESTS_DIR))
import utils

BIN_PATH = Path(__file__).parents[2] / "bin" / "run"
INPUTS_DIR = Path(__file__).parent / "inputs" / "semicore_thc"

#
# Tolerances of the interaction energies (kcal/mol) for each accuracy level
#
TOLERANCES = {
    "default": {"HF A...B": 1.0e-5, "MP2 A...B": 4.0e-3, "MP2 CV A...B": 4.0e-3},
    "ludicrous": {"HF A...B": 1.0e-5, "MP2 A...B": 7.0e-5, "MP2 CV A...B": 7.0e-5},
}

FLOAT_REGEX = r"([-+]?\d*\.\d+(?:[Ee][-+]?\d+)?)"

# (term, reference key in the preamble)
TERMS = [
    ("HF A...B", "HF interaction A...B (kcal/mol)"),
    ("MP2 A...B", "MP2 correlation interaction A...B (kcal/mol)"),
]

#
# Core-valence correction: MP2 A...B of the semicore input
# minus MP2 A...B of the valence input at the same accuracy level
#
CV_TERM = "MP2 CV A...B"
CV_INPUTS = {
    level: (INPUTS_DIR / f"co_mgo_semicore_accuracy_{level}.inp", INPUTS_DIR / f"co_mgo_valence_accuracy_{level}.inp")
    for level in TOLERANCES
}

INTERACTION_KEYS = ["Eint(HF)", "Eint(1-RDM linear)", "Eint(1-RDM quadratic)", "Eint(total MP2)"]


def get_reference_inputs() -> list[Path]:
    return sorted(INPUTS_DIR.glob("*_accuracy_*.inp"))


@functools.cache
def run(filepath: Path, nthreads: int) -> subprocess.CompletedProcess:
    """
    Run each input once. The core-valence test reuses the output.
    """
    return subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)


def extract_ref_energies(filepath: Path) -> dict:
    energies = {}
    for line in filepath.read_text().splitlines():
        if not line.startswith("!"):
            break
        for term, key in TERMS:
            if line[1:].strip().startswith(key + ":"):
                energies[term] = float(line.split(":", 1)[1])
    return energies


def extract_calc_energies(text: str) -> dict:
    """
    The HF interaction energy includes the 1-RDM corrections, which
    account for the difference between the SCF integrals and the
    accurate integrals of the post-SCF step.
    """
    interaction = {}
    for key in INTERACTION_KEYS:
        match = re.search(r"^\s*" + re.escape(key) + r"\s+" + FLOAT_REGEX, text, re.MULTILINE)
        if match:
            interaction[key] = float(match.group(1))
    terms = {}
    if all(key in interaction for key in INTERACTION_KEYS[:3]):
        terms["HF A...B"] = interaction["Eint(HF)"] + interaction["Eint(1-RDM linear)"] + interaction["Eint(1-RDM quadratic)"]
    if "Eint(total MP2)" in interaction:
        terms["MP2 A...B"] = interaction["Eint(total MP2)"]
    return terms


def tolerance(filepath: Path, term: str) -> float:
    accuracy = filepath.stem.rsplit("_accuracy_", 1)[1]
    return TOLERANCES[accuracy][term]


def core_valence_correction(semicore: Path, valence: Path, nthreads: int) -> tuple[float, float]:
    """
    Reference and calculated MP2 A...B of the semicore input minus that of the valence input
    """
    ref = {f: extract_ref_energies(f)["MP2 A...B"] for f in (semicore, valence)}
    calc = {f: extract_calc_energies(run(f, nthreads).stdout)["MP2 A...B"] for f in (semicore, valence)}
    return ref[semicore] - ref[valence], calc[semicore] - calc[valence]


@pytest.mark.parametrize("filepath", get_reference_inputs(), ids=lambda p: p.stem)
def test_semicore_thc(filepath: Path, record_property):
    ref = extract_ref_energies(filepath)
    assert len(ref) == len(TERMS), f"Reference energies missing in {filepath.name}"
    result = run(filepath, utils.get_thread_count())
    assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
    calc = extract_calc_energies(result.stdout)
    for term, _ in TERMS:
        assert term in calc, f"Calculated value of {term} missing from output."
        dev = abs(calc[term] - ref[term])
        record_property(f"reference {term}", ref[term])
        record_property(f"calculated {term}", calc[term])
        record_property(f"deviation {term}", dev)
    for term, _ in TERMS:
        tol = tolerance(filepath, term)
        assert calc[term] == pytest.approx(ref[term], abs=tol), \
            f"{term} deviation ({abs(calc[term] - ref[term]):.2e} kcal/mol) exceeds {tol:.1e}"


@pytest.mark.parametrize("filepath, valence", [
    pytest.param(semicore, valence, id=f"co_mgo_core_valence_accuracy_{level}")
    for level, (semicore, valence) in CV_INPUTS.items()
])
def test_core_valence_correction(filepath: Path, valence: Path, record_property):
    nthreads = utils.get_thread_count()
    for f in (filepath, valence):
        result = run(f, nthreads)
        assert result.returncode == 0, f"beyond-rpa failed for {f.name}:\n{result.stderr}"
    ref, calc = core_valence_correction(filepath, valence, nthreads)
    dev = abs(calc - ref)
    record_property(f"reference {CV_TERM}", ref)
    record_property(f"calculated {CV_TERM}", calc)
    record_property(f"deviation {CV_TERM}", dev)
    tol = tolerance(filepath, CV_TERM)
    assert calc == pytest.approx(ref, abs=tol), \
        f"{CV_TERM} deviation ({dev:.2e} kcal/mol) exceeds {tol:.1e}"


def table_row(term: str, ref: float, calc: float, tol: float) -> str:
    dev = abs(calc - ref)
    status = "PASSED" if dev <= tol else "FAILED"
    return f"{term:<12} | {'kcal/mol':<8} | {ref:>16.8f} | {calc:>16.8f} | {dev:>10.2e} | {status}"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run semicore THC grid tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (including slow tests)")
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print("\nTolerances:")
    for accuracy, tolerances in TOLERANCES.items():
        shown = ", ".join(f"{term} {tol:.1e}" for term, tol in tolerances.items())
        print(f"  {accuracy + ':':<11} {shown} kcal/mol")

    files = get_reference_inputs()
    if not args.full:
        files = [f for f in files if utils.is_fast_test(f)]

    header = f"{'Term':<12} | {'Unit':<8} | {'Ref':>16} | {'Calc':>16} | {'Deviation':>10} | Status"
    width = len(header)
    print("\n" + "." * width)
    print(header)
    print("." * width)
    for filepath in files:
        print(f"Running {filepath.stem}... ", end="", flush=True)
        start_time = time.time()
        result = run(filepath, nthreads)
        elapsed = time.time() - start_time
        if result.returncode != 0:
            print(f"CRASHED ({elapsed:.2f}s)")
            print("-" * width)
            continue
        print(f"done ({elapsed:.2f}s)")
        ref = extract_ref_energies(filepath)
        calc = extract_calc_energies(result.stdout)
        for term, _ in TERMS:
            if term not in ref or term not in calc:
                print(f"{term:<12} | {'kcal/mol':<8} | {'N/A':>16} | {'N/A':>16} | {'N/A':>10} | ERROR")
                continue
            print(table_row(term, ref[term], calc[term], tolerance(filepath, term)))
        print("-" * width)
    for level, (semicore, valence) in CV_INPUTS.items():
        if not args.full and not utils.is_fast_test(semicore):
            continue
        print(f"Running co_mgo_core_valence_accuracy_{level}... ", end="", flush=True)
        start_time = time.time()
        try:
            ref, calc = core_valence_correction(semicore, valence, nthreads)
        except KeyError:
            print(f"ERROR ({time.time() - start_time:.2f}s)")
            print("-" * width)
            continue
        print(f"done ({time.time() - start_time:.2f}s)")
        print(table_row(CV_TERM, ref, calc, tolerance(semicore, CV_TERM)))
        print("-" * width)
