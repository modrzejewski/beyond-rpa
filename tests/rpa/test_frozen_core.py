"""
Frozen Core Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

The frozen_orbitals keyword is compared with the energy-threshold selection (coreorbthresh)
of the same orbitals. The reference energy is computed in the same session, so no reference
values are stored. Each invalid input states the expected error message in its header.
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
INPUTS_DIR = Path(__file__).parent / "inputs" / "frozen_core"

TOLERANCE_ENERGY = 1.0e-8  # a.u.
TOLERANCE_EINT = 3.0e-5  # kcal/mol

FLOAT_REGEX = r"([-+]?\d*\.\d+(?:[Ee][-+]?\d+)?)"
ENERGIES_HEADER = "Single-Point Energies"

# (input with frozen_orbitals, reference input, energy label, tolerance, results equal)
CASES = [
    ("water_frozen_O1_H0", "water_default", "E(total)", TOLERANCE_ENERGY, True),
    ("water_frozen_O0_H0", "water_all_correlated", "E(total)", TOLERANCE_ENERGY, True),
    ("water_frozen_O0_H0", "water_default", "E(total)", TOLERANCE_ENERGY, False),
    ("dimer_frozen_O1_H0", "dimer_default", "Eint(total)", TOLERANCE_EINT, True),
    ("xenon_frozen_0", "xenon_all_correlated", "E(total)", TOLERANCE_ENERGY, True),
    ("zn_water_frozen_Zn9_O1_H0", "zn_water_coreorbthresh_m2", "E(total)", TOLERANCE_ENERGY, True),
]
CASE_IDS = [f"{frozen}_vs_{reference}" for frozen, reference, *_ in CASES]

INVALID = sorted(path.stem for path in INPUTS_DIR.glob("invalid_*.inp"))


def run(name: str, nthreads: int) -> subprocess.CompletedProcess:
    filepath = INPUTS_DIR / f"{name}.inp"
    return subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)


def energy(name: str, label: str, nthreads: int) -> float:
    result = run(name, nthreads)
    assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
    match = re.search(r"^\s*" + re.escape(label) + r"\s+" + FLOAT_REGEX, result.stdout, re.MULTILINE)
    assert match, f"{label} not found in the output of {name}"
    return float(match.group(1))


def expected_error(name: str) -> str:
    for line in (INPUTS_DIR / f"{name}.inp").read_text().splitlines():
        if line.startswith("! Expected error:"):
            return line.split(":", 1)[1].strip()
    raise ValueError(f"No expected error message in {name}.inp")


@pytest.mark.parametrize("frozen, reference, label, tolerance, equal", CASES, ids=CASE_IDS)
def test_frozen_orbitals(frozen, reference, label, tolerance, equal, record_property):
    nthreads = utils.get_thread_count()
    ref = energy(reference, label, nthreads)
    calc = energy(frozen, label, nthreads)
    record_property("reference", ref)
    record_property("calculated", calc)
    record_property("deviation", abs(calc - ref))
    if equal:
        assert calc == pytest.approx(ref, abs=tolerance)
    else:
        assert calc != pytest.approx(ref, abs=tolerance)


@pytest.mark.parametrize("name", INVALID)
def test_invalid_input(name):
    result = run(name, utils.get_thread_count())
    assert expected_error(name) in result.stdout
    assert ENERGIES_HEADER not in result.stdout


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run frozen core tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (all frozen core tests are fast)")
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print("\nTolerances:")
    print(f"  E(total):    {TOLERANCE_ENERGY:.1e} a.u.")
    print(f"  Eint(total): {TOLERANCE_EINT:.1e} kcal/mol")

    width = 112
    print("\n" + "." * width)
    print(f"{'Test':<45} | {'Property':<11} | {'Ref':>14} | {'Calc':>14} | {'Deviation':>10} | {'Expected':<8} | {'Status'}")
    print("." * width)
    for (frozen, reference, label, tolerance, equal), case_id in zip(CASES, CASE_IDS):
        print(f"Running {case_id}... ", end="", flush=True)
        start_time = time.time()
        try:
            ref = energy(reference, label, nthreads)
            calc = energy(frozen, label, nthreads)
        except AssertionError:
            print(f"FAILED ({time.time() - start_time:.2f}s)")
            print(f"{case_id:<45} | {label:<11} | {'N/A':>14} | {'N/A':>14} | {'N/A':>10} | {'N/A':<8} | CRASHED")
            print("-" * width)
            continue
        print(f"done ({time.time() - start_time:.2f}s)")
        dev = abs(calc - ref)
        status = "PASSED" if (dev <= tolerance) == equal else "FAILED"
        print(f"{case_id:<45} | {label:<11} | {ref:>14.8f} | {calc:>14.8f} | {dev:>10.2e} | {'equal' if equal else 'differ':<8} | {status}")
        print("-" * width)

    width = 100
    print("\n" + "." * width)
    print(f"{'Invalid input':<30} | {'Expected message':<50} | {'Message':<7} | {'Energies':<8} | {'Status'}")
    print("." * width)
    for name in INVALID:
        print(f"Running {name}... ", end="", flush=True)
        start_time = time.time()
        result = run(name, nthreads)
        print(f"done ({time.time() - start_time:.2f}s)")
        message = expected_error(name)
        found = message in result.stdout
        computed = ENERGIES_HEADER in result.stdout
        status = "PASSED" if found and not computed else "FAILED"
        print(f"{name:<30} | {message:<50} | {'found' if found else 'missing':<7} | {'yes' if computed else 'no':<8} | {status}")
        print("-" * width)
