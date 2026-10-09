"""
Quadrupole Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

The traceless quadrupole printed after SCF is compared with PySCF
computed from the same converged HF density. The reference values
are stored in the header of each input file.
Use inputs/generate_inputs.py to regenerate the inputs.
"""

import sys
import re
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
INPUTS_DIR = Path(__file__).parent / "inputs"

TOLERANCE = 1.0e-3  # Debye*Angs

COMPONENTS = {"xx": (0, 0), "yy": (1, 1), "zz": (2, 2), "xy": (0, 1), "xz": (0, 2), "yz": (1, 2)}
FLOAT_REGEX = r"([-+]?\d*\.\d+(?:[EeDd][-+]?\d+)?)"
HEADER_QUADRUPOLE = "traceless quadrupole moment"


def get_input_files():
    return sorted(INPUTS_DIR.glob("*.inp"))


def extract_ref_quadrupole(filepath: Path) -> dict:
    quadrupole = {}
    with filepath.open("r") as f:
        for line in f:
            match = re.match(r"!\s*Q(\w\w)\s*=\s*" + FLOAT_REGEX, line)
            if match:
                quadrupole[match.group(1)] = float(match.group(2))
    return quadrupole


def extract_calc_quadrupole(text: str) -> dict:
    lines = text.splitlines()
    start = next(i for i, line in enumerate(lines) if HEADER_QUADRUPOLE in line)
    tensor = [[0.0] * 3 for _ in range(3)]
    for i in range(3):
        fields = lines[start + 2 + i].split()
        for j in range(3):
            tensor[i][j] = float(fields[1 + j])
    return {name: tensor[i][j] for name, (i, j) in COMPONENTS.items()}


@pytest.mark.parametrize("filepath", get_input_files(), ids=lambda p: p.stem)
def test_quadrupole(filepath: Path, record_property):
    print(f"\nTesting {filepath.name} ... ", end="", flush=True)

    try:
        ref = extract_ref_quadrupole(filepath)
        assert set(ref) == set(COMPONENTS), f"Reference quadrupole incomplete in {filepath.name}"

        ncores = utils.get_thread_count()
        result = subprocess.run([str(BIN_PATH), "-nt", str(ncores), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"

        calc = extract_calc_quadrupole(result.stdout)
        for name in COMPONENTS:
            dev = abs(calc[name] - ref[name])
            record_property(f"reference_Q{name}", ref[name])
            record_property(f"calculated_Q{name}", calc[name])
            record_property(f"deviation_Q{name}", dev)

        for name in COMPONENTS:
            assert calc[name] == pytest.approx(ref[name], abs=TOLERANCE), (
                f"Q{name} deviation ({abs(calc[name] - ref[name]):.2e}) exceeds {TOLERANCE}"
            )
        print("PASSED")
    except Exception:
        print("FAILED")
        raise


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run quadrupole tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (including slow tests)")
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print("\nTolerance:")
    print(f"  traceless quadrupole: {TOLERANCE:.1e} Debye*Angs")

    header = f"{'Term':<4} | {'Ref (D*A)':>12} | {'Calc (D*A)':>12} | {'Deviation':>12} | Status"
    width = len(header)
    print("\n" + "." * width)
    print(header)
    print("." * width)

    files = get_input_files()
    if not args.full:
        files = [f for f in files if utils.is_fast_test(f)]

    for filepath in files:
        ref = extract_ref_quadrupole(filepath)

        print(f"Running {filepath.name}... ", end="", flush=True)
        start_time = time.time()

        try:
            result = subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)
            elapsed = time.time() - start_time
            if result.returncode != 0:
                print(f"FAILED ({elapsed:.2f}s)")
                print(f"{'N/A':<4} | {'N/A':>12} | {'N/A':>12} | {'N/A':>12} | CRASHED")
                print("-" * width)
                continue

            print(f"done ({elapsed:.2f}s)")
            calc = extract_calc_quadrupole(result.stdout)

            for name in COMPONENTS:
                dev = abs(calc[name] - ref[name])
                status = "PASSED" if dev <= TOLERANCE else "FAILED"
                print(f"{name:<4} | {ref[name]:>12.6f} | {calc[name]:>12.6f} | {dev:>12.2e} | {status}")
            print("-" * width)

        except Exception:
            print("ERROR")
            print(f"{'N/A':<4} | {'N/A':>12} | {'N/A':>12} | {'N/A':>12} | ERROR")
            print("-" * width)
