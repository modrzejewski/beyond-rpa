"""
Frozen Core Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

Each reference input freezes a different set of core orbitals of a dimer. The HF and direct RPA
energies of the dimer, of both monomers in the dimer basis, and the interaction energies are
compared with PySCF references stored in the input preamble (see pyscf_*.py and
inject_reference_preambles.py in the inputs directory).
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

#
# Tolerances for each accuracy level: (single point in a.u., interaction energy in kcal/mol)
#
TOLERANCES = {
    "default": (1.0e-4, 5.0e-4),
    "ludicrous": (3.0e-5, 5.0e-5),
}

FLOAT_REGEX = r"([-+]?\d*\.\d+(?:[Ee][-+]?\d+)?)"
SUBSYSTEMS = ["AB", "A", "B"]

# (term, reference key in the preamble, unit)
TERMS = (
    [(f"HF {s}", f"HF single point {s} (a.u.)", "a.u.") for s in SUBSYSTEMS]
    + [(f"dRPA {s}", f"dRPA single point {s} (a.u.)", "a.u.") for s in SUBSYSTEMS]
    + [
        ("HF A...B", "HF interaction A...B (kcal/mol)", "kcal/mol"),
        ("dRPA A...B", "dRPA interaction A...B (kcal/mol)", "kcal/mol"),
    ]
)

SINGLE_POINT_KEYS = ["mean field", "1-RDM linear", "1-RDM quadratic", "direct ring"]
INTERACTION_KEYS = ["Eint(HF)", "Eint(1-RDM linear)", "Eint(1-RDM quadratic)", "Eint(direct ring)"]


def get_reference_inputs() -> list[Path]:
    return sorted(INPUTS_DIR.glob("*_accuracy_*.inp"))


def run(filepath: Path, nthreads: int) -> subprocess.CompletedProcess:
    return subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)


def extract_ref_energies(filepath: Path) -> dict:
    energies = {}
    for line in filepath.read_text().splitlines():
        if not line.startswith("!"):
            break
        for term, key, _ in TERMS:
            if line[1:].strip().startswith(key + ":"):
                energies[term] = float(line.split(":", 1)[1])
    return energies


def subsystem_label(header: str) -> str:
    if "Monomer A" in header:
        return "A"
    if "Monomer B" in header:
        return "B"
    return "AB"


def extract_calc_energies(text: str) -> dict:
    """
    Single points are taken from the first section printed after the first
    "RPA for" header of each subsystem. For monomers, this section contains the
    canonical-orbital direct ring energy used in the final interaction energy.
    """
    terms = {}
    found = {}
    label = None
    for line in text.splitlines():
        if "RPA for " in line:
            new_label = subsystem_label(line)
            label = new_label if new_label not in found else None
            if label is not None:
                found[label] = {}
            continue
        if label is None:
            continue
        for key in SINGLE_POINT_KEYS:
            if key not in found[label]:
                match = re.match(r"^\s*" + re.escape(key) + r"\s+" + FLOAT_REGEX, line)
                if match:
                    found[label][key] = float(match.group(1))
    for s, values in found.items():
        if all(key in values for key in SINGLE_POINT_KEYS):
            terms[f"HF {s}"] = values["mean field"] + values["1-RDM linear"] + values["1-RDM quadratic"]
            terms[f"dRPA {s}"] = values["direct ring"]
    interaction = {}
    for key in INTERACTION_KEYS:
        match = re.search(r"^\s*" + re.escape(key) + r"\s+" + FLOAT_REGEX, text, re.MULTILINE)
        if match:
            interaction[key] = float(match.group(1))
    if all(key in interaction for key in INTERACTION_KEYS):
        terms["HF A...B"] = interaction["Eint(HF)"] + interaction["Eint(1-RDM linear)"] + interaction["Eint(1-RDM quadratic)"]
        terms["dRPA A...B"] = interaction["Eint(direct ring)"]
    return terms


def tolerance(filepath: Path, unit: str) -> float:
    accuracy = filepath.stem.rsplit("_accuracy_", 1)[1]
    single_point, interaction = TOLERANCES[accuracy]
    return single_point if unit == "a.u." else interaction


@pytest.mark.parametrize("filepath", get_reference_inputs(), ids=lambda p: p.stem)
def test_frozen_core(filepath: Path, record_property):
    ref = extract_ref_energies(filepath)
    assert len(ref) == len(TERMS), f"Reference energies missing in {filepath.name}"
    result = run(filepath, utils.get_thread_count())
    assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
    calc = extract_calc_energies(result.stdout)
    for term, _, unit in TERMS:
        assert term in calc, f"Calculated value of {term} missing from output."
        dev = abs(calc[term] - ref[term])
        record_property(f"reference {term}", ref[term])
        record_property(f"calculated {term}", calc[term])
        record_property(f"deviation {term}", dev)
    for term, _, unit in TERMS:
        tol = tolerance(filepath, unit)
        assert calc[term] == pytest.approx(ref[term], abs=tol), \
            f"{term} deviation ({abs(calc[term] - ref[term]):.2e} {unit}) exceeds {tol:.1e}"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run frozen core tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (including slow tests)")
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print("\nTolerances:")
    for accuracy, (single_point, interaction) in TOLERANCES.items():
        print(f"  {accuracy + ':':<11} single point {single_point:.1e} a.u., interaction {interaction:.1e} kcal/mol")

    files = get_reference_inputs()
    if not args.full:
        files = [f for f in files if utils.is_fast_test(f)]

    header = f"{'Term':<10} | {'Unit':<8} | {'Ref':>16} | {'Calc':>16} | {'Deviation':>10} | Status"
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
        for term, _, unit in TERMS:
            if term not in ref or term not in calc:
                print(f"{term:<10} | {unit:<8} | {'N/A':>16} | {'N/A':>16} | {'N/A':>10} | ERROR")
                continue
            dev = abs(calc[term] - ref[term])
            status = "PASSED" if dev <= tolerance(filepath, unit) else "FAILED"
            print(f"{term:<10} | {unit:<8} | {ref[term]:>16.8f} | {calc[term]:>16.8f} | {dev:>10.2e} | {status}")
        print("-" * width)
