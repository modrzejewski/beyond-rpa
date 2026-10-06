"""
ccRepo Basis Set Test Suite for beyond-rpa.

Tests the cc-pwCVXZ basis sets from ccRepo (basis cc-repo/cc-pwCVXZ),
the atomic guess densities generated for them (jobtype atomic_guess),
and the errors for invalid basis set labels.
The inputs are in tests/basis/inputs/cc_repo. Their reference energies
come from the PySCF scripts in that folder; transfer_references.py
writes them into the inputs.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

To run the test suite:
- Pytest Mode: `pytest tests/basis/test_cc_repo.py`
- Standalone Mode: `python tests/basis/test_cc_repo.py [--full]`
"""

import argparse
import re
import shutil
import subprocess
import tempfile
import time
from pathlib import Path

import pytest

ROOT = Path(__file__).parent.parent.parent
BIN_PATH = ROOT / "bin" / "run"
INPUTS = Path(__file__).parent / "inputs" / "cc_repo"
#
# Tolerances must be set by the user (see .agents/TESTING.md).
# A check whose tolerance is None is skipped. The electron counts
# must equal Z in all printed digits, hence zero tolerance.
#
TOLERANCES = {
    "energy": 1.0e-8,
    "electron_count": 0.0,
    "basis_transform": 1.0e-14,
    "angular_mean": 1.0e-14,
}


def is_fast_test(filepath: Path) -> bool:
    """All inputs are fast. Each takes a few seconds; the atomic guess
    densities of Mg are already the converged densities."""
    return True


def preamble(filepath: Path, key: str) -> str | None:
    for line in filepath.read_text().splitlines():
        if not line.startswith("!"):
            break
        if line.startswith(f"! {key}:"):
            return line.split(":", 1)[1].strip()
    return None


def inputs(key: str) -> list[Path]:
    return sorted(f for f in INPUTS.glob("*.inp") if preamble(f, key) is not None)


def tolerance(name: str) -> float:
    value = TOLERANCES[name]
    if value is None:
        pytest.skip(f"tolerance '{name}' not set by the user")
    return value


def run(filepath: Path, workdir: Path) -> str:
    """Run a copy of the input in workdir, where the program writes its files."""
    inp = workdir / filepath.name
    shutil.copy(filepath, inp)
    result = subprocess.run([str(BIN_PATH), str(inp)], capture_output=True, text=True)
    return result.stdout + result.stderr


def value_after(output: str, label: str) -> float:
    match = re.search(re.escape(label) + r"\s+([-+0-9.Ee]+)", output)
    if match is None:
        raise ValueError(f"'{label}' not found in output")
    return float(match.group(1))


def energy_results(filepath: Path, workdir: Path) -> dict:
    output = run(filepath, workdir)
    return {
        "reference": float(preamble(filepath, "reference energy from pyscf")),
        "calculated": value_after(output, "Converged energy"),
        "complete_guess": "Incomplete SCF guess" not in output,
    }


def guess_results(filepath: Path, workdir: Path) -> dict:
    output = run(filepath, workdir)
    return {
        "electron_count": float(preamble(filepath, "reference electron count")),
        "electron_count_cart": value_after(output, "Tr(RhoAvg S), Cartesian AOs"),
        "electron_count_spher": value_after(output, "Tr(RhoAvg S), spherical AOs"),
        "basis_transform": value_after(output, "max|T RhoAvg T**T - RhoAvg_sao|"),
        "angular_mean": value_after(output, "max|c(RhoAvg) - c(Rho)|"),
        "change": value_after(output, "max|RhoAvg - Rho|"),
    }


@pytest.mark.parametrize("filepath", inputs("reference energy from pyscf"), ids=lambda p: p.stem)
def test_cc_repo_energy(filepath: Path, tmp_path, record_property):
    r = energy_results(filepath, tmp_path)
    dev = abs(r["calculated"] - r["reference"])
    record_property("reference", r["reference"])
    record_property("calculated", r["calculated"])
    record_property("deviation", dev)
    assert r["complete_guess"], "Incomplete SCF guess: atomic guess densities are missing"
    assert r["calculated"] == pytest.approx(r["reference"], abs=tolerance("energy"))


@pytest.mark.parametrize("filepath", inputs("reference electron count"), ids=lambda p: p.stem)
def test_atomic_guess(filepath: Path, tmp_path, record_property):
    r = guess_results(filepath, tmp_path)
    for key, value in r.items():
        record_property(key, value)
    z = r["electron_count"]
    assert r["electron_count_cart"] == pytest.approx(z, abs=tolerance("electron_count"))
    assert r["electron_count_spher"] == pytest.approx(z, abs=tolerance("electron_count"))
    assert r["basis_transform"] <= tolerance("basis_transform")
    assert r["angular_mean"] <= tolerance("angular_mean")


@pytest.mark.parametrize("filepath", inputs("expected message"), ids=lambda p: p.stem)
def test_label_error(filepath: Path, tmp_path):
    output = run(filepath, tmp_path)
    assert preamble(filepath, "expected message") in output


def table_rows(filepath: Path, workdir: Path) -> list[tuple]:
    """Return (quantity, reference, result, tolerance key) rows for one input."""
    if preamble(filepath, "reference energy from pyscf") is not None:
        r = energy_results(filepath, workdir)
        rows = [("E(HF)", r["reference"], r["calculated"], "energy")]
        rows.append(("complete atomic guess", 1.0, float(r["complete_guess"]), None))
    elif preamble(filepath, "reference electron count") is not None:
        r = guess_results(filepath, workdir)
        z = r["electron_count"]
        rows = [
            ("Tr(RhoAvg S) Cartesian", z, r["electron_count_cart"], "electron_count"),
            ("Tr(RhoAvg S) spherical", z, r["electron_count_spher"], "electron_count"),
            ("max|T D T^T - D_sao|", 0.0, r["basis_transform"], "basis_transform"),
            ("max|c(D_avg) - c(D)|", 0.0, r["angular_mean"], "angular_mean"),
        ]
    else:
        found = preamble(filepath, "expected message") in run(filepath, workdir)
        rows = [("expected error message", 1.0, float(found), None)]
    return rows


def status(ref: float, value: float, key: str | None) -> str:
    if key is None:
        return "PASSED" if value == ref else "FAILED"
    tol = TOLERANCES[key]
    if tol is None:
        return "NO TOLERANCE"
    return "PASSED" if abs(value - ref) <= tol else "FAILED"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--full", action="store_true", help="also run the slow tests")
    args = parser.parse_args()

    print("\n" + "." * 124)
    print(f"{'Test Title':<26} | {'Quantity':<26} | {'Reference':>17} | {'Result':>17} | {'Deviation':>10} | Status")
    print("." * 124)
    for filepath in sorted(INPUTS.glob("*.inp")):
        if not args.full and not is_fast_test(filepath):
            continue
        print(f"Running {filepath.stem}...", end=" ", flush=True)
        start = time.time()
        with tempfile.TemporaryDirectory() as tmp:
            try:
                rows = table_rows(filepath, Path(tmp))
            except Exception as error:
                rows = [(f"error: {error}", float("nan"), float("nan"), None)]
        print(f"done ({time.time() - start:.2f}s)")
        for quantity, ref, value, key in rows:
            print(f"{filepath.stem:<26} | {quantity:<26} | {ref:>17.10f} | {value:>17.10f} | "
                  f"{abs(value - ref):>10.2e} | {status(ref, value, key)}")
    print("\n")
