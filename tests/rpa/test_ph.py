"""
RPA+ph Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.
By default, it only runs tests with "default" accuracy to save time.

To run the full test suite (including expensive tests at "tight" and "ludicrous" accuracy levels):
- Pytest Mode: Set the environment variable `BEYOND_RPA_FULL=1` (e.g. `BEYOND_RPA_FULL=1 pytest tests/rpa/test_rpa.py`)
- Standalone Mode: Run with the `--full` argument (e.g. `python tests/rpa/test_rpa.py --full`)
"""

import os
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

BIN_PATH = Path(__file__).parent.parent.parent / "bin" / "run"

REF_KEYS = [
    "Eint(direct ring)",
    "Eint(SOSEX)",
    "Eint(3rd order ph)",
    "Eint(total)",
    "EintNadd(direct ring)",
    "EintNadd(SOSEX)",
    "EintNadd(3rd order ph)",
    "EintNadd(total)",
]

CALC_KEYS = [
    "Eint(direct ring)",
    "Eint(SOSEX)",
    "Eint(3rd order ph)",
    "Eint(total)",
    "EintNadd(direct ring)",
    "EintNadd(SOSEX)",
    "EintNadd(3rd order ph)",
    "EintNadd(total)",
]

KEY_MAP = dict(zip(REF_KEYS, CALC_KEYS))

#
# Tolerances for energy components in kcal/mol
#
TOLERANCE_DEFAULT = 3.0e-4
TOLERANCE_HIGH_ACCURACY = 3.0e-5

def get_input_files():
    inputs_dir = Path(__file__).parent / "inputs" / "ph"
    return sorted(inputs_dir.glob("*.inp"))

def is_full_run():
    # Standalone mode flag
    if "--full" in sys.argv:
        return True
    # Pytest mode environment variable
    if os.environ.get("BEYOND_RPA_FULL") == "1":
        return True
    return False

def extract_ref_energies(filepath: Path) -> dict:
    energies = {}
    with open(filepath, 'r') as f:
        for line in f:
            if not line.startswith('!'):
                if line.strip():
                    break
                continue
            for ref_key in REF_KEYS:
                pattern = r"!\s*" + re.escape(ref_key) + r"\s*=\s*([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"
                match = re.search(pattern, line)
                if match:
                    energies[ref_key] = float(match.group(1))
    return energies

def extract_calc_energies(text: str) -> dict:
    energies = {}
    for key in CALC_KEYS:
        # Match lines starting with the key to avoid matching table headers
        pattern = r"^\s*" + re.escape(key) + r"\s+([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"
        match = re.search(pattern, text, re.MULTILINE)
        if match:
            energies[key] = float(match.group(1))
    return energies

def get_tolerance(filepath: Path) -> float:
    name = filepath.name
    if "accuracy_tight" in name or "accuracy_ludicrous" in name:
        return TOLERANCE_HIGH_ACCURACY
    return TOLERANCE_DEFAULT

@pytest.mark.parametrize("filepath", get_input_files(), ids=lambda p: p.stem)
def test_rpa_energy(filepath: Path, record_property):
    if not is_full_run() and "accuracy_default" not in filepath.name:
        pytest.skip("Skipping non-default accuracy test. Run with --full (standalone) or BEYOND_RPA_FULL=1 (pytest).")
        
    print(f"\nTesting {filepath.name} ... ", end="", flush=True)
    
    try:
        ref_energies = extract_ref_energies(filepath)
        if not ref_energies:
            pytest.skip(f"No reference energies found in {filepath.name}.")
            
        ncores = utils.get_thread_count()
        result = subprocess.run([str(BIN_PATH), "-nt", str(ncores), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
        
        calc_energies = extract_calc_energies(result.stdout)
        tolerance = get_tolerance(filepath)
        
        # Check all available keys
        for ref_key, ref_val in ref_energies.items():
            calc_key = KEY_MAP[ref_key]
            if calc_key in calc_energies:
                calc_val = calc_energies[calc_key]
                dev = abs(calc_val - ref_val)
                
                # CI/CD Property recording per value
                safe_key = calc_key.replace(" ", "_").replace("(", "_").replace(")", "")
                record_property(f"reference_{safe_key}", ref_val)
                record_property(f"calculated_{safe_key}", calc_val)
                record_property(f"deviation_{safe_key}", dev)
                
                assert calc_val == pytest.approx(ref_val, abs=tolerance), f"{calc_key} deviation ({dev:.2e}) exceeds {tolerance}"
            else:
                pytest.fail(f"{calc_key} mapped from {ref_key} found in reference but missing in calculated output.")
                
        print("PASSED")
    except Exception:
        print("FAILED")
        raise

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run RPA tests.")
    parser.add_argument("--full", action="store_true", help="Run full test suite (including tight/ludicrous)")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use (default: physical cores)")
    args = parser.parse_args()

    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print("\n" + f"Tolerance for deviations in the interaction energy (default accuracy): {TOLERANCE_DEFAULT:.1e} kcal/mol")
    print(f"Tolerance for deviations in the interaction energy (high accuracy): {TOLERANCE_HIGH_ACCURACY:.1e} kcal/mol")
    
    print("\n" + "."*85)
    print(f"{'Property':<25} | {'Ref (kcal/mol)':>15} | {'Calc (kcal/mol)':>15} | {'Deviation':>12} | {'Status'}")
    print("." * 85)
    
    all_files = get_input_files()
    if not args.full:
        all_files = [f for f in all_files if "accuracy_default" in f.name]
        
    for filepath in all_files:
        print(f"Running {filepath.name}... ", end="", flush=True)
        start_time = time.time()
        
        ref_energies = extract_ref_energies(filepath)
        if not ref_energies:
            print("FAILED")
            print(f"{'N/A':<25} | {'N/A':>15} | {'N/A':>15} | {'N/A':>12} | NO REF ENERGIES")
            print("-" * 85)
            continue
            
        tolerance = get_tolerance(filepath)
        
        try:
            result = subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)
            if result.returncode != 0:
                elapsed = time.time() - start_time
                print(f"FAILED ({elapsed:.2f}s)")
                print(f"{'N/A':<25} | {'N/A':>15} | {'N/A':>15} | {'N/A':>12} | CRASHED")
                print("-" * 85)
                continue
                
            elapsed = time.time() - start_time
            print(f"done ({elapsed:.2f}s)")
            calc_energies = extract_calc_energies(result.stdout)
            
            for ref_key, ref_val in ref_energies.items():
                calc_key = KEY_MAP[ref_key]
                calc_val = calc_energies.get(calc_key)
                if calc_val is not None:
                    dev = abs(calc_val - ref_val)
                    status = "PASSED" if dev <= tolerance else "FAILED"
                    dev_str = f"{dev:.2e}"
                    calc_str = f"{calc_val:.6f}"
                else:
                    dev_str = "N/A"
                    calc_str = "N/A"
                    status = "FAILED"
                    
                ref_str = f"{ref_val:.6f}"
                print(f"{calc_key:<25} | {ref_str:>15} | {calc_str:>15} | {dev_str:>12} | {status}")
                
        except Exception as e:
            elapsed = time.time() - start_time
            print(f"FAILED ({elapsed:.2f}s)")
            print(f"{'N/A':<25} | {'N/A':>15} | {'N/A':>15} | {'N/A':>12} | ERROR")
            
        print("-" * 85)
        
    print("\n")
