"""
rPT2 Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.
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

# Keys frozen in the .inp comments
REF_KEYS = [
    "Eint(direct ring)",
    "Eint(total)",
    "EintNadd(direct ring)",
    "EintNadd(total)",
]

# Keys output by the beyond-rpa program (can be safely modified in the future)
CALC_KEYS = [
    "Eint(direct ring)",
    "Eint(total)",
    "EintNadd(direct ring)",
    "EintNadd(total)",
]

KEY_MAP = dict(zip(REF_KEYS, CALC_KEYS))

#
# Tolerance for energy components in kcal/mol
#
TOLERANCE = 5.0e-4


def get_input_files():
    inputs_dir = Path(__file__).parent / "inputs" / "rpt2"
    return sorted(inputs_dir.glob("*.inp"))


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
        pattern = r"^\s*" + re.escape(key) + r"\s+([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"
        match = re.search(pattern, text, re.MULTILINE)
        if match:
            energies[key] = float(match.group(1))
    return energies

@pytest.mark.parametrize("filepath", get_input_files(), ids=lambda p: p.stem)
def test_rpt2_energy(filepath: Path, record_property):
    print(f"\nTesting {filepath.name} ... ", end="", flush=True)
    
    try:
        ref_energies = extract_ref_energies(filepath)
        if not ref_energies:
            pytest.skip(f"No reference energies found in {filepath.name}.")
            
        ncores = utils.get_thread_count()
        result = subprocess.run([str(BIN_PATH), "-nt", str(ncores), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
        
        calc_energies = extract_calc_energies(result.stdout)
        
        for ref_key, ref_val in ref_energies.items():
            calc_key = KEY_MAP[ref_key]
            if calc_key in calc_energies:
                calc_val = calc_energies[calc_key]
                dev = abs(calc_val - ref_val)
                
                safe_key = calc_key.replace(" ", "_").replace("(", "_").replace(")", "")
                record_property(f"reference_{safe_key}", ref_val)
                record_property(f"calculated_{safe_key}", calc_val)
                record_property(f"deviation_{safe_key}", dev)
                
                assert calc_val == pytest.approx(ref_val, abs=TOLERANCE), f"{calc_key} deviation ({dev:.2e}) exceeds {TOLERANCE}"
            else:
                pytest.fail(f"{calc_key} mapped from {ref_key} found in reference but missing in calculated output.")
                
        print("PASSED")
    except Exception:
        print("FAILED")
        raise

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run rPT2 tests.")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (including slow avqz and trimer tests)")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use (default: physical cores)")
    args = parser.parse_args()
    
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    print(f"Tolerance for deviations in the interaction energy: {TOLERANCE:.1e} kcal/mol")
    
    print("\n" + "."*85)
    print(f"{'Property':<25} | {'Ref (kcal/mol)':>15} | {'Calc (kcal/mol)':>15} | {'Deviation':>12} | {'Status'}")
    print("." * 85)
    
    all_files = get_input_files()
    if not args.full:
        all_files = [f for f in all_files if utils.is_fast(f)]
        
    for filepath in all_files:
        print(f"Running {filepath.name}... ", end="", flush=True)
        start_time = time.time()
        
        ref_energies = extract_ref_energies(filepath)
        if not ref_energies:
            print("FAILED")
            print(f"{'N/A':<25} | {'N/A':>15} | {'N/A':>15} | {'N/A':>12} | NO REF ENERGIES")
            print("-" * 85)
            continue
            
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
                    status = "PASSED" if dev <= TOLERANCE else "FAILED"
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
