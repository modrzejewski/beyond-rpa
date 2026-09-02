"""
Point Charges (QM/MM) Test Suite for beyond-rpa.

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

BIN_PATH = Path(__file__).parents[2] / "bin" / "run"

INPUTS_DIR = Path(__file__).parent / "inputs"

TEST_CASES = [
    {
        "input_path": INPUTS_DIR / "water_dimer_mp2_avdz_16_charges_sp.inp",
        "property": "single_point"
    },
    {
        "input_path": INPUTS_DIR / "water_dimer_mp2_avdz_16_charges.inp",
        "property": "interaction"
    },
    {
        "input_path": INPUTS_DIR / "water_dimer_mp2_avdz_10000_charges.inp",
        "property": "interaction"
    }
]

REF_KEYS_SP = [
    "reference HF single point energy of AB (in a.u.)",
    "reference MP2 correlation energy of AB (in a.u.)",
]

REF_KEYS_INT = [
    "HF interaction energy of A...B (in kcal/mol)",
    "MP2 correlation interaction energy of A...B (in kcal/mol)",
]

REF_KEYS = REF_KEYS_SP + REF_KEYS_INT

CALC_KEYS_SP = [
    "E(HF)",
    "E(total MP2)",
]

CALC_KEYS_INT = [
    "Eint(HF)",
    "Eint(total MP2)",
]

CALC_KEYS = CALC_KEYS_SP + CALC_KEYS_INT

KEY_MAP = dict(zip(REF_KEYS, CALC_KEYS))

#
# Tolerances
#
TOLERANCE_INTERACTION = 5.0e-5  # kcal/mol
TOLERANCE_SINGLE_POINT = 5.0e-6 # a.u.

FLOAT_REGEX = r"([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"

def extract_ref_energies(filepath: Path, property_type: str) -> dict:
    energies = {}
    valid_keys = REF_KEYS_INT if property_type == "interaction" else REF_KEYS_SP
    with open(filepath, 'r') as f:
        for line in f:
            if not line.startswith('!'):
                if line.strip():
                    break
                continue
            for ref_key in valid_keys:
                pattern = r"!\s*" + re.escape(ref_key) + r"\s*:\s*" + FLOAT_REGEX
                match = re.search(pattern, line)
                if match:
                    energies[ref_key] = float(match.group(1))
    return energies

def extract_calc_energies(text: str, property_type: str) -> dict:
    energies = {}
    valid_keys = CALC_KEYS_INT if property_type == "interaction" else CALC_KEYS_SP
    
    for calc_key in valid_keys:
        pattern = r"^\s*" + re.escape(calc_key) + r"\s+" + FLOAT_REGEX
        match = re.search(pattern, text, re.MULTILINE)
        if match:
            energies[calc_key] = float(match.group(1))
                
    return energies

def get_tolerance(calc_key: str) -> float:
    if "Eint" in calc_key:
        return TOLERANCE_INTERACTION
    return TOLERANCE_SINGLE_POINT

def get_unit(calc_key: str) -> str:
    if "Eint" in calc_key:
        return "kcal/mol"
    return "a.u."

def print_results_table(results: list[dict]):
    for res in results:
        calc_key = res.get("calculated_key", "N/A")
        unit = res.get("unit", "N/A")
        
        ref_val = res.get("reference_value")
        ref_str = f"{ref_val:.6f}" if ref_val is not None else "N/A"
        
        calc_val = res.get("calculated_value")
        calc_str = f"{calc_val:.6f}" if calc_val is not None else "N/A"
        
        dev = res.get("deviation")
        dev_str = f"{dev:.2e}" if dev is not None else "N/A"
        
        status = res.get("status", "FAILED")
        
        print(f"{calc_key:<25} | {unit:<10} | {ref_str:>15} | {calc_str:>15} | {dev_str:>12} | {status}")

@pytest.mark.parametrize("test_case", TEST_CASES, ids=lambda tc: tc["input_path"].stem)
def test_point_charges(test_case: dict, record_property):
    filepath = test_case["input_path"]
    prop = test_case["property"]
    
    print(f"\nTesting {filepath.name} ({prop}) ... ", end="", flush=True)
    
    try:
        ref_energies = extract_ref_energies(filepath, prop)
        if not ref_energies:
            pytest.skip(f"No reference energies found in {filepath.name} for {prop}.")
            
        ncores = utils.get_thread_count()
        result = subprocess.run([str(BIN_PATH), "-nt", str(ncores), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
        
        calc_energies = extract_calc_energies(result.stdout, prop)
        
        for ref_key, ref_val in ref_energies.items():
            calc_key = KEY_MAP[ref_key]
            if calc_key in calc_energies:
                calc_val = calc_energies[calc_key]
                dev = abs(calc_val - ref_val)
                tolerance = get_tolerance(calc_key)
                
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

def _display_tolerances():
    print("\nTolerances:")
    print(f"  interaction energy:  {TOLERANCE_INTERACTION:.1e} kcal/mol")
    print(f"  single point energy: {TOLERANCE_SINGLE_POINT:.1e} a.u.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run QM/MM Point Charges tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use (default: physical cores)")
    
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()

    print(f"\nNumber of threads: {nthreads}")
    
    _display_tolerances()
    
    print("\n" + "." * 100)
    print(f"{'Property':<25} | {'Unit':<10} | {'Ref':>15} | {'Calc':>15} | {'Deviation':>12} | {'Status'}")
    print("." * 100)
    
    for tc in TEST_CASES:
        filepath = tc["input_path"]
        prop = tc["property"]
        print(f"Running {filepath.name} ({prop})... ", end="", flush=True)
        start_time = time.time()
        
        ref_energies = extract_ref_energies(filepath, prop)
        if not ref_energies:
            print("FAILED")
            print_results_table([{
                "calculated_key": "N/A",
                "status": "NO REF ENERGIES"
            }])
            print("-" * 100)
            continue
        
        try:
            result = subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)
            if result.returncode != 0:
                elapsed = time.time() - start_time
                print(f"FAILED ({elapsed:.2f}s)")
                print_results_table([{
                    "calculated_key": "N/A",
                    "status": "CRASHED"
                }])
                print("-" * 100)
                continue
                
            elapsed = time.time() - start_time
            print(f"done ({elapsed:.2f}s)")
            calc_energies = extract_calc_energies(result.stdout, prop)
            
            results = []
            for ref_key, ref_val in ref_energies.items():
                calc_key = KEY_MAP[ref_key]
                calc_val = calc_energies.get(calc_key)
                unit = get_unit(calc_key)
                tolerance = get_tolerance(calc_key)
                
                row = {
                    "calculated_key": calc_key,
                    "unit": unit,
                    "reference_value": ref_val,
                }
                
                if calc_val is not None:
                    dev = abs(calc_val - ref_val)
                    status = "PASSED" if dev <= tolerance else "FAILED"
                    row.update({"calculated_value": calc_val, "deviation": dev, "status": status})
                else:
                    row.update({"status": "FAILED"})
                    
                results.append(row)
                
            print_results_table(results)
                
        except Exception as e:
            elapsed = time.time() - start_time
            print(f"FAILED ({elapsed:.2f}s)")
            print_results_table([{
                "calculated_key": "N/A",
                "status": "ERROR"
            }])
            
        print("-" * 100)
        
    print("\n")
