"""
ECP Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.
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

TOLERANCE_INTERACTION_DEFAULT = 5.0e-4  # kcal/mol
TOLERANCE_INTERACTION_LUDICROUS = 5.0e-5  # kcal/mol

CALC_KEY_HF = "Eint(HF)"
CALC_KEY_1RDM_LIN = "Eint(1-RDM linear)"
CALC_KEY_1RDM_QUAD = "Eint(1-RDM quadratic)"
CALC_KEY_DRPA = "Eint(direct ring)"

def get_input_files():
    files = list(INPUTS_DIR.glob("*.inp"))
    files.sort()
    return files

def extract_ref_energies(filepath: Path) -> dict:
    energies = {}
    with open(filepath, 'r') as f:
        for line in f:
            if "! HF interaction A...B (kcal/mol):" in line:
                energies["HF"] = float(line.split(":")[1].strip())
            elif "! dRPA interaction A...B (kcal/mol):" in line:
                energies["dRPA"] = float(line.split(":")[1].strip())
    return energies

FLOAT_REGEX = r"([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"

def extract_calc_energies(text: str) -> dict:
    energies = {}
    
    # HF component
    match_hf = re.search(r"^\s*" + re.escape(CALC_KEY_HF) + r"\s+" + FLOAT_REGEX, text, re.MULTILINE)
    match_1rdm_lin = re.search(r"^\s*" + re.escape(CALC_KEY_1RDM_LIN) + r"\s+" + FLOAT_REGEX, text, re.MULTILINE)
    match_1rdm_quad = re.search(r"^\s*" + re.escape(CALC_KEY_1RDM_QUAD) + r"\s+" + FLOAT_REGEX, text, re.MULTILINE)
    
    if match_hf and match_1rdm_lin and match_1rdm_quad:
        e_hf = float(match_hf.group(1))
        e_1rdm_lin = float(match_1rdm_lin.group(1))
        e_1rdm_quad = float(match_1rdm_quad.group(1))
        energies["HF"] = e_hf + e_1rdm_lin + e_1rdm_quad
        
    # dRPA component
    match_drpa = re.search(r"^\s*" + re.escape(CALC_KEY_DRPA) + r"\s+" + FLOAT_REGEX, text, re.MULTILINE)
    if match_drpa:
        energies["dRPA"] = float(match_drpa.group(1))
        
    return energies

def get_tolerance(filepath: Path) -> float:
    if "_ludicrous" in filepath.name:
        return TOLERANCE_INTERACTION_LUDICROUS
    return TOLERANCE_INTERACTION_DEFAULT

@pytest.mark.parametrize("filepath", get_input_files(), ids=lambda p: str(p.relative_to(TESTS_DIR)))
def test_ecp(filepath: Path, record_property):
    print(f"\nTesting {filepath.name} ... ", end="", flush=True)
    
    try:
        ref_energies = extract_ref_energies(filepath)
        if "HF" not in ref_energies or "dRPA" not in ref_energies:
            pytest.skip(f"Reference energies missing in {filepath.name}")
            
        ncores = utils.get_thread_count()
        result = subprocess.run([str(BIN_PATH), "-nt", str(ncores), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
        
        calc_energies = extract_calc_energies(result.stdout)
        
        tol = get_tolerance(filepath)
        
        for key in ["HF", "dRPA"]:
            assert key in calc_energies, f"Calculated energy for {key} missing from output."
            dev = abs(calc_energies[key] - ref_energies[key])
            
            record_property(f"reference_{key}", ref_energies[key])
            record_property(f"calculated_{key}", calc_energies[key])
            record_property(f"deviation_{key}", dev)
            
            assert calc_energies[key] == pytest.approx(ref_energies[key], abs=tol), f"{key} deviation ({dev:.2e}) exceeds {tol}"
            
        print("PASSED")
    except Exception:
        print("FAILED")
        raise

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Run ECP tests.")
    parser.add_argument("-nt", "--nthreads", type=int, default=None, help="Number of OpenMP threads to use")
    parser.add_argument("--full", action="store_true", help="Run the full test suite (including slow tests)")
    args = parser.parse_args()
    nthreads = args.nthreads if args.nthreads is not None else utils.get_thread_count()
    
    print(f"\nNumber of threads: {nthreads}")
    print("\n" + "." * 105)
    print(f"{'Test File':<35} | {'Term':<4} | {'Ref (kcal/mol)':>15} | {'Calc (kcal/mol)':>15} | {'Deviation':>12} | {'Status'}")
    print("." * 105)
    
    all_files = get_input_files()
    if not args.full:
        files = [f for f in all_files if utils.is_fast_ecp(f)]
        if len(files) < len(all_files):
            print(f"Running {len(files)} fast tests. Use --full to run all {len(all_files)} tests.")
            print("-" * 105)
    else:
        files = all_files
        
    for filepath in files:
        ref_energies = extract_ref_energies(filepath)
        if not ref_energies:
            continue
            
        print(f"Running {filepath.name}... ", end="", flush=True)
        start_time = time.time()
        
        try:
            result = subprocess.run([str(BIN_PATH), "-nt", str(nthreads), str(filepath)], capture_output=True, text=True)
            elapsed = time.time() - start_time
            if result.returncode != 0:
                print(f"FAILED ({elapsed:.2f}s)")
                print(f"{'':<35} | {'N/A':<4} | {'N/A':>15} | {'N/A':>15} | {'N/A':>12} | CRASHED")
                print("-" * 105)
                continue
                
            print(f"done ({elapsed:.2f}s)")
            calc_energies = extract_calc_energies(result.stdout)
            tol = get_tolerance(filepath)
            
            for key in ["HF", "dRPA"]:
                ref_val = ref_energies.get(key)
                calc_val = calc_energies.get(key)
                
                if ref_val is None or calc_val is None:
                    status = "ERROR"
                    dev_str = "N/A"
                    ref_str = "N/A"
                    calc_str = "N/A"
                else:
                    dev = abs(calc_val - ref_val)
                    status = "PASSED" if dev <= tol else "FAILED"
                    dev_str = f"{dev:.2e}"
                    ref_str = f"{ref_val:.6f}"
                    calc_str = f"{calc_val:.6f}"
                    
                prefix = filepath.name if key == "HF" else ""
                print(f"{prefix:<35} | {key:<4} | {ref_str:>15} | {calc_str:>15} | {dev_str:>12} | {status}")
                
        except Exception:
            elapsed = time.time() - start_time
            print(f"FAILED ({elapsed:.2f}s)")
            print(f"{'':<35} | {'N/A':<4} | {'N/A':>15} | {'N/A':>15} | {'N/A':>12} | ERROR")
            
        print("-" * 105)
    print("\n")
