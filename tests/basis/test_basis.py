"""
Basis Set Test Suite for beyond-rpa.

This module functions simultaneously as an automated pytest suite and a manual standalone debugging script.

To run the test suite:
- Pytest Mode: `pytest tests/basis/test_basis.py`
- Standalone Mode: `python tests/basis/test_basis.py`
"""

import re
import subprocess
from pathlib import Path
import pytest

BIN_PATH = Path(__file__).parent.parent.parent / "bin" / "run"

def get_input_files():
    inputs_dir = Path(__file__).parent / "inputs"
    return list(inputs_dir.glob("*.inp"))

def get_reference_energy(filepath: Path) -> float:
    with filepath.open("r") as f:
        first_line = f.readline().strip()
    match = re.search(r"reference energy from pyscf:\s*([-+]?\d*\.\d+|\d+)", first_line)
    if match:
        return float(match.group(1))
    raise ValueError(f"Could not find reference energy in {filepath}")

def extract_energy_from_output(stdout: str) -> float:
    match = re.search(r"Converged energy\s+([-+]?\d*\.\d+|\d+)", stdout)
    if match:
        return float(match.group(1))
    raise ValueError("Could not find converged energy in output")

@pytest.mark.parametrize("filepath", get_input_files(), ids=lambda p: p.stem)
def test_basis_energy(filepath: Path, record_property):
    print(f"\nTesting {filepath.name} ... ", end="", flush=True)
    
    try:
        ref_energy = get_reference_energy(filepath)
        
        result = subprocess.run([str(BIN_PATH), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
        
        calc_energy = extract_energy_from_output(result.stdout)
        dev = abs(calc_energy - ref_energy)
        
        # Log properties to XML report for CI/CD
        record_property("reference", ref_energy)
        record_property("calculated", calc_energy)
        record_property("deviation", dev)
        
        assert calc_energy == pytest.approx(ref_energy, abs=1e-6)
        print("PASSED")
    except Exception:
        print("FAILED")
        raise

if __name__ == "__main__":
    print("\n" + "="*95)
    print(" BASIS TEST RESULTS SUMMARY ".center(95, "="))
    print("="*95)
    print(f"{'Test Title':<45} | {'Reference':<15} | {'Result':<15} | {'Deviation':<12} | {'Status'}")
    print("-" * 95)
    
    for filepath in get_input_files():
        ref_energy = get_reference_energy(filepath)
        calc_energy = None
        dev = None
        status = "FAILED"
        
        try:
            result = subprocess.run([str(BIN_PATH), str(filepath)], capture_output=True, text=True)
            if result.returncode == 0:
                calc_energy = extract_energy_from_output(result.stdout)
                dev = abs(calc_energy - ref_energy)
                if dev <= 1e-6:
                    status = "PASSED"
        except Exception as e:
            pass
            
        dev_str = f"{dev:.2e}" if isinstance(dev, float) else "N/A"
        calc_str = f"{calc_energy:.8f}" if isinstance(calc_energy, float) else "N/A"
        ref_str = f"{ref_energy:.8f}" if isinstance(ref_energy, float) else "N/A"
        
        print(f"{filepath.name:<45} | {ref_str:<15} | {calc_str:<15} | {dev_str:<12} | {status}")
    
    print("="*95 + "\n")
