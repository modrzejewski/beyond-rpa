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

@pytest.mark.parametrize("filepath", get_input_files(), ids=lambda p: p.name)
def test_basis_energy(filepath: Path):
    print(f"\nTesting {filepath.name} ... ", end="", flush=True)
    try:
        ref_energy = get_reference_energy(filepath)
        
        result = subprocess.run([str(BIN_PATH), str(filepath)], capture_output=True, text=True)
        assert result.returncode == 0, f"beyond-rpa failed:\n{result.stderr}"
        
        calc_energy = extract_energy_from_output(result.stdout)
        assert calc_energy == pytest.approx(ref_energy, abs=1e-6)
        print("PASSED")
    except Exception:
        print("FAILED")
        raise
