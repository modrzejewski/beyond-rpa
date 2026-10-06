"""Run the PySCF scripts and write their reference energies into the inputs.

Each pyscf_*.py prints lines "<input name>: <energy>". The first line of
<input name>.inp becomes "! reference energy from pyscf: <energy>".
"""
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
PREAMBLE = "! reference energy from pyscf:"


def references(script: Path) -> list[tuple[str, str]]:
    """Return (input name, energy) pairs printed by a PySCF script."""
    result = subprocess.run(
        [sys.executable, str(script)],
        capture_output=True,
        text=True,
        check=True,
        cwd=HERE,
    )
    return re.findall(r"^(\S+): ([-+]?\d+\.\d+)$", result.stdout, flags=re.M)


def write_preamble(inp: Path, energy: str) -> None:
    """Replace or add the reference line at the top of an input file."""
    lines = inp.read_text().splitlines()
    if lines and lines[0].startswith(PREAMBLE):
        lines = lines[1:]
    inp.write_text("\n".join([f"{PREAMBLE} {energy}"] + lines) + "\n")


if __name__ == "__main__":
    for script in sorted(HERE.glob("pyscf_*.py")):
        print(f"Running {script.name}...", flush=True)
        for name, energy in references(script):
            write_preamble(HERE / f"{name}.inp", energy)
            print(f"  {name}.inp: {energy}")
