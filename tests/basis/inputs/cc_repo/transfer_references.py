"""Run the PySCF scripts and write their reference energies into the inputs.

Each pyscf_*.py prints lines "<input name>: <energy>". The line
"! reference energy from pyscf: <energy>" follows the test category tag
at the top of <input name>.inp.
"""
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
PREAMBLE = "! reference energy from pyscf:"

sys.path.insert(0, str(HERE.parents[2]))
import utils


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
    """Replace or add the reference line below the test category tag."""
    content = inp.read_text()
    tags = utils.preamble_tags(content)
    n_preamble = len(utils.preamble_lines(content))
    lines = content.splitlines()
    preamble = [
        line for line in lines[:n_preamble]
        if line not in tags and not line.startswith(PREAMBLE)
    ]
    inp.write_text("\n".join(tags + [f"{PREAMBLE} {energy}"] + preamble + lines[n_preamble:]) + "\n")


if __name__ == "__main__":
    for script in sorted(HERE.glob("pyscf_*.py")):
        print(f"Running {script.name}...", flush=True)
        for name, energy in references(script):
            write_preamble(HERE / f"{name}.inp", energy)
            print(f"  {name}.inp: {energy}")
