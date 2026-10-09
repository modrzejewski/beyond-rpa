"""
Copy the PySCF reference energies into the preambles of the semicore THC inputs.

Reads every pyscf_*.txt file in this directory. Each file contains blocks
that start with "Input: <name>" and list "<key>: <value>" lines. The preamble
(leading comment and blank lines) of every <name>_*.inp is replaced
with the values of the block. The test category tag of the preamble
("! test category: fast" or "slow") is kept. Blocks without matching
inputs are skipped.

Usage: python inject_reference_preambles.py
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[3]))
import utils


def parse_blocks(text: str) -> dict[str, list[tuple[str, str]]]:
    blocks = {}
    name = None
    for line in text.splitlines():
        if line.startswith("Input:"):
            name = line.split(":", 1)[1].strip()
            blocks[name] = []
        elif name is not None and ":" in line:
            key, value = line.split(":", 1)
            blocks[name].append((key.strip(), value.strip()))
    return blocks


def strip_preamble(content: str) -> str:
    lines = content.splitlines()
    start_idx = len(lines)
    for i, line in enumerate(lines):
        if not (line.startswith("!") or not line.strip()):
            start_idx = i
            break
    return "\n".join(lines[start_idx:]) + "\n"


def main():
    inputs_dir = Path(__file__).parent
    for txt_file in sorted(inputs_dir.glob("pyscf_*.txt")):
        for name, entries in parse_blocks(txt_file.read_text()).items():
            inp_files = sorted(inputs_dir.glob(f"{name}_*.inp"))
            width = max(len(key) for key, _ in entries) + 2
            references = ["! reference values from pyscf"]
            references += [f"! {key + ':':<{width}}{value}" for key, value in entries]
            for inp_file in inp_files:
                content = inp_file.read_text()
                preamble = utils.preamble_tags(content) + references + [""]
                updated = "\n".join(preamble) + "\n" + strip_preamble(content)
                if updated == content:
                    print(f"Unchanged {inp_file.name}")
                    continue
                inp_file.write_text(updated)
                print(f"Updated {inp_file.name} with references from {txt_file.name}")


if __name__ == "__main__":
    main()
