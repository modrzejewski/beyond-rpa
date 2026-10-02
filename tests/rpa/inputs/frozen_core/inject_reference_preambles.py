"""
Copy the PySCF reference energies into the preambles of the frozen-core inputs.

Reads every pyscf_*.txt file in this directory. Each file contains blocks
that start with "Input: <name>" and list "<key>: <value>" lines. The preamble
(leading comment and blank lines) of every <name>_accuracy_*.inp is replaced
with the values of the block. Inputs without a block, e.g., invalid_*.inp,
are not modified.

Usage: python inject_reference_preambles.py
"""

from pathlib import Path


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
            inp_files = sorted(inputs_dir.glob(f"{name}_accuracy_*.inp"))
            if not inp_files:
                print(f"No input file for block {name} in {txt_file.name}")
                continue
            width = max(len(key) for key, _ in entries) + 2
            preamble = ["! reference values from pyscf"]
            preamble += [f"! {key + ':':<{width}}{value}" for key, value in entries]
            preamble.append("")
            for inp_file in inp_files:
                content = inp_file.read_text()
                updated = "\n".join(preamble) + "\n" + strip_preamble(content)
                if updated == content:
                    print(f"Unchanged {inp_file.name}")
                    continue
                inp_file.write_text(updated)
                print(f"Updated {inp_file.name} with references from {txt_file.name}")


if __name__ == "__main__":
    main()
