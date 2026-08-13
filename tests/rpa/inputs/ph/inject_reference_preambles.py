import re
from pathlib import Path

KEYS_TO_CHECK = [
    "Eint(direct ring)",
    "Eint(SOSEX)",
    "Eint(3rd order ph)",
    "Eint(total)",
    "EintNadd(direct ring)",
    "EintNadd(SOSEX)",
    "EintNadd(3rd order ph)",
    "EintNadd(total)",
]

def extract_energies(text: str) -> dict:
    energies = {}
    for key in KEYS_TO_CHECK:
        pattern = r"^\s*" + re.escape(key) + r"\s+([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"
        match = re.search(pattern, text, re.MULTILINE)
        if match:
            energies[key] = match.group(1)
    return energies

def main():
    inputs_dir = Path(__file__).parent
    
    # 1. Group inputs by base system name
    systems = set()
    for inp_file in inputs_dir.glob("*.inp"):
        # Strip _accuracy_*
        base_name = re.sub(r"_accuracy_.*\.inp$", "", inp_file.name)
        systems.add(base_name)
        
    for base_name in sorted(systems):
        # 2. Find highest accuracy .txt available
        best_acc = None
        best_txt = None
        for acc in ["ludicrous", "tight", "default"]:
            txt_path = inputs_dir / f"{base_name}_accuracy_{acc}.txt"
            if txt_path.exists():
                best_acc = acc
                best_txt = txt_path
                break
                
        if not best_txt:
            continue
            
        # Extract energies
        with open(best_txt, 'r') as f:
            energies = extract_energies(f.read())
            
        if not energies:
            continue
            
        # Construct new preamble
        preamble_lines = []
        preamble_lines.append(f"! Reference values computed with version from 2025 with accuracy set to {best_acc}:")
        for key in KEYS_TO_CHECK:
            if key in energies:
                preamble_lines.append(f"! {key} = {energies[key]}")
        preamble_lines.append("")
        new_preamble = "\n".join(preamble_lines) + "\n"
        
        # 3. Update all .inp files for this system
        for inp_file in inputs_dir.glob(f"{base_name}_accuracy_*.inp"):
            with open(inp_file, 'r') as f:
                content = f.read()
                
            # Strip old preamble (all lines starting with ! or empty lines at the very beginning)
            lines = content.splitlines()
            start_idx = 0
            for i, line in enumerate(lines):
                if line.startswith('!') or not line.strip():
                    continue
                else:
                    start_idx = i
                    break
                    
            core_content = "\n".join(lines[start_idx:])
            if not core_content.endswith("\n"):
                core_content += "\n"
                
            # Write new content
            with open(inp_file, 'w') as f:
                f.write(new_preamble + core_content)
                
            print(f"Updated {inp_file.name} to use {best_acc} references.")

if __name__ == "__main__":
    main()
