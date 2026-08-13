import glob
import re
from pathlib import Path

def extract_energies(lines):
    keys = ["Eint(direct ring)", "Eint(total)", "EintNadd(direct ring)", "EintNadd(total)"]
    energies = {}
    for line in lines:
        for key in keys:
            if line.strip().startswith(key):
                pattern = r"^\s*" + re.escape(key) + r"\s+([-+]?\d*\.\d+[Ee][-+]?\d+|[-+]?\d*\.\d+)"
                match = re.search(pattern, line)
                if match:
                    energies[key] = float(match.group(1))
    return energies

def generate_inputs():
    script_dir = Path(__file__).parent
    for txt_path in script_dir.glob("*.txt"):
        inp_path = txt_path.with_suffix('.inp')
        with open(txt_path, 'r') as f:
            lines = f.readlines()
            
        energies = extract_energies(lines)
        
        extracted_lines = [
            "! Reference values computed with version from 2024 with accuracy set to ludicrous:"
        ]
        if "Eint(direct ring)" in energies:
            extracted_lines.append(f"! Eint(direct ring) = {energies['Eint(direct ring)']}")
        if "Eint(total)" in energies:
            extracted_lines.append(f"! Eint(total) = {energies['Eint(total)']}")
        if "EintNadd(direct ring)" in energies:
            extracted_lines.append(f"! EintNadd(direct ring) = {energies['EintNadd(direct ring)']}")
        if "EintNadd(total)" in energies:
            extracted_lines.append(f"! EintNadd(total) = {energies['EintNadd(total)']}")
        extracted_lines.append("")
        
        in_input = False
        for line in lines:
            if line.startswith(" > "):
                in_input = True
                content = line[3:].strip()
                
                # Modernization rules
                if content == "rpa":
                    extracted_lines.append("rpa")
                    extracted_lines.append("accuracy default")
                elif content == "rPT2":
                    extracted_lines.append("TheoryLevel rPT2")
                elif content.startswith("accuracy ") or \
                     content.startswith("thresh density") or \
                     content.startswith("charges ") or \
                     content.startswith("jobtype "):
                    continue # Skip deprecated/replaced lines
                else:
                    extracted_lines.append(content)
            elif in_input and "---" in line and len(extracted_lines) > 5:
                break # Reached the end of the input block

        with open(inp_path, 'w') as f:
            f.write("\n".join(extracted_lines) + "\n")

if __name__ == "__main__":
    generate_inputs()
