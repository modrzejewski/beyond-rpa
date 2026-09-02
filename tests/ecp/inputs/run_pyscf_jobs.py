import os
import subprocess
import glob

def main():
    py_files = glob.glob("pyscf_*.py")
    py_files.sort()
    
    for py_file in py_files:
        print(f"Running {py_file}...")
        
        result = subprocess.run(["python", py_file], capture_output=True, text=True)
        
        if result.returncode != 0:
            print(f"Error running {py_file}:\n{result.stderr}")
            continue
            
        output_text = result.stdout
        
        # Base name for the corresponding beyond-rpa input file
        # e.g., pyscf_cd2_avdz-pp.py -> cd2_avdz-pp
        base_name = py_file.replace("pyscf_", "").replace(".py", "")
        
        txt_filename = f"{base_name}.txt"
        
        # Direct the output to the .txt file
        with open(txt_filename, "w") as f:
            f.write(output_text)
        print(f"  -> Saved output to {txt_filename}")
        
        for suffix in ["_accuracy_default.inp", "_accuracy_ludicrous.inp"]:
            inp_filename = f"{base_name}{suffix}"
            if not os.path.exists(inp_filename):
                print(f"  -> Warning: Corresponding beyond-rpa input file '{inp_filename}' not found.")
                continue
                
            with open(inp_filename, "r") as f:
                content = f.read()
                
            pasted_text = "! reference values from pyscf\n"
            for line in output_text.strip().split("\n"):
                line = line.strip()
                if line and ("(a.u.)" in line or "(kcal/mol)" in line):
                    pasted_text += f"! {line}\n"
            pasted_text += "\n"
            
            if "! reference values from pyscf" in content:
                lines = content.split('\n')
                new_lines = []
                in_ref_block = False
                for line in lines:
                    if line.startswith("! reference values from pyscf"):
                        in_ref_block = True
                        continue
                    if in_ref_block:
                        if line.startswith("!") or line.strip() == "":
                            continue
                        else:
                            in_ref_block = False
                            new_lines.append(line)
                    else:
                        new_lines.append(line)
                new_content = pasted_text + "\n".join(new_lines).lstrip("\n")
                with open(inp_filename, "w") as f:
                    f.write(new_content)
                print(f"  -> Replaced reference energies in {inp_filename}")
            else:
                new_content = pasted_text + content
                with open(inp_filename, "w") as f:
                    f.write(new_content)
                print(f"  -> Pasted reference energies into {inp_filename}")

if __name__ == "__main__":
    main()
