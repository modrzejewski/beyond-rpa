import subprocess
from pathlib import Path

SCRIPT_DIRECTORY = Path(__file__).parent
EXECUTABLE_PATH = SCRIPT_DIRECTORY.parent.parent.parent.parent / "bin" / "run"

def run_calculations():
    """Execute calculations for all input files."""
    for input_path in SCRIPT_DIRECTORY.glob("*.inp"):
        output_path = input_path.with_suffix(".txt")
        command = [
            str(EXECUTABLE_PATH),
            "-nt",
            "4",
            str(input_path.name)
        ]
        with open(output_path, "w") as output_stream:
            subprocess.run(command, stdout=output_stream, check=True, cwd=SCRIPT_DIRECTORY)

if __name__ == "__main__":
    run_calculations()

