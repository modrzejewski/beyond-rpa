import sys
import shutil
from pathlib import Path
from datetime import datetime, timezone

SCRIPT_DIR = Path(__file__).resolve().parent
sys.path.append(str(SCRIPT_DIR / '../../src'))
import LauncherScript


def main():
    if len(sys.argv) < 7:
        print("Usage: post_build.py <exe> <bin_dir> <launcher> <intel_caf> <compiler_cmd> <dummy_out>")
        sys.exit(1)

    input_exe = Path(sys.argv[1])
    bin_dir = Path(sys.argv[2])
    launcher_cmd = sys.argv[3]
    intel_caf_config = sys.argv[4]
    compiler_cmd = sys.argv[5]
    dummy_out = Path(sys.argv[6])

    bin_dir.mkdir(parents=True, exist_ok=True)

    # 1. Copy executable
    exe_dest = bin_dir / 'a'
    shutil.copy2(input_exe, exe_dest)
    print(f"Copied executable to {exe_dest}")

    # 2. Generate launcher script
    run_file = bin_dir / 'run'
    build_date = datetime.now(timezone.utc).strftime("%a %b %d %H:%M:%S UTC %Y")
    LauncherScript.make_runscript(
        str(run_file),
        compiler_cmd,
        build_date,
        CAFLauncher=launcher_cmd,
        IntelCAFConfig=intel_caf_config,
    )
    print(f"Generated {run_file}")

    # Touch dummy output for Meson custom_target
    dummy_out.write_text("done")


if __name__ == "__main__":
    main()
