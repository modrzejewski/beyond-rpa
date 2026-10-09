import os
from pathlib import Path

TEST_CATEGORY_TAG = "! test category:"


def preamble_lines(content: str) -> list[str]:
    """
    Return the leading comment and blank lines of an input.
    """
    lines = []
    for line in content.splitlines():
        if not (line.startswith("!") or not line.strip()):
            break
        lines.append(line)
    return lines


def preamble_tags(content: str) -> list[str]:
    """
    Return the test category tags in the preamble of an input.
    """
    return [line for line in preamble_lines(content) if line.startswith(TEST_CATEGORY_TAG)]


def is_fast_test(filepath: Path) -> bool:
    """
    Read the tag "! test category: fast" or "slow" in the input preamble.
    The developer declares the category of each input. A missing,
    repeated, or invalid tag is an error.
    """
    categories = [tag.split(":", 1)[1].strip() for tag in preamble_tags(filepath.read_text())]
    if len(categories) != 1 or categories[0] not in ("fast", "slow"):
        raise ValueError(
            f"{filepath}: declare the test category in the input preamble with exactly one line "
            f"\"{TEST_CATEGORY_TAG} fast\" or \"{TEST_CATEGORY_TAG} slow\""
        )
    return categories[0] == "fast"


def get_thread_count() -> int:
    """
    Returns the number of threads to use for tests.
    Prioritizes the OMP_NUM_THREADS environment variable.
    Falls back to the number of physical CPU cores if the variable is not set.
    """
    # 1. Check environment variable first
    omp_env = os.environ.get("OMP_NUM_THREADS")
    if omp_env is not None:
        try:
            return int(omp_env)
        except ValueError:
            pass # Fall back if invalid
            
    # 2. Fall back to physical cores
    try:
        cores = len(os.sched_getaffinity(0))
    except AttributeError:
        cores = os.cpu_count() or 1
        
    return cores

def filter_test_files(all_files: list[Path], args) -> list[Path]:
    """
    Filters test files based on explicit boolean command-line flags.
    """
    if args.full:
        return all_files
        
    # Group the requested flags by category
    acc_flags = [f for f in ["accuracy_default", "accuracy_ludicrous"] if getattr(args, f, False)]
    basis_flags = [f for f in ["avdz", "avtz", "avqz"] if getattr(args, f, False)]
    size_flags = [f for f in ["dimer", "trimer"] if getattr(args, f, False)]
    
    has_explicit_filter = bool(acc_flags or basis_flags or size_flags)
    
    filtered_files = []
    for f in all_files:
        name = f.name
        
        if has_explicit_filter:
            match = True
            # Enforce OR within categories, AND across categories
            if acc_flags and not any(a in name for a in acc_flags): match = False
            if basis_flags and not any(b in name for b in basis_flags): match = False
            if size_flags and not any(s in name for s in size_flags): match = False
            
            if match:
                filtered_files.append(f)
        else:
            # Fallback to default 'fast' behavior
            if is_fast_test(f):
                filtered_files.append(f)
                
    return filtered_files
