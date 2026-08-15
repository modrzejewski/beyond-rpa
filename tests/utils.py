import os
from pathlib import Path

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

def is_fast(filepath: Path) -> bool:
    """
    Determine if a test is considered 'fast' (default behavior).
    Fast tests are dimers with avtz or avdz basis, and default accuracy.
    """
    name = filepath.name
    is_dimer = "dimer" in name
    is_fast_basis = "avtz" in name or "avdz" in name
    # If the filename contains accuracy information, require it to be default
    is_fast_acc = "accuracy_default" in name if "accuracy" in name else True 
    
    return is_dimer and is_fast_basis and is_fast_acc
