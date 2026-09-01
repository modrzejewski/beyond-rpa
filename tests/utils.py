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

def is_fast_ecp(filepath: Path) -> bool:
    """
    Determine if an ECP test is considered 'fast'.
    Slow ECP tests are those using the avtz-pp or avqz-pp basis set.
    """
    return "avtz-pp" not in filepath.name and "avqz-pp" not in filepath.name

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

def filter_test_files(all_files: list[Path], args) -> list[Path]:
    """
    Filters test files based on explicit boolean command-line flags.
    """
    if args.full:
        return all_files
        
    # Group the requested flags by category
    acc_flags = [f for f in ["accuracy_default", "accuracy_tight", "accuracy_ludicrous"] if getattr(args, f, False)]
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
            if is_fast(f):
                filtered_files.append(f)
                
    return filtered_files
