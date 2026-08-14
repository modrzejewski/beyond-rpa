import os

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
