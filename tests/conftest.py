import pytest
from pathlib import Path
import utils

def pytest_addoption(parser):
    """Register custom command line flags."""
    parser.addoption(
        "--full", 
        action="store_true", 
        default=False, 
        help="Run the full test suite (including tight, ludicrous, and slow tests)"
    )

def pytest_collection_modifyitems(config, items):
    """Dynamically assign markers based on the test name/ID."""
    run_full = config.getoption("--full")
    skip_slow = pytest.mark.skip(reason="requires --full option")
    
    for item in items:
        test_id = getattr(item.callspec, "id", "") if hasattr(item, "callspec") else ""
        filepath = Path(test_id)
        
        # ECP tests use a specific fast/slow helper
        is_ecp = "test_ecp" in getattr(item, "nodeid", "")

        # Quadrupole integral tests are single-molecule SCF calculations, always fast
        is_quadrupole = "test_quadrupole" in getattr(item, "nodeid", "")

        # Frozen core tests are single-molecule and small-dimer calculations, always fast
        is_frozen_core = "test_frozen_core" in getattr(item, "nodeid", "")

        # Basis assignment tests are single-molecule SCF calculations, always fast
        is_basis = "test_basis" in getattr(item, "nodeid", "")

        # Semicore tests: the module's own rule, based on the input file name
        is_semicore = "test_semicore" in getattr(item, "nodeid", "")

        # Use shared logic to determine if it's slow
        if is_semicore:
            is_fast_test = item.module.is_fast_test(item.callspec.params["filepath"])
        elif is_ecp:
            is_fast_test = utils.is_fast_ecp(filepath)
        elif is_quadrupole or is_frozen_core or is_basis:
            is_fast_test = True
        else:
            is_fast_test = utils.is_fast(filepath)
            
        if not is_fast_test:
            item.add_marker(pytest.mark.slow)
            if not run_full:
                item.add_marker(skip_slow)
        
        # 1. Apply Accuracy Markers
        if "accuracy_default" in test_id: item.add_marker(pytest.mark.accuracy_default)
        if "accuracy_tight" in test_id: item.add_marker(pytest.mark.accuracy_tight)
        if "accuracy_ludicrous" in test_id: item.add_marker(pytest.mark.accuracy_ludicrous)
                
        # 2. Apply Basis Set Markers
        if "avdz" in test_id: item.add_marker(pytest.mark.avdz)
        if "avtz" in test_id: item.add_marker(pytest.mark.avtz)
        if "avqz" in test_id: item.add_marker(pytest.mark.avqz)

        # 3. Apply System Size Markers
        if "dimer" in test_id: item.add_marker(pytest.mark.dimer)
        if "trimer" in test_id: item.add_marker(pytest.mark.trimer)
