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
        
        # ECP tests are unconditionally fast
        is_ecp = "test_ecp" in getattr(item, "nodeid", "")
        
        # Use shared logic to determine if it's slow
        if not is_ecp and not utils.is_fast(filepath):
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
