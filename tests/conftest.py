import pytest
import utils

def pytest_addoption(parser):
    """Register custom command line flags."""
    parser.addoption(
        "--full",
        action="store_true",
        default=False,
        help="Run the full test suite (including ludicrous and slow tests)"
    )

def pytest_collection_modifyitems(config, items):
    """Dynamically assign markers based on the test name/ID."""
    run_full = config.getoption("--full")
    skip_slow = pytest.mark.skip(reason="requires --full option")
    errors = []

    for item in items:
        test_id = getattr(item.callspec, "id", "") if hasattr(item, "callspec") else ""
        params = item.callspec.params if hasattr(item, "callspec") else {}

        # Fast or slow: the test category tag in the preamble of the input
        if "filepath" not in params:
            errors.append(f"{item.nodeid}: parametrize the test with its input file as \"filepath\"")
            continue
        try:
            is_fast_test = utils.is_fast_test(params["filepath"])
        except ValueError as error:
            errors.append(str(error))
            continue

        if not is_fast_test:
            item.add_marker(pytest.mark.slow)
            if not run_full:
                item.add_marker(skip_slow)

        # 1. Apply Accuracy Markers
        if "accuracy_default" in test_id: item.add_marker(pytest.mark.accuracy_default)
        if "accuracy_ludicrous" in test_id: item.add_marker(pytest.mark.accuracy_ludicrous)

        # 2. Apply Basis Set Markers
        if "avdz" in test_id: item.add_marker(pytest.mark.avdz)
        if "avtz" in test_id: item.add_marker(pytest.mark.avtz)
        if "avqz" in test_id: item.add_marker(pytest.mark.avqz)

        # 3. Apply System Size Markers
        if "dimer" in test_id: item.add_marker(pytest.mark.dimer)
        if "trimer" in test_id: item.add_marker(pytest.mark.trimer)

    if errors:
        raise pytest.UsageError("\n".join(dict.fromkeys(errors)))
