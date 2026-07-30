# Testing Guidelines

When writing, refactoring, or modifying tests for the `beyond-rpa` project, you must strictly adhere to the following architectural decisions:

## 1. Dual-Purpose Architecture
All Python test scripts must be designed to function simultaneously as an automated `pytest` suite and a manual standalone debugging script.
- **Automated Mode (via `pytest` / `meson test`)**: Tests should use standard `@pytest.mark.parametrize` decorators and should execute silently on success.
- **Standalone Mode (via `python script.py`)**: The script must include an `if __name__ == '__main__':` block. This block manually loops through the test cases, executes the external binary, and prints a detailed, tabular summary directly to standard output. The table must explicitly list Test Titles, Reference Values, Calculated Results, Deviations, and Pass/Fail Status.
- **Numerical Tolerance**: Tests should rely on `pytest.approx` for numerical tolerance checking. The exact numerical value of this tolerance must always be determined by the human programmer. An AI agent must not guess or set this value independently, and must prompt the user for the required tolerance when creating testing scripts.

## 2. CI/CD Structured Reporting
When running in `pytest` mode, you must use the `record_property` fixture within the test signature to log critical metrics (e.g., `record_property("deviation", dev)`). This guarantees that the generated JUnit XML files contain all numerical properties required for CI/CD visualization.

## 3. Pathlib Standard
Always utilize modern `pathlib.Path` objects for file I/O and path manipulation. Avoid legacy `os.path` and `glob` module constructs.
