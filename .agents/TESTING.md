# Testing Guidelines

When writing, refactoring, or modifying tests for the `beyond-rpa` project, you must strictly adhere to the following architectural decisions:

## 1. Dual-Purpose Architecture
All Python test scripts must be designed to function simultaneously as an automated `pytest` suite and a manual standalone debugging script.
- **Automated Mode (via `pytest` / `meson test`)**: Tests should use standard `@pytest.mark.parametrize` decorators and should execute silently on success.
- **Standalone Mode (via `python script.py`)**: The script must include an `if __name__ == '__main__':` block. This block manually loops through the test cases, executes the external binary, and prints a detailed, tabular summary directly to standard output. The table columns are the computed quantity, Reference Value, Calculated Result, Deviation, and Pass/Fail Status.
  - **Progress Indication**: To provide immediate feedback, the script must print `Running <test title>...` *before* executing the binary. Once the binary finishes, it must print the elapsed time (e.g. `done (3.42s)`) and *only then* print the corresponding table rows for that test.
  - **Table Layout**: The table has no test-title column. The `Running <test title>...` line names the test whose rows follow, which keeps the table narrow. Compute the table width from the header string, print a dotted line (`"." * width`) above and below the header, and print a dashed line (`"-" * width`) after the rows of each test. Example: `tests/rpa/test_frozen_core.py`.
  - **Tolerances**: Before the table, print every tolerance with its unit, for each test category and computed quantity. Example: `tests/integrals/test_semicore_thc.py`.
- **Numerical Tolerance**: Tests should rely on `pytest.approx` for numerical tolerance checking. The exact numerical value of this tolerance must always be determined by the human programmer. An AI agent must not guess or set this value independently, and must prompt the user for the required tolerance when creating testing scripts.

## 2. CI/CD Structured Reporting
When running in `pytest` mode, you must use the `record_property` fixture within the test signature to log critical metrics (e.g., `record_property("deviation", dev)`). This guarantees that the generated JUnit XML files contain all numerical properties required for CI/CD visualization.

## 3. Pathlib Standard
Always utilize modern `pathlib.Path` objects for file I/O and path manipulation. Avoid legacy `os.path` and `glob` module constructs.

## 4. Test Categorization (Fast vs Slow)
Computationally expensive tests (e.g., those using large basis sets like `avqz` or testing large molecular complexes like trimers) must be categorized as "slow" and skipped by default to ensure the standard test suite remains snappy.
- **Category Tag**: The developer declares the category in the preamble (the leading comment and blank lines) of each input with exactly one line `! test category: fast` or `! test category: slow`. A missing, repeated, or invalid tag is an error at test collection.
- **Implementation**: `is_fast_test()` in `tests/utils.py` reads the tag. Test modules do not define their own categorization rules.
- **Pytest Mode**: Parametrize each test with its input file as `filepath`. `tests/conftest.py` calls `utils.is_fast_test()` and skips slow tests unless pytest runs with `--full`.
- **Standalone Mode**: The standalone script must require an explicit flag (`--full` via `argparse`) to execute the slow tests. It selects the inputs with `utils.is_fast_test()`.
- **Input Writers**: Scripts that write or rewrite inputs, e.g., reference injectors, must keep the tag. `utils.preamble_tags()` returns the tag lines of an input.

## 5. Registration in Meson
Every pytest module (`tests/**/test_*.py`) must be registered in `meson.build`, so that `meson test` and `pytest` run the same tests.
- **Entry**: Add a `test()` call inside the `if pytest.found()` block. It runs `pytest` on the module with `-v`, `-s`, and `--junitxml=<suite>_report.xml`, and sets `depends: post_build_target` and `workdir: meson.project_source_root()`.
- **Timeout**: Set `timeout` from the measured runtime of the tests that run by default, with a safety margin.
- **Verification**: `meson test -C <builddir> --list` must show one entry per pytest module, and `pytest --collect-only` from the project root must collect the same tests as the registered modules.
