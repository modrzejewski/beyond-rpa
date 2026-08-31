# Coding Style Guidelines

## Blank Spaces

* **Functions and Subroutines:** Prefer two lines of blank space between functions and subroutines defined at the module level (not inside other functions or subroutines).

## Naming Conventions

* **Naming:** Function and variable names must correspond to their physical meaning. Avoid unnecessary words (e.g., "data", "container").
* **CamelCase:** Variables representing counts, sizes, or quantities should be prefixed with an uppercase `N` followed by CamelCase (e.g., `NAtoms`, `NShells`, `NConfigs`).
* **Types:** Types and Objects are prefixed with an uppercase `T` (e.g., `TBasisAssignment`, `TSCFParams`).

## Fortran Coding Style

* **Line Length:** Avoid excessively long lines (prefer keeping them narrow around 80 characters, though they can exceed a little bit). For long function or subroutine calls, break them elegantly into multiple lines every few arguments using the `&` continuation character.
* **Alignment:** When declaring arguments in a subroutine or function signature, the `::` symbols must ALWAYS be strictly aligned under a single, well-spaced column for all arguments, padding shorter type definitions with spaces. This rule does NOT apply to local variables declared in the function body.
* **Output Arguments:** Output arguments (`intent(out)`) should generally appear as the first arguments in a subroutine signature, followed by `intent(inout)` and `intent(in)`.
* **Memory and Strings:** Avoid hardcoded sizes like `MAX_PATH` when allocating strings unless necessary. Prefer dynamic `character(:), allocatable` if the max length can be determined at runtime to save memory. Use `io_text_readline` in `io.f90` to safely read arbitrary-length lines instead of static buffers.
* **Fractions:** Represent physical or mathematical formulas using fractions as `a/b` (e.g., `3.0_F64/2.0_F64` instead of `1.5_F64`). If available, prefer using named constants defined in `arithmetic.f90` or `math_constants.f90` like `ONE`, `TWO`, `THREE` (and for fractions `ONE/TWO`, `TWO/THREE`) so that formulas are more human readable. Do not change existing fractions in the code.
* **User Messages:** All messages printed for the user should be done by calling the `msg` subroutine from the `display` module, instead of using standard `print` or `write` statements. Use appropriate priority levels like `MSG_ERROR` if needed. Since calls to `msg` often result in very long lines, they should be elegantly split across multiple lines using the `&` continuation character to maintain readability.
* **Imports:** All `use` statements (imports) must be placed at the top of the module, never inside individual subroutines or functions.
* **Functions:** Functions should be declared without the `result(...)` suffix. Furthermore, the declaration of the type of the function result should be the very first declaration inside the function body, preceding the list of dummy arguments.
* **Associate Construct:** When defining an `associate` block, always place a line break after the `associate (` statement, listing each association on a separate indented line, followed by a closing parenthesis on its own line. Example:
  ```fortran
  associate ( &
        Var1 => Object%Var1, &
        Var2 => Object%Var2 &
        )
  ```
