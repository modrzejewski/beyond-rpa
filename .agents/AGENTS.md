# GUIDELINES FOR AI AGENTS

This file provides instructions for AI agents interacting with this repository.

## Project Overview

* **Target Audience:** Chemists and physicists with strong backgrounds in statistical thermodynamics, quantum chemistry, and linear algebra. This audience definition informs the expected level of technical understanding.

## General Guidelines

* Before implementing changes, understand the overall program structure and the typical user workflows.

## Documentation, Comments, and Naming

* **Language Style:** Use short, direct language adhering to the standards of technical writing. Minimize the number of words. Avoid non-essential adjectives and adverbs.
* **Docstrings:** For subroutines and functions, use the imperative mood (e.g., "Print a 3x3 matrix", not "Prints a 3x3 matrix"). For types and classes, use noun phrases without verbs as the preamble (e.g., "Basis set configuration"). Place docstrings inside type definitions using standard `!` tokens, mimicking Python class documentation. Do not emphasize words in docstrings by using all caps.
* **Code Comments:** Comments must be short. Explain *only* nontrivial parts of the code. The logical flow and called function names should suffice for understanding. Do not place comments in the body of a subroutine unless strictly necessary. When writing block comments, do not use blank lines around them. The blank 'separator' which guides the eye should be comment symbols `!` without any comment. Example:
  ```fortran
  !
  ! Some comment
  !
  ```
* **Abstraction:** In documentation, abstract from technical details, formats, or technologies that may change.
* **Naming:** Function and variable names must correspond to their physical meaning. Avoid unnecessary words (e.g., "data", "container").
* **CamelCase:** Variables representing counts, sizes, or quantities should be prefixed with an uppercase `N` followed by CamelCase (e.g., `NAtoms`, `NShells`, `NConfigs`).
* **Types:** Types and Objects are prefixed with an uppercase `T` (e.g., `TBasisAssignment`, `TSCFParams`).

## Fortran Coding Style

* **Output Arguments:** Output arguments (`intent(out)`) should generally appear as the first arguments in a subroutine signature, followed by `intent(inout)` and `intent(in)`.
* **Line Length:** Avoid excessively long lines (prefer keeping them under 100-120 characters). For long function or subroutine calls, break them elegantly into multiple lines every few arguments using the `&` continuation character.
* **Memory and Strings:** Avoid hardcoded sizes like `MAX_PATH` when allocating strings unless necessary. Prefer dynamic `character(:), allocatable` if the max length can be determined at runtime to save memory. Use `io_text_readline` in `io.f90` to safely read arbitrary-length lines instead of static buffers.
* **Fractions:** Represent physical or mathematical formulas using fractions as `a/b` (e.g., `3.0_F64/2.0_F64` instead of `1.5_F64`). Do not change existing fractions in the code.
* **Alignment:** When declaring arguments in a subroutine or function signature, the `::` symbols must ALWAYS be strictly aligned under a single, well-spaced column for all arguments, padding shorter type definitions with spaces. This rule does NOT apply to local variables declared in the function body.
* **Type Definitions:** For structures and types, place the docstrings **inside** the type definition, not above it. Example:
  ```fortran
  type TBasisAssignment
        !
        ! Structure that maps basis set files...
        !
        type(TStringList) :: AtomMap
  end type TBasisAssignment
  ```
* **User Messages:** All messages printed for the user should be done by calling the `msg` subroutine from the `display` module, instead of using standard `print` or `write` statements. Use appropriate priority levels like `MSG_ERROR` if needed. Since calls to `msg` often result in very long lines, they should be elegantly split across multiple lines using the `&` continuation character to maintain readability.
* **Imports:** All `use` statements (imports) must be placed at the top of the module, never inside individual subroutines or functions.
