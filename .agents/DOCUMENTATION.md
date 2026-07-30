# Documentation Guidelines

## Documentation and Comments

* **Docstrings:** For subroutines and functions, use the imperative mood (e.g., "Print a 3x3 matrix", not "Prints a 3x3 matrix"). For types and classes, use noun phrases without verbs as the preamble (e.g., "Basis set configuration"). Place docstrings inside type definitions using standard `!` tokens, mimicking Python class documentation. Do not emphasize words in docstrings by using all caps.
* **Code Comments:** Comments must be short and use relatively short line breaks. Do not use full stops (periods) at the end of single-sentence comments. Explain *only* nontrivial parts of the code and avoid documenting self-explanatory variables. The logical flow and called function names should suffice for understanding. Do not place comments in the body of a subroutine unless strictly necessary. When writing block comments, do not use blank lines around them. The blank 'separator' which guides the eye should be comment symbols `!` without any comment. Example:
  ```fortran
  !
  ! Some comment
  !
  ```
* **Abstraction:** In documentation, abstract from technical details, formats, or technologies that may change.

## Type Definitions

* **Type Definitions:** For structures and types, place the docstrings **inside** the type definition, not above it. Explain every non-trivial argument using the `! \n  ! comment \n  !` format directly before the argument definition, instead of relying solely on a generic preamble. Ensure descriptions are precise and describe exactly what the argument represents. Example:
  ```fortran
  type TExampleType
        !
        ! General preamble describing the type
        !
        integer :: Id
        !
        ! Precise description of the argument
        !
        real(F64), dimension(:), allocatable :: Values
  end type TExampleType
  ```
