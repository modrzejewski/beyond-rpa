## Description

**Beyond-RPA** is a program for performing electronic single-point energy calculations using
the coupled-cluster formulation of the random-phase approximation (RPA) with a series
of corrections derived from the expectation value of the Hamiltonian.

Aside from the availability
of the beyond-RPA level of theory, the program's unique feature is the combination of modern
computational techniques based on tensor decompositions with near-perfect numerical precision
of the results. Specifically, the numerical thresholds available in the program
are designed for a numerically-stable calculation of
* two-body noncovalent interaction energies,
* nonadditive energies of molecular trimers,
* nonadditive energies of molecular tetramers,
  
which contribute to the many-body expansion (MBE) of the lattice energy of a molcular crystal.
While you can use this software for any finite system in a Gaussian basis set, the conservative
thresholds adjusted for MBE might be unnecessarily tight for some purposes.

## Compilation

To install **Beyond-RPA**, clone the repository and compile the source code using the Intel Fortran compiler (tested with this compiler):

```bash
git clone https://github.com/modrzejewski/beyond-rpa.git
cd beyond-rpa/src
./build.py compiler_flags
```
where `compiler_flags` stands for a the set of compiler options appropriate for your system and its value
is one of the subdirectory names in `./src/CompilerFlags`. Some compiler options are available in
`./src/CompilerFlags`, but your can add your own set and pass it as an argument to `./build.py`.
