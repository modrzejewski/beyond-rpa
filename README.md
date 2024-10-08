## Description

**Beyond-RPA** is a program for performing electronic single-point energy calculations using
the coupled-cluster formulation of the random-phase approximation (RPA) with a series
of corrections derived from the expectation value of the Hamiltonian. While this software can
be applied to any system, the algorithms and numerical thresholds have been adjusted for a
numerically-stable calculation of 100s or 1000s of small energy terms which contribute to the
many-body expansion of the crystal lattice energy:
* two-body noncovalent interaction energies,
* nonadditive energies of molecular trimers,
* nonadditive energies of molecular tetramers.

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

## Authors
* Marcin Modrzejewski (main)
* Dominik Ciśliński (RPA amplitudes)
* Aleksandra Tucholska (CC 2-RDM)
* Grzegorz Czekało (higher-order corrections)
* Krystyna Syty (decomposition of amplitudes)
* Khanh Ngoc Pham (finding bugs)
   
## References
* 
