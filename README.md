**Beyond-rpa** is a program for performing electronic single-point energy calculations using
the coupled-cluster formulation of the random-phase approximation (RPA) with a series
of corrections derived from the expectation value of the Hamiltonian. While this software can
be applied to any system, the algorithms and numerical thresholds have been adjusted for a
numerically-stable calculation of 100s or 1000s of small energy terms which appear in the
many-body expansion of the crystal lattice energy:
* two-body noncovalent interaction energies,
* nonadditive energies of molecular trimers,
* nonadditive energies of molecular tetramers.

### Compilation

Clone the repository and compile the source code using a Fortran compiler (tested mostly on the Intel Fortran compiler):

```bash
git clone https://github.com/modrzejewski/beyond-rpa.git
cd beyond-rpa/src
./build.py -np 4 compiler_flags
```
where `compiler_flags` are compiler options and `-np` sets the number of concurrent compilation processes. For example,
```bash
./build.py -np 4 ifort-I64
```
The chosen value of `compiler_flags` should correspond to one of the subdirectories in `./src/CompilerFlags`.
Some compiler options are available, but you can create your own subdirectory and add your own commands in the 
`compiler` and `linker` text files.

### Authors
* Marcin Modrzejewski (main)
* Dominik Cieśliński (RPA amplitudes)
* Aleksandra Tucholska (CC 2-RDM)
* Grzegorz Czekało (higher-order corrections)
* Krystyna Syty (decomposition of amplitudes)
* Khanh Ngoc Pham (finding bugs)
   
### Literature references
You can use **beyond-rpa** to replicate the numerical results from the following publications.
* Cieśliński, D., Tucholska, A., Modrzejewski, M., J. Chem. Theory Comput. 19, 6619 (2023); doi: 10.1021/acs.jctc.3c00496
* Pham, K.N., Modrzejewski, M., Klimeš, J., J. Chem. Phys. 160, 224101 (2024); doi: 10.1063/5.0207090
* Pham, K.N., Modrzejewski, M., Klimeš, J., J. Chem. Phys. 158, 144119 (2023); doi: 10.1063/5.0142348

### License
You can use and modify this program under the conditions of the MIT license in `LICENSE.txt`.
