# Overview
**Beyond-rpa** performs electronic single-point energy calculations using the random-phase
approximation (RPA) methods with higher-order corrections expressed via simplified
coupled-cluster amplitudes. While **beyond-rpa** can
be applied to any system, the algorithms and numerical thresholds have been
hand-tuned for a numerically-stable calculation of 100s or 1000s of small
energy terms in the many-body expansion of the crystal lattice energy:
* two-body noncovalent interaction energies,
* nonadditive energies of molecular trimers,
* nonadditive energies of molecular tetramers.

You can use **beyond-rpa** like any other electronic structure software, 
but the simplest way to carry out automated fragment-based lattice energy
calculations is to use it in combination with the **mbe-automation**
companion program available
at [github](https://github.com/modrzejewski/mbe-automation).

# Compilation

1. Clone the repository
```
git clone https://github.com/modrzejewski/beyond-rpa.git
```

2. Go to `./beyond-rpa/src` and run the compilation script
```
./build.py -np 4 ifort-I64
```
The compiler flags, here it is `ifort-I64`, should correspond to one of
the subdirectories in `./src/CompilerFlags`. Choose a set appropriate for your
system among the available sets of flags or make your own. You simply need to create
your own subdirectory and add the command lines in the `compiler` and `linker`
text files.

# Usage
1. Run the launcher script in the bin directory,
```
./bin/run -nt 16 example.inp 
```
where `-nt 16` specifies that there are `16` concurrent threads and `example.inp`
is the input text file.

2. Example input: noncovalent interaction energy of a water dimer
```
jobtype uks rpa
basis aug-cc-pVDZ
scf
 xcfunc HF
end

rpa
 accuracy default
 TheoryLevel JCTC2024
end

xyz
3 3
O      1.531750     0.005922    -0.120880
H      0.575968    -0.005249     0.024966
H      1.906249    -0.037561     0.763218
O     -1.396226    -0.004990     0.106766
H     -1.789372    -0.742283    -0.371009
H     -1.777037     0.777638    -0.304264
end
```

3. Example input: nonadditive 3-body interaction energy of a formaldehyde trimer
```
jobtype uks rpa
basis aug-cc-pVDZ
scf
 xcfunc HF
end

rpa
 accuracy default
 TheoryLevel JCTC2024
end

xyz
4 4 4
O   2.074088   0.498855   1.635515
C   2.169454   0.154280   2.793163
H   2.970226  -0.523067   3.124239
H   1.470340   0.502963   3.564302
O   2.074088   0.498855   6.109515
C   2.169454   0.154280   7.267163
H   2.970226  -0.523067   7.598239
H   1.470340   0.502963   8.038302
O   2.074088   0.498855  10.583515
C   2.169454   0.154280  11.741163
H   2.970226  -0.523067  12.072239
H   1.470340   0.502963  12.512302
end
```

# Authors
* Marcin Modrzejewski (main author)

with contributions from:
* Dominik Cieśliński (direct-ring amplitudes)
* Aleksandra Tucholska (coupled-cluster 2-RDM)
* Grzegorz Czekało (reference code for the quadratic corrections)
* Krystyna Syty (decomposition of amplitudes)
* Khanh Ngoc Pham (finding bugs)
   
# Literature
You can use **beyond-rpa** to replicate the numerical results from the following publications:
* Cieśliński, D., Tucholska, A., Modrzejewski, M., J. Chem. Theory Comput. 19, 6619 (2023); doi: 10.1021/acs.jctc.3c00496
* Pham, K.N., Modrzejewski, M., Klimeš, J., J. Chem. Phys. 160, 224101 (2024); doi: 10.1063/5.0207090
* Pham, K.N., Modrzejewski, M., Klimeš, J., J. Chem. Phys. 158, 144119 (2023); doi: 10.1063/5.0142348
* Modrzejewski, M., Yourdkhani, S., Śmiga, Sz., Klimeš, J., J. Chem. Theory Comput. 17, 804 (2021); doi: 10.1021/acs.jctc.0c00966
* Modrzejewski, M., Yourdkhani, S., Klimeš, J., J. Chem. Theory Comput. 16, 427 (2020); doi: 10.1021/acs.jctc.9b00979

# License
You can freely use this program, modify it, and embed it in your code provided
that you provide attribution to the original authors and cite the papers
related to the implementation and derivation of the methods. The full text
of the license is available in `LICENSE.txt`.

