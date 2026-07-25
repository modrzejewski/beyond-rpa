# Overview
`beyond-rpa` performs electronic single-point energy calculations using the random-phase
approximation (RPA) methods with higher-order corrections expressed via simplified
coupled-cluster amplitudes. While `beyond-rpa` can
be applied to any system, the algorithms and numerical thresholds have been
hand-tuned for a numerically-stable calculation of 100s or 1000s of small
energy terms in the many-body expansion of the crystal lattice energy:
* two-body noncovalent interaction energies,
* nonadditive energies of molecular trimers,
* nonadditive energies of molecular tetramers.

You can use `beyond-rpa` like any other electronic structure software, 
but the simplest way to carry out automated fragment-based lattice energy
calculations is to use it in combination with the `mbe-automation`
companion program available
at [github](https://github.com/modrzejewski/mbe-automation).

# Documentation

* [Setup](docs/00_setup.md)
* [Usage and Examples](docs/01_usage.md)

# Authors
* Marcin Modrzejewski (main author)

with contributions from:
* Dominik Cieśliński (direct-ring amplitudes)
* Aleksandra Tucholska (coupled-cluster 2-RDM)
* Grzegorz Czekało (reference code for RPA+ph+pp/hh)
* Krystyna Syty (decomposition of amplitudes)
* Khanh Ngoc Pham (finding bugs)
   
# Citation
When this software or its derivatives are used in scientific publications, please cite the following works:

```bibtex
@article{syty2025multilevel,
  author = {Syty, Krystyna and Czekało, Grzegorz and Pham, Khanh Ngoc and Modrzejewski, Marcin},
  title = {Multi-Level Coupled-Cluster Description of Crystal Lattice Energies},
  journal = {J. Chem. Theory Comput.},
  volume = {21},
  pages = {5533},
  year = {2025},
  doi = {10.1021/acs.jctc.5c00428}
}

@article{cieslinski2023post,
  author = {Cieśliński, Dominik and Tucholska, Aleksandra and Modrzejewski, Marcin},
  title = {Post-Kohn-Sham Random-Phase Approximation and Correction Terms in the Expectation-Value Coupled-Cluster Formulation},
  journal = {J. Chem. Theory Comput.},
  volume = {19},
  pages = {6619},
  year = {2023},
  doi = {10.1021/acs.jctc.3c00496}
}
```

# Literature
You can also use `beyond-rpa` to replicate the numerical results from the following publications:
1. Syty, K., Czekało, G., Pham, K.N., Modrzejewski, M., J. Chem. Theory Computat 21, 5533 (2025); [doi: 10.1021/acs.jctc.5c00428](https://doi.org/10.1021/acs.jctc.5c00428)
2. Cieśliński, D., Tucholska, A., Modrzejewski, M., J. Chem. Theory Comput. 19, 6619 (2023); [doi: 10.1021/acs.jctc.3c00496](https://doi.org/10.1021/acs.jctc.3c00496)
3. Pham, K.N., Modrzejewski, M., Klimeš, J., J. Chem. Phys. 160, 224101 (2024); [doi: 10.1063/5.0207090](https://doi.org/10.1063/5.0207090)
4. Pham, K.N., Modrzejewski, M., Klimeš, J., J. Chem. Phys. 158, 144119 (2023); [doi: 10.1063/5.0142348](https://doi.org/10.1063/5.0142348)
5. Modrzejewski, M., Yourdkhani, S., Śmiga, Sz., Klimeš, J., J. Chem. Theory Comput. 17, 804 (2021); [doi: 10.1021/acs.jctc.0c00966](https://doi.org/10.1021/acs.jctc.0c00966)
6. Modrzejewski, M., Yourdkhani, S., Klimeš, J., J. Chem. Theory Comput. 16, 427 (2020); [doi: 10.1021/acs.jctc.9b00979](https://doi.org/10.1021/acs.jctc.9b00979)

# License
This program is freely available for use, modification, and integration into other software under the MIT License. For full licensing details, please refer to the `LICENSE` file.
