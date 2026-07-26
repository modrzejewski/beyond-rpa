# Overview
The `beyond-rpa` program provides a robust implementation of the RPA+ph electronic-structure method, an efficient extension of the traditional random-phase approximation of the electron-correlation energy using the coupled-cluster formalism. It is intended as a low-level companion method which enables fragment-based multi-level calculations of the CCSD(T) energies of condensed-phase systems [[Syty2025](docs/02_literature.md), [Cieśliński2023](docs/02_literature.md)]. The primary use cases for RPA+ph to date have been multi-level CCSD(T) computations of molecular crystal lattice energies [[Syty2025](docs/02_literature.md)].

The program is designed for high numerical robustness with its default settings. In particular, no numerical precision settings need to be adjusted to evaluate the long-distance and many-body terms, which are usually problematic due to numerical noise. The RPA+ph method provided here is a good match for the LNO-CCSD(T) approximation available in the [MRCC](https://www.mrcc.hu) program.

The simplest way to use `beyond-rpa` for automated lattice energy evaluations for molecular crystals is via the [`mbe-automation`](https://github.com/modrzejewski/mbe-automation) software, which handles the whole workflow from initial crystal structure in a CIF file up to generation of molecular clusters for the correlated wave-function calculation.

# Documentation

* [Setup](docs/00_setup.md)
* [Usage and Examples](docs/01_usage.md)
* [Literature](docs/02_literature.md)

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

# License
This program is freely available for use, modification, and integration into other software under the MIT License. For full licensing details, please refer to the `LICENSE` file.
