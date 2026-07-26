# Overview
The `beyond-rpa` program provides a robust implementation of the RPA+ph electronic-structure method, an efficient extension of the traditional random-phase approximation of the electron-correlation energy using the coupled-cluster formalism. RPA+ph is intended as a low-level companion method enabling multi-level calculations of the CCSD(T) energies of condensed-phase systems [[Syty2025](docs/02_literature.md), [Cieśliński2023](docs/02_literature.md)].

The algorithms employed in `beyond-rpa` are designed for high numerical stability out of the box. No precision settings need adjustment to reliably evaluate the long-distance and many-body terms, typically sensitive to numerical noise. In particular, the RPA+ph method implemented here is a good match for the LNO-CCSD(T) approximation available in the [MRCC](https://www.mrcc.hu) program.

While `beyond-rpa` can be used as a standalone program, in the context of molecular crystal thermodynamics, we recommend using the [`mbe-automation`](https://github.com/modrzejewski/mbe-automation) workflow automation program, which handles the whole workflow, from the initial crystal structure in a CIF file, through generation of molecular clusters for the correlated wave-function calculation, to the final free energy evaluation with thermal contributions obtained from machine-learning interatomic potentials.

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
