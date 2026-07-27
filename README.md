# Overview
The `beyond-rpa` program implements the RPA+ph electronic-structure method, which extends the random-phase approximation of the electron-correlation energy using the coupled-cluster formalism. RPA+ph serves as a low-level companion method for multi-level calculations of CCSD(T) energies in condensed-phase systems [[Syty2025](docs/02_literature.md), [Cieśliński2023](docs/02_literature.md)].

The algorithms in `beyond-rpa` are numerically stable by design. They evaluate noise-sensitive long-distance and many-body terms without requiring adjustments to precision settings.

While `beyond-rpa` operates as a standalone program, a complete multi-level coupled-cluster energy calculation requires three software components:
* **High-level method:** We recommend the LNO-CCSD(T) approximation implemented in the [MRCC](https://www.mrcc.hu) program.
* **Low-level method:** `beyond-rpa` handles the long-distance or many-body components.
* **Workflow coordination software:** For molecular crystals, we recommend [`mbe-automation`](https://github.com/modrzejewski/mbe-automation). This tool manages the entire workflow: reading the initial crystal structure from a CIF file, generating molecular clusters for the correlated wave-function calculation, and evaluating the final free energy using thermal contributions from machine-learning interatomic potentials.

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
