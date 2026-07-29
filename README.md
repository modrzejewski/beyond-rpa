# Overview
The `beyond-rpa` program implements the RPA+ph electronic-structure method, a coupled-cluster based extension of the random-phase approximation designed for an improved description of van der Waals interactions in molecular clusters. RPA+ph serves as a low-level companion method for multi-level calculations of CCSD(T) energies in condensed-phase systems [[Syty2025](docs/02_literature.md), [Cieśliński2023](docs/02_literature.md)].

The algorithms used in `beyond-rpa` are the result of a series of works on the numerical stability of RPA-derived approximations in the many-body expansion of the crystal lattice energy [[Syty2025](docs/02_literature.md), [Cieśliński2023](docs/02_literature.md), [Pham2025](docs/02_literature.md), [Pham2024](docs/02_literature.md), [Pham2023](docs/02_literature.md), [Modrzejewski2021](docs/02_literature.md), [Modrzejewski2020](docs/02_literature.md)]. With high probability, no threshold adjustment is needed to evaluate long-distance two-body and many-body interactions, which are usually prone to numerical noise.

While `beyond-rpa` operates as a standalone program to evaluate long-distance or many-body components and perform extrapolation with the system size, it is a part of the software suite needed for multi-level coupled-cluster energy evaluation. You will need the following other components:
* **The high-level CCSD(T) approximation:** We recommend the LNO-CCSD(T) approximation implemented in the [MRCC](https://www.mrcc.hu) program.
* **Workflow coordination library:** For molecular crystals, we recommend [`mbe-automation`](https://github.com/modrzejewski/mbe-automation). This tool manages the entire workflow: reading the initial crystal structure from a CIF file, generating molecular clusters for the correlated wave-function calculation, and evaluating the final free energy using thermal contributions from machine-learning interatomic potentials.

# Documentation

* [Setup](docs/00_setup.md)
* [Usage and Examples](docs/01_usage.md)
* [Literature](docs/02_literature.md)

# Authors
* Marcin Modrzejewski [ORCID: 0000-0001-9979-8355](https://orcid.org/0000-0001-9979-8355) (main author)

with contributions from:
* Dominik Cieśliński [ORCID: 0000-0002-0858-363X](https://orcid.org/0000-0002-0858-363X) (direct-ring amplitudes)
* Aleksandra Tucholska [ORCID: 0000-0003-2691-5463](https://orcid.org/0000-0003-2691-5463) (coupled-cluster 2-RDM)
* Grzegorz Czekało [ORCID: 0009-0005-4337-8024](https://orcid.org/0009-0005-4337-8024) (reference code for RPA+ph+pp/hh)
* Krystyna Syty [ORCID: 0009-0006-3417-1205](https://orcid.org/0009-0006-3417-1205) (decomposition of amplitudes)
* Khanh Ngoc Pham [ORCID: 0000-0003-1249-8259](https://orcid.org/0000-0003-1249-8259) (finding bugs)
   
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
