# Theory Primer

## 1. What physical quantities can be computed with `beyond-rpa`?
The `beyond-rpa` program is a part of a software suite required to compute the
lattice energy of an organic molecular crystal in the multi-level
coupled-cluster approximation using the many-body expansion technique
[[Syty2025](04_literature.md)].
When converged with cutoffs and basis set size, the resulting lattice energies
can serve as calibration points for simpler models of
crystal thermodynamics, such as DFT and machine-learning interatomic potentials.

## 2. What is the multi-level scheme?
The multi-level scheme optimizes the cost by matching appropriate computational approximations to different physical interactions separated in the many-body expansion of the lattice energy.
This approach yields significant savings, especially for nonadditive three-body interactions, which grow rapidly in number with cutoff distance but require smaller basis sets and lower-order coupled-cluster approximations than pairwise interactions.

## 3. What developments were required to make the multi-level scheme feasible?
The approach required three recent methodological advances: (1) 
local coupled-cluster methods with high accuracy for noncovalent interactions (LNO-CCSD(T) [[Nagy2024](04_literature.md)]);
(2) low-level coupled-cluster method which handles long-range and
many-body interactions (RPA+ph [[Syty2025](04_literature.md), [Cieśliński2023](04_literature.md)]); 
and (3) efficient periodic Hartree-Fock
implementations.

## 4. What is the random-phase approximation?
RPA is the simplest coupled-cluster approximation. Despite severe
simplifications to the $T$ operator, the energy model captures many-body
dispersion, a key physical interaction in noncovalent systems. Nonadditive
interactions are implicitly included up to arbitrary order $n>2$.

## 5. Why is the random-phase approximation efficient?
Direct RPA contains only the direct-ring contributions, where
the contractions between tensors, such as wave-function amplitudes and two-electron integrals, reduce
to matrix-matrix multiplications. Thus, the heaviest step
reduces to compute-bound BLAS Level
3 routines perfectly optimized on modern hardware.

## 6. What are the limitations of RPA?
Direct RPA violates the Pauli exclusion principle and lacks triple excitations. However, in the multi-level scheme we
compensate for these limitations by including the ph beyond-RPA corrections and using RPA only as a low-level admixture to CCSD(T).

## 7. Why are corrections other than SOSEX needed?
The SOSEX correction to noncovalent interaction energies in
post-Hartree-Fock RPA is almost negligible. The third-order
particle-hole corrections are large and reduce the underbinding present in RPA(HF) [[Syty2025](04_literature.md), [Cieśliński2023](04_literature.md)].

## 8. What's the role of periodic Hartree-Fock embedding?
The correlation energy is smooth and decays rapidly with intermolecular distance, but the mean-field component 
keeps fluctuating even at very large distances, beyond any realistic cutoff that can be applied in the explicit many-body
expansion [see the supporting information in [Syty2025](04_literature.md)]. 

## 9. Why use Hartree-Fock instead of Kohn-Sham orbitals?
Density-functional theory approximations introduce qualitative errors when
describing many-body noncovalent interactions, which then propagate to the
post-SCF RPA [[Modrzejewski2020](04_literature.md),
[Modrzejewski2021](04_literature.md)]. Hartree-Fock theory is free from
these artifacts [[Pham2024](04_literature.md)].

## 10. What are the future directions?
A complete description of the thermodynamics of organic molecular crystals.
