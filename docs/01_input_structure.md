# Input

## Running `beyond-rpa`

Execute the launcher script
```bash
./beyond-rpa/bin/run -nt 16 example.inp 
```
where
* `-nt 16` specifies the number of concurrent threads used by the program. To get the best efficiency, the number of threads should be equal to the number of physical cores available for your calculations. For example, if you reserved a single node with shared memory and 2 CPUs, each having 18 physical cores, then the optimal setting is `-nt 36`.
* `example.inp` is the input text file which contains the definition of the physical system and the requested level of theory. Detailed examples are provided in the Examples chapter.

## General Input Structure

A complete input file consists of global settings (like the basis set) and specific blocks defining the geometry (`xyz`), self-consistent field options (`scf`), and beyond-mean-field methods (`rpa`).

Below is a mockup illustrating the structure:

```text
basis {basis_set_params}

scf
  xcfunc {xc_model}
end

rpa
  TheoryLevel {rpa_method}
end

xyz
  {number_of_atoms_per_fragment}
  {atom_symbol} {x} {y} {z}
  ...
end
```

## Basis set

The basis set name corresponds to the filenames available in the `basis-sets/` directory (case-insensitive). All basis set parameters are downloaded from the EMSL basis set exchange website.

| Keys | Description |
|---|---|
| `cc-pvdz`, `cc-pvtz`, `cc-pvqz`, `cc-pv5z` | Dunning's correlation-consistent polarized valence basis sets |
| `aug-cc-pvdz`, `aug-cc-pvtz`, `aug-cc-pvqz`, `aug-cc-pv5z` | augmented Dunning's correlation-consistent polarized valence basis sets |
| `def2-svp`, `def2-tzvp`, `def2-tzvpp`, `def2-qzvp`, `def2-qzvpp` | Ahlrichs' def2 basis sets |
| `def2-svpd`, `def2-tzvpd`, `def2-tzvppd`, `def2-qzvpd`, `def2-qzvppd` | augmented Ahlrichs' def2 basis sets |

## Mean-field hamiltonian

This section is configured within the `scf` block using the `xcfunc` keyword followed by the `{xc_model}`.

| Value | Description |
|---|---|
| `HF` | Hartree-Fock |
| `PBE`, `PBE0` | Perdew-Burke-Ernzerhof (pure and hybrid) |
| `TPSS`, `TPSSh` | Tao-Perdew-Staroverov-Scuseria (pure and hybrid) |
| `SCAN` | Strongly constrained and appropriately normed exchange-correlation functional |

## Post-SCF correlation

This section is configured within the `rpa` block using the `TheoryLevel` keyword followed by the `{rpa_method}`.

| Value | Description |
|---|---|
| `RPA+ph` | Random-phase approximation with the ph correction [[Syty2025](02_literature.md)] |

#### Additional RPA-based methods
The following methods are implemented primarily for testing purposes and are significantly less efficient than `RPA+ph`.

| Value | Description |
|---|---|
| `RPA` | Direct-ring random-phase approximation |
| `RPA+RSE` | RPA with single excitations [[Modrzejewski2020](02_literature.md), [Modrzejewski2021](02_literature.md)] |
| `rPT2` | Renormalized second-order perturbation theory [[Ren2013](02_literature.md)] |
| `RPA+2g` | RPA with singles corrections, SOSEX, and additional higher order term referred as 2g [[Cieśliński2023](02_literature.md)] |

## Geometry

The `xyz` block defines the geometry of the system. The string that specifies the number of atoms in the molecular subsystems controls how the calculation is performed. Subsystems (e.g., monomers A and B) are specified together with the total system (e.g., dimer AB) because their calculation reuses intermediates from the total system.

| Molecular complex | Number of atoms | Calculated property |
|---|---|---|
| Single molecule | One integer (e.g., `3`) | single-point energy |
| Dimer | Two integers (e.g., `3 3`) | noncovalent dimer interaction energy |
| Trimer | Three integers (e.g., `4 4 4`) | nonadditive three-body interaction energy |
| Tetramer | Four integers (e.g., `3 3 3 3`) | nonadditive four-body interaction energy |

Example for a dimer:
```text
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
