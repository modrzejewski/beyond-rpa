# Input

## Running `beyond-rpa`

Execute the launcher script
```bash
./beyond-rpa/bin/run -nt 16 example.inp 
```
where
* `-nt 16` specifies the number of concurrent threads used by the program. To get the best efficiency, the number of threads should be equal to the number of physical cores available for your calculations. For example, if you reserved a single node with shared memory and 2 CPUs, each having 18 physical cores, then the optimal setting is `-nt 36`.
* `example.inp` is the input text file which contains the definition of the physical system and the requested level of theory. Detailed examples are provided in [Examples](03_examples.md).

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

By default, a global basis set can be defined with the `basis` keyword. Alternatively, you can define different basis sets for specific elements or individual atoms using the `basis_assignment` block.

Inside the `basis_assignment` block, you can use:
* **Element symbols:** To assign a basis set to all atoms of a specific element.
* **Atom indices:** To assign a basis set to a specific atom (1-indexed based on the order in the `xyz` block).
* **Asterisk (`*`):** To assign a fallback/global basis set.

**Prioritization Rules**
When evaluating which basis set applies to a particular atom, the program resolves assignments in the following order of priority:
1. **Atom-specific rules** (highest priority)
2. **Element-specific rules**
3. **Global fallback (`*`)** (lowest priority)

**Example 1: By Element**
```text
basis_assignment
 O cc-pVDZ
 H cc-pVTZ
end
```
In this example, all oxygen (O) atoms are assigned the `cc-pVDZ` basis set, while all hydrogen (H) atoms are assigned the `cc-pVTZ` basis set.

**Example 2: By Atom Index**
```text
basis_assignment
 1 cc-pVDZ
 2 cc-pVTZ
 3 cc-pVDZ
end
```
Here, basis sets are assigned based on the order in which the atoms are listed in the `xyz` geometry block. The first and third atoms receive the `cc-pVDZ` basis set, while the second atom receives the `cc-pVTZ` basis set.

**Example 3: Mixed Assignment with a Fallback (`*`)**
```text
basis_assignment
 1 cc-pVDZ
 O aug-cc-pVDZ
 * cc-pVTZ
end
```
This example highlights the prioritization rules. The first atom (regardless of its element) is forced to use `cc-pVDZ` (priority 1). Any other oxygen atoms use `aug-cc-pVDZ` (priority 2). Every other atom in the system defaults to the `cc-pVTZ` basis set via the `*` fallback (priority 3).

### Effective core potentials

When an element requires a pseudopotential, `beyond-rpa` automatically inspects the assigned atomic orbital basis set file (e.g., `def2-SVP` or `aug-cc-pVDZ-PP`) and extracts the corresponding ECP parameters from the `$ECP` section (GAMESS format, can be downloaded directly from EMSL basis set exchange).

If a different pseudopotential is required, or to override the defaults for specific atoms, use the `ecp_assignment` block. The syntax and priority rules are analogous to `basis_assignment`:

* **Element symbols:** Assigns an ECP to all atoms of an element (e.g., `Cd def2-SVP` or `Hg cc-pVDZ-PP`).
* **Atom indices:** Assigns an ECP to a specific 1-indexed atom from the `xyz` block.
* **Custom files:** The keyword `file` followed by a path relative to the working directory for a file in GAMESS format (e.g., `Cd file ./custom_ecp.txt`).
* **Global fallback (`*`):** Fallback rule applied to any remaining heavy atoms.

**Prioritization Rules**
1. Atom-specific rules (`1 cc-pVDZ-PP`)
2. Element-specific rules (`Cd def2-SVP`)
3. Global fallback (`* cc-pVDZ-PP`)
4. Automatic extraction from the AO basis set parameter file

**Example: Mixed ECP Assignment**
```text
ecp_assignment
  Hg cc-pVDZ-PP
  Xe def2-tzvp
end
```
In this example, Hg atoms use `cc-pVDZ-PP` while Xe atoms use `def2-tzvp`. A complete input is provided in [Examples](03_examples.md#3-dimer-interaction-energy-with-two-different-pseudopotentials).

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
| `RPA+ph` | Random-phase approximation with the ph correction [[Syty2025](04_literature.md)] |

#### Additional RPA-based methods
The following methods are implemented primarily for testing purposes and are significantly less efficient than `RPA+ph`.

| Value | Description |
|---|---|
| `RPA` | Direct-ring random-phase approximation |
| `RPA+RSE` | RPA with single excitations [[Modrzejewski2020](04_literature.md), [Modrzejewski2021](04_literature.md)] |
| `rPT2` | Renormalized second-order perturbation theory [[Ren2013](04_literature.md)] |
| `RPA+2g` | RPA with singles corrections, SOSEX, and additional higher order term referred as 2g [[Cieśliński2023](04_literature.md)] |

### Frozen core

Frozen core orbitals are selected in the `rpa` block in one of two ways.

| Keyword | Selection |
|---|---|
| `CoreOrbThresh {energy}` | Occupied orbitals with energies below `{energy}` (Hartree) are frozen. The default is `-3.0`. |
| `FrozenOrbitals {element} {n_orbitals}` | The `{n_orbitals}` lowest occupied orbitals of each atom of the element are frozen. |

If specified, `FrozenOrbitals` must be given for every element of the system. It takes precedence over `CoreOrbThresh`.

```text
rpa
  FrozenOrbitals O 1
  FrozenOrbitals H 0
end
```

`FrozenOrbitals` counts orbitals above the ECP core. Each subsystem of a dimer or trimer freezes the orbitals of its own atoms. SCF and the mean-field correction terms use all electrons.

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

## Embedding

QM/MM embedding is configured using the `embedding` block. Environment centers can carry a point charge, an effective core potential (ECP), or both. Boundary ECPs prevent electron spillover into the environment.

The general header syntax is:
```text
point_charges({N}) [ecp_centers({M})] [associate_with({monomer})]
```

* `point_charges({N})` — Total number of embedding centers. `{N}` must be positive.
* `ecp_centers({M})` — Optional. Number of centers carrying an ECP ($0 \le M \le N$). Defaults to 0. Exactly `{M}` charge lines must contain the `ECP(...)` tag.
* `associate_with({monomer})` — Optional. Associates the embedding environment exclusively with the specified monomer (`A`, `B`, `C`, or `D`).

A complete input is provided in [Examples](03_examples.md#4-interaction-of-an-embedded-dimer-associate_with).

### Point charges

When only electrostatic embedding is needed, omit `ecp_centers`. Each point charge is specified on a new line using:
```text
Q({charge}) {x} {y} {z}
```
where `{charge}` is the partial charge in atomic units, and `{x}`, `{y}`, `{z}` are its Cartesian coordinates in Angstroms.

Example: Water dimer with 16 point charges computed at the RPA+ph level of theory.
```text
basis aug-cc-pVDZ

scf
  xcfunc HF
end

rpa
  TheoryLevel RPA+ph
  accuracy default
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

embedding
point_charges(16)
Q(0.1) 4.12 -3.21 2.55
Q(-0.1) -4.55 2.11 -3.1
Q(0.1) 3.05 4.44 -2.15
Q(-0.1) -2.9 -4.8 1.5
Q(0.1) 5.1 0.25 0.85
Q(-0.1) -5.2 -0.15 -1.25
Q(0.1) 0.85 5.5 3.2
Q(-0.1) -0.75 -5.6 -2.8
Q(0.1) 2.2 -2.3 4.9
Q(-0.1) -1.8 3.4 -4.5
Q(0.1) 4.8 2.9 1.1
Q(-0.1) -3.7 -2.5 -1.7
Q(0.1) 1.5 -4.1 5.5
Q(-0.1) -1.1 5.2 -5.1
Q(0.1) 0.0 0.0 6.0
Q(-0.1) 0.0 0.0 -6.0
end
```

### Effective core potentials

To place pseudopotentials on boundary centers, append `ecp_centers({M})` to the header line. Insert `ECP({element} {params_name})` between the charge and the coordinates on exactly `{M}` lines:

```text
Q({charge}) ECP({element} {params_name}) {x} {y} {z}
```

* `{element}` — Chemical symbol of the environment atom (used to determine nuclear charge).
* `{params_name}` — Pseudopotential library name (e.g., `def2-SVP`, `cc-pVDZ-PP`) or `file {path}` for a custom file in GAMESS format relative to the working directory.

Example: An embedding block where centers carry library pseudopotentials and a custom parameter file:
```text
embedding
point_charges(16) ecp_centers(4)
Q(0.1) 4.12 -3.21 2.55
Q(-0.1) -4.55 2.11 -3.10
Q(0.1) ECP(Xe def2-SVP) 5.10 0.25 0.85
Q(-0.1) ECP(Xe def2-SVP) -5.20 -0.15 -1.25
Q(0.1) 0.85 5.50 3.20
Q(-0.1) -0.75 -5.60 -2.80
Q(0.1) ECP(Hg file ./my-ecp/ecp.txt) 4.80 2.90 1.10
Q(-0.1) ECP(Hg file ./my-ecp/ecp.txt) -3.70 -2.50 -1.70
...
end
```

### Fragment association (`associate_with`)

This option is useful when modeling the interaction of molecules with a surface, where the embedding belongs exclusively to the surface fragment (monomer A).

Specifying `associate_with(A)` controls the counterpoise-corrected many-body decomposition:
* **Total complex ($AB$) and monomer with real atoms of A ($A[\text{ghost } B]$):** The embedding field is active.
* **Isolated fragment B ($B[\text{ghost } A]$):** Only ghost basis functions of fragment A are present; the embedding field is deactivated. The isolated fragment B is calculated in the unperturbed dimer AO basis without embedding potential.

