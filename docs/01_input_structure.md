# Input

This chapter documents the structure of the input file and lists the possible values for various configuration keys.

## `jobtype {JOBTYPE}`

| Value | Description |
|---|---|
| `uks rpa` | Unrestricted Kohn-Sham RPA calculation |
| `uks sp` | Unrestricted Kohn-Sham single point |

## `basis {basis_set_params}`

The basis set name corresponds to the filenames available in the `basis-sets/` directory (case-insensitive). All basis set parameters are downloaded from the EMSL basis set exchange website.

| Keys | Description |
|---|---|
| `cc-pvdz`, `cc-pvtz`, `cc-pvqz`, `cc-pv5z`, `cc-pvddz`, `cc-pvtdz`, `cc-pvqdz`, `cc-pv5dz`, `cc-pcvtz`, `cc-pcvqz`, `cc-pwcvqz`, `cc-pwcv5z`, `cc-pvdz-pp`, `cc-pvtz-pp` | Dunning's correlation-consistent polarized valence basis sets |
| `aug-cc-pvdz`, `aug-cc-pvtz`, `aug-cc-pvqz`, `aug-cc-pv5z`, `aug-cc-pvddz`, `aug-cc-pvtdz`, `aug-cc-pvqdz`, `aug-cc-pv5dz`, `aug-cc-pcvtz`, `aug-cc-pcvqz`, `aug-cc-pwcvqz`, `aug-cc-pwcv5z`, `aug-cc-pvdz-pp`, `aug-cc-pvtz-pp`, `d-aug-cc-pvdz`, `d-aug-cc-pvtz`, `d-aug-cc-pvqz`, `d-aug-cc-pv5z` | augmented Dunning's correlation-consistent polarized valence basis sets |
| `def2-sv_p`, `def2-svp`, `def2-tzvp`, `def2-tzvpp`, `def2-qzvp`, `def2-qzvpp` | Ahlrichs' def2 basis sets |
| `def2-svpd`, `def2-tzvpd`, `def2-tzvppd`, `def2-qzvpd`, `def2-qzvppd` | augmented Ahlrichs' def2 basis sets |

## `scf xcfunc {xc_model}`

| Value | Description |
|---|---|
| `HF` | Hartree-Fock |
| `PBE` | Perdew-Burke-Ernzerhof |
| `PBE0` | Perdew-Burke-Ernzerhof hybrid |
| `TPSS` | Tao-Perdew-Staroverov-Scuseria |
| `SCAN` | Strongly Constrained and Appropriately Normed |

## `rpa TheoryLevel {rpa_method}`

| Value | Description |
|---|---|
| `RPA+ph` | Random-phase approximation with the ph correction [[Syty2025](02_literature.md)] |

#### Additional RPA-based methods
The following methods are implemented primarily for testing purposes and are significantly less efficient than `RPA+ph`.

| Value | Description |
|---|---|
| `RPA` | Standard RPA |
| `RPA+RSE` | RPA with single excitations |
| `rPT2` | Renormalized second-order perturbation theory |
| `RPA+2g` | RPA with second-order exchange |

## Geometry specification

The `xyz` block defines the geometry of the system. Depending on the system, the number of atoms is specified as a space-separated list of integers:

- **Single molecule:** One integer (e.g., `3`)
- **Dimer:** Two integers (e.g., `3 3`)
- **Trimer:** Three integers (e.g., `4 4 4`)
- **Tetramer:** Four integers (e.g., `3 3 3 3`)

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
